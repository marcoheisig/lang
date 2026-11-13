(in-package #:lang.internals)

;;; Cross-Heap Garbage Collection
;;;
;;; A mirror owns its target, so a cycle that crosses the language boundary
;;; keeps itself alive.  A typical example is a Lisp object that references a
;;; Python list, which in turn contains the Lisp object: SBCL considers the
;;; Lisp object live, because *MIRROR-INTO-PYTHON-TABLE* references it as long
;;; as its Python mirror exists, and Python considers the list live, because
;;; its Lisp mirror owns a reference.
;;;
;;; COLLECT-GARBAGE reclaims such cycles.  It describes the part of the Python
;;; heap that only Lisp keeps alive to SBCL, and lets SBCL's collector decide:
;;;
;;; 1. Determine which Python objects would stay alive if all Lisp mirrors
;;;    released their references.  We call those objects rooted.  Python
;;;    can't enumerate its roots, so we use the method of its own cycle
;;;    collector: From the reference count of each object, subtract the
;;;    references we can see by traversing all other objects.  A remainder
;;;    means that there is a reference we can't see, such as a local variable
;;;    of a running Python function, so the object is a root.  The only
;;;    difference to Python's collector is that we also subtract the
;;;    references owned by Lisp mirrors.
;;;
;;; 2. Remove the table entries of all Python mirrors that aren't rooted, and
;;;    describe the Python objects that reach them with ephemerons instead:
;;;    One stand-in per Python object, and one entry in a weak-key hash table
;;;    per Python object, which maps its stand-in to the stand-ins of the
;;;    objects it references.  The entry of a Python mirror's stand-in also
;;;    references the mirrored Lisp object, and each Lisp mirror maps to the
;;;    stand-in of its Python object.
;;;
;;; 3. Run a full GC.  A weak-key hash table entry keeps its value alive only
;;;    while its key is alive, even if the value references the key, so a
;;;    Lisp object survives exactly if it is reachable from a Lisp root,
;;;    possibly through Python objects that a surviving Lisp mirror reaches.
;;;
;;; 4. Restore the table entries of the Python mirrors whose Lisp object
;;;    survived, and release the references of the Lisp mirrors that died.  The Python
;;;    mirrors whose Lisp object died are zombies, which Python frees as soon
;;;    as nothing references them, but a zombie can survive for a while, e.g.,
;;;    as part of a Python cycle.  Mirroring a zombie into Lisp signals an
;;;    error.
;;;
;;; Steps 1 to 4 happen while no other thread runs Python code: with the
;;; global interpreter lock held or, in free-threaded builds, with all other
;;; Python threads paused, see WITH-WORLD-STOPPED.  The references of Lisp
;;; mirrors that die in between aren't released either, because releasing a
;;; reference needs the same.
;;;
;;; Any mistake in step 1 must err on the side of calling an object rooted,
;;; which merely delays its collection.  References we can't see always err
;;; on that side, e.g., references from objects of a C extension type without
;;; tp_traverse.  The only way to err on the dangerous side is a traversal
;;; that reports more references than the reference count includes.  Python
;;; 3.14 does exactly that for frames, see FRAME-PYTYPE-ADDRESSES.

(defconstant +pyobject-type-offset+
  (with-global-interpreter-lock-held
    (with-new-pyobjects ((pylist (|%PyList_New| 0)))
      (loop for offset from 0 to 64 by +pointer-size+
            when (cffi:pointer-eq (cffi:mem-ref pylist :pointer offset)
                                  *list-pyobject*)
              return offset
            finally
               (error "Failed to determine the type offset of Python objects."))))
  "The byte offset from the start of a Python object to its type.")

(defparameter *free-threaded-python-p*
  (with-pyobjects ((pyvalue (expr "bool(__import__('sysconfig').get_config_var('Py_GIL_DISABLED'))")))
    (|%PyObject_IsTrue| pyvalue))
  "Whether Python runs without the global interpreter lock.")

(declaim (inline address-refcount))
(defun address-refcount (address)
  "Returns the reference count of the Python object at ADDRESS.  The result is
at least 2^31 for immortal objects and, in free-threaded builds, for objects
with deferred reference counts."
  (if *free-threaded-python-p*
      ;; Free-threaded builds split the reference count into a local and a
      ;; shared part.  Objects with deferred reference counts, which may be
      ;; referenced from interpreter stacks without being counted, have
      ;; 2^60 added to their shared part, so they always look rooted.
      (cffi:foreign-funcall-pointer *py-refcnt-function* ()
                                    :uintptr address :ssize)
      ;; Since Python 3.12, only the lower 32 bits of the reference count
      ;; field count references on 64-bit platforms, and immortal objects
      ;; have bit 31 set.  On little-endian platforms, these are the first
      ;; four bytes.
      (cffi:mem-ref (cffi:make-pointer address) :uint32 +pyobject-refcount-offset+)))

(defun frame-pytype-addresses ()
  "Returns the addresses of the Python types whose instances may report one
reference twice when traversed, although only one of them is counted.

Since Python 3.14, LOAD_FAST_BORROW pushes a local variable onto the
evaluation stack without incrementing its reference count, and such a
borrowed reference can stay on the stack of a suspended generator.
Traversing the frame visits both the local variable and the stack entry.
Python's own collector skips the stack entry, but only for its internal
visitor (_PyGC_VisitStackRef in Python/gc.c).  A borrowed stack entry always
duplicates a local variable of the same frame, so we count at most one
reference from each frame to each object.  The price is that an object with
two counted references from the same frame looks rooted."
  (let ((hexversion (cffi:mem-ref (cffi:foreign-symbol-pointer "Py_Version")
                                  :unsigned-long)))
    (if (< hexversion #x030E0000)
        '()
        (loop for name in '("PyFrame_Type" "PyGen_Type" "PyCoro_Type" "PyAsyncGen_Type")
              collect (cffi:pointer-address (cffi:foreign-symbol-pointer name))))))

;;; The Heap Graph

(defstruct (heap-graph
            (:constructor make-heap-graph ())
            (:copier nil)
            (:predicate nil))
  "The Python objects that matter for COLLECT-GARBAGE, numbered from zero, and
the references between them."
  ;; Maps the address of each node to its number.
  (numbers (make-hash-table) :type hash-table :read-only t)
  ;; The number of nodes, and the address of each node.
  (size 0 :type (and fixnum unsigned-byte))
  (addresses (make-array 4096 :element-type '(unsigned-byte 64))
   :type (simple-array (unsigned-byte 64) (*)))
  ;; The successors of node I are the elements of EDGES from (AREF STARTS I)
  ;; below (AREF STARTS (1+ I)).
  (starts (make-array 4096 :element-type 'fixnum)
   :type (simple-array fixnum (*)))
  (nedges 0 :type (and fixnum unsigned-byte))
  (edges (make-array 16384 :element-type 'fixnum)
   :type (simple-array fixnum (*)))
  ;; Maps the address of each type to the address of its tp_traverse
  ;; function, or to zero if it has none.
  (traverse-functions (make-hash-table) :type hash-table :read-only t))

(defun grow-vector (vector)
  "Returns a vector twice as long as VECTOR that starts with its elements."
  (replace (make-array (* 2 (length vector))
                       :element-type (array-element-type vector))
           vector))

(defun heap-graph-add-node (graph address)
  "Add the Python object at ADDRESS to GRAPH and return its number."
  (let ((number (heap-graph-size graph)))
    (when (= number (length (heap-graph-addresses graph)))
      (setf (heap-graph-addresses graph)
            (grow-vector (heap-graph-addresses graph))))
    (setf (aref (heap-graph-addresses graph) number) address)
    (setf (gethash address (heap-graph-numbers graph)) number)
    (setf (heap-graph-size graph) (1+ number))
    number))

(defun heap-graph-add-edge (graph number)
  "Add an edge from the node that is being traversed to node NUMBER."
  (let ((index (heap-graph-nedges graph)))
    (when (= index (length (heap-graph-edges graph)))
      (setf (heap-graph-edges graph)
            (grow-vector (heap-graph-edges graph))))
    (setf (aref (heap-graph-edges graph) index) number)
    (setf (heap-graph-nedges graph) (1+ index))))

(defun heap-graph-remove-duplicate-edges (graph start)
  "Remove all duplicates among the edges of GRAPH from index START on."
  (let ((edges (heap-graph-edges graph))
        (end start))
    (loop for index from start below (heap-graph-nedges graph)
          for number = (aref edges index)
          unless (find number edges :start start :end end)
            do (setf (aref edges end) number)
               (incf end))
    (setf (heap-graph-nedges graph) end)))

(defun heap-graph-traverse-function (graph type-address)
  (or (gethash type-address (heap-graph-traverse-functions graph))
      (setf (gethash type-address (heap-graph-traverse-functions graph))
            (cffi:pointer-address
             (|%PyType_GetSlot| (cffi:make-pointer type-address) :tp-traverse)))))

;;; The callbacks below receive the graph they extend in this variable,
;;; because foreign code can't pass Lisp objects around.  Only one thread can
;;; use it at a time, because it is only used while holding the global
;;; interpreter lock.
(sb-ext:defglobal **heap-graph** nil)

(cffi:defcallback heap-graph-add-tracked-object :int
    ((address :uintptr) (argument :pointer))
  (declare (ignore argument))
  (heap-graph-add-node **heap-graph** address)
  ;; Continue with the next object.
  1)

(cffi:defcallback heap-graph-visit :int
    ((address :uintptr) (argument :pointer))
  (declare (ignore argument))
  (let* ((graph **heap-graph**)
         (number (gethash address (heap-graph-numbers graph))))
    (cond (number
           (heap-graph-add-edge graph number))
          ;; Objects that can't be traversed can't reach a mirror, so they
          ;; don't matter.  Mirrors that can't be traversed are already
          ;; nodes.
          ((|%PyObject_IS_GC| address)
           (heap-graph-add-edge graph (heap-graph-add-node graph address)))))
  ;; Continue the traversal.
  0)

(defun snapshot-python-heap (addresses)
  "Returns a heap graph of the objects at the supplied ADDRESSES and of the
objects that can be traversed and that are reachable from them.  Unless Python
is free-threaded, the graph also contains all objects tracked by Python's
garbage collector.

Leaving out other objects is safe, because their references to the nodes of
the graph aren't subtracted, so these nodes look rooted.  It only costs
precision when such an object is garbage.  In free-threaded builds, we have no
choice, because enumerating the tracked objects pauses all other threads
itself, so it can't be done while they are paused already."
  (let ((graph (make-heap-graph))
        (frame-types (frame-pytype-addresses)))
    (setf **heap-graph** graph)
    (unwind-protect
         (progn
           (unless *free-threaded-python-p*
             (|%PyUnstable_GC_VisitObjects|
              (cffi:callback heap-graph-add-tracked-object)
              (cffi:null-pointer)))
           (dolist (address addresses)
             (unless (gethash address (heap-graph-numbers graph))
               (heap-graph-add-node graph address)))
           ;; Traverse each node, including the nodes that traversing adds.
           (loop for number from 0
                 while (< number (heap-graph-size graph))
                 do (let ((start (heap-graph-nedges graph))
                          (address (aref (heap-graph-addresses graph) number)))
                      (when (= number (length (heap-graph-starts graph)))
                        (setf (heap-graph-starts graph)
                              (grow-vector (heap-graph-starts graph))))
                      (setf (aref (heap-graph-starts graph) number) start)
                      ;; Not every object whose type has a tp_traverse
                      ;; function may be traversed, e.g., static types.
                      (when (|%PyObject_IS_GC| address)
                        (let* ((type (cffi:mem-ref (cffi:make-pointer address)
                                                   :uintptr +pyobject-type-offset+))
                               (traverse (heap-graph-traverse-function graph type)))
                          (unless (zerop traverse)
                            (cffi:foreign-funcall-pointer
                             (cffi:make-pointer traverse) ()
                             :uintptr address
                             :pointer (cffi:callback heap-graph-visit)
                             :pointer (cffi:null-pointer)
                             :int)
                            (when (member type frame-types)
                              (heap-graph-remove-duplicate-edges graph start)))))))
           ;; Mark the end of the edges of the last node.
           (let ((size (heap-graph-size graph)))
             (when (= size (length (heap-graph-starts graph)))
               (setf (heap-graph-starts graph)
                     (grow-vector (heap-graph-starts graph))))
             (setf (aref (heap-graph-starts graph) size)
                   (heap-graph-nedges graph)))
           graph)
      (setf **heap-graph** nil))))

;;; Rootedness

(defun external-reference-counts (graph held-addresses)
  "Returns a vector with the number of references to each node of GRAPH that
come neither from another node nor from a Lisp mirror.  HELD-ADDRESSES are
the addresses of the objects whose Lisp mirror owns one reference."
  (let* ((size (heap-graph-size graph))
         (addresses (heap-graph-addresses graph))
         (edges (heap-graph-edges graph))
         (counts (make-array size :element-type 'fixnum)))
    (dotimes (number size)
      (setf (aref counts number)
            (address-refcount (aref addresses number))))
    (dolist (address held-addresses)
      (decf (aref counts (gethash address (heap-graph-numbers graph)))))
    (dotimes (index (heap-graph-nedges graph))
      (decf (aref counts (aref edges index))))
    counts))

(defun mark-rooted-nodes (graph counts)
  "Returns a bit vector with a one for each node of GRAPH that is reachable
from a root, i.e., from a node whose count of external references is not
zero.  A negative count means that something is wrong with our bookkeeping,
so that node is a root, too."
  (let* ((size (heap-graph-size graph))
         (starts (heap-graph-starts graph))
         (edges (heap-graph-edges graph))
         (marked (make-array size :element-type 'bit :initial-element 0))
         ;; Each node is pushed at most once.
         (stack (make-array size :element-type 'fixnum))
         (pointer 0))
    (declare (fixnum pointer))
    (dotimes (number size)
      (unless (zerop (aref counts number))
        (setf (sbit marked number) 1)
        (setf (aref stack pointer) number)
        (incf pointer)))
    (loop until (zerop pointer) do
      (let ((number (aref stack (decf pointer))))
        (loop for index from (aref starts number) below (aref starts (1+ number))
              for successor = (aref edges index)
              when (zerop (sbit marked successor))
                do (setf (sbit marked successor) 1)
                   (setf (aref stack pointer) successor)
                   (incf pointer))))
    marked))

(defun unmarked-nodes-reaching (graph marked targets)
  "Returns a bit vector with a one for each unmarked node of GRAPH from which
one of the nodes in the list TARGETS can be reached through unmarked nodes."
  (let* ((size (heap-graph-size graph))
         (starts (heap-graph-starts graph))
         (edges (heap-graph-edges graph))
         ;; The predecessors of node J are the elements of PREDECESSORS from
         ;; (AREF PREDECESSOR-STARTS J) below (AREF PREDECESSOR-STARTS (1+ J)).
         (predecessor-starts (make-array (1+ size) :element-type 'fixnum
                                                   :initial-element 0)))
    (flet ((map-unmarked-edges (function)
             (dotimes (number size)
               (when (zerop (sbit marked number))
                 (loop for index from (aref starts number) below (aref starts (1+ number))
                       for successor = (aref edges index)
                       when (zerop (sbit marked successor))
                         do (funcall function number successor))))))
      (declare (dynamic-extent #'map-unmarked-edges))
      (map-unmarked-edges
       (lambda (number successor)
         (declare (ignore number))
         (incf (aref predecessor-starts (1+ successor)))))
      (loop for number from 1 to size do
        (incf (aref predecessor-starts number)
              (aref predecessor-starts (1- number))))
      (let ((predecessors (make-array (aref predecessor-starts size)
                                      :element-type 'fixnum))
            (fill (copy-seq predecessor-starts)))
        (map-unmarked-edges
         (lambda (number successor)
           (setf (aref predecessors (aref fill successor)) number)
           (incf (aref fill successor))))
        (let ((result (make-array size :element-type 'bit :initial-element 0))
              (stack '()))
          (dolist (target targets)
            (when (zerop (sbit result target))
              (setf (sbit result target) 1)
              (push target stack)))
          (loop until (null stack) do
            (let ((number (pop stack)))
              (loop for index from (aref predecessor-starts number)
                      below (aref predecessor-starts (1+ number))
                    for predecessor = (aref predecessors index)
                    when (zerop (sbit result predecessor))
                      do (setf (sbit result predecessor) 1)
                         (push predecessor stack))))
          result)))))

;;; Collecting Garbage

(defun held-addresses ()
  "Returns a list with the address of the Python object of each reference
owned by Lisp.  An address occurs once per reference."
  (with-owned-references-locked
    (loop for index below **owned-reference-count**
          collect (aref **owned-reference-addresses** index))))

(defun python-mirror-addresses ()
  "Returns the addresses of all Python mirrors of Lisp objects."
  (let ((result '()))
    (sb-ext:with-locked-hash-table (*mirror-into-python-table*)
      (maphash (lambda (object pyobject)
                 (declare (ignore object))
                 (push (pyobject-address pyobject) result))
               *mirror-into-python-table*))
    (delete-duplicates result)))

(defun python-mirror-object (address)
  "Returns the Lisp object whose Python mirror is at ADDRESS and true, or NIL
and NIL if there is no such mirror."
  (multiple-value-bind (object presentp)
      (gethash address *mirror-into-lisp-table*)
    (if (and presentp
             (let ((pyobject (gethash object *mirror-into-python-table*)))
               (and pyobject (= (pyobject-address pyobject) address))))
        (values object t)
        (values nil nil))))

(defun detach-mirrors (graph relevant unrooted-mirrors)
  "Perform step 2 of the algorithm described above for the nodes of GRAPH
that have a one in the bit vector RELEVANT.  UNROOTED-MIRRORS is a list of the
numbers of the Python mirrors that aren't rooted.  Returns the weak hash table
of ephemerons, and a list of entries (ADDRESS . WEAK-POINTER) for the Python
mirrors whose table entry was removed."
  (let* ((size (heap-graph-size graph))
         (numbers (heap-graph-numbers graph))
         (starts (heap-graph-starts graph))
         (edges (heap-graph-edges graph))
         (stand-ins (make-array size :initial-element nil))
         (ephemerons (make-hash-table :test 'eq :weakness :key))
         (detached '()))
    (dotimes (number size)
      (when (= 1 (sbit relevant number))
        (setf (aref stand-ins number) (list number))))
    (dotimes (number size)
      (let ((stand-in (aref stand-ins number)))
        (when stand-in
          (setf (gethash stand-in ephemerons)
                (loop for index from (aref starts number) below (aref starts (1+ number))
                      for successor-stand-in = (aref stand-ins (aref edges index))
                      when successor-stand-in
                        collect successor-stand-in)))))
    ;; Each Lisp mirror maps to the stand-in of its Python object.  A mirror
    ;; that has died in a GC since we looked it up is no longer a key.
    (with-owned-references-locked
      (dotimes (index **owned-reference-count**)
        (let* ((address (aref **owned-reference-addresses** index))
               (stand-in (aref stand-ins (gethash address numbers))))
          (when stand-in
            (multiple-value-bind (owner alivep)
                (sb-ext:weak-pointer-value (svref **owned-reference-owners** index))
              (when alivep
                (setf (gethash owner ephemerons) stand-in)))))))
    ;; Detach the Python mirrors last, because this must not be interrupted
    ;; by an error.
    (dolist (number unrooted-mirrors)
      (let ((address (aref (heap-graph-addresses graph) number)))
        (multiple-value-bind (object presentp) (python-mirror-object address)
          (when presentp
            (push object (gethash (aref stand-ins number) ephemerons))
            (remhash object *mirror-into-python-table*)
            (push (cons address (sb-ext:make-weak-pointer object))
                  detached)))))
    (values ephemerons detached)))

(defun reattach-python-mirrors (entries)
  "Restore the table entry of each detached Python mirror whose Lisp object is
still alive, and turn the others into zombies.  Returns the number of
zombies."
  (loop for (address . weak-pointer) in entries
        count (multiple-value-bind (object alivep)
                  (sb-ext:weak-pointer-value weak-pointer)
                (if alivep
                    (progn (setf (gethash object *mirror-into-python-table*)
                                 (cffi:make-pointer address))
                           nil)
                    (setf (gethash address *zombie-mirror-table*) t)))))

(defparameter *stop-the-world-function*
  (cffi:foreign-symbol-pointer "_PyEval_StopTheWorld")
  "A pointer to the private function _PyEval_StopTheWorld, or NIL.  CPython's
own collector uses it in free-threaded builds, and Python 3.14 exports it.")

(defparameter *start-the-world-function*
  (cffi:foreign-symbol-pointer "_PyEval_StartTheWorld")
  "A pointer to the private function _PyEval_StartTheWorld, or NIL.")

(defun call-with-world-stopped (thunk)
  "Call THUNK while no other thread runs Python code.  Unless Python is
free-threaded, holding the global interpreter lock already ensures that.

A paused thread can't attach to the interpreter until the world is started
again, so this also waits until every Lisp thread that holds a Python thread
state, e.g., in WITH-GLOBAL-INTERPRETER-LOCK-HELD, has released it.  THUNK
must not run Python code, because that might wait for a paused thread."
  (if (not *free-threaded-python-p*)
      (funcall thunk)
      (let ((interpreter (|%PyInterpreterState_Get|)))
        (cffi:foreign-funcall-pointer *stop-the-world-function* ()
                                      :pointer interpreter :void)
        (unwind-protect (funcall thunk)
          (cffi:foreign-funcall-pointer *start-the-world-function* ()
                                        :pointer interpreter :void)))))

(defmacro with-world-stopped (&body body)
  `(call-with-world-stopped (lambda () ,@body)))

(defun collect-in-stopped-world ()
  "Perform steps 1 to 4 of the algorithm described above, except for releasing
references, and return a property list of statistics."
  (let* ((start-time (get-internal-real-time))
         (held (held-addresses))
         (mirrors (python-mirror-addresses))
         (graph (snapshot-python-heap (append held mirrors)))
         (numbers (heap-graph-numbers graph))
         (counts (external-reference-counts graph held))
         (marked (mark-rooted-nodes graph counts))
         (unrooted-mirrors
           (loop for address in mirrors
                 for number = (gethash address numbers)
                 when (zerop (sbit marked number))
                   collect number))
         (ephemerons nil)
         (detached '())
         (zombies 0)
         (gc-start-time nil))
    (when unrooted-mirrors
      (let ((relevant (unmarked-nodes-reaching graph marked unrooted-mirrors)))
        (setf (values ephemerons detached)
              (detach-mirrors graph relevant unrooted-mirrors))))
    (setf gc-start-time (get-internal-real-time))
    ;; SBCL scans the control stack conservatively, and the dead frames of
    ;; DETACH-MIRRORS still contain pointers to the objects of detached
    ;; mirrors.  The frames of SB-EXT:GC don't overwrite all of them.
    (sb-sys:scrub-control-stack)
    (unwind-protect (sb-ext:gc :full t)
      (touch ephemerons)
      (setf zombies (reattach-python-mirrors detached)))
    (flet ((seconds (start end)
             (float (/ (- end start) internal-time-units-per-second))))
      (list :python-objects (heap-graph-size graph)
            :references (heap-graph-nedges graph)
            :unrooted (count 0 marked)
            :anomalies (count-if #'minusp counts)
            :detached (length detached)
            :zombies zombies
            :analysis-seconds (seconds start-time gc-start-time)
            :lisp-gc-seconds (seconds gc-start-time (get-internal-real-time))))))

(defun %collect-garbage ()
  "Perform the algorithm described above and return a property list of
statistics.  Must be called while holding the global interpreter lock, or, in
free-threaded builds, a Python thread state."
  (let* (;; References of dead Lisp mirrors would look like roots.
         (released-before (release-dead-references))
         (statistics (with-world-stopped (collect-in-stopped-world)))
         ;; Releasing references may free zombies, so this has to happen
         ;; after they have been created.  It may also run Python code, so it
         ;; can't happen while the world is stopped.
         (released (release-dead-references)))
    (list* :released-before released-before
           :released released
           statistics)))

(defun collect-garbage (&key verbose)
  "Run a full garbage collection in Lisp and Python that also reclaims cycles
of objects that cross the language boundary.  If VERBOSE is true, report
statistics on *TRACE-OUTPUT*.

Python code that runs while such a cycle is being freed, e.g., a __del__
method of one of its objects, gets a ZOMBIE-MIRROR-ERROR when it accesses a
Lisp object that has already been collected."
  (when (and *free-threaded-python-p*
             (not (and *py-refcnt-function*
                       *stop-the-world-function*
                       *start-the-world-function*)))
    (error "COLLECT-GARBAGE needs Python 3.14 or newer in free-threaded builds."))
  #-little-endian
  (error "COLLECT-GARBAGE only supports little-endian platforms.")
  (let ((statistics
          (with-global-interpreter-lock-held
            (prog1 (%collect-garbage)
              ;; Let Python collect the cycles that only the dead Lisp
              ;; mirrors kept alive.  We do this before releasing the global
              ;; interpreter lock, so that other Python code can't reach one
              ;; of these objects through a weak reference, and from there a
              ;; zombie.  CPython clears the weak references to the objects
              ;; it collects before it runs their finalizers.  In
              ;; free-threaded builds, other threads run again before this
              ;; point, so a weak reference can lead them to a zombie.
              (|%PyGC_Collect|)))))
    (when verbose
      (format *trace-output* "~&;; Collected garbage: ~{~(~A~) ~A~^, ~}~%"
              (loop for (key value) on statistics by #'cddr
                    collect key
                    collect value)))
    (values)))
