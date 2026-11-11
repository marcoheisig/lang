(in-package #:lang.internals)

(defgeneric convert (strategy object)
  (:documentation
   "Converts the supplied object, and possibly the objects referenced therein,
using some strategy.  The strategy should be an instance of a class that
describes the exact nature of the conversion, or the name of such a class.")
  (:method ((strategy symbol) object)
    (convert (make-instance strategy) object)))

(defgeneric convert-object (strategy object)
  (:documentation
   "Returns the converted object, and, optionally, a second value that is a thunk
that initializes all references within that converted object."))

;;; Convert Once

(defclass convert-once ()
  ()
  (:documentation
   "Convert the supplied object, but don't convert any of its slots."))

(defvar *convert-once-marker* nil)

(defmethod convert ((strategy convert-once) object)
  (multiple-value-bind (converted thunk)
      (convert-object strategy object)
    (let ((*convert-once-marker* t))
      (when thunk (funcall thunk)))
    converted))

(defmethod convert-object ((strategy convert-once) object)
  (if *convert-once-marker*
      object
      (call-next-method)))

;;; Convert Tree

(defclass convert-tree ()
  ()
  (:documentation
   "Convert the supplied object, its slots, and the slots thereof, but don't
check for circularity or multiply referenced objects."))

(defvar *convert-tree-worklist* nil
  "An adjustable vector that tracks all initialization thunks that have yet to run.")

(defmethod convert ((strategy convert-tree) object)
  (let ((*convert-tree-worklist* (make-array 0 :adjustable t :fill-pointer t)))
    (prog1 (convert-object strategy object)
      (loop until (zerop (length *convert-tree-worklist*)) do
        (funcall (vector-pop *convert-tree-worklist*))))))

;;; Arrange that any thunk returned by CONVERT-OBJECT is pushed to the
;;; worklist.
(defmethod convert-object :around ((strategy convert-tree) object)
  (multiple-value-bind (converted thunk)
      (call-next-method)
    (when thunk (vector-push-extend thunk *convert-tree-worklist*))
    (values converted thunk)))

;;; Convert Graph

(defclass convert-graph ()
  ()
  (:documentation
   "Convert the supplied object and its children in a way that properly handles
 circularity and multiply referenced objects."))

(defvar *convert-graph-conversion-in-progress-marker* nil
  "An opaque object that is used for marking a conversion that is still in
process.")

(defvar *convert-graph-table* nil
  "A hash table, mapping from objects to either the conversion-in-progress marker
or the result of the conversion.")

(defvar *convert-graph-worklist* nil
  "An adjustable vector that tracks all initialization thunks that have yet to run.")

(defmethod convert
    ((strategy convert-graph) object)
  (let ((*convert-graph-conversion-in-progress-marker* (list '.conversion-in-progress.))
        (*convert-graph-table* (make-hash-table))
        (*convert-graph-worklist* (make-array 0 :adjustable t :fill-pointer t)))
    (prog1 (convert-object strategy object)
      (loop until (zerop (length *convert-graph-worklist*)) do
        (funcall (vector-pop *convert-graph-worklist*))))))

(defmethod convert-object :around
    ((strategy convert-graph) object)
  (multiple-value-bind (conversion presentp)
      (gethash object *convert-graph-table*)
    (cond ((eq conversion *convert-graph-conversion-in-progress-marker*)
           (error "~@<Erroneous calls to CONVERT-OBJECT on the same object, ~
                 probably by a method that doesn't take circularity ~
                 into account.~:@>"))
          (presentp conversion)
          (t
           (setf (gethash object *convert-graph-table*)
                 *convert-graph-conversion-in-progress-marker*)
           (multiple-value-bind (conversion thunk)
               (call-next-method)
             (setf (gethash object *convert-graph-table*)
                   conversion)
             (when thunk
               (vector-push-extend thunk *convert-graph-worklist*))
             (values conversion thunk))))))
