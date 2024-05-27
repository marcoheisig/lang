(cl:in-package #:cl-user)

(progn
  (defpackage #:ouroboros.python
    #1=
    (:export
     ;; Keywords
     #:and
     #:as
     #:assert
     #:async
     #:await
     #:break
     #:class
     #:continue
     #:def
     #:del
     #:elif
     #:else
     #:except
     #:false
     #:finally
     #:for
     #:from
     #:global
     #:if
     #:import
     #:in
     #:is
     #:lambda
     #:none
     #:nonlocal
     #:not
     #:or
     #:pass
     #:raise
     #:return
     #:true
     #:try
     #:while
     #:with
     #:yield
     ;; Miscellaneous
     #:...
     #:_
     #:__debug__
     #:case
     #:match
     #:module
     #:NotImplemented
     #:Ellipsis
     ;; Operators
     #:+
     #:-
     #:*
     #:**
     #:/
     #://
     #:%
     #:@
     #:<<
     #:>>
     #:&
     #:\|
     #:^
     #:~
     #:\:=
     #:<
     #:>
     #:<=
     #:>=
     #:==
     #:!=
     ;; Operator Macros
     #:+=
     #:-=
     #:*=
     #:/=
     #://=
     #:%=
     #:@=
     #:&=
     #:\|=
     #:^=
     #:>>=
     #:<<=
     #:**=
     ;; Exceptions
     #:base-exception
     #:base-exception-group
     #:exception
     #:generator-exit
     #:keyboard-interrupt
     #:system-exit
     #:arithmetic-error
     #:assertion-error
     #:attribute-error
     #:buffer-error
     #:eof-error
     #:import-error
     #:lookup-error
     #:memory-error
     #:name-error
     #:os-error
     #:reference-error
     #:runtime-error
     #:stop-async-iteration
     #:stop-iteration
     #:syntax-error
     #:system-error
     #:type-error
     #:value-error
     #:warning
     #:floating-point-error
     #:overflow-error
     #:zero-division-error
     #:bytes-warning
     #:deprecation-warning
     #:encoding-warning
     #:future-warning
     #:import-warning
     #:pending-deprecation-warning
     #:resource-warning
     #:runtime-warning
     #:syntax-warning
     #:unicode-warning
     #:user-warning
     #:blocking-io-error
     #:child-process-error
     #:connection-error
     #:file-exists-error
     #:file-not-found-error
     #:interrupted-error
     #:is-a-directory-error
     #:not-a-directory-error
     #:permission-error
     #:process-lookup-error
     #:timeout-error
     #:indentation-error
     #:index-error
     #:key-error
     #:module-not-found-error
     #:not-implemented-error
     #:recursion-error
     #:unbound-local-error
     #:unicode-error
     #:broken-pipe-error
     #:connection-aborted-error
     #:connection-refused-error
     #:connection-reset-error
     #:tab-error
     #:unicode-decode-error
     #:unicode-encode-error
     #:unicode-translate-error
     #:exception-group
     #:environment-error
     #:io-error)
    #2=
    (:export
     ;; Built-in Functions
     #:abs
     #:aiter
     #:all
     #:anext
     #:any
     #:ascii
     #:bin
     #:bool
     #:breakpoint
     #:bytearray
     #:bytes
     #:callable
     #:chr
     #:classmethod
     #:compile
     #:complex
     #:delattr
     #:dict
     #:dir
     #:divmod
     #:enumerate
     #:eval
     #:exec
     #:filter
     #:float
     #:format
     #:frozenset
     #:getattr
     #:globals
     #:hasattr
     #:hash
     #:help
     #:hex
     #:id
     #:input
     #:int
     #:isinstance
     #:issubclass
     #:iter
     #:len
     #:list
     #:locals
     #:map
     #:max
     #:memoryview
     #:min
     #:next
     #:object
     #:oct
     #:open
     #:ord
     #:pow
     #:print
     #:property
     #:range
     #:repr
     #:reversed
     #:round
     #:set
     #:setattr
     #:slice
     #:sorted
     #:staticmethod
     #:str
     #:sum
     #:super
     #:tuple
     #:type
     #:vars
     #:zip
     #:__import__))

  (defpackage #:ouroboros.python.builtins
    (:use #:ouroboros.python)
    #2#)

  (defpackage #:ouroboros
    (:use #:closer-common-lisp #:ouroboros.python)
    ;; Conflicting Symbols
    (:shadow
     #:compile
     #:import
     #:eval
     #:float
     #:break
     #:and
     #:open
     #:or
     #:<=
     #:>=
     #:<
     #:>
     #:/=
     #:+
     #:++
     #:+++
     #:-
     #:*
     #:**
     #:***
     #:/
     #://
     #:///
     #:round
     #:if
     #:assert
     #:abs
     #:type
     #:min
     #:max
     #:print
     #:case
     #:complex
     #:continue
     #:lambda
     #:return
     #:not
     #:set
     #:format
     #:list
     #:class
     #:map
     #:warning
     #:type-error
     #:arithmetic-error)
    ;; Export all Python symbols
    #1#
    #2#
    ;; Export all but a few Common Lisp symbols
    #.
    `(:export
      ,@(let ((exclude '(/// *** ++ +++)))
          (loop for symbol being the external-symbols of "CL"
                unless (member symbol exclude)
                  collect symbol)))
    ;; Export Ouroboros-specific symbols
    (:export
     ;; convert.lisp
     #:convert
     #:convert-object
     #:convert-object-to-dummy
     #:convert-slot
     #:register-converted-object
     #:finalize-conversion
     #:slot-conversion-original-value
     #:slot-conversion-converted-value
     #:slot-conversion-specifier
     #:convert-once
     #:convert-tree
     #:convert-graph
     ;; lispify.lisp
     #:lispify-once
     #:lispify-tree
     #:lispify-graph
     ;; pythonize.lisp
     #:pythonize-once
     #:pythonize-tree
     #:pythonize-graph))

  (defpackage #:ouroboros-user
    (:use #:ouroboros))

  (defpackage #:ouroboros.internals
    (:local-nicknames
     (#:python #:ouroboros.python))
    (:use #:closer-common-lisp)))

