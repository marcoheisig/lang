(cl:in-package #:cl-user)

(defpackage #:python
  (:use)
  (:export
   ;; A
   #:abs
   #:aiter
   #:all
   #:anext
   #:any
   #:ascii
   ;; B
   #:bin
   #:bool
   #:breakpoint
   #:bytearray
   #:bytes
   ;; C
   #:call
   #:callable
   #:chr
   #:classmethod
   #:compile
   #:complex
   ;; D
   #:delattr
   #:dict
   #:dir
   #:divmod
   ;; E
   #:enumerate
   #:eval
   #:exec
   ;; F
   #:filter
   #:float
   #:format
   #:frozenset
   ;; G
   #:getattr
   #:globals
   ;; H
   #:hasattr
   #:hash
   #:help
   #:hex
   ;; I
   #:id
   #:import
   #:input
   #:int
   #:isinstance
   #:issubclass
   #:iter
   ;; L
   #:len
   #:list
   #:locals
   ;; M
   #:map
   #:max
   #:memoryview
   #:min
   ;; N
   #:next
   #:none
   ;; O
   #:object
   #:oct
   #:open
   #:ord
   ;; P
   #:pow
   #:print
   #:property
   ;; R
   #:range
   #:repr
   #:reversed
   #:round
   ;; S
   #:set
   #:setattr
   #:slice
   #:sorted
   #:staticmethod
   #:str
   #:sum
   #:super
   ;; T
   #:tuple
   #:type
   ;; V
   #:vars
   ;; Z
   #:zip))

(defpackage #:sbclmodule
  (:use #:closer-common-lisp)
  (:export
   #:python-object-pointer
   #:python-object-refcount
   #:python-object-attribute
   #:python-object-representation
   #:python-object-string))
