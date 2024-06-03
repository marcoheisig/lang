# Seamless Programming LANGuage Interoperability

The **lang** module/system/library can be loaded from multiple programming
languages to seamlessly integrate all the other supported ones.  The currently
supported languages are Python and Lisp.

## Installation

Each language requires one or more auxiliary libraries to be installed on your system:

- **lisp** requires libsbcl in your library path.

- **python** requires libpython3.11 or later in your library path.

## Showcases

### Lisp

```lisp
(in-package #:cl-user)
(sb-ext:add-package-local-nickname '#:python '#:lang.python)

(python:list #(1 2.0 #c(3 4) "5 6"))
 => [1, 2.0, #C(3 4) "5 6"]

(python:tuple (python:map #'class-of *))
 => (<class 'lang.lisp.cl.fixnum'>,
     <class 'lang.lisp.cl.single-float'>,
     <class 'lang.lisp.cl.complex'>,
     <class 'lang.lisp.sb-kernel.simple-character-string'>)

(python:getitem (python:dir 42) (python:slice 5))
 => ['__class__', '__bool__', '__and__', '__add__', '__abs__']

(python:isinstance (python:getitem (python:dir 42) 0) (find-class 'python:str))
 => True

(typep (python:getitem (python:dir 42) 0) 'python:str)
 => t
```
