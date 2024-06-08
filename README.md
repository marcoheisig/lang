# lang

The **lang** module/system/library can be loaded from multiple programming
languages to seamlessly integrate all the other supported ones.  The currently
supported languages are Python and Lisp.

## Installation

Each language requires one or more auxiliary libraries to be installed on your system:

- **lisp** requires libsbcl in your library path.

- **python** requires libpython3.11 or later in your library path.

## Showcases

### Python from Lisp

```lisp
(in-package #:cl-user)
(asdf:load-system :lang.python)
(sb-ext:add-package-local-nickname '#:python '#:lang.python)

(python:list #(1 2.0 #c(3 4) "5 6"))
 => [1, 2.0, #C(3 4) "5 6"]

(python:tuple (python:map #'class-of *))
 => (<class 'lang.lisp.cl.fixnum'>,
     <class 'lang.lisp.cl.single-float'>,
     <class 'lang.lisp.cl.complex'>,
     <class 'lang.lisp.sb-kernel.simple-character-string'>)

(asdf:load-system :lang.python.numpy)
(sb-ext:add-package-local-nickname '#:np '#:lang.python.numpy)
(np:array #(1 2 3) np:float32)
 => array([1., 2., 3.], dtype=float32)

(python:* * 3)
 => array([3., 6., 9.], dtype=float32)
```

### Lisp from Python

```python
import lang.lisp
```
