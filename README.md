# lang

The **lang** module/system/library can be loaded from multiple programming
languages to seamlessly integrate all the other supported ones.  The currently
supported languages are Python and Lisp.

## Installation

Each language requires one or more auxiliary libraries to be installed on your system:

### Lisp

Requires libsbcl in your library path.  Either install libsbcl with your
favorite package manager or build and install it yourself.

## Python

Requires libpython3.13 or later in your library path.  To select a particular
python environment, put the following into your ```.sbclrc```:

```lisp
(progn
  (ql:quickload :cffi)
  (pushnew "~/miniforge3/envs/MYENV/lib/"
           (symbol-value (intern "*FOREIGN-LIBRARY-DIRECTORIES*" "CFFI"))
           :test #'string=))
```

## Showcases

### Python from Lisp

```lisp
(in-package #:cl-user)
(asdf:load-system :lang.python)
(sb-ext:add-package-local-nickname '#:python '#:lang.python)

(python:list #(1 2.0 #c(3 4) "5 6"))
 => [1, 2.0, #C(3 4) "5 6"]

(python:tuple (python:map #'class-of *))
 => (<class 'lang.lisp.common-lisp.fixnum'>,
     <class 'lang.lisp.common-lisp.single-float'>,
     <class 'lang.lisp.common-lisp.complex'>,
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

## Next Steps

Things I (or some contributor?) plan to do next, in no particular order.  Some
of these ideas can probably be implemented in a few hours and others will be
near impossible, but it is hard to tell which are which in advance.

### Convert Lisp Packages to Python Modules

We can already convert Python modules into Lisp packages, and all the necessary
machinery is there.  The next step is to turn Lisp packages into Python
modules.  The plan is to fill each resulting Python module according to some
fuzzy heuristics, and give each module `foo` two submodules `foo.__functions__`
and `foo.__values__` that accurately reflect the two Lisp namespaces.  Unless
there is a collision, these submodules have the aliases `foo.functions` and
`foo.values`.  When a Lisp package name contains dots, we turn it into several
nested modules.

A possible next step would be to not just convert packages to modules, but to
turn them into modules directly, i.e., to have Lisp packages inherit from the
Python module type.

### Documentation, Exports

Most of the functionality in this library is not properly exported and
documented.  The interface also needs some curating.  Address this.

### Design Document

Write a document that codifies the rules we have used so far to embed one
language into another.  This document can then be consulted when adding a new
language to keep things consistent.

One example of such a rule is that each object of language A has at exactly one
mirror object in language B so that reference semantics are preserved.
However, there are exceptions to this rule such as that the python class
`object` maps directly to the Lisp class `T` to preserve the lattice property
of each object hierarchy.

Another topic worth writing about in such a document is the mapping of
identifiers in one language to those in another.

The document should also describe the object conversion protocol, and the
decision of why we convert to Common Lisp by default (most flexible
(meta-)object system, no restriction on symbol names, performance, macros).

### Sequence Protocol

Use SBCL's extensible sequence protocol to provide the standard CL sequence
functions for other languages' sequences.

### Number Protocol

Convince the SBCL developers to provide an extensible protocol for numbers, so
that we can finally use the standard functions on matrices, quaternions, and
numbers of other programming languages.

### Zero-copy Python strings.

Right now, Python strings and Lisp strings have to be carefully converted into
each other, typically using UTF8-encoded buffers.  However, I expect that they
both have a very similar internal representation.  If so, we could somehow pass
them by reference.  Might be a stupid idea, though.

### Lambda Lists

Right now, the lambda list of each Python function is `(&rest rest)`.  Use
introspection to make each lambda list resemble that of the corresponding
Python function.  Do the same for the other direction, so that Lisp functions
in Python also display the correct structure of arguments.  Bonus points for
keeping this in sync when functions are redefined.  Extra bonus points for
automatically adding type annotations to each function, e.g.,
`symbol_name(symbol: lang.lisp.common-lisp.symbol)`.

### Auto-install

When loading a module that is not yet installed, the default should be to
download it from the internet using a configurable approach for each language.
Make this mechanism extensible, and also provide an option to not download
anything from the internet.  Write proper error handling to report when any
part of this doesn't work.

### Vectorcall Everywhere

We already use Python's fast vectorcall calling convention in some places, but
not yet everywhere.  Address this.

Bonus point for not using vectorcall for functions that actually expect a
Python dictionary of keywords.

Write benchmarks to show whether everything works as intended.

### Test Suite

Write a comprehensive test suite that ensures that everything works as
expected.

One idea would be to use Python's AST module to turn Python code into Lisp code
in the `PYTHON` package and to use that on the test suites of various Python
libraries.

Write tests that check for the presence of memory leaks.

Write tests that use multiple Lisp and Python threads at the same time.

Write tests that perform various consistency checks.

Write tests that detect performance regressions.

### Metaclasses

Python 3.12 adds the C API function PyType_FromMetaclass, so we can finally
specify the correct metaclasses of each Lisp class.  Right now, the type of
each mirrored Lisp class is lang.lisp.LispType.  Use the Python mirror of the
Lisp class's metaclass instead.

### Documentation Mirroring

Automatically convert Lisp docstrings to Python docstrings and vice versa.
Bonus points for figuring out a way that to keep them in sync after a change.
Possible solutions for keeping things in sync are the dependent maintenance
protocol, or a generic writer function with an :after method.

### Displace foreign arrays into Lisp arrays

Make it possible to convert Lisp arrays to Numpy arrays (or others) with
by-reference semantics.  The Lisp array either has to be displaced into a
static vector, or pinned to its memory location.

### Lisp Exceptions as Conditions

Make each Python condition a subclass of `serious-condition`, and arrange that
calls between Python and Lisp propagate these conditions correctly.  Bonus
points for writing a Swank plugin that displays the correct stack trace
containing both Python and Lisp stack frames.

### Performance Improvements

The goal is to make the `PYTHON` package of Common Lisp faster than cpython :)
The main challenge is probably the overhead of cross-language calls, which can
be discussed with the SBCL developers.  On the plus side, we have native
compilation, and could even compile the bytecode of existing Python functions
to Lisp on the fly.

### Cross-language Garbage Collection

Neither garbage collector can reclaim a cycle that crosses the language
boundary, such as a Lisp object that references a Python list that contains
the Lisp object.  `lang.internals:collect-garbage` reclaims such cycles.  It
determines which Python objects are only kept alive by Lisp, describes that
part of the Python heap to SBCL as ephemerons, and lets a full SBCL GC decide.
It has to be called explicitly, and it stops the world for a full collection
in both languages.  What's left to do:

- Run it automatically, e.g., after every few full SBCL GCs, or when many
  mirrors have been created since the last run.
- Make lang safe on free-threaded Python builds.  `collect-garbage` already
  works with Python 3.14t, but the mirror tables still rely on the GIL, and
  reference counts go wrong for Python code that runs on SBCL's main thread.
- Make the heap walk faster.  It takes about 0.2 seconds per million Python
  objects, four times as long as Python's own `gc.collect()`, and most of that
  time goes into hash table lookups.
- Support more than two languages, e.g., by describing the heaps of all other
  languages to SBCL.

### New Language: APL

Use the April APL compiler to implement `lang.apl`.

### New Language: Forth

### New Language: Lua

### New Language: R

### New Language: Ruby

### New Languages: Rust, C, C++, Fortran

Use LLVM to support all these languages.

### New Language: Scheme

Could we turn SBCL into a Scheme compiler?  The main challenge is probably to
implement first-class continuations.  Sounds like a task for Kartik :)

### New Languages: Java, Scala, Kotlin, Clojure

Make it possible to run JVM languages.  One direction could be solved by
implementing the JVM in Common Lisp.  The other direction could be solved by
porting `lang` to ABCL.  This task is a lot of work, but also extremely
rewarding.

### New Languages: C#, F#

Make it possible to run .NET languages.  Approach would be similar to Java.
