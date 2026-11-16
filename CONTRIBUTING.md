# Contributing to lang

This document is for people working on lang itself.  For what lang is and how
to use it, see the [README](README.md).

## Status

- **Lisp → Python** works: `lang.python` embeds CPython in SBCL and mirrors
  Python modules as Lisp packages.
- **Python → Lisp** is not usable yet.  The import machinery in
  `lang/lisp/lisp.py` is a stub, and it will require SBCL built as a shared
  library (`libsbcl`).  `lang/internals/gencore.lisp` builds the core for that.

## Prerequisites

- SBCL (developed with 2.5.x) and [Quicklisp](https://www.quicklisp.org/).
- This repository on ASDF's search path, e.g. by symlinking it into
  `~/quicklisp/local-projects/`.
- The Lisp dependencies from `lang/internals/lang.internals.asd` (alexandria,
  cffi, closer-mop, trivial-package-local-nicknames, trivial-garbage).
  Quicklisp fetches them on the first load.
- CPython 3.13 or later as a shared library (`libpython3.14.so` or
  `libpython3.13.so`).  numpy is optional; the README examples use it.

## Selecting the Python Interpreter

`lang/internals/03-foreign.lisp` loads the first of `libpython3.14` and
`libpython3.13` that CFFI can find, then initializes it unless it is already
running.  To use a particular installation (a venv, conda environment, or
uv-managed Python), add its `lib/` directory to
`cffi:*foreign-library-directories*` before loading lang; the README shows how
to do this in `~/.sbclrc`.  To check which interpreter you got:

```lisp
(lang.internals::expr "__import__('sys').version + ' | ' + __import__('sys').prefix")
```

lang doesn't look for free-threaded builds (`libpython3.14t.so`), and it isn't
safe there yet, because its mirror tables rely on the GIL.  To try one anyway,
put a symlink named `libpython3.14.so` to it into a directory of
`cffi:*foreign-library-directories*`.  Some constants are computed at compile
time, so give each Python version its own fasl directory, e.g., with
`asdf:initialize-output-translations`.

Python 3.14 and later check for C stack overflows by comparing the stack
pointer with the bounds that `pthread_getattr_np` reports for the current
thread.  On x86-64 Linux, SBCL runs its main thread on a control stack that it
allocates itself, so these bounds are wrong there: Python 3.14.0 aborts with
"Unrecoverable stack overflow", and later versions corrupt reference counts.
lang therefore passes SBCL's control stack bounds to
`PyUnstable_ThreadState_SetStackProtection` (new in Python 3.14.1) whenever
it acquires the global interpreter lock.  With Python 3.14.0, which lacks that
function, lang signals an error when Python is called from SBCL's main thread,
including when lang is loaded there; load and use it from another thread
instead.  uv's Python 3.14.0 also needs `PYTHONHOME` set to its installation
directory, or it fails with "Failed to import encodings module".

## Tests

```sh
sbcl --non-interactive --eval '(asdf:test-system :lang.python)'
```

This runs the system `lang.python/test`, which is defined in
`lang/python/lang.python.asd` and lives in `lang/python/test/`.  It reports
each failing test and a summary, and exits with a non-zero status if any test
failed.  (Add `--load ~/quicklisp/setup.lisp` before `--eval` if your init file
does not load Quicklisp.)

The tests use a small harness in `harness.lisp` instead of a test framework,
so they have no dependencies beyond lang itself.  Define tests with `deftest`
and check results with `is` and `signals`.  To run tests from a REPL:

```lisp
(asdf:load-system :lang.python/test)
(lang.python.test:run-tests)
```

Reference count tests run a full GC and `release-dead-references` before
comparing counts, because the Lisp mirror of a Python object owns a reference
until it has been collected.  SBCL also calls `release-dead-references` after
each GC, but in its finalizer thread, so that release isn't synchronous.
Probe with objects you create yourself: many builtin Python objects are
immortal, and their reference counts never change.  The GC may keep the last
temporary object of a loop alive, so `does-not-leak` takes its baseline after
one warm-up iteration and then checks for growth.

The tests of `collect-garbage` check whether objects have been collected, but
SBCL scans stacks conservatively, so a stale pointer on the stack can keep an
object alive.  These tests create and inspect their objects in threads of
their own, whose stacks are gone once they have finished.

## Code Map

The ASDF systems build on each other in this order:

| System           | Location                    | Purpose                                                          |
|------------------|-----------------------------|------------------------------------------------------------------|
| `lang`           | `lang/lang.lisp`            | Maps names like `lang.python.numpy` to generated ASDF systems.  |
| `lang.internals` | `lang/internals/`           | The bridge itself: C API, mirroring, conversion, naming.         |
| `lang.python`    | `lang/python/python.lisp`   | Registers the searcher that turns Python modules into packages.  |
| `lang.lisp`      | `lang/lisp/lisp.lisp`       | Makes Lisp systems available under the `lang.lisp.` prefix.     |

`lang.internals` is `:serial`, so its files load in the order listed in
`lang.internals.asd`.  The most important ones:

- `03-foreign.lisp`: every Python C API function lang uses, in one table of
  `(C-NAME ARGUMENT-TYPES RESULT-TYPE)`.  Each row defines `|%C-NAME|` in the
  package `lang.python.c-api`; these operate on raw pointers and must be
  called with the GIL held.
- `15-c-api.lisp`: the function `|C-NAME|` for most of these, which acquires
  the GIL itself and takes and returns mirror objects.  A compiler macro
  merges nested calls, so that intermediate results are never mirrored.
- `06-mirror-into-lisp.lisp`, `07-mirror-into-python.lisp`: creating the
  object in one language that stands for an object of the other.
- `01-naming.lisp`: translating identifiers between the languages.
- `12-convert.lisp`, `13-lispify.lisp`, `14-pythonize.lisp`: value conversion.
- `16-garbage-collection.lisp`: `collect-garbage`, which also reclaims cycles
  that cross the language boundary.  The comment at the top of the file
  describes the algorithm.
- `10-lambda-lists.lisp` is currently disabled (`#+(or)` in the `.asd`).  It is
  the only user of the `typeshed_client` Python dependency.

The Python side lives in `lang/lang.py` (a meta path finder that dispatches
`lang.FOO.…` imports to the finder registered for language `FOO`) and the
per-language `__init__.py`/`*.py` files.

## Rules for Changing the Bridge

`lang/internals/design.org` describes the naming and translation rules.  In
addition, the following rules about mirrors and reference counts are settled;
changes that break them need a discussion first.

- **Canonical mirrors.**  At any time, an object has at most one live mirror in
  the other language, so that `eq` and `is` behave as expected.  A mirror may
  be collected and recreated later.
- **Mirrors own their targets; tables don't.**  The tables that map objects to
  mirrors never hold a strong reference.  The one exception is `LispType`, the
  metaclass of all mirrored Lisp classes, which lives forever.
- **Every mirror is finalizable.**  All types that mirror Lisp classes are
  instances of `LispType`, so finalization can remove their table entries.
- **Each Lisp mirror owns exactly one reference** to its Python object, unless
  that object is immortal, and records it with `register-owned-reference`.
  Instead of a finalizer per mirror, `release-dead-references` releases the
  references of all collected mirrors under a single acquisition of the GIL
  after each GC.  `collect-garbage` subtracts one reference per entry of that
  registry to find out which Python objects only Lisp keeps alive, so a
  registered reference that isn't owned would make a live object look like
  garbage.
- **Lookup and incref are atomic** under the GIL.
- **Reference ownership is explicit.**  C API declarations in `03-foreign.lisp`
  use the types `pyobject` (borrowed argument), `stolen-pyobject`,
  `new-pyobject`, `borrowed-pyobject`, and their `-or-null` variants.  Use
  `with-new-pyobjects` / `with-pyobjects` for temporaries so that references
  are released on every exit.

Cycles that cross the language boundary (Lisp → Python → Lisp) are only
reclaimed when someone calls `collect-garbage`.  A Python mirror whose Lisp
object it has collected is a zombie until Python frees it, which normally
happens before `collect-garbage` releases the GIL.  Only Python code that runs
while this garbage is being freed, such as a `__del__` method, can reach a
zombie, and mirroring a zombie into Lisp signals a `zombie-mirror-error`.

In free-threaded builds (Python 3.14t or later), `collect-garbage` pauses all
other Python threads with CPython's private `_PyEval_StopTheWorld`, and it only
analyzes the part of the Python heap that is reachable from Lisp.  Other
threads run again before Python frees the garbage, so there a weak reference
can lead them to a zombie.
