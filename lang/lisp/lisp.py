import importlib.abc
import importlib.machinery
import importlib.util
import lang.internals

###############################################################################
###
### Lisp Packages as Python Modules
###
### The goal of this file is to make Lisp packages available as Python modules,
### by means of a finder that queries ASDF, and a loader that loads the ASDF
### system and makes it available as a Python module.

__all__ = []
__dict__ = {}


class LispLoader(importlib.abc.Loader):

    def __init__(self, _loader: importlib.abc.Loader):
        super().__init__() # TODO

    def create_module(self, spec):
        pass # TODO

    def exec_module(self, module):
        pass # TODO


class LispFinder(importlib.abc.MetaPathFinder):

    def find_spec(self, fullname, path, target = None):
        _, _ = path, target
        prefix = "lang.lisp."
        assert fullname.startswith(prefix)
        module_name = fullname[len(prefix):]
        pass # TODO


lang.register_language('lisp', LispFinder())
