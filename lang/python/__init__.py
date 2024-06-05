import importlib.abc
import importlib.machinery
import importlib.util
import lang

###############################################################################
###
### Python Modules with 'lang.python' Prefix
###
### The goal of this file is to make Python modules available relative to the
### lang.python module, so that 'import foo' and 'import lang.python.foo'
### become synonymous.

__all__ = []
__dict__ = {}


class PythonLoader(importlib.abc.Loader):
    _loader: importlib.abc.Loader

    def __init__(self, _loader: importlib.abc.Loader):
        super().__init__()
        self._loader = _loader

    def create_module(self, spec):
        module = self._loader.create_module(spec)
        name = module.__name__
        if not ('.' in name):
            __all__.append(name)
            __dict__[name] = module
        return module

    def exec_module(self, module):
        return self._loader.exec_module(module)


class PythonFinder(importlib.abc.MetaPathFinder):

    def find_spec(self, fullname, path, target = None):
        _, _ = path, target
        prefix = "lang.python."
        assert fullname.startswith(prefix)
        module_name = fullname[len(prefix):]
        module_spec = importlib.util.find_spec(module_name)
        if module_spec is None:
            return None
        loader = module_spec.loader
        if loader is None:
            return None
        module_spec.loader = PythonLoader(loader)
        return module_spec


lang.register_language('python', PythonFinder())
