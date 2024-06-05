import sys
import importlib.abc
import importlib.machinery
import importlib.util


_lang_loaders : dict[str, importlib.abc.MetaPathFinder] = {}


class LangFinder(importlib.abc.MetaPathFinder):

    def find_spec(self, fullname, path, target = None):
        prefix = "lang."
        if not isinstance(fullname, str):
            return None
        if not fullname.startswith(prefix):
            return None
        rest = fullname[len(prefix):]
        pos = rest.find('.')
        end = len(rest) if pos == -1 else pos
        lang = rest[:end]
        mpf = _lang_loaders.get(lang)
        if mpf is None:
            return None
        return mpf.find_spec(fullname, path, target)


if LangFinder not in sys.meta_path:
    sys.meta_path.append(LangFinder())


def register_language(name: str, mpf: importlib.abc.MetaPathFinder):
    _lang_loaders[name] = mpf

__all__ = ["register_language"]
