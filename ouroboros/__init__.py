from ctypes import CDLL, RTLD_GLOBAL, c_char_p
import os.path
import pathlib
import subprocess

moduledir = pathlib.Path(__file__).resolve().parent
gencore_path = pathlib.Path(moduledir, "gencore.lisp")
core_path = pathlib.Path(moduledir, "ouroboros.core")
quicklisp_path = pathlib.Path("~/quicklisp/setup.lisp").expanduser()

assert gencore_path.exists()
assert quicklisp_path.exists()

if ((not core_path.exists()) or
    # Ensure the core is newer than gencore.lisp
    (os.path.getmtime(core_path) < os.path.getmtime(gencore_path))):
    # (Re)generate the core file
    subprocess.run(
        ['sbcl',
         '--eval', f'(load "{quicklisp_path}")',
         '--eval', f'(defparameter cl-user::*ouroboros-core* "{core_path}")',
         '--script', str(gencore_path)])

libsbcl = CDLL("libsbcl.so", mode=RTLD_GLOBAL)

def initialize(args):
    argc = len(args)
    argv = (c_char_p * argc)(*[c_char_p(arg.encode('utf-8')) for arg in args])
    libsbcl.initialize_lisp(argc, argv)

initialize(["", "--core", str(core_path), "--noinform"])
