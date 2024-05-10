#define PY_SSIZE_T_CLEAN
#include "python3.10/Python.h"

extern int initialize_lisp(int argc, char** argv);

int (*sbclmodule_apply)(PyObject* fn, PyObject* args);

int PyInit_sbclmodule(void)
{
    char *argv[4];
    argv[0] = "./sbclmodule.so";
    argv[1] = "--core";
    argv[2] = "./sbclmodule.core";
    argv[3] = "--noinform";
    initialize_lisp(4, argv);
    return 0;
}
