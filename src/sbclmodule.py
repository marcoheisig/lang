import ctypes

sbclmodule = ctypes.CDLL("./sbclmodule.dylib", mode=ctypes.RTLD_GLOBAL)

sbclmodule.PyInit_sbclmodule()
