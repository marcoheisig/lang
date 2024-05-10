SBCL_SRC=/home/marcoheisig/userdata/proj/sbcl

.PHONY: all clean

all: libsbcl.so sbclmodule.core sbclmodule.dylib

libsbcl.so: $(SBCL_SRC)
	test ! -e $(SBCL_SRC)/src/runtime/libsbcl.so && cd $(SBCL_SRC) && ./make-shared-library.sh || true
	cp $(SBCL_SRC)/src/runtime/libsbcl.so ./

sbclmodule.dylib: sbclmodule.c libsbcl.so
	$(CC) -shared -o $@ sbclmodule.c -L$(SBCL_SRC)/src/runtime -lsbcl

sbclmodule.exe: sbclmodule.c libsbcl.so
	$(CC) -o $@ sbclmodule.c -L$(SBCL_SRC)/src/runtime -lsbcl

sbclmodule.core: sbclmodule.lisp Makefile.lisp
	$(SBCL_SRC)/run-sbcl.sh --script "Makefile.lisp"

clean:
	rm -f libsbcl.so sbclmodule.core sbclmodule.so
