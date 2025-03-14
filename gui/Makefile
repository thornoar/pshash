export ROOT_DIR=${PWD}
UNAME := $(shell uname)

# make sure you build parent project first or it won't find the libraries

all: ffitest

ffitest: ffitest.cpp
# set linker path on linux
# not sure why I need clang++ or -stdlib=libc++ but it fixes some missing symbol errors
ifeq ($(UNAME), Linux)
	clang++ -g -Wall ffitest.cpp -o $@ \
	-stdlib=libc++ \
	-Wl,-rpath='$$ORIGIN' \
	-lpshashapi \
	-L./
else
	g++ -g -Wall ffitest.cpp -o $@ \
	-I../csrc \
	-lpotato \
	-L./
endif
# set rpath path on mac, see https://medium.com/@donblas/fun-with-rpath-otool-and-install-name-tool-e3e41ae86172
ifeq ($(UNAME), Darwin)
	install_name_tool -add_rpath @executable_path/. ffitest
endif


run: ffitest
	./ffitest

clean:
	rm -rf *.o *.a *.dSYM *.dylib ffitest

.PHONY: clean all run
