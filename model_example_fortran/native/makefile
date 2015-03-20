#
#
# Makefile for simple fortran DLL/SO an its test
#
#

BIN_DIR=../bin

all:
	if [ ! -d "$(BIN_DIR)" ]; then \
		mkdir $(BIN_DIR); \
	fi
	(cd simple_fortran_dll ; make)
	(cd simple_fortran_dll_test ; make)

install:
	(cd simple_fortran_dll ; make install)

clean:
	(cd simple_fortran_dll ; make clean)
	(cd simple_fortran_dll_test ; make clean)

clobber:
	(cd simple_fortran_dll ; make clobber)
	(cd simple_fortran_dll_test ; make clobber)

