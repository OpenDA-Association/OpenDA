#
#
# Makefile for EFDC fortran DLL/SO an its test
#
#

BIN_DIR=../bin

all:
	if [ ! -d "$(BIN_DIR)" ]; then \
		mkdir $(BIN_DIR); \
	fi
	(cd efdc_fortran_dll ; make)
#	(cd efdc_fortran_dll_test ; make)

openmp:
	if [ ! -d "$(BIN_DIR)" ]; then \
		mkdir $(BIN_DIR); \
	fi
	(cd efdc_fortran_dll ; make openmp)

install:
	(cd efdc_fortran_dll ; make install)

clean:
	(cd efdc_fortran_dll ; make clean)
#	(cd efdc_fortran_dll_test ; make clean)

clobber:
	(cd efdc_fortran_dll ; make clobber)
#	(cd efdc_fortran_dll_test ; make clobber)

