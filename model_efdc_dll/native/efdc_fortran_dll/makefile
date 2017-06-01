#
#
# Makefile for EFDC fortran DLL/SO an its test
#
#
all:
	(cd original_efdc_files ; make)
	(cd openDA_wrapper ; make)

debug:
	(cd original_efdc_files ; make debug)
	(cd openDA_wrapper ; make debug)

openmp:
	(cd original_efdc_files ; make openmp)
	(cd openDA_wrapper ; make openmp)

install:
	(cd openDA_wrapper ; make install)

clean:
	(cd original_efdc_files ; make clean)
	(cd openDA_wrapper ; make clean)

clobber:
	(cd original_efdc_files ; make clobber)
	(cd openDA_wrapper ; make clobber)
