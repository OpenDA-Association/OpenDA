# $Id: Makefile 252 2013-06-10 10:07:22Z larsnerger $

#######################################################
# Generic Makefile to build library of SANGOMA tools  #
#######################################################

.SUFFIXES: .F90 .o

#######################################################
### COMPILERS, ETC

# Compiler, Linker, and Archiver
FC = gfortran

# Optimization specs for compiler
FFLAGS = -O3 -fdefault-real-8 

# Module files are in ../..
FFLAGS += -I../..

# Setting for the SANGOMA tools library
LIB_SANGOMA = -L../../ -lsangoma_tools

# Settings for Libraries
LIBS = $(LIB_SANGOMA) -llapack -lblas 

# Archiver
AR = ar

# VERSION of SANGOMA release
VERSION = 0.1

#######################################################
### OBJECTS


MODULE_BASE =	../../sangoma_base.o

OBJECTS_POD =	example_PODcostgrad.o
OBJECTS_TEST_UTILS =	test_sangoma_utils.o


#########################################################
# Compile targets

all: example_podcostgrad test_sangoma_utils

example_podcostgrad : sangoma_lib $(OBJECTS_POD)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(CPP_DEFS) $(OBJECTS_POD) $(LIBS)

test_sangoma_utils : sangoma_lib $(OBJECTS_TEST_UTILS)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(CPP_DEFS) $(OBJECTS_TEST_UTILS) $(LIBS)


sangoma_lib:
	@echo === Build SANGOMA tools library ===
	cd ../../; make


###################################################

clean:
	@rm -f *.o *.mod *.a example_podcostgrad test_sangoma_utils

clean_lib:
	cd ../../; make clean

cleanall: clean clean_lib

test:
	./test_sangoma_utils

###################################################

.F90.o :
	$(FC) $(FFLAGS) $(CPP_DEFS) $(INCS) -c $*.F90 -o $*.o

