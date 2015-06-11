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
FFLAGS = -O3 -fdefault-real-8 -I../../ -I../

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

MODULE_BASE =  ../../sangoma_base.o

OBJECTS_ANAM	=	example_Anamorphosis.o \
			sorting_callback.o

#########################################################
# Compile targets

all: example_Anamorphosis

example_Anamorphosis : sangoma_lib $(MODULE_BASE) $(OBJECTS_ANAM)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_ANAM) $(LIBS)

sangoma_lib:
	@echo === Build SANGOMA tools library ===
	cd ../../; make

###################################################

clean:
	@rm -f *.o *.mod *.a example_Anamorphosis

clean_lib:
	cd ../../; make clean

cleanall: clean clean_lib

###################################################

.F90.o :
	$(FC) $(FFLAGS) $(CPP_DEFS) -c $*.F90 -o $*.o

%.o:	%.mod
