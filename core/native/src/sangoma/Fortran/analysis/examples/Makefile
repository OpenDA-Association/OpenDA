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

# Octave or Matlab interpreter
# for Octave use e.g.
# OCTAVE = octave --quiet
# for Matlab use e.g.
# OCTAVE = matlab -nodisplay

OCTAVE = octave --quiet

# VERSION of SANGOMA release
VERSION = 0.1

#######################################################
### OBJECTS


MODULE_BASE =	../../sangoma_base.o

OBJECTS_EXAMPLE_ENSEMBLE_ANALYSIS = example_ensemble_analysis.o
OBJECTS_EXAMPLE_LOCAL_ENSEMBLE_ANALYSIS = example_local_ensemble_analysis.o
OBJECTS_TEST_ENSEMBLE_ANALYSIS =	test_ensemble_analysis.o
OBJECTS_TEST_LOCAL_ENSEMBLE_ANALYSIS =	test_local_ensemble_analysis.o


#########################################################
# Compile targets

all: example_ensemble_analysis example_local_ensemble_analysis test_ensemble_analysis test_local_ensemble_analysis

example_ensemble_analysis : sangoma_lib $(OBJECTS_EXAMPLE_ENSEMBLE_ANALYSIS)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(CPP_DEFS) $(OBJECTS_EXAMPLE_ENSEMBLE_ANALYSIS) $(LIBS)

example_local_ensemble_analysis : sangoma_lib $(OBJECTS_EXAMPLE_LOCAL_ENSEMBLE_ANALYSIS)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(CPP_DEFS) $(OBJECTS_EXAMPLE_LOCAL_ENSEMBLE_ANALYSIS) $(LIBS)

test_ensemble_analysis : sangoma_lib $(OBJECTS_TEST_ENSEMBLE_ANALYSIS)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(CPP_DEFS) $(OBJECTS_TEST_ENSEMBLE_ANALYSIS) $(LIBS)

test_local_ensemble_analysis : sangoma_lib $(OBJECTS_TEST_LOCAL_ENSEMBLE_ANALYSIS)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(CPP_DEFS) $(OBJECTS_TEST_LOCAL_ENSEMBLE_ANALYSIS) $(LIBS)

sangoma_lib:
	@echo === Build SANGOMA tools library ===
	cd ../../; make


###################################################

clean:
	@rm -f *.o *.mod *.a example_ensemble_analysis example_local_ensemble_analysis \
	 test_ensemble_analysis test_local_ensemble_analysis

clean_lib:
	cd ../../; make clean

cleanall: clean clean_lib

test:
	./test_ensemble_analysis
	./test_local_ensemble_analysis
	cd ../../../Matlab/analysis; \
	$(OCTAVE) < test_local_ensemble_analysis.m; \
	$(OCTAVE) < test_ensemble_analysis.m \


###################################################

.F90.o :
	$(FC) $(FFLAGS) $(CPP_DEFS) $(INCS) -c $*.F90 -o $*.o

