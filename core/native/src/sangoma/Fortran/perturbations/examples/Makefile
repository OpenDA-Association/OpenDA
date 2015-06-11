# $Id: Makefile 252 2013-06-10 10:07:22Z larsnerger $

#######################################################
# Generic Makefile to build library of SANGOMA tools  #
#######################################################

.SUFFIXES: .F90 .o

#######################################################
### COMPILERS, ETC

# Compiler, Linker, and Archiver
FC =  gfortran

# Optimization specs for compiler
FFLAGS = -O3 -fdefault-real-8 


# Setting for the SANGOMA tools library
LIB_SANGOMA = -L../../ -lsangoma_tools

# Settings for Libraries
# We need lapack, blas and libfftw3; if pkg-config doesn't work, specify the library manually
LIBS = $(LIB_SANGOMA) -llapack -lblas $(shell pkg-config --libs fftw3)

# Specification of include directory
# For fftw.f: If pkg-config doesn't work, specify the path manually like  '-I/sw/include'
INCS = -I../../ -I$(shell pkg-config --variable=includedir fftw3) 

# Archiver
AR = ar

# VERSION of SANGOMA release
VERSION = 0.1

#######################################################
### OBJECTS

MODULE_BASE =  ../../sangoma_base.o

OBJECTS_EOF_UV	=	example_EOFcovar_uv.o

OBJECTS_EOF_MV	=	example_EOFcovar_mv.o 

OBJECTS_ENS_MV	=	example_SampleEns_mv.o 

OBJECTS_ENS	=	example_SampleEns.o 

OBJECTS_MVNORM	=	example_MVNormalize.o 

OBJECTS_PSEUDORND = mod_set_random_seed.o \
      example_pseudornd2D.o

#########################################################
# Compile targets

all: example_eofcovar_uv example_eofcovar_mv example_mvnormalize example_pseudornd2D \
	example_sampleens_mv example_sampleens

example_eofcovar_uv : sangoma_lib $(OBJECTS_EOF_UV)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_EOF_UV) $(LIBS)

example_eofcovar_mv : sangoma_lib $(OBJECTS_EOF_MV)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_EOF_MV) $(LIBS)

example_sampleens_mv : sangoma_lib $(OBJECTS_ENS_MV)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_ENS_MV) $(LIBS)

example_sampleens : sangoma_lib $(OBJECTS_ENS)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_ENS) $(LIBS)

example_mvnormalize : sangoma_lib $(OBJECTS_MVNORM)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_MVNORM) $(LIBS)

example_pseudornd2D: sangoma_lib $(OBJECTS_PSEUDORND)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_PSEUDORND) $(LIBS)

sangoma_lib:
	@echo === Build SANGOMA tools library ===
	cd ../../; make

###################################################

clean:
	@rm -f *.o *.mod *.a example_eofcovar_uv example_eofcovar_mv example_mvnormalize \
		example_pseudornd2D example_sampleens_mv example_sampleens \
		ens_?.txt ensB_?.txt eof_?.txt svals.txt meanstate.txt

clean_lib:
	cd ../../; make clean

cleanall: clean clean_lib

###################################################

.F90.o :
	$(FC) $(FFLAGS) $(CPP_DEFS) $(INCS) -c $*.F90 -o $*.o

