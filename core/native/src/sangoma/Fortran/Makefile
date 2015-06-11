# $Id: Makefile 627 2015-05-11 11:36:38Z abarth93 $

#######################################################
# Generic Makefile to build library of SANGOMA tools  #
#######################################################

.SUFFIXES: .F90 .o

#######################################################
### COMPILERS, ETC

# Compiler, Linker, and Archiver
FC = gfortran

# Optimization specs for compiler
# and include directory
FFLAGS = -O3 -fdefault-real-8 
# Compile settings for cdflib.f90. This is a Fortran77 library with explicit
# specification of the kind (double precision)
FFLAGS_CDFLIB = -O3 -std=legacy
# Include fftw.f for mod_sangoma_pseudornd2d
# If pkg-config doesn't work, specify the path manually like '_i/sw//include'
#FFLAGS := $(FFLAGS) -I$(shell pkg-config --variable=includedir fftw3)

# Archiver
AR = ar

# Ranlib
RANLIB = ranlib 

# VERSION of SANGOMA release, e.g 1.0 or 1.0-rc
VERSION = 1.0

#######################################################
### OBJECTS

MODULES =       sangoma_base.o

OBJECTS	= 	diagnostics/sangoma_ComputeEnsStats.o \
		diagnostics/sangoma_ComputeHistogram.o \
                diagnostics/sangoma_ComputeInvStats.o \
		diagnostics/sangoma_ComputeMutInf.o \
		diagnostics/sangoma_ComputeSensitivity.o \
		diagnostics/sangoma_ComputeSensitivity_op.o \
		diagnostics/sangoma_ComputeRE.o \
		diagnostics/sangoma_computeCRPS.o \
		diagnostics/sangoma_computeRCRV.o \
		diagnostics/sangoma_computeBRIER.o \
		diagnostics/sangoma_arm.o \
		diagnostics/sangoma_CheckWhiteness.o \
                diagnostics/sangoma_ComputeEffSample.o \
                diagnostics/sangoma_Desrozier.o \
                diagnostics/sangoma_CompareDes.o \
                diagnostics/sangoma_CheckEnsSpread.o \
                diagnostics/sangoma_checkNormality.o \
                diagnostics/sangoma_ComputeCRIGN.o \
		perturbations/sangoma_EOFCovar.o \
		perturbations/sangoma_MVNormalize.o \
		perturbations/sangoma_SampleEns.o \
		utilities/sangoma_computepod.o \
		utilities/sangoma_costgrad.o \
		utilities/sangoma_utils.o \
		analysis/sangoma_ensemble_analysis.o \
		transformations/sangoma_ComputeQuantiles.o \
		transformations/sangoma_Anamorphosis.o
#		perturbations/mod_sangoma_pseudornd.o \

OBJ_CDFLIB = diagnostics/cdflib.o

#########################################################
# Compile rules

libsangoma_tools.a : $(MODULES) $(OBJ_CDFLIB) $(OBJECTS)
	$(AR) -r $(AR_SPEC) $@  $(OBJECTS) $(OBJ_CDFLIB)
	$(RANLIB) $@

clean:
	@rm -f libsangoma_tools.a
	@rm -f $(OBJECTS) $(OBJ_CDFLIB)
	@rm -f sangoma_base.o *.mod

%.o:	%.mod

.F90.o :
	$(FC) $(FFLAGS) $(CPP_DEFS) -c $*.F90 -o $*.o

diagnostics/cdflib.o : diagnostics/cdflib.f90
	$(FC) $(FFLAGS_CDFLIB) $(CPP_DEFS) -c $*.f90 -o $*.o

#########################################################
# Make a new software release.
# svn+ssh://abarth93@svn.code.sf.net/p/sangoma/code/tools/trunk/Fortran

release:
	tmpdir=$${TMPDIR-"/tmp"}/sangoma-$$$$; \
	mkdir $$tmpdir; \
	cd $$tmpdir; \
	svn export https://svn.code.sf.net/p/sangoma/code/tools/trunk sangoma-$(VERSION); \
	perl -pi -e 's/_VERSION_/$(VERSION)/g' sangoma-$(VERSION)/README.html; \
	tar -czvf sangoma-$(VERSION).tar.gz sangoma-$(VERSION); \
	mv sangoma-$(VERSION).tar.gz $(CURDIR); \
	rm -R $$tmpdir

# Upload software release to sourceforge.
# Release will be available at
# https://sourceforge.net/projects/sangoma/files/.

upload:
	scp sangoma-$(VERSION).tar.gz  frs.sourceforge.net:/home/frs/project/sangoma
