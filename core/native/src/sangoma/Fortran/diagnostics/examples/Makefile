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
FFLAGS = -O3 -fdefault-real-8 -fbacktrace -I../../ -I../

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

OBJECTS_ENSSTATS	=	example_ComputeEnsStats.o 

OBJECTS_INVSTATS        =        example_ComputeInvStats.o 

OBJECTS_PVALUE      =         example_checkPvalue.o                                

OBJECTS_HIST	=	example_ComputeHistogram.o 
 
OBJECTS_SENS	=	example_ComputeSensitivity.o \
			obs_op_callback.o

OBJECTS_MI	=	example_ComputeMutInf.o \
			obs_op_callback.o

OBJECTS_RE	= 	example_ComputeRE.o

OBJECTS_ARM	=	example_arm.o

OBJECTS_BRIER	=	example_ComputeBRIER.o

OBJECTS_CRPS	=	example_ComputeCRPS.o \
			sorting_callback.o

OBJECTS_CRIGN	=	example_computeCRIGN.o \
			sorting_callback.o

OBJECTS_RCRV	=	example_ComputeRCRV.o

OBJECTS_WHITE	=	example_checkwhiteness.o

OBJECTS_ESS    =        example_ComputeEffsample.o

OBJECTS_Desrozier    =   example_ComputeDes.o \
                        diag_R_callback.o \
                        obs_op_callback.o

OBJECTS_ENSSPREAD   =	example_CheckEnsSpread.o

OBJECTS_ADT   =	example_checkNormality.o

#########################################################
# Compile targets

all: example_computeEnsStats example_computeHistogram \
	example_computeSens example_computeMutInf  \
         example_checkPvalue example_ComputeInvStats \
	example_arm example_computeRE example_computeBRIER \
	example_computeCRPS example_computeCRIGN \
        example_computeRCRV \
        example_computeDesrozier example_computeEffSample \
	example_CheckWhiteness example_CheckEnsSpread \
	example_checkNormality \
	example_ComputeInvStats example_checkPvalue

example_computeEnsStats : sangoma_lib $(MODULE_BASE) $(OBJECTS_ENSSTATS)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_ENSSTATS) $(LIBS)

example_ComputeInvStats : sangoma_lib $(MODULE_BASE) $(OBJECTS_INVSTATS)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_INVSTATS) $(LIBS)

example_checkPvalue : sangoma_lib $(MODULE_BASE) $(OBJECTS_PVALUE)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_PVALUE) $(LIBS)

example_computeHistogram : sangoma_lib $(MODULE_BASE) $(OBJECTS_HIST)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_HIST) $(LIBS)

example_computeSens : sangoma_lib $(MODULE_BASE) $(OBJECTS_SENS)  
	$(FC) $(FFLAGS) -o $@ $(MODULE_BASE) $(OBJECTS_SENS) $(LIBS)

example_computeMutInf : sangoma_lib $(MODULE_BASE) $(OBJECTS_MI)  
	$(FC) $(FFLAGS) -o $@ $(MODULE_BASE) $(OBJECTS_MI) $(LIBS)

example_arm : sangoma_lib $(MODULE_BASE) $(OBJECTS_ARM)  
	$(FC) $(FFLAGS) -o $@ $(MODULE_BASE) $(OBJECTS_ARM) $(LIBS)

example_computeRE : sangoma_lib $(MODULE_BASE) $(OBJECTS_RE)  
	$(FC) $(FFLAGS) -o $@ $(MODULE_BASE) $(OBJECTS_RE) $(LIBS)

example_computeBRIER : sangoma_lib $(MODULE_BASE) $(OBJECTS_BRIER)  
	$(FC) $(FFLAGS) -o $@ $(MODULE_BASE) $(OBJECTS_BRIER) $(LIBS)

example_computeCRPS : sangoma_lib $(MODULE_BASE) $(OBJECTS_CRPS)  
	$(FC) $(FFLAGS) -o $@ $(MODULE_BASE) $(OBJECTS_CRPS) $(LIBS)

example_computeCRIGN : sangoma_lib $(MODULE_BASE) $(OBJECTS_CRIGN)  
	$(FC) $(FFLAGS) -o $@ $(MODULE_BASE) $(OBJECTS_CRIGN) $(LIBS)

example_computeRCRV : sangoma_lib $(MODULE_BASE) $(OBJECTS_RCRV)  
	$(FC) $(FFLAGS) -o $@ $(MODULE_BASE) $(OBJECTS_RCRV) $(LIBS)

example_CheckWhiteness : sangoma_lib $(MODULE_BASE) $(OBJECTS_WHITE)  
	$(FC) $(FFLAGS) -o $@ $(MODULE_BASE) $(OBJECTS_WHITE) $(LIBS)

example_computeEffSample : sangoma_lib $(MODULE_BASE) $(OBJECTS_ESS)
	$(FC) $(FFLAGS) -o $@ $(MODULE_BASE) $(OBJECTS_ESS) $(LIBS)

example_computeDesrozier : sangoma_lib $(MODULE_BASE) $(OBJECTS_Desrozier)
	$(FC) $(FFLAGS) -o $@ $(MODULE_BASE) $(OBJECTS_Desrozier) $(LIBS)

example_CheckEnsSpread : sangoma_lib $(MODULE_BASE) $(OBJECTS_ENSSPREAD)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_ENSSPREAD) $(LIBS)

example_checkNormality : sangoma_lib $(MODULE_BASE) $(OBJECTS_ADT)
	$(FC) -o $@ $(FFLAGS) $(MODULE_BASE) $(OBJECTS_ADT) $(LIBS)

sangoma_lib:
	@echo === Build SANGOMA tools library ===
	cd ../../; make

###################################################

clean:
	@rm -f *.o *.mod *.a example_computeEnsStats example_computeHistogram\
		example_computeSens example_computeMutInf example_arm \
		example_computeRE example_computeBRIER examplt_computeCRIGN \
                example_computeRCRV example_computeCRPS \
		example_computeEffSample example_computeDesrozier \
		example_arm_output example_CheckWhiteness example_CheckEnsSpread \
		example_ComputeInvStats example_checkPvalue\
		example_checkNormality example_arm_output--*

clean_lib:
	cd ../../; make clean

cleanall: clean clean_lib

###################################################

.F90.o :
	$(FC) $(FFLAGS) $(CPP_DEFS) -c $*.F90 -o $*.o

%.o:	%.mod
