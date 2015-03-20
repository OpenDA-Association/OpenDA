#
#
# Makefile for simple fortran DLL
#
#-----	Known suffixes  ---------------------------------------------
#
.SUFFIXES: .f90 .o


#-----	Fortran Compiler Settings  ---------------------------------------------

#MAKE_FF = gfortran		# gfortran compiler
MAKE_FF = ifort			# ifort compiler

MAKE_FOPTIBUG = -g		# debug
#MAKE_FOPTIBUG = -O   	# optimize

#MAKE_FFLAGS_SPECIFIC = -fPIC -fpp	# ifort
#MAKE_FFLAGS_SPECIFIC = -fPIC 		# gfortran

MAKE_FFLAGS = \
    $(MAKE_FFLAGS_SPECIFIC) \
    $(MAKE_FOPTIBUG) \

# additional libs (if required)
MAKE_FORTLIBS = \
    #-LadditionalLibDir \
    #-ladditionalLib \


#-----	Source and binary names  ---------------------------------------------------------------

BINDIR = .

EXE = $(BINDIR)/pollutionmodel_linux

SRC_F90 = \
	collectOutput.f90 \
     	computeNumberOfData.f90 \
	pollution_model.f90 \
	computeNextTimeStep.f90 \
	inputFileVariables.f90 \
	writeOutput.f90


OBJS = $(SRC_F90:.f90=.o) 

TEST_DATA_BIN_DIR = .

#-----	Targets  ---------------------------------------------------------------

all: $(EXE)

$(EXE): $(SRC_F90)
	@echo Creating executable $(EXE)
	@mkdir -p $(BINDIR)
	@rm -f $(BINDIR)/$(EXE)
	$(MAKE_FF) $(MAKE_FFLAGS) -o $(EXE) $(SRC_F90)

clean:
	@rm -f \
	    *.o \
	    *.mod \

clobber:
	@make clean
	@rm -f $(EXE)

.f90.o: 
	$(MAKE_FF) -c $(MAKE_FFLAGS) $<

