#
#
# Makefile for simple fortran DLL
#
#-----	Known suffixes  ---------------------------------------------
#
.SUFFIXES: .F90 .o


#-----	Fortran Compiler Settings  ---------------------------------------------

MAKE_FF = gfortran		# gfortran compiler
MAKE_FF = ifort			# ifort compiler

MAKE_FOPTIBUG = -g		# debug
MAKE_FOPTIBUG = -O   	# optimize

MAKE_FFLAGS_SPECIFIC = -fPIC -fpp	# ifort
MAKE_FFLAGS_SPECIFIC = -fPIC 		# gfortran

MAKE_FFLAGS = \
    -I../simple_fortran_dll \
    $(MAKE_FFLAGS_SPECIFIC) \
    $(MAKE_FOPTIBUG) \

#-----	Link Settings  --------------------------------------------------

MAKE_EXE_OPTIONS_SPECIFIC =  # ifort
MAKE_EXE_OPTIONS_SPECIFIC =  # gfortran

MAKE_EXE =  $(MAKE_FF) $(MAKE_EXE_OPTIONS_SPECIFIC)


#-----	Source and binary names  ---------------------------------------------------------------

BINDIR = ../../bin

MY_LIBS = \
	-L$(BINDIR) \
	-lsimplefortran \


EXE = $(BINDIR)/simplefortrandlltest

SRC_F90_PP = \
	SimpleFortranDLLTest.F90 \

OBJS = $(SRC_F90_PP:.F90=.o) 


#-----	Targets  ---------------------------------------------------------------

all: $(EXE)

$(EXE): $(OBJS) makefile
	@echo Creating test exe $(EXE)
	@rm -f $(BINDIR)/$(EXE)
	$(MAKE_EXE) $(MAKE_EXE_OPTIONS) -o $(EXE) $(MY_LIBS) $(OBJS)

clean:
	@rm -f \
	    *.o \
	    *.mod \

clobber:
	@make clean
	@rm -f $(EXE)

.F90.o: 
	$(MAKE_FF) -c $(MAKE_FFLAGS) $<

