#
#
# Makefile for simple fortran DLL
#
#-----	Known suffixes  ---------------------------------------------
#
.SUFFIXES: .f90 .for .o


#-----	Fortran Compiler Settings  ---------------------------------------------
# gfortran compiler
MAKE_FF = gfortran
MAKE_FFLAGS_SPECIFIC = -I../efdc_fortran_dll/openDA_wrapper -I../efdc_fortran_dll/original_efdc_files
# ifort compiler
#MAKE_FF = ifort
#MAKE_FFLAGS_SPECIFIC =  -fpp -CB -I../efdc_fortran_dll/ -I../efdc_fortran_dll/original_files

MAKE_FOPTIBUG = -g 		# debug
#MAKE_FOPTIBUG = -O   	# optimize

MAKE_FFLAGS = \
    $(MAKE_FFLAGS_SPECIFIC) \
    $(MAKE_FOPTIBUG) \

#-----	Link Settings  --------------------------------------------------

MAKE_EXE_OPTIONS_SPECIFIC =  # gfortran
MAKE_EXE_OPTIONS = -lEFDC -L../../native_bin/linux64_gnu/lib -lm -v

MAKE_EXE =  $(MAKE_FF) 

#-----	Source and binary names  ----------------------------------------
BINDIR = .

WRAPPERTEST = ensemble_simulation

SRC_F90_WRAPPER = \
	ensemble_simulation.f90 \

OBJS_WRAPPER = \
	$(SRC_F90_WRAPPER:.f90=.o) \

#-----	Targets  ---------------------------------------------------------------

all: $(WRAPPERTEST)

$(WRAPPERTEST): $(OBJS_WRAPPER) makefile
	@echo Creating test exe $(WRAPPERTEST)
	@rm -f $(BINDIR)/$(WRAPPERTEST)
	$(MAKE_EXE) $(MAKE_EXE_OPTIONS) -o $(WRAPPERTEST)  $(OBJS_WRAPPER)

clean:
	@rm -f \
	    *.o \
	    *.mod \

clobber:
	@make clean
	@rm -f $(WRAPPERTEST)

%.o: %.for
	$(MAKE_FF) -c $(MAKE_FFLAGS) $<

%.o: %.f90   
	$(MAKE_FF) -c $(MAKE_FFLAGS) $<

