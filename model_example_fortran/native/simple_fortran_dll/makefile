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
    $(MAKE_FFLAGS_SPECIFIC) \
    $(MAKE_FOPTIBUG) \

# additional libs (if required)
MAKE_FORTLIBS = \
    #-LadditionalLibDir \
     #-ladditionalLib \


#-----	Link Settings  --------------------------------------------------

MAKE_SO_OPTIONS_SPECIFIC = -shared 	# ifort
MAKE_SO_OPTIONS_SPECIFIC = -shared 	# gfortran

MAKE_SO =  $(MAKE_FF) $(MAKE_SO_OPTIONS_SPECIFIC)


#-----	Source and binary names  ---------------------------------------------------------------

BINDIR = ../../bin

SO = $(BINDIR)/libsimplefortran.so

SRC_F90_PP = \
	SimpleModelInstanceData.F90 \
	SimpleFortranDLL.F90 \

OBJS = $(SRC_F90_PP:.F90=.o) 

TEST_DATA_BIN_DIR = ../../java/test/org/openda/examples/simplef90model/testData/bin
BIN_EXTERNAL_DIR = ../../bin_external

#-----	Targets  ---------------------------------------------------------------

all: $(SO)

$(SO): $(OBJS) makefile
	@echo Creating library $(SO)
	@mkdir -p $(BINDIR)
	@rm -f $(BINDIR)/$(SO)
	$(MAKE_SO) $(MAKE_SO_OPTIONS) -o $(SO) $(OBJS)

install: $(SO)
	@cp $(SO) $(TEST_DATA_BIN_DIR)
	@cp $(SO) $(BIN_EXTERNAL_DIR)

clean:
	@rm -f \
	    *.o \
	    *.mod \

clobber:
	@make clean
	@rm -f $(SO)

.F90.o: 
	$(MAKE_FF) -c $(MAKE_FFLAGS) $<

