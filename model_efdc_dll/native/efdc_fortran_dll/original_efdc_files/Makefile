#
#
# Makefile for original EFDC files
# Build static library
#

#-----	Fortran Compiler Settings  ---------------------------------------------
# gfortran compiler
MAKE_FF=gfortran

# we want to use linux32 command to simulate 32bit on a 64bit machine
MACH = $(shell arch)
ARCH = 32
MFLAG_ARCH= -m32
ifeq ($(MACH), x86_64)
  ARCH = 64
  MFLAG_ARCH = -m64
endif
ifeq ($(MAKE_FF), ifort)
	F_OPENMP = -openmp
endif
ifeq ($(MAKE_FF), gfortran)
	F_OPENMP = -fopenmp
endif


# MAKE_FDEBUG = -g -fbounds-check -Wall -fbacktrace -finit-real=nan -ffpe-trap=invalid,zero,overflow		# debug
MAKE_FDEBUG = -g -fbounds-check -Wall -fbacktrace -finit-real=nan -ffpe-trap=invalid,zero,overflow		# debug
MAKE_FOPTIMIZE = -O   	# optimize

MAKE_FFLAGS_SPECIFIC = -fPIC -ffixed-line-length-none -ffree-line-length-none # gfortran

MAKE_FFLAGS = \
    $(MAKE_FFLAGS_SPECIFIC) $(MFLAG_ARCH)


#FFLAGS = -ffixed-line-length-none -ffree-line-length-none -fPIC

POBJECTS = CGATEFLX.po RWQC1.po

OBJECTS =  \
ACON.o       CALEXP.o      CALTSXY.o     READWIMS1.o  WQSKE4.o     GATECTLREAD.o  SCANEFDC.o \
VELPLTH.o    DRIFTER.o     SURFPLT.o     WINDWAVE.o   s_sedzlj.o   EEXPOUT.o      RESTOUT.o \
WQ3D.o       s_shear.o     CALHEAT.o     CALPUVTT.o   VARZEROReal.o \
BAL2T5.o     READOIL.o     CALEXP2T0.o \
AINIT.o      CALEXP2T.o    CALUVW.o      DUMP.o       READWIMS2.o  SCANGATECTL.o  SVBKSB.o          WQZERO.o \
BAL2T1.o     CALFQC.o      CALVEGSER.o   RELAX2T.o    SCANGSER.o   SVDCMP.o       WQZERO2.o \
BAL2T2.o     CALHDMF.o     CALWQC.o      RESTIN1.o    SCANGWSR.o   TIMELOG.o      WQZERO3.o \
BAL2T3A.o    CBALEV1.o     FDSTRSE.o     RESTIN10.o   SCANMASK.o   TMSR.o         WQZERO4.o \
BAL2T3B.o    CBALEV2.o     FHYDCN.o      RESTIN2.o    SCANMODC.o   TOXCHEM.o      WSMRST.o  \
BAL2T4.o     CALHTA.o      CBALEV3.o     FSBDLD.o     RESTMOD.o    SCANPSER.o     VALKH.o           WSMTS.o \
CALIMP2T.o   CBALEV4.o     FSEDMODE.o    SCANQCTL.o   VARALLOC.o   WSMTSBIN.o \
CALMMT.o     CBALEV5.o     FSTRSE.o      SCANQSER.o   VARINIT.o    WWQNC.o \
BEDINIT.o    CALPGCORR.o   CBALOD1.o     FUNDEN.o     RESTRAN.o    SCANSEDZLJ.o   VARZEROInt.o      WWQRST.o \
BEDLOAD.o    CALPNHS.o     CBALOD2.o     ROUT3D.o     SCANSFSR.o   WWQTS.o \
BEDPLTH.o    CALPSER.o     CBALOD3.o     RSALPLTH.o   SCANSSER.o   WWQTSBIN.o \
BUDGET1.o    CALPUV2C.o    CBALOD4.o     HDMT.o       RSALPLTV.o   SCANTSER.o     ZBRENT.o \
BUDGET2.o    CALPUV2T.o    CBALOD5.o     HDMT2T.o     RSMICI.o     SCANWQ.o       \
BUDGET3.o    CALPUV9.o     CELLMAP.o     INITBIN3.o   RSMRST.o     SCANWSER.o     VELPLTV.o \
BUDGET5.o    CALPUV9C.o    CELLMASK.o    INITBIN4.o   RSURFPLT.o   SCNTXSED.o     VSFP.o            foodchain.o \
CALAVB.o     CEQICM.o      INITBIN5.o    RVELPLTH.o   SEDFLUX.o    Var_Global_Mod.o  \
CALAVB2.o    INPUT.o       RVELPLTV.o    SEEK.o       WASP4.o      initbin.o \
CALAVBOLD.o  CALQQ1.o      JPEFDC.o      RWQAGR.o     SETBCS.o     WASP5.o        initbin0.o \
CALBAL1.o    CALQQ1OLD.o   CONGRAD.o     LSQHARM.o    RWQATM.o     SETFPOCB.o     WASP6.o           initbin2.o \
CALBAL2.o    CALQQ2.o      CONGRADC.o    LUBKSB.o     RWQBEN2.o    SETOBC2T.o     WASP7.o           s_bedload.o \
CALBAL3.o    CALQQ2T.o     COSTRAN.o     LUDCMP.o          SETOPENBC.o    WASP7EPA.o        s_main.o \
CALBAL4.o    CALQQ2TOLD.o  COSTRANW.o    NEGDEP.o     RWQCSR.o     SETOPENBC2.o   WAVEBL.o          s_morph.o \
CALBAL5.o    CALQVS.o      CSEDRESB.o    OILCHEM.o    RWQICI.o     SETSHLD.o      WAVESXY.o         s_sedic.o \
CALBED.o     CALSED.o      CSEDRESS.o    OUT3D.o      RWQPSL.o     SETSTVEL.o     WELCOME.o         \
CALBED9.o    CALSFT.o      CSEDSET.o     OUTOIL.o     RWQRST.o     SHOWVAL.o      \
CALBLAY.o    CALSND.o      CSEDTAUB.o    OUTPUT1.o    RWQSTL.o     SKIPCOMM.o     \
CALBUOY.o    CALSTEP.o     CSEDTAUS.o    OUTPUT2.o    RWQSUN.o     SMINIT.o       \
CALCONC.o    CALSTEPD.o    CSEDVIS.o     PPLOT.o      SALPLTH.o    SMMBE.o        tecplot.o \
CALCSER.o    CALTBXY.o     CSNDEQC.o     QUIT.o       SALPLTV.o    SMRIN1.o       WQ3DINP.o \
CALDIFF.o    CALTOX.o      CSNDSET.o     RCAHQ.o      SALTSMTH.o   SOLVSMBE.o     WQSKE0.o \
CALDISP2.o   CALTOXB.o     CSNDZEQ.o     SCANASER.o   SCANGTAB.o   SSEDTOX.o    WQSKE1.o \
CALDISP3.o   CALTRAN.o     DEPPLT.o      SCANDSER.o   SUBCHAN.o    WQSKE2.o \
VARALLOC1.o  VARALLOC2.o   VARALLOC3.o   VARALLOC4.o   VARALLOC5.o VARALLOC6.o    VARALLOC7.o  VARALLOC8.o\
CALEBI.o     CALTRANQ.o    READTOX.o     WQSKE3.o     pbm_cut.o    Sub_spore.o   WQSTOKES01.o

COMPAT_OBJS = \
drand.o 

.SUFFIXES:	.for .po
.SUFFIXES:	.f90 .po

all: MAKE_FFLAGS += $(MAKE_FOPTIMIZE)
all:	libEfdcOrig.a

debug: MAKE_FFLAGS += $(MAKE_FDEBUG) 
debug:	libEfdcOrig.a

openmp: MAKE_FFLAGS += $(F_OPENMP)
openmp: MAKE_SO += $(F_OPENMP)
openmp: libEfdcOrig.a

libEfdcOrig.a:	$(POBJECTS) $(OBJECTS) $(COMPAT_OBJS)
	rm -f $@
	ar cq $@ $(POBJECTS) $(OBJECTS) $(COMPAT_OBJS)

install:

clean:	
	rm -f *.a *.o *.po *.patch.for *.patch.f90

clobber:	

.for.po:
    ifeq ($(MAKE_FF),gfortran)
		patch -o $(<:.for=.patch.for) < $(<:.for=.patch) 
		$(MAKE_FF) $(MAKE_FFLAGS) -c  $(<:.for=.patch.for) -o $@
    else
		$(MAKE_FF) $(MAKE_FFLAGS) -c  $< -o $@
    endif

.f90.po:
    ifeq ($(MAKE_FF),gfortran)
		patch -o $(<:.f90=.patch.f90) < $(<:.f90=.patch) 
		$(MAKE_FF) $(MAKE_FFLAGS) -c  $(<:.f90=.patch.f90) -o $@
    else
		$(MAKE_FF) $(MAKE_FFLAGS) -c  $< -o $@
    endif

.for.o:	
	$(MAKE_FF) $(MAKE_FFLAGS) -c  $< -o $@

.f90.o:	
	$(MAKE_FF) $(MAKE_FFLAGS) -c  $< -o $@

global.mod:	Var_Global_Mod.o
drifter.mod:	DRIFTER.o
windwave.mod:	WINDWAVE.o

BAL2T5.o:	global.mod
SCANEFDC.o:	global.mod
VELPLTH.o:	global.mod
WQ3D.o:	global.mod
EEXPOUT.o:	global.mod
SURFPLT.o:	global.mod
CALHEAT.o:	global.mod
GATECTLREAD.o:	global.mod
CALPUVTT.o:	global.mod
READOIL.o:	global.mod
CGATEFLX.po:	global.mod
RESTOUT.o:	global.mod
VARZEROReal.o:	global.mod
s_sedzlj.o:	global.mod
s_shear.o:	global.mod
DRIFTER.o:	global.mod 
WINDWAVE.o:	global.mod drifter.mod


HDMT.o:		global.mod drifter.mod
HDMT2T.o:	global.mod drifter.mod windwave.mod
INPUT.o:	global.mod drifter.mod

ACON.o:		global.mod 
AINIT.o:	global.mod
BAL2T1.o:	global.mod
BAL2T2.o:	global.mod
BAL2T3A.o:	global.mod
BAL2T3B.o:	global.mod
BAL2T4.o:	global.mod
BEDINIT.o:	global.mod
BEDLOAD.o:	global.mod
BEDPLTH.o:	global.mod
BUDGET1.o:	global.mod
BUDGET2.o:	global.mod
BUDGET3.o:	global.mod
BUDGET5.o:	global.mod
CALAVB.o:	global.mod
CALAVB2.o:	global.mod
CALAVBOLD.o:	global.mod
CALBAL1.o:	global.mod
CALBAL2.o:	global.mod
CALBAL3.o:	global.mod
CALBAL4.o:	global.mod
CALBAL5.o:	global.mod
CALBED.o:	global.mod
CALBED9.o:	global.mod
CALBLAY.o:	global.mod
CALBUOY.o:	global.mod
CALCONC.o:	global.mod
CALCSER.o:	global.mod
CALDIFF.o:	global.mod
CALDISP2.o:	global.mod
CALDISP3.o:	global.mod
CALEBI.o:	global.mod
CALEXP.o:	global.mod
CALEXP2T.o:	global.mod
CALFQC.o:	global.mod
CALHDMF.o:	global.mod
CALHTA.o:	global.mod
CALIMP2T.o:	global.mod
CALMMT.o:	global.mod
CALPGCORR.o:	global.mod
CALPNHS.o:	global.mod
CALPSER.o:	global.mod
CALPUV2C.o:	global.mod
CALPUV2T.o:	global.mod
CALPUV9.o:	global.mod
CALPUV9C.o:	global.mod
CALQQ1.o:	global.mod
CALQQ1OLD.o:	global.mod
CALQQ2.o:	global.mod
CALQQ2T.o:	global.mod
CALQQ2TOLD.o:	global.mod
CALQVS.o:	global.mod
CALSED.o:	global.mod
CALSFT.o:	global.mod
CALSND.o:	global.mod
CALSTEP.o:	global.mod
CALSTEPD.o:	global.mod
CALTBXY.o:	global.mod
CALTOX.o:	global.mod
CALTOXB.o:	global.mod
CALTRAN.o:	global.mod
CALTRANQ.o:	global.mod
CALTSXY.o:	global.mod
CALUVW.o:	global.mod
CALVEGSER.o:	global.mod
CALWQC.o:	global.mod
CBALEV1.o:	global.mod
CBALEV2.o:	global.mod
CBALEV3.o:	global.mod
CBALEV4.o:	global.mod
CBALEV5.o:	global.mod
CBALOD1.o:	global.mod
CBALOD2.o:	global.mod
CBALOD3.o:	global.mod
CBALOD4.o:	global.mod
CBALOD5.o:	global.mod
CELLMAP.o:	global.mod
CELLMASK.o:	global.mod
CEQICM.o:	global.mod
CONGRAD.o:	global.mod
CONGRADC.o:	global.mod
COSTRAN.o:	global.mod
COSTRANW.o:	global.mod
DEPPLT.o:	global.mod
DUMP.o:		global.mod
INITBIN3.o:	global.mod
INITBIN4.o:	global.mod
INITBIN5.o:	global.mod
JPEFDC.o:	global.mod
LSQHARM.o:	global.mod
NEGDEP.o:	global.mod
OUT3D.o:	global.mod
OUTOIL.o:	global.mod
OUTPUT1.o:	global.mod
OUTPUT2.o:	global.mod
PPLOT.o:	global.mod
RCAHQ.o:	global.mod
READTOX.o:	global.mod
READWIMS1.o:	global.mod
READWIMS2.o:	global.mod
RELAX2T.o:	global.mod
RESTIN1.o:	global.mod
RESTIN10.o:	global.mod
RESTIN2.o:	global.mod
RESTMOD.o:	global.mod
RESTRAN.o:	global.mod
ROUT3D.o:	global.mod
RSALPLTH.o:	global.mod
RSALPLTV.o:	global.mod
RSMICI.o:	global.mod
RSMRST.o:	global.mod
RSURFPLT.o:	global.mod
RVELPLTH.o:	global.mod
RVELPLTV.o:	global.mod
RWQAGR.o:	global.mod
RWQATM.o:	global.mod
RWQBEN2.o:	global.mod
RWQC1.o:	global.mod
RWQCSR.o:	global.mod
RWQICI.o:	global.mod
RWQPSL.o:	global.mod
RWQRST.o:	global.mod
RWQSTL.o:	global.mod
RWQSUN.o:	global.mod
SALPLTH.o:	global.mod
SALPLTV.o:	global.mod
SALTSMTH.o:	global.mod
SCANASER.o:	global.mod
SCANDSER.o:	global.mod
SCANGATECTL.o:	global.mod
SCANGSER.o:	global.mod
SCANGWSR.o:	global.mod
SCANMASK.o:	global.mod
SCANMODC.o:	global.mod
SCANPSER.o:	global.mod
SCANQCTL.o:	global.mod
SCANQSER.o:	global.mod
SCANSFSR.o:	global.mod
SCANSSER.o:	global.mod
SCANTSER.o:	global.mod
SCANWQ.o:	global.mod
SCANWSER.o:	global.mod
SCNTXSED.o:	global.mod
SEDFLUX.o:	global.mod
SETBCS.o:	global.mod
SETFPOCB.o:	global.mod
SETOBC2T.o:	global.mod
SETOPENBC.o:	global.mod
SETOPENBC2.o:	global.mod
SHOWVAL.o:	global.mod
SMINIT.o:	global.mod
SMMBE.o:	global.mod
SMRIN1.o:	global.mod
SSEDTOX.o:	global.mod
SUBCHAN.o:	global.mod
TMSR.o:		global.mod
TOXCHEM.o:	global.mod
VALKH.o:	global.mod
VARALLOC.o:	global.mod
VARALLOC1.o:	global.mod
VARALLOC2.o:	global.mod
VARALLOC3.o:	global.mod
VARALLOC4.o:	global.mod
VARALLOC5.o:	global.mod
VARALLOC6.o:	global.mod
VARALLOC7.o:	global.mod
VARALLOC8.o:	global.mod
VARINIT.o:	global.mod
VARZEROInt.o:	global.mod
VELPLTV.o:	global.mod
VSFP.o:		global.mod
WASP4.o:	global.mod
WASP5.o:	global.mod
WASP6.o:	global.mod
WASP7.o:	global.mod
WASP7EPA.o:	global.mod
WAVEBL.o:	global.mod
WAVESXY.o:	global.mod
WQ3DINP.o:	global.mod
WQSKE0.o:	global.mod
WQSKE1.o:	global.mod
WQSKE2.o:	global.mod
WQSKE3.o:	global.mod
WQSKE4.o:	global.mod
WQZERO.o:	global.mod
WQZERO2.o:	global.mod
WQZERO3.o:	global.mod
WQZERO4.o:	global.mod
WSMRST.o:	global.mod
WSMTS.o:	global.mod
WSMTSBIN.o:	global.mod
WWQNC.o:	global.mod
WWQRST.o:	global.mod
WWQTS.o:	global.mod
WWQTSBIN.o:	global.mod
foodchain.o:	global.mod
initbin.o:	global.mod
initbin0.o:	global.mod
initbin2.o:	global.mod
s_bedload.o:	global.mod
s_main.o:	global.mod
s_morph.o:	global.mod
s_sedic.o:	global.mod
