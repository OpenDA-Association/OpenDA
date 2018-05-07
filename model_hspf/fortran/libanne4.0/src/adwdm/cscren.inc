C     CSCREN.INC 2.1 9/4/91
C
C     This COMMON block is used by routines which do screen i/o.
C
      COMMON /CSCREN/  NFLDS,BASREC,HLPOFF,HDST,CROW,CFLD,CCOL,QFLAG,
     1                 SCNINI,FPROT,       SCOL,       FLEN,
     2                 MXROW,HPRWF,HPLEN,NMHDRW,DTRWF,DTRWL,WDRWF,WDRWL,
     3                 IMIN,       IMAX,       IDEF,
     5                 RMIN,       RMAX,       RDEF,
     4                 CCNT,       CDEF,       APOS,       SPINIT,
     6                 FLIN,       FREC,       FSLOT,
     7                 FDVAL,       FDINV,       ZFILUN,
     8                 WINFLG,WROW,WFLD,ASCLU,ASGRP
      INTEGER          NFLDS,BASREC,HLPOFF,HDST,CROW,CFLD,CCOL,QFLAG,
     1                 SCNINI,FPROT(MXFLD),SCOL(MXFLD),FLEN(MXFLD),
     2                 MXROW,HPRWF,HPLEN,NMHDRW,DTRWF,DTRWL,WDRWF,WDRWL,
     3                 IMIN(MXFLD),IMAX(MXFLD),IDEF(MXFLD),
     4                 CCNT(MXFLD),CDEF(MXFLD),APOS(MXFLD),SPINIT,
     5                 FLIN(MXFLD),FREC(MXFLD),FSLOT(MXFLD),
     6                 FDVAL(MXFLD),FDINV(MXFLD),ZFILUN(MXFLD),
     7                 WINFLG,WROW,WFLD,ASCLU,ASGRP
      REAL             RMIN(MXFLD),RMAX(MXFLD),RDEF(MXFLD)
C
      COMMON /CSCRED/  DMIN,       DMAX,       DDEF
      DOUBLE PRECISION DMIN(MXFLD),DMAX(MXFLD),DDEF(MXFLD)
C
      COMMON /CSCREC/ DELIM,MULT,FTC,FTI,FTR,FTD,FTO,FTF,BLNK,DONE,HELP,
     1                HEADR,    FTYP,       TMES,    CSTR,       ASDSFG,
     2                FDFMT
      CHARACTER*1     DELIM,MULT,FTC,FTI,FTR,FTD,FTO,FTF,BLNK,DONE,HELP,
     1                HEADR(80),FTYP(MXFLD),TMES(40),CSTR(80,MXFLD),
     2                ASDSFG(MXFLD)
      CHARACTER*8     FDFMT(MXFLD)
C
      SAVE   /CSCREN/, /CSCRED/, /CSCREC/
C
