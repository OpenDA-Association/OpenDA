C
C     This common block contains buffers for in memory storage of
C     records from multiple watershed files.
C
C     + + + PARAMETERS + + +
      INTEGER     CONREC,MXWDM
      PARAMETER   (CONREC=10,MXWDM=5)
C
      COMMON /CFBUFF/ WIBUFF,RECNO,WDMFUN,NXTPOS,PREPOS,FREPOS,
     1                WDMCNT,WDMOPN,MAXREC
      INTEGER     RECNO(CONREC),WDMFUN(CONREC),NXTPOS(CONREC),
     1            PREPOS(CONREC),FREPOS,
     2            WDMCNT,WDMOPN(MXWDM),MAXREC(MXWDM)
      INTEGER*4   WIBUFF(512,CONREC)
C
      SAVE   /CFBUFF/
C
      EQUIVALENCE (WIBUFF,WRBUFF)
      REAL        WRBUFF(512,CONREC)
C
