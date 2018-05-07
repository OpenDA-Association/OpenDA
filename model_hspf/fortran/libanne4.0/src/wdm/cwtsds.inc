C
C     This common block contains info about the current time series
C
      COMMON /CWTSDS/ CURREC,CURBKS,CURPOS,CURNOV,CURTST,CURTUN,CURCMP,
     1                CURQUA,CURCNT,CURDAT,   CURVAL,PREVAL
      INTEGER         CURREC,CURBKS,CURPOS,       CURTST,CURTUN,CURCMP,
     1                CURQUA,       CURDAT(6)
      INTEGER*4                            CURNOV,
     1                       CURCNT
      REAL                                    CURVAL,PREVAL
C
      SAVE   /CWTSDS/
C
