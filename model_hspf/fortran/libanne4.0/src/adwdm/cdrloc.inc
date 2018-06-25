C
C     This common block contains pointers to file definition record
C     locations for wdm.  Values are initialized in subroutine WDBFIN.
C
      COMMON /CDRLOC/ PFNAME,PMXREC,PFRREC,PTSNUM,PDIRPT
      INTEGER         PFNAME,PMXREC,PFRREC,PTSNUM,PDIRPT
C
      SAVE   /CDRLOC/
C
