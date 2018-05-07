C     COLOR.INC 2.1 9/4/91
C
C     This COMMON BLOCK is used to set and retrieve screen colors.
C
      COMMON /COLOR/ FRM,FRT,FRL,FRE,FRC,FRP,FRD,FRN,FRS,
     1               BKO,BKE,BKD,BKS,FORE,BACK
      INTEGER        FRM,FRT,FRL,FRE,FRC,FRP,FRD,FRN,FRS,
     1               BKO,BKE,BKD,BKS,FORE,BACK
C
      SAVE   /COLOR/
C
      EQUIVALENCE   (COLARR,FRM)
      INTEGER        COLARR(15)
C
