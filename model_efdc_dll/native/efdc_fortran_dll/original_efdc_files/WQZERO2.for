C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C
      SUBROUTINE WQZERO2
C
C**********************************************************************C
C
C M. MORTON  12 APR 1999
C INITIALIZES THE DIURNAL DO SUMMATION ARRAYS:
C
C **  LAST MODIFIED BY JOHN HAMRICK AND MIKE MORTON ON 8 AUGUST 2001
C
C **  THIS SUBROUTINE IS PART OF  EFDC-FULL VERSION 1.0a 
C
C **  LAST MODIFIED BY JOHN HAMRICK ON 1 NOVEMBER 2001
C
C----------------------------------------------------------------------C
C
C CHANGE RECORD
C DATE MODIFIED     BY                 DATE APPROVED    BY
C
C----------------------------------------------------------------------C
C
C
C**********************************************************************C
C
      USE GLOBAL 
C
      DO LL=2,LA
        DO K=1,KC
C
C ZERO THE DIURNAL DO VARIABLES:
C
          SODSUM(LL,K) = 0.0
          RKASUM(LL,K) = 0.0
          SWQSUM(LL,K) = 0.0
          TEMSUM(LL,K) = 0.0
          DZSUM(LL,K)  = 0.0
          DOOSUM(LL,K) = 0.0
          DOSSUM(LL,K) = 0.0
          CYASUM(LL,K) = 0.0
          DIASUM(LL,K) = 0.0
          GRNSUM(LL,K) = 0.0
          XMACSUM(LL,K) = 0.0
          DO I=1,4
            RESPSUM(LL,K,I) = 0.0
            PRODSUM(LL,K,I) = 0.0
          ENDDO
        ENDDO
      ENDDO
      TIMESUM2 = 0.0
      NDOCNT = 0
C
      RETURN
      END
