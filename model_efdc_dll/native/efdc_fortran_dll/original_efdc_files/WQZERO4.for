      SUBROUTINE WQZERO4  
C  
C M. MORTON  02 JUN 1999  
C INITIALIZES THE BENTHIC FLUX ARRAYS TO 0.0  
C CHANGE RECORD  
C  
      USE GLOBAL  
      DO LL=2,LA  
C  
C ZERO THE BENTHIC FLUX ARRAYS:  
C  
        BFO2SUM(LL)  = 0.0  
        BFNH4SUM(LL) = 0.0  
        BFNO3SUM(LL) = 0.0  
        BFPO4SUM(LL) = 0.0  
        BFSADSUM(LL) = 0.0  
        BFCODSUM(LL) = 0.0  
        BFSMTSUM(LL) = 0.0  
        BFBSTSUM(LL) = 0.0  
      ENDDO  
      TIMEBF = 0.0  
      NBFCNT = 0  
      RETURN  
      END  

