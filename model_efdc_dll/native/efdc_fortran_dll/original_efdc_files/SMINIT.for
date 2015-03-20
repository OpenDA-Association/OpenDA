      SUBROUTINE SMINIT  
C  
C CHANGE RECORD  
C  
      USE GLOBAL  
      SMTSNAME(1) = 'SOM'  
      SMTSNAME(2) = 'SIM'  
      SMTSNAME(3) = 'SBF'  
      DO L=2,LA  
        SMHYST(L)=.FALSE.  
      ENDDO  
      RETURN  
      END  

