      SUBROUTINE WQZERO  
C  
C M. MORTON  28 JUN 1998  
C INITIALIZES THE WATER QUALITY AVERAGING SUMMATION ARRAYS:  
C CHANGE RECORD  
C  
      USE GLOBAL 
C 
      DO LL=2,LA  
        DO K=1,KC  
          DO NW=1,NWQV  
            WQVSUM(LL,K,NW) = 0.0  
            WQVMIN(LL,K,NW) = 9.99E+21  
            WQVMAX(LL,K,NW) = 0.0  
          ENDDO  
          PO4DWQSUM(LL,K) = 0.0  
          SADWQSUM(LL,K) = 0.0  
          TPWQSUM(LL,K) = 0.0  
          TSIWQSUM(LL,K) = 0.0  
          BOD5SUM(LL,K) = 0.0  
          CHLMSUM(LL,K) = 0.0  
          POCSUM(LL,K) = 0.0  
          POPSUM(LL,K) = 0.0  
          PONSUM(LL,K) = 0.0  
          TPWQMIN(LL,K) = 9.99E+21  
          TPWQMAX(LL,K) = 0.0  
          SADWQMIN(LL,K) = 9.99E+21  
          SADWQMAX(LL,K) = 0.0  
          POCMIN(LL,K) = 9.99E+21  
          POCMAX(LL,K) = 0.0  
          POPMIN(LL,K) = 9.99E+21  
          POPMAX(LL,K) = 0.0  
          PONMIN(LL,K) = 9.99E+21  
          PONMAX(LL,K) = 0.0  
          CHLMMIN(LL,K) = 9.99E+21  
          CHLMMAX(LL,K) = 0.0  
          SALSUM(LL,K) = 0.0  
          SALMN(LL,K) = 9.99E+21  
          SALMX(LL,K) = 0.0  
          WQTEMSUM(LL,K) = 0.0  
          WQTEMMIN(LL,K) = 9.99E+21  
          WQTEMMAX(LL,K) = 0.0  
          TSSSUM(LL,K) = 0.0  
          TSSMN(LL,K) = 9.99E+21  
          TSSMX(LL,K) = 0.0  
          WQKETSUM(LL,K) = 0.0  
          WQKETMN(LL,K) = 9.99E+21  
          WQKETMX(LL,K) = 0.0  
        ENDDO  
      ENDDO  
      TIMESUM = 0.0  
      NWQCNT = 0  
      RETURN  
      END  

