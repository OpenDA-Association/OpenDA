      SUBROUTINE WWQNC  
C  
C CHANGE RECORD  
C WRITE INFORMATION OF NEGATIVE WQ STATE VARIABLES (UNIT IWQONC).  
C  
      USE GLOBAL  
      CHARACTER*5 WQVN(NTSWQVM)  

      DATA WQVN/  
     &    'BC ','BD ','BG ','RPOC','LPOC','DOC ','RPOP','LPOP',  
     &    'DOP ','PO4T','RPON','LPON','DON ','NH4 ','NO3 ','SU ',  
     &    'SA   ','COD  ','O2   ','TAM  ','FCB  ', 'CO2 ','MALG '/  

      OPEN(1,FILE=NCOFN,STATUS='UNKNOWN',POSITION='APPEND')  

      DO L=2,LA  
        DO K=1,KC  
          DO NW=1,NTSWQV  
            IF(WQV(L,K,NW).LT.0.0) WRITE(1,90) WQVN(NW),  
     &          ITNWQ,L,IL(L),JL(L),K,WQV(L,K,NW)  
          ENDDO  
        ENDDO  
      ENDDO  
      CLOSE(1)  
   90 FORMAT(A5, I8, 4I5, E11.3)  
      RETURN  
      END  

