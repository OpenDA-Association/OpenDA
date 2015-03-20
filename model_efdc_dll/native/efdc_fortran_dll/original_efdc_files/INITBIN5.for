      SUBROUTINE INITBIN5  
C  
C Y.S.SONG 2011. 10. 31  
C  
      USE GLOBAL  

        OPEN(3,FILE='WQDOCOMP.BIN',FORM='UNFORMATTED',STATUS='UNKNOWN')  
        CLOSE(3, STATUS='DELETE')  
        OPEN(3,FILE='WQDOCOMP.BIN',FORM='UNFORMATTED',STATUS='UNKNOWN')   

        DO L=2,LA  
          WRITE(3) IL(L)  
        ENDDO  
        DO L=2,LA  
          WRITE(3) JL(L)  
        ENDDO 
        DO L=2,LA  
          WRITE(3) DLON(L)  
        ENDDO 
        DO L=2,LA  
          WRITE(3) DLAT(L)  
        ENDDO 

        CLOSE(3)

        OPEN(3,FILE='WQRATE.BIN',FORM='UNFORMATTED',STATUS='UNKNOWN')  
        CLOSE(3, STATUS='DELETE')  
        OPEN(3,FILE='WQRATE.BIN',FORM='UNFORMATTED',STATUS='UNKNOWN')   

        DO L=2,LA  
          WRITE(3) IL(L)  
        ENDDO  
        DO L=2,LA  
          WRITE(3) JL(L)  
        ENDDO 
        DO L=2,LA  
          WRITE(3) DLON(L)  
        ENDDO 
        DO L=2,LA  
          WRITE(3) DLAT(L)  
        ENDDO 

        CLOSE(3)

      RETURN  
      END  