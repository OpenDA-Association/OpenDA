      SUBROUTINE SCANSFSR(NCSER4)  
      USE GLOBAL  
      WRITE(*,'(A)')'SCANNING INPUT FILE: SFSER.INP'  
      OPEN(1,FILE='SFSER.INP',STATUS='OLD')  
      DO NS=1,NCSER4  
   10   READ(1,*,ERR=10,END=40)I,M,R,R,R,R  
        NDCSER=MAX(NDCSER,M)  
        IF(I.EQ.1)THEN  
          READ(1,*,ERR=20,END=40)(R,K=1,KC)  
          DO J=1,M  
            READ(1,*,ERR=20,END=40)R,R  
          ENDDO  
        ELSE  
          DO J=1,M  
            READ(1,*,ERR=20,END=40)R,(R,K=1,KC)  
          ENDDO  
        ENDIF  
      ENDDO  
      CLOSE(1)  
      RETURN  
   20 WRITE(*,30)  
      WRITE(8,30)  
   30 FORMAT('READ ERROR IN INPUT FILE')  
      STOP  
   40 WRITE(*,50)  
      WRITE(8,50)  
   50 FORMAT('UNEXPECTED END OF INPUT FILE')  
      STOP  
      END  

