      SUBROUTINE SCANWSER  
      USE GLOBAL  
      WRITE(*,'(A)')'SCANNING INPUT FILE: WSER.INP'  
      OPEN(1,FILE='WSER.INP',STATUS='OLD')  
      DO NS=1,NWSER  
   10   READ(1,*,ERR=10,END=40)M,R,R,R,I  
        NDWSER=MAX(NDWSER,M)  
        DO I=1,M  
          READ(1,*,ERR=20,END=40)R,R,R  
        ENDDO  
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

