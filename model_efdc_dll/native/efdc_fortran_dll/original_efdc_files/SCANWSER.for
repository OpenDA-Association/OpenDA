      SUBROUTINE SCANWSER  
      USE GLOBAL  
      USE MPI
      INTEGER IOS
      IF(MYRANK.EQ.0)WRITE(*,'(A)')'SCANNING INPUT FILE: WSER.INP'  
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
   20 CONTINUE
      IF(MYRANK.EQ.0) WRITE(*,30)  
      IF(MYRANK.EQ.0) WRITE(8,30)  
   30 FORMAT('READ ERROR IN INPUT FILE')  
      STOP  
   40 CONTINUE
      IF(MYRANK.EQ.0) WRITE(*,50)  
      IF(MYRANK.EQ.0) WRITE(8,50)  
   50 FORMAT('UNEXPECTED END OF INPUT FILE')  
      STOP  
      END  

