      SUBROUTINE SCANMODC  
      USE GLOBAL  
      USE MPI
      INTEGER IOS
      IF(MYRANK.EQ.0)WRITE(*,'(A)')'SCANNING INPUT FILE: MODCHAN.INP'  
      OPEN(1,FILE='MODCHAN.INP',STATUS='OLD')  
   10 READ(1,*,ERR=10,END=40)M,I,I  
      NCHANM=MAX(1,M)  
      READ(1,*,ERR=20,END=40)I,I,R  
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

