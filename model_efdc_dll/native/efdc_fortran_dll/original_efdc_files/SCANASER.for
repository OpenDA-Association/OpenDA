      SUBROUTINE SCANASER  
      USE GLOBAL  
      USE MPI
      CHARACTER*120 LIN
      
      IF(MYRANK.EQ.0) WRITE(*,'(A)')'SCANNING INPUT FILE: ASER.INP'  
      OPEN(1,FILE='ASER.INP',STATUS='OLD')  
      DO N=1,NASER  
   10   READ(1,*,ERR=10,END=40)M,R,R,I,R,R,R,R  
        READ(1,*,ERR=20,END=40)I,R,R,R,R,R,R,R,R,R  
        NDASER=MAX(NDASER,M)  
        DO I=1,M  
          READ(1,*,ERR=20,END=40)R,R,R,R,R,R,R,R  
        ENDDO  
      ENDDO  
      CLOSE(1)  

      IF(ISTRAN(8).GT.0)THEN
        IF(IWQSUN.EQ.1)THEN
          IF(MYRANK.EQ.0) 
     &    WRITE(*,'(A)')'SCANNING INPUT FILE: SUNDAY.INP'  
          OPEN(1,FILE='SUNDAY.INP',STATUS='UNKNOWN')  
          M=0  
          DO I = 1,7
            READ(1,"(A120)",ERR=20,END=40)LIN
          END DO
          READ(1,*,ERR=20,END=40)M,R,R,R,R
          CLOSE(1)  
          NDASER=MAX(NDASER,M)  
        ENDIF
      ENDIF
      
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

