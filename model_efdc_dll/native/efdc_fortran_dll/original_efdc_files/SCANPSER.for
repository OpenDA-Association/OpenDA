      SUBROUTINE SCANPSER  
      USE GLOBAL  
      INTEGER IOS
      WRITE(*,'(A)')'SCANNING INPUT FILE: PSER.INP'  
      OPEN(1,FILE='PSER.INP',STATUS='OLD')  
      DO NS=1,NPSER  
        IOS=1
        DO WHILE (IOS>0)
          READ(1,*,IOSTAT=IOS,END=40)M,R,R,R,R  
        ENDDO
        NDPSER=MAX(NDPSER,M)  
        DO I=1,M  
          READ(1,*,ERR=20,END=40)R,R  
        ENDDO  
      ENDDO  
      CLOSE(1)  
      RETURN  
C  
   20 WRITE(*,30)  
      WRITE(8,30)  
   30 FORMAT('READ ERROR IN INPUT FILE')  
      STOP  
   40 WRITE(*,50)  
      WRITE(8,50)  
   50 FORMAT('UNEXPECTED END OF INPUT FILE')  
      STOP  
      END  

