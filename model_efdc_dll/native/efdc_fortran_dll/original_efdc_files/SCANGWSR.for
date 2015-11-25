! Advanced EFDC Hydraulic structure ! GEOSR. 2011. 12. JGCHO
!
      SUBROUTINE SCANGWSR  
      USE GLOBAL  
      WRITE(*,'(A)')'SCANNING INPUT FILE: GWSER.INP'  
      OPEN(1,FILE='GWSER.INP',STATUS='OLD')  
   10 READ(1,*,ERR=10,END=40)NGWSER  
      NGWSERM=MAX(1,NGWSER)  
      DO NS=1,NGWSER  
        READ(1,*,ERR=20,END=40)M,R,R,R,R  
        NDGWSER=MAX(NDGWSER,M)  
        DO I=1,M  
          READ(1,*,ERR=20,END=40)R,R,(R,J=1,4+NSED+NSND+NTOX)  
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

