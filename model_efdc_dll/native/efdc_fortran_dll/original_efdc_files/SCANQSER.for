      SUBROUTINE SCANQSER  
      USE GLOBAL
      INTEGER*4 NS, I, J, M
        
      WRITE(*,'(A)')'SCANNING INPUT FILE: QSER.INP'  
      OPEN(1,FILE='QSER.INP',STATUS='OLD')  

      DO NS=1,NQSER  
   10   READ(1,*,ERR=10,END=40)I,M,R,R,R,R,J  
        NDQSER=MAX(NDQSER,M)  
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
      
C *****************************************************************************
      
      SUBROUTINE SCANQWSER  
      USE GLOBAL  
      INTEGER*4  NTMP, I, J, M, NV
      
      NTMP=4+NSED+NSND+NTOX
      ! *** Handle Water Quality variables, if needed
      IF(ISTRAN(8).GT.0)THEN
        WRITE(*,'(A)')'SCANNING INPUT FILE: WQ3DWC.INP (PRELIM)'
        OPEN(1,FILE='WQ3DWC.INP',STATUS='OLD')

        CALL SEEK('C02')  
        READ(1,*) ISWQLVL,NWQV,NWQZ,NWQPS,NWQTD,NWQTS,NTSWQV,NSMG,NSMZ,
     &            NTSSMV,NSMTS,NWQKDPT  
        CLOSE(1)
          
        NTMP=NTMP+NWQV 
      ENDIF

      WRITE(*,'(A)')'SCANNING INPUT FILE: QWRS.INP'  
      OPEN(1,FILE='QWRS.INP',STATUS='OLD') 

      DO NS=1,NQWRSR  
   10   READ(1,*,ERR=10,END=40)I,M,R,R,R,R
        NDQWRSR=MAX(NDQWRSR,M)  
        IF(I.EQ.0)THEN  
          ! *** Flow Only
          DO J=1,M  
            READ(1,*,ERR=20,END=40)R,R  
          ENDDO  
        ELSE
          ! *** Flow with Rise/Fall
          DO J=1,M  
            READ(1,*,ERR=20,END=40)R,R,(R,NV=1,NTMP)  
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
