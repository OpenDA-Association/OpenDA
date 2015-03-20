      SUBROUTINE SCANWQ

      ! *** Merged SNL & DS-INTL Codes

      USE GLOBAL

      CHARACTER*10  INFILE
      CHARACTER*2   SNUM
      CHARACTER*120 LINE
      CHARACTER*11  FNWQSR(40)  
      LOGICAL*4     BFLAG
      INTEGER*4     I,J,K,ITMP,NW
      REAL*4        XPSQ
      
      WRITE(*,'(A)')'SCANNING INPUT FILE: WQ3DWC.INP'
      INFILE='WQ3DWC.INP'

      OPEN(1,FILE='WQ3DWC.INP',STATUS='UNKNOWN')  

      CALL SEEK('C02')  
      READ(1,*) ISWQLVL,NWQV,NWQZ,NWQPS,NWQTD,NWQTS,NTSWQV,NSMG,NSMZ,  
     &          NTSSMV,NSMTS,NWQKDPT  
      NWQZM=MAX(1,NWQZ)
      NWQPSM=MAX(1,NWQPS)
      NWQTDM=MAX(1,NWQTD)
      NWQVM=MAX(23,NTSWQV)		!VB changed from 22 to 23	!SHOULD THIS BE 22 OR 23 FOR EE????
      IF(NWQVM.GT.23)THEN			!VB changed from 22 to 23	
        STOP ' NTSWQV.GT.23'		!VB changed from 22 to 23
      ENDIF
      NWQTSM=MAX(1,NWQTS)
      NSMGM=MAX(1,NSMG)
      NSMZM=MAX(1,NSMZ)
      NWQVM=MAX(NTSSMV,NWQVM)
      NSMTSM=MAX(1,NSMTS)  ! *** PMC-NOT USED

      CALL SEEK('C05')  
      READ(1,*) IWQICI,IWQAGR,IWQSTL,IWQSUN,IWQPSL,IWQNPL, ISDIURDO,  
     &    WQDIUDT, IWQKIN  

      CALL SEEK('C06')  
      READ(1,*) IWQTS,TWQTSB,TWQTSE,WQTSDT, ISWQAVG, ISWQMIN, ISWQMAX,  
     &          ISCOMP  
      IF(IWQTS.GT.NWQTSM)THEN
        STOP ' IWQTS.GT.NWQTSM'
      ENDIF

C  *** C29
      CALL SEEK('C29')  
      DO M=1,9
        READ(1,9)LINE
      ENDDO  
      READ(1,*) (NWQCSR(NW),NW=1,NWQV) 
      DO NW=1,NWQV
        NWQCSRM=MAX(NWQCSRM,NWQCSR(NW))
      ENDDO 
C  
      CALL SEEK('C48')  
      READ(1,*) IWQPS,NPSTMSR  
      IF(IWQPS.GT.NWQPSM)THEN
        STOP ' IWQPS.GT.NWQPSM'
      ENDIF

      IF(IWQPSL.EQ.2)THEN
        DO K=1,3
          READ(1,9)LINE
        ENDDO
        DO M=1,IWQPS  
          READ(1,*) I,J,K,ITMP,XPSQ
          READ(1,9)LINE
          READ(1,9)LINE
          NCSERM=MAX(1,NCSERM,ITMP)  
        ENDDO
      ELSE
        NWQPSRM=MAX(1,NPSTMSR)
      ENDIF
      
      CLOSE(1)  

      ! *** SCAN THE TIME SERIES
      IF(NPSTMSR.GE.1.AND.IWQPSL.NE.2)THEN  
        WRITE(*,'(A)')'SCANNING INPUT FILE: WQPSL.INP'
        OPEN(1,FILE='WQPSL.INP',STATUS='UNKNOWN')  
        DO IS=1,13  
          READ(1,1)  
        ENDDO  
        DO NS=1,NPSTMSR  
   10     READ(1,*,ERR=10,END=20)M,TM,TA,RMULADJ,ADDADJ  
          NDWQPSR=MAX(NDWQPSR,M)
          DO J=1,M  
            !READ(1,*)T,(RLDTMP(K),K=1,NWQV)  
            DO K=1,3
              READ(1,9)LINE
            ENDDO
          ENDDO  
        ENDDO
   20   CONTINUE
        CLOSE(1)
      ENDIF
      
      ! *** SCAN THE OPEN BC TIME SERIES
      DO NW = 1,40
        WRITE(SNUM,'(I2.2)')NW
        FNWQSR(NW)='CWQSR'//SNUM//'.INP'
      ENDDO
C  
C **  READ IN OPEN BOUNDARY OR VOLUMETRIC SOURCE WQ CONCENTRATION  
C **  TIME SERIES FROM THE FILES CWQSRNN.INP  
C  
      DO NW=1,NWQV  
        ! IF(NWQCSR(NW).GE.1)THEN  
        INQUIRE(FILE=FNWQSR(NW),EXIST=BFLAG)
        IF(BFLAG)THEN
          OPEN(1,FILE=FNWQSR(NW),STATUS='UNKNOWN')  
          DO IS=1,15  
            READ(1,1)  
          ENDDO  
          DO NS=1,1000  
   30       READ(1,*,ERR=30,END=40)ISTYP,M,T1,T2,RMULADJ,ADDADJ  
            NDWQCSR=MAX(NDWQCSR,M)
            DO J=1,M  
              !READ(1,*)T,(RLDTMP(K),K=1,NWQV)  
              READ(1,9)LINE
            ENDDO  
          ENDDO
   40     CONTINUE
          CLOSE(1)
        ENDIF
      ENDDO

      IPMC=0

   50 RETURN
   
    1 FORMAT(1X)
    9 FORMAT(A120) 
      END
