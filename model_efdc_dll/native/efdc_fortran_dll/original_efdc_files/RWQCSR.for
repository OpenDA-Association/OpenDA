      SUBROUTINE RWQCSR  
C  
C CHANGE RECORD  
C  
      USE GLOBAL  
      USE MPI
      IMPLICIT NONE
      
      CHARACTER*11 FNWQSR(40)
      CHARACTER*2  SNUM
      INTEGER*4    NT,NW,IS,NS,ISO,ISTYP,K,M
      REAL         RMULADJ,ADDADJ,CSERTMP
      CHARACTER*12 FNWQSRX(NXSP) ! X-species 
      INTEGER*4 nsp ! Number of x-species.
        
      IF(MYRANK.EQ.0)THEN
      PRINT *,'WQ: READING CWQSRxx.INP - WQ CONCENTRATION TIME SERIES'
      ENDIF

      ! *** DEFINE THE INPUT FILE NAMES
      DO NW = 1,NWQV
        WRITE(SNUM,'(I2.2)')NW
        FNWQSR(NW)='CWQSR'//SNUM//'.INP'
      ENDDO
C  
C **  READ IN OPEN BOUNDARY OR VOLUMETRIC SOURCE WQ CONCENTRATION  
C **  TIME SERIES FROM THE FILES CWQSRNN.INP  
C  
      DO NW=1,NWQV  
        IF(NWQCSR(NW).GE.1)THEN  
          OPEN(1,FILE=FNWQSR(NW),STATUS='UNKNOWN')  
C  
C **  SKIP OVER TITLE AND AND HEADER LINES  
C  
          DO IS=1,15  
            READ(1,1)  
          ENDDO  
          
          NT=4+NTOX+NSED+NSND+NW
          DO NS=1,NWQCSR(NW)  
            MCTLAST(NS,NT)=1  
            READ(1,*,IOSTAT=ISO)ISTYP,MCSER(NS,NT),TCCSER(NS,NT),  
     &          TACSER(NS,NT),RMULADJ,ADDADJ  
            IF(ISO.GT.0) GOTO 900  
            IF(ISTYP.EQ.1)THEN  
              READ(1,*,IOSTAT=ISO) (WKQ(K),K=1,KC)  
              IF(ISO.GT.0) GOTO 900  
              DO M=1,MCSER(NS,NT)  
                READ(1,*,IOSTAT=ISO)TCSER(M,NS,NT),CSERTMP  
                IF(ISO.GT.0) GOTO 900  
                TCSER(M,NS,NT)=TCSER(M,NS,NT)+TACSER(NS,NT)  
                DO K=1,KC  
                  CSER(M,K,NS,NT)=(RMULADJ*(CSERTMP+ADDADJ))*WKQ(K)  
                ENDDO  
              ENDDO  
            ELSE  
              DO M=1,MCSER(NS,NT)  
                READ(1,*,IOSTAT=ISO)TCSER(M,NS,NT),  
     &              (CSER(M,K,NS,NT), K=1,KC)  
                IF(ISO.GT.0) GOTO 900  
                TCSER(M,NS,NT)=TCSER(M,NS,NT)+TACSER(NS,NT)  
                DO K=1,KC  
                  CSER(M,K,NS,NT)=RMULADJ*(CSER(M,K,NS,NT)+ADDADJ)  
                ENDDO  
              ENDDO  
            ENDIF  
          ENDDO  
          CLOSE(1)  
        ENDIF  
      ENDDO  
      GOTO 901  
  900 CONTINUE  
      WRITE(6,601)NW,NS,M  
      STOP  
  901 CONTINUE  
      if (NXSP.gt.0) then
      IF(MYRANK.EQ.0)THEN
      PRINT *,'WQ: READING CWQSRX##.INP, X WQ CONCENTRATION TIME SERIES'
      ENDIF
        DO nsp=1,NXSP
          WRITE(SNUM,'(I2.2)')nsp
          FNWQSRX(nsp)='CWQSRX'//SNUM//'.INP'
        ENDDO
        
        DO nsp=1,NXSP  
          IF(NWQCSRX(nsp).GE.1)THEN  
            OPEN(1,FILE=FNWQSRX(nsp),STATUS='UNKNOWN')  
C  
C **  SKIP OVER TITLE AND AND HEADER LINES  
C  
            DO IS=1,15  
              READ(1,1)  
            ENDDO  
            
            NT=4+NTOX+NSED+NSND+NWQV+nsp
            DO NS=1,NWQCSRX(nsp)
              MCTLAST(NS,NT)=1  
              READ(1,*,IOSTAT=ISO)ISTYP,MCSER(NS,NT),TCCSER(NS,NT),  
     &            TACSER(NS,NT),RMULADJ,ADDADJ  
              IF(ISO.GT.0) GOTO 800  
              IF(ISTYP.EQ.1)THEN  
                READ(1,*,IOSTAT=ISO) (WKQ(K),K=1,KC)  
                IF(ISO.GT.0) GOTO 800  
                DO M=1,MCSER(NS,NT)  
                  READ(1,*,IOSTAT=ISO)TCSER(M,NS,NT),CSERTMP  
                  IF(ISO.GT.0) GOTO 800  
                  TCSER(M,NS,NT)=TCSER(M,NS,NT)+TACSER(NS,NT)  
                  DO K=1,KC  
                    CSER(M,K,NS,NT)=(RMULADJ*(CSERTMP+ADDADJ))*WKQ(K)  
                  ENDDO  
                ENDDO  
              ELSE  
                DO M=1,MCSER(NS,NT)  
                  READ(1,*,IOSTAT=ISO)TCSER(M,NS,NT),  
     &                (CSER(M,K,NS,NT), K=1,KC)  
                  IF(ISO.GT.0) GOTO 800  
                  TCSER(M,NS,NT)=TCSER(M,NS,NT)+TACSER(NS,NT)  
                  DO K=1,KC  
                    CSER(M,K,NS,NT)=RMULADJ*(CSER(M,K,NS,NT)+ADDADJ)  
                  ENDDO  
                ENDDO  
              ENDIF  
            ENDDO  
            CLOSE(1)  
          ENDIF  
        ENDDO
      endif !if (NXSP.gt.0) then
      GOTO 801  
  800 CONTINUE  
      WRITE(6,601)NW,NS,M  
      STOP  
  801 CONTINUE
    1 FORMAT(120X)  
  601 FORMAT(' READ ERROR WQ TIME SERIES, NWQ,NSER,MDATA = ',3I5)  
C 602 FORMAT(' READ OF FILES CWQSRNN.INP SUCCESSFUL'/)  
      RETURN  
      END  

