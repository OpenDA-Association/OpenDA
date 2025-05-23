C  
C READ IN TEMPORALLY VARYING PARAMETERS FOR DAILY SOLAR RADIATION (WQI0)  
C AND FRACTIONAL DAYLENGTH (WQFD) (UNIT INWQSUN).  
C  
      SUBROUTINE RWQSUN  
C  
C **  NEW VERSION BY J. M. HAMRICK  7 APRIL 1997  
C CHANGE RECORD  
C **  READS AND INTERPOLATES DAILY AVERAGE SOLAR RADIATION AND  
C **  DAYLIGHT FRACTION  
C  
      USE GLOBAL  
      USE MPI
      IF(ITNWQ.GT.0) GOTO 1000  
C  
C **  READ IN DAILY AVERAGE SOLAR SW RAD SERIES FROM FILE 'SUNDAY.INP'  
C  
      IF(MYRANK.EQ.0)PRINT *,'WQ: SUNDAY.INP'
      OPEN(1,FILE='SUNDAY.INP',STATUS='UNKNOWN')  
C  
C **  SKIP OVER TITLE AND AND HEADER LINES  
C  
      DO IS=1,7  
        READ(1,1)  
      ENDDO  
      M=0  
      ISPAR=1  
C  
C      MCSUNDAY=1 TEMP USE ISPAR FOR MCSUNDAY  
C  
      READ(1,*,IOSTAT=ISO)NSUNDAY,TCSUNDAY,  
     &    TASUNDAY,RMULADJ,ADDADJ  
      IF(ISO.GT.0) GOTO 900  
      DO M=1,NSUNDAY  
        READ(1,*,IOSTAT=ISO)TSSRD(M),SOLSRD(M),SOLFRD(M)  
        IF(ISO.GT.0) GOTO 900  
        TSSRD(M)=TCSUNDAY*( TSSRD(M)+TASUNDAY )  
        SOLSRD(M)=RMULADJ*(SOLSRD(M)+ADDADJ) * PARADJ  
      ENDDO  
      CLOSE(1)  
      GOTO 901  
  900 CONTINUE  
      IF(MYRANK.EQ.0)WRITE(6,601)M  
      STOP  
  901 CONTINUE  
    1 FORMAT(120X)  
  601 FORMAT(' READ ERROR FILE SUNDAY.INP ')  
 1000 CONTINUE  
C  
C **  DAILY AVERAGE SOLAR SW RADIATION INTERPOLTATION FOR WATER QUALITY  
C  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=(DT*FLOAT(N)+TCON*TBEGIN)/86400.  
      ELSE  
        TIME=TIMESEC/86400.  
      ENDIF  
      M1=ISPAR  
C  
C      TEMP USE ISPAR FOR MCSUNDAY  
C  
  100 CONTINUE  
      M2=M1+1  
      IF((TIME-EPS).GT.TSSRD(M2))THEN  
        M1=M2  
        GOTO 100  
      ELSE  
        ISPAR=M1  
C  
C      TEMP USE ISPAR FOR MCSUNDAY  
C  
      ENDIF  
      TDIFF=TSSRD(M2)-TSSRD(M1)  
      WTM1=(TSSRD(M2)-TIME)/TDIFF  
      WTM2=(TIME-TSSRD(M1))/TDIFF  
      SOLSRDT=WTM1*SOLSRD(M1)+WTM2*SOLSRD(M2)  
      SOLFRDT=WTM1*SOLFRD(M1)+WTM2*SOLFRD(M2)  
      RETURN  
      END  

