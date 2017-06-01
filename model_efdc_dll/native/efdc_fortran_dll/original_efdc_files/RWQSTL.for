
      SUBROUTINE RWQSTL(TIMTMP)  ! Read in days instead of index
C  
C CHANGE RECORD  
C READ IN SPATIALLY AND/OR TEMPORALLY VARYING PARAMETERS FOR SETTLING  
C VELOCITIES OF ALGAE, RPOM, LPOM & PARTICULATE METAL (UNIT INWQSTL).  
C ALSO SPATIALLY/TEMPORALLY VARYING REAERATION ADJUSTMENT FACTOR.  
C  
C
C ***    WQWSC = Settling velocity for cyanobacteria (m/day)
C ***    WQWSD = Settling velocity for algae diatoms (m/day)
C ***    WQWSG = Settling velocity for algae greens  (m/day)
C ***   WQWSRP = Settling velocity for refractory POM (m/day)
C ***   WQWSLP = Settling velocity for labile POM (m/day)
C ***    WQWSS = Settling velocity for particles sorbed to TAM (m/day)
C ***    WQWSM = Settling velocity for macroalgae (m/day = 0.0)
C ***    WQWSM = Reaeration adjustment factor (NOT SAVED)
C
      USE GLOBAL  
C
      CHARACTER TITLE(3)*79, STLCONT*3  
C
      OPEN(7892,FILE=STLFN,STATUS='UNKNOWN')  
      OPEN(2,FILE='WQ3D.OUT',STATUS='UNKNOWN',POSITION='APPEND')  
      IF(STLDAY.EQ.0) THEN
        READ(7892,50) (TITLE(M),M=1,3)  
        WRITE(2,999)  
        WRITE(2,50) (TITLE(M),M=1,3)  
      ENDIF  
      WRITE(2,60)'* SETTLING VELOCITY AT  ', TIMTMP,  
     &    ' TH DAY FROM MODEL START'  
      READ(7892,999)  
      READ(7892,50) TITLE(1)  
      WRITE(2,50) TITLE(1)  
      IF(NXSP.EQ.0)THEN
        DO I=1,IWQZ  
          READ(7892,*) MM,WQWSC(I),WQWSD(I),WQWSG(I),WQWSRP(I),  
     &        WQWSLP(I),WQWSS(I), WQWSM  
          WRITE(2,51) MM,WQWSC(I),WQWSD(I),WQWSG(I),WQWSRP(I),WQWSLP(I),  
     &        WQWSS(I), WQWSM  
        ENDDO  
      ELSE
        ! x-species require more variables to be exchanged
        DO I=1,IWQZ  
          READ(7892,*) MM,WQWSC(I),WQWSD(I),WQWSG(I),WQWSRP(I),  
     &        WQWSLP(I),WQWSS(I), WQWSM,(WQWSX(I,NSP),NSP=1,NXSP)  
          WRITE(2,51) MM,WQWSC(I),WQWSD(I),WQWSG(I),WQWSRP(I),WQWSLP(I),  
     &        WQWSS(I), WQWSM,(WQWSX(I,NSP),NSP=1,NXSP)   
        ENDDO            
      ENDIF  
      READ(7892,*) STLDAY, STLCONT  
      WRITE(2,*) STLDAY, STLCONT  
      IF(STLCONT.EQ.'END')THEN  
        CLOSE(7892)  
        IWQSTL = 0  
      ENDIF  
      CLOSE(2)  
  999 FORMAT(1X)  
   50 FORMAT(A79)  
   51 FORMAT(I3, 50F8.3)  
   52 FORMAT(I7, 1X, A3)  
   60 FORMAT(/, A24, F5.1, A24)  
      RETURN  
      END  

