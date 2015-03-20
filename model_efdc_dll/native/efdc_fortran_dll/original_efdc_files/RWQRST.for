      SUBROUTINE RWQRST  
C  
C CHANGE RECORD  
C READ ICS FROM RESTART FILE FROM INWQRST.  
C  
      USE GLOBAL  
      LOGICAL FEXIST  
C  
C CHECK FIRST TO SEE IF BINARY RESTART FILE EXISTS.  IF NOT, USE  
C THE ASCII FILE INSTEAD.  
C  
      LK=(LA-1)*KC  
      INQUIRE(FILE='WQWCRST.BIN', EXIST=FEXIST)  
      IF(.NOT. FEXIST)THEN  
        PRINT *,'WQ: RESTART: WQWCRST.INP'
        OPEN(1,FILE='WQWCRST.INP',STATUS='UNKNOWN')  
        READ(1,999)  
        READ(1,999)  
        NWQV0=NWQV  
        IF(IDNOTRVA.GT.0) NWQV0=NWQV0+1  
        DO M=1,LK  
          READ(1,* ) L,K,(WQV(L,K,NW),NW=1,NWQV0)  
        ENDDO  
        CLOSE(1)  
      ELSE  
        PRINT *,'WQ: RESTART: WQWCRST.BIN'
        OPEN(UNIT=1, FILE='WQWCRST.BIN',  
     &      FORM='UNFORMATTED', STATUS='UNKNOWN')  
        READ(1) NN_, XTIME  
        XTIME=XTIME  
        WRITE(0,911) NN_, XTIME  
  911 FORMAT(' READING BINARY WQWCRST.BIN FILE ...    NN, TIME = ',  
     &    I7, F11.5)  
        NWQV0=NWQV  
        IF(IDNOTRVA.GT.0) NWQV0=NWQV0+1  
        DO M=1,LK  
          READ(1) L, K  
          DO NW=1,NWQV0  
            READ(1) WQV(L, K, NW)  
          ENDDO  
        ENDDO  
        CLOSE(1)  
      ENDIF  
C  
C INITIALIZE MACROALGAE SO BIOMASS ONLY EXISTS IN BOTTOM LAYER:  
C  
      IF(IDNOTRVA.GT.0)THEN  
        IF(KC.GT.1)THEN  
          DO K=2,KC  
            DO L=2,LA  
              WQV(L,K,22)=0.  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
   90 FORMAT(2I5, 21E12.4)  
  999 FORMAT(1X)  
      RETURN  
      END  

