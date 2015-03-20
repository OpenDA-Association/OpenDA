      SUBROUTINE CALCSER (ISTL_)  
C  
C CHANGE RECORD  
C ** SUBROUTINE CALPSER UPDATES TIME VARIABLE SALINITY, TEMPERATURE  
C ** DYE, SEDIMENT, AND SHELL FISH LARVAE  
C ** BOUNDARY CONDITIONS AND INFLOW CONCENTRATIONS  
C  
      USE GLOBAL  
  
	IMPLICIT NONE
	INTEGER::NS,K,NT,NTT,ISTL_,M1,M2,NQ
	REAL::TIME,TDIFF,WTM1,WTM2
C  
C **  INITIALIZE NULL SERIES CONCENTRATIONS  
C  
      NTT=4+NTOX+NSED+NSND  
      DO NT=1,NTT  
        CQWRSERT(0,NT)=0.  
        DO K=1,KC  
          CSERT(K,0,NT)=0.  
        ENDDO  
      ENDDO  
C  
C **  CONCENTRATION SERIES INTERPOLTATION, SAL,TEM,DYE,SFL  
C  
      DO NC=1,4  
        IF(ISTRAN(NC).EQ.0) GOTO 200  
        DO NS=1,NCSER(NC)  
          IF(ISTL_.EQ.2)THEN  
            IF(ISDYNSTP.EQ.0)THEN  
              TIME=DT*(FLOAT(N)-0.5)/TCCSER(NS,NC)  
     &            +TBEGIN*(TCON/TCCSER(NS,NC))  
            ELSE  
              TIME=TIMESEC/TCCSER(NS,NC)  
            ENDIF  
          ELSE  
            IF(ISDYNSTP.EQ.0)THEN  
              TIME=DT*FLOAT(N-1)/TCCSER(NS,NC)  
     &            +TBEGIN*(TCON/TCCSER(NS,NC))  
            ELSE  
              TIME=TIMESEC/TCCSER(NS,NC)  
            ENDIF  
          ENDIF  
          M1=MCTLAST(NS,NC)  
  100     CONTINUE  
          M2=M1+1  
          IF(TIME.GT.TCSER(M2,NS,NC))THEN  
            M1=M2  
            GOTO 100  
          ELSE  
            MCTLAST(NS,NC)=M1  
          ENDIF  
          TDIFF=TCSER(M2,NS,NC)-TCSER(M1,NS,NC)  
          WTM1=(TCSER(M2,NS,NC)-TIME)/TDIFF  
          WTM2=(TIME-TCSER(M1,NS,NC))/TDIFF  
          DO K=1,KC  
            CSERT(K,NS,NC)=WTM1*CSER(M1,K,NS,NC)+WTM2*CSER(M2,K,NS,NC)  
          ENDDO  
        ENDDO  
  200   CONTINUE  
      ENDDO  
C  
C **  CONCENTRATION SERIES INTERPOLTATION FOR  TOX  
C  
      IF(ISTRAN(5).GE.1)THEN  
        DO NT=1,NTOX  
          NC=MSVTOX(NT)  
          DO NS=1,NCSER(NC)  
            IF(ISTL_.EQ.2)THEN  
              IF(ISDYNSTP.EQ.0)THEN  
                TIME=DT*(FLOAT(N)-0.5)/TCCSER(NS,NC)  
     &              +TBEGIN*(TCON/TCCSER(NS,NC))  
              ELSE  
                TIME=TIMESEC/TCCSER(NS,NC)  
              ENDIF  
            ELSE  
              IF(ISDYNSTP.EQ.0)THEN  
                TIME=DT*FLOAT(N-1)/TCCSER(NS,NC)  
     &              +TBEGIN*(TCON/TCCSER(NS,NC))  
              ELSE  
                TIME=TIMESEC/TCCSER(NS,NC)  
              ENDIF  
            ENDIF  
            M1=MCTLAST(NS,NC)  
  101       CONTINUE  
            M2=M1+1  
            IF(TIME.GT.TCSER(M2,NS,NC))THEN  
              M1=M2  
              GOTO 101  
            ELSE  
              MCTLAST(NS,NC)=M1  
            ENDIF  
            TDIFF=TCSER(M2,NS,NC)-TCSER(M1,NS,NC)  
            WTM1=(TCSER(M2,NS,NC)-TIME)/TDIFF  
            WTM2=(TIME-TCSER(M1,NS,NC))/TDIFF  
            DO K=1,KC  
              CSERT(K,NS,NC)=WTM1*CSER(M1,K,NS,NC)+WTM2*CSER(M2,K,NS,NC)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  CONCENTRATION SERIES INTERPOLTATION FOR  SED  
C  
      IF(ISTRAN(6).GE.1)THEN  
        DO NT=1,NSED  
          NC=MSVSED(NT)  
          DO NS=1,NCSER(NC)  
            IF(ISTL_.EQ.2)THEN  
              IF(ISDYNSTP.EQ.0)THEN  
                TIME=DT*(FLOAT(N)-0.5)/TCCSER(NS,NC)  
     &              +TBEGIN*(TCON/TCCSER(NS,NC))  
              ELSE  
                TIME=TIMESEC/TCCSER(NS,NC)  
              ENDIF  
            ELSE  
              IF(ISDYNSTP.EQ.0)THEN  
                TIME=DT*(FLOAT(N-1))/TCCSER(NS,NC)  
     &              +TBEGIN*(TCON/TCCSER(NS,NC))  
              ELSE  
                TIME=TIMESEC/TCCSER(NS,NC)  
              ENDIF  
            ENDIF  
            M1=MCTLAST(NS,NC)  
  102       CONTINUE  
            M2=M1+1  
            IF(TIME.GT.TCSER(M2,NS,NC))THEN  
              M1=M2  
              GOTO 102  
            ELSE  
              MCTLAST(NS,NC)=M1  
            ENDIF  
            TDIFF=TCSER(M2,NS,NC)-TCSER(M1,NS,NC)  
            WTM1=(TCSER(M2,NS,NC)-TIME)/TDIFF  
            WTM2=(TIME-TCSER(M1,NS,NC))/TDIFF  
            DO K=1,KC  
              CSERT(K,NS,NC)=WTM1*CSER(M1,K,NS,NC)+WTM2*CSER(M2,K,NS,NC)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  CONCENTRATION SERIES INTERPOLTATION FOR  SND  
C  
      IF(ISTRAN(7).GE.1)THEN  
        DO NT=1,NSND  
          NC=MSVSND(NT)  
          DO NS=1,NCSER(NC)  
            IF(ISTL_.EQ.2)THEN  
              IF(ISDYNSTP.EQ.0)THEN  
                TIME=DT*(FLOAT(N)-0.5)/TCCSER(NS,NC)  
     &              +TBEGIN*(TCON/TCCSER(NS,NC))  
              ELSE  
                TIME=TIMESEC/TCCSER(NS,NC)  
              ENDIF  
            ELSE  
              IF(ISDYNSTP.EQ.0)THEN  
                TIME=DT*(FLOAT(N-1))/TCCSER(NS,NC)  
     &              +TBEGIN*(TCON/TCCSER(NS,NC))  
              ELSE  
                TIME=TIMESEC/TCCSER(NS,NC)  
              ENDIF  
            ENDIF  
            M1=MCTLAST(NS,NC)  
  103       CONTINUE  
            M2=M1+1  
            IF(TIME.GT.TCSER(M2,NS,NC))THEN  
              M1=M2  
              GOTO 103  
            ELSE  
              MCTLAST(NS,NC)=M1  
            ENDIF  
            TDIFF=TCSER(M2,NS,NC)-TCSER(M1,NS,NC)  
            WTM1=(TCSER(M2,NS,NC)-TIME)/TDIFF  
            WTM2=(TIME-TCSER(M1,NS,NC))/TDIFF  
            DO K=1,KC  
              CSERT(K,NS,NC)=WTM1*CSER(M1,K,NS,NC)+WTM2*CSER(M2,K,NS,NC)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  

C  
C **  CONCENTRATION SERIES INTERPOLTATION FOR WATER QUALITY
C  
      IF(ISTRAN(8).GE.1)THEN   ! .AND.IWQPSL.EQ.2)THEN  
        DO NQ=1,NWQV  
          NC=4+NTOX+NSED+NSND+NQ  
          DO NS=1,NCSER(NC)  
            IF(ISTL_.EQ.2)THEN  
              IF(ISDYNSTP.EQ.0)THEN 
                TIME=DT*(FLOAT(N)-0.5)/TCCSER(NS,NC)  
     &              +TBEGIN*(TCON/TCCSER(NS,NC))  
              ELSE  
                ! *** VARIABLE TIME STEPPING CURRENTLY NOT OPERATIONAL FOR WQ 
                TIME=TIMESEC/TCCSER(NS,NC)  
              ENDIF  
            ELSE  
              IF(ISDYNSTP.EQ.0)THEN  
                TIME=DT*(FLOAT(N-1))/TCCSER(NS,NC)  
     &              +TBEGIN*(TCON/TCCSER(NS,NC))  
              ELSE  
                TIME=TIMESEC/TCCSER(NS,NC)  
              ENDIF  
            ENDIF  
            M1=MCTLAST(NS,NC)  
  104       CONTINUE  
            M2=M1+1
            IF(TIME.GT.TCSER(M2,NS,NC))THEN  
              M1=M2  
              GOTO 104  
            ELSE  
              MCTLAST(NS,NC)=M1  
            ENDIF  
            TDIFF=TCSER(M2,NS,NC)-TCSER(M1,NS,NC)  
            WTM1=(TCSER(M2,NS,NC)-TIME)/TDIFF  
            WTM2=(TIME-TCSER(M1,NS,NC))/TDIFF  
            DO K=1,KC  
              CSERT(K,NS,NC)=WTM1*CSER(M1,K,NS,NC)+WTM2*CSER(M2,K,NS,NC)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  WRITE DIAGNOSTIC FILE FOR CSER INTERPOLTATION  
C  
      !IF(ISDIQ.GE.1.AND.N.EQ.1.AND.DEBUG)THEN
      IF(ISDIQ.GE.1.AND.N.EQ.N.AND.DEBUG)THEN
        OPEN(1,FILE='CDIAG.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='CDIAG.OUT',STATUS='UNKNOWN')  
        write(1,*) 'n =', n, timeday 
        DO NC=1,NTT + NWQV
          WRITE(1,1001)NC  
          DO NS=1,NCSER(NC)  
            WRITE(1,1002)NS,(CSERT(K,NS,NC),K=1,KC)  
          ENDDO  
        ENDDO  
        CLOSE(1)  
      ENDIF  
 1001 FORMAT(/' TRANSPORT VARIABLE ID =',I5/)  
 1002 FORMAT(I5,2X,12E12.4)  
C  
C **  SHELL FISH LARVAE BEHAVIOR TIME SERIES INTERPOLTATION  
C  
      IF(ISTRAN(4).EQ.0) GOTO 400  
      IF(ISTL_.EQ.2)THEN  
        IF(ISDYNSTP.EQ.0)THEN  
          TIME=DT*(FLOAT(N)-0.5)/TCSFSER  
     &        +TBEGIN*(TCON/TCSFSER)  
        ELSE  
          TIME=TIMESEC/TCSFSER  
        ENDIF  
      ELSE  
        IF(ISDYNSTP.EQ.0)THEN  
          TIME=DT*FLOAT(N-1)/TCSFSER  
     &        +TBEGIN*(TCON/TCSFSER)  
        ELSE  
          TIME=TIMESEC/TCSFSER  
        ENDIF  
      ENDIF  
      M1=MSFTLST  
  300 CONTINUE  
      M2=M1+1  
      IF(TIME.GT.TSFSER(M2))THEN  
        M1=M2  
        GOTO 300  
      ELSE  
        MSFTLST=M1  
      ENDIF  
      TDIFF=TSFSER(M2)-TSFSER(M1)  
      WTM1=(TSFSER(M2)-TIME)/TDIFF  
      WTM2=(TIME-TSFSER(M1))/TDIFF  
      RKDSFLT=WTM1*RKDSFL(M1)+WTM2*RKDSFL(M2)  
      WSFLSTT=WTM1*WSFLST(M1)+WTM2*WSFLST(M2)  
      WSFLSMT=WTM1*WSFLSM(M1)+WTM2*WSFLSM(M2)  
      DSFLMNT=WTM1*DSFLMN(M1)+WTM2*DSFLMN(M2)  
      DSFLMXT=WTM1*DSFLMX(M1)+WTM2*DSFLMX(M2)  
      SFNTBET=WTM1*SFNTBE(M1)+WTM2*SFNTBE(M2)  
      SFATBTT=WTM1*SFATBT(M1)+WTM2*SFATBT(M2)  
  400 CONTINUE  
 6000 FORMAT('N, CSERT(1),CSERT(KC) = ',I6,4X,2F12.2)  
      RETURN  
      END  

