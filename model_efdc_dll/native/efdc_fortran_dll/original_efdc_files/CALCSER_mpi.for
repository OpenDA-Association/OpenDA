      SUBROUTINE CALCSER_mpi(ISTL_)
C
C CHANGE RECORD
C ** SUBROUTINE CALPSER UPDATES TIME VARIABLE SALINITY, TEMPERATURE
C ** DYE, SEDIMENT, AND SHELL FISH LARVAE
C ** BOUNDARY CONDITIONS AND INFLOW CONCENTRATIONS
C
      USE GLOBAL
      USE MPI

      IMPLICIT NONE
      INTEGER::NS,K,NT,NTT,ISTL_,M1,M2,NQ
      REAL::TIME,TDIFF,WTM1,WTM2
C
C **  INITIALIZE NULL SERIES CONCENTRATIONS
C
      S1TIME=MPI_TIC()
      NTT=4+NTOX+NSED+NSND
      DO NT=1,NTT
        CQWRSERT(0,NT)=0.
        DO K=1,KC
          CSERT(K,0,NT)=0.
        ENDDO
      ENDDO
      MPI_WTIMES(451)=MPI_WTIMES(451)+MPI_TOC(S1TIME)
C
C **  CONCENTRATION SERIES INTERPOLTATION, SAL,TEM,DYE,SFL
C
      CSERT_TMP=0.
      S1TIME=MPI_TIC()
      DO NC=1,4
        IF(ISTRAN(NC).EQ.0) GOTO 200
!!$OMP PARALLEL DO PRIVATE(TIME,M1,M2,TDIFF,WTM1,WTM2)
        DO NS=1,NCSER(NC)
        IF(IS_CSER(NS,NC))THEN
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
C          DO K=1,KC
          CSERT(:,NS,NC)=WTM1*CSER(M1,:,NS,NC)+WTM2*CSER(M2,:,NS,NC)
C          ENDDO
        ENDIF
        ENDDO
  200   CONTINUE
      ENDDO
      MPI_WTIMES(452)=MPI_WTIMES(452)+MPI_TOC(S1TIME)
C
C **  CONCENTRATION SERIES INTERPOLTATION FOR  TOX
C
      S1TIME=MPI_TIC()
      IF(ISTRAN(5).GE.1)THEN
!!$OMP PARALLEL DO PRIVATE(NC,TIME,M1,M2,TDIFF,WTM1,WTM2)
        DO NT=1,NTOX
          NC=MSVTOX(NT)
          DO NS=1,NCSER(NC)
          IF(IS_CSER(NS,NC))THEN
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
C            DO K=1,KC
            CSERT(:,NS,NC)=WTM1*CSER(M1,:,NS,NC)+WTM2*CSER(M2,:,NS,NC)
C            ENDDO
          ENDIF
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(453)=MPI_WTIMES(453)+MPI_TOC(S1TIME)
C
C **  CONCENTRATION SERIES INTERPOLTATION FOR  SED
C
      S1TIME=MPI_TIC()
      IF(ISTRAN(6).GE.1)THEN
!!$OMP PARALLEL DO PRIVATE(NC,TIME,M1,M2,TDIFF,WTM1,WTM2)
        DO NT=1,NSED
          NC=MSVSED(NT)
          DO NS=1,NCSER(NC)
          IF(IS_CSER(NS,NC))THEN
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
C            DO K=1,KC
            CSERT(:,NS,NC)=WTM1*CSER(M1,:,NS,NC)+WTM2*CSER(M2,:,NS,NC)
C            ENDDO
          ENDIF
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(454)=MPI_WTIMES(454)+MPI_TOC(S1TIME)

C **  CONCENTRATION SERIES INTERPOLTATION FOR  SND
C
      S1TIME=MPI_TIC()
      IF(ISTRAN(7).GE.1)THEN
!!$OMP PARALLEL DO PRIVATE(NC,TIME,M1,M2,TDIFF,WTM1,WTM2)
        DO NT=1,NSND
          NC=MSVSND(NT)
          DO NS=1,NCSER(NC)
          IF(IS_CSER(NS,NC))THEN
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
C            DO K=1,KC
            CSERT(:,NS,NC)=WTM1*CSER(M1,:,NS,NC)+WTM2*CSER(M2,:,NS,NC)
C            ENDDO
          ENDIF
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(455)=MPI_WTIMES(455)+MPI_TOC(S1TIME)
C
C **  CONCENTRATION SERIES INTERPOLTATION FOR WATER QUALITY
C
      IF(ISTRAN(8).GE.1)THEN   ! .AND.IWQPSL.EQ.2)THEN
        S1TIME=MPI_TIC()
!!$OMP PARALLEL DO PRIVATE(NC,TIME,M1,M2,TDIFF,WTM1,WTM2)
        DO NQ=1,NWQV
          NC=4+NTOX+NSED+NSND+NQ
          DO NS=1,NCSER(NC)
C          IF(IS_QSER(NS))THEN
          IF(IS_CSER(NS,NC))THEN
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
C            DO K=1,KC
            CSERT(:,NS,NC)=WTM1*CSER(M1,:,NS,NC)+WTM2*CSER(M2,:,NS,NC)
C            ENDDO
          ENDIF
          ENDDO
        ENDDO
        MPI_WTIMES(456)=MPI_WTIMES(456)+MPI_TOC(S1TIME)
!{ GEOSR x-species. jgcho 2015.11.04
        S1TIME=MPI_TIC()
!!$OMP PARALLEL DO PRIVATE(NC,TIME,M1,M2,TDIFF,WTM1,WTM2)
        DO NQ=1,NXSP
          NC=4+NTOX+NSED+NSND+NWQV+NQ
          DO NS=1,NCSER(NC)
C          IF(IS_QSER(NS))THEN
          IF(IS_CSER(NS,NC))THEN
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
  105       CONTINUE
            M2=M1+1
            IF(TIME.GT.TCSER(M2,NS,NC))THEN
              M1=M2
              GOTO 105
            ELSE
              MCTLAST(NS,NC)=M1
            ENDIF
            TDIFF=TCSER(M2,NS,NC)-TCSER(M1,NS,NC)
            WTM1=(TCSER(M2,NS,NC)-TIME)/TDIFF
            WTM2=(TIME-TCSER(M1,NS,NC))/TDIFF
C            DO K=1,KC
            CSERT(:,NS,NC)=WTM1*CSER(M1,:,NS,NC)+WTM2*CSER(M2,:,NS,NC)
C            ENDDO
          ENDIF
          ENDDO
        ENDDO
        MPI_WTIMES(457)=MPI_WTIMES(457)+MPI_TOC(S1TIME)
!} GEOSR x-species. jgcho 2015.11.04
      ENDIF
C
C **  WRITE DIAGNOSTIC FILE FOR CSER INTERPOLTATION
C
      S1TIME=MPI_TIC()
      IF(ISDIQ.GE.1.AND.N.EQ.1.AND.DEBUG.AND.MYRANK.EQ.0)THEN
        OPEN(1,FILE='CDIAG.OUT',STATUS='UNKNOWN')
        CLOSE(1,STATUS='DELETE')
        OPEN(1,FILE='CDIAG.OUT',STATUS='UNKNOWN')
        DO NC=1,NTT
          WRITE(1,1001)NC
          DO NS=1,NCSER(NC)
            WRITE(1,1002)NS,(CSERT(K,NS,NC),K=1,KC)
          ENDDO
        ENDDO
        CLOSE(1)
      ENDIF
      MPI_WTIMES(458)=MPI_WTIMES(458)+MPI_TOC(S1TIME)
 1001 FORMAT(/' TRANSPORT VARIABLE ID =',I5/)
 1002 FORMAT(I5,2X,12E12.4)
C
C **  SHELL FISH LARVAE BEHAVIOR TIME SERIES INTERPOLTATION
C
      S1TIME=MPI_TIC()
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
      MPI_WTIMES(459)=MPI_WTIMES(459)+MPI_TOC(S1TIME)
C6000 FORMAT('N, CSERT(1),CSERT(KC) = ',I6,4X,2F12.2)
      RETURN
      END

