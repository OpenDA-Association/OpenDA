      SUBROUTINE HDMT2T_mpi
C
C **  SUBROUTINE HDMT2T EXECUTES THE FULL HYDRODYNAMIC AND MASS
C **   TRANSPORT TIME INTERGATION USING A TWO TIME LEVEL SCHEME
C
C **  THIS SUBROUTINE IS PART OF  EFDC-FULL VERSION
C
C----------------------------------------------------------------------C
C
C CHANGE RECORD
C DATE MODIFIED     BY                 DATE APPROVED    BY
C
C 05/01/2002        John Hamrick       05/01/2002       John Hamrick
C  modified calls to calbal and budget subroutines
C  added calls to bal2t1, bal2t4, bal2t5
C 05/02/2002        John Hamrick       05/01/2002       John Hamrick
C  modified calculation of cell center bed stress (stored as QQ(l,0))
C  for cells have source/sinks
C 09-22-2004        Paul M. Craig
C  Merged DS and TT versions with the 06-04-2004 TT code
C----------------------------------------------------------------------C
C
C**********************************************************************C
C
      USE GLOBAL
      USE DRIFTER
      USE WINDWAVE ,ONLY:WINDWAVEINIT,WINDWAVETUR
      USE MPI

      REAL TTMP, T1TMP, TMP, T2TMP

      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:)::ISSBCP
      LOGICAL BTEST, LTEST, ERRTEST

      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WCOREW
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WCORNS

      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:)::LCORNER
      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:)::LCORNWE
      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:)::LCORNSN

! { GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2010.11.10
      INTEGER ISHYD,IHYDCNT
      INTEGER ISAVESEDDT
      INTEGER LN
      REAL SNAPSHOTHYD
      INTEGER NTMPVAL
      IHYDCNT=0
      NTMPVAL=0
      SNAPSHOTHYD=0.0
      BTEST=.FALSE.
      LN=0
      ISAVESEDDT=0
! } GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2010.11.10
C
![ykchoi 10.04.26 for linux version
!      INTERFACE TO FUNCTION KBHIT
!     &    [C,ALIAS:'__kbhit']
!     &    ()
!      LOGICAL KBHIT*1
!      END
!      INTERFACE TO FUNCTION GETCH
!     &    [C,ALIAS:'__getch']
!     &    ()
!      INTEGER GETCH*1
!      END
!ykchoi]
C
      IF(.NOT.ALLOCATED(WCOREW))THEN
        ALLOCATE(WCOREW(LCM))
        ALLOCATE(WCORNS(LCM))
        ALLOCATE(LCORNER(LCM))
        ALLOCATE(LCORNWE(LCM))
        ALLOCATE(LCORNSN(LCM))
        ! *** ALLOCATE LOCAL ARRAYS
        WCOREW=0.0
        WCORNS=0.0
        LCORNER=0
        LCORNWE=0
        LCORNSN=0
      ENDIF
C
      CALL CPU_TIME(TTMP)
      ICALLTP=0
C
      ISTL=2
      FOURDPI=4./PI
      ISTL=2
      IS2TL=1
      MPI_WTIMES=0
      CALL ISINPUTS(IS_PSER,IS_CSER,IS_QSER,IS_QCTL)
C
C**********************************************************************C
C
C **  SET FLAGS FOR CORNER CELL BED STRESS CORRECTIONS
C
C *** DSLLC BEGIN BLOCK
      IF(ISCORTBC.GE.1) THEN
C
C **  SET FLAG FOR CELLS HAVING VOLUME SOURCE OR SINKS
C
        IF(.NOT.ALLOCATED(ISSBCP))ALLOCATE(ISSBCP(LCM))
        DO L=1,LC
          ISSBCP(L)=0
        ENDDO
C
        DO L=2,LA
          IF(RSSBCE(L).GT.1.5)ISSBCP(L)=1
          IF(RSSBCW(L).GT.1.5)ISSBCP(L)=1
          IF(RSSBCN(L).GT.1.5)ISSBCP(L)=1
          IF(RSSBCS(L).GT.1.5)ISSBCP(L)=1
        ENDDO
C
      ENDIF
C
      DO L=2,LA
        WCOREST(L)=1.
        WCORWST(L)=1.
        WCORNTH(L)=1.
        WCORSTH(L)=1.
      ENDDO
      ! *** DSLLC
C
C**********************************************************************C
C
C **  REINITIALIZE VARIABLES
C
      DO L=2,LA
        H1P(L)=HP(L)
        H1U(L)=HU(L)
        H1UI(L)=HUI(L)
        H1V(L)=HV(L)
        H1VI(L)=HVI(L)
        UHDY1E(L)=UHDYE(L)
        VHDX1E(L)=VHDXE(L)
      ENDDO
C
      DO K=1,KC
        DO L=2,LA
          U1(L,K)=U(L,K)
          V1(L,K)=V(L,K)
          UHDY1(L,K)=UHDY(L,K)
          VHDX1(L,K)=VHDX(L,K)
        ENDDO
      ENDDO
C
C**********************************************************************C
C
C **  INITIALIZE COURANT NUMBER DIAGNOSTICS
C
      DO K=1,KC
        DO L=2,LA
          CFLUUU(L,K)=0.
          CFLVVV(L,K)=0.
          CFLWWW(L,K)=0.
          CFLCAC(L,K)=0.
        ENDDO
      ENDDO
C
C**********************************************************************C
C
      ILOGC=0
C
C**********************************************************************C
C
C **  CALCULATE U AT V AND V AT U USING ENERGY CONSERVING WEIGHTING
C **  CALCULATE VELOCITY GRADIENTS
C
C----------------------------------------------------------------------C
C
      DO L=2,LA
        LN=LNC(L)
        LS=LSC(L)
        LNW=LNWC(L)
        LSE=LSEC(L)
        LSW=LSWC(L)

        UV(L)=0.25*(HP(LS)*(U(LSE,1)+U(LS,1))
     &      +HP(L)*(U(L+1,1)+U(L,1)))*HVI(L)
        U1V(L)=0.25*(H1P(LS)*(U1(LSE,1)+U1(LS,1))
     &      +H1P(L)*(U1(L+1,1)+U1(L,1)))*H1VI(L)
        VU(L)=0.25*(HP(L-1)*(V(LNW,1)+V(L-1,1))
     &      +HP(L)*(V(LN,1)+V(L,1)))*HUI(L)
        V1U(L)=0.25*(H1P(L-1)*(V1(LNW,1)+V1(L-1,1))
     &      +H1P(L)*(V1(LN,1)+V1(L,1)))*H1UI(L)
        ! *** DSLLC END BLOCK
      ENDDO

C
C**********************************************************************C
C
C **  CALCULATE WAVE BOUNDARY LAYER AND WAVE REYNOLDS STRESS FORCINGS
C
CC      IF(N.EQ.1.AND.MYRANK.EQ.0) PRINT*,'SWITCH ISWAVE',ISWAVE
      IF(ISWAVE.EQ.1) CALL WAVEBL
      IF(ISWAVE.EQ.2) CALL WAVESXY
      IF(ISWAVE.EQ.3.AND.NWSER > 0) THEN
        CALL WINDWAVEINIT
        CALL WINDWAVETUR   !DHC FIRST CALL
      ENDIF
C
C**********************************************************************C
C
C **  FIRST CALL TO INITIALIZE BOTTOM STRESS COEFFICINETS
C
      DTDYN=DT  ! *** PMC - FOR INITIALIZATION
      CALL CALTBXY_mpi(ISTL,IS2TL)
C
C**********************************************************************C
C
C **  CALCULATE HORIZONTAL VISCOSITY AND DIFFUSIVE MOMENTUM FLUXES
C
      IF(ISHDMF.GE.1) CALL CALHDMF_mpi
C
C**********************************************************************C
C
C **  CALCULATE BOTTOM AND SURFACE STRESS AT TIME LEVEL (N-1) AND N
C
C----------------------------------------------------------------------C
C
      N=-1
      CALL CALTSXY_mpi
C
C**********************************************************************C
C
C **  SECOND CALL TO INITIALIZE BOTTOM STRESS COEFFICINETS
C
      CALL CALTBXY_mpi(ISTL,IS2TL)
C
C**********************************************************************C
C
C **  SET BOTTOM AND SURFACE STRESSES
C
C----------------------------------------------------------------------C
C
      DO L=2,LA
        TBX(L)=(AVCON1*HUI(L)+STBX(L)*SQRT(VU(L)*VU(L)
     &      +U(L,1)*U(L,1)))*U(L,1)
        TBY(L)=(AVCON1*HVI(L)+STBY(L)*SQRT(UV(L)*UV(L)
     &      +V(L,1)*V(L,1)))*V(L,1)
      ENDDO
      CALL broadcast_boundary(TBX,ic)
      CALL broadcast_boundary(TBY,ic)
C
      N=0
      CALL CALTSXY_mpi
C
C----------------------------------------------------------------------C
C
C **  SET DEPTH DEVIATION FROM UNIFORM FLOW ON FLOW FACES
C
      DO L=2,LA
        HDFUFX(L)=1.
        HDFUFY(L)=1.
        HDFUF(L)=1.
      ENDDO
C
      IF(ISBSDFUF.GE.1)THEN
        HDFUFM=1.E-12
C
        DO L=2,LA
          LS=LSC(L)
          HDFUFX(L)=HDFUFM+G*SUB(L)*HU(L)*(BELV(L-1)-BELV(L))*DXIU(L)
          HDFUFY(L)=HDFUFM+G*SVB(L)*HV(L)*(BELV(LS )-BELV(L))*DYIV(L)
        ENDDO
C
        DO L=2,LA
          HDFUFX(L)=TBX(L)/HDFUFX(L)
          HDFUFY(L)=TBY(L)/HDFUFY(L)
        ENDDO
C
        DO L=2,LA
          HDFUFX(L)=MAX(HDFUFX(L),-1.0)
          HDFUFY(L)=MAX(HDFUFY(L),-1.0)
        ENDDO
C
        DO L=2,LA
          HDFUFX(L)=MIN(HDFUFX(L),1.0)
          HDFUFY(L)=MIN(HDFUFY(L),1.0)
        ENDDO
C
      ENDIF
C
C**********************************************************************C
C
C **  SET BOTTOM AND SURFACE TURBULENT INTENSITY SQUARED
C
C----------------------------------------------------------------------C
C
      IF(ISWAVE.EQ.0)THEN
C
C----------------------------------------------------------------------c
C
        IF(ISCORTBC.EQ.0) THEN
C
          DO L=2,LA
            TVAR3S(L)=TSY(LNC(L))
            TVAR3W(L)=TSX(L+1)
            TVAR3E(L)=TBX(L+1   )
            TVAR3N(L)=TBY(LNC(L))
          ENDDO
C
          DO L=2,LA
! { GEOSR (IBM request)
            IF (ISNAN(TVAR3S(L))) TVAR3S(L)=0.
            IF (ISNAN(TVAR3W(L))) TVAR3W(L)=0.
            IF (ISNAN(TVAR3E(L))) TVAR3E(L)=0.
            IF (ISNAN(TVAR3N(L))) TVAR3N(L)=0.
            IF (ISNAN(TSY(L))) TSY(L)=0.
            IF (ISNAN(TSX(L))) TSX(L)=0.
            IF (ISNAN(TBY(L))) TBY(L)=0.
            IF (ISNAN(TBX(L))) TBX(L)=0.
! } GEOSR (IBM request)
            QQ(L,0 )=0.5*CTURB2*SQRT(
     &          (RSSBCE(L)*TVAR3E(L)+RSSBCW(L)*TBX(L))**2
     &          +(RSSBCN(L)*TVAR3N(L)+RSSBCS(L)*TBY(L))**2)
            TAUBSED(L)=QQ(L,0 )/CTURB2
            QQ(L,KC)=0.5*CTURB2*SQRT(
     &          (RSSBCE(L)*TVAR3W(L)+RSSBCW(L)*TSX(L))**2
     &          +(RSSBCN(L)*TVAR3S(L)+RSSBCS(L)*TSY(L))**2)
            QQSQR(L,0)=SQRT(QQ(L,0))  ! *** DSLLC
          ENDDO
C
        ENDIF
C
C----------------------------------------------------------------------c
C
        IF(ISCORTBC.GE.1) THEN
C
          IF(DEBUG)THEN
            IF(ISCORTBCD.GE.1)THEN
              OPEN(1,FILE='ADJSTRESSE.OUT')
              CLOSE(1,STATUS='DELETE')
            ENDIF
C
            OPEN(1,FILE='TBCORINIT.OUT')
          ENDIF
C
          DO L=2,LA
            IF(ISSBCP(L).EQ.0)THEN
              IF(SUB(L+1).LT.0.5) WCOREST(L)=FSCORTBCV(L)
              IF(SUB(L).LT.0.5) WCORWST(L)=FSCORTBCV(L)
              IF(SVB(LNC(L)).LT.0.5) WCORNTH(L)=FSCORTBCV(L)
              IF(SVB(L).LT.0.5) WCORSTH(L)=FSCORTBCV(L)
            ENDIF
          ENDDO
C
          DO L=2,LA
            WCOREW(L)=1./(WCOREST(L)+WCORWST(L))
            WCORNS(L)=1./(WCORNTH(L)+WCORSTH(L))
          ENDDO
C
          DO L=2,LA
            WCOREST(L)=WCOREST(L)*WCOREW(L)
            WCORWST(L)=WCORWST(L)*WCOREW(L)
            WCORNTH(L)=WCORNTH(L)*WCORNS(L)
            WCORSTH(L)=WCORSTH(L)*WCORNS(L)
          ENDDO
C
          DO L=2,LA
            TVAR3S(L)=TSY(LNC(L))
            TVAR3W(L)=TSX(L+1)
            TVAR3E(L)=TBX(L+1   )
            TVAR3N(L)=TBY(LNC(L))
          ENDDO
C
          DO L=2,LA
! { GEOSR (IBM request)
            IF (ISNAN(TVAR3S(L))) TVAR3S(L)=0.
            IF (ISNAN(TVAR3W(L))) TVAR3W(L)=0.
            IF (ISNAN(TVAR3E(L))) TVAR3E(L)=0.
            IF (ISNAN(TVAR3N(L))) TVAR3N(L)=0.
            IF (ISNAN(TSY(L))) TSY(L)=0.
            IF (ISNAN(TSX(L))) TSX(L)=0.
            IF (ISNAN(TBY(L))) TBY(L)=0.
            IF (ISNAN(TBX(L))) TBX(L)=0.
! } GEOSR (IBM request)
            QQ(L,0 )=CTURB2*SQRT(
     &          (RSSBCE(L)*WCOREST(L)*TVAR3E(L)
     &          +RSSBCW(L)*WCORWST(L)*TBX(L))**2
     &          +(RSSBCN(L)*WCORNTH(L)*TVAR3N(L)
     &          +RSSBCS(L)*WCORSTH(L)*TBY(L))**2)
            QQ(L,KC)=0.5*CTURB2*SQRT(
     &          (RSSBCE(L)*TVAR3W(L)+RSSBCW(L)*TSX(L))**2
     &          +(RSSBCN(L)*TVAR3S(L)+RSSBCS(L)*TSY(L))**2)
            QQSQR(L,0)=SQRT(QQ(L,0))  ! *** DSLLC
          ENDDO
C
          DO L=2,LA
            TAUBSED(L)=QQ(L,0 )/CTURB2
            TAUBSND(L)=QQ(L,0 )/CTURB2
          ENDDO

          IF(DEBUG)THEN
            DO L=2,LA
              IF(WCORSTH(L).LT.0.49.OR.WCORSTH(L).GT.0.51)THEN
                IF(WCORWST(L).LT.0.49.OR.WCORWST(L).GT.0.51)THEN
                  WRITE(1,3678)IL(L),JL(L),WCORWST(L),WCOREST(L),
     &              WCORSTH(L),WCORNTH(L)
                ENDIF
              ENDIF
            ENDDO
C
            CLOSE(1)
          ENDIF
C
        ENDIF
C
C----------------------------------------------------------------------c
C
      ENDIF
C
C     ENDIF
C
C**********************************************************************C
C
C **  SET BOTTOM AND SURFACE TURBULENT INTENSITY SQUARED
C
C----------------------------------------------------------------------C
C
      IF(ISWAVE.GE.1)THEN
C
        DO L=2,LA
          TVAR3S(L)=TSY(LNC(L))
          TVAR3W(L)=TSX(L+1)
          TVAR3E(L)=TBX(L+1   )
          TVAR3N(L)=TBY(LNC(L))
        ENDDO
C
        DO L=2,LA
! { GEOSR (IBM request)
            IF (ISNAN(TVAR3S(L))) TVAR3S(L)=0.
            IF (ISNAN(TVAR3W(L))) TVAR3W(L)=0.
            IF (ISNAN(TVAR3E(L))) TVAR3E(L)=0.
            IF (ISNAN(TVAR3N(L))) TVAR3N(L)=0.
            IF (ISNAN(TSY(L))) TSY(L)=0.
            IF (ISNAN(TSX(L))) TSX(L)=0.
            IF (ISNAN(TBY(L))) TBY(L)=0.
            IF (ISNAN(TBX(L))) TBX(L)=0.
! } GEOSR (IBM request)
          TAUBC2 = (RSSBCE(L)*TVAR3E(L)+RSSBCW(L)*TBX(L))**2
     &            +(RSSBCN(L)*TVAR3N(L)+RSSBCS(L)*TBY(L))**2
          TAUBC=0.5*SQRT(TAUBC2)
          UTMP=0.5*STCUV(L)*(U(L+1,1)+U(L,1))+1.E-12
          VTMP=0.5*STCUV(L)*(V(LN,1)+V(L,1))
          CURANG=ATAN2(VTMP,UTMP)
          TAUB2=TAUBC*TAUBC+0.5*(QQWV1(L)*QQWV1(L))
     &        +FOURDPI*TAUBC*QQWV1(L)*COS(CURANG-WACCWE(L))
          TAUB2=MAX(TAUB2,0.)
          QQ(L,0 )=CTURB2*SQRT(TAUB2)
          QQ(L,KC)=0.5*CTURB2*SQRT((TVAR3W(L)+TSX(L))**2
     &        +(TVAR3S(L)+TSY(L))**2)
          QQSQR(L,0)=SQRT(QQ(L,0))  ! *** DSLLC
        ENDDO
C
      ENDIF
C
C     ENDIF
C
C**********************************************************************C
C
C **   SET SWITCHES FOR TWO TIME LEVEL INTEGRATION
C
      ISTL=2
      IS2TL=1
      DELT=DT
      DELTD2=DT/2.
      DZDDELT=DZ/DELT
C
C**********************************************************************C
C
C **  BEGIN TIME LOOP FOR FULL HYDRODYNAMIC AND MASS TRANSPORT
C **  CALCULATION
C
C **  SET CYCLE COUNTER AND CALL TIMER
C
      NTIMER=0
      ISSREST=0
      NRESTO=ISRESTO*NTSPTC
      N=0
C
C *** EE BEGIN BLOCK
C **  INITIALZE & RECORD TIME
C
      TIMEDAY=TCON*TBEGIN/86400.
      IF(MYRANK.EQ.0) CALL TIMELOG(0,TIMEDAY)
      IF(ISDYNSTP.GT.0)THEN
        ! *** ALLOW FOR SEDIMENT RAMPUP
        ISAVESEDDT=ISEDDT
        ISEDDT=1
      ENDIF
C
C *** EE END BLOCK
C
      NTIMER=1
      NINCRMT=1
      NLOOP=0
C
      PRINT_SUM=.FALSE.
      IF(PRINT_SUM)THEN
      call collect_in_zero(TSX)
      call collect_in_zero(TSY)
      call collect_in_zero(TBX)
      call collect_in_zero(TBY)
      call collect_in_zero_array(AV)
      call collect_in_zero_array(AB)
      call collect_in_zero_array(AQ)
      call collect_in_zero(HP)
      call collect_in_zero(HU)
      call collect_in_zero(HV)
      call collect_in_zero(P)
      call collect_in_zero_array(U)
      call collect_in_zero_array(V)
      call collect_in_zero_array(W)
      call collect_in_zero_array(TEM)
      call collect_in_zero_array(SEDT)
      do k=0,kcm
         call collect_in_zero(QQ(:,k))
         call collect_in_zero(QQL(:,k))
      enddo
      DO NW=1,NWQV
        call collect_in_zero_array(WQV(:,:,NW))
      ENDDO
      DO NSP=1,NXSP
        call collect_in_zero_array(WQVX(:,:,NSP))
      ENDDO
      IF(MYRANK.EQ.0)THEN
        PRINT*, 0,'TSX  = ', sum(abs(dble(TSX)))
        PRINT*, 0,'TSY  = ', sum(abs(dble(TSY)))
        PRINT*, 0,'TBX  = ', sum(abs(dble(TBX)))
        PRINT*, 0,'TBY  = ', sum(abs(dble(TBY)))
        PRINT*, 0,'AV   = ', sum(abs(dble(AV)))
        PRINT*, 0,'AB   = ', sum(abs(dble(AB)))
        PRINT*, 0,'AQ   = ', sum(abs(dble(AQ)))
        PRINT*, 0,'HP   = ', sum(abs(dble(HP)))
        PRINT*, 0,'HU   = ', sum(abs(dble(HU)))
        PRINT*, 0,'HV   = ', sum(abs(dble(HV)))
        PRINT*, 0,'P    = ', sum(abs(dble(P)))
        PRINT*, 0,'U    = ', sum(abs(dble(U)))
        PRINT*, 0,'V    = ', sum(abs(dble(V)))
        PRINT*, 0,'W    = ', sum(abs(dble(W)))
        PRINT*, 0,'TEM  = ', sum(abs(dble(TEM)))
        PRINT*, 0,'SEDT = ', sum(abs(dble(SEDT)))
        PRINT*, 0,'QQ   = ', sum(abs(dble(QQ)))
        PRINT*, 0,'QQL  = ', sum(abs(dble(QQL)))
        PRINT*, 0,'WQV  = ', sum(abs(dble(WQV)))
        PRINT*, 0,'WQVX = ', sum(abs(dble(WQVX)))
      ENDIF
      ENDIF
C----------------------------------------------------------------------C
C
 1001 CONTINUE
      IF(N.GE.NTS)GO TO 1000
C
C  ITERATION START
      TTIME=MPI_TIC()
      STIME=MPI_TIC()
      IF(ISDYNSTP.EQ.0)THEN
        N=N+1
        ETIMESEC=DT*FLOAT(N)
        ETIMEDAY=DT*FLOAT(N)/86400.
        TIMESEC=(DT*FLOAT(N)+TCON*TBEGIN)
        TIMEDAY=(DT*FLOAT(N)+TCON*TBEGIN)/86400.
      ELSE
        NLOOP=NLOOP+1
        IF(NLOOP.GT.ITRMADJ)THEN
          ISEDDT=ISAVESEDDT ! *** PMC-ALLOW FOR SEDIMENT RAMPUP ALSO
          IF(IDRYTBP.EQ.0)THEN
            CALL CALSTEP
          ELSE
            CALL CALSTEPD
          ENDIF
        ELSE
          DTDYN=DT
          NINCRMT=1
        ENDIF
        DELT=DTDYN
        DELTD2=DTDYN/2.
        DZDDELT=DZ/DTDYN
        N=N+NINCRMT
        ETIMESEC=DT*FLOAT(N)
        ETIMEDAY=(DT*FLOAT(N))/86400.
        TIMESEC=(DT*FLOAT(N)+TCON*TBEGIN)
        TIMEDAY=(DT*FLOAT(N)+TCON*TBEGIN)/86400.
      ENDIF
C
C PMC      IF(ILOGC.EQ.NTSMMT)THEN
      IF(ILOGC.EQ.NTSPTC.AND.MYRANK.EQ.0)THEN
        CLOSE(8,STATUS='DELETE')
        OPEN(8,FILE='EFDCLOG.OUT',STATUS='UNKNOWN')
        IF(DEBUG)THEN
          IF(ISDRY.GT.0)THEN
            OPEN(1,FILE='DRYWET.LOG',STATUS='UNKNOWN')
            CLOSE(1,STATUS='DELETE')
          ENDIF
          IF(ISCFL.EQ.1)THEN
            OPEN(1,FILE='CFL.OUT',STATUS='UNKNOWN')
            CLOSE(1,STATUS='DELETE')
          ENDIF
        ENDIF
        ILOGC=0
      ENDIF
C
      IF(ISDYNSTP.EQ.0)THEN
        ILOGC=ILOGC+1
      ELSE
        ILOGC=ILOGC+NINCRMT
      ENDIF
C
C *** DSLLC BEGIN BLOCK
      IF(N.LE.NLTS)THEN
        SNLT=0.
      ELSEIF(N.GT.NLTS.AND.N.LE.NTTS)THEN
        NTMP1=N-NLTS
        NTMP2=NTTS-NLTS+1
        SNLT=FLOAT(NTMP1)/FLOAT(NTMP2)
      ELSE
        SNLT=1.
      ENDIF

      ! *** TURN OFF WIND SHELTERING FOR ICE CONDITIONS (TO BE REPLACED AFTER FULL ICE SUBMODEL ADDED)
      IF(WINTER_END > WINTER_START)THEN
        IF(TIMEDAY > WINTER_START)THEN
          IF(WINDSTKA_SAVE(1)==0.)THEN
            ! *** TOGGLE OFF THE WIND SHELTERING COEFFICIENTS
            DO L=2,LA
              WINDSTKA_SAVE(L)=WINDSTKA(L)
              WINDSTKA(L)=0.
            ENDDO
            WINDSTKA_SAVE(1) = 1.
          ENDIF
          IF(TIMEDAY > WINTER_END)THEN
            ! *** TOGGLE ON THE WIND SHELTERING COEFFICIENTS
            DO L=2,LA
              WINDSTKA(L) = WINDSTKA_SAVE(L)
            ENDDO
            WINDSTKA_SAVE(1) = 0.
            WINTER_START = WINTER_START+365.
            WINTER_END = WINTER_END+365.
          ENDIF
        ENDIF
      ENDIF
C *** DSLLC END BLOCK
C
      IF(N.LE.NTSVB)THEN
        GP=GPO*(FLOAT(N)/FLOAT(NTSVB))
      ELSE
        GP=GPO
      ENDIF
C
      MPI_WTIMES(2)=MPI_WTIMES(2)+MPI_TOC(STIME)
C
C----------------------------------------------------------------------C
C
C **  INITIALIZE TWO-TIME LEVEL BALANCES
C
      STIME=MPI_TIC()
C
      IF(IS2TIM.GE.1) THEN
        IF(ISBAL.GE.1)THEN
          CALL BAL2T1
        ENDIF
      ENDIF
C
      MPI_WTIMES(3)=MPI_WTIMES(3)+MPI_TOC(STIME)
C
C----------------------------------------------------------------------C
C
C **  REENTER HERE FOR TWO TIME LEVEL LOOP
C
C 500 CONTINUE
C
C**********************************************************************C
C
C **  CALCULATE VERTICAL VISCOSITY AND DIFFUSIVITY AT TIME LEVEL (N)
C
      STIME=MPI_TIC()
C
      IF(KC.GT.1)THEN
        IF(ISQQ.EQ.1)THEN
          IF(MYRANK.EQ.0)THEN
          ENDIF
          IF(ISTOPT(0).EQ.0)CALL CALAVBOLD_mpi (ISTL)
          IF(ISTOPT(0).GE.1)CALL CALAVB_mpi (ISTL)
        ENDIF
        IF(MYRANK.EQ.0)THEN
        ENDIF
        IF(ISQQ.EQ.2) CALL CALAVB2 (ISTL)
      ENDIF
C
      IF(.FALSE.)THEN
      call collect_in_zero_array(AVUI)
      call collect_in_zero_array(AVVI)
      call collect_in_zero_array(AV  )
      call collect_in_zero_array(AB  )
      call collect_in_zero_array(AQ  )
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'AVUI  = ', sum(abs(dble(AVUI )))
        PRINT*, n,'AVVI  = ', sum(abs(dble(AVVI )))
        PRINT*, n,'AV    = ', sum(abs(dble(AV   )))
        PRINT*, n,'AB    = ', sum(abs(dble(AB   )))
        PRINT*, n,'AQ    = ', sum(abs(dble(AQ   )))
      ENDIF
      ENDIF
C
      MPI_WTIMES(4)=MPI_WTIMES(4)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  CALCULATE WAVE BOUNDARY LAYER AND WAVE REYNOLDS STRESS FORCINGS
C
      STIME=MPI_TIC()
C
      IF(ISWAVE.EQ.1) CALL WAVEBL
      IF(ISWAVE.EQ.2) CALL WAVESXY
      IF(ISWAVE.EQ.3.AND.NWSER > 0) CALL WINDWAVETUR   !DHC NEXT CALL
C
      MPI_WTIMES(5)=MPI_WTIMES(5)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  ADVANCE TIME VARIABLE SURFACE WIND STRESS AND UPDATE NEW WIND
C **  STRESSES  *** DSLLC MOVED
C
C----------------------------------------------------------------------C
C
      STIME=MPI_TIC()
C
      CALL CALTSXY_mpi
C
      IF(.FALSE.)THEN
      call collect_in_zero(TSX  )
      call collect_in_zero(TSY  )
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'TSX   = ', sum(abs(dble(TSX )))
        PRINT*, n,'TSY   = ', sum(abs(dble(TSY )))
      ENDIF
      ENDIF
C
      MPI_WTIMES(6)=MPI_WTIMES(6)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  CALCULATE EXPLICIT MOMENTUM EQUATION TERMS
C
      STIME=MPI_TIC()
C
      IF(IS2TIM.EQ.1) CALL CALEXP2T_mpi
      IF(IS2TIM.EQ.2) CALL CALIMP2T
C
      IF(.FALSE.)THEN
      call collect_in_zero(FCAXE)
      call collect_in_zero(FCAYE)
      call collect_in_zero(FXE  )
      call collect_in_zero(FYE  )
      call collect_in_zero_array(FX  )
      call collect_in_zero_array(FY  )
      call collect_in_zero_array(FBBX)
      call collect_in_zero_array(FBBY)
      call collect_in_zero_array(DU  )
      call collect_in_zero_array(DV  )
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'FCAXE = ', sum(abs(dble(FCAXE)))
        PRINT*, n,'FCAYE = ', sum(abs(dble(FCAYE)))
        PRINT*, n,'FXE   = ', sum(abs(dble(FXE  )))
        PRINT*, n,'FYE   = ', sum(abs(dble(FYE  )))
        PRINT*, n,'FX    = ', sum(abs(dble(FX   )))
        PRINT*, n,'FY    = ', sum(abs(dble(FY   )))
        PRINT*, n,'FBBX  = ', sum(abs(dble(FBBX )))
        PRINT*, n,'FBBY  = ', sum(abs(dble(FBBY )))
        PRINT*, n,'DU    = ', sum(abs(dble(DU   )))
        PRINT*, n,'DV    = ', sum(abs(dble(DV   )))
      ENDIF
      ENDIF
      MPI_WTIMES(7)=MPI_WTIMES(7)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  UPDATE TIME VARIABLE VOLUME SOURCES AND SINKS, CONCENTRATIONS,
C **  VEGETATION CHARACTERISTICS AND SURFACE ELEVATIONS
C
      STIME=MPI_TIC()
C
      CALL CALCSER_mpi (ISTL)
      CALL CALVEGSER_mpi (ISTL)
      CALL CALQVS_mpi (ISTL)
      PSERT(0)=0.
      IF(NPSER.GE.1) CALL CALPSER_mpi (ISTL)
C
      MPI_WTIMES(8)=MPI_WTIMES(8)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  SOLVE EXTERNAL MODE EQUATIONS FOR P, UHDYE, AND VHDXE
C
      STIME=MPI_TIC()
C
      IF(ISCHAN.EQ.0.AND.ISDRY.EQ.0) CALL CALPUV2T
      IF(ISCHAN.GE.1.OR.ISDRY.GE.1) CALL CALPUV2C_mpi
C
      MPI_WTIMES(9)=MPI_WTIMES(9)+MPI_TOC(STIME)
C
      IF(.FALSE.)THEN
      call collect_in_zero(UHDYE)
      call collect_in_zero(VHDXE)
      call collect_in_zero(HU   )
      call collect_in_zero(HV   )
      call collect_in_zero(P    )
      call collect_in_zero(TBX  )
      call collect_in_zero(TBY  )
      call collect_in_zero(FCAXE)
      call collect_in_zero(FCAYE)
      call collect_in_zero(FPGXE)
      call collect_in_zero(FPGYE)
      call collect_in_zero(FXE  )
      call collect_in_zero(FYE  )
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'UHDYE = ', sum(abs(dble(UHDYE)))
        PRINT*, n,'VHDXE = ', sum(abs(dble(VHDXE)))
        PRINT*, n,'HU    = ', sum(abs(dble(HU   )))
        PRINT*, n,'HV    = ', sum(abs(dble(HV   )))
        PRINT*, n,'P     = ', sum(abs(dble(P    )))
        PRINT*, n,'TBX   = ', sum(abs(dble(TBX  )))
        PRINT*, n,'TBY   = ', sum(abs(dble(TBY  )))
        PRINT*, n,'FCAXE = ', sum(abs(dble(FCAXE)))
        PRINT*, n,'FCAYE = ', sum(abs(dble(FCAYE)))
        PRINT*, n,'FPGXE = ', sum(abs(dble(FPGXE)))
        PRINT*, n,'FPGYE = ', sum(abs(dble(FPGYE)))
        PRINT*, n,'FXE   = ', sum(abs(dble(FXE  )))
        PRINT*, n,'FYE   = ', sum(abs(dble(FYE  )))
      ENDIF
      ENDIF
C
      STIME=MPI_TIC()
      CALL MPI_MASKDRY
      MPI_WTIMES(62)=MPI_WTIMES(62)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  WRITE DIAGNOSTICS
C
C----------------------------------------------------------------------C
C
C **  DTIME AND FLUSH ARE SUPPORTED ON SUN SYSTEMS, BUT MAY NOT BE
C **  SUPPORTED ON OTHER SYSTEMS.
C
      STIME=MPI_TIC()
C
      IF(ISLOG.GE.1.AND.MYRANK.EQ.0)THEN
        WRITE(8,17)N,ITER,RSQ,CFMAX,AVMAX,ABMIN,ABMAX,ABMIN
      ENDIF
C
   17 FORMAT('  N,ITER,RSQ,CFMAX,AVMAX,AVMIN,ABMAX,ABMIN',
     &    I7,I5,2E12.4,4(1X,F8.4))
C
      ERRMAX=MAX(ERRMAX,ERR)
      ERRMIN=MIN(ERRMIN,ERR)
      ITRMAX=MAX(ITRMAX,ITER)
      IRRMIN=MIN(ITRMIN,ITER)
C
      MPI_WTIMES(48)=MPI_WTIMES(48)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  ADVANCE INTERNAL VARIABLES
C
C----------------------------------------------------------------------C
      STIME=MPI_TIC()
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      MPI_WTIMES(63)=MPI_WTIMES(63)+MPI_TOC(STIME)
C
      STIME=MPI_TIC()
C
      DO K=1,KC
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          UHDY2(L,K)=UHDY1(L,K)
          UHDY1(L,K)=UHDY(L,K)
          VHDX2(L,K)=VHDX1(L,K)
          VHDX1(L,K)=VHDX(L,K)
          U2(L,K)=U1(L,K)
          V2(L,K)=V1(L,K)
          U1(L,K)=U(L,K)
          V1(L,K)=V(L,K)
          W2(L,K)=W1(L,K)
          W1(L,K)=W(L,K)
        ENDDO
      ENDDO
C
      MPI_WTIMES(10)=MPI_WTIMES(10)+MPI_TOC(STIME)
C
      STIME=MPI_TIC()
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      MPI_WTIMES(61)=MPI_WTIMES(61)+MPI_TOC(STIME)
      STIME=MPI_TIC()
      call broadcast_boundary_array(UHDY2,IC)
      call broadcast_boundary_array(UHDY1,IC)
      MPI_WTIMES(51)=MPI_WTIMES(51)+MPI_TOC(STIME)
      STIME=MPI_TIC()
      call broadcast_boundary_array(VHDX2,IC)
      call broadcast_boundary_array(VHDX1,IC)
      MPI_WTIMES(52)=MPI_WTIMES(52)+MPI_TOC(STIME)
      STIME=MPI_TIC()
      call broadcast_boundary_array(U2,IC)
      call broadcast_boundary_array(V2,IC)
      MPI_WTIMES(53)=MPI_WTIMES(53)+MPI_TOC(STIME)
      STIME=MPI_TIC()
      call broadcast_boundary_array(U1,IC)
      call broadcast_boundary_array(V1,IC)
      MPI_WTIMES(54)=MPI_WTIMES(54)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  SOLVE INTERNAL SHEAR MODE EQUATIONS FOR U, UHDY, V, VHDX, AND W
C
C----------------------------------------------------------------------C
C
      STIME=MPI_TIC()
C
      IF(KC.GT.1)THEN
        CALL CALUVW_mpi (ISTL,IS2TL)
      ELSE
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          UHDY(L,1)=UHDYE(L)
          U(L,1)=UHDYE(L)*HUI(L)*DYIU(L)
          VHDX(L,1)=VHDXE(L)
          V(L,1)=VHDXE(L)*HVI(L)*DXIV(L)
          W(L,1)=0.
        ENDDO
        CALL CALUVW_mpi (ISTL,IS2TL)
      ENDIF
      call broadcast_boundary_array(U,ic)
      call broadcast_boundary_array(V,ic)
C
      MPI_WTIMES(11)=MPI_WTIMES(11)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  CALCULATE SALINITY, TEMPERATURE, DYE AND SEDIMENT CONCENTRATIONS
C **  AT TIME LEVEL (N+1)
C
C----------------------------------------------------------------------C
C
      STIME=MPI_TIC()
      CALL CALCONC_mpi (ISTL,IS2TL)
      MPI_WTIMES(12)=MPI_WTIMES(12)+MPI_TOC(STIME)
C
C----------------------------------------------------------------------C
C
C
      ! *** PMC BYPASS IF NOT SIMULATING SEDIMENTS
      STIME=MPI_TIC()
C
      IF(ISTRAN(6).GT.0.OR.ISTRAN(7).GT.0)THEN
        S1TIME=MPI_TIC()
        DO K=1,KB
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            SEDBT(L,K)=0.
            SNDBT(L,K)=0.
          ENDDO
        ENDDO
        MPI_WTIMES(551)=MPI_WTIMES(551)+MPI_TOC(S1TIME)
C
        S1TIME=MPI_TIC()
        DO NS=1,NSED
          DO K=1,KB
!$OMP PARALLEL DO
            DO L=LMPI1,LMPILC
              SEDBT(L,K)=SEDBT(L,K)+SEDB(L,K,NS)
            ENDDO
          ENDDO
        ENDDO
        MPI_WTIMES(552)=MPI_WTIMES(552)+MPI_TOC(S1TIME)
C
        S1TIME=MPI_TIC()
        DO NS=1,NSND
          DO K=1,KB
!$OMP PARALLEL DO
            DO L=LMPI1,LMPILC
              SNDBT(L,K)=SNDBT(L,K)+SNDB(L,K,NS)
            ENDDO
          ENDDO
        ENDDO
        MPI_WTIMES(553)=MPI_WTIMES(553)+MPI_TOC(S1TIME)
C
        S1TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            SEDT(L,K)=0.
            SNDT(L,K)=0.
          ENDDO
        ENDDO
        MPI_WTIMES(554)=MPI_WTIMES(554)+MPI_TOC(S1TIME)
C
        S1TIME=MPI_TIC()
        DO NS=1,NSED
          DO K=1,KC
!$OMP PARALLEL DO
            DO L=LMPI1,LMPILC
              SEDT(L,K)=SEDT(L,K)+SED(L,K,NS)
            ENDDO
          ENDDO
        ENDDO
        MPI_WTIMES(555)=MPI_WTIMES(555)+MPI_TOC(S1TIME)
C
        S1TIME=MPI_TIC()
        DO NS=1,NSND
          DO K=1,KC
!$OMP PARALLEL DO
            DO L=LMPI1,LMPILC
              SNDT(L,K)=SNDT(L,K)+SND(L,K,NS)
            ENDDO
          ENDDO
        ENDDO
        MPI_WTIMES(556)=MPI_WTIMES(556)+MPI_TOC(S1TIME)
      ENDIF
C
      MPI_WTIMES(13)=MPI_WTIMES(13)+MPI_TOC(STIME)
C
C----------------------------------------------------------------------C
C
C **  CHECK RANGE OF SALINITY AND DYE CONCENTRATION
C
      STIME=MPI_TIC()
C
      IF(ISMMC.EQ.1)THEN
C
        SALMAX=-100000.
        SALMIN=100000.
        DO K=1,KC
          DO L=2,LA
            IF(SAL(L,K).GT.SALMAX)THEN
              SALMAX=SAL(L,K)
              IMAX=IL(L)
              JMAX=JL(L)
              KMAX=K
            ENDIF
            IF(SAL(L,K).LT.SALMIN)THEN
              SALMIN=SAL(L,K)
              IMIN=IL(L)
              JMIN=JL(L)
              KMIN=K
            ENDIF
          ENDDO
        ENDDO
C
        IF(MYRANK.EQ.0) WRITE(6,6001)N
        IF(MYRANK.EQ.0) WRITE(6,6002)SALMAX,IMAX,JMAX,KMAX
        IF(MYRANK.EQ.0) WRITE(6,6003)SALMIN,IMIN,JMIN,KMIN
C
        SALMAX=-100000.
        SALMIN=100000.
        DO K=1,KC
          DO L=2,LA
            IF(DYE(L,K).GT.SALMAX)THEN
              SALMAX=DYE(L,K)
              IMAX=IL(L)
              JMAX=JL(L)
              KMAX=K
            ENDIF
            IF(DYE(L,K).LT.SALMIN)THEN
              SALMIN=DYE(L,K)
              IMIN=IL(L)
              JMIN=JL(L)
              KMIN=K
            ENDIF
          ENDDO
        ENDDO
C
        IF(MYRANK.EQ.0) WRITE(6,6004)SALMAX,IMAX,JMAX,KMAX
        IF(MYRANK.EQ.0) WRITE(6,6005)SALMIN,IMIN,JMIN,KMIN
C
        IF(MYRANK.EQ.0) WRITE(8,6004)SALMAX,IMAX,JMAX,KMAX
        IF(MYRANK.EQ.0) WRITE(8,6005)SALMIN,IMIN,JMIN,KMIN
C
        SALMAX=-100000.
        SALMIN=100000.
        DO K=1,KC
          DO L=2,LA
            IF(SFL(L,K).GT.SALMAX)THEN
              SALMAX=SFL(L,K)
              IMAX=IL(L)
              JMAX=JL(L)
              KMAX=K
            ENDIF
            IF(SFL(L,K).LT.SALMIN)THEN
              SALMIN=SFL(L,K)
              IMIN=IL(L)
              JMIN=JL(L)
              KMIN=K
            ENDIF
          ENDDO
        ENDDO
C
        WRITE(6,6006)SALMAX,IMAX,JMAX,KMAX
        WRITE(6,6007)SALMIN,IMIN,JMIN,KMIN
C
      ENDIF
C
C
      IF(ISMMC.EQ.2)THEN
C
        SALMAX=-100000.
        SALMIN=100000.
        DO K=1,KC
          DO L=2,LA
            IF(TEM(L,K).GT.SALMAX)THEN
              SALMAX=TEM(L,K)
              IMAX=IL(L)
              JMAX=JL(L)
              KMAX=K
            ENDIF
            IF(TEM(L,K).LT.SALMIN)THEN
              SALMIN=TEM(L,K)
              IMIN=IL(L)
              JMIN=JL(L)
              KMIN=K
            ENDIF
          ENDDO
        ENDDO
C
        WRITE(6,6001)N
        WRITE(6,6008)SALMAX,IMAX,JMAX,KMAX
        WRITE(6,6009)SALMIN,IMIN,JMIN,KMIN
C
      ENDIF
C
      MPI_WTIMES(14)=MPI_WTIMES(14)+MPI_TOC(STIME)
C
 6001 FORMAT('  N=',I10)
 6002 FORMAT('  SALMAX=',F14.4,5X,'I,J,K=',(3I10))
 6003 FORMAT('  SALMIN=',F14.4,5X,'I,J,K=',(3I10))
 6004 FORMAT('  DYEMAX=',F14.4,5X,'I,J,K=',(3I10))
 6005 FORMAT('  DYEMIN=',F14.4,5X,'I,J,K=',(3I10))
 6006 FORMAT('  SFLMAX=',F14.4,5X,'I,J,K=',(3I10))
 6007 FORMAT('  SFLMIN=',F14.4,5X,'I,J,K=',(3I10))
 6008 FORMAT('  TEMMAX=',F14.4,5X,'I,J,K=',(3I10))
 6009 FORMAT('  TEMMIN=',F14.4,5X,'I,J,K=',(3I10))

      STIME=MPI_TIC()
      ! *** DSLLC
      IF(DEBUG.AND.MYRANK.EQ.-1)THEN
        S1TIME=MPI_TIC()
        BTEST=.FALSE.
        LTEST=.FALSE.
        DO L=2,LA
          IF(ISNAN(HP(L)))THEN
            BTEST=.TRUE.
            IF(.NOT.LTEST)THEN
              OPEN(1,FILE='ERROR.LOG',POSITION='APPEND',
     &             STATUS='UNKNOWN')
              WRITE(1,*)' * DEBUG: ERROR IN DEPTH VARIABLES'
            ENDIF
            WRITE(1,910) TIMEDAY, L, IL(L), JL(L),
     &                   HP(L),H1P(L)
            HP(L)=H1P(L)
            LTEST=.TRUE.
          ENDIF
        ENDDO
        IF(LTEST)CLOSE(1,STATUS='KEEP')
        MPI_WTIMES(571)=MPI_WTIMES(571)+MPI_TOC(S1TIME)

        S1TIME=MPI_TIC()
        LTEST=.FALSE.
        IF(KC.GT.1)THEN
          DO L=2,LA
            DO K=1,KC
              IF(ISNAN(AV(L,K)))THEN
                BTEST=.TRUE.
                IF(.NOT.LTEST)THEN
                  OPEN(1,FILE='ERROR.LOG',POSITION='APPEND',
     &                 STATUS='UNKNOWN')
                  WRITE(1,*)' * DEBUG: ERROR IN VERTICAL VISCOSITY'
                ENDIF
                WRITE(1,9101) TIMEDAY, L, IL(L), JL(L), K, 'AV ',
     &                       AV(L,K)
                LTEST=.TRUE.
              ENDIF
            ENDDO
          ENDDO
        ENDIF
        IF(LTEST)CLOSE(1,STATUS='KEEP')
        MPI_WTIMES(572)=MPI_WTIMES(572)+MPI_TOC(S1TIME)

        S1TIME=MPI_TIC()
        LTEST=.FALSE.
        IF(ISTRAN(1).GE.1)THEN
          DO L=2,LA
            DO K=1,KC
              IF(ISNAN(SAL(L,K)))THEN
                BTEST=.TRUE.
                IF(.NOT.LTEST)THEN
                  OPEN(1,FILE='ERROR.LOG',POSITION='APPEND',
     &                 STATUS='UNKNOWN')
                  WRITE(1,*)' * DEBUG: ERROR IN SALINITY VARIABLES'
                ENDIF
                WRITE(1,911) TIMEDAY, L, IL(L), JL(L), K,
     &                       SAL(L,K),SAL1(L,K)
                SAL(L,K)=SAL1(L,K)
                LTEST=.TRUE.
              ENDIF
            ENDDO
          ENDDO
        ENDIF
        IF(LTEST)CLOSE(1,STATUS='KEEP')
        MPI_WTIMES(573)=MPI_WTIMES(573)+MPI_TOC(S1TIME)
C
        S1TIME=MPI_TIC()
        LTEST=.FALSE.
        IF(ISTRAN(2).GE.1)THEN
          DO L=2,LA
            DO K=1,KC
              IF(ISNAN(TEM(L,K)))THEN
                BTEST=.TRUE.
                IF(.NOT.LTEST)THEN
                  OPEN(1,FILE='ERROR.LOG',POSITION='APPEND',
     &                 STATUS='UNKNOWN')
                  WRITE(1,*)' * DEBUG: ERROR IN TEMPERATURE VARIABLES'
                ENDIF
                WRITE(1,912) TIMEDAY, L, IL(L), JL(L), K,
     &                       TEM(L,K),TEM1(L,K)
                TEM(L,K)=TEM1(L,K)
                LTEST=.TRUE.
              ENDIF
            ENDDO
          ENDDO
        ENDIF
        IF(LTEST)CLOSE(1,STATUS='KEEP')
        MPI_WTIMES(574)=MPI_WTIMES(574)+MPI_TOC(S1TIME)

!{ GEOSR 2012.8.30 jgcho
        S1TIME=MPI_TIC()
        LTEST=.FALSE.
        IF(ISTRAN(3).GE.1)THEN
          DO L=2,LA
            DO K=1,KC
              IF(ISNAN(DYE(L,K)))THEN
                BTEST=.TRUE.
                IF(.NOT.LTEST)THEN
                  OPEN(1,FILE='ERROR.LOG',POSITION='APPEND',
     &                 STATUS='UNKNOWN')
                  WRITE(1,*)' * DEBUG: ERROR IN DYE VARIABLES'
                ENDIF
                WRITE(1,912) TIMEDAY, L, IL(L), JL(L), K,
     &                       DYE(L,K),DYE1(L,K)
                DYE(L,K)=DYE1(L,K)
                LTEST=.TRUE.
              ENDIF
            ENDDO
          ENDDO
        ENDIF
        IF(LTEST)CLOSE(1,STATUS='KEEP')
        MPI_WTIMES(575)=MPI_WTIMES(575)+MPI_TOC(S1TIME)

        S1TIME=MPI_TIC()
        LTEST=.FALSE.
        IF(ISTRAN(6).GE.1)THEN
          ERRTEST=.FALSE.
          DO NS=1,NSED
            DO K=1,KC
              DO L=LMPI2,LMPILA
                IF(ISNAN(SED(L,K,NS)))THEN
                  ERRTEST=.TRUE.
                ENDIF
              ENDDO
            ENDDO
          ENDDO
          CALL MPI_ALLREDUCE(ERRTEST,MPI_LG,1,MPI_LOGICAL,MPI_LOR,
     &                       MPI_COMM_WORLD,IERR)
          ERRTEST=MPI_LG
          IF(ERRTEST)THEN
          DO NS=1,NSED
            DO K=1,KC
              DO L=2,LA
                IF(ISNAN(SED(L,K,NS)))THEN
                  BTEST=.TRUE.
                  IF(.NOT.LTEST)THEN
                    OPEN(1,FILE='ERROR.LOG',POSITION='APPEND',
     &                 STATUS='UNKNOWN')
                    WRITE(1,*)' * DEBUG: ERROR IN SED VARIABLES'
                  ENDIF
                  WRITE(1,916) TIMEDAY, L, IL(L), JL(L), K, NS,
     &                         SED(L,K,NS),SED1(L,K,NS)
                  SED(L,K,NS)=SED1(L,K,NS)
                  LTEST=.TRUE.
                ENDIF
              ENDDO
            ENDDO
          ENDDO
          ENDIF
        ENDIF
        IF(LTEST)CLOSE(1,STATUS='KEEP')
        MPI_WTIMES(577)=MPI_WTIMES(577)+MPI_TOC(S1TIME)

        S1TIME=MPI_TIC()
        LTEST=.FALSE.
        IF(ISTRAN(7).GE.1)THEN
          DO L=2,LA
            DO K=1,KC
              DO NS=1,NSND
                IF(ISNAN(SND(L,K,NS)))THEN
                  BTEST=.TRUE.
                  IF(.NOT.LTEST)THEN
                    OPEN(1,FILE='ERROR.LOG',POSITION='APPEND',
     &                   STATUS='UNKNOWN')
                    WRITE(1,*)' * DEBUG: ERROR IN SAND VARIABLES'
                  ENDIF
                  WRITE(1,917) TIMEDAY, L, IL(L), JL(L), K, NS,
     &                         SND(L,K,NS),SND(L,K,NS)
                  SND(L,K,NS)=SND1(L,K,NS)
                  LTEST=.TRUE.
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        IF(LTEST)CLOSE(1,STATUS='KEEP')
        MPI_WTIMES(577)=MPI_WTIMES(577)+MPI_TOC(S1TIME)

        S1TIME=MPI_TIC()
        LTEST=.FALSE.
        IF(ISTRAN(8).GE.1)THEN
          ERRTEST=.FALSE.
          DO NW=1,21
            DO K=1,KC
              DO L=LMPI2,LMPILA
                IF(ISNAN(WQV(L,K,NW)))THEN
                  ERRTEST=.TRUE.
                ENDIF
              ENDDO
            ENDDO
          ENDDO
          CALL MPI_ALLREDUCE(ERRTEST,MPI_LG,1,MPI_LOGICAL,MPI_LOR,
     &                       MPI_COMM_WORLD,IERR)
          ERRTEST=MPI_LG
          IF(ERRTEST)THEN
          DO NW=1,21
            DO K=1,KC
              DO L=2,LA
                IF(ISNAN(WQV(L,K,NW)))THEN
                  BTEST=.TRUE.
                  OPEN(1,FILE='ERROR.LOG',POSITION='APPEND',
     &                 STATUS='UNKNOWN')
                  WRITE(1,*)' * DEBUG: ERROR IN WATER QUALITY VARIABLES'
                  WRITE(1,918) TIMEDAY, L, IL(L), JL(L), K, NW,
     &                         WQV(L,K,NW),WQVO(L,K,NW)
                  CLOSE(1,STATUS='KEEP')
                  WQV(L,K,NW)=WQVO(L,K,NW)
                  LTEST=.TRUE.
                ENDIF
              ENDDO
            ENDDO
          ENDDO
          ENDIF
        ENDIF
        IF(LTEST)CLOSE(1,STATUS='KEEP')
        MPI_WTIMES(579)=MPI_WTIMES(579)+MPI_TOC(S1TIME)
      ENDIF

  910 FORMAT('ERROR: TIME, L, I, J, HP = ',        F10.5,3I6,2F10.4)
 9101 FORMAT('ERROR: TIME, L, I, J, K, ',A3,' = ', F10.5,4I6,2F10.4)
  911 FORMAT('ERROR: TIME, L, I, J, K, SAL = ',    F10.5,4I6,2F10.4)
  912 FORMAT('ERROR: TIME, L, I, J, K, TEM = ',    F10.5,4I6,2F10.4)
  916 FORMAT('ERROR: TIME, L, I, J, K, NS, SED = ',F10.5,5I6,2F10.4)
  917 FORMAT('ERROR: TIME, L, I, J, K, NX, SND = ',F10.5,5I6,2F10.4)
  918 FORMAT('ERROR: TIME, L, I, J, K, NW, WQV = ',F10.5,5I6,2F10.4)

        ! *** DUMP THE RESULTS (JUST PRIOR) TO EE FOR ANALYSIS
      IF(DEBUG.AND.MYRANK.EQ.-1)THEN
        S1TIME=MPI_TIC()
        IF(BTEST)THEN
          CALL SURFPLT
          CALL VELPLTH_mpi
          CALL EEXPOUT_mpi(-1)
          CLOSE(7)
          CLOSE(8)
          CLOSE(9)
          STOP 'ERROR: NANs have been computed!'
        ENDIF
        MPI_WTIMES(580)=MPI_WTIMES(580)+MPI_TOC(S1TIME)
      ENDIF
C
      MPI_WTIMES(15)=MPI_WTIMES(15)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  CALCULATE SHELL FISH LARVAE AND/OR WATER QUALITY CONSTITUENT
C **  CONCENTRATIONS AT TIME LEVEL (N+1) AFTER SETTING DOUBLE TIME
C **  STEP TRANSPORT FIELD
C
C----------------------------------------------------------------------C
C
      STIME=MPI_TIC()
C
      ITMP=0
      IF(ISTRAN(4).GE.1) ITMP=1
      IF(ISTRAN(8).GE.1) ITMP=1
      IF(ISWASP.GE.1)ITMP=1              ! 6/7/2005 a stoddard dsllc
      IF(ISICM.GE.1) ITMP=1
C
      IF(ITMP.EQ.1)THEN
C
C **  CALCULATE CONSERVATION OF VOLUME FOR THE WATER QUALITY ADVECTION
C
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          HWQ(L)=HP(L)
          WWQ(L,0)=0.
        ENDDO
C
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            UHDYWQ(L,K)=UHDY2(L,K)
            VHDXWQ(L,K)=VHDX2(L,K)
            UWQ(L,K)=U2(L,K)
            VWQ(L,K)=V2(L,K)
            WWQ(L,K)=W2(L,K)
          ENDDO
        ENDDO
C
C     ADD CHANNEL INTERACTIONS
C

        IF(MDCHH.GE.1)THEN
          DO NMD=1,MDCHH
            IF(MDCHTYP(NMD).EQ.1)THEN
              HWQ(LMDCHH(NMD))=HWQ(LMDCHH(NMD))
     &            +DT2*DXYIP(LMDCHH(NMD))*(QCHANU(NMD))
              HWQ(LMDCHU(NMD))=HWQ(LMDCHU(NMD))
     &            -DT2*DXYIP(LMDCHU(NMD))*(QCHANU(NMD))
            ENDIF
            IF(MDCHTYP(NMD).EQ.2)THEN
              HWQ(LMDCHH(NMD))=HWQ(LMDCHH(NMD))
     &            +DT2*DXYIP(LMDCHH(NMD))*(QCHANV(NMD))
              HWQ(LMDCHV(NMD))=HWQ(LMDCHV(NMD))
     &            -DT2*DXYIP(LMDCHV(NMD))*(QCHANV(NMD))
            ENDIF
            IF(MDCHTYP(NMD).EQ.3)THEN
              HWQ(LMDCHH(NMD))=HWQ(LMDCHH(NMD))
     &            +DT2*DXYIP(LMDCHH(NMD))*(QCHANU(NMD))
     &            +DT2*DXYIP(LMDCHH(NMD))*(QCHANV(NMD))
              HWQ(LMDCHU(NMD))=HWQ(LMDCHU(NMD))
     &            -DT2*DXYIP(LMDCHU(NMD))*(QCHANU(NMD))
              HWQ(LMDCHV(NMD))=HWQ(LMDCHV(NMD))
     &            -DT2*DXYIP(LMDCHV(NMD))*(QCHANV(NMD))
            ENDIF
          ENDDO
        ENDIF
C
C     END ADD CHANNEL INTERACTIONS
C
        IF(ISTRAN(8).GE.1) CALL WQ3D_mpi(ISTL,IS2TL)
C
        IF(ISTRAN(4).GE.1) CALL CALSFT_mpi(ISTL,IS2TL)
C
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          H2WQ(L)=HWQ(L)
        ENDDO
C
      ENDIF
C
      MPI_WTIMES(16)=MPI_WTIMES(16)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  UPDATE BUOYANCY AND CALCULATE NEW BUOYANCY USING
C **  AN EQUATION OF STATE
C
      STIME=MPI_TIC()
C
      DO K=1,KC
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          B1(L,K)=B(L,K)
        ENDDO
      ENDDO
C
      IF(BSC.GT.1.E-6)THEN
        CALL CALBUOY_mpi
      ELSE
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            B(L,K)=0.
          ENDDO
        ENDDO
      ENDIF
C
      MPI_WTIMES(17)=MPI_WTIMES(17)+MPI_TOC(STIME)
C
      STIME=MPI_TIC()
      CALL broadcast_boundary_array(B1,ic)
      CALL broadcast_boundary_array(B,ic)
      MPI_WTIMES(55)=MPI_WTIMES(55)+MPI_TOC(STIME)
C
C
C **  CALL TWO-TIME LEVEL BALANCES
C
      STIME=MPI_TIC()
C
      IF(ISBAL.GE.1)THEN
        CALL BAL2T4
      ENDIF
C
      MPI_WTIMES(18)=MPI_WTIMES(18)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  CALCULATE U AT V AND V AT U AT TIME LEVEL (N+1)
C
C----------------------------------------------------------------------C
C
      STIME=MPI_TIC()
C
!$OMP PARALLEL DO PRIVATE(LN,LS,LNW,LSE,LSW)
      DO L=LMPI2,LMPILA
        LN=LNC(L)
        LS=LSC(L)
        LNW=LNWC(L)
        LSE=LSEC(L)
        LSW=LSWC(L)
        U1V(L)=UV(L)
        V1U(L)=VU(L)
        UV(L)=0.25*(HP(LS)*(U(LSE,1)+U(LS,1))
     &      +HP(L)*(U(L+1,1)+U(L,1)))*HVI(L)
        VU(L)=0.25*(HP(L-1)*(V(LNW,1)+V(L-1,1))
     &      +HP(L)*(V(LN,1)+V(L,1)))*HUI(L)
      ENDDO
C
      MPI_WTIMES(19)=MPI_WTIMES(19)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  CALCULATE HORIZONTAL VISCOSITY AND MOMENTUM DIFFUSION FLUXES
C **  AT TIME LEVEL (N)
C
      STIME=MPI_TIC()
C
      IF(ISHDMF.GE.1) CALL CALHDMF_mpi
C
      MPI_WTIMES(20)=MPI_WTIMES(20)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  CALCULATE BOTTOM STRESS AT LEVEL (N+1)
C
      STIME=MPI_TIC()
C
      CALL CALTBXY_mpi(ISTL,IS2TL)
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        TBX(L)=(AVCON1*HUI(L)+STBX(L)*SQRT(VU(L)*VU(L)
     &      +U(L,1)*U(L,1)))*U(L,1)
        TBY(L)=(AVCON1*HVI(L)+STBY(L)*SQRT(UV(L)*UV(L)
     &      +V(L,1)*V(L,1)))*V(L,1)
      ENDDO
      MPI_WTIMES(21)=MPI_WTIMES(21)+MPI_TOC(STIME)
C
      STIME=MPI_TIC()
      CALL broadcast_boundary(TBX,ic)
      CALL broadcast_boundary(TBY,ic)
      MPI_WTIMES(56)=MPI_WTIMES(56)+MPI_TOC(STIME)
C
C
C**********************************************************************C
C
C **  SET DEPTH DEVIATION FROM UNIFORM FLOW ON FLOW FACES
C
      STIME=MPI_TIC()
C
      IF(ISBSDFUF.GE.1)THEN
        HDFUFM=1.E-12
C
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          LS=LSC(L)
          HDFUFX(L)=HDFUFM+G*SUB(L)*HU(L)*(BELV(L-1)-BELV(L))*DXIU(L)
          HDFUFY(L)=HDFUFM+G*SVB(L)*HV(L)*(BELV(LS )-BELV(L))*DYIV(L)
        ENDDO
C
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          HDFUFX(L)=TBX(L)/HDFUFX(L)
          HDFUFY(L)=TBY(L)/HDFUFY(L)
        ENDDO
C
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          HDFUFX(L)=MAX(HDFUFX(L),-1.0)
          HDFUFY(L)=MAX(HDFUFY(L),-1.0)
        ENDDO
C
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          HDFUFX(L)=MIN(HDFUFX(L),1.0)
          HDFUFY(L)=MIN(HDFUFY(L),1.0)
        ENDDO
C
      ENDIF
C
      MPI_WTIMES(22)=MPI_WTIMES(22)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  SET BOTTOM AND SURFACE TURBULENT INTENSITY SQUARED AT (N+1)
C
C----------------------------------------------------------------------C
C
C
      IF(ISWAVE.EQ.0)THEN
C
C
C----------------------------------------------------------------------c
C
        IF(ISCORTBC.EQ.0) THEN
C
          STIME=MPI_TIC()
          S1TIME=MPI_TIC()
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            TVAR3S(L)=TSY(LNC(L))
            TVAR3W(L)=TSX(L+1)
            TVAR3E(L)=TBX(L+1   )
            TVAR3N(L)=TBY(LNC(L))
          ENDDO
          MPI_WTIMES(891)=MPI_WTIMES(891)+MPI_TOC(S1TIME)
C
          S1TIME=MPI_TIC()
!$OMP PARALLEL DO PRIVATE(TMP)
          DO L=LMPI2,LMPILA
! { GEOSR (IBM request)
            IF (ISNAN(TVAR3S(L))) TVAR3S(L)=0.
            IF (ISNAN(TVAR3W(L))) TVAR3W(L)=0.
            IF (ISNAN(TVAR3E(L))) TVAR3E(L)=0.
            IF (ISNAN(TVAR3N(L))) TVAR3N(L)=0.
            IF (ISNAN(TSY(L))) TSY(L)=0.
            IF (ISNAN(TSX(L))) TSX(L)=0.
            IF (ISNAN(TBY(L))) TBY(L)=0.
            IF (ISNAN(TBX(L))) TBX(L)=0.
! } GEOSR (IBM request)
            TMP = (RSSBCE(L)*TVAR3E(L)+RSSBCW(L)*TBX(L))**2
     &           +(RSSBCN(L)*TVAR3N(L)+RSSBCS(L)*TBY(L))**2
            QQ(L,0 )=0.5*CTURB2*SQRT(TMP)

            TMP = (RSSBCE(L)*TVAR3W(L)+RSSBCW(L)*TSX(L))**2
     &           +(RSSBCN(L)*TVAR3S(L)+RSSBCS(L)*TSY(L))**2
            QQ(L,KC)=0.5*CTURB2*SQRT(TMP)

            QQSQR(L,0)=SQRT(QQ(L,0))  ! *** DSLLC
          ENDDO
          MPI_WTIMES(892)=MPI_WTIMES(892)+MPI_TOC(S1TIME)
          MPI_WTIMES(23)=MPI_WTIMES(23)+MPI_TOC(STIME)
C
        ENDIF
C
C----------------------------------------------------------------------c
C
      STIME=MPI_TIC()
C
        IF(ISCORTBC.GE.1) THEN
C
          IF(ISCORTBCD.GE.1)THEN
            NTMPVAL=MOD(N,NTSPTC)
            IF(NTMPVAL.EQ.0.AND.DEBUG)THEN
              OPEN(1,FILE='ADJSTRESSE.OUT',ACCESS='APPEND')
            ENDIF
          ENDIF
C
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            TVAR3S(L)=TSY(LNC(L))
            TVAR3W(L)=TSX(L+1)
            TVAR3E(L)=TBX(L+1   )
            TVAR3N(L)=TBY(LNC(L))
          ENDDO
C
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            WCOREST(L)=1.
            WCORWST(L)=1.
            WCORNTH(L)=1.
            WCORSTH(L)=1.
          ENDDO
C
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(ISSBCP(L).EQ.0)THEN
              IF(SUB(L+1).LT.0.5)WCOREST(L)=FSCORTBCV(L)
              IF(SUB(L).LT.0.5)WCORWST(L)=FSCORTBCV(L)
              IF(SVB(LNC(L)).LT.0.5)WCORNTH(L)=FSCORTBCV(L)
              IF(SVB(L).LT.0.5)WCORSTH(L)=FSCORTBCV(L)
            ENDIF
          ENDDO
C
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            WCOREW(L)=1./(WCOREST(L)+WCORWST(L))
            WCORNS(L)=1./(WCORNTH(L)+WCORSTH(L))
          ENDDO
C
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            WCOREST(L)=WCOREST(L)*WCOREW(L)
            WCORWST(L)=WCORWST(L)*WCOREW(L)
            WCORNTH(L)=WCORNTH(L)*WCORNS(L)
            WCORSTH(L)=WCORSTH(L)*WCORNS(L)
          ENDDO
C
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
! { GEOSR (IBM request)
            IF (ISNAN(TVAR3S(L))) TVAR3S(L)=0.
            IF (ISNAN(TVAR3W(L))) TVAR3W(L)=0.
            IF (ISNAN(TVAR3E(L))) TVAR3E(L)=0.
            IF (ISNAN(TVAR3N(L))) TVAR3N(L)=0.
            IF (ISNAN(TSY(L))) TSY(L)=0.
            IF (ISNAN(TSX(L))) TSX(L)=0.
            IF (ISNAN(TBY(L))) TBY(L)=0.
            IF (ISNAN(TBX(L))) TBX(L)=0.
! } GEOSR (IBM request)
            QQ(L,0 )=CTURB2*SQRT(
     &          (RSSBCE(L)*WCOREST(L)*TVAR3E(L)
     &          +RSSBCW(L)*WCORWST(L)*TBX(L))**2
     &          +(RSSBCN(L)*WCORNTH(L)*TVAR3N(L)
     &          +RSSBCS(L)*WCORSTH(L)*TBY(L))**2)
            QQ(L,KC)=0.5*CTURB2*SQRT(
     &          (RSSBCE(L)*TVAR3W(L)+RSSBCW(L)*TSX(L))**2
     &          +(RSSBCN(L)*TVAR3S(L)+RSSBCS(L)*TSY(L))**2)
            QQSQR(L,0)=SQRT(QQ(L,0))  ! *** DSLLC
          ENDDO
C
          IF(ISCORTBCD.GE.1.AND.NTMPVAL.EQ.0)THEN
C
!$OMP PARALLEL DO PRIVATE(KCORNER)
            DO L=LMPI2,LMPILA
              LCORNER(L)=0
              KCORNER=0
              IF(WCORWST(L).GT.0.505)THEN
                KCORNER=KCORNER+1
                LCORNWE(L)=L-1
              ENDIF
              IF(WCOREST(L).GT.0.505)THEN
                KCORNER=KCORNER+1
                LCORNWE(L)=L+1
              ENDIF
              IF(WCORNTH(L).GT.0.505)THEN
                KCORNER=KCORNER+1
                LCORNSN(L)=LNC(L)
              ENDIF
              IF(WCORSTH(L).GT.0.505)THEN
                KCORNER=KCORNER+1
                LCORNSN(L)=LSC(L)
              ENDIF
              IF(KCORNER.EQ.2)LCORNER(L)=1
            ENDDO
C
            NCORCELLS=0
!$OMP PARALLEL DO REDUCTION(+:NCORCELLS)
            DO L=LMPI2,LMPILA
              NCORCELLS=NCORCELLS+LCORNER(L)
            ENDDO
            CALL MPI_ALLREDUCE(NCORCELLS,MPI_I4,1,MPI_INT,MPI_SUM,
     &                       MPI_COMM_WORLD,IERR)
            NCORCELLS=MPI_I4
C
            IF(DEBUG.AND.MYRANK.EQ.0)THEN
              WRITE(1,3675)TIMEDAY,NCORCELLS
              DO L=2,LA
                IF(LMASKDRY(L))THEN
                  IF(LCORNER(L).EQ.1)THEN
                    LWE=LCORNWE(L)
                    LSN=LCORNSN(L)
                    TAUTMP=QQ(L,0)/CTURB2
                    TAUTMPWE=QQ(LWE,0)/CTURB2
                    TAUTMPSN=QQ(LSN,0)/CTURB2
                    WRITE(1,3677)IL(L),JL(L),TAUTMP,TAUBSND(L),
     &                TAUBSED(L)
                    WRITE(1,3676)IL(LWE),JL(LWE),TAUTMPWE,TAUBSND(LWE),
     &                TAUBSED(LWE)
                    WRITE(1,3676)IL(LSN),JL(LSN),TAUTMPSN,TAUBSND(LSN),
     &                TAUBSED(LSN)
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
C
          ENDIF

          IF(DEBUG)CLOSE(1)
C
        ENDIF
C
C----------------------------------------------------------------------c
C
      ENDIF
C
      MPI_WTIMES(25)=MPI_WTIMES(25)+MPI_TOC(STIME)
C
 3678 FORMAT(2I6,4F13.3)
C3679 FORMAT(12x,4F13.3)
C3680 FORMAT(12x,6F13.5)
C3681 FORMAT(12X,5E13.4,F13.5)
 3677 FORMAT('CORNER',2I5,5E14.5)
 3676 FORMAT(6X,2I5,5E14.5)
 3675 FORMAT(F11.3,I6,' TIME IN DAYS AND NUMBER OF CORNERS')
C
C
C**********************************************************************C
C
C **  SET BOTTOM AND SURFACE TURBULENT INTENSITY SQUARED AT (N+1)
C
C----------------------------------------------------------------------C
C
      STIME=MPI_TIC()
C
      IF(ISWAVE.GE.1)THEN
C
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          TVAR3S(L)=TSY(LNC(L))
          TVAR3W(L)=TSX(L+1)
          TVAR3E(L)=TBX(L+1   )
          TVAR3N(L)=TBY(LNC(L))
        ENDDO
C
!$OMP PARALLEL DO PRIVATE(TAUBC2,TAUBC,UTMP,VTMP,CURANG)
        DO L=LMPI2,LMPILA
! { GEOSR (IBM request)
          IF (ISNAN(TVAR3S(L))) TVAR3S(L)=0.
          IF (ISNAN(TVAR3W(L))) TVAR3W(L)=0.
          IF (ISNAN(TVAR3E(L))) TVAR3E(L)=0.
          IF (ISNAN(TVAR3N(L))) TVAR3N(L)=0.
          IF (ISNAN(TSY(L))) TSY(L)=0.
          IF (ISNAN(TSX(L))) TSX(L)=0.
          IF (ISNAN(TBY(L))) TBY(L)=0.
          IF (ISNAN(TBX(L))) TBX(L)=0.
! } GEOSR (IBM request)
          TAUBC2 = (RSSBCE(L)*TVAR3E(L)+RSSBCW(L)*TBX(L))**2
     &            +(RSSBCN(L)*TVAR3N(L)+RSSBCS(L)*TBY(L))**2
          TAUBC=0.5*SQRT(TAUBC2)
          UTMP=0.5*STCUV(L)*(U(L+1,1)+U(L,1))+1.E-12
          VTMP=0.5*STCUV(L)*(V(LN,1)+V(L,1))
          CURANG=ATAN2(VTMP,UTMP)
          TAUB2=TAUBC*TAUBC+0.5*(QQWV1(L)*QQWV1(L))
     &        +FOURDPI*TAUBC*QQWV1(L)*COS(CURANG-WACCWE(L))
          TAUB2=MAX(TAUB2,0.)
          QQ(L,0 )=CTURB2*SQRT(TAUB2)
          QQ(L,KC)=0.5*CTURB2*SQRT((TVAR3W(L)+TSX(L))**2
     &        +(TVAR3S(L)+TSY(L))**2)
          QQSQR(L,0)=SQRT(QQ(L,0))  ! *** DSLLC
        ENDDO
C
      ENDIF
C
      MPI_WTIMES(26)=MPI_WTIMES(26)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  CALCULATE TURBULENT INTENSITY SQUARED
C
      STIME=MPI_TIC()
C
      IF(KC.GT.1)THEN
        IF(ISQQ.EQ.1)THEN
          IF(ISTOPT(0).EQ.0)CALL CALQQ2TOLD_mpi (ISTL)
          IF(ISTOPT(0).GE.1)CALL CALQQ2T_mpi (ISTL)
        ENDIF
        IF(ISQQ.EQ.2) CALL CALQQ2 (ISTL)
      ENDIF
C
      MPI_WTIMES(27)=MPI_WTIMES(27)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  CALCULATE MEAN MASS TRANSPORT FIELD
C
      STIME=MPI_TIC()
C
      IF(ISSSMMT.NE.2)THEN
        IF(ISICM.GE.1)THEN
          NTMP=MOD(N,2)
          IF(ISTL.EQ.3.AND.NTMP.EQ.0) CALL CALMMT
        ENDIF
      ENDIF
C
C      IF(ISSSMMT.NE.2) CALL CALMMT
C
      MPI_WTIMES(28)=MPI_WTIMES(28)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  HYDRODYNAMIC CALCULATIONS FOR THIS TIME STEP ARE COMPLETED
C
C**********************************************************************C
C
C **  WRITE TO TIME SERIES FILES
C
      STIME=MPI_TIC()
C
      IF(ISDYNSTP.EQ.0)THEN
        CTIM=DT*FLOAT(N)+TCON*TBEGIN
        CTIM=CTIM/TCON
      ELSE
        CTIM=TIMESEC/TCON
      ENDIF
C
CDYN      IF(ISTMSR.GE.1)THEN
CDYN        IF(N.GE.NBTMSR.AND.N.LE.NSTMSR)THEN
CDYN          IF(NCTMSR.EQ.NWTMSR)THEN
CDYN            CALL TMSR
CDYN            ICALLTP=1
CDYN            NCTMSR=1
CDYN           ELSE
CDYN            NCTMSR=NCTMSR+1
CDYN          ENDIF
CDYN        ENDIF
CDYN      ENDIF
C
C
      IF(ISTMSR.GE.1)THEN
c        IF(N.GE.NBTMSR.AND.N.LE.NSTMSR)THEN
        IF(NCTMSR.GE.NWTMSR)THEN
          CALL TMSR
          NDIFF=NWTMSR-NCTMSR
          ICALLTP=1
          NCTMSR=NINCRMT+NDIFF
        ELSE
          NCTMSR=NCTMSR+NINCRMT
        ENDIF
c        ENDIF
      ENDIF
C
C****************************************************
C **  WRITE TO DUMP FILES  ******************C
C
C
      IF(ISDUMP.GE.1)THEN
        IF(CTIM.GE.TSDUMP.AND.CTIM.LE.TEDUMP)THEN
C          IF(NCDUMP.EQ.NSDUMP)THEN
          IF(NCDUMP.GE.NSDUMP)THEN
            CALL DUMP
            NDIFF=NSDUMP-NCDUMP
            ICALLTP=1
C            NCDUMP=1
            NCDUMP=NINCRMT+NDIFF
          ELSE
C            NCDUMP=NCDUMP+1
            NCDUMP=NCDUMP+NINCRMT
          ENDIF
        ENDIF
      ENDIF
C
C**********************************************************************C
C
C **  OUTPUT ZERO DIMENSION VOLUME BALANCE
C
C----------------------------------------------------------------------C
C
      IF(ISDRY.GE.1.AND.ISDRY.LT.98)THEN
        IF(ICALLTP.EQ.1.AND.DEBUG)THEN
          OPEN(1,FILE='ZVOLBAL.OUT',POSITION='APPEND',STATUS='UNKNOWN')
          DO LS=1,LORMAX
            IF(VOLZERD.GE.VOLSEL(LS).AND.VOLZERD.LT.VOLSEL(LS+1))THEN
              WTM=VOLSEL(LS+1)-VOLZERD
              WTMP=VOLZERD-VOLSEL(LS)
              DELVOL=VOLSEL(LS+1)-VOLSEL(LS)
              WTM=WTM/DELVOL
              WTMP=WTMP/DELVOL
              SELZERD=WTM*BELSURF(LS)+WTMP*BELSURF(LS+1)
              ASFZERD=WTM*ASURFEL(LS)+WTMP*ASURFEL(LS+1)
            ENDIF
          ENDDO
          IF(ISDYNSTP.EQ.0)THEN
            CTIM=DT*FLOAT(N)+TCON*TBEGIN
            CTIM=CTIM/TCTMSR
          ELSE
            CTIM=TIMESEC/TCTMSR
          ENDIF
          WRITE(1,5304) CTIM,SELZERD,ASFZERD,VOLZERD,VETZERD
          CLOSE(1)
        ENDIF
      ENDIF
      ICALLTP=0
C
 5304 FORMAT(2X,F10.4,2X,F10.5,3(2X,E12.4))
C
C**********************************************************************C
C
C **  WRITE VERTICAL SCALAR FIELD PROFILES
C
      IF(ISVSFP.EQ.1)THEN
        IF(N.GE.NBVSFP.AND.N.LE.NSVSFP)THEN
          CALL VSFP
        ENDIF
      ENDIF
C
      MPI_WTIMES(29)=MPI_WTIMES(29)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  CALCULATE MEAN MASS TRANSPORT FIELD
C
      STIME=MPI_TIC()
C
      IF(ISSSMMT.NE.2)THEN
        IF(ISICM.EQ.0) CALL CALMMT
      ENDIF
C
C      IF(ISSSMMT.NE.2) CALL CALMMT
C
      MPI_WTIMES(30)=MPI_WTIMES(30)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  ADVANCE NEUTRALLY BUOYANT PARTICLE DRIFTER TRAJECTORIES
C
      !IF(ISPD.EQ.1)THEN
      !  IF(N.GE.NPDRT) CALL DRIFTER
C
      STIME=MPI_TIC()
C
!{GEOSR, OIL, CWCHO, 101122
      IF(ISPD.GE.2.AND.IDTOX.LT.4440) THEN   !DHC
        IF (TIMEDAY.GE.LA_BEGTI.AND.TIMEDAY.LE.LA_ENDTI) THEN
          CALL CPU_TIME(T1TMP)
          CALL DRIFTERC
          CALL CPU_TIME(T2TMP)
          TLRPD=TLRPD+T2TMP-T1TMP
        ENDIF
      ENDIF


      IF(IDTOX.GE.4440)THEN
            IF (TIMEDAY.GE.REAL(NPTXLDS/86400.).AND.
     &            TIMEDAY.LE.REAL(NPTXLDE/86400.)) THEN
            CALL DRIFTERC
          ENDIF
      ENDIF
!GEOSR}
C
      MPI_WTIMES(31)=MPI_WTIMES(31)+MPI_TOC(STIME)
C
!      IF(ISLRPD.GE.1)THEN
!        CALL CPU_TIME(T1TMP)                  !DHC:13-04-09
!        IF(ISLRPD.LE.2)THEN
!          IF(N.GE.NLRPDRT(1)) CALL LAGRES
!        ENDIF
!        IF(ISLRPD.GE.3)THEN
!          IF(N.GE.NLRPDRT(1)) CALL GLMRES
!        ENDIF
!        TLRPD=TLRPD+T1TMP-SECOND()
!      ENDIF
C
C**********************************************************************C
C
C **  CALCULATE VOLUME MASS, MOMENTUM AND ENERGY BALANCES
C
C      IF(ISBAL.GE.1)THEN
C         CALL CALBAL5
C         NTMP=MOD(N,2)
C         IF(NTMP.EQ.0)THEN
C           CALL CBALEV5
C          ELSE
C           CALL CBALOD5
C         ENDIF
C       ENDIF
C
C   SEDIMENT BUDGET CALCULATION     (DLK 10/15)
C
C       IF(ISSBAL.GE.1)THEN
C       CALL BUDGET5
C       ENDIF
C       NTMP=MOD(N,2)
C       IF(NTMP.EQ.0)THEN
C         CALL BUDGEV5
C        ELSE
C         CALL BUDGOD5
C       ENDIF
C
C **  CALL TWO-TIME LEVEL BALANCES
C
      STIME=MPI_TIC()
C
      IF(ISBAL.GE.1)THEN
        CALL BAL2T5
      ENDIF
C
      MPI_WTIMES(32)=MPI_WTIMES(32)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  PERFORM AN M2 TIDE HARMONIC ANALYSIS EVERY 2 M2 PERIODS
C
      STIME=MPI_TIC()
C
      IF(ISHTA.EQ.1) CALL CALHTA
C
      MPI_WTIMES(33)=MPI_WTIMES(33)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  CALCULATE DISPERSION COEFFICIENTS
C
C     IF(N.GE.NDISP)THEN
      STIME=MPI_TIC()
C
      IF(N.GE.NDISP.AND.NCTBC.EQ.1)THEN
        IF(ISDISP.EQ.2) CALL CALDISP2
        IF(ISDISP.EQ.3) CALL CALDISP3
      ENDIF
C
      MPI_WTIMES(34)=MPI_WTIMES(34)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  PERFORM LEAST SQUARES HARMONIC ANALYSIS AT SELECTED LOCATIONS
C
      STIME=MPI_TIC()
C
      IF(ISLSHA.EQ.1.AND.N.EQ.NCLSHA)THEN
        CALL LSQHARM
        NCLSHA=NCLSHA+(NTSPTC/24)
      ENDIF
C
      MPI_WTIMES(35)=MPI_WTIMES(35)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  PRINT INTERMEDIATE RESULTS
C
C----------------------------------------------------------------------C
C
      STIME=MPI_TIC()
C
      IF(NPRINT .EQ. NTSPP)THEN
        NPRINT=1
        CALL OUTPUT1
      ELSE
        NPRINT=NPRINT+1
      ENDIF
C
      MPI_WTIMES(36)=MPI_WTIMES(36)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  WRITE TO TIME VARYING GRAPHICS FILES
C
C----------------------------------------------------------------------C
C
CDYN      IF(N.EQ.NCPPH.AND.ISPPH.EQ.1)THEN
Cpmc      IF(N.GE.NCPPH.AND.ISPPH.GE.1)THEN
C
      STIME=MPI_TIC()
C
      IF(TIMEDAY.GE.SNAPSHOTS(NSNAPSHOTS))THEN
        CALL SURFPLT
      ENDIF
C
      MPI_WTIMES(37)=MPI_WTIMES(37)+MPI_TOC(STIME)
C
C
C----------------------------------------------------------------------C
C
CDYN      IF(N.EQ.NCBPH.AND.ISBPH.EQ.1)THEN
C
      STIME=MPI_TIC()
C
      IF(N.GE.NCBPH.AND.ISBPH.GE.1)THEN
        IF(ISBEXP.EQ.0)THEN
          CALL BEDPLTH
          NCBPH=NCBPH+(NTSPTC/NPBPH)
        ENDIF
      ENDIF
C
      MPI_WTIMES(38)=MPI_WTIMES(38)+MPI_TOC(STIME)
C
C----------------------------------------------------------------------C
C
CDYN      IF(N.EQ.NCVPH.AND.ISVPH.GE.1)THEN
C
      STIME=MPI_TIC()
C
      IPLTTMP=0
      IF(ISVPH.EQ.1.OR.ISVPH.EQ.2)IPLTTMP=1
      IF(TIMEDAY.GE.SNAPSHOTS(NSNAPSHOTS).AND.IPLTTMP.EQ.1)THEN
        CALL VELPLTH_mpi
      ENDIF
C
      MPI_WTIMES(39)=MPI_WTIMES(39)+MPI_TOC(STIME)
C
C----------------------------------------------------------------------C
C
CDYN      IF(N.EQ.NCVPV.AND.ISVPV.GE.1)THEN
C
      STIME=MPI_TIC()
C
      IF(N.GE.NCVPV.AND.ISVPV.GE.1)THEN
        CALL VELPLTV
        NCVPV=NCVPV+(NTSPTC/NPVPV)
      ENDIF
C
      MPI_WTIMES(40)=MPI_WTIMES(40)+MPI_TOC(STIME)
C
C----------------------------------------------------------------------C
C
      STIME=MPI_TIC()
C
      DO K=1,KC
!$OMP PARALLEL DO
        DO L=LMPI1,LMPILC
          TVAR1S(L,K)=TOX(L,K,1)
        ENDDO
      ENDDO
C
      IPLTTMP=0
      IF(ISSPH(1).EQ.1.OR.ISSPH(1).EQ.2)IPLTTMP=1
      IF(N.GE.NCSPH(1).AND.IPLTTMP.EQ.1)THEN
        IF(ISTRAN(1).GE.1) CALL SALPLTH (1,SAL)
        NCSPH(1)=NCSPH(1)+(NTSPTC/NPSPH(1))
      ENDIF
C
      IPLTTMP=0
      IF(ISSPH(2).EQ.1.OR.ISSPH(2).EQ.2)IPLTTMP=1
      IF(N.GE.NCSPH(2).AND.IPLTTMP.EQ.1)THEN
        IF(ISTRAN(2).GE.1) CALL SALPLTH (2,TEM)
        NCSPH(2)=NCSPH(2)+(NTSPTC/NPSPH(2))
      ENDIF
C
      IPLTTMP=0
      IF(ISSPH(3).EQ.1.OR.ISSPH(3).EQ.2)IPLTTMP=1
      IF(N.GE.NCSPH(3).AND.IPLTTMP.EQ.1)THEN
        IF(ISTRAN(3).GE.1) CALL SALPLTH (3,DYE)
        NCSPH(3)=NCSPH(3)+(NTSPTC/NPSPH(3))
      ENDIF
C
      IPLTTMP=0
      IF(ISSPH(4).EQ.1.OR.ISSPH(4).EQ.2)IPLTTMP=1
      IF(N.GE.NCSPH(4).AND.IPLTTMP.EQ.1)THEN
        IF(ISTRAN(4).GE.1) CALL SALPLTH (4,SFL)
        NCSPH(4)=NCSPH(4)+(NTSPTC/NPSPH(4))
      ENDIF
C
      IPLTTMP=0
      IF(ISSPH(5).EQ.1.OR.ISSPH(5).EQ.2)IPLTTMP=1
      IF(N.GE.NCSPH(5).AND.IPLTTMP.EQ.1)THEN
        IF(ISTRAN(5).GE.1) CALL SALPLTH (5,TVAR1S)
        NCSPH(5)=NCSPH(5)+(NTSPTC/NPSPH(5))
      ENDIF
C
      IPLTTMP=0
      IF(ISSPH(6).EQ.1.OR.ISSPH(6).EQ.2)IPLTTMP=1
      IF(N.GE.NCSPH(6).AND.IPLTTMP.EQ.1)THEN
        IF(ISTRAN(6).GE.1) CALL SALPLTH (6,SEDT)
        NCSPH(6)=NCSPH(6)+(NTSPTC/NPSPH(6))
      ENDIF
C
      IPLTTMP=0
      IF(ISSPH(7).EQ.1.OR.ISSPH(7).EQ.2)IPLTTMP=1
      IF(N.GE.NCSPH(7).AND.IPLTTMP.EQ.1)THEN
        IF(ISTRAN(7).GE.1) CALL SALPLTH (7,SNDT)
        NCSPH(7)=NCSPH(7)+(NTSPTC/NPSPH(7))
      ENDIF
C
      MPI_WTIMES(41)=MPI_WTIMES(41)+MPI_TOC(STIME)
C
C----------------------------------------------------------------------C
C
      STIME=MPI_TIC()
C
      DO ITMP=1,7
        IF(N.GE.NCSPV(ITMP).AND.ISSPV(ITMP).GE.1)THEN
          CALL SALPLTV(ITMP)
          NCSPV(ITMP)=NCSPV(ITMP)+(NTSPTC/NPSPV(ITMP))
        ENDIF
      ENDDO
C
      MPI_WTIMES(42)=MPI_WTIMES(42)+MPI_TOC(STIME)
C
C----------------------------------------------------------------------C
C
C **  WRITE EFDC EXPLORER FORMAT OUTPUT
C
      STIME=MPI_TIC()
C
      IF(ISSPH(8).EQ.1.OR.ISBEXP.EQ.1)THEN
        IF(TIMEDAY.GE.SNAPSHOTS(NSNAPSHOTS))THEN
          IF(IBIN_TYPE.EQ.1)THEN
             IF(MYRANK.EQ.0) WRITE(88,*) 'EEXPOUT_mpi'
             CALL EEXPOUT_mpi(0)
          ELSEIF(IBIN_TYPE.EQ.0)THEN
             IF(MYRANK.EQ.0) WRITE(88,*) 'EEXPOUT_opt_mpi'
             CALL EEXPOUT_opt_mpi(0)
          ENDIF
        ENDIF
      ENDIF
      IF(TIMEDAY.GE.SNAPSHOTS(NSNAPSHOTS))THEN
        NSNAPSHOTS=NSNAPSHOTS+1
      ENDIF
C
      MPI_WTIMES(43)=MPI_WTIMES(43)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  WRITE TO TIME VARYING 3D HDF GRAPHICS FILES
C
C----------------------------------------------------------------------C
C
      STIME=MPI_TIC()
C
      IF(N.EQ.NC3DO.AND.IS3DO.EQ.1)THEN
        CALL OUT3D
        NC3DO=NC3DO+(NTSPTC/NP3DO)
      ENDIF
C
      MPI_WTIMES(44)=MPI_WTIMES(44)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  WRITE RESTART FILE EVERY ISRESTO M2 TIDAL CYCLES
C
      STIME=MPI_TIC()
C
      IF(ISRESTO.GE.1)THEN
        IF((N-ISSREST).GT.NRESTO)THEN
          if(myrank.eq.0) print*,'R1ESTOUT(0)'
          CALL RESTOUT(0)
          IF(ISTRAN(8).GE.1)THEN
            IF(IWQRST.EQ.1) CALL WWQRST(0)
            IF(IWQBEN.EQ.1 .AND. ISMRST.EQ.1) CALL WSMRST(0)
          ENDIF
          ISSREST=N
        ENDIF
      ENDIF
! { GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.5.23
      IF(ISRESTO.LT.-20)THEN
        IF((N-ISSREST).GT.NTSPTC)THEN
            if(myrank.eq.0) print*,'R1ESTOUT(-19)'
            CALL RESTOUT(-19)
          IF(ISTRAN(8).GE.1)THEN
            IF(IWQRST.EQ.1) CALL WWQRST(1)
            IF(IWQBEN.EQ.1 .AND. ISMRST.EQ.1) CALL WSMRST(1)
          ENDIF
          ISSREST=N
        ENDIF
      ENDIF
! } GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.5.23

! { GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2010.11.10
      IF(ISRESTO.LT.-20)THEN
        ISHYD=-1*ISRESTO-20
        IF (N.EQ.1) THEN
          IHYDCNT=1
          SNAPSHOTHYD=FLOAT(ISHYD*IHYDCNT)*60./86400.+TBEGIN
        ENDIF
        IF(TIMEDAY.GE.SNAPSHOTHYD) THEN
!          WRITE(*,*)'WRITE================',N,TIMEDAY,TIMEDAY*1440.
!          CALL RESTOUT(-21)
          IHYDCNT=IHYDCNT+1
          SNAPSHOTHYD=FLOAT(ISHYD*IHYDCNT)*60./86400.+TBEGIN
        ENDIF
      ENDIF
! } GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2010.11.10
C
      MPI_WTIMES(45)=MPI_WTIMES(45)+MPI_TOC(STIME)
C
C**********************************************************************C
C
C **  RECORD TIME
C
C **  DTIME AND FLUSH ARE SUPPORTED ON SUN SYSTEMS, BUT MAY NOT BE
C **  SUPPORTED ON OTHER SYSTEMS.
C
      STIME=MPI_TIC()

      IF(NTIMER.EQ.NTSPTC)THEN
C *** EE BEGIN BLOCK
        IF(MYRANK.EQ.0) CALL TIMELOG(N,TIMEDAY)
C *** EE END BLOCK
        NTIMER=1
      ELSE
        NTIMER=NTIMER+1
      ENDIF
C
C**********************************************************************C
C
      IF(N.EQ.1)THEN
      OPEN(1,FILE='SHOW.INP',STATUS='OLD')
      DO NSKIP=1,6; READ(1,*); ENDDO
      READ(1,*)NSHTYPE,NSHOWR,ICSHOW,JCSHOW,ISHPRT
      CLOSE(1)
      ENDIF
C
      L=LIJ(ICSHOW,JCSHOW)
      IF(ISHOW.GT.0.AND.L.GE.LMPI2.AND.L.LE.LMPILA) CALL SHOWVAL
C
      MPI_WTIMES(46)=MPI_WTIMES(46)+MPI_TOC(STIME)
C**********************************************************************C
C
C *** DJB
![ykchoi 10.04.26 for linux version
      MPI_WTIMES(1)=MPI_WTIMES(1)+MPI_TOC(TTIME)
      WT_RATIO=1
      IF(PRINT_SUM)THEN
      IF(MOD(N,100).EQ.0)THEN
        call collect_in_zero(TSX)
        call collect_in_zero(TSY)
        call collect_in_zero(TBX)
        call collect_in_zero(TBY)
        call collect_in_zero_array(AV)
        call collect_in_zero_array(AB)
        call collect_in_zero_array(AQ)
        call collect_in_zero(HP)
        call collect_in_zero(HU)
        call collect_in_zero(HV)
        call collect_in_zero(P)
        call collect_in_zero(TEMB)
        call collect_in_zero_array(U)
        call collect_in_zero_array(V)
        call collect_in_zero_array(W)
        call collect_in_zero_array(TEM)
        call collect_in_zero_array(SAL)
        call collect_in_zero_array(SEDT)
        do k=0,kcm
          call collect_in_zero(QQ(:,k))
          call collect_in_zero(QQL(:,k))
        enddo
        DO NW=1,NWQV
          call collect_in_zero_array(WQV(:,:,NW))
        ENDDO
        DO NSP=1,NXSP
          call collect_in_zero_array(WQVX(:,:,NSP))
        ENDDO
        call collect_in_zero_array(QSUM)
        IF(MYRANK.EQ.0)THEN
          PRINT*, n,'TSX  = ', sum(abs(dble(TSX)))
          PRINT*, n,'TSY  = ', sum(abs(dble(TSY)))
          PRINT*, n,'TBX  = ', sum(abs(dble(TBX)))
          PRINT*, n,'TBY  = ', sum(abs(dble(TBY)))
          PRINT*, n,'AV   = ', sum(abs(dble(AV)))
          PRINT*, n,'AB   = ', sum(abs(dble(AB)))
          PRINT*, n,'AQ   = ', sum(abs(dble(AQ)))
          PRINT*, n,'HP   = ', sum(abs(dble(HP)))
          PRINT*, n,'HU   = ', sum(abs(dble(HU)))
          PRINT*, n,'HV   = ', sum(abs(dble(HV)))
          PRINT*, n,'P    = ', sum(abs(dble(P)))
          PRINT*, n,'U    = ', sum(abs(dble(U)))
          PRINT*, n,'V    = ', sum(abs(dble(V)))
          PRINT*, n,'W    = ', sum(abs(dble(W)))
          PRINT*, n,'TEM  = ', sum(abs(dble(TEM)))
          PRINT*, n,'SAL  = ', sum(abs(dble(SAL)))
          PRINT*, n,'TEMB = ', sum(abs(dble(TEMB)))
          PRINT*, n,'SEDT = ', sum(abs(dble(SEDT)))
          PRINT*, n,'QQ   = ', sum(abs(dble(QQ)))
          PRINT*, n,'QQL  = ', sum(abs(dble(QQL)))
          PRINT*, n,'WQV  = ', sum(abs(dble(WQV)))
          PRINT*, n,'WQVX = ', sum(abs(dble(WQVX)))
          PRINT*, n,'QSUM = ', sum(abs(dble(QSUM)))
        ENDIF
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      ENDIF
      ENDIF

      IF(PRINT_SUM)THEN
      IF(MOD(N,NTSPTC/WT_RATIO/24).EQ.0)THEN
        call collect_in_zero(TSX)
        call collect_in_zero(TSY)
        call collect_in_zero(TBX)
        call collect_in_zero(TBY)
        call collect_in_zero_array(AV)
        call collect_in_zero_array(AB)
        call collect_in_zero_array(AQ)
        call collect_in_zero(HP)
        call collect_in_zero(HU)
        call collect_in_zero(HV)
        call collect_in_zero(P)
        call collect_in_zero(TEMB)
        call collect_in_zero_array(U)
        call collect_in_zero_array(V)
        call collect_in_zero_array(W)
        call collect_in_zero_array(TEM)
        call collect_in_zero_array(SAL)
        call collect_in_zero_array(SEDT)
        do k=0,kcm
          call collect_in_zero(QQ(:,k))
          call collect_in_zero(QQL(:,k))
        enddo
        DO NW=0,NWQV
          call collect_in_zero_array(WQV(:,:,NW))
        ENDDO
        DO NSP=1,NXSP
          call collect_in_zero_array(WQVX(:,:,NSP))
        ENDDO
        call collect_in_zero_array(QSUM)
        IF(MYRANK.EQ.0)THEN
          PRINT*, n,'TSX  = ', sum(abs(dble(TSX)))
          PRINT*, n,'TSY  = ', sum(abs(dble(TSY)))
          PRINT*, n,'TBX  = ', sum(abs(dble(TBX)))
          PRINT*, n,'TBY  = ', sum(abs(dble(TBY)))
          PRINT*, n,'AV   = ', sum(abs(dble(AV)))
          PRINT*, n,'AB   = ', sum(abs(dble(AB)))
          PRINT*, n,'AQ   = ', sum(abs(dble(AQ)))
          PRINT*, n,'HP   = ', sum(abs(dble(HP)))
          PRINT*, n,'HU   = ', sum(abs(dble(HU)))
          PRINT*, n,'HV   = ', sum(abs(dble(HV)))
          PRINT*, n,'P    = ', sum(abs(dble(P)))
          PRINT*, n,'U    = ', sum(abs(dble(U)))
          PRINT*, n,'V    = ', sum(abs(dble(V)))
          PRINT*, n,'W    = ', sum(abs(dble(W)))
          PRINT*, n,'TEM  = ', sum(abs(dble(TEM)))
          PRINT*, n,'SAL  = ', sum(abs(dble(SAL)))
          PRINT*, n,'TEMB = ', sum(abs(dble(TEMB)))
          PRINT*, n,'SEDT = ', sum(abs(dble(SEDT)))
          PRINT*, n,'QQ   = ', sum(abs(dble(QQ)))
          PRINT*, n,'QQL  = ', sum(abs(dble(QQL)))
          PRINT*, n,'WQV  = ', sum(abs(dble(WQV)))
          PRINT*, n,'WQVX = ', sum(abs(dble(WQVX)))
          PRINT*, n,'QSUM = ', sum(abs(dble(QSUM)))
        ENDIF
      ENDIF
      ENDIF

      IF(MOD(N,NTSPTC/WT_RATIO).EQ.0)THEN
         MPI_HOSTSPOTS    ='NULL'
         MPI_HOSTSPOTS( 1)='HDMT2T_TOTAL'
         MPI_HOSTSPOTS( 4)='CALAVB'
         MPI_HOSTSPOTS( 6)='CALTSXY'
         MPI_HOSTSPOTS( 7)='CALEXP2T'
         MPI_HOSTSPOTS( 8)='CALCSER'
         MPI_HOSTSPOTS( 9)='CALPUV2C'
         MPI_HOSTSPOTS(10)='ADVANCE'
         MPI_HOSTSPOTS(11)='CALUVW'
         MPI_HOSTSPOTS(12)='CALCONC'
         MPI_HOSTSPOTS(13)='SEDIMENT'
         MPI_HOSTSPOTS(15)='DSLLC_WRITE'
         MPI_HOSTSPOTS(16)='WQ3D'
         MPI_HOSTSPOTS(17)='CALBUOY'
         MPI_HOSTSPOTS(19)='NLEVEL'
         MPI_HOSTSPOTS(20)='CALHDMF'
         MPI_HOSTSPOTS(21)='CALTBXY'
         MPI_HOSTSPOTS(23)='QQSQR'
         MPI_HOSTSPOTS(27)='CALQQ2T'
         MPI_HOSTSPOTS(35)='LSQHARM'
         MPI_HOSTSPOTS(37)='SURFPLT'
         MPI_HOSTSPOTS(39)='VELPLTH'
         MPI_HOSTSPOTS(41)='SALPTH'
         MPI_HOSTSPOTS(43)='EEXPOUT'
         IF(NPROCS.GE.2)THEN
            MPI_HOSTSPOTS(51)='BCAST1'
            MPI_HOSTSPOTS(52)='BCAST2'
            MPI_HOSTSPOTS(53)='BCAST3'
            MPI_HOSTSPOTS(54)='BCAST4'
            MPI_HOSTSPOTS(55)='BCAST5'
            MPI_HOSTSPOTS(56)='BCAST6'
            MPI_HOSTSPOTS(61)='BARRIER1'
            MPI_HOSTSPOTS(62)='BARRIER2'
            MPI_HOSTSPOTS(63)='BARRIER3'
            MPI_HOSTSPOTS(64)='BARRIER4'
            MPI_HOSTSPOTS(65)='BARRIER5'
         ENDIF

         IF(MYRANK.EQ.0)THEN
         PRINT*,'HDMT2T'
         DO II=1,65
            IF(TRIM(MPI_HOSTSPOTS(000+II)).NE.'NULL')
     &      WRITE(*,'(I5,2X,A20,F10.3)') II,MPI_HOSTSPOTS(000+II),
     &                       (WT_RATIO*REAL(MPI_WTIMES(000+II)))
         ENDDO
         ENDIF
      ENDIF

      GOTO 1001
!      IF(.NOT.KBHIT())GOTO 1001
!      I1=GETCH()
!      WRITE(*,'(A)')'PROGRAM PAUSED BY USER'
!      WRITE(*,'(A)')'  EFDC_DS: TO EXIT PRESS THE SAME KEY'
!      WRITE(*,'(A)')'  EFDC_DS: TO CONTINUE RUN PRESS ANY OTHER KEY'
!      I2=GETCH()
!      IF(I1.NE.I2)GOTO 1001
!ykchoi]
C
 1000 CONTINUE
C
C**********************************************************************C
C
C **  TIME LOOP COMPLETED
C
      CALL CPU_TIME(T1TMP)
      THDMT=THDMT+T1TMP-TTMP
C
C**********************************************************************C
C *** EE BEGIN BLOCK
C       MOVED THE TIMING OUTPUT BLOCK TO THE MAIN AAEFDC TO ELIMINATE
C       UNNECESSARY DUPLICATION
C *** EE END BLOCK
C**********************************************************************C
C
C2000 CONTINUE
C
C**********************************************************************C
C
C **  PRINT FINAL RESULTS
C
      IF(MYRANK.EQ.0) CALL OUTPUT2
C
C**********************************************************************C
C
C **  WRITE RESTART FILE
C
C      IF(ISRESTO.EQ.-1.OR.ISRESTO.EQ.-11)THEN  ! GEOSR : JGCHO 2011.6.15
      IF(ISRESTO.EQ.-1.OR.ISRESTO.EQ.-11.OR.ISRESTO.LT.-20)THEN  ! GEOSR : JGCHO 2011.6.15
        if(myrank.eq.0) print*,'R2ESTOUT(0)'
        CALL RESTOUT(0)
        IF(ISTRAN(8).GE.1)THEN
          IF(IWQRST.EQ.1) CALL WWQRST(0)
          IF(IWQBEN.EQ.1 .AND. ISMRST.EQ.1) CALL WSMRST(0)
        ENDIF
      ENDIF
      IF(ISRESTO.EQ.-2)THEN
        CALL RESTMOD
      ENDIF
! { GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.6.3
      IF(ISRESTO.LT.-20)THEN
        if(myrank.eq.0) print*,'R2ESTOUT(-19)'
        CALL RESTOUT(-19)
        IF(ISTRAN(8).GE.1)THEN
          IF(IWQRST.EQ.1) CALL WWQRST(1)
          IF(IWQBEN.EQ.1 .AND. ISMRST.EQ.1) CALL WSMRST(1)
        ENDIF
      ENDIF
! } GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.6.3
C
C**********************************************************************C
C
C **  COMPLETE LEAST SQUARES HARMONIC ANALYSIS
C
      LSLSHA=1
      IF(ISLSHA.EQ.1) CALL LSQHARM
C
C**********************************************************************C
C
C **  OUTPUT COURANT NUMBER DIAGNOSTICS
C
C *** DSLLC BEGIN BLOCK
      IF(MYRANK.EQ.0)THEN
      IF(ISINWV.GT.0.AND.DEBUG)THEN
        OPEN(1,FILE='CFLMAX.OUT')
        CLOSE(1,STATUS='DELETE')
        OPEN(1,FILE='CFLMAX.OUT')
C
        DO L=2,LA
          WRITE(1,1991)IL(L),JL(L),(CFLUUU(L,K),K=1,KC)
          WRITE(1,1992)(CFLVVV(L,K),K=1,KC)
          WRITE(1,1992)(CFLWWW(L,K),K=1,KC)
          WRITE(1,1992)(CFLCAC(L,K),K=1,KC)
        ENDDO
C
        CLOSE(1)
      ENDIF
      ENDIF
C *** DSLLC END BLOCK
C
 1991 FORMAT(2I5,12F8.3)
 1992 FORMAT(10X,12F8.3)
 1993 FORMAT(2I5,E13.5)
C
C**********************************************************************C
C
C **  OUTPUT COSMETIC VOLUME LOSSES FORM DRY CELLS
C
      IF(MYRANK.EQ.0)THEN
      IF(NDRYSTP.LT.0.AND.DEBUG) THEN
C
        OPEN(1,FILE='DRYLOSS.OUT')
        CLOSE(1,STATUS='DELETE')
        OPEN(1,FILE='DRYLOSS.OUT')
C
        DO L=2,LA
          WRITE(1,1993)IL(L),JL(L),VDWASTE(L)
        ENDDO
C
        CLOSE(1)
C
      ENDIF
      ENDIF
C
C**********************************************************************C
C
C **  OUTPUT FINAL FOOD CHAIN AVERAGING PERIOD
C
      IF(ISTRAN(5).GE.1.AND.ISFDCH.GE.1)CALL FOODCHAIN(1)
C
C**********************************************************************C
C
C **  OUTPUT FINAL MASS AND VOLUME BALANCES
C
      IF(IS2TIM.GE.1) THEN
        IF(ISBAL.GE.1)THEN
          CALL BAL2T5
        ENDIF
      ENDIF
C
C**********************************************************************C
C
      CLOSE(90)
      CLOSE(98)

      RETURN
      END
