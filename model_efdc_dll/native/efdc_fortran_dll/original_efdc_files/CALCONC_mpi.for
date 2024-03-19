      SUBROUTINE CALCONC_mpi(ISTL_,IS2TL_)
C
C CHANGE RECORD
C  MODIFIED CALLS TO CALBAL AND BUDGET SUBROUTINES
C  ADDED CALLS TO BAL2T2, BAL2T3
C **  SUBROUTINE CALCULATES THE CONCENTRATION OF DISSOLVED AND
C **  SUSPENDED CONSTITUTENTS, INCLUDING SALINITY, TEMPERATURE, DYE AND
C **  AND SUSPENDED SEDIMENT AT TIME LEVEL (N+1). THE VALUE OF ISTL
C **  INDICATES THE NUMBER OF TIME LEVELS IN THE STEP
C
      USE GLOBAL
      USE MPI

      IMPLICIT NONE
      INTEGER::K,L,NT,NS,ND,NSID,LDATA,NLC,IWASM,NDAYA,NX
      INTEGER::IBALSTDT,NTMP,ISTL_,IS2TL_,M,LF,LL
      REAL::TTMP,T1TMP,RCDZKMK,CONASMOLD,SALASM
      REAL::TEMASM,DYEASM,SFLASM,RCDZKK,CCUBTMP,CCMBTMP
      REAL::DELTD2,CDYETMP,TMP,DAGE

      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::EEB
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::CCLBTMP
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::EEB_2D,CCLBTMP_2D

      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TOXASM
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SEDASM
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SNDASM

      IF(.NOT.ALLOCATED(EEB))THEN
        ALLOCATE(EEB(LCM))
        ALLOCATE(CCLBTMP(LCM))
        ALLOCATE(EEB_2D(LCM,KCM))
        ALLOCATE(CCLBTMP_2D(LCM,KCM))
        ALLOCATE(TOXASM(NTXM))
        ALLOCATE(SEDASM(NSCM))
        ALLOCATE(SNDASM(NSNM))
        EEB=0.0
        CCLBTMP=0.0
        EEB_2D=0.0
        CCLBTMP_2D=0.0
        TOXASM=0.0
        SEDASM=0.0
        SNDASM=0.0
      ENDIF

      DELT=DT2
      IF(ISTL_.EQ.2)THEN
        IF(ISDYNSTP.EQ.0)THEN
          DELT=DT
        ELSE
          DELT=DTDYN
        END IF
      ENDIF
      DELTD2=DELT
      S1TIME=MPI_TIC()
      IF(IS2TIM.GE.1) THEN
        IF(ISBAL.GE.1)THEN
          CALL BAL2T3A
        ENDIF
      ENDIF
      MPI_WTIMES(601)=MPI_WTIMES(601)+MPI_TOC(S1TIME)
C
C **  VERTICAL DIFFUSION EXPLICIT HALF STEP CALCULATION
C
C 500 CONTINUE
C
C **  3D ADVECTI0N TRANSPORT CALCULATION-COSMIC INITIALIZATION
C
      IF(ISCOSMIC.EQ.1)THEN
        S1TIME=MPI_TIC()
        DO K=1,KC
          RCOSMICX(1 ,K)=0.
          RCOSMICX(LC,K)=0.
          RCOSMICY(1 ,K)=0.
          RCOSMICY(LC,K)=0.
          RCOSMICZ(1 ,K)=0.
          RCOSMICZ(LC,K)=0.
          COSMICXP(1 ,K)=0.
          COSMICXP(LC,K)=0.
          COSMICYP(1 ,K)=0.
          COSMICYP(LC,K)=0.
          COSMICZP(1 ,K)=0.
          COSMICZP(LC,K)=0.
          COSMICXN(1 ,K)=0.
          COSMICXN(LC,K)=0.
          COSMICYN(1 ,K)=0.
          COSMICYN(LC,K)=0.
          COSMICZN(1 ,K)=0.
          COSMICZN(LC,K)=0.
        ENDDO
!$OMP PARALLEL DO
        DO L=LMPI1,LMPILC
          COSMICZP(L,0)=0.
          COSMICZP(L,KC)=0.
          COSMICZN(L,0)=0.
          COSMICZN(L,KC)=0.
        ENDDO
        MPI_WTIMES(602)=MPI_WTIMES(602)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO PRIVATE(TMP)
          DO L=LMPI2,LMPILA
            RCOSMICX(L,K)=-1.
            TMP=U2(L,K)*U2(L+1,K)
            IF(TMP.LT.0.) RCOSMICX(L,K)=0.
            RCOSMICY(L,K)=-1.
            TMP=V2(L,K)*V2(LNC(L),K)
            IF(TMP.LT.0.) RCOSMICY(L,K)=0.
            RCOSMICZ(L,K)=-1.
            TMP=W2(L,K)*W2(L,K-1)
            IF(TMP.LT.0.) RCOSMICZ(L,K)=0.
          ENDDO
        ENDDO
        MPI_WTIMES(603)=MPI_WTIMES(603)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            COSMICXP(L,K)=DELT*DXIU(L)*U2(L,K)
            COSMICYP(L,K)=DELT*DYIV(L)*V2(L,K)
          ENDDO
        ENDDO
        MPI_WTIMES(604)=MPI_WTIMES(604)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            COSMICXN(L,K)=MIN(COSMICXP(L,K),0.)
            COSMICYN(L,K)=MIN(COSMICYP(L,K),0.)
            COSMICXP(L,K)=MAX(COSMICXP(L,K),0.)
            COSMICYP(L,K)=MAX(COSMICYP(L,K),0.)
          ENDDO
        ENDDO
        MPI_WTIMES(605)=MPI_WTIMES(605)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(KC.GE.2.AND.ISTL_.EQ.3)THEN
          DO K=1,KS
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              COSMICZP(L,K)=DELT*DZIG(K)*W2(L,K)/H1P(L)
            ENDDO
          ENDDO
          DO K=1,KS
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              COSMICZN(L,K)=MIN(COSMICZP(L,K),0.)
              COSMICZP(L,K)=MAX(COSMICZP(L,K),0.)
            ENDDO
          ENDDO
        ENDIF
        MPI_WTIMES(606)=MPI_WTIMES(606)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(KC.GE.2.AND.ISTL_.EQ.2)THEN
          DO K=1,KS
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              COSMICZP(L,K)=2.*DELT*DZIG(K)*W2(L,K)/(HP(L)+H1P(L))
            ENDDO
          ENDDO
          DO K=1,KS
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              COSMICZN(L,K)=MIN(COSMICZP(L,K),0.)
              COSMICZP(L,K)=MAX(COSMICZP(L,K),0.)
            ENDDO
          ENDDO
        ENDIF
        MPI_WTIMES(607)=MPI_WTIMES(607)+MPI_TOC(S1TIME)
      ENDIF
C
C **  3D ADVECTI0N TRANSPORT CALCULATION
C
C **  PRESPECIFY THE UPWIND CELLS FOR 3D ADVECTION
C
      S1TIME=MPI_TIC()
      DO K=1,KC
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          IF(LMASKDRY(L))THEN
            IF(UHDY2(L,K).GE.0.0)THEN
              LUPU(L,K)=L-1
            ELSE
              LUPU(L,K)=L
            END IF
            IF(VHDX2(L,K).GE.0.0)THEN
              LUPV(L,K)=LSC(L)
            ELSE
              LUPV(L,K)=L
            END IF
          END IF
        ENDDO
      ENDDO
      MPI_WTIMES(608)=MPI_WTIMES(608)+MPI_TOC(S1TIME)
      S1TIME=MPI_TIC()
      IF(KC.GT.1)THEN
        DO K=1,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              IF(W2(L,K).GE.0.)THEN
                KUPW(L,K)=K
              ELSE
                KUPW(L,K)=K+1  ! *** DSLLC SINGLE LINE CHANGE, CHANGED K-1 TO K+1
              END IF
            END IF
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(609)=MPI_WTIMES(609)+MPI_TOC(S1TIME)
C
      S1TIME=MPI_TIC()
      IF(ISTRAN(1).EQ.1.AND.ISCDCA(1).LT.4)
     &    CALL CALTRAN_mpi (ISTL_,IS2TL_,1,1,SAL,SAL1)
      MPI_WTIMES(610)=MPI_WTIMES(610)+MPI_TOC(S1TIME)
      IF(PRINT_SUM)THEN
      call collect_in_zero_array(TEM)
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'3TEM  = ', sum(abs(dble(TEM)))
      ENDIF
      ENDIF
      S1TIME=MPI_TIC()
      IF(ISTRAN(2).EQ.1.AND.ISCDCA(2).LT.4)
     &    CALL CALTRAN_mpi (ISTL_,IS2TL_,2,2,TEM,TEM1)
      MPI_WTIMES(611)=MPI_WTIMES(611)+MPI_TOC(S1TIME)
      IF(PRINT_SUM)THEN
      call collect_in_zero_array(TEM)
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'4TEM  = ', sum(abs(dble(TEM)))
      ENDIF
      ENDIF
      S1TIME=MPI_TIC()
      IF(ISTRAN(3).EQ.1.AND.ISCDCA(3).LT.4)
     &    CALL CALTRAN_mpi (ISTL_,IS2TL_,3,3,DYE,DYE1)
      MPI_WTIMES(612)=MPI_WTIMES(612)+MPI_TOC(S1TIME)
      S1TIME=MPI_TIC()
      IF(ISTRAN(5).EQ.1.AND.ISCDCA(5).LT.4)THEN
        DO NT=1,NTOX
          M=MSVTOX(NT)
          CALL CALTRAN_mpi (ISTL_,IS2TL_,5,M,TOX(1,1,NT),TOX1(1,1,NT))
        ENDDO
      ENDIF
      MPI_WTIMES(613)=MPI_WTIMES(613)+MPI_TOC(S1TIME)
      IF(PRINT_SUM)THEN
      DO NS=1,NSED
        call collect_in_zero_array(SED(:,:,NS))
      ENDDO
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'SED01  = ', sum(abs(dble(SED)))
      ENDIF
      ENDIF
      S1TIME=MPI_TIC()
      IF(ISTRAN(6).EQ.1.AND.ISCDCA(6).LT.4)THEN
        DO NS=1,NSED
          M=MSVSED(NS)
          CALL CALTRAN_mpi (ISTL_,IS2TL_,6,M,SED(1,1,NS),SED1(1,1,NS))
        ENDDO
      ENDIF
      MPI_WTIMES(614)=MPI_WTIMES(614)+MPI_TOC(S1TIME)
      IF(PRINT_SUM)THEN
      DO NS=1,NSED
        call collect_in_zero_array(SED(:,:,NS))
      ENDDO
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'SED10  = ', sum(abs(dble(SED)))
      ENDIF
      ENDIF
C
      S1TIME=MPI_TIC()
      IF(ISTRAN(7).EQ.1.AND.ISCDCA(7).LT.4)THEN
        DO NS=1,NSND
          M=MSVSND(NS)
          CALL CALTRAN_mpi (ISTL_,IS2TL_,6,M,SND(1,1,NS),SND1(1,1,NS))
        ENDDO
      ENDIF
      MPI_WTIMES(615)=MPI_WTIMES(615)+MPI_TOC(S1TIME)
C
C **  3D COSMIC ADVECTI0N TRANSPORT CALCULATION
C
      IF(ISCOSMIC.EQ.1)THEN
        CALL CPU_TIME(TTMP)
        S1TIME=MPI_TIC()
        IF(ISTRAN(1).EQ.1.AND.ISCDCA(1).EQ.4)
     &      CALL COSTRANW (ISTL,IS2TL,1,1,SAL,SAL1)
        MPI_WTIMES(616)=MPI_WTIMES(616)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(ISTRAN(2).EQ.1.AND.ISCDCA(2).EQ.4)
     &      CALL COSTRANW (ISTL,IS2TL,2,2,TEM,TEM1)
        MPI_WTIMES(617)=MPI_WTIMES(617)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(ISTRAN(3).EQ.1.AND.ISCDCA(3).EQ.4)
     &      CALL COSTRANW (ISTL,IS2TL,3,3,DYE,DYE1)
        MPI_WTIMES(618)=MPI_WTIMES(618)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(ISTRAN(1).EQ.1.AND.ISCDCA(1).EQ.5)
     &      CALL COSTRAN (ISTL,IS2TL,1,1,SAL,SAL1)
        MPI_WTIMES(619)=MPI_WTIMES(619)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(ISTRAN(2).EQ.1.AND.ISCDCA(2).EQ.5)
     &      CALL COSTRAN (ISTL,IS2TL,2,2,TEM,TEM1)
        MPI_WTIMES(620)=MPI_WTIMES(620)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(ISTRAN(3).EQ.1.AND.ISCDCA(3).EQ.5)
     &      CALL COSTRAN (ISTL,IS2TL,3,3,DYE,DYE1)
        MPI_WTIMES(621)=MPI_WTIMES(621)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(ISTRAN(5).EQ.1.AND.ISCDCA(5).EQ.4)THEN
          DO NT=1,NTOX
            M=MSVTOX(NT)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                TVAR1S(L,K)=TOX1(L,K,NT)
                TVAR2S(L,K)=TOX(L,K,NT)
              ENDDO
            ENDDO
            CALL COSTRANW (ISTL,IS2TL,5,M,TVAR2S,TVAR1S)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                TOX1(L,K,NT)=TVAR1S(L,K)
                TOX(L,K,NT)=TVAR2S(L,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        MPI_WTIMES(622)=MPI_WTIMES(622)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(ISTRAN(5).EQ.1.AND.ISCDCA(5).EQ.5)THEN
          DO NT=1,NTOX
            M=MSVTOX(NT)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                TVAR1S(L,K)=TOX1(L,K,NT)
                TVAR2S(L,K)=TOX(L,K,NT)
              ENDDO
            ENDDO
            CALL COSTRAN (ISTL,IS2TL,5,M,TVAR2S,TVAR1S)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                TOX1(L,K,NT)=TVAR1S(L,K)
                TOX(L,K,NT)=TVAR2S(L,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        MPI_WTIMES(623)=MPI_WTIMES(623)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(ISTRAN(6).EQ.1.AND.ISCDCA(6).EQ.4)THEN
          DO NS=1,NSED
            M=MSVSED(NS)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                TVAR1S(L,K)=SED1(L,K,NS)
                TVAR2S(L,K)=SED(L,K,NS)
              ENDDO
            ENDDO
            CALL COSTRANW (ISTL,IS2TL,6,M,TVAR2S,TVAR1S)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                SED1(L,K,NS)=TVAR1S(L,K)
                SED(L,K,NS)=TVAR2S(L,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        MPI_WTIMES(624)=MPI_WTIMES(624)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(ISTRAN(6).EQ.1.AND.ISCDCA(6).EQ.5)THEN
          DO NS=1,NSED
            M=MSVSED(NS)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                TVAR1S(L,K)=SED1(L,K,NS)
                TVAR2S(L,K)=SED(L,K,NS)
              ENDDO
            ENDDO
            CALL COSTRAN (ISTL,IS2TL,6,M,TVAR2S,TVAR1S)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                SED1(L,K,NS)=TVAR1S(L,K)
                SED(L,K,NS)=TVAR2S(L,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        MPI_WTIMES(625)=MPI_WTIMES(625)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(ISTRAN(7).EQ.1.AND.ISCDCA(7).EQ.4)THEN
          DO NS=1,NSND
            M=MSVSND(NS)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                TVAR1S(L,K)=SND1(L,K,NS)
                TVAR2S(L,K)=SND(L,K,NS)
              ENDDO
            ENDDO
            CALL COSTRANW (ISTL,IS2TL,7,M,TVAR2S,TVAR1S)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                SND1(L,K,NS)=TVAR1S(L,K)
                SND(L,K,NS)=TVAR2S(L,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        MPI_WTIMES(626)=MPI_WTIMES(626)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(ISTRAN(7).EQ.1.AND.ISCDCA(7).EQ.5)THEN
          DO NS=1,NSND
            M=MSVSND(NS)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                TVAR1S(L,K)=SND1(L,K,NS)
                TVAR2S(L,K)=SND(L,K,NS)
              ENDDO
            ENDDO
            CALL COSTRAN (ISTL,IS2TL,7,M,TVAR2S,TVAR1S)
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI1,LMPILC
                SND1(L,K,NS)=TVAR1S(L,K)
                SND(L,K,NS)=TVAR2S(L,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        CALL CPU_TIME(T1TMP)
        TSADV=TSADV+T1TMP-TTMP
      ENDIF
      MPI_WTIMES(627)=MPI_WTIMES(627)+MPI_TOC(S1TIME)
      IF(PRINT_SUM)THEN
      call collect_in_zero_array(TEM)
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'5TEM  = ', sum(abs(dble(TEM)))
      ENDIF
      ENDIF
C
C **  1D ADVECTI0N TRANSPORT CALCULATION
C
C *** REMOVED 2004-09-19  PMC
C
C **  SURFACE AND INTERNAL HEAT SOURCE-SINK CALCULATION
C
      S1TIME=MPI_TIC()
      IF(ISTRAN(2).GE.1) CALL CALHEAT_mpi(ISTL_)
      MPI_WTIMES(628)=MPI_WTIMES(628)+MPI_TOC(S1TIME)
      IF(PRINT_SUM)THEN
      call collect_in_zero_array(TEM)
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'6TEM  = ', sum(abs(dble(TEM)))
      ENDIF
      ENDIF
C
C **  FULL IMPLICIT DYE AND TOXIC CONTAMINANT DECAY/GROWTH CALCULATION
C
      S1TIME=MPI_TIC()
      IF(ISTRAN(3).GE.1)THEN
        ! *** DSLLC BEGIN BLOCK
        IF(RKDYE.EQ.1000.0)THEN
          ! *** Age of Water
          DAGE=DELT/86400.
          DO K=1,KC
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              DYE(L,K)=DYE(L,K)+DAGE
            ENDDO
          ENDDO
        ELSE
          IF(RKDYE.LT.0.0)THEN
            CDYETMP=EXP(-RKDYE*DELT)
          ELSE
            CDYETMP=1./(1.+DELT*RKDYE)
          ENDIF

          DO ND=1,NDM
            LF=2+(ND-1)*LDM
            LL=LF+LDM-1
            DO K=1,KC
              DO L=LF,LL
                DYE(L,K)=CDYETMP*DYE(L,K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        ! *** DSLLC END BLOCK
      ENDIF
      MPI_WTIMES(629)=MPI_WTIMES(629)+MPI_TOC(S1TIME)
C
C **  BOTTOM AND INTERNAL SEDIMENT AND TOXIC CONTAMINAT
C **  SOURCE-SINK CALCULATION
C
C
C **  SEDIMENT AND TOXICS SETTLING,DEPOSITION,RESUSPENSION,ETC
C **  FOR TWO TIME LEVEL SIMULATION
C
      IF(ISTRAN(6).GE.1.OR.ISTRAN(7).GE.1) THEN
        IF(IS2TIM.GE.1)THEN
          ISEDDTC=ISEDDTC+1
          IF(ISEDDTC.EQ.1)THEN
            DTSED=DELT
          ELSE
            DTSED=DTSED+DELT
          ENDIF
          IBALSTDT=0
          S1TIME=MPI_TIC()
          IF(ISEDDTC.EQ.ISEDDT)THEN
            CALL SSEDTOX(ISTL,IS2TL,1.0)
            IBALSTDT=1
            ISEDDTC=0
          ENDIF
          MPI_WTIMES(630)=MPI_WTIMES(630)+MPI_TOC(S1TIME)
C
C **  SEDIMENT AND TOXICS SETTLING,DEPOSITION,RESUSPENSION,ETC
C **  FOR THREE TIME LEVEL SIMULATION
C
        ELSE  ! IF(IS2TIM.EQ.0)THEN
          S1TIME=MPI_TIC()
          IBALSTDT=0
          DTSED=FLOAT(NTSTBC)*DT
          CALL SSEDTOX(ISTL,IS2TL,1.0)
          IBALSTDT=1
          MPI_WTIMES(631)=MPI_WTIMES(631)+MPI_TOC(S1TIME)
        ENDIF
      ENDIF
C
C 888 FORMAT('N,IC,I,DTS,DT = ',3I5,2F12.8)
C 889 FORMAT('N,IC,I,DTS = ',3I5,F12.8,12X,'SSEDTOX CALLED')
      IF(PRINT_SUM)THEN
      call collect_in_zero_array(TEM)
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'7TEM  = ', sum(abs(dble(TEM)))
      ENDIF
      ENDIF
C
C **  OPTIONAL MASS BALANCE CALCULATION
C
      IF(IS2TIM.EQ.0) THEN
        IF(ISTL_.NE.2.AND.ISBAL.GE.1)THEN
          S1TIME=MPI_TIC()
          CALL CALBAL2
          CALL CALBAL3
          MPI_WTIMES(632)=MPI_WTIMES(632)+MPI_TOC(S1TIME)
          NTMP=MOD(N,2)
          IF(NTMP.EQ.0)THEN
            S1TIME=MPI_TIC()
            CALL CBALEV2
            CALL CBALEV3
            MPI_WTIMES(633)=MPI_WTIMES(633)+MPI_TOC(S1TIME)
          ELSE
            S1TIME=MPI_TIC()
            CALL CBALOD2
            CALL CBALOD3
            MPI_WTIMES(634)=MPI_WTIMES(634)+MPI_TOC(S1TIME)
          ENDIF
        ENDIF
      ENDIF
C
C **  CALLS TO TWO-TIME LEVEL BALANCES
C
      IF(IS2TIM.GE.1) THEN
        IF(ISBAL.GE.1)THEN
          S1TIME=MPI_TIC()
          CALL BAL2T2
          CALL BAL2T3B(IBALSTDT)
          MPI_WTIMES(635)=MPI_WTIMES(635)+MPI_TOC(S1TIME)
        ENDIF
      ENDIF
C
C **  SEDIMENT BUDGET CALCULATION    (DLK 10/15)
C
      IF(IS2TIM.EQ.0) THEN
        IF(ISTL_.NE.2.AND.ISSBAL.GE.1)THEN
          S1TIME=MPI_TIC()
          CALL BUDGET2
          CALL BUDGET3
          MPI_WTIMES(636)=MPI_WTIMES(636)+MPI_TOC(S1TIME)
        ENDIF
      ENDIF
      IF(PRINT_SUM)THEN
      call collect_in_zero_array(TEM)
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'8TEM  = ', sum(abs(dble(TEM)))
      ENDIF
      ENDIF
C
C **  VERTICAL DIFFUSION IMPLICIT HALF STEP CALCULATION
C
      IF(KC.EQ.1) GOTO 1500
      CALL CPU_TIME(TTMP)
      RCDZKK=-DELTD2*CDZKK(1)
      S1TIME=MPI_TIC()
!$OMP PARALLEL DO PRIVATE(CCUBTMP,CCMBTMP)
      DO L=LMPI2,LMPILA
          CCUBTMP=RCDZKK*HPI(L)*AB(L,1)
          CCMBTMP=1.-CCUBTMP
          EEB(L)=1./CCMBTMP
          CU1(L,1)=CCUBTMP*EEB(L)
      ENDDO
      IF(ISTRAN(1).GE.1)THEN
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          SAL(L,1)=SAL(L,1)*EEB(L)
        ENDDO
      ENDIF
      IF(ISTRAN(2).GE.1)THEN
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          TEM(L,1)=TEM(L,1)*EEB(L)
        ENDDO
      ENDIF
      IF(ISTRAN(3).GE.1)THEN
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          DYE(L,1)=DYE(L,1)*EEB(L)
        ENDDO
      ENDIF
      IF(ISTRAN(5).GE.1)THEN
        DO NT=1,NTOX
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            TOX(L,1,NT)=TOX(L,1,NT)*EEB(L)
          ENDDO
        ENDDO
      ENDIF
      IF(ISTRAN(6).GE.1)THEN
        DO NS=1,NSED
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            SED(L,1,NS)=SED(L,1,NS)*EEB(L)
          ENDDO
        ENDDO
      ENDIF
      IF(ISTRAN(7).GE.1)THEN
        DO NS=1,NSND
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            SND(L,1,NS)=SND(L,1,NS)*EEB(L)
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(637)=MPI_WTIMES(637)+MPI_TOC(S1TIME)
      S1TIME=MPI_TIC()

      DO K=2,KS
        RCDZKMK=-DELTD2*CDZKMK(K)
        RCDZKK=-DELTD2*CDZKK(K)
!$OMP PARALLEL DO PRIVATE(CCUBTMP,CCMBTMP)
        DO L=LMPI2,LMPILA
          CCLBTMP_2D(L,K)=RCDZKMK*HPI(L)*AB(L,K-1)
          CCUBTMP=RCDZKK*HPI(L)*AB(L,K)
          CCMBTMP=1.-CCLBTMP_2D(L,K)-CCUBTMP
          EEB_2D(L,K)=1./(CCMBTMP-CCLBTMP_2D(L,K)*CU1(L,K-1))
          CU1(L,K)=CCUBTMP*EEB_2D(L,K)
        ENDDO
      ENDDO

      IF(ISTRAN(1).GE.1)THEN
        DO K=2,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            SAL(L,K)=(SAL(L,K)-CCLBTMP_2D(L,K)*SAL(L,K-1))*EEB_2D(L,K)
          ENDDO
        ENDDO
      ENDIF

      IF(ISTRAN(2).GE.1)THEN
        DO K=2,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            TEM(L,K)=(TEM(L,K)-CCLBTMP_2D(L,K)*TEM(L,K-1))*EEB_2D(L,K)
          ENDDO
        ENDDO
      ENDIF

      IF(ISTRAN(3).GE.1)THEN
        DO K=2,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            DYE(L,K)=(DYE(L,K)-CCLBTMP_2D(L,K)*DYE(L,K-1))*EEB_2D(L,K)
          ENDDO
        ENDDO
      ENDIF
      IF(ISTRAN(5).GE.1)THEN
        DO NT=1,NTOX
          DO K=2,KS
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              TOX(L,K,NT)=(TOX(L,K,NT)-CCLBTMP_2D(L,K)*TOX(L,K-1,NT))
     &                    *EEB_2D(L,K)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF(ISTRAN(6).GE.1)THEN
        DO NS=1,NSED
          DO K=2,KS
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              SED(L,K,NS)=(SED(L,K,NS)-CCLBTMP_2D(L,K)*SED(L,K-1,NS))
     &                    *EEB_2D(L,K)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF(ISTRAN(7).GE.1)THEN
        DO NS=1,NSND
          DO K=2,KS
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
             SND(L,K,NS)=(SND(L,K,NS)-CCLBTMP_2D(L,K)*SND(L,K-1,NS))
     &                   *EEB_2D(L,K)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(638)=MPI_WTIMES(638)+MPI_TOC(S1TIME)
      IF(PRINT_SUM)THEN
      call collect_in_zero_array(TEM)
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'9TEM  = ', sum(abs(dble(TEM)))
      ENDIF
      ENDIF
      K=KC
      RCDZKMK=-DELTD2*CDZKMK(K)
      S1TIME=MPI_TIC()
!$OMP PARALLEL DO PRIVATE(CCMBTMP)
      DO L=LMPI2,LMPILA
        CCLBTMP(L)=RCDZKMK*HPI(L)*AB(L,K-1)
        CCMBTMP=1.-CCLBTMP(L)
        EEB(L)=1./(CCMBTMP-CCLBTMP(L)*CU1(L,K-1))
      ENDDO
      IF(ISTRAN(1).GE.1)THEN
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          SAL(L,K)=(SAL(L,K)-CCLBTMP(L)*SAL(L,K-1))*EEB(L)
        ENDDO
      ENDIF
      IF(ISTRAN(2).GE.1)THEN
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          TEM(L,K)=(TEM(L,K)-CCLBTMP(L)*TEM(L,K-1))*EEB(L)
        ENDDO
      ENDIF
      IF(ISTRAN(3).GE.1)THEN
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          DYE(L,K)=(DYE(L,K)-CCLBTMP(L)*DYE(L,K-1))*EEB(L)
        ENDDO
      ENDIF
      IF(ISTRAN(5).GE.1)THEN
        DO NT=1,NTOX
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            TOX(L,K,NT)=(TOX(L,K,NT)-CCLBTMP(L)*TOX(L,K-1,NT))*EEB(L)
          ENDDO
        ENDDO
      ENDIF
      IF(ISTRAN(6).GE.1)THEN
        DO NS=1,NSED
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            SED(L,K,NS)=(SED(L,K,NS)-CCLBTMP(L)*SED(L,K-1,NS))*EEB(L)
          ENDDO
        ENDDO
      ENDIF
      IF(ISTRAN(7).GE.1)THEN
        DO NS=1,NSND
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            SND(L,K,NS)=(SND(L,K,NS)-CCLBTMP(L)*SND(L,K-1,NS))*EEB(L)
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(639)=MPI_WTIMES(639)+MPI_TOC(S1TIME)
      S1TIME=MPI_TIC()
      IF(ISTRAN(1).GE.1)THEN
        DO K=KC-1,1,-1
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            SAL(L,K)=SAL(L,K)-CU1(L,K)*SAL(L,K+1)
          ENDDO
        ENDDO
      ENDIF

      IF(ISTRAN(2).GE.1)THEN
        DO K=KC-1,1,-1
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
              TEM(L,K)=TEM(L,K)-CU1(L,K)*TEM(L,K+1)
          ENDDO
        ENDDO
      ENDIF

      IF(ISTRAN(3).GE.1)THEN
        DO K=KC-1,1,-1
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            DYE(L,K)=DYE(L,K)-CU1(L,K)*DYE(L,K+1)
          ENDDO
        ENDDO
      ENDIF
      IF(ISTRAN(5).GE.1)THEN
        DO NT=1,NTOX
          DO K=KC-1,1,-1
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              TOX(L,K,NT)=TOX(L,K,NT)-CU1(L,K)*TOX(L,K+1,NT)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF(ISTRAN(6).GE.1)THEN
        DO NS=1,NSED
          DO K=KC-1,1,-1
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              SED(L,K,NS)=SED(L,K,NS)-CU1(L,K)*SED(L,K+1,NS)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF(ISTRAN(7).GE.1)THEN
        DO NS=1,NSND
          DO K=KC-1,1,-1
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              SND(L,K,NS)=SND(L,K,NS)-CU1(L,K)*SND(L,K+1,NS)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(640)=MPI_WTIMES(640)+MPI_TOC(S1TIME)
      IF(PRINT_SUM)THEN
      call collect_in_zero_array(TEM)
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'ATEM  = ', sum(abs(dble(TEM)))
      ENDIF
      ENDIF
      S1TIME=MPI_TIC()
C      DO K=1,KB
!$OMP PARALLEL DO
        DO L=LMPI1,LMPILC
          SEDBT(L,1:KB)=0.
          SNDBT(L,1:KB)=0.
        ENDDO
C      ENDDO
C      DO K=1,KC
!$OMP PARALLEL DO
        DO L=LMPI1,LMPILC
          SEDT(L,1:KC)=0.
          SNDT(L,1:KC)=0.
        ENDDO
C      ENDDO
      DO K=1,KB
        DO NS=1,NSED
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            SEDBT(L,K)=SEDBT(L,K)+SEDB(L,K,NS)
          ENDDO
        ENDDO
      ENDDO
      DO NS=1,NSND
        DO K=1,KB
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            SNDBT(L,K)=SNDBT(L,K)+SNDB(L,K,NS)
          ENDDO
        ENDDO
      ENDDO
      DO NS=1,NSED
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            SEDT(L,K)=SEDT(L,K)+SED(L,K,NS)
          ENDDO
        ENDDO
      ENDDO
      DO NS=1,NSND
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            SNDT(L,K)=SNDT(L,K)+SND(L,K,NS)
          ENDDO
        ENDDO
      ENDDO
      MPI_WTIMES(641)=MPI_WTIMES(641)+MPI_TOC(S1TIME)
      CALL CPU_TIME(T1TMP)
      TVDIF=TVDIF+T1TMP-TTMP
 1500 CONTINUE
C
C **  DATA ASSIMILATION
C
      S1TIME=MPI_TIC()
      IF(NLCDA.GT.0)THEN
        SALASM=0.0
        TEMASM=0.0
        DYEASM=0.0
        SFLASM=0.0
        DO NT=1,NTOX
          TOXASM(NT)=0.0
        ENDDO
        DO NS=1,NSED
          SEDASM(NS)=0.0
        ENDDO
        DO NS=1,NSND
          SNDASM(NS)=0.0
        ENDDO
C
        IWASM=0
C
        IF(N.EQ.1.AND.DEBUG.AND.MYRANK.EQ.0)THEN
          OPEN(1,FILE='CDATASM.DIA')
          CLOSE(1,STATUS='DELETE')
          OPEN(1,FILE='CDATASM.DIA')
          IWASM=1
          DO NLC=1,NLCDA
            DO NDAYA=1,NTC
              FSALASM(NDAYA,NLC)=0.
              FVOLASM(NDAYA,NLC)=0.
              FTEMASM(NDAYA,NLC)=0.
            ENDDO
          ENDDO
        ENDIF
C
        NDAYA=MOD(N,NTSPTC)
        NDAYA=1+(N-NDAYA)/NTSPTC
C
        IF(N.EQ.NTSPTC.AND.MYRANK.EQ.0)THEN
          OPEN(1,FILE='CDATASM.DIA',POSITION='APPEND')
          IWASM=1
          WRITE(1,1212)N,NDAYA
        ENDIF
C
        IF(ISCDA(1).GT.0)THEN
          DO K=1,KC
            DO NLC=1,NLCDA
              L=LIJ(ICDA(NLC),JCDA(NLC))
              CONASMOLD=SAL(L,K)
              NSID=NCSERA(NLC,1)
              IF(IWASM.EQ.1) WRITE(1,1111)N,NLC,ICDA(NLC),JCDA(NLC),NS,
     &            CSERT(K,NS,1),SAL(L,K)
              IF(ITPCDA(NLC).EQ.0)THEN
                IF(NSID.GT.0)THEN
                  IF(CSERT(K,NSID,1).GT.0)THEN
                    FSALASM(NDAYA,NLC)=FSALASM(NDAYA,NLC)+TSCDA*DZC(K)*
     &                  DXYP(L)*HP(L)*(CSERT(K,NSID,1)-SAL(L,K))
                    FVOLASM(NDAYA,NLC)=FVOLASM(NDAYA,NLC)+TSCDA*DZC(K)*
     &                  DXYP(L)*HP(L)*(1.0-( CSERT(K,NSID,1)/SAL(L,K) ))
                    SAL(L,K)=TSCDA*CSERT(K,NSID,1)+(1.-TSCDA)*SAL(L,K)
                  ENDIF
                ENDIF
              ENDIF
              IF(IWASM.EQ.1) WRITE(1,1111)N,NLC,ICDA(NLC),JCDA(NLC),NS,
     &            CSERT(K,NS,1),SAL(L,K)
              IF(ITPCDA(NLC).EQ.1)THEN
                LDATA=LIJ(ICCDA(NLC),JCCDA(NLC))
                SAL(L,K)=TSCDA*SAL(LDATA,K)+(1.-TSCDA)*SAL(L,K)
              ENDIF
              SALASM=SALASM+HP(L)*DXYP(L)*(SAL(L,K)-CONASMOLD)*DZC(K)
            ENDDO
          ENDDO
        ENDIF
C
        IF(ISCDA(2).GT.0)THEN
          DO K=1,KC
            DO NLC=1,NLCDA
              L=LIJ(ICDA(NLC),JCDA(NLC))
              CONASMOLD=TEM(L,K)
              NSID=NCSERA(NLC,2)
              IF(IWASM.EQ.1) WRITE(1,1112)N,NLC,ICDA(NLC),JCDA(NLC),NS,
     &            CSERT(K,NS,2),TEM(L,K)
              IF(ITPCDA(NLC).EQ.0)THEN
                IF(NSID.GT.0)THEN
                  IF(CSERT(K,NSID,2).GT.0)THEN
                    FTEMASM(NDAYA,NLC)=FTEMASM(NDAYA,NLC)+TSCDA*DZC(K)*
     &                  DXYP(L)*HP(L)*(CSERT(K,NSID,2)-TEM(L,K))
                    TEM(L,K)=TSCDA*CSERT(K,NSID,2)+(1.-TSCDA)*TEM(L,K)
                  ENDIF
                ENDIF
              ENDIF
              IF(IWASM.EQ.1) WRITE(1,1112)N,NLC,ICDA(NLC),JCDA(NLC),NS,
     &            CSERT(K,NSID,2),TEM(L,K)
              IF(ITPCDA(NLC).EQ.1)THEN
                LDATA=LIJ(ICCDA(NLC),JCCDA(NLC))
                TEM(L,K)=TSCDA*TEM(LDATA,K)+(1.-TSCDA)*TEM(L,K)
              ENDIF
              TEMASM=TEMASM+HP(L)*DXYP(L)*(TEM(L,K)-CONASMOLD)*DZC(K)
            ENDDO
          ENDDO
        ENDIF
C
        IF(ISCDA(3).GT.0)THEN
          DO K=1,KC
            DO NLC=1,NLCDA
              L=LIJ(ICDA(NLC),JCDA(NLC))
              CONASMOLD=DYE(L,K)
              NSID=NCSERA(NLC,3)
              IF(ITPCDA(NLC).EQ.0)THEN
                IF(NS.GT.0)THEN
                  IF(CSERT(K,NSID,3).GT.0)THEN
                    DYE(L,K)=TSCDA*CSERT(K,NSID,3)+(1.-TSCDA)*DYE(L,K)
                  ENDIF
                ENDIF
              ENDIF
              IF(ITPCDA(NLC).EQ.1)THEN
                LDATA=LIJ(ICCDA(NLC),JCCDA(NLC))
                DYE(L,K)=TSCDA*DYE(LDATA,K)+(1.-TSCDA)*DYE(L,K)
              ENDIF
              DYEASM=DYEASM+HP(L)*DXYP(L)*(DYE(L,K)-CONASMOLD)*DZC(K)
            ENDDO
          ENDDO
        ENDIF
C
        IF(ISCDA(4).GT.0)THEN
          DO K=1,KC
            DO NLC=1,NLCDA
              L=LIJ(ICDA(NLC),JCDA(NLC))
              CONASMOLD=SFL(L,K)
              NSID=NCSERA(NLC,4)
              IF(ITPCDA(NLC).EQ.0)THEN
                IF(NSID.GT.0)THEN
                  IF(CSERT(K,NSID,4).GT.0)THEN
                    SFL(L,K)=TSCDA*CSERT(K,NSID,4)+(1.-TSCDA)*SFL(L,K)
                  ENDIF
                ENDIF
              ENDIF
              IF(ITPCDA(NLC).EQ.1)THEN
                LDATA=LIJ(ICCDA(NLC),JCCDA(NLC))
                SFL(L,K)=TSCDA*SFL(LDATA,K)+(1.-TSCDA)*SFL(L,K)
              ENDIF
              SFLASM=SFLASM+HP(L)*DXYP(L)*(SFL(L,K)-CONASMOLD)*DZC(K)
            ENDDO
          ENDDO
        ENDIF
C
        IF(ISCDA(5).GT.0)THEN
          DO NT=1,NTOX
            M=MSVTOX(NT)
            DO K=1,KC
              DO NLC=1,NLCDA
                L=LIJ(ICDA(NLC),JCDA(NLC))
                CONASMOLD=TOX(L,K,NT)
                NSID=NCSERA(NLC,M)
                IF(ITPCDA(NLC).EQ.0)THEN
                  IF(NSID.GT.0)THEN
                    IF(CSERT(K,NSID,M).GT.0)THEN
                      TOX(L,K,NT)=TSCDA*CSERT(K,NSID,M)+(1.-TSCDA)*
     &                    TOX(L,K,NT)
                    ENDIF
                  ENDIF
                ENDIF
                IF(ITPCDA(NLC).EQ.1)THEN
                  LDATA=LIJ(ICCDA(NLC),JCCDA(NLC))
                TOX(L,K,NT)=TSCDA*TOX(LDATA,K,NT)+(1.-TSCDA)*TOX(L,K,NT)
                ENDIF
                TOXASM(NT)=TOXASM(NT)
     &              +HP(L)*DXYP(L)*(TOX(L,K,NT)-CONASMOLD)*DZC(K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
C
        IF(ISCDA(6).GT.0)THEN
          DO NS=1,NSED
            M=MSVSED(NS)
            DO K=1,KC
              DO NLC=1,NLCDA
                L=LIJ(ICDA(NLC),JCDA(NLC))
                CONASMOLD=SED(L,K,NS)
                NSID=NCSERA(NLC,M)
                IF(ITPCDA(NLC).EQ.0)THEN
                  IF(NSID.GT.0)THEN
                    IF(CSERT(K,NSID,M).GT.0)THEN
                      SED(L,K,NS)=TSCDA*CSERT(K,NSID,M)+(1.-TSCDA)*
     &                    SED(L,K,NS)
                    ENDIF
                  ENDIF
                ENDIF
                IF(ITPCDA(NLC).EQ.1)THEN
                  LDATA=LIJ(ICCDA(NLC),JCCDA(NLC))
                SED(L,K,NS)=TSCDA*SED(LDATA,K,NS)+(1.-TSCDA)*SED(L,K,NS)
                ENDIF
                SEDASM(NS)=SEDASM(NS)
     &              +HP(L)*DXYP(L)*(SED(L,K,NS)-CONASMOLD)*DZC(K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
C
C6222 FORMAT(' TC,SNEW,SASSM,SOLD='4F10.2)
C
        IF(ISCDA(7).GT.0)THEN
          DO NX=1,NSND
            M=MSVSND(NX)
            DO K=1,KC
              DO NLC=1,NLCDA
                L=LIJ(ICDA(NLC),JCDA(NLC))
                CONASMOLD=SND(L,K,NX)
                NSID=NCSERA(NLC,M)
                IF(ITPCDA(NLC).EQ.0)THEN
                  IF(NSID.GT.0)THEN
                    IF(CSERT(K,NSID,M).GT.0)THEN
                      SND(L,K,NX)=TSCDA*CSERT(K,NSID,M)+(1.-TSCDA)*
     &                    SND(L,K,NX)
                    ENDIF
                  ENDIF
                ENDIF
                IF(ITPCDA(NLC).EQ.1)THEN
                  LDATA=LIJ(ICCDA(NLC),JCCDA(NLC))
                SND(L,K,NX)=TSCDA*SND(LDATA,K,NX)+(1.-TSCDA)*SND(L,K,NX)
                ENDIF
                SNDASM(NX)=SNDASM(NX)
     &              +HP(L)*DXYP(L)*(SND(L,K,NX)-CONASMOLD)*DZC(K)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
C
        IF(IWASM.EQ.1.AND.MYRANK.EQ.0)THEN
          CLOSE(1)
        ENDIF
C
        IF(IS2TIM.GE.1) THEN
          IF(ISBAL.GE.1)THEN
            SALOUT=SALOUT-SALASM
            DYEOUT=DYEOUT-DYEASM
            DO NT=1,NTOX
              TOXOUT2T(NT)=TOXOUT2T(NT)-TOXASM(NT)
            ENDDO
            DO NS=1,NSED
              SEDOUT2T(NS)=SEDOUT2T(NS)-SEDASM(NS)
            ENDDO
            DO NS=1,NSND
              SNDOUT2T(NS)=SNDOUT2T(NS)-SNDASM(NS)
            ENDDO
          ENDIF
        ENDIF
C
      ENDIF
      MPI_WTIMES(642)=MPI_WTIMES(642)+MPI_TOC(S1TIME)
C
 1111 FORMAT(' SAL '5I5,2F10.3)
 1112 FORMAT(' TEM '5I5,2F10.3)
 1212 FORMAT(' N,NDAYA = ',2I12)
C
C **  SURFACE AND INTERNAL HEAT SOURCE-SINK CALCULATION
C **  DYE DECAY CALCULATION
C **  BOTTOM AND INTERNAL SEDIMENT SOURCE-SINK CALCULATION
C
      RETURN
      END
