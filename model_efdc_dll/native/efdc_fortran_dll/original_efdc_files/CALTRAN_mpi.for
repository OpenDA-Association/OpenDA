      SUBROUTINE CALTRAN_mpi (ISTL_,IS2TL_,MVAR,MO,CON,CON1)
C
C CHANGE RECORD
C  ADDED TRANSPORT BYPASS MASK, IMASKDRY FOR DRY CELLS
C **  SUBROUTINE CALTRAN CALCULATES THE ADVECTIVE
C **  TRANSPORT OF DISSOLVED OR SUSPENDED CONSITITUENT M LEADING TO
C **  A NEW VALUE AT TIME LEVEL (N+1). THE VALUE OF ISTL INDICATES
C **  THE NUMBER OF TIME LEVELS IN THE STEP
C
      USE GLOBAL
      USE MPI

      DIMENSION CON(LCM,KCM),CON1(LCM,KCM)

      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::CONTMN
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::CONTMX
!      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::FQCPAD
!      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::QSUMNAD
!      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::QSUMPAD
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::POS

      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::WQBCCON
      REAL CTMP
      CTMP=0.0

      IF(.NOT.ALLOCATED(CONTMN))THEN
        ALLOCATE(CONTMN(LCM,KCM))
        ALLOCATE(CONTMX(LCM,KCM))
        ALLOCATE(FQCPAD(LCM,KCM))
        ALLOCATE(QSUMNAD(LCM,KCM))
        ALLOCATE(QSUMPAD(LCM,KCM))
        ALLOCATE(POS(LCM,KCM))
        ALLOCATE(WQBCCON(LCM,KCM))

        DO L=1,LC
          FWU(L,0)=0.
          FWU(L,KC)=0.
        ENDDO
        CONTMN=0.0
        CONTMX=0.0
        FQCPAD=0.0
        QSUMNAD=0.0  ! *** NOT USED
        QSUMPAD=0.0
        POS=0.0
        WQBCCON=0.0
      ENDIF
C
      BSMALL=1.0E-6
      ISUD=1
      IF(ISDYNSTP.EQ.0)THEN
        DELT=DT2
        DELTA=DT2
        IF(ISCDCA(MVAR).EQ.2) DELTA=DT
        DELTD2=DT
        IF(ISTL_.NE.3)THEN
          DELT=DT
          DELTA=DT
          DELTD2=0.5*DT
          IF(IS2TIM.EQ.0)ISUD=0  ! *** PMC SINGLE LINE CHANGE
        ENDIF
      ELSE
        DELT=DTDYN
        DELTA=DTDYN
        DELTD2=0.5*DTDYN
      END IF
      DELTA4=0.25*DELTA

      S3TIME=MPI_TIC()
      ! *** DSLLC BEGIN
      M=MO
      IF(IS2TL_.EQ.1)THEN
        ISUD=1
        IF(MVAR.NE.8)THEN ! *** ARRAYS
          DO K=1,KC
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              CON1(L,K)=CON(L,K)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
      MPI_WTIMES(650)=MPI_WTIMES(650)+MPI_TOC(S3TIME)

      S3TIME=MPI_TIC()
      ! *** SAVE OLD WQ CONCENTRATIONS FOR OPEN BOUNDARY CELLS
      IF(MVAR.EQ.8)THEN
        DO IOBC=1,NBCSOP
          L=LOBCS(IOBC)
          DO K=1,KC
            WQBCCON(L,K)=CON(L,K)
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(651)=MPI_WTIMES(651)+MPI_TOC(S3TIME)

      ! *** DSLLC END
C
C **  CALCULATED EXTERNAL SOURCES AND SINKS
C
      S3TIME=MPI_TIC()
      CALL CALFQC_mpi (ISTL_,IS2TL_,MVAR,M,CON,CON1)!,
!     & FQCPAD,QSUMPAD,QSUMNAD)
      MPI_WTIMES(652)=MPI_WTIMES(652)+MPI_TOC(S3TIME)
C
C
C **  SELECT TRANSPORT OPTION, ISPLIT=1 FOR HORIZONTAL-VERTICAL
C **  OPERATOR SPLITTING
C **  BEGIN COMBINED ADVECTION SCHEME
C **  ADVECTIVE FLUX CALCULATION
C
      IF(ISTL_.EQ.2) GOTO 300
      IF(ISCDCA(MVAR).EQ.0) GOTO 300
      IF(ISCDCA(MVAR).EQ.1) GOTO 400
      IF(ISCDCA(MVAR).EQ.2) GOTO 350
C
C **  CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE WITH ADVECTION
C **  AVERAGED BETWEEN (N) AND (N+1) OR (N-1) AND (N+1) AND ADVECTED
C **  AT (N) OR (N-1) IF ISTL EQUALS 2 OR 3 RESPECTIVELY
C
  300 CONTINUE
C
      S3TIME=MPI_TIC()
      CALL broadcast_boundary_array(CON1,ic)
      MPI_WTIMES(692)=MPI_WTIMES(692)+MPI_TOC(S3TIME)
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FUHU)
        call collect_in_zero_array(FVHU)
        call collect_in_zero_array(FWU )
        call collect_in_zero_array(UHDY2 )
        call collect_in_zero_array(VHDX2 )
        call collect_in_zero_array(CON1 )
        call collect_in_zero_array(W2 )
        IF(MYRANK.EQ.0) PRINT*, '0FUHU = ', sum(abs(dble(FUHU)))
        IF(MYRANK.EQ.0) PRINT*, '0FVHU = ', sum(abs(dble(FVHU)))
        IF(MYRANK.EQ.0) PRINT*, '0FWU  = ', sum(abs(dble(FWU )))
        IF(MYRANK.EQ.0) PRINT*, '0UHDY2  = ', sum(abs(dble(UHDY2 )))
        IF(MYRANK.EQ.0) PRINT*, '0VHDX2  = ', sum(abs(dble(VHDX2 )))
        IF(MYRANK.EQ.0) PRINT*, '0CON1  = ', sum(abs(dble(CON1 )))
        IF(MYRANK.EQ.0) PRINT*, '0W2  = ', sum(abs(dble(W2 )))
      endif
      call mpi_barrier(mpi_comm_world,ierr)

      S3TIME=MPI_TIC()
      IF(IDRYTBP.EQ.0)THEN
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
              FUHU(L,K)=UHDY2(L,K)*CON1(LUPU(L,K),K)
              FVHU(L,K)=VHDX2(L,K)*CON1(LUPV(L,K),K)
          ENDDO
        ENDDO
        IF(KC.GT.1)THEN
          DO K=1,KS
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              FWU(L,K)=W2(L,K)*CON1(L,KUPW(L,K))
            ENDDO
          ENDDO
        ENDIF
      ELSE
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              FUHU(L,K)=UHDY2(L,K)*CON1(LUPU(L,K),K)
              FVHU(L,K)=VHDX2(L,K)*CON1(LUPV(L,K),K)
            ELSE
              FUHU(L,K)=0.
              FVHU(L,K)=0.
            ENDIF
          ENDDO
        ENDDO
        IF(KC.GT.1)THEN
          DO K=1,KS
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              IF(LMASKDRY(L))THEN
                FWU(L,K)=W2(L,K)*CON1(L,KUPW(L,K))
              ELSE
                FWU(L,K)=0.
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
      MPI_WTIMES(653)=MPI_WTIMES(653)+MPI_TOC(S3TIME)
C
      call mpi_barrier(mpi_comm_world,ierr)
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FUHU)
        call collect_in_zero_array(FVHU)
        call collect_in_zero_array(FWU )
        IF(MYRANK.EQ.0) PRINT*, '1FUHU = ', sum(abs(dble(FUHU)))
        IF(MYRANK.EQ.0) PRINT*, '1FVHU = ', sum(abs(dble(FVHU)))
        IF(MYRANK.EQ.0) PRINT*, '1FWU  = ', sum(abs(dble(FWU )))
        if(n.eq.12.and.myrank.eq.0)then
           do l=2,lcm ; do k=1,kcm
           print*, l,k,fvhu(l,k)
           enddo ; enddo
        endif
      endif

      GOTO 500
C
C **  CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE WITH ADVECTION
C **  AVERAGED BETWEEN  (N-1) AND (N+1) AND ADVECTED FIELD AVERAGED
C **  BETWEEN AT (N-1) AND (N) IF ISTL 3 ONLY
C
  350 CONTINUE
C
      S3TIME=MPI_TIC()
      CALL broadcast_boundary_array(CONT,ic)
      MPI_WTIMES(692)=MPI_WTIMES(692)+MPI_TOC(S3TIME)

      S3TIME=MPI_TIC()
      DO K=1,KC
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          CONT(L,K)=0.5*(CON(L,K)+CON1(L,K))
     &        +DELT*0.5*FQC(L,K)*DXYIP(L)/H2P(L)
        ENDDO
      ENDDO
      DO K=1,KC
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          FUHU(L,K)=UHDY2(L,K)*CONT(LUPU(L,K),K)
          FVHU(L,K)=VHDX2(L,K)*CONT(LUPV(L,K),K)
        ENDDO
      ENDDO
      IF(KC.GT.1)THEN
        DO K=1,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            FWU(L,K)=W2(L,K)*CONT(L,KUPW(L,K))
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(654)=MPI_WTIMES(654)+MPI_TOC(S3TIME)
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FUHU)
        call collect_in_zero_array(FVHU)
        call collect_in_zero_array(FWU )
        IF(MYRANK.EQ.0) PRINT*, '2FUHU = ', sum(abs(dble(FUHU)))
        IF(MYRANK.EQ.0) PRINT*, '2FVHU = ', sum(abs(dble(FVHU)))
        IF(MYRANK.EQ.0) PRINT*, '2FWU  = ', sum(abs(dble(FWU )))
      endif
      GOTO 500
C
C **  CALCULATE ADVECTIVE FLUXES BY CENTRAL DIFFERENCE WITH TRANSPORT
C **  AVERAGED BETWEEN (N+1) AND (N-1) AND TRANSPORTED FIELD AT (N)
C
  400 CONTINUE
C
      S3TIME=MPI_TIC()
      CALL broadcast_boundary_array(CON,ic)
      MPI_WTIMES(692)=MPI_WTIMES(692)+MPI_TOC(S3TIME)
C
      S3TIME=MPI_TIC()
      DO K=1,KC
!$OMP PARALLEL DO PRIVATE(LS)
        DO L=LMPI2,LMPILA
          LS=LSC(L)
          FUHU(L,K)=0.5*UHDY2(L,K)*(CON(L,K)+CON(L-1,K))
          FVHU(L,K)=0.5*VHDX2(L,K)*(CON(L,K)+CON(LS,K))
        ENDDO
      ENDDO
      MPI_WTIMES(655)=MPI_WTIMES(655)+MPI_TOC(S3TIME)
C
      S3TIME=MPI_TIC()
      CALL broadcast_boundary_array(VHDX2,ic)
      CALL broadcast_boundary_array(UHDY2,ic)
      MPI_WTIMES(693)=MPI_WTIMES(693)+MPI_TOC(S3TIME)
C
      S3TIME=MPI_TIC()
      DO K=1,KC
        DO LL=1,NCBS
          L=LCBS(LL)
          LN=LNC(L)
          IF(VHDX2(LN,K).LT.0.) FVHU(LN,K)=VHDX2(LN,K)*CON1(LN,K)
        ENDDO
        DO LL=1,NCBW
          L=LCBW(LL)
          IF(UHDY2(L+1,K).LT.0.) FUHU(L+1,K)=UHDY2(L+1,K)*CON1(L+1,K)
        ENDDO
        DO LL=1,NCBE
          L=LCBE(LL)
          IF(UHDY2(L,K).GT.0.) FUHU(L,K)=UHDY2(L,K)*CON1(L-1,K)
        ENDDO
        DO LL=1,NCBN
          L=LCBN(LL)
          LS =LSC(L)
          IF(VHDX2(L,K).GT.0.) FVHU(L,K)=VHDX2(L,K)*CON1(LS,K)
        ENDDO
      ENDDO
      MPI_WTIMES(656)=MPI_WTIMES(656)+MPI_TOC(S3TIME)
C
      S3TIME=MPI_TIC()
      DO K=1,KS
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          FWU(L,K)=0.5*W2(L,K)*(CON(L,K+1)+CON(L,K))
        ENDDO
      ENDDO
      MPI_WTIMES(657)=MPI_WTIMES(657)+MPI_TOC(S3TIME)
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FUHU)
        call collect_in_zero_array(FVHU)
        call collect_in_zero_array(FWU )
        IF(MYRANK.EQ.0) PRINT*, '3FUHU = ', sum(abs(dble(FUHU)))
        IF(MYRANK.EQ.0) PRINT*, '3FVHU = ', sum(abs(dble(FVHU)))
        IF(MYRANK.EQ.0) PRINT*, '3FWU  = ', sum(abs(dble(FWU )))
      endif
C **  STANDARD ADVECTION CALCULATION
C
  500 CONTINUE
C
C BEGIN IF ON TRANSPORT OPTION CHOICE
C
      ! *** CALCULATE AND ADD HORIZONTAL DIFFUSION FLUX (PMC MOVED)
      S3TIME=MPI_TIC()
      IF(ISHDMF.EQ.2) CALL CALDIFF_mpi (ISTL_,M,CON1)
      MPI_WTIMES(658)=MPI_WTIMES(658)+MPI_TOC(S3TIME)
C
      S3TIME=MPI_TIC()
      CALL broadcast_boundary_array(FUHU,ic)
      CALL broadcast_boundary_array(FVHU,ic)
      CALL broadcast_boundary_array(FWU,ic)
      MPI_WTIMES(694)=MPI_WTIMES(694)+MPI_TOC(S3TIME)
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FUHU)
        call collect_in_zero_array(FVHU)
        call collect_in_zero_array(FWU )
        call collect_in_zero_array(CON1 )
        call collect_in_zero_array(FQC )
        call collect_in_zero(H1P )
        call COLLECT_IN_ZERO_INT(IMASKDRY)
        IF(MYRANK.EQ.0) PRINT*, 'FUHU = ', sum(abs(dble(FUHU)))
        IF(MYRANK.EQ.0) PRINT*, 'FVHU = ', sum(abs(dble(FVHU)))
        IF(MYRANK.EQ.0) PRINT*, 'FWU  = ', sum(abs(dble(FWU )))
        IF(MYRANK.EQ.0) PRINT*, 'CON1  = ', sum(abs(dble(CON1 )))
        IF(MYRANK.EQ.0) PRINT*, 'FQC  = ', sum(abs(dble(FQC )))
        IF(MYRANK.EQ.0) PRINT*, 'H1P  = ', sum(abs(dble(H1P )))
        IF(MYRANK.EQ.0) PRINT*, 'IMASKDRY= ',sum(abs(dble(IMASKDRY )))
      endif

      ! *** IF ISACAC EQ 0 INCLUDE FQC MASS SOURCES IN UPDATE
      IF(ISCDCA(MVAR).EQ.0)THEN
        IF(ISTL_.EQ.2)THEN
C
          S3TIME=MPI_TIC()
          IF(IDRYTBP.EQ.0)THEN
            DO K=1,KC
              RDZIC=DZIC(K)
!$OMP PARALLEL DO
              DO L=LMPI2,LMPILA
                CH(L,K)=CON1(L,K)*H1P(L)
     &              +DELT*( ( RDZIC*FQC(L,K)
     &              +FUHU(L,K)-FUHU(L+1,K)
     &              +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &              +(FWU(L,K-1)-FWU(L,K))*RDZIC )
             ENDDO
            ENDDO
          ELSE
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(CH)
        IF(MYRANK.EQ.0) PRINT*, 'cc','CH = ', sum(abs(dble(CH)))
      endif
            DO K=1,KC
              RDZIC=DZIC(K)
!$OMP PARALLEL DO
              DO L=LMPI2,LMPILA
                IF(IMASKDRY(L).EQ.0)
     &              CH(L,K)=CON1(L,K)*H1P(L)
     &              +DELT*( ( RDZIC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)
     &              +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &              +(FWU(L,K-1)-FWU(L,K))*RDZIC )
                IF(IMASKDRY(L).EQ.1)
     &              CH(L,K)=CON1(L,K)*H1P(L)
     &              +DELT*( ( FQC(L,K) )*DXYIP(L) )
                IF(IMASKDRY(L).EQ.2)
     &              CH(L,K)=CON1(L,K)*H1P(L)
              ENDDO
            ENDDO
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(CH)
        IF(MYRANK.EQ.0) PRINT*, 'c0','CH = ', sum(abs(dble(CH)))
      endif
          ENDIF
          IF(ISFCT(MVAR).GE.1.AND.ISADAC(MVAR).GT.0)THEN  ! *** DSLLC SINGLE LINE
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI2,LMPILA
                CON2(L,K)=CON1(L,K)
              ENDDO
            ENDDO
          ENDIF
          MPI_WTIMES(659)=MPI_WTIMES(659)+MPI_TOC(S3TIME)
C
C ELSE ON TIME LEVEL CHOICE FOR ISCDCA=0
C
        ELSE
C
          S3TIME=MPI_TIC()
          DO K=1,KC
            RDZIC=DZIC(K)
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              CH(L,K)=CON1(L,K)*H2P(L)
     &            +DELT*( ( RDZIC*FQC(L,K)
     &            +FUHU(L,K)-FUHU(L+1,K)
     &            +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &            +(FWU(L,K-1)-FWU(L,K))*RDZIC )
            ENDDO
          ENDDO
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(CH)
        IF(MYRANK.EQ.0) PRINT*, 'c1','CH = ', sum(abs(dble(CH)))
      endif
          IF(ISFCT(MVAR).GE.1.AND.ISADAC(MVAR).GT.0)THEN  ! *** DSLLC SINGLE LINE
            DO K=1,KC
!$OMP PARALLEL DO
              DO L=LMPI2,LMPILA
                CON2(L,K)=CON(L,K)
              ENDDO
            ENDDO
          ENDIF
          MPI_WTIMES(660)=MPI_WTIMES(660)+MPI_TOC(S3TIME)
C
        ENDIF
C
        if(PRINT_SUM)THEN
        DO NW=0,NWQV
          call collect_in_zero_array(WQV(:,:,NW))
        ENDDO
        IF(MYRANK.EQ.0)THEN
          IF(MO.LE.NWQV)PRINT*, n,'h11WQV  = ', sum(abs(dble(WQV))),mo
        ENDIF
        ENDIF
C ENDIF ON TIME LEVEL CHOICE FOR ISCDCA=0
C
        IF(ISUD.EQ.1.AND.IS2TL_.EQ.0.AND.MVAR.NE.8)THEN
          S3TIME=MPI_TIC()
          DO K=1,KC
!$OMP PARALLEL DO PRIVATE(L)
            DO IOBC=1,NBCSOP
              L=LOBCS(IOBC)
              IF(ISDOMAIN(L))THEN
                 CON(L,K)=CON1(L,K)
              ENDIF
            ENDDO
          ENDDO
          MPI_WTIMES(661)=MPI_WTIMES(661)+MPI_TOC(S3TIME)
          S3TIME=MPI_TIC()
          DO K=1,KC
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              CON1(L,K)=CON(L,K)
            ENDDO
          ENDDO
          MPI_WTIMES(662)=MPI_WTIMES(662)+MPI_TOC(S3TIME)
        ENDIF
        ! *** UPDATE NEW CONCENTRATIONS
        S3TIME=MPI_TIC()
C        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            CON(L,1:KC)=CH(L,1:KC)*HPI(L)
          ENDDO
C        ENDDO
        MPI_WTIMES(663)=MPI_WTIMES(663)+MPI_TOC(S3TIME)
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
         call collect_in_zero_array(CON)
         call collect_in_zero(HPI)
         IF(MYRANK.EQ.0) PRINT*, 'd1','CON = ', sum(abs(dble(CON)))
         IF(MYRANK.EQ.0) PRINT*, 'e1','HPI = ', sum(abs(dble(HPI)))
      endif
C *** ELSE ON TRANSPORT OPTION CHOICE
C *** IF ISACAC NE 0 DO NOT INCLUDE FQC MASS SOURCES IN UPDATE
C
      ELSE
C
C BEGIN IF ON TIME LEVEL CHOICE FOR ISCDCA.NE.0
C
        IF(ISTL_.EQ.2)THEN
C
          S3TIME=MPI_TIC()
          IF(IDRYTBP.EQ.0)THEN
            DO K=1,KC
              RDZIC=DZIC(K)
!$OMP PARALLEL DO
              DO L=LMPI2,LMPILA
                CH(L,K)=CON1(L,K)*H1P(L)
     &              +DELT*( ( RDZIC*FQC(L,K)
     &              +FUHU(L,K)-FUHU(L+1,K)
     &              +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &              +(FWU(L,K-1)-FWU(L,K))*RDZIC )
              ENDDO
            ENDDO
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(CH)
        IF(MYRANK.EQ.0) PRINT*, 'c2','CH = ', sum(abs(dble(CH)))
      endif
          ELSE
            DO K=1,KC
              RDZIC=DZIC(K)
!$OMP PARALLEL DO
              DO L=LMPI2,LMPILA
                IF(IMASKDRY(L).EQ.0)
     &              CH(L,K)=CON1(L,K)*H1P(L)
     &              +DELT*( ( RDZIC*FQC(L,K)
     &              +FUHU(L,K)-FUHU(L+1,K)
     &              +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &              +(FWU(L,K-1)-FWU(L,K))*RDZIC )
                IF(IMASKDRY(L).EQ.1)
     &              CH(L,K)=CON1(L,K)*H1P(L)
     &              +DELT*( ( FQC(L,K) )*DXYIP(L) )
                IF(IMASKDRY(L).EQ.2)
     &              CH(L,K)=CON1(L,K)*H1P(L)
              ENDDO
            ENDDO
          ENDIF
          IF(ISFCT(MVAR).GE.1)THEN
            CON2=CON1   ! *** ARRAYS
          ENDIF
          MPI_WTIMES(664)=MPI_WTIMES(664)+MPI_TOC(S3TIME)
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(CH)
        IF(MYRANK.EQ.0) PRINT*, 'c3','CH = ', sum(abs(dble(CH)))
      endif
C ELSE ON TIME LEVEL CHOICE FOR ISCDCA.NE.0 AND ISTL.EQ.3
C
        ELSE
C
          S3TIME=MPI_TIC()
          DO K=1,KC
            RDZIC=DZIC(K)
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              CH(L,K)=CON1(L,K)*H2P(L)
     &            +DELT*( ( RDZIC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)
     &            +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &            +(FWU(L,K-1)-FWU(L,K))*RDZIC )
            ENDDO
          ENDDO
          IF(ISFCT(MVAR).GE.1)THEN
            CON2=CON    ! *** ARRAYS
          ENDIF
          MPI_WTIMES(664)=MPI_WTIMES(664)+MPI_TOC(S3TIME)
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(CH)
        IF(MYRANK.EQ.0) PRINT*, 'c4','CH = ', sum(abs(dble(CH)))
      endif
        ENDIF
C
C ENDIF ON TIME LEVEL CHOICE FOR ISCDCA.NE.0
C
        S3TIME=MPI_TIC()
        IF(ISUD.EQ.1.AND.MVAR.NE.8)THEN
          DO K=1,KC
            DO IOBC=1,NBCSOP
              L=LOBCS(IOBC)
              CON(L,K)=CON1(L,K)
            ENDDO
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              CON1(L,K)=CON(L,K)
            ENDDO
          ENDDO
        ENDIF
        ! *** PMC-BOUNDARY CONDITIONS APPLIED BELOW
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            CON(L,K)=CH(L,K)*HPI(L)
          ENDDO
        ENDDO
        MPI_WTIMES(664)=MPI_WTIMES(664)+MPI_TOC(S3TIME)
      if(PRINT_SUM.AND.MVAR.eq.2)then
         call collect_in_zero_array(CON)
         call collect_in_zero(HPI)
         IF(MYRANK.EQ.0) PRINT*, 'd2','CON = ', sum(abs(dble(CON)))
         IF(MYRANK.EQ.0) PRINT*, 'e2','HPI = ', sum(abs(dble(HPI)))
      endif
C
      ENDIF
C
C ENDIF ON TRANSPORT OPTION CHOICE
C
C *** APPLY OPEN BOUNDARY CONDITIONS, BASED ON DIRECTION OF FLOW
C
      ! *** ALL OTHER WATER CONSTITUENTS
      S3TIME=MPI_TIC()
      IF(MVAR.EQ.8)THEN   !.AND.IWQPSL.EQ.2)THEN
        M=4+NTOX+NSED+NSND+MO
        ! *** RESTORE ORIGINAL CONCENTRATIONS PRIOR TO APPLYING OPEN BC'S
        DO K=1,KC
          DO IOBC=1,NBCSOP
            L=LOBCS(IOBC)
            CON1(L,K)=WQBCCON(L,K)
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(665)=MPI_WTIMES(665)+MPI_TOC(S3TIME)
C
      ! *** SOUTH OPEN BC
      S3TIME=MPI_TIC()
      DO K=1,KC
        DO LL=1,NCBS
          IF(ISDOMAIN(LCBS(LL)))THEN
          NSID=NCSERS(LL,M)
          L=LCBS(LL)
          LN=LNC(L)
          IF(VHDX2(LN,K).LE.0.)THEN
            ! *** FLOWING OUT OF DOMAIN
            IF(ISTL_.EQ.2)THEN
              CTMP=CON1(L,K)+DELT*(VHDX2(LN,K)*CON1(L,K)
     &            -FVHU(LN,K))*DXYIP(L)*HPI(L)
            ELSE
              IF(ISCDCA(MVAR).NE.2)CTMP=CON1(L,K)+DELT*(VHDX2(LN,K)
     &            *CON1(L,K)-FVHU(LN,K))*DXYIP(L)*HPI(L)
              IF(ISCDCA(MVAR).EQ.2) CTMP=0.5*(CON1(L,K)+CON(L,K))
     &            +0.5*(CON1(L,K)-CON(L,K))*H2P(L)*HPI(L)
     &            +DELT*(0.5*VHDX2(LN,K)*(CON1(L,K)+CON(L,K))
     &            -FVHU(LN,K))*DXYIP(L)*HPI(L)
              CON1(L,K)=CON(L,K)
            ENDIF
            CON(L,K)=CTMP
            CBSTMP=CBS(LL,1,M)+CSERT(1,NSID,M)
            IF(M.EQ.1.AND.CON(L,K).GT.CBSTMP)THEN
              CON(L,K)=CBSTMP
            ENDIF
            CLOS(LL,K,M)=CON(L,K)
            NLOS(LL,K,M)=N
          ELSE
            ! *** FLOWING INTO DOMAIN
            IF(ISUD.EQ.1) CON1(L,K)=CON(L,K)
            CBT=WTCI(K,1)*CBS(LL,1,M)+WTCI(K,2)*CBS(LL,2,M)+CSERT(
     &          K,NSID,M)
            NMNLO=N-NLOS(LL,K,M)
            IF(NMNLO.GE.NTSCRS(LL))THEN
              CON(L,K)=CBT
            ELSE
              CON(L,K)=CLOS(LL,K,M)
     &            +(CBT-CLOS(LL,K,M))*FLOAT(NMNLO)/FLOAT(NTSCRS(LL))
            ENDIF
          ENDIF
          ENDIF
        ENDDO
      ENDDO
      MPI_WTIMES(666)=MPI_WTIMES(666)+MPI_TOC(S3TIME)
C
      ! *** WEST OPEN BC
      S3TIME=MPI_TIC()
      DO K=1,KC
        DO LL=1,NCBW
          IF(ISDOMAIN(LCBW(LL)))THEN
          NSID=NCSERW(LL,M)
          L=LCBW(LL)
          IF(UHDY2(L+1,K).LE.0.)THEN
            ! *** FLOWING OUT OF DOMAIN
            IF(ISTL_.EQ.2)THEN
              CTMP=CON1(L,K)+DELT*(UHDY2(L+1,K)*CON1(L,K)
     &            -FUHU(L+1,K))*DXYIP(L)*HPI(L)
            ELSE
              IF(ISCDCA(MVAR).NE.2) CTMP=CON1(L,K)
     &            +DELT*(UHDY2(L+1,K)*CON1(L,K)-FUHU(L+1,K))*DXYIP(L)
     &            *HPI(L)
              IF(ISCDCA(MVAR).EQ.2) CTMP=0.5*(CON1(L,K)+CON(L,K))
     &            +0.5*(CON1(L,K)-CON(L,K))*H2P(L)*HPI(L)
     &            +DELT*(0.5*UHDY2(L+1,K)*(CON1(L,K)+CON(L,K))
     &            -FUHU(L+1,K))*DXYIP(L)*HPI(L)
              CON1(L,K)=CON(L,K)
            ENDIF
            CON(L,K)=CTMP
            CBWTMP=CBW(LL,1,M)+CSERT(1,NSID,M)
            IF(M.EQ.1.AND.CON(L,K).GT.CBWTMP) CON(L,K)=CBWTMP
            CLOW(LL,K,M)=CON(L,K)
            NLOW(LL,K,M)=N
          ELSE
            ! *** FLOWING INTO DOMAIN
            IF(ISUD.EQ.1) CON1(L,K)=CON(L,K)
            CBT=WTCI(K,1)*CBW(LL,1,M)+WTCI(K,2)*CBW(LL,2,M)+CSERT(
     &          K,NSID,M)
            NMNLO=N-NLOW(LL,K,M)
            IF(NMNLO.GE.NTSCRW(LL))THEN
              CON(L,K)=CBT
            ELSE
              CON(L,K)=CLOW(LL,K,M)
     &            +(CBT-CLOW(LL,K,M))*FLOAT(NMNLO)/FLOAT(NTSCRW(LL))
            ENDIF
          ENDIF
          ENDIF
        ENDDO
      ENDDO
      MPI_WTIMES(667)=MPI_WTIMES(667)+MPI_TOC(S3TIME)
C
      ! *** EAST OPEN BC
      S3TIME=MPI_TIC()
      DO K=1,KC
        DO LL=1,NCBE
          IF(ISDOMAIN(LCBE(LL)))THEN
          NSID=NCSERE(LL,M)
          L=LCBE(LL)
          IF(UHDY2(L,K).GE.0.)THEN
            ! *** FLOWING OUT OF DOMAIN
            IF(ISTL_.EQ.2)THEN
              CTMP=CON1(L,K)+DELT*(FUHU(L,K)
     &            -UHDY2(L,K)*CON1(L,K))*DXYIP(L)*HPI(L)
            ELSE
              IF(ISCDCA(MVAR).NE.2) CTMP=CON1(L,K)+DELT*(FUHU(L,K)
     &            -UHDY2(L,K)*CON1(L,K))*DXYIP(L)*HPI(L)
              IF(ISCDCA(MVAR).EQ.2) CTMP=0.5*(CON1(L,K)+CON(L,K))
     &           +0.5*(CON1(L,K)-CON(L,K))*H2P(L)*HPI(L)+DELT*(FUHU(L,K)
     &            -0.5*UHDY2(L,K)*(CON1(L,K)+CON(L,K)))*DXYIP(L)*HPI(L)
              CON1(L,K)=CON(L,K)
            ENDIF
            CON(L,K)=CTMP
            CBETMP=CBE(LL,1,M)+CSERT(1,NSID,M)
            IF(M.EQ.1.AND.CON(L,K).GT.CBETMP) CON(L,K)=CBETMP
            CLOE(LL,K,M)=CON(L,K)
            NLOE(LL,K,M)=N
          ELSE
            ! *** FLOWING INTO DOMAIN
            IF(ISUD.EQ.1) CON1(L,K)=CON(L,K)
            CBT=WTCI(K,1)*CBE(LL,1,M)+WTCI(K,2)*CBE(LL,2,M)+CSERT(
     &          K,NSID,M)
            NMNLO=N-NLOE(LL,K,M)
            IF(NMNLO.GE.NTSCRE(LL))THEN
              CON(L,K)=CBT
            ELSE
              CON(L,K)=CLOE(LL,K,M)
     &            +(CBT-CLOE(LL,K,M))*FLOAT(NMNLO)/FLOAT(NTSCRE(LL))
            ENDIF
          ENDIF
          ENDIF
        ENDDO
      ENDDO
      MPI_WTIMES(668)=MPI_WTIMES(668)+MPI_TOC(S3TIME)
C
      ! *** NORTH OPEN BC
      S3TIME=MPI_TIC()
      DO K=1,KC
        DO LL=1,NCBN
          IF(ISDOMAIN(LCBN(LL)))THEN
          NSID=NCSERN(LL,M)
          L=LCBN(LL)
          LS=LSC(L)
!          if(lcbn(ll).eq.16759.and.k.eq.1) print*,'f1',nsid,l,ls
          IF(VHDX2(L,K).GE.0.)THEN
            ! *** FLOWING OUT OF DOMAIN
            IF(ISTL_.EQ.2)THEN
              CTMP=CON1(L,K)+DELT*(FVHU(L,K)
     &            -VHDX2(L,K)*CON1(L,K))*DXYIP(L)*HPI(L)
!          if(lcbn(ll).eq.16759.and.k.eq.1) print*,'f2',ctmp
            ELSE
              IF(ISCDCA(MVAR).NE.2) CTMP=CON1(L,K)+DELT*(FVHU(L,K)
     &            -VHDX2(L,K)*CON1(L,K))*DXYIP(L)*HPI(L)
              IF(ISCDCA(MVAR).EQ.2) CTMP=0.5*(CON1(L,K)+CON(L,K))
     &           +0.5*(CON1(L,K)-CON(L,K))*H2P(L)*HPI(L)+DELT*(FVHU(L,K)
     &            -0.5*VHDX2(L,K)*(CON1(L,K)+CON(L,K)))*DXYIP(L)*HPI(L)
              CON1(L,K)=CON(L,K)
!          if(lcbn(ll).eq.16759.and.k.eq.1) print*,'f3',ctmp
            ENDIF
            CON(L,K)=CTMP
            CBNTMP=CBN(LL,1,M)+CSERT(1,NSID,M)
!          if(lcbn(ll).eq.16759.and.k.eq.1) print*,'f4',CON(L,K)
            IF(M.EQ.1.AND.CON(L,K).GT.CBNTMP) CON(L,K)=CBNTMP
!          if(lcbn(ll).eq.16759.and.k.eq.1) print*,'f5',CON(L,K)
            CLON(LL,K,M)=CON(L,K)
            NLON(LL,K,M)=N
          ELSE
            ! *** FLOWING INTO DOMAIN
            IF(ISUD.EQ.1) CON1(L,K)=CON(L,K)
            CBT=WTCI(K,1)*CBN(LL,1,M)+WTCI(K,2)*CBN(LL,2,M)+CSERT(
     &          K,NSID,M)
            NMNLO=N-NLON(LL,K,M)
            IF(NMNLO.GE.NTSCRN(LL))THEN
              CON(L,K)=CBT
!          if(lcbn(ll).eq.16759.and.k.eq.1) print*,'f7',CON(L,K)
            ELSE
              CON(L,K)=CLON(LL,K,M)
     &            +(CBT-CLON(LL,K,M))*FLOAT(NMNLO)/FLOAT(NTSCRN(LL))
            ENDIF
          ENDIF
          ENDIF
        ENDDO
      ENDDO
      MPI_WTIMES(669)=MPI_WTIMES(669)+MPI_TOC(S3TIME)
C
C **  ANTI-DIFFUSIVE ADVECTIVE FLUX CALCULATION
C
      IF(ISADAC(MVAR).EQ.0) GOTO 2000
      IF(ISCDCA(MVAR).EQ.1) GOTO 2000
      IF(ISFCT(MVAR).GT.0)THEN
        ! *** DU & DV are used as a temporary array in this sub
        DO K=1,KC
          DU(1,K)=0.
          DV(1,K)=0.
          DU(LC,K)=0.
          DV(LC,K)=0.
        ENDDO
      ENDIF
C
C **  STANDARD ANTI-DIFFUSIVE ADVECTIVE FLUX CALCULATION
C
      ! *** PMC BEGIN BLOCK
      ! *** GET ONLY POSITIVE CONCENTRATIONS
      S3TIME=MPI_TIC()
      DO K=1,KC
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          POS(L,K)=MAX(CON(L,K),0.)
        ENDDO
      ENDDO
      CALL broadcast_boundary_array(POS,ic)
      MPI_WTIMES(670)=MPI_WTIMES(670)+MPI_TOC(S3TIME)
      ! *** PMC END BLOCK
C
      IF(IDRYTBP.EQ.0)THEN
        S3TIME=MPI_TIC()
        DO K=1,KC
          UUU(LC,K)=0.0
          VVV(LC,K)=0.0
          UUU(1,K)=0.0
          VVV(1,K)=0.0
        ENDDO
!$OMP PARALLEL DO
        DO L=LMPI1,LMPILC
          WWW(L,0)=0.0
          WWW(L,KC)=0.0
        ENDDO
C
        DO K=1,KC
!$OMP PARALLEL DO PRIVATE(LS)
          DO L=LMPI2,LMPILA
            LS=LSC(L)
            UUU(L,K)=U2(L,K)*(POS(L,K)-POS(L-1,K))*DXIU(L)
            VVV(L,K)=V2(L,K)*(POS(L,K)-POS(LS,K))*DYIV(L)
          ENDDO
        ENDDO
        DO K=1,KS
          RDZIG=DZIG(K)
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            WWW(L,K)=W2(L,K)*(POS(L,K+1)-POS(L,K))*HPI(L)*RDZIG
          ENDDO
        ENDDO
        MPI_WTIMES(671)=MPI_WTIMES(671)+MPI_TOC(S3TIME)
C
        S3TIME=MPI_TIC()
        CALL broadcast_boundary_array(UUU,ic)
        CALL broadcast_boundary_array(VVV,ic)
        CALL broadcast_boundary_array(WWW,ic)
        MPI_WTIMES(696)=MPI_WTIMES(696)+MPI_TOC(S3TIME)
C
        S3TIME=MPI_TIC()
        CALL broadcast_boundary_array(CON,ic)
        CALL broadcast_boundary_array(CON1,ic)
        MPI_WTIMES(695)=MPI_WTIMES(695)+MPI_TOC(S3TIME)

        S3TIME=MPI_TIC()
        CALL broadcast_boundary_array(FQCPAD,ic)
        CALL broadcast_boundary_array(QSUMPAD,ic)
        MPI_WTIMES(695)=MPI_WTIMES(695)+MPI_TOC(S3TIME)

        S3TIME=MPI_TIC()
        DO K=1,KC
          RDZIC=DZIC(K)
!$OMP PARALLEL DO PRIVATE(LN,LS,LNW,LSE,AUHU,AVHV,UTERM,VTERM,
!$OMP+    SSCORUE,SSCORUW,SSCORVN,SSCORVS,SSCORU,SSCORV,UHU,VHV)
          DO L=LMPI2,LMPILA
            LN=LNC(L)
            LS=LSC(L)
            LNW=LNWC(L)
            LSE=LSEC(L)
            AUHU=ABS(UHDY2(L,K))
            AVHV=ABS(VHDX2(L,K))
            UTERM=AUHU*(POS(L,K)-POS(L-1,K))
            VTERM=AVHV*(POS(L,K)-POS(LS,K))
            IF(ISADAC(MVAR).GE.2)THEN
              SSCORUE=DELTA*RDZIC*DXYIP(L  )*HPI(L  )*(FQCPAD(L  ,K)
     &            -QSUMPAD(L  ,K)*CON(L  ,K))
              SSCORUW=DELTA*RDZIC*DXYIP(L-1)*HPI(L-1)*(FQCPAD(L-1,K)
     &            -QSUMPAD(L-1,K)*CON(L-1,K))
              SSCORVN=DELTA*RDZIC*DXYIP(L  )*HPI(L  )*(FQCPAD(L  ,K)
     &            -QSUMPAD(L  ,K)*CON(L  ,K))
              SSCORVS=DELTA*RDZIC*DXYIP(LS )*HPI(LS )*(FQCPAD(LS ,K)
     &            -QSUMPAD(LS ,K)*CON(LS ,K))
              SSCORU=MAX(UHDY2(L,K),0.0)*SSCORUW+MIN(UHDY2(L,K),0.0)
     &            *SSCORUE
              SSCORV=MAX(VHDX2(L,K),0.0)*SSCORVS+MIN(VHDX2(L,K),0.0)
     &            *SSCORVN
              UTERM=UTERM+SSCORU
              VTERM=VTERM+SSCORV
            ENDIF
            IF(UHDY2(L,K).GE.0.0)THEN
              UTERM=UTERM-0.5*DELTA*UHDY2(L,K)*
     &            (VVV(LNW,K)+VVV(L-1,K)+WWW(L-1,K)+WWW(L-1,K-1)
     &            +UUU(L,K)+UUU(L-1,K))
            ELSE
              UTERM=UTERM-0.5*DELTA*UHDY2(L,K)*
     &            (VVV(LN,K)+VVV(L,K)+WWW(L,K)+WWW(L,K-1)
     &            +UUU(L,K)+UUU(L+1,K))
            ENDIF
            IF(VHDX2(L,K).GE.0.0)THEN
              VTERM=VTERM-0.5*DELTA*VHDX2(L,K)*
     &            (UUU(LS,K)+UUU(LSE,K)+WWW(LS,K)+WWW(LS,K-1)
     &            +VVV(LS,K)+VVV(L,K))
            ELSE
              VTERM=VTERM-0.5*DELTA*VHDX2(L,K)*
     &            (UUU(L,K)+UUU(L+1,K)+WWW(L,K)+WWW(L,K-1)
     &            +VVV(LN,K)+VVV(L,K))
            ENDIF
            IF(ISFCT(MVAR).GE.2)THEN
              FUHU(L,K)=0.5*UTERM
              FVHU(L,K)=0.5*VTERM
              IF(ISFCT(MVAR).EQ.3)THEN
                FUHU(L,K)=UTERM
                FVHU(L,K)=VTERM
              ENDIF
            ELSE
              UHU=UTERM/(POS(L,K)+POS(L-1,K)+BSMALL)
              VHV=VTERM/(POS(L,K)+POS(LS,K)+BSMALL)
              FUHU(L,K)=MAX(UHU,0.)*POS(L-1,K)
     &            +MIN(UHU,0.)*POS(L,K)
              FVHU(L,K)=MAX(VHV,0.)*POS(LS,K)
     &            +MIN(VHV,0.)*POS(L,K)
            ENDIF
          ENDDO
        ENDDO
        MPI_WTIMES(672)=MPI_WTIMES(672)+MPI_TOC(S3TIME)
C
        S3TIME=MPI_TIC()
        DO K=1,KS
!$OMP PARALLEL DO PRIVATE(LN,AWW,WTERM,SSCORWA,SSCORWB,SSCORW,WW)
          DO L=LMPI2,LMPILA
            LN=LNC(L)
            AWW=ABS(W2(L,K))
            WTERM=AWW*(POS(L,K+1)-POS(L,K))
            IF(ISADAC(MVAR).GE.2)THEN
              SSCORWA=DELTA*DZIG(K+1)*HPI(L)*DXYIP(L)
     &            *(FQCPAD(L,K+1)-QSUMPAD(L,K+1)*POS(L,K+1))
              SSCORWB=DELTA*DZIG(K)*HPI(L)*DXYIP(L)
     &            *(FQCPAD(L,K  )-QSUMPAD(L,K  )*POS(L,K  ))
              SSCORW=MAX(W2(L,K),0.0)*SSCORWB+MIN(W2(L,K),0.0)*SSCORWA
              WTERM=WTERM+SSCORW
            ENDIF
            IF(W2(L,K).GE.0.0)THEN
              WTERM=WTERM-0.5*DELTA*W2(L,K)*
     &            (UUU(L,K)+UUU(L+1,K)+VVV(L,K)+VVV(LN,K)
     &            +WWW(L,K)+WWW(L,K-1))
            ELSE
              WTERM=WTERM-0.5*DELTA*W2(L,K)*
     &            (UUU(L+1,K+1)+UUU(L,K+1)+VVV(LN,K+1)+VVV(L,K+1)
     &            +WWW(L,K)+WWW(L,K+1))
            ENDIF
            IF(ISFCT(MVAR).GE.2)THEN
              FWU(L,K)=0.5*WTERM
              IF(ISFCT(MVAR).EQ.3)THEN
                FWU(L,K)=WTERM
              ENDIF
            ELSE
              WW=WTERM/(POS(L,K+1)+POS(L,K)+BSMALL)
              FWU(L,K)=MAX(WW,0.)*POS(L,K)
     &            +MIN(WW,0.)*POS(L,K+1)
            ENDIF
          ENDDO
        ENDDO
        MPI_WTIMES(673)=MPI_WTIMES(673)+MPI_TOC(S3TIME)
C
C ** SET ANTIDIFFUSIVE FLUXES TO ZERO FOR SOURCE CELLS
C
        S3TIME=MPI_TIC()
        IF(ISADAC(MVAR).EQ.1)THEN
          ! *** ANTIDIFFUSION TURNED OFF FOR SOURCE CELLS
          DO K=1,KC
!$OMP PARALLEL DO PRIVATE(LN,LS)
            DO L=LMPI2,LMPILA
              LN=LNC(L)
              LS=LSC(L)
              IF(QSUMPAD(L,K).GT.0.0)THEN
                FUHU(L  ,K)=0.
                FUHU(L+1,K)=0.
                FVHU(L  ,K)=0.
                FVHU(LN ,K)=0.
                FWU(L,K  )=0.
                FWU(L,K-1)=0.
              ENDIF
              IF(QSUMPAD(LS,K).GT.0.0)THEN   ! MPI
                FVHU(L  ,K)=0.
              ENDIF
              IF(QSUMPAD(L-1,K).GT.0.0)THEN  ! MPI
                FUHU(L  ,K)=0.
              ENDIF
            ENDDO
          ENDDO
        ENDIF
        MPI_WTIMES(674)=MPI_WTIMES(674)+MPI_TOC(S3TIME)
C
C ** SET ANTIDIFFUSIVE FLUXES TO ZERO FOR OPEN BOUNDARY CELLS
C
        S3TIME=MPI_TIC()
        DO K=1,KC
          DO LL=1,NCBS
            L=LCBS(LL)
            LN=LNC(L)
            FVHU(LN,K)=0.0
          ENDDO
          DO LL=1,NCBW
            L=LCBW(LL)
            FUHU(L+1,K)=0.0
          ENDDO
          DO LL=1,NCBE
            L=LCBE(LL)
            FUHU(L,K)=0.0
          ENDDO
          DO LL=1,NCBN
            L=LCBN(LL)
            FVHU(L,K)=0.0
          ENDDO
        ENDDO
        MPI_WTIMES(675)=MPI_WTIMES(675)+MPI_TOC(S3TIME)
C
      S3TIME=MPI_TIC()
      CALL broadcast_boundary_array(FUHU,ic)
      CALL broadcast_boundary_array(FVHU,ic)
      CALL broadcast_boundary_array(FWU,ic)
      MPI_WTIMES(697)=MPI_WTIMES(697)+MPI_TOC(S3TIME)
C
C **  CALCULATE AND APPLY FLUX CORRECTED TRANSPORT LIMITERS
C
        if(PRINT_SUM.AND.MVAR.eq.2)then
          call collect_in_zero_array(con)
          IF(MYRANK.EQ.0) PRINT*, 'cn1','con = ', sum(abs(dble(con)))
        endif

        IF(ISFCT(MVAR).EQ.0) GOTO 1100
C
C **  DETERMINE MAX AND MIN CONCENTRATIONS
C
        S3TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            CONTMX(L,K)=0.0
            CONTMN(L,K)=0.0
          ENDDO
        ENDDO
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            CONTMX(L,K)=MAX(CON(L,K),CON2(L,K))
            CONTMN(L,K)=MIN(CON(L,K),CON2(L,K))
          ENDDO
        ENDDO
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          CMAX(L,1)=MAX(CONTMX(L,1),CONTMX(L,2))
          CMAX(L,KC)=MAX(CONTMX(L,KS),CONTMX(L,KC))
          CMIN(L,1)=MIN(CONTMN(L,1),CONTMN(L,2))
          CMIN(L,KC)=MIN(CONTMN(L,KS),CONTMN(L,KC))
        ENDDO
        DO K=2,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            CMAXT=MAX(CONTMX(L,K-1),CONTMX(L,K+1))
            CMAX(L,K)=MAX(CONTMX(L,K),CMAXT)
            CMINT=MIN(CONTMN(L,K-1),CONTMN(L,K+1))
            CMIN(L,K)=MIN(CONTMN(L,K),CMINT)
          ENDDO
        ENDDO
        MPI_WTIMES(676)=MPI_WTIMES(676)+MPI_TOC(S3TIME)
C
        S3TIME=MPI_TIC()
        CALL broadcast_boundary_array(CONTMX,ic)
        CALL broadcast_boundary_array(CONTMN,ic)
        MPI_WTIMES(698)=MPI_WTIMES(698)+MPI_TOC(S3TIME)
C
        S3TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO PRIVATE(LS,LN,CWMAX,CEMAX,CSMAX,CNMAX,CMAXT,
!$OMP+                    CWMIN,CEMIN,CSMIN,CNMIN,CMINT)
          DO L=LMPI2,LMPILA
            LS=LSC(L)
            LN=LNC(L)
            CWMAX=SUB(L)*CONTMX(L-1,K)
            CEMAX=SUB(L+1)*CONTMX(L+1,K)
            CSMAX=SVB(L)*CONTMX(LS,K)
            CNMAX=SVB(LN)*CONTMX(LN,K)
            CMAXT=MAX(CNMAX,CEMAX)
            CMAXT=MAX(CMAXT,CSMAX)
            CMAXT=MAX(CMAXT,CWMAX)
            CMAX(L,K)=MAX(CMAX(L,K),CMAXT)
            CWMIN=SUB(L)*CONTMN(L-1,K)+1.E+6*(1.-SUB(L))
            CEMIN=SUB(L+1)*CONTMN(L+1,K)+1.E+6*(1.-SUB(L+1))
            CSMIN=SVB(L)*CONTMN(LS,K)+1.E+6*(1.-SVB(L))
            CNMIN=SVB(LN)*CONTMN(LN,K)+1.E+6*(1.-SVB(LN))
            CMINT=MIN(CNMIN,CEMIN)
            CMINT=MIN(CMINT,CSMIN)
            CMINT=MIN(CMINT,CWMIN)
            CMIN(L,K)=MIN(CMIN(L,K),CMINT)
          ENDDO
        ENDDO
        MPI_WTIMES(677)=MPI_WTIMES(677)+MPI_TOC(S3TIME)
C
C **  SEPARATE POSITIVE AND NEGATIVE FLUXES PUTTING NEGATIVE FLUXES
C **  INTO FUHV, FVHV, AND FWV
C
        S3TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            FUHV(L,K)=MIN(FUHU(L,K),0.)
            FUHU(L,K)=MAX(FUHU(L,K),0.)
            FVHV(L,K)=MIN(FVHU(L,K),0.)
            FVHU(L,K)=MAX(FVHU(L,K),0.)
          ENDDO
        ENDDO
        DO K=1,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            FWV(L,K)=MIN(FWU(L,K),0.)
            FWU(L,K)=MAX(FWU(L,K),0.)
          ENDDO
        ENDDO
        CALL broadcast_boundary_array(FUHV,ic)
        CALL broadcast_boundary_array(FUHU,ic)
        CALL broadcast_boundary_array(FVHV,ic)
        CALL broadcast_boundary_array(FVHU,ic)
        CALL broadcast_boundary_array(FWV, ic)
        CALL broadcast_boundary_array(FWU, ic)
        MPI_WTIMES(678)=MPI_WTIMES(678)+MPI_TOC(S3TIME)
C
C **  CALCULATE INFLUX AND OUTFLUX IN CONCENTRATION UNITS AND LOAD
C **  INTO DU AND DV, THEN ADJUCT VALUES AT BOUNDARIES
C
        S3TIME=MPI_TIC()
        DO K=1,KC
          RDZIC=DZIC(K)
!$OMP PARALLEL DO PRIVATE(LN)
          DO L=LMPI2,LMPILA
            LN=LNC(L)
            DU(L,K)=DELT*(DXYIP(L)*(FUHU(L,K)-FUHV(L+1,K)
     &          +FVHU(L,K)-FVHV(LN,K))
     &          +RDZIC*(FWU(L,K-1)-FWV(L,K)) )*HPI(L)
            DV(L,K)=DELT*(DXYIP(L)*(FUHU(L+1,K)-FUHV(L,K)
     &          +FVHU(LN,K)-FVHV(L,K))
     &          +RDZIC*(FWU(L,K)-FWV(L,K-1)) )*HPI(L)
          ENDDO
        ENDDO
        DO K=1,KC
          DO IOBC=1,NBCSOP
            L=LOBCS(IOBC)
            DU(L,K)=0.
            DV(L,K)=0.
          ENDDO
        END DO
        DO K=1,KC
          DO LL=1,NCBS
            L=LCBS(LL)
            LN=LNC(L)
            DU(LN,K)=0.
            DV(LN,K)=0.
          ENDDO
          DO LL=1,NCBW
            L=LCBW(LL)
            DU(L+1,K)=0.
            DV(L+1,K)=0.
          ENDDO
          DO LL=1,NCBE
            L=LCBE(LL)
            DU(L-1,K)=0.
            DV(L-1,K)=0.
          ENDDO
          DO LL=1,NCBN
            L=LCBN(LL)
            LS=LSC(L)
            DU(LS,K)=0.
            DV(LS,K)=0.
          ENDDO
        ENDDO
        MPI_WTIMES(679)=MPI_WTIMES(679)+MPI_TOC(S3TIME)
C
C **  CALCULATE BETA COEFFICIENTS WITH BETAUP AND BETADOWN IN DU AND DV
C
        S3TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
          IF(DU(L,K).GT.0.)DU(L,K)=(CMAX(L,K)-POS(L,K))/(DU(L,K)+BSMALL)
            DU(L,K)=MIN(DU(L,K),1.)
          IF(DV(L,K).GT.0.)DV(L,K)=(CON(L,K)-CMIN(L,K))/(DV(L,K)+BSMALL)
            DV(L,K)=MIN(DV(L,K),1.)
          ENDDO
        ENDDO
        MPI_WTIMES(680)=MPI_WTIMES(680)+MPI_TOC(S3TIME)
C
      S3TIME=MPI_TIC()
      CALL broadcast_boundary_array(DU,ic)
      CALL broadcast_boundary_array(DV,ic)
      MPI_WTIMES(699)=MPI_WTIMES(699)+MPI_TOC(S3TIME)
C
C **  LIMIT FLUXES
C
        S3TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO PRIVATE(LS)
          DO L=LMPI2,LMPILA
            LS=LSC(L)
            FUHU(L,K)=MIN(DV(L-1,K),DU(L,K))*FUHU(L,K)
     &          +MIN(DU(L-1,K),DV(L,K))*FUHV(L,K)
            FVHU(L,K)=MIN(DV(LS,K),DU(L,K))*FVHU(L,K)
     &          +MIN(DU(LS,K),DV(L,K))*FVHV(L,K)
          ENDDO
        ENDDO
        DO K=1,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            FWU(L,K)=MIN(DV(L,K),DU(L,K+1))*FWU(L,K)
     &          +MIN(DU(L,K),DV(L,K+1))*FWV(L,K)
          ENDDO
        ENDDO
        MPI_WTIMES(681)=MPI_WTIMES(681)+MPI_TOC(S3TIME)
C
C **  ANTI-DIFFUSIVE ADVECTION CALCULATION
C
 1100   CONTINUE
C
      S3TIME=MPI_TIC()
      CALL broadcast_boundary_array(FUHU,ic)
      CALL broadcast_boundary_array(FVHU,ic)
      MPI_WTIMES(700)=MPI_WTIMES(700)+MPI_TOC(S3TIME)
C
      if(PRINT_SUM.AND.MVAR.eq.6)then
      DO NS=1,NSED; call collect_in_zero_array(SED(:,:,NS)); ENDDO
      IF(MYRANK.EQ.0) PRINT*, MO,'b6', sum(abs(dble(SED)))
      endif
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(con)
        IF(MYRANK.EQ.0) PRINT*, 'cn2','con = ', sum(abs(dble(con)))
      endif
C
        S3TIME=MPI_TIC()
        DO K=1,KC
          RDZIC=DZIC(K)
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            CH(L,K)=CON(L,K)*HP(L)
     &          +DELT*( (FUHU(L,K)-FUHU(L+1,K)
     &          +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &          +(FWU(L,K-1)-FWU(L,K))*RDZIC )
            CON(L,K)=SCB(L)*CH(L,K)*HPI(L)+(1.-SCB(L))*CON(L,K)
          ENDDO
        ENDDO
        MPI_WTIMES(682)=MPI_WTIMES(682)+MPI_TOC(S3TIME)
C
        if(PRINT_SUM.AND.MVAR.eq.2)then
          call collect_in_zero_array(con)
          IF(MYRANK.EQ.0) PRINT*, 'cn3','con = ', sum(abs(dble(con)))
        endif
C
      S3TIME=MPI_TIC()
      CALL broadcast_boundary_array(CON,ic)
      MPI_WTIMES(700)=MPI_WTIMES(700)+MPI_TOC(S3TIME)
C
C **  ADD REMAINING SEDIMENT SETTLING AND FLUX
C
      ENDIF
C
C **  ANTI-DIFFUSIVE ADVECTIVE FLUX CALCULATION WITH DRY BYPASS
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FUHU)
        call collect_in_zero_array(FVHU)
        call collect_in_zero_array(FWU )
        IF(MYRANK.EQ.0) PRINT*, '1FUHU = ', sum(abs(dble(FUHU)))
        IF(MYRANK.EQ.0) PRINT*, '1FVHU = ', sum(abs(dble(FVHU)))
        IF(MYRANK.EQ.0) PRINT*, '1FWU  = ', sum(abs(dble(FWU )))
      endif
C
      S3TIME=MPI_TIC()
      IF(IDRYTBP.GT.0)THEN
        ! *** DSLLC BEGIN
!$OMP PARALLEL DO
        DO L=LMPI1,LMPILC
          WWW(L,0)=0.0
          WWW(L,KC)=0.0
        ENDDO
        DO K=1,KC
          UUU(LC,K)=0.0
          VVV(LC,K)=0.0
          UUU(1,K)=0.0
          VVV(1,K)=0.0
        ENDDO

        DO K=1,KC
!$OMP PARALLEL DO PRIVATE(LS)
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              LS=LSC(L)
              UUU(L,K)=U2(L,K)*(POS(L,K)-POS(L-1,K))*DXIU(L)
              VVV(L,K)=V2(L,K)*(POS(L,K)-POS(LS,K))*DYIV(L)
            ELSE
              UUU(L,K)=0.
              VVV(L,K)=0.
            ENDIF
          ENDDO
        ENDDO

        DO K=1,KS
          RDZIG=DZIG(K)
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              WWW(L,K)=W2(L,K)*(POS(L,K+1)-POS(L,K))*HPI(L)*RDZIG
            ELSE
              WWW(L,K)=0.0
            ENDIF
          ENDDO
        ENDDO
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(UUU)
        call collect_in_zero_array(VVV)
        call collect_in_zero_array(WWW )
        IF(MYRANK.EQ.0) PRINT*, 'UUU = ', sum(abs(dble(UUU)))
        IF(MYRANK.EQ.0) PRINT*, 'VVV = ', sum(abs(dble(VVV)))
        IF(MYRANK.EQ.0) PRINT*, 'WWW = ', sum(abs(dble(WWW)))
      endif
C
      CALL broadcast_boundary_array(UUU,ic)
      CALL broadcast_boundary_array(VVV,ic)
      CALL broadcast_boundary_array(WWW,ic)
C
        DO K=1,KC
          RDZIC=DZIC(K)
!$OMP PARALLEL DO PRIVATE(LN,LS,LNW,LSE,AUHU,AVHV,UTERM,VTERM,SSCORUE,
!$OMP+                    SSCORUW,SSCORVN,SSCORVS,SSCORU,SSCORV,UHU,VHV)
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              LN=LNC(L)
              LS=LSC(L)
              LNW=LNWC(L)
              LSE=LSEC(L)
              AUHU=ABS(UHDY2(L,K))
              AVHV=ABS(VHDX2(L,K))
              UTERM=AUHU*(POS(L,K)-POS(L-1,K))
              VTERM=AVHV*(POS(L,K)-POS(LS,K))
              IF(ISADAC(MVAR).GE.2)THEN
                SSCORUE=DELTA*RDZIC*DXYIP(L  )*HPI(L  )*(FQCPAD(L  ,K)
     &            -QSUMPAD(L  ,K)*CON(L  ,K))
                SSCORUW=DELTA*RDZIC*DXYIP(L-1)*HPI(L-1)*(FQCPAD(L-1,K)
     &            -QSUMPAD(L-1,K)*CON(L-1,K))
                SSCORVN=DELTA*RDZIC*DXYIP(L  )*HPI(L  )*(FQCPAD(L  ,K)
     &            -QSUMPAD(L  ,K)*CON(L  ,K))
                SSCORVS=DELTA*RDZIC*DXYIP(LS )*HPI(LS )*(FQCPAD(LS ,K)
     &            -QSUMPAD(LS ,K)*CON(LS ,K))
                SSCORU=MAX(UHDY2(L,K),0.0)*SSCORUW+MIN(UHDY2(L,K),0.0)
     &            *SSCORUE
                SSCORV=MAX(VHDX2(L,K),0.0)*SSCORVS+MIN(VHDX2(L,K),0.0)
     &            *SSCORVN
                UTERM=UTERM+SSCORU
                VTERM=VTERM+SSCORV
              ENDIF
              IF(UHDY2(L,K).GE.0.0)THEN
                UTERM=UTERM-0.5*DELTA*UHDY2(L,K)*
     &            (VVV(LNW,K)+VVV(L-1,K)+WWW(L-1,K)+WWW(L-1,K-1)
     &            +UUU(L,K)+UUU(L-1,K))
              ELSE
                UTERM=UTERM-0.5*DELTA*UHDY2(L,K)*
     &            (VVV(LN,K)+VVV(L,K)+WWW(L,K)+WWW(L,K-1)
     &            +UUU(L,K)+UUU(L+1,K))
              ENDIF
              IF(VHDX2(L,K).GE.0.0)THEN
                VTERM=VTERM-0.5*DELTA*VHDX2(L,K)*
     &            (UUU(LS,K)+UUU(LSE,K)+WWW(LS,K)+WWW(LS,K-1)
     &            +VVV(LS,K)+VVV(L,K))
              ELSE
                VTERM=VTERM-0.5*DELTA*VHDX2(L,K)*
     &            (UUU(L,K)+UUU(L+1,K)+WWW(L,K)+WWW(L,K-1)
     &            +VVV(LN,K)+VVV(L,K))
              ENDIF
              IF(ISFCT(MVAR).GE.2)THEN
                FUHU(L,K)=0.5*UTERM
                FVHU(L,K)=0.5*VTERM
                IF(ISFCT(MVAR).EQ.3)THEN
                  FUHU(L,K)=UTERM
                  FVHU(L,K)=VTERM
                ENDIF
              ELSE
                UHU=UTERM/(POS(L,K)+POS(L-1,K)+BSMALL)
                VHV=VTERM/(POS(L,K)+POS(LS,K)+BSMALL)
                FUHU(L,K)=MAX(UHU,0.)*POS(L-1,K)
     &            +MIN(UHU,0.)*POS(L,K)
                FVHU(L,K)=MAX(VHV,0.)*POS(LS,K)
     &            +MIN(VHV,0.)*POS(L,K)
              ENDIF
            ELSE
              FUHU(L,K)=0.
              FVHU(L,K)=0.
            ENDIF
          ENDDO
        ENDDO
C
        if(PRINT_SUM.AND.MVAR.eq.2)then
          call collect_in_zero_array(FUHU)
          call collect_in_zero_array(FVHU)
          call collect_in_zero_array(FWU )
          IF(MYRANK.EQ.0) PRINT*, '2.FUHU = ', sum(abs(dble(FUHU)))
          IF(MYRANK.EQ.0) PRINT*, '2.FVHU = ', sum(abs(dble(FVHU)))
          IF(MYRANK.EQ.0) PRINT*, '2.FWU  = ', sum(abs(dble(FWU )))
        endif
C
        DO K=1,KS
!$OMP PARALLEL DO PRIVATE(LN,AWW,WTERM,SSCORWA,SSCORWB,SSCORW,WW)
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              LN=LNC(L)
              AWW=ABS(W2(L,K))
              WTERM=AWW*(POS(L,K+1)-POS(L,K))
              IF(ISADAC(MVAR).GE.2)THEN
                SSCORWA=DELTA*DZIG(K+1)*HPI(L)*DXYIP(L)
     &            *(FQCPAD(L,K+1)-QSUMPAD(L,K+1)*CON(L,K+1))
                SSCORWB=DELTA*DZIG(K)*HPI(L)*DXYIP(L)
     &            *(FQCPAD(L,K  )-QSUMPAD(L,K  )*CON(L,K  ))
                SSCORW=MAX(W2(L,K),0.0)*SSCORWB+MIN(W2(L,K),0.0)*SSCORWA
                WTERM=WTERM+SSCORW
              ENDIF
              IF(W2(L,K).GE.0.0)THEN
                WTERM=WTERM-0.5*DELTA*W2(L,K)*
     &            (UUU(L,K)+UUU(L+1,K)+VVV(L,K)+VVV(LN,K)
     &            +WWW(L,K)+WWW(L,K-1))
              ELSE
                WTERM=WTERM-0.5*DELTA*W2(L,K)*
     &            (UUU(L+1,K+1)+UUU(L,K+1)+VVV(LN,K+1)+VVV(L,K+1)
     &            +WWW(L,K)+WWW(L,K+1))
              ENDIF
              IF(ISFCT(MVAR).GE.2)THEN
                FWU(L,K)=0.5*WTERM
                IF(ISFCT(MVAR).EQ.3)THEN
                  FWU(L,K)=WTERM
                ENDIF
              ELSE
                WW=WTERM/(POS(L,K+1)+POS(L,K)+BSMALL)
                FWU(L,K)=MAX(WW,0.)*POS(L,K)
     &            +MIN(WW,0.)*POS(L,K+1)
              ENDIF
            ELSE
              FWU(L,K)=0.
            ENDIF
          ENDDO

        ENDDO
C
C ** SET ANTIDIFFUSIVE FLUXES TO ZERO FOR SOURCE CELLS
C
        IF(ISADAC(MVAR).EQ.1)THEN
          DO K=1,KC
!$OMP PARALLEL DO PRIVATE(LN)
            DO L=LMPI2,LMPILA
              IF(LMASKDRY(L))THEN
                IF(ABS(QSUM(L,K)).GT.1.E-12)THEN
                  LN=LNC(L)
                  FUHU(L  ,K)=0.
                  FUHU(L+1,K)=0.
                  FVHU(L  ,K)=0.
                  FVHU(LN ,K)=0.
                  FWU(L,K  )=0.
                  FWU(L,K-1)=0.
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDIF
        MPI_WTIMES(683)=MPI_WTIMES(683)+MPI_TOC(S3TIME)
        if(PRINT_SUM.AND.MVAR.eq.2)then
          call collect_in_zero_array(FUHU)
          call collect_in_zero_array(FVHU)
          call collect_in_zero_array(FWU )
          IF(MYRANK.EQ.0) PRINT*, '2FUHU = ', sum(abs(dble(FUHU)))
          IF(MYRANK.EQ.0) PRINT*, '2FVHU = ', sum(abs(dble(FVHU)))
          IF(MYRANK.EQ.0) PRINT*, '2FWU  = ', sum(abs(dble(FWU )))
        endif
C
C ** SET ANTIDIFFUSIVE FLUXES TO ZERO FOR OPEN BOUNDARY CELLS
C
        S3TIME=MPI_TIC()
        DO K=1,KC
          DO LL=1,NCBS
            L=LCBS(LL)
            LN=LNC(L)
            FVHU(LN,K)=0.0
          ENDDO
          DO LL=1,NCBW
            L=LCBW(LL)
            FUHU(L+1,K)=0.0
          ENDDO
          DO LL=1,NCBE
            L=LCBE(LL)
            FUHU(L,K)=0.0
          ENDDO
          DO LL=1,NCBN
            L=LCBN(LL)
            FVHU(L,K)=0.0
          ENDDO
        ENDDO
        MPI_WTIMES(684)=MPI_WTIMES(684)+MPI_TOC(S3TIME)
C
C **  CALCULATE AND APPLY FLUX CORRECTED TRANSPORT LIMITERS
C
        IF(ISFCT(MVAR).EQ.0) GOTO 1101
C
C **  DETERMINE MAX AND MIN CONCENTRATIONS
C
        S3TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            CMIN(L,K)=0.
            CMAX(L,K)=0.
          ENDDO
        ENDDO
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              CONTMX(L,K)=MAX(CON(L,K),CON2(L,K))
              CONTMN(L,K)=MIN(CON(L,K),CON2(L,K))
            ENDIF
          ENDDO
        ENDDO
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          IF(LMASKDRY(L))THEN
            CMAX(L,1)=MAX(CONTMX(L,1),CONTMX(L,2))
            CMAX(L,KC)=MAX(CONTMX(L,KS),CONTMX(L,KC))
            CMIN(L,1)=MIN(CONTMN(L,1),CONTMN(L,2))
            CMIN(L,KC)=MIN(CONTMN(L,KS),CONTMN(L,KC))
          ENDIF
        ENDDO
        DO K=2,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              CMAXT=MAX(CONTMX(L,K-1),CONTMX(L,K+1))
              CMAX(L,K)=MAX(CONTMX(L,K),CMAXT)
              CMINT=MIN(CONTMN(L,K-1),CONTMN(L,K+1))
              CMIN(L,K)=MIN(CONTMN(L,K),CMINT)
            ENDIF
          ENDDO
        ENDDO
        CALL broadcast_boundary_array(CONTMN,ic)
        CALL broadcast_boundary_array(CONTMX,ic)
        DO K=1,KC
!$OMP PARALLEL DO PRIVATE(LS,LN,CWMAX,CEMAX,CSMAX,CNMAX,CMAXT,
!$OMP+     CWMIN,CEMIN,CSMIN,CNMIN,CMINT)
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              LS=LSC(L)
              LN=LNC(L)
              CWMAX=SUB(L)*CONTMX(L-1,K)
              CEMAX=SUB(L+1)*CONTMX(L+1,K)
              CSMAX=SVB(L)*CONTMX(LS,K)
              CNMAX=SVB(LN)*CONTMX(LN,K)
              CMAXT=MAX(CNMAX,CEMAX)
              CMAXT=MAX(CMAXT,CSMAX)
              CMAXT=MAX(CMAXT,CWMAX)
              CMAX(L,K)=MAX(CMAX(L,K),CMAXT)
              CWMIN=SUB(L)*CONTMN(L-1,K)+1.E+6*(1.-SUB(L))
              CEMIN=SUB(L+1)*CONTMN(L+1,K)+1.E+6*(1.-SUB(L+1))
              CSMIN=SVB(L)*CONTMN(LS,K)+1.E+6*(1.-SVB(L))
              CNMIN=SVB(LN)*CONTMN(LN,K)+1.E+6*(1.-SVB(LN))
              CMINT=MIN(CNMIN,CEMIN)
              CMINT=MIN(CMINT,CSMIN)
              CMINT=MIN(CMINT,CWMIN)
              CMIN(L,K)=MIN(CMIN(L,K),CMINT)
            ENDIF
          ENDDO
        ENDDO
        MPI_WTIMES(685)=MPI_WTIMES(685)+MPI_TOC(S3TIME)
C
C **  SEPARATE POSITIVE AND NEGATIVE FLUXES PUTTING NEGATIVE FLUXES
C **  INTO FUHV, FVHV, AND FWV
C
        S3TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              FUHV(L,K)=MIN(FUHU(L,K),0.)
              FUHU(L,K)=MAX(FUHU(L,K),0.)
              FVHV(L,K)=MIN(FVHU(L,K),0.)
              FVHU(L,K)=MAX(FVHU(L,K),0.)
            ELSE
              FUHV(L,K)=0.
              FVHV(L,K)=0.
            ENDIF
          ENDDO
        ENDDO
        DO K=1,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              FWV(L,K)=MIN(FWU(L,K),0.)
              FWU(L,K)=MAX(FWU(L,K),0.)
            ELSE
              FWV(L,K)=0.
            ENDIF
          ENDDO
        ENDDO
        MPI_WTIMES(686)=MPI_WTIMES(686)+MPI_TOC(S3TIME)
        CALL broadcast_boundary_array(FUHV,ic)
        CALL broadcast_boundary_array(FUHU,ic)
        CALL broadcast_boundary_array(FVHU,ic)
        CALL broadcast_boundary_array(FVHV,ic)
C
C **  CALCULATE INFLUX AND OUTFLUX IN CONCENTRATION UNITS AND LOAD
C **  INTO DU AND DV, THEN ADJUCT VALUES AT BOUNDARIES
C
        S3TIME=MPI_TIC()
        DO K=1,KC
          RDZIC=DZIC(K)
!$OMP PARALLEL DO PRIVATE(LN)
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              LN=LNC(L)
              DU(L,K)=DELT*SCB(L)*( DXYIP(L)*(FUHU(L,K)-FUHV(L+1,K)
     &            +FVHU(L,K)-FVHV(LN,K))
     &            +RDZIC*(FWU(L,K-1)-FWV(L,K)) )*HPI(L)
              DV(L,K)=DELT*SCB(L)*( DXYIP(L)*(FUHU(L+1,K)-FUHV(L,K)
     &            +FVHU(LN,K)-FVHV(L,K))
     &            +RDZIC*(FWU(L,K)-FWV(L,K-1)) )*HPI(L)
            ELSE
              DU(L,K)=0.
              DV(L,K)=0.
            ENDIF
          ENDDO
        ENDDO
        DO K=1,KC
          DO LL=1,NCBS
            L=LCBS(LL)
            LN=LNC(L)
            DU(LN,K)=0.
            DV(LN,K)=0.
          ENDDO
          DO LL=1,NCBW
            L=LCBW(LL)
            DU(L+1,K)=0.
            DV(L+1,K)=0.
          ENDDO
          DO LL=1,NCBE
            L=LCBE(LL)
            DU(L-1,K)=0.
            DV(L-1,K)=0.
          ENDDO
          DO LL=1,NCBN
            L=LCBN(LL)
            LS=LSC(L)
            DU(LS,K)=0.
            DV(LS,K)=0.
          ENDDO
        ENDDO
        MPI_WTIMES(687)=MPI_WTIMES(687)+MPI_TOC(S3TIME)
C
C **  CALCULATE BETA COEFFICIENTS WITH BETAUP AND BETADOWN IN DU AND DV
C
        S3TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              IF(DU(L,K).GT.0.)DU(L,K)=(CMAX(L,K)-POS(L,K))/(DU(L,K)
     &            +BSMALL)
              DU(L,K)=MIN(DU(L,K),1.)
              IF(DV(L,K).GT.0.)DV(L,K)=(CON(L,K)-CMIN(L,K))/(DV(L,K)
     &            +BSMALL)
              DV(L,K)=MIN(DV(L,K),1.)
            ENDIF
          ENDDO
        ENDDO
        CALL broadcast_boundary_array(DU,ic)
        CALL broadcast_boundary_array(DV,ic)
C
C **  LIMIT FLUXES
C
        DO K=1,KC
!$OMP PARALLEL DO PRIVATE(LS)
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              LS=LSC(L)
              FUHU(L,K)=MIN(DV(L-1,K),DU(L,K))*FUHU(L,K)
     &            +MIN(DU(L-1,K),DV(L,K))*FUHV(L,K)
              FVHU(L,K)=MIN(DV(LS,K),DU(L,K))*FVHU(L,K)
     &            +MIN(DU(LS,K),DV(L,K))*FVHV(L,K)
            ENDIF
          ENDDO
        ENDDO
        DO K=1,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              FWU(L,K)=MIN(DV(L,K),DU(L,K+1))*FWU(L,K)
     &            +MIN(DU(L,K),DV(L,K+1))*FWV(L,K)
            ENDIF
          ENDDO
        ENDDO
        CALL broadcast_boundary_array(FUHU,ic)
        CALL broadcast_boundary_array(FVHU,ic)
        MPI_WTIMES(688)=MPI_WTIMES(688)+MPI_TOC(S3TIME)
C
C **  END OF ANTI-DIFFUSIVE ADVECTION CALCULATION
C
 1101   CONTINUE
C
        S3TIME=MPI_TIC()
        DO K=1,KC
          RDZIC=DZIC(K)
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              CH(L,K)=CON(L,K)*HP(L)
     &            +DELT*( (FUHU(L,K)-FUHU(L+1,K)
     &            +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &            +(FWU(L,K-1)-FWU(L,K))*RDZIC )
              CON(L,K)=SCB(L)*CH(L,K)*HPI(L)+(1.-SCB(L))*CON(L,K)
            ENDIF
          ENDDO
        ENDDO
        MPI_WTIMES(689)=MPI_WTIMES(689)+MPI_TOC(S3TIME)
C
C **  ADD REMAINING SEDIMENT SETTLING AND FLUX
C
      ENDIF
C
C **  DIAGNOSE FCT SCHEME
C
      S3TIME=MPI_TIC()
      IF(ISFCT(MVAR).EQ.99)THEN
        WRITE(6,6110)N
        DO K=1,KC
!$OMP PARALLEL DO PRIVATE(CCMAX,CCMIN)
          DO L=LMPI2,LMPILA
            CCMAX=SCB(L)*(CON(L,K)-CMAX(L,K))
            IF(CCMAX.GT.0.)THEN
              WRITE(6,6111)CON(L,K),CMAX(L,K),IL(L),JL(L),K
            ENDIF
            CCMIN=SCB(L)*(CMIN(L,K)-CON(L,K))
            IF(CCMIN.GT.0.)THEN
              WRITE(6,6112)CMIN(L,K),CON(L,K),IL(L),JL(L),K
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(690)=MPI_WTIMES(690)+MPI_TOC(S3TIME)
 6110 FORMAT('  FCT DIAGNOSTICS AT N = ',I5)
 6111 FORMAT('  CON = ',E12.4,3X,'CMAX = ',E12.4,3X,'I,J,K=',(3I10))
 6112 FORMAT('  CMIN = ',E12.4,3X,'CON = ',E12.4,3X,'I,J,K=',(3I10))

      ! *** ZERO HEAT FLUXES
 2000 IF(MVAR.EQ.2)THEN
        ! *** ZERO EVAP/RAINFALL
        S3TIME=MPI_TIC()
  !$OMP PARALLEL DO
        DO L=LMPI1,LMPILC
          FQC(L,KC)=0.
        ENDDO
        IF(ISADAC(MVAR).GE.2)THEN
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            FQCPAD(L,KC)=0.
          ENDDO
        ENDIF
        IF(ISADAC(MVAR).GT.0)THEN
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            QSUMPAD(L,KC)=0.
          ENDDO
        ENDIF
        MPI_WTIMES(691)=MPI_WTIMES(691)+MPI_TOC(S3TIME)
      ENDIF
      RETURN
      END