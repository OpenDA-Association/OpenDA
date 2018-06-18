      SUBROUTINE CALTRAN (ISTL_,IS2TL_,MVAR,MO,CON,CON1)  
C  
C CHANGE RECORD  
C  ADDED TRANSPORT BYPASS MASK, IMASKDRY FOR DRY CELLS  
C **  SUBROUTINE CALTRAN CALCULATES THE ADVECTIVE  
C **  TRANSPORT OF DISSOLVED OR SUSPENDED CONSITITUENT M LEADING TO  
C **  A NEW VALUE AT TIME LEVEL (N+1). THE VALUE OF ISTL INDICATES  
C **  THE NUMBER OF TIME LEVELS IN THE STEP  
C  
      USE GLOBAL
  
      REAL, DIMENSION(LCM,KCM), intent(inout) :: CON,CON1
      REAL, DIMENSION(:,:), allocatable :: UTERM0, VTERM0, 
     +                           SSCORUEWNS, SSCORWAB
      INTEGER, dimension(0:nthds-1,KC) ::  icount

      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::CONTMN  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::CONTMX  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::FQCPAD  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::QSUMNAD  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::QSUMPAD  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::POS

      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::WQBCCON

      ALLOCATE(UTERM0(LC,KC)) 
      ALLOCATE(VTERM0(LC,KC)) 
      ALLOCATE(SSCORUEWNS(LC,KC)) 
      ALLOCATE(SSCORWAB(LC,KC)) 
      
      IF(.NOT.ALLOCATED(CONTMN))THEN
        ALLOCATE(CONTMN(0:LCM1,KCM))  
        ALLOCATE(CONTMX(0:LCM1,KCM))  
        ALLOCATE(FQCPAD(0:LCM1,KCM))  
        ALLOCATE(QSUMNAD(0:LCM1,KCM))  
        ALLOCATE(QSUMPAD(0:LCM1,KCM))  
        ALLOCATE(POS(0:LCM1,KCM))  
        ALLOCATE(WQBCCON(0:LCM1,KCM))

!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_LC(1,ithds)
         LL=jse_LC(2,ithds)
c
        DO L=LF,LL
          FWU(L,0)=0.  
          FWU(L,KC)=0.  
        ENDDO  
c
      enddo
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

      ! *** DSLLC BEGIN
      M=MO
      IF(IS2TL_.EQ.1)THEN  
        ISUD=1  
        IF(MVAR.NE.8)THEN
c         CON1=CON    ! *** ARRAYS
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_LC(1,ithds)
         LL=jse_LC(2,ithds)
c
        DO K=1,KC
        DO L=LF,LL
           CON1(L,K)=CON(L,K)
        ENDDO
        ENDDO
c
      enddo

        ENDIF            
      ENDIF  
      
      ! *** SAVE OLD WQ CONCENTRATIONS FOR OPEN BOUNDARY CELLS
      IF(MVAR.EQ.8)THEN
        DO IOBC=1,NBCSOP  
          L=LOBCS(IOBC)  
          DO K=1,KC
            WQBCCON(L,K)=CON(L,K)  
          ENDDO  
        ENDDO  
      ENDIF
      ! *** DSLLC END
C  
C **  CALCULATED EXTERNAL SOURCES AND SINKS  
C  
      CALL CALFQC (ISTL_,IS2TL_,MVAR,M,CON,CON1,FQCPAD,QSUMPAD,QSUMNAD)  
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
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      IF(IDRYTBP.EQ.0)THEN  
        DO K=1,KC  
          DO L=LF,LL
              FUHU(L,K)=UHDY2(L,K)*CON1(LUPU(L,K),K)  
              FVHU(L,K)=VHDX2(L,K)*CON1(LUPV(L,K),K)  
          ENDDO  
        ENDDO  
        IF(KC.GT.1)THEN
          DO K=1,KS  
            DO L=LF,LL
              FWU(L,K)=W2(L,K)*CON1(L,KUPW(L,K))  
            ENDDO  
          ENDDO  
        ENDIF
      ELSE  
        DO K=1,KC  
          DO L=LF,LL
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
            DO L=LF,LL
              IF(LMASKDRY(L))THEN  
                FWU(L,K)=W2(L,K)*CON1(L,KUPW(L,K))  
              ELSE
                FWU(L,K)=0.
              ENDIF  
            ENDDO 
          ENDDO  
        ENDIF  
      ENDIF  
c
      enddo
      GOTO 500  
C  
C **  CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE WITH ADVECTION  
C **  AVERAGED BETWEEN  (N-1) AND (N+1) AND ADVECTED FIELD AVERAGED  
C **  BETWEEN AT (N-1) AND (N) IF ISTL 3 ONLY  
C  
  350 CONTINUE  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO K=1,KC  
        DO L=LF,LL
          CONT(L,K)=0.5*(CON(L,K)+CON1(L,K))  
     &        +DELT*0.5*FQC(L,K)*DXYIP(L)/H2P(L)  
        ENDDO  
      ENDDO  
c
      enddo
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO K=1,KC  
        DO L=LF,LL
          FUHU(L,K)=UHDY2(L,K)*CONT(LUPU(L,K),K)  
          FVHU(L,K)=VHDX2(L,K)*CONT(LUPV(L,K),K)  
        ENDDO  
      ENDDO  
      IF(KC.GT.1)THEN
        DO K=1,KS  
          DO L=LF,LL
            FWU(L,K)=W2(L,K)*CONT(L,KUPW(L,K))  
          ENDDO  
        ENDDO
      ENDIF  
c
      enddo
      GOTO 500  
C  
C **  CALCULATE ADVECTIVE FLUXES BY CENTRAL DIFFERENCE WITH TRANSPORT  
C **  AVERAGED BETWEEN (N+1) AND (N-1) AND TRANSPORTED FIELD AT (N)  
C  
  400 CONTINUE  
C PMC      DO K=1,KC  
C PMC        DO L=2,LA  
C PMC          CONT(L,K)=CON1(L,K)  
C PMC        ENDDO  
C PMC      ENDDO  
!$OMP PARALLEL DO PRIVATE(LF,LL,LS)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO K=1,KC  
        DO L=LF,LL
          LS=LSC(L)  
          FUHU(L,K)=0.5*UHDY2(L,K)*(CON(L,K)+CON(L-1,K))  
          FVHU(L,K)=0.5*VHDX2(L,K)*(CON(L,K)+CON(LS,K))  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO L=LF,LL
          FWU(L,K)=0.5*W2(L,K)*(CON(L,K+1)+CON(L,K))  
        ENDDO  
      ENDDO  
c
      enddo

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
C  
C **  STANDARD ADVECTION CALCULATION  
C  
  500 CONTINUE  
C  
C BEGIN IF ON TRANSPORT OPTION CHOICE  
C  
      ! *** CALCULATE AND ADD HORIZONTAL DIFFUSION FLUX (PMC MOVED)
      IF(ISHDMF.EQ.2) CALL CALDIFF (ISTL_,M,CON1)

      ! *** IF ISACAC EQ 0 INCLUDE FQC MASS SOURCES IN UPDATE  
      IF(ISCDCA(MVAR).EQ.0)THEN  
!$OMP PARALLEL DO PRIVATE(LF,LL,RDZIC)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        IF(ISTL_.EQ.2)THEN  
          IF(IDRYTBP.EQ.0)THEN  
            DO K=1,KC  
              RDZIC=DZIC(K)  
              DO L=LF,LL
                CH(L,K)=CON1(L,K)*H1P(L)  
     &              +DELT*( ( RDZIC*FQC(L,K)  
     &              +FUHU(L,K)-FUHU(L+1,K)  
     &              +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)  
     &              +(FWU(L,K-1)-FWU(L,K))*RDZIC )  
             ENDDO  
            ENDDO  
          ELSE  
            DO K=1,KC  
              RDZIC=DZIC(K)  
              DO L=LF,LL
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
          ENDIF  
          IF(ISFCT(MVAR).GE.1.AND.ISADAC(MVAR).GT.0)THEN  ! *** DSLLC SINGLE LINE
            DO K=1,KC  
              DO L=LF,LL
                CON2(L,K)=CON1(L,K)  
              ENDDO  
            ENDDO  
          ENDIF  
C  
C ELSE ON TIME LEVEL CHOICE FOR ISCDCA=0  
C  
        ELSE
          DO K=1,KC  
            RDZIC=DZIC(K)  
            DO L=LF,LL
              CH(L,K)=CON1(L,K)*H2P(L)  
     &            +DELT*( ( RDZIC*FQC(L,K)  
     &            +FUHU(L,K)-FUHU(L+1,K)  
     &            +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)  
     &            +(FWU(L,K-1)-FWU(L,K))*RDZIC )  
            ENDDO  
          ENDDO  
          IF(ISFCT(MVAR).GE.1.AND.ISADAC(MVAR).GT.0)THEN  ! *** DSLLC SINGLE LINE
            DO K=1,KC  
              DO L=LF,LL
                CON2(L,K)=CON(L,K)  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDIF  
c
      enddo
C  
C ENDIF ON TIME LEVEL CHOICE FOR ISCDCA=0  
C  
        IF(ISUD.EQ.1.AND.IS2TL_.EQ.0.AND.MVAR.NE.8)THEN
          DO K=1,KC
            DO IOBC=1,NBCSOP  
              L=LOBCS(IOBC)  
              CON(L,K)=CON1(L,K)  
            ENDDO  
          ENDDO  

!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
          DO K=1,KC
            DO L=LF,LL
              CON1(L,K)=CON(L,K)
            ENDDO  
          ENDDO  
c
      enddo
        ENDIF

        ! *** UPDATE NEW CONCENTRATIONS        
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO K=1,KC  
          DO L=LF,LL
            CON(L,K)=CH(L,K)*HPI(L)  
          ENDDO  
        ENDDO  
c
      enddo
C  
C *** ELSE ON TRANSPORT OPTION CHOICE  
C *** IF ISACAC NE 0 DO NOT INCLUDE FQC MASS SOURCES IN UPDATE  
C
      ELSE  
C  
C BEGIN IF ON TIME LEVEL CHOICE FOR ISCDCA.NE.0  
C  
        IF(ISTL_.EQ.2)THEN  
!$OMP PARALLEL DO PRIVATE(LF,LL,RDZIC)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
          IF(IDRYTBP.EQ.0)THEN  
            DO K=1,KC  
              RDZIC=DZIC(K)  
              DO L=LF,LL
                CH(L,K)=CON1(L,K)*H1P(L)  
     &              +DELT*( ( RDZIC*FQC(L,K)
     &              +FUHU(L,K)-FUHU(L+1,K)  
     &              +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)  
     &              +(FWU(L,K-1)-FWU(L,K))*RDZIC )  
              ENDDO  
            ENDDO  
          ELSE  
            DO K=1,KC  
              RDZIC=DZIC(K)  
              DO L=LF,LL
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
c
      enddo

          IF(ISFCT(MVAR).GE.1)THEN  
            CON2=CON1   ! *** ARRAYS
          ENDIF  
C  
C ELSE ON TIME LEVEL CHOICE FOR ISCDCA.NE.0 AND ISTL.EQ.3
C  
        ELSE  
!$OMP PARALLEL DO PRIVATE(LF,LL,RDZIC)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
          DO K=1,KC  
            RDZIC=DZIC(K)  
            DO L=LF,LL
              CH(L,K)=CON1(L,K)*H2P(L)  
     &            +DELT*( ( RDZIC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)  
     &            +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)  
     &            +(FWU(L,K-1)-FWU(L,K))*RDZIC )  
            ENDDO  
          ENDDO  
c
      enddo

          IF(ISFCT(MVAR).GE.1)THEN  
            CON2=CON    ! *** ARRAYS
          ENDIF  
        ENDIF  
C  
C ENDIF ON TIME LEVEL CHOICE FOR ISCDCA.NE.0  
C  
        IF(ISUD.EQ.1.AND.MVAR.NE.8)THEN  
!$OMP PARALLEL DO PRIVATE(L)
          DO K=1,KC
            DO IOBC=1,NBCSOP  
              L=LOBCS(IOBC)  
              CON(L,K)=CON1(L,K)  
            ENDDO  
          ENDDO  

!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
          DO K=1,KC
            DO L=LF,LL
              CON1(L,K)=CON(L,K)
            ENDDO  
          ENDDO  
c
      enddo
        ENDIF

        ! *** PMC-BOUNDARY CONDITIONS APPLIED BELOW
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO K=1,KC  
          DO L=LF,LL
            CON(L,K)=CH(L,K)*HPI(L)  
          ENDDO  
        ENDDO  
c
      enddo

      ENDIF  
C  
C ENDIF ON TRANSPORT OPTION CHOICE  
C
C *** APPLY OPEN BOUNDARY CONDITIONS, BASED ON DIRECTION OF FLOW
C  
      ! *** ALL OTHER WATER CONSTITUENTS 
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
      
      ! *** SOUTH OPEN BC
      DO K=1,KC  
        DO LL=1,NCBS  
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
        ENDDO  
      ENDDO  
        
      ! *** WEST OPEN BC
      DO K=1,KC  
        DO LL=1,NCBW  
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
        ENDDO  
      ENDDO
      
      ! *** EAST OPEN BC
      DO K=1,KC  
        DO LL=1,NCBE  
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
        ENDDO  
      ENDDO  
      
      ! *** NORTH OPEN BC
      DO K=1,KC  
        DO LL=1,NCBN  
          NSID=NCSERN(LL,M)  
          L=LCBN(LL)  
          LS=LSC(L)  
          IF(VHDX2(L,K).GE.0.)THEN  
            ! *** FLOWING OUT OF DOMAIN
            IF(ISTL_.EQ.2)THEN  
              CTMP=CON1(L,K)+DELT*(FVHU(L,K)  
     &            -VHDX2(L,K)*CON1(L,K))*DXYIP(L)*HPI(L)  
            ELSE  
              IF(ISCDCA(MVAR).NE.2) CTMP=CON1(L,K)+DELT*(FVHU(L,K)  
     &            -VHDX2(L,K)*CON1(L,K))*DXYIP(L)*HPI(L)  
              IF(ISCDCA(MVAR).EQ.2) CTMP=0.5*(CON1(L,K)+CON(L,K))  
     &           +0.5*(CON1(L,K)-CON(L,K))*H2P(L)*HPI(L)+DELT*(FVHU(L,K)  
     &            -0.5*VHDX2(L,K)*(CON1(L,K)+CON(L,K)))*DXYIP(L)*HPI(L)  
              CON1(L,K)=CON(L,K)  
            ENDIF  
            CON(L,K)=CTMP  
            CBNTMP=CBN(LL,1,M)+CSERT(1,NSID,M)  
            IF(M.EQ.1.AND.CON(L,K).GT.CBNTMP) CON(L,K)=CBNTMP  
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
            ELSE  
              CON(L,K)=CLON(LL,K,M)  
     &            +(CBT-CLON(LL,K,M))*FLOAT(NMNLO)/FLOAT(NTSCRN(LL))  
            ENDIF  
          ENDIF  
        ENDDO  
      ENDDO  
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
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO K=1,KC
      DO L=LF,LL
          POS(L,K)=MAX(CON(L,K),0.)
        ENDDO
      ENDDO
c
      enddo
      ! *** PMC END BLOCK

      IF(IDRYTBP.EQ.0)THEN  
C
!$OMP PARALLEL DO PRIVATE(LF,LL,LF_LC,LL_LC,L,K,
!$OMP& RDZIG,LS,AUHU,AVHV)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
         LF_LC=jse_LC(1,ithds)
         LL_LC=jse_LC(2,ithds)
c
        DO K=1,KC  
          IF(LF.eq.2) THEN
             L=1
            UUU(L,K)=0.0
            VVV(L,K)=0.0
          ENDIF
          DO L=LF,LL
            LS=LSC(L)  
            UUU(L,K)=U2(L,K)*(POS(L,K)-POS(L-1,K))*DXIU(L)  
            VVV(L,K)=V2(L,K)*(POS(L,K)-POS(LS,K))*DYIV(L)  
c           AUHU=ABS(UHDY2(L,K))
c           AVHV=ABS(VHDX2(L,K))
c           UTERM0(L,K)=AUHU*(POS(L,K)-POS(L-1,K))
c           VTERM0(L,K)=AVHV*(POS(L,K)-POS(LS,K))
          ENDDO  
          IF(LL.eq.LA) THEN
             L=LC
            UUU(L,K)=0.0
            VVV(L,K)=0.0
          ENDIF
        ENDDO  
           K=0
          DO L=LF_LC,LL_LC
            WWW(L,K)=0.0
          ENDDO
        DO K=1,KS  
          RDZIG=DZIG(K)  
          DO L=LF,LL
            WWW(L,K)=W2(L,K)*(POS(L,K+1)-POS(L,K))*HPI(L)*RDZIG  
          ENDDO  
        ENDDO  
           K=KC
          DO L=LF_LC,LL_LC
            WWW(L,K)=0.0
          ENDDO
c
       enddo
c     t00=rtc()-t00
c     write(6,*) '==>5C',t00*1d6
c     t00=rtc()
       IF(ISADAC(MVAR).GE.2)THEN
!$OMP PARALLEL DO PRIVATE(LF,LL,LF_LC,LL_LC,
!$OMP& RDZIC)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
         LF_LC=jse_LC(1,ithds)
         LL_LC=jse_LC(2,ithds)
c
        DO K=1,KC
          RDZIC=DZIC(K)
          DO L=LF_LC,LL_LC
              SSCORUEWNS(L,K)=DELTA*RDZIC*DXYIP(L)*HPI(L)*(FQCPAD(L,K)
     &            -QSUMPAD(L,K)*CON(L,K))
          ENDDO
          DO L=LF,LL
              SSCORWAB(L,K)=DELTA*DZIG(K)*HPI(L)*DXYIP(L)
     &            *(FQCPAD(L,K  )-QSUMPAD(L,K  )*POS(L,K  ))
          ENDDO
        ENDDO
c
      enddo
       ENDIF

!$OMP PARALLEL DO PRIVATE(LF,LL,
!$OMP& RDZIC,LN,LS,LNW,LSE,
!$OMP& AUHU,AVHV,UTERM,VTERM,SSCORUE,SSCORUW,SSCORVN,SSCORVS,
!$OMP& SSCORU,SSCORV,UHU,VHV,
!$OMP& AWW,WTERM,SSCORWA,SSCORWB,SSCORW,WW)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO K=1,KC  
          RDZIC=DZIC(K)  
          DO L=LF,LL
            LN=LNC(L)  
            LS=LSC(L)  
            LNW=LNWC(L)  
            LSE=LSEC(L)  
            AUHU=ABS(UHDY2(L,K))  
            AVHV=ABS(VHDX2(L,K))  
            UTERM=AUHU*(POS(L,K)-POS(L-1,K))  
            VTERM=AVHV*(POS(L,K)-POS(LS,K))  
c           UTERM=UTERM0(L,K)
c           VTERM=VTERM0(L,K)
            IF(ISADAC(MVAR).GE.2)THEN  
c             SSCORUE=DELTA*RDZIC*DXYIP(L  )*HPI(L  )*(FQCPAD(L  ,K)  
c    &            -QSUMPAD(L  ,K)*CON(L  ,K))  
c             SSCORUW=DELTA*RDZIC*DXYIP(L-1)*HPI(L-1)*(FQCPAD(L-1,K)  
c    &            -QSUMPAD(L-1,K)*CON(L-1,K))  
c             SSCORVN=DELTA*RDZIC*DXYIP(L  )*HPI(L  )*(FQCPAD(L  ,K)  
c    &            -QSUMPAD(L  ,K)*CON(L  ,K))  
c             SSCORVS=DELTA*RDZIC*DXYIP(LS )*HPI(LS )*(FQCPAD(LS ,K)  
c    &            -QSUMPAD(LS ,K)*CON(LS ,K))  
              SSCORUE=SSCORUEWNS(L,K)
              SSCORUW=SSCORUEWNS(L-1,K)
              SSCORVN=SSCORUEWNS(L,K)
              SSCORVS=SSCORUEWNS(LS,K)

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
        DO K=1,KS  
          DO L=LF,LL
            LN=LNC(L)  
            AWW=ABS(W2(L,K))  
            WTERM=AWW*(POS(L,K+1)-POS(L,K))  
            IF(ISADAC(MVAR).GE.2)THEN  
c             SSCORWA=DELTA*DZIG(K+1)*HPI(L)*DXYIP(L)  
c    &            *(FQCPAD(L,K+1)-QSUMPAD(L,K+1)*POS(L,K+1))  
c             SSCORWB=DELTA*DZIG(K)*HPI(L)*DXYIP(L)  
c    &            *(FQCPAD(L,K  )-QSUMPAD(L,K  )*POS(L,K  ))  
              SSCORWA=SSCORWAB(L,K+1)
              SSCORWB=SSCORWAB(L,K)

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
c
        enddo
c     t00=rtc()-t00
c     write(6,*) '==>6C',t00*1d6
c     t00=rtc()
C  
C ** SET ANTIDIFFUSIVE FLUXES TO ZERO FOR SOURCE CELLS
C  
        IF(ISADAC(MVAR).EQ.1)THEN  
          ! *** ANTIDIFFUSION TURNED OFF FOR SOURCE CELLS  
!$OMP PARALLEL DO PRIVATE(LF,LL,L)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
          DO K=1,KC  
            DO L=LF,LL-1
c           DO L=2,LA
              IF(QSUMPAD(L,K).GT.0.0)THEN  
                IF(FUHU(L  ,K).NE.0.) FUHU(L  ,K)=0.  
                IF(FUHU(L+1,K).NE.0.) FUHU(L+1,K)=0.  
                IF(FVHU(L  ,K).NE.0.) FVHU(L  ,K)=0.
                IF(FWU(L,K  ).NE.0.) FWU(L,K  )=0.  
                IF(FWU(L,K-1).NE.0.) FWU(L,K-1)=0.  
              ENDIF  
            ENDDO  
          ENDDO  
      enddo
c
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
          DO K=1,KC
               L=LL
              IF(QSUMPAD(L,K).GT.0.0)THEN
                IF(FUHU(L  ,K).NE.0.) FUHU(L  ,K)=0.  
                IF(FUHU(L+1,K).NE.0.) FUHU(L+1,K)=0.  
                IF(FVHU(L  ,K).NE.0.) FVHU(L  ,K)=0.
                IF(FWU(L,K  ).NE.0.) FWU(L,K  )=0.  
                IF(FWU(L,K-1).NE.0.) FWU(L,K-1)=0.  
              ENDIF
          ENDDO
      enddo


!$OMP PARALLEL DO PRIVATE(LF,LL,LN,ii) 
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
          DO K=1,KC
            ii=0
            DO L=LF,LL
              IF(QSUMPAD(L,K).GT.0.0)THEN
                LN=LNC(L)
                IF(LN.NE.LC) THEN
                 IF(FVHU(LN ,K).NE.0.) FVHU(LN ,K)=0.
                ELSE
                 ii=ii+1 
                ENDIF
              ENDIF
            ENDDO
            icount(ithds,K)=ii
          ENDDO
      enddo
          DO K=1,KC
            ii=0
            do ithds=0,nthds-1
               ii=ii+icount(ithds,K)
            enddo
          if(ii.gt.0) then
             LN=LC
             FVHU(LN ,K)=0.
          endif   
          ENDDO

        ENDIF  
C  
C ** SET ANTIDIFFUSIVE FLUXES TO ZERO FOR OPEN BOUNDARY CELLS  
C  
          DO LL=1,NCBS  
            L=LCBS(LL)  
            LN=LNC(L)  
        DO K=1,KC  
            FVHU(LN,K)=0.0  
        ENDDO  
          ENDDO  
          DO LL=1,NCBW  
            L=LCBW(LL)  
        DO K=1,KC  
            FUHU(L+1,K)=0.0  
        ENDDO  
          ENDDO  
          DO LL=1,NCBE  
            L=LCBE(LL)  
        DO K=1,KC  
            FUHU(L,K)=0.0  
        ENDDO  
          ENDDO  
          DO LL=1,NCBN  
            L=LCBN(LL)  
        DO K=1,KC  
            FVHU(L,K)=0.0  
        ENDDO  
          ENDDO  
C  
C **  CALCULATE AND APPLY FLUX CORRECTED TRANSPORT LIMITERS  
C  
c     t00=rtc()-t00
c     write(6,*) '==>7C',t00*1d6
c     t00=rtc()
        IF(ISFCT(MVAR).EQ.0) GOTO 1100  
C  
C **  DETERMINE MAX AND MIN CONCENTRATIONS  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL,L)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO K=1,KC  
          IF(LF.eq.2) THEN
             L=1
            CONTMX(L,K)=0.0
            CONTMN(L,K)=0.0
          ENDIF
          DO L=LF,LL
            CONTMX(L,K)=MAX(CON(L,K),CON2(L,K))  
            CONTMN(L,K)=MIN(CON(L,K),CON2(L,K))  
          ENDDO  
          IF(LL.eq.LA) THEN
             L=LC
            CONTMX(L,K)=0.0
            CONTMN(L,K)=0.0
          ENDIF
        ENDDO  
c
        enddo
c     t00=rtc()-t00
c     write(6,*) '==>8C',t00*1d6
c     t00=rtc()

!$OMP PARALLEL DO PRIVATE(LF,LL,K,
!$OMP& LS,LN,
!$OMP& CWMAX,CEMAX,CSMAX,CNMAX,CMAXT,CWMIN,CEMIN,CSMIN,CNMIN,CMINT)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO L=LF,LL
          CMAX(L,1)=MAX(CONTMX(L,1),CONTMX(L,2))
          CMAX(L,KC)=MAX(CONTMX(L,KS),CONTMX(L,KC))
          CMIN(L,1)=MIN(CONTMN(L,1),CONTMN(L,2))
          CMIN(L,KC)=MIN(CONTMN(L,KS),CONTMN(L,KC))
        ENDDO
        DO K=2,KS
          DO L=LF,LL
            CMAXT=MAX(CONTMX(L,K-1),CONTMX(L,K+1))
            CMAX(L,K)=MAX(CONTMX(L,K),CMAXT)
            CMINT=MIN(CONTMN(L,K-1),CONTMN(L,K+1))
            CMIN(L,K)=MIN(CONTMN(L,K),CMINT)
          ENDDO
        ENDDO
        DO K=1,KC
          DO L=LF,LL
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

C  
C **  SEPARATE POSITIVE AND NEGATIVE FLUXES PUTTING NEGATIVE FLUXES  
C **  INTO FUHV, FVHV, AND FWV  
C  
        DO K=1,KC  
          DO L=LF,LL
            FUHV(L,K)=MIN(FUHU(L,K),0.)  
            FUHU(L,K)=MAX(FUHU(L,K),0.)  
            FVHV(L,K)=MIN(FVHU(L,K),0.)  
            FVHU(L,K)=MAX(FVHU(L,K),0.)  
          ENDDO  
        ENDDO  
        DO K=1,KS  
          DO L=LF,LL
            FWV(L,K)=MIN(FWU(L,K),0.)  
            FWU(L,K)=MAX(FWU(L,K),0.)  
          ENDDO  
        ENDDO  
c
        enddo
c     t00=rtc()-t00
c     write(6,*) '==>9C',t00*1d6
c     t00=rtc()
C  
C **  CALCULATE INFLUX AND OUTFLUX IN CONCENTRATION UNITS AND LOAD  
C **  INTO DU AND DV, THEN ADJUCT VALUES AT BOUNDARIES  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL,
!$OMP& RDZIC,LN)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO K=1,KC  
          RDZIC=DZIC(K)  
          DO L=LF,LL
            LN=LNC(L)  
            DU(L,K)=DELT*(DXYIP(L)*(FUHU(L,K)-FUHV(L+1,K)  
     &          +FVHU(L,K)-FVHV(LN,K))  
     &          +RDZIC*(FWU(L,K-1)-FWV(L,K)) )*HPI(L)  
            DV(L,K)=DELT*(DXYIP(L)*(FUHU(L+1,K)-FUHV(L,K)  
     &          +FVHU(LN,K)-FVHV(L,K))  
     &          +RDZIC*(FWU(L,K)-FWV(L,K-1)) )*HPI(L)  
          ENDDO  
        ENDDO  
c
        enddo

c     t00=rtc()-t00
c     write(6,*) '==>10C',t00*1d6
c     t00=rtc()
          DO IOBC=1,NBCSOP  
            L=LOBCS(IOBC)  
        DO K=1,KC
            DU(L,K)=0.  
            DV(L,K)=0.  
          ENDDO  
        END DO
          DO LL=1,NCBS  
            L=LCBS(LL)  
            LN=LNC(L)  
        DO K=1,KC  
            DU(LN,K)=0.  
            DV(LN,K)=0.  
          ENDDO  
        ENDDO  
          DO LL=1,NCBW  
            L=LCBW(LL)  
        DO K=1,KC  
            DU(L+1,K)=0.  
            DV(L+1,K)=0.  
          ENDDO  
        ENDDO  
          DO LL=1,NCBE  
            L=LCBE(LL)  
            DU(L-1,K)=0.  
        DO K=1,KC  
            DV(L-1,K)=0.  
          ENDDO  
        ENDDO  
          DO LL=1,NCBN  
            L=LCBN(LL)  
            LS=LSC(L)  
        DO K=1,KC  
            DU(LS,K)=0.  
            DV(LS,K)=0.  
          ENDDO  
        ENDDO  
C  
C **  CALCULATE BETA COEFFICIENTS WITH BETAUP AND BETADOWN IN DU AND DV  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL,BB)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO K=1,KC  
          DO L=LF,LL
          IF(DU(L,K).GT.0.) THEN
          IF((CMAX(L,K)-POS(L,K)).LT.(DU(L,K)+BSMALL)) THEN
            BB=(CMAX(L,K)-POS(L,K))/(DU(L,K)+BSMALL)
          ELSE
            BB=1.
          ENDIF  
          ELSE
           BB=MIN(DU(L,K),1.)
          ENDIF
          DU(L,K)=BB
c         IF(DU(L,K).GT.0.)DU(L,K)=(CMAX(L,K)-POS(L,K))/(DU(L,K)+BSMALL)  
c           DU(L,K)=MIN(DU(L,K),1.)  
c          if(BB.ne.DU(L,K)) THEN
c     cc     write(6,*) BB,DU(L,K)
c            stop 10
c          endif
          IF(DV(L,K).GT.0.) THEN
          IF((CON(L,K)-CMIN(L,K)).LT.(DV(L,K)+BSMALL)) THEN
            BB=(CON(L,K)-CMIN(L,K))/(DV(L,K)+BSMALL)
          ELSE
            BB=1.
          ENDIF
          ELSE
           BB=MIN(DV(L,K),1.)
          ENDIF
          DV(L,K)=BB

c         IF(DV(L,K).GT.0.)DV(L,K)=(CON(L,K)-CMIN(L,K))/(DV(L,K)+BSMALL)  
c           DV(L,K)=MIN(DV(L,K),1.)  
c          if(BB.ne.DV(L,K)) THEN
c     cc     write(6,*) BB,DV(L,K)
c            stop 10
c          endif

          ENDDO  
        ENDDO  
c
      enddo
C  
c     t00=rtc()-t00
c     write(6,*) '==>11C',t00*1d6
c     t00=rtc()
C **  LIMIT FLUXES  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL,
!$OMP& LS)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO K=1,KC  
          DO L=LF,LL
            LS=LSC(L)  
            FUHU(L,K)=MIN(DV(L-1,K),DU(L,K))*FUHU(L,K)  
     &          +MIN(DU(L-1,K),DV(L,K))*FUHV(L,K)  
            FVHU(L,K)=MIN(DV(LS,K),DU(L,K))*FVHU(L,K)  
     &          +MIN(DU(LS,K),DV(L,K))*FVHV(L,K)  
          ENDDO  
        ENDDO  
        DO K=1,KS  
          DO L=LF,LL
            FWU(L,K)=MIN(DV(L,K),DU(L,K+1))*FWU(L,K)  
     &          +MIN(DU(L,K),DV(L,K+1))*FWV(L,K)  
          ENDDO  
        ENDDO  
c
      enddo
C  
C **  ANTI-DIFFUSIVE ADVECTION CALCULATION  
C  
 1100   CONTINUE  
C    
c     t00=rtc()-t00
c     write(6,*) '==>12C',t00*1d6
c     t00=rtc()
!$OMP PARALLEL DO PRIVATE(LF,LL,
!$OMP& RDZIC)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO K=1,KC  
          RDZIC=DZIC(K)  
          DO L=LF,LL
            CH(L,K)=CON(L,K)*HP(L)  
     &          +DELT*( (FUHU(L,K)-FUHU(L+1,K)  
     &          +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)  
     &          +(FWU(L,K-1)-FWU(L,K))*RDZIC )  
            CON(L,K)=SCB(L)*CH(L,K)*HPI(L)+(1.-SCB(L))*CON(L,K)  
          ENDDO  
        ENDDO  
c
      enddo
C  
C **  ADD REMAINING SEDIMENT SETTLING AND FLUX  
C  
      ENDIF  
c     t00=rtc()-t00
c     write(6,*) '==>13C',t00*1d6
C  
C **  ANTI-DIFFUSIVE ADVECTIVE FLUX CALCULATION WITH DRY BYPASS  
C  
      IF(IDRYTBP.GT.0)THEN  
c     t00=rtc()
        ! *** DSLLC BEGIN 
        DO L=1,LC  
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
          DO L=2,LA  
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
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
            WWW(L,K)=W2(L,K)*(POS(L,K+1)-POS(L,K))*HPI(L)*RDZIG  
            ELSE
              WWW(L,K)=0.0
            ENDIF
          ENDDO  
        ENDDO  
C
        DO K=1,KC  
          RDZIC=DZIC(K)  
          DO L=2,LA  
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
        DO K=1,KS  
          DO L=2,LA  
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
            DO L=2,LA
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
C  
C ** SET ANTIDIFFUSIVE FLUXES TO ZERO FOR OPEN BOUNDARY CELLS  
C  
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
C  
C **  CALCULATE AND APPLY FLUX CORRECTED TRANSPORT LIMITERS  
C  
        IF(ISFCT(MVAR).EQ.0) GOTO 1101  
C  
C **  DETERMINE MAX AND MIN CONCENTRATIONS  
C  
        DO K=1,KC  
          DO L=2,LA  
            CMIN(L,K)=0.  
            CMAX(L,K)=0.  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
            CONTMX(L,K)=MAX(CON(L,K),CON2(L,K))  
            CONTMN(L,K)=MIN(CON(L,K),CON2(L,K))  
            ENDIF  
          ENDDO  
        ENDDO  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
          CMAX(L,1)=MAX(CONTMX(L,1),CONTMX(L,2))  
          CMAX(L,KC)=MAX(CONTMX(L,KS),CONTMX(L,KC))  
          CMIN(L,1)=MIN(CONTMN(L,1),CONTMN(L,2))  
          CMIN(L,KC)=MIN(CONTMN(L,KS),CONTMN(L,KC))  
          ENDIF  
        ENDDO  
        DO K=2,KS  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
            CMAXT=MAX(CONTMX(L,K-1),CONTMX(L,K+1))  
            CMAX(L,K)=MAX(CONTMX(L,K),CMAXT)  
            CMINT=MIN(CONTMN(L,K-1),CONTMN(L,K+1))  
            CMIN(L,K)=MIN(CONTMN(L,K),CMINT)  
            ENDIF  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
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
C  
C **  SEPARATE POSITIVE AND NEGATIVE FLUXES PUTTING NEGATIVE FLUXES  
C **  INTO FUHV, FVHV, AND FWV  
C  
        DO K=1,KC  
          DO L=2,LA  
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
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              FWV(L,K)=MIN(FWU(L,K),0.)  
              FWU(L,K)=MAX(FWU(L,K),0.)  
            ELSE
              FWV(L,K)=0.  
            ENDIF  
          ENDDO  
        ENDDO  
C  
C **  CALCULATE INFLUX AND OUTFLUX IN CONCENTRATION UNITS AND LOAD  
C **  INTO DU AND DV, THEN ADJUCT VALUES AT BOUNDARIES  
C  
        DO K=1,KC  
          RDZIC=DZIC(K)  
          DO L=2,LA  
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
C  
C **  CALCULATE BETA COEFFICIENTS WITH BETAUP AND BETADOWN IN DU AND DV  
C  
        DO K=1,KC  
          DO L=2,LA  
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
C  
C **  LIMIT FLUXES  
C  
        DO K=1,KC  
          DO L=2,LA  
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
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              FWU(L,K)=MIN(DV(L,K),DU(L,K+1))*FWU(L,K)  
     &            +MIN(DU(L,K),DV(L,K+1))*FWV(L,K)  
            ENDIF  
          ENDDO  
        ENDDO  
C  
C **  END OF ANTI-DIFFUSIVE ADVECTION CALCULATION
C  
 1101   CONTINUE  
C  
        DO K=1,KC  
          RDZIC=DZIC(K)  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN
              CH(L,K)=CON(L,K)*HP(L)  
     &            +DELT*( (FUHU(L,K)-FUHU(L+1,K)  
     &            +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)  
     &            +(FWU(L,K-1)-FWU(L,K))*RDZIC )  
              CON(L,K)=SCB(L)*CH(L,K)*HPI(L)+(1.-SCB(L))*CON(L,K)
            ENDIF  
          ENDDO  
        ENDDO  
C  
C **  ADD REMAINING SEDIMENT SETTLING AND FLUX  
C  
      ENDIF  
C  
C **  DIAGNOSE FCT SCHEME  
C  
      IF(ISFCT(MVAR).EQ.99)THEN  
        WRITE(6,6110)N  
        DO K=1,KC  
          DO L=2,LA  
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
 6110 FORMAT('  FCT DIAGNOSTICS AT N = ',I5)  
 6111 FORMAT('  CON = ',E12.4,3X,'CMAX = ',E12.4,3X,'I,J,K=',(3I10))  
 6112 FORMAT('  CMIN = ',E12.4,3X,'CON = ',E12.4,3X,'I,J,K=',(3I10)) 
 
      ! *** ZERO HEAT FLUXES
 2000 IF(MVAR.EQ.2)THEN        
c     t00=rtc()
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_LC(1,ithds)
         LL=jse_LC(2,ithds)
c
        ! *** ZERO EVAP/RAINFALL
        DO L=LF,LL 
          FQC(L,KC)=0.  
        ENDDO  
        IF(ISADAC(MVAR).GE.2)THEN
          DO L=LF,LL 
            FQCPAD(L,KC)=0.  
          ENDDO  
        ENDIF
        IF(ISADAC(MVAR).GT.0)THEN
          DO L=LF,LL 
            QSUMPAD(L,KC)=0.  
          ENDDO  
        ENDIF
c
      enddo
      ENDIF
      DEALLOCATE(UTERM0) 
      DEALLOCATE(VTERM0) 
      DEALLOCATE(SSCORUEWNS) 
      DEALLOCATE(SSCORWAB) 
      
      
      RETURN  
      END  
