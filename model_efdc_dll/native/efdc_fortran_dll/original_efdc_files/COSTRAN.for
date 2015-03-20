      SUBROUTINE COSTRAN (ISTL_,IS2TL_,MVAR,M,CON,CON1)  
C  
C CHANGE RECORD  
C  ADDED DYNAMIC TIME STEPPING  
C **  SUBROUTINE COSTRAN CALCULATES THE ADVECTIVE  
C **  TRANSPORT OF DISSOLVED OR SUSPENDED CONSITITUENT M LEADING TO  
C **  A NEW VALUE AT TIME LEVEL (N+1). THE VALUE OF ISTL INDICATES  
C **  THE NUMBER OF TIME LEVELS IN THE STEP  
C  
      USE GLOBAL  
C
      DIMENSION CON(LCM,KCM),CON1(LCM,KCM)  
C
      !*** DSLLC BEGIN
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCX  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCXY  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCXYZ  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCXZ  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCY  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCYX  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCYZ  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCYZX  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCZ  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCZX  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCZXY  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONCZY  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONTMN  
      REAL,ALLOCATABLE,DIMENSION(:,:)::CONTMX  
      REAL,ALLOCATABLE,DIMENSION(:,:)::DELCX  
      REAL,ALLOCATABLE,DIMENSION(:,:)::DELCY  
      REAL,ALLOCATABLE,DIMENSION(:,:)::DELCZ  
      REAL,ALLOCATABLE,DIMENSION(:,:)::FQCPAD  
      REAL,ALLOCATABLE,DIMENSION(:,:)::QSUMNAD  
      REAL,ALLOCATABLE,DIMENSION(:,:)::QSUMPAD  

      IF(.NOT.ALLOCATED(CONCX))THEN
		ALLOCATE(CONCX(LCM,KCM))
		ALLOCATE(CONCXY(LCM,KCM))
		ALLOCATE(CONCXYZ(LCM,KCM))
		ALLOCATE(CONCXZ(LCM,KCM))
		ALLOCATE(CONCY(LCM,KCM))
		ALLOCATE(CONCYX(LCM,KCM))
		ALLOCATE(CONCYZ(LCM,KCM))
		ALLOCATE(CONCYZX(LCM,KCM))
		ALLOCATE(CONCZ(LCM,KCM))
		ALLOCATE(CONCZX(LCM,KCM))
		ALLOCATE(CONCZXY(LCM,KCM))
		ALLOCATE(CONCZY(LCM,KCM))
		ALLOCATE(CONTMN(LCM,KCM))
		ALLOCATE(CONTMX(LCM,KCM))
		ALLOCATE(DELCX(LCM,KCM))
		ALLOCATE(DELCY(LCM,KCM))
		ALLOCATE(DELCZ(LCM,KCM))
		ALLOCATE(FQCPAD(LCM,KCM))
		ALLOCATE(QSUMNAD(LCM,KCM))
		ALLOCATE(QSUMPAD(LCM,KCM))
          ! *** INITIALIZE LOCAL ARRAYS
	    CONCX=0.0 
	    CONCXY=0.0 
	    CPNCXYZ=0.0 
	    CONCXZ=0.0 
	    CONCY=0.0 
	    CONCYX=0.0 
	    CONCYZ=0.0 
	    CONCYZX=0.0 
	    CONCZ=0.0 
	    CONCZX=0.0 
	    CONCZXY=0.0 
	    CONCZY=0.0 
	    CONTMN=0.0 
	    CONTMX=0.0 
	    DELCX=0.0 
	    DELCY=0.0 
	    DELCZ=0.0 
	    FQCPAD=0.0 
	    QSUMNAD=0.0     ! *** NOT USED
	    QSUMPAD=0.0 
	ENDIF
C  
      BSMALL=1.0E-6  
      IF(ISDYNSTP.EQ.0)THEN  
        DELT=DT2  
        DELTA=DT2  
        IF(ISCDCA(MVAR).EQ.2) DELTA=DT  
        DELTD2=DT  
        ISUD=1  
        IF(ISTL_.NE.3)THEN  
          DELT=DT  
          DELTA=DT  
          DELTD2=0.5*DT  
          ISUD=0  
        ENDIF  
      ELSE  
        DELT=DTDYN  
        DELTA=DTDYN  
        DELTD2=0.5*DTDYN  
        ISUD=0  
      END IF  
      DELTA4=0.25*DELTA  
      DO K=1,KC  
        DO L=1,LC  
          FUHU(L,K)=0.  
          FUHV(L,K)=0.  
          FVHU(L,K)=0.  
          FVHV(L,K)=0.  
          UUU(L,K)=0.  
          VVV(L,K)=0.  
          DU(L,K)=0.  
          DV(L,K)=0.  
        ENDDO  
      ENDDO  
      IF(IS2TL_.EQ.1)THEN  
        ISUD=1  
        DO K=1,KC  
          DO L=1,LC  
            CON1(L,K)=CON(L,K)  
          ENDDO  
        ENDDO  
      ENDIF  
      DO K=1,KC  
        DO L=1,LC  
          CONT(L,K)=0.  
          CMAX(L,K)=0.  
          CMIN(L,K)=0.  
        ENDDO  
      ENDDO  
C  
C **  CALCULATED EXTERNAL SOURCES AND SINKS  
C  
      CALL CALFQC (ISTL_,IS2TL_,MVAR,M,CON,CON1,FQCPAD,QSUMPAD,QSUMNAD)  
C  
C **  BEGIN COMBINED ADVECTION SCHEME  
C **  INTERMEDIATE ADVECTION CALCULATIONS  
C **  CX,CY,CZ  
C  
      DO K=1,KC  
        DO L=2,LA  
          DELCX(L,K)=CON1(L,K)-CON1(L-1   ,K)  
          DELCY(L,K)=CON1(L,K)-CON1(LSC(L),K)  
        ENDDO  
      ENDDO  
      IF(KC.GE.2)THEN  
        DO K=1,KS  
          DO L=2,LA  
            DELCZ(L,K)=CON1(L,K+1)-CON1(L,K)  
          ENDDO  
        ENDDO  
      ENDIF  
      DO K=1,KC  
        DO L=2,LA  
          CONCX(L,K)=CON1(L,K)+RCOSMICX(L,K)*  
     &        ( COSMICXP(L  ,K)*DELCX(L  ,K)  
     &        +COSMICXN(L+1,K)*DELCX(L+1,K) )  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          CONCY(L,K)=CON1(L,K)+RCOSMICY(L,K)*  
     &        ( COSMICYP(L     ,K)*DELCY(L     ,K)  
     &        +COSMICYN(LNC(L),K)*DELCY(LNC(L),K) )  
        ENDDO  
      ENDDO  
      IF(KC.EQ.1)THEN  
        DO L=2,LA  
          CONCZ(L,1)=CON1(L,1)  
        ENDDO  
      ENDIF  
      IF(KC.GE.2)THEN  
        DO L=2,LA  
          CONCZ(L,1)=CON1(L,1)  
     &        +RCOSMICZ(L,1)*(COSMICZN(L,1)*DELCZ(L,1) )  
        ENDDO  
        DO L=2,LA  
          CONCZ(L,KC)=CON1(L,KC)  
     &        +RCOSMICZ(L,KC)*(COSMICZP(L,KS)*DELCZ(L,KS) )  
        ENDDO  
      ENDIF  
      IF(KC.GE.3)THEN  
        DO K=2,KS  
          DO L=2,LA  
            CONCZ(L,K)=CON1(L,K)+RCOSMICZ(L,K)*  
     &          ( COSMICZP(L,K-1)*DELCZ(L,K-1)  
     &          +COSMICZN(L  ,K)*DELCZ(L,K  ) )  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  INTERMEDIATE ADVECTION CALCULATIONS  
C **  CXY,CXZ  
C  
      DO K=1,KC  
        DO L=2,LA  
          DELCY(L,K)=CONCX(L,K)-CONCX(LSC(L),K)  
        ENDDO  
      ENDDO  
      IF(KC.GE.2)THEN  
        DO K=1,KS  
          DO L=2,LA  
            DELCZ(L,K)=CONCX(L,K+1)-CONCX(L,K)  
          ENDDO  
        ENDDO  
      ENDIF  
      DO K=1,KC  
        DO L=2,LA  
          CONCXY(L,K)=CONCX(L,K)+RCOSMICY(L,K)*  
     &        ( COSMICYP(L     ,K)*DELCY(L     ,K)  
     &        +COSMICYN(LNC(L),K)*DELCY(LNC(L),K) )  
        ENDDO  
      ENDDO  
      IF(KC.EQ.1)THEN  
        DO L=2,LA  
          CONCXZ(L,1)=CONCX(L,1)  
        ENDDO  
      ENDIF  
      IF(KC.GE.2)THEN  
        DO L=2,LA  
          CONCXZ(L,1)=CONCX(L,1)  
     &        +RCOSMICZ(L,1)*(COSMICZN(L,1)*DELCZ(L,1) )  
        ENDDO  
        DO L=2,LA  
          CONCXZ(L,KC)=CONCX(L,KC)  
     &        +RCOSMICZ(L,KC)*(COSMICZP(L,KS)*DELCZ(L,KS) )  
        ENDDO  
      ENDIF  
      IF(KC.GE.3)THEN  
        DO K=2,KS  
          DO L=2,LA  
            CONCXZ(L,K)=CONCX(L,K)+RCOSMICZ(L,K)*  
     &          ( COSMICZP(L,K-1)*DELCZ(L,K-1)  
     &          +COSMICZN(L  ,K)*DELCZ(L,K  ) )  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  INTERMEDIATE ADVECTION CALCULATIONS  
C **  CYZ,CYX  
C  
      DO K=1,KC  
        DO L=2,LA  
          DELCX(L,K)=CONCY(L,K)-CONCY(L-1,K)  
        ENDDO  
      ENDDO  
      IF(KC.GE.2)THEN  
        DO K=1,KS  
          DO L=2,LA  
            DELCZ(L,K)=CONCY(L,K+1)-CONCY(L,K)  
          ENDDO  
        ENDDO  
      ENDIF  
      DO K=1,KC  
        DO L=2,LA  
          CONCYX(L,K)=CONCY(L,K)+RCOSMICX(L,K)*  
     &        ( COSMICXP(L  ,K)*DELCX(L  ,K)  
     &        +COSMICXN(L+1,K)*DELCX(L+1,K) )  
        ENDDO  
      ENDDO  
      IF(KC.EQ.1)THEN  
        DO L=2,LA  
          CONCZ(L,1)=CONCY(L,1)  
        ENDDO  
      ENDIF  
      IF(KC.GE.2)THEN  
        DO L=2,LA  
          CONCYZ(L,1)=CONCY(L,1)  
     &        +RCOSMICZ(L,1)*(COSMICZN(L,1)*DELCZ(L,1) )  
        ENDDO  
        DO L=2,LA  
          CONCYZ(L,KC)=CONCY(L,KC)  
     &        +RCOSMICZ(L,KC)*(COSMICZP(L,KS)*DELCZ(L,KS) )  
        ENDDO  
      ENDIF  
      IF(KC.GE.3)THEN  
        DO K=2,KS  
          DO L=2,LA  
            CONCYZ(L,K)=CONCY(L,K)+RCOSMICZ(L,K)*  
     &          ( COSMICZP(L,K-1)*DELCZ(L,K-1)  
     &          +COSMICZN(L  ,K)*DELCZ(L,K  ) )  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  INTERMEDIATE ADVECTION CALCULATIONS  
C **  CZX,CZY  
C  
      DO K=1,KC  
        DO L=2,LA  
          DELCX(L,K)=CONCZ(L,K)-CONCZ(L-1   ,K)  
          DELCY(L,K)=CONCZ(L,K)-CONCZ(LSC(L),K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          CONCZX(L,K)=CONCZ(L,K)+RCOSMICX(L,K)*  
     &        ( COSMICXP(L  ,K)*DELCX(L  ,K)  
     &        +COSMICXN(L+1,K)*DELCX(L+1,K) )  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          CONCZY(L,K)=CONCZ(L,K)+RCOSMICY(L,K)*  
     &        ( COSMICYP(L     ,K)*DELCY(L     ,K)  
     &        +COSMICYN(LNC(L),K)*DELCY(LNC(L),K) )  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          CONCXYZ(L,K)=( 2.*CON1(L,K)+CONCY(L,K)+CONCYZ(L,K)  
     &        +CONCZ(L,K)+CONCZY(L,K) )/6.  
          CONCYZX(L,K)=( 2.*CON1(L,K)+CONCZ(L,K)+CONCZX(L,K)  
     &        +CONCX(L,K)+CONCXZ(L,K) )/6.  
          CONCZXY(L,K)=( 2.*CON1(L,K)+CONCY(L,K)+CONCYX(L,K)  
     &        +CONCX(L,K)+CONCXY(L,K) )/6.  
        ENDDO  
      ENDDO  
C  
C **  ADVECTIVE FLUX CALCULATION  
C **  CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE WITH ADVECTION  
C **  AVERAGED BETWEEN (N) AND (N+1) OR (N-1) AND (N+1) AND ADVECTED  
C **  AT (N) OR (N-1) IF ISTL EQUALS 2 OR 3 RESPECTIVELY  
C  
      DO K=1,KC  
        DO L=2,LA  
          FUHU(L,K)=MAX(UHDY2(L,K),0.)*CONCXYZ(L-1,K)  
     &        +MIN(UHDY2(L,K),0.)*CONCXYZ(L,K)  
          FVHU(L,K)=MAX(VHDX2(L,K),0.)*CONCYZX(LSC(L),K)  
     &        +MIN(VHDX2(L,K),0.)*CONCYZX(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO L=2,LA  
          FWU(L,K)=MAX(W2(L,K),0.)*CONCZXY(L,K)  
     &        +MIN(W2(L,K),0.)*CONCZXY(L,K+1)  
        ENDDO  
      ENDDO  
C  
C **  STANDARD ADVECTION CALCULATION  
C **  IF ISACAC EQ 0 INCLUDE FQC MASS SOURCES IN UPDATE  
C BEGIN IF ON TRANSPORT OPTION CHOICE  
C  
      IF(ISCDCA(MVAR).EQ.0.)THEN  
C  
C BEGIN IF ON TIME LEVEL CHOICE FOR ISCDCA=0  
C  
        IF(ISTL_.EQ.2)THEN  
          DO K=1,KC  
            RDZIC=DZIC(K)  
            DO L=2,LA  
              CH(L,K)=CON1(L,K)*H1P(L)  
     &            +DELT*( ( RDZIC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)  
     &            +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)  
     &            +(FWU(L,K-1)-FWU(L,K))*RDZIC )  
            ENDDO  
          ENDDO  
          IF(ISFCT(MVAR).GE.1)THEN  
            DO K=1,KC  
              DO L=2,LA  
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
            DO L=2,LA  
              CH(L,K)=CON1(L,K)*H2P(L)  
     &            +DELT*( ( RDZIC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)  
     &            +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)  
     &            +(FWU(L,K-1)-FWU(L,K))*RDZIC )  
            ENDDO  
          ENDDO  
          IF(ISFCT(MVAR).GE.1)THEN  
            DO K=1,KC  
              DO L=2,LA  
                CON2(L,K)=CON(L,K)  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDIF  
C  
C ENDIF ON TIME LEVEL CHOICE FOR ISCDCA=0  
C  
        IF(ISUD.EQ.1)THEN  
          DO K=1,KC  
            DO L=2,LA  
              CON1(L,K)=SCB(L)*CON(L,K)+(1.-SCB(L))*CON1(L,K)  
            ENDDO  
          ENDDO  
        ENDIF  
        DO K=1,KC  
          DO L=2,LA  
            CON(L,K)=SCB(L)*CH(L,K)*HPI(L)+(1.-SCB(L))*CON(L,K)  
            CONT(L,K)=0.0  
          ENDDO  
        ENDDO  
C  
C **  ADD REMAINING SEDIMENT SETTLING AND FLUX  
C **  IF ISACAC NE 0 DO NOT INCLUDE FQC MASS SOURCES IN UPDATE  
C ELSE ON TRANSPORT OPTION CHOICE  
C  
      ELSE  
C  
C BEGIN IF ON TIME LEVEL CHOICE FOR ISCDCA.NE.0  
C  
        IF(ISTL_.EQ.2)THEN  
          DO K=1,KC  
            RDZIC=DZIC(K)  
            DO L=2,LA  
              CH(L,K)=CON1(L,K)*H1P(L)  
     &            +DELT*( ( RDZIC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)  
     &            +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)  
     &            +(FWU(L,K-1)-FWU(L,K))*RDZIC )  
            ENDDO  
          ENDDO  
          IF(ISFCT(MVAR).GE.1)THEN  
            DO K=1,KC  
              DO L=2,LA  
                CON2(L,K)=CON1(L,K)  
              ENDDO  
            ENDDO  
          ENDIF  
C  
C ELSE ON TIME LEVEL CHOICE FOR ISCDCA.NE.0  
C  
        ELSE  
          DO K=1,KC  
            RDZIC=DZIC(K)  
            DO L=2,LA  
              CH(L,K)=CON1(L,K)*H2P(L)  
     &            +DELT*( ( RDZIC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)  
     &            +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)  
     &            +(FWU(L,K-1)-FWU(L,K))*RDZIC )  
            ENDDO  
          ENDDO  
          IF(ISFCT(MVAR).GE.1)THEN  
            DO K=1,KC  
              DO L=2,LA  
                CON2(L,K)=CON(L,K)  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDIF  
C  
C ENDIF ON TIME LEVEL CHOICE FOR ISCDCA.NE.0  
C  
        IF(ISUD.EQ.1)THEN  
          DO K=1,KC  
            DO L=2,LA  
              CON1(L,K)=SCB(L)*CON(L,K)+(1.-SCB(L))*CON1(L,K)  
            ENDDO  
          ENDDO  
        ENDIF  
        DO K=1,KC  
          DO L=2,LA  
            CON(L,K)=SCB(L)*CH(L,K)*HPI(L)+(1.-SCB(L))*CON(L,K)  
            CONT(L,K)=0.0  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C ENDIF ON TRANSPORT OPTION CHOICE  
C **  CALCULATE LAST OUTFLOWING CONCENTRATION OR SPECIFY INFLOW  
C **  CONCENTRATION AT OPEN BOUNDARIES  
C  
      DO K=1,KC  
        DO LL=1,NCBS  
          NSID=NCSERS(LL,M)  
          L=LCBS(LL)  
          LN=LNC(L)  
          IF(VHDX2(LN,K).LT.0.)THEN  
            IF(ISTL_.EQ.2)THEN  
              CTMP=CON1(L,K)+DELT*(VHDX2(LN,K)*CON1(L,K)  
     &            -FVHU(LN,K))*DXYIP(L)*HPI(L)  
            ELSE  
              IF(ISCDCA(MVAR).NE.2)CTMP=CON1(L,K)+DELT*(VHDX2(LN,K)
     &      *CON1(L,K)-FVHU(LN,K))*DXYIP(L)*HPI(L)  
              IF(ISCDCA(MVAR).EQ.2) CTMP=0.5*(CON1(L,K)+CON(L,K))  
     &            +0.5*(CON1(L,K)-CON(L,K))*H2P(L)*HPI(L)  
     &            +DELT*(0.5*VHDX2(LN,K)*(CON1(L,K)+CON(L,K))  
     &            -FVHU(LN,K))*DXYIP(L)*HPI(L)  
              CON1(L,K)=CON(L,K)  
            ENDIF  
            CON(L,K)=CTMP  
            CBSTMP=CBS(LL,1,M)+CSERT(1,NSID,M)  
            IF(M.EQ.1.AND.CON(L,K).GT.CBSTMP) CON(L,K)=CBSTMP  
            CLOS(LL,K,M)=CON(L,K)  
            NLOS(LL,K,M)=N  
          ELSE  
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
 6001 FORMAT('N,K,CBTS = ',2I10,F12.3)  
      DO K=1,KC  
        DO LL=1,NCBW  
          NSID=NCSERW(LL,M)  
          L=LCBW(LL)  
          IF(UHDY2(L+1,K).LT.0.)THEN  
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
      DO K=1,KC  
        DO LL=1,NCBE  
          NSID=NCSERE(LL,M)  
          L=LCBE(LL)  
          IF(UHDY2(L,K).GT.0.)THEN  
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
      DO K=1,KC  
        DO LL=1,NCBN  
          NSID=NCSERN(LL,M)  
          L=LCBN(LL)  
          LS=LSC(L)  
          IF(VHDX2(L,K).GT.0.)THEN  
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
 6002 FORMAT('N,K,CBTN = ',2I10,F12.3)  
C  
C **  MODIFIY VERTICAL MASS DIFFUSION IF ANTI-DIFFUSIVE ADVECTIVE  
C **  IS TURNED OFF  
C **  ANTI-DIFFUSIVE ADVECTIVE FLUX CALCULATION  
C  
      IF(ISADAC(MVAR).EQ.0) RETURN  
      IF(ISCDCA(MVAR).EQ.1) RETURN  
C  
C **  STANDARD ANTI-DIFFUSIVE ADVECTIVE FLUX CALCULATION  
C  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L)  
          LS=LSC(L)  
          LNW=LNWC(L)  
          LSE=LSEC(L)  
          AUHU=ABS(UHDY2(L,K))  
          AVHV=ABS(VHDX2(L,K))  
        UTERM=AUHU*(1.-DELTA*AUHU*DXYIU(L)*HUI(L))*(CON(L,K)-CON(L-1,K))  
         VTERM=AVHV*(1.-DELTA*AVHV*DXYIV(L)*HVI(L))*(CON(L,K)-CON(LS,K))  
          IF(ISADAC(MVAR).GE.2)THEN  
            SSCORUE=DELTA*RDZIC*DXYIP(L  )*HPI(L  )*(FQCPAD(L  ,K)  
     &          -QSUMPAD(L  ,K)*CON(L  ,K))  
            SSCORUW=DELTA*RDZIC*DXYIP(L-1)*HPI(L-1)*(FQCPAD(L-1,K)  
     &          -QSUMPAD(L-1,K)*CON(L-1,K))  
            SSCORVN=DELTA*RDZIC*DXYIP(L  )*HPI(L  )*(FQCPAD(L  ,K)  
     &          -QSUMPAD(L  ,K)*CON(L  ,K))  
            SSCORVS=DELTA*RDZIC*DXYIP(LS )*HPI(LS )*(FQCPAD(LS ,K)  
     &          -QSUMPAD(LS ,K)*CON(LS ,K))  
          SSCORU=MAX(UHDY2(L,K),0.0)*SSCORUW+MIN(UHDY2(L,K),0.0)*SSCORUE  
          SSCORV=MAX(VHDX2(L,K),0.0)*SSCORVS+MIN(VHDX2(L,K),0.0)*SSCORVN  
            UTERM=UTERM+SSCORU  
            VTERM=VTERM+SSCORV  
          ENDIF  
          IF(ISFCT(MVAR).GE.2)THEN  
            FUHU(L,K)=0.5*UTERM  
            FVHU(L,K)=0.5*VTERM  
            IF(ISFCT(MVAR).EQ.3)THEN  
              FUHU(L,K)=UTERM  
              FVHU(L,K)=VTERM  
            ENDIF  
          ELSE  
            UHU=UTERM/(CON(L,K)+CON(L-1,K)+BSMALL)  
            VHV=VTERM/(CON(L,K)+CON(LS,K)+BSMALL)  
            FUHU(L,K)=MAX(UHU,0.)*CON(L-1,K)  
     &          +MIN(UHU,0.)*CON(L,K)  
            FVHU(L,K)=MAX(VHV,0.)*CON(LS,K)  
     &          +MIN(VHV,0.)*CON(L,K)  
          ENDIF  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO L=2,LA  
          LN=LNC(L)  
          AWW=ABS(W2(L,K))  
          WTERM=AWW*(1.-DELTA*AWW*DZIG(K)*HPI(L))*(CON(L,K+1)-CON(L,K))  
          IF(ISADAC(MVAR).GE.2)THEN  
            SSCORWA=DELTA*DZIG(K+1)*HPI(L)*DXYIP(L)  
     &          *(FQCPAD(L,K+1)-QSUMPAD(L,K+1)*CON(L,K+1))  
            SSCORWB=DELTA*DZIG(K)*HPI(L)*DXYIP(L)  
     &          *(FQCPAD(L,K  )-QSUMPAD(L,K  )*CON(L,K  ))  
            SSCORW=MAX(W2(L,K),0.0)*SSCORWB+MIN(W2(L,K),0.0)*SSCORWA  
            WTERM=WTERM+SSCORW  
          ENDIF  
          IF(ISFCT(MVAR).GE.2)THEN  
            FWU(L,K)=0.5*WTERM  
            IF(ISFCT(MVAR).EQ.3)THEN  
              FWU(L,K)=WTERM  
            ENDIF  
          ELSE  
            WW=WTERM/(CON(L,K+1)+CON(L,K)+BSMALL)  
            FWU(L,K)=MAX(WW,0.)*CON(L,K)  
     &          +MIN(WW,0.)*CON(L,K+1)  
          ENDIF  
        ENDDO  
      ENDDO  
C  
C ** SET ANTIDIFFUSIVE FLUXES TO ZERO FOR SOURCE CELLS JMH20MARCH97  
C  ** ANTIDIFFUSION TURNED OFF FOR SOURCE CELLS  
C  
      IF(ISADAC(MVAR).EQ.1)THEN  
        DO K=1,KC  
          DO L=2,LA  
            IF(QSUMPAD(L,K).GT.0.0)THEN  
              LN=LNC(L)  
              FUHU(L  ,K)=0.  
              FUHU(L+1,K)=0.  
              FVHU(L  ,K)=0.  
              FVHU(LN ,K)=0.  
              FWU(L,K  )=0.  
              FWU(L,K-1)=0.  
              CONT(L,K)=0.  
            ELSE  
              CONT(L,K)=1.  
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
      IF(ISFCT(MVAR).EQ.0) GOTO 1100  
C  
C **  DETERMINE MAX AND MIN CONCENTRATIONS  
C  
      DO K=1,KC  
        DO L=1,LC  
          CONTMX(L,K)=0.0  
          CONTMN(L,K)=0.0  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          CONTMX(L,K)=MAX(CON(L,K),CON2(L,K))  
          CONTMN(L,K)=MIN(CON(L,K),CON2(L,K))  
        ENDDO  
      ENDDO  
      DO L=2,LA  
        CMAX(L,1)=MAX(CONTMX(L,1),CONTMX(L,2))  
        CMAX(L,KC)=MAX(CONTMX(L,KS),CONTMX(L,KC))  
        CMIN(L,1)=MIN(CONTMN(L,1),CONTMN(L,2))  
        CMIN(L,KC)=MIN(CONTMN(L,KS),CONTMN(L,KC))  
      ENDDO  
      DO K=2,KS  
        DO L=2,LA  
          CMAXT=MAX(CONTMX(L,K-1),CONTMX(L,K+1))  
          CMAX(L,K)=MAX(CONTMX(L,K),CMAXT)  
          CMINT=MIN(CONTMN(L,K-1),CONTMN(L,K+1))  
          CMIN(L,K)=MIN(CONTMN(L,K),CMINT)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
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
        DO L=2,LA  
          FUHV(L,K)=MIN(FUHU(L,K),0.)  
          FUHU(L,K)=MAX(FUHU(L,K),0.)  
          FVHV(L,K)=MIN(FVHU(L,K),0.)  
          FVHU(L,K)=MAX(FVHU(L,K),0.)  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO L=2,LA  
          FWV(L,K)=MIN(FWU(L,K),0.)  
          FWU(L,K)=MAX(FWU(L,K),0.)  
        ENDDO  
      ENDDO  
C  
C **  CALCULATE INFLUX AND OUTFLUX IN CONCENTRATION UNITS AND LOAD  
C **  INTO DU AND DV, THEN ADJUCT VALUES AT BOUNDARIES  
C  
      DO K=1,KC  
        RDZIC=DZIC(K)  
        DO L=2,LA  
          LN=LNC(L)  
          DU(L,K)=DELT*( DXYIP(L)*(FUHU(L,K)-FUHV(L+1,K)  
     &        +FVHU(L,K)-FVHV(LN,K))  
     &        +RDZIC*(FWU(L,K-1)-FWV(L,K)) )*HPI(L)  
          DV(L,K)=DELT*( DXYIP(L)*(FUHU(L+1,K)-FUHV(L,K)  
     &        +FVHU(LN,K)-FVHV(L,K))  
     &        +RDZIC*(FWU(L,K)-FWV(L,K-1)) )*HPI(L)  
        ENDDO  
      ENDDO  
      ! *** DSLLC Start
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
      ! *** DSLLC Start
C  
C         DU(L+1,K)=0.  
C         DV(LNC(L),K)=0.  
C         DU(LU+1,K)=0.  
C         DV(LNC(LU),K)=0.  
C         DU(LD+1,K)=0.  
C         DV(LNC(LD),K)=0.  
C         DU(LU+1,K)=0.  
C         DV(LNC(LU),K)=0.  
C         DU(LD+1,K)=0.  
C         DV(LNC(LD),K)=0.  
C **  CALCULATE BETA COEFFICIENTS WITH BETAUP AND BETADOWN IN DU AND DV  
C  
      DO K=1,KC  
        DO L=2,LA  
          IF(DU(L,K).GT.0.)DU(L,K)=(CMAX(L,K)-CON(L,K))/(DU(L,K)+BSMALL)  
          DU(L,K)=MIN(DU(L,K),1.)  
          IF(DV(L,K).GT.0.)DV(L,K)=(CON(L,K)-CMIN(L,K))/(DV(L,K)+BSMALL)  
          DV(L,K)=MIN(DV(L,K),1.)  
        ENDDO  
      ENDDO  
C  
C **  LIMIT FLUXES  
C  
      DO K=1,KC  
        DO L=2,LA  
          LS=LSC(L)  
          FUHU(L,K)=MIN(DV(L-1,K),DU(L,K))*FUHU(L,K)  
     &        +MIN(DU(L-1,K),DV(L,K))*FUHV(L,K)  
          FVHU(L,K)=MIN(DV(LS,K),DU(L,K))*FVHU(L,K)  
     &        +MIN(DU(LS,K),DV(L,K))*FVHV(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO L=2,LA  
          FWU(L,K)=MIN(DV(L,K),DU(L,K+1))*FWU(L,K)  
     &        +MIN(DU(L,K),DV(L,K+1))*FWV(L,K)  
        ENDDO  
      ENDDO  
C  
C **  ANTI-DIFFUSIVE ADVECTION CALCULATION  
C  
 1100 CONTINUE  
C  
C **  ADD SECOND CALL TO EXPERIMENTAL SED CALCULATION FOR  
C **  ANTIDIFFUSIVE CORRECTION  
C !!  THIS IS A TEMPORARY TRIAL IF DOESN'T WORK MAKE CALL  
C !!  FOR ALL CASES AT PREVIOUS LOCATION  
C !!  NOT ALSO THAT FQC HAS BEEN MOVED TO HERE FOR ISADAC=1  
C !!  IF PROBLEMS ARISE PUT BACK IN AS BEFORE AT OLD LOCATION  
C !!  RIGHT AFTER SECOND CALL TO CALSED2  
C  
      ! *** DSLLC Start
      DO K=1,KC
        DO IOBC=1,NBCSOP  
          L=LOBCS(IOBC)  
          CON1(L,K)=CON(L,K)  
        ENDDO
      END DO  

      DO K=1,KC  
        RDZIC=DZIC(K)  
        DO L=2,LA  
          CH(L,K)=CON(L,K)*HP(L)  
     &        +DELT*( (FUHU(L,K)-FUHU(L+1,K)  
     &        +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)  
     &        +(FWU(L,K-1)-FWU(L,K))*RDZIC )  
          !CON(L,K)=SCB(L)*CH(L,K)*HPI(L)+(1.-SCB(L))*CON(L,K)  
          CON(L,K)=CH(L,K)*HPI(L)
        ENDDO  
      ENDDO  

      DO K=1,KC
        DO IOBC=1,NBCSOP  
          L=LOBCS(IOBC)  
          CON(L,K)=CON1(L,K)  
        ENDDO
      END DO  
      ! *** DSLLC End
C  
C **  ADD REMAINING SEDIMENT SETTLING AND FLUX  
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
      RETURN  
      END  

