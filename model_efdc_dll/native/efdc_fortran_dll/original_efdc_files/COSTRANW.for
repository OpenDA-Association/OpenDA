C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C
      SUBROUTINE COSTRANW (ISTL_,IS2TL_,MVAR,M,CON,CON1)
C
C **  THIS SUBROUTINE IS PART OF  EFDC-FULL VERSION 1.0a 
C
C **  LAST MODIFIED BY JOHN HAMRICK ON 1 NOVEMBER 2001
C
C----------------------------------------------------------------------C
C
C CHANGE RECORD
C DATE MODIFIED     BY                 DATE APPROVED    BY
C 03/05/2002        john hamrick       03/05/2002       john hamrick
C  added dynamic time stepping
C----------------------------------------------------------------------C
C
C **  SUBROUTINE COSTRAN CALCULATES THE ADVECTIVE
C **  TRANSPORT OF DISSOLVED OR SUSPENDED CONSITITUENT M LEADING TO
C **  A NEW VALUE AT TIME LEVEL (N+1). THE VALUE OF ISTL_ INDICATES 
C **  THE NUMBER OF TIME LEVELS IN THE STEP
C
C**********************************************************************C
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
C**********************************************************************C
C
      BSMALL=1.0E-6
CBUG    WFQC=0.0
C
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
C
      DELTA4=0.25*DELTA
C
      IF(ISLTMT.GE.1) ISUD=1
C
      DO K=1,KC
       DO L=1,LC
        FUHU(L,K)=0.
        FUHV(L,K)=0.
        FVHU(L,K)=0.
        FVHV(L,K)=0.
        UUU(L,K)=0.
        VVV(L,K)=0.
c pmc        DU(L,K)=0.
c pmc        DV(L,K)=0.
       ENDDO
      ENDDO
C
      IF(IS2TL_.EQ.1)THEN
        ISUD=1
        DO K=1,KC
        DO L=1,LC
          CON1(L,K)=CON(L,K)
        ENDDO
        ENDDO
      ENDIF
C
C**********************************************************************C
C 
        DO K=1,KC
         DO L=1,LC
         CONT(L,K)=0.
         CMAX(L,K)=0.
         CMIN(L,K)=0.
         ENDDO
        ENDDO
C
C**********************************************************************C
C
C **  CALCULATED EXTERNAL SOURCES AND SINKS 
C
C----------------------------------------------------------------------C
C
      CALL CALFQC (ISTL_,IS2TL_,MVAR,M,CON,CON1,FQCPAD,QSUMPAD,QSUMNAD)
C     IF(ISTRAN(M).EQ.1) CALL CALFQC (ISTL_,M,CON,CON1)
C     IF(ISTRAN(M).EQ.3) CALL CALFQC (ISTL_,M,CON,CON1)
C     IF(M.EQ.4)THEN
C      IF(ISTOPT(4).EQ.2) CALL CALSED2(ISTL_,0.5,CON1)
C      NTSWVD=ISWVSD
C      IF(ISTOPT(4).EQ.13) CALL CALSED3(ISTL_,0.5,SED)
C      IF(ISTOPT(4).EQ.14.AND.N.GE.NTSWVD) CALL CALSED3(ISTL_,0.5,SED)
C      IF(ISTOPT(4).EQ.15) CALL CALSED3(ISTL_,0.5,SED)
C      IF(ISTOPT(4).EQ.16.AND.N.GE.NTSWVD) CALL CALSED3(ISTL_,0.5,SED)
C     ENDIF
C
C**********************************************************************C
C
C **  BEGIN COMBINED ADVECTION SCHEME
C
C**********************************************************************C
C
C **  INTERMEDIATE ADVECTION CALCULATIONS
C **  CX,CY,CZ
C
      DO K=1,KC    
      DO L=2,LA
       DELCX(L,K)=CON1(L,K)-CON1(L-1   ,K)
       DELCY(L,K)=CON1(L,K)-CON1(LSC(L),K)
      ENDDO
      ENDDO
C
      IF(KC.GE.2)THEN
        DO K=1,KS    
        DO L=2,LA
         DELCZ(L,K)=CON1(L,K+1)-CON1(L,K)
        ENDDO
        ENDDO
      ENDIF
C
      DO K=1,KC    
      DO L=2,LA
      CONCX(L,K)=CON1(L,K)+RCOSMICX(L,K)*
     &           ( COSMICXP(L  ,K)*DELCX(L  ,K)
     &            +COSMICXN(L+1,K)*DELCX(L+1,K) )
      ENDDO
      ENDDO
C
      DO K=1,KC    
      DO L=2,LA
      CONCY(L,K)=CON1(L,K)+RCOSMICY(L,K)*
     &           ( COSMICYP(L     ,K)*DELCY(L     ,K)
     &            +COSMICYN(LNC(L),K)*DELCY(LNC(L),K) )
      ENDDO
      ENDDO
C
      IF(KC.EQ.1)THEN
        DO L=2,LA
         CONCZ(L,1)=CON1(L,1)
        ENDDO
      ENDIF
C
      IF(KC.GE.2)THEN
        DO L=2,LA
         CONCZ(L,1)=CON1(L,1)
     &     +RCOSMICZ(L,1)*(COSMICZN(L,1)*DELCZ(L,1) )
        ENDDO
        DO L=2,LA
         CONCZ(L,KC)=CON1(L,KC)
     &     +RCOSMICZ(L,KC)*(COSMICZP(L,KS)*DELCZ(L,KS) )
        ENDDO
      ENDIF
C
      IF(KC.GE.3)THEN
        DO K=2,KS    
        DO L=2,LA
         CONCZ(L,K)=CON1(L,K)+RCOSMICZ(L,K)*
     &              ( COSMICZP(L,K-1)*DELCZ(L,K-1) 
     &               +COSMICZN(L  ,K)*DELCZ(L,K  ) )
        ENDDO
        ENDDO
      ENDIF
C
C**********************************************************************C
C
C **  INTERMEDIATE ADVECTION CALCULATIONS
C **  CXY,CXZ
C
      DO K=1,KC    
      DO L=2,LA
       DELCY(L,K)=CONCX(L,K)-CONCX(LSC(L),K)
      ENDDO
      ENDDO
C
      IF(KC.GE.2)THEN
        DO K=1,KS    
        DO L=2,LA
         DELCZ(L,K)=CONCX(L,K+1)-CONCX(L,K)
        ENDDO
        ENDDO
      ENDIF
C
      DO K=1,KC    
      DO L=2,LA
      CONCXY(L,K)=CONCX(L,K)+RCOSMICY(L,K)*
     &            ( COSMICYP(L     ,K)*DELCY(L     ,K)
     &             +COSMICYN(LNC(L),K)*DELCY(LNC(L),K) )
      ENDDO
      ENDDO
C
      IF(KC.EQ.1)THEN
        DO L=2,LA
         CONCXZ(L,1)=CONCX(L,1)
        ENDDO
      ENDIF
C
      IF(KC.GE.2)THEN
        DO L=2,LA
         CONCXZ(L,1)=CONCX(L,1)
     &     +RCOSMICZ(L,1)*(COSMICZN(L,1)*DELCZ(L,1) )
        ENDDO
        DO L=2,LA
         CONCXZ(L,KC)=CONCX(L,KC)
     &     +RCOSMICZ(L,KC)*(COSMICZP(L,KS)*DELCZ(L,KS) )
        ENDDO
      ENDIF
C
      IF(KC.GE.3)THEN
        DO K=2,KS    
        DO L=2,LA
         CONCXZ(L,K)=CONCX(L,K)+RCOSMICZ(L,K)*
     &              ( COSMICZP(L,K-1)*DELCZ(L,K-1) 
     &               +COSMICZN(L  ,K)*DELCZ(L,K  ) )
        ENDDO
        ENDDO
      ENDIF
C
C
C**********************************************************************C
C
C **  INTERMEDIATE ADVECTION CALCULATIONS
C **  CYZ,CYX
C
      DO K=1,KC    
      DO L=2,LA
       DELCX(L,K)=CONCY(L,K)-CONCY(L-1,K)
      ENDDO
      ENDDO
C
      IF(KC.GE.2)THEN
        DO K=1,KS    
        DO L=2,LA
         DELCZ(L,K)=CONCY(L,K+1)-CONCY(L,K)
        ENDDO
        ENDDO
      ENDIF
C
      DO K=1,KC    
      DO L=2,LA
      CONCYX(L,K)=CONCY(L,K)+RCOSMICX(L,K)*
     &           ( COSMICXP(L  ,K)*DELCX(L  ,K)
     &            +COSMICXN(L+1,K)*DELCX(L+1,K) )
      ENDDO
      ENDDO
C
      IF(KC.EQ.1)THEN
        DO L=2,LA
         CONCZ(L,1)=CONCY(L,1)
        ENDDO
      ENDIF
C
      IF(KC.GE.2)THEN
        DO L=2,LA
         CONCYZ(L,1)=CONCY(L,1)
     &     +RCOSMICZ(L,1)*(COSMICZN(L,1)*DELCZ(L,1) )
        ENDDO
        DO L=2,LA
         CONCYZ(L,KC)=CONCY(L,KC)
     &     +RCOSMICZ(L,KC)*(COSMICZP(L,KS)*DELCZ(L,KS) )
        ENDDO
      ENDIF
C
      IF(KC.GE.3)THEN
        DO K=2,KS    
        DO L=2,LA
         CONCYZ(L,K)=CONCY(L,K)+RCOSMICZ(L,K)*
     &              ( COSMICZP(L,K-1)*DELCZ(L,K-1) 
     &               +COSMICZN(L  ,K)*DELCZ(L,K  ) )
        ENDDO
        ENDDO
      ENDIF
C
C**********************************************************************C
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
C
      DO K=1,KC    
      DO L=2,LA
      CONCZX(L,K)=CONCZ(L,K)+RCOSMICX(L,K)*
     &           ( COSMICXP(L  ,K)*DELCX(L  ,K)
     &            +COSMICXN(L+1,K)*DELCX(L+1,K) )
      ENDDO
      ENDDO
C
      DO K=1,KC    
      DO L=2,LA
      CONCZY(L,K)=CONCZ(L,K)+RCOSMICY(L,K)*
     &           ( COSMICYP(L     ,K)*DELCY(L     ,K)
     &            +COSMICYN(LNC(L),K)*DELCY(LNC(L),K) )
      ENDDO
      ENDDO
C
C**********************************************************************C
C
      DO K=1,KC
      DO L=2,LA
        CONCXYZ(L,K)=( 2.*CON1(L,K)+CONCY(L,K)+CONCYZ(L,K)
     &                             +CONCZ(L,K)+CONCZY(L,K) )/6. 
        CONCYZX(L,K)=( 2.*CON1(L,K)+CONCZ(L,K)+CONCZX(L,K)
     &                             +CONCX(L,K)+CONCXZ(L,K) )/6. 
        CONCZXY(L,K)=( 2.*CON1(L,K)+CONCY(L,K)+CONCYX(L,K)
     &                             +CONCX(L,K)+CONCXY(L,K) )/6. 
      ENDDO
      ENDDO
C
C**********************************************************************C
C
C **  ADVECTIVE FLUX CALCULATION
C **  CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE WITH ADVECTION
C **  AVERAGED BETWEEN (N) AND (N+1) OR (N-1) AND (N+1) AND ADVECTED
C **  AT (N) OR (N-1) IF ISTL_ EQUALS 2 OR 3 RESPECTIVELY
C
C----------------------------------------------------------------------C
C
C HAMRICK'S IMPLEMENTATION OF FLUX LIMITER FOR WASP6 COMPATIBILITY
C LIMITER IS ROE'S SUPERBEE LIMITOR
c
      DO K=1,KC    
      DO L=2,LA
	CSTARN=0.0
	CSTARP=0.0
	IF(UHDY2(L,K).GT.0.0)THEN
         CSTARD=CONCXYZ(L,K)
	   CSTARU=CONCXYZ(L-1,K)
	   CSTARUU=CONCXYZ(L-1,K)
	   IF(SUB(L-1).GT.0.5) CSTARUU=CONCXYZ(L-2,K)
         TMPVAL=1.-DELT*UHDY2(L,K)*DXYIU(L)*HUI(L)
         RLIM=0.
         IF(SCB(L).GT.0.5)THEN
           TOP=CSTARU-CSTARUU
           BOT=CSTARD-CSTARU
           IF(TOP.EQ.BOT) RLIM=1.
           ABSBOT=ABS(BOT)
           IF(SUB(L-1).GT.0.5.AND.ABSBOT.GE.1.E-23) THEN
             RVAL=TOP/BOT
             RMIN1=MIN(2.*RVAL,1.)
             RMIN2=MIN(RVAL,2.)
             RLIM=MAX(0.,RMIN1,RMIN2)
           ENDIF
         ENDIF
         FLIMU=1.-RLIM*TMPVAL
         FLIMC=1.-FLIMU
         CSTARP=FLIMU*CSTARU+0.5*FLIMC*(CSTARU+CSTARD)
      ENDIF
	IF(UHDY2(L,K).LT.0.0)THEN
         CSTARD=CONCXYZ(L-1,K)
	   CSTARU=CONCXYZ(L,K)
	   CSTARUU=CONCXYZ(L,K)
	   IF(SUB(L+1).GT.0.5) CSTARUU=CONCXYZ(L+1,K)
         TMPVAL=1.+DELT*UHDY2(L,K)*DXYIU(L)*HUI(L)
         RLIM=0.
         IF(SCB(L-1).GT.0.5)THEN
           TOP=CSTARU-CSTARUU
           BOT=CSTARD-CSTARU
           IF(TOP.EQ.BOT) RLIM=1.
           ABSBOT=ABS(BOT)
           IF(SUB(L+1).GT.0.5.AND.ABSBOT.GE.1.E-23) THEN
             RVAL=TOP/BOT
             RMIN1=MIN(2.*RVAL,1.)
             RMIN2=MIN(RVAL,2.)
             RLIM=MAX(0.,RMIN1,RMIN2)
           END IF
         END IF
         FLIMU=1.-RLIM*TMPVAL
         FLIMC=1.-FLIMU
         CSTARN=FLIMU*CSTARU+0.5*FLIMC*(CSTARU+CSTARD)
      ENDIF
      FUHU(L,K)=MAX(UHDY2(L,K),0.)*CSTARP
     &         +MIN(UHDY2(L,K),0.)*CSTARN
      ENDDO
      ENDDO
C
      DO K=1,KC    
      DO L=2,LA
	LS=LSC(L)
	LSS=LSC(LS)
	LN=LNC(L)
	CSTARN=0.0
	CSTARP=0.0
	IF(VHDX2(L,K).GT.0.0)THEN
         CSTARD=CONCYZX(L,K)
	   CSTARU=CONCYZX(LS,K)
	   CSTARUU=CONCYZX(LS,K)
	   IF(SVB(LS).GT.0.5) CSTARUU=CONCYZX(LSS,K)
         TMPVAL=1.-DELT*VHDX2(L,K)*DXYIV(L)*HVI(L)
         RLIM=0.
         IF(SCB(L).GT.0.5)THEN
           TOP=CSTARU-CSTARUU
           BOT=CSTARD-CSTARU
           IF(TOP.EQ.BOT) RLIM=1.
           ABSBOT=ABS(BOT)
           IF(SVB(LS).GT.0.5.AND.ABSBOT.GE.1.E-23) THEN
             RVAL=TOP/BOT
             RMIN1=MIN(2.*RVAL,1.)
             RMIN2=MIN(RVAL,2.)
             RLIM=MAX(0.,RMIN1,RMIN2)
           END IF
         END IF
         FLIMU=1.-RLIM*TMPVAL
         FLIMC=1.-FLIMU
         CSTARP=FLIMU*CSTARU+0.5*FLIMC*(CSTARU+CSTARD)
      ENDIF
	IF(VHDX2(L,K).LT.0.0)THEN
         CSTARD=CONCYZX(LS,K)
	   CSTARU=CONCYZX(L,K)
 	   CSTARUU=CONCYZX(L,K)
	   IF(SVB(LN).GT.0.5) CSTARUU=CONCYZX(LN,K)
         TMPVAL=1.+DELT*VHDX2(L,K)*DXYIV(L)*HVI(L)
         RLIM=0.
         IF(SCB(LS).GT.0.5)THEN
           TOP=CSTARU-CSTARUU
           BOT=CSTARD-CSTARU
           IF(TOP.EQ.BOT) RLIM=1.
           ABSBOT=ABS(BOT)
           IF(SVB(LN).GT.0.5.AND.ABSBOT.GE.1.E-23) THEN
             RVAL=TOP/BOT
             RMIN1=MIN(2.*RVAL,1.)
             RMIN2=MIN(RVAL,2.)
             RLIM=MAX(0.,RMIN1,RMIN2)
           END IF
         ENDIF
         FLIMU=1.-RLIM*TMPVAL
         FLIMC=1.-FLIMU
         CSTARN=FLIMU*CSTARU+0.5*FLIMC*(CSTARU+CSTARD)
      ENDIF
      FVHU(L,K)=MAX(VHDX2(L,K),0.)*CSTARP
     &         +MIN(VHDX2(L,K),0.)*CSTARN
      ENDDO
      ENDDO
C
 1069 FORMAT(I8,10E13.5)
C
      DO K=1,KS
      DO L=2,LA
	IF(W2(L,K).GT.0.0)THEN
         CSTARD=CONCXYZ(L,K+1)
	   CSTARU=CONCXYZ(L,K)
	   CSTARUU=CONCXYZ(L,K)
	   IF(K.GT.1) CSTARUU=CONCXYZ(L,K-1)
         TMPVAL=1.-DELT*W2(L,K)*DZIC(K)*HPI(L)
         RLIM=0.
         TOP=CSTARU-CSTARUU
         BOT=CSTARD-CSTARU
         IF(TOP.EQ.BOT) RLIM=1
         ABSBOT=ABS(BOT)
         IF(K.GT.1.AND.ABSBOT.GE.1.E-23) THEN
           RVAL=TOP/BOT
           RMIN1=MIN(2.*RVAL,1.)
           RMIN2=MIN(RVAL,2.)
           RLIM=MAX(0.,RMIN1,RMIN2)
         END IF
         FLIMU=1.-RLIM*TMPVAL
         FLIMC=1.-FLIMU
         CSTARP=FLIMU*CSTARU+0.5*FLIMC*(CSTARU+CSTARD)
      ENDIF
	IF(W2(L,K).LT.0.0)THEN
         CSTARD=CONCXYZ(L,K)
	   CSTARU=CONCXYZ(L,K+1)
	   CSTARUU=CONCXYZ(L,K+1)
	   IF(K.LT.KS) CSTARUU=CONCXYZ(L,K+1)
         TMPVAL=1.+DELT*W2(L,K)*DZIC(K)*HPI(L)
         RLIM=0.
         TOP=CSTARU-CSTARUU
         BOT=CSTARD-CSTARU
         IF(TOP.EQ.BOT) RLIM=1
         ABSBOT=ABS(BOT)
         IF(K.LT.KS.AND.ABSBOT.GE.1.E-23) THEN
           RVAL=TOP/BOT
           RMIN1=MIN(2.*RVAL,1.)
           RMIN2=MIN(RVAL,2.)
           RLIM=MAX(0.,RMIN1,RMIN2)
         END IF
         FLIMU=1.-RLIM*TMPVAL
         FLIMC=1.-FLIMU
         CSTARN=FLIMU*CSTARU+0.5*FLIMC*(CSTARU+CSTARD)
      ENDIF
      FWU(L,K)=MAX(W2(L,K),0.)*CSTARP
     &        +MIN(W2(L,K),0.)*CSTARN
      ENDDO
      ENDDO
C
C**********************************************************************C
C
C **  STANDARD ADVECTION CALCULATION
C
C----------------------------------------------------------------------C
C
C **  IF ISACAC EQ 0 INCLUDE FQC MASS SOURCES IN UPDATE
C
C BEGIN IF ON TRANSPORT OPTION CHOICE
C
      IF(ISCDCA(MVAR).EQ.0.)THEN
C
C BEGIN IF ON TIME LEVEL CHOICE FOR ISCDCA=0
C
      IF(ISTL_.EQ.2)THEN
C
      DO K=1,KC
      RDZIC=DZIC(K)
      DO L=2,LA
CX      CH(L,K)=CONT(L,K)*H1P(L)
CX     &       +DELT*( (WFQC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)
      CH(L,K)=CON1(L,K)*H1P(L)
     &       +DELT*( ( RDZIC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)
     &                        +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &                        +(FWU(L,K-1)-FWU(L,K))*RDZIC )
      ENDDO
      ENDDO
C
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
C
      DO K=1,KC
      RDZIC=DZIC(K)
      DO L=2,LA
CX      CH(L,K)=CONT(L,K)*H2P(L)
CX     &       +DELT*( (WFQC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)
      CH(L,K)=CON1(L,K)*H2P(L)
     &       +DELT*( ( RDZIC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)
     &                        +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &                        +(FWU(L,K-1)-FWU(L,K))*RDZIC )
      ENDDO
      ENDDO
C
      IF(ISFCT(MVAR).GE.1)THEN
       DO K=1,KC
       DO L=2,LA
       CON2(L,K)=CON(L,K)
       ENDDO
       ENDDO
      ENDIF
C
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
C
      DO K=1,KC
      DO L=2,LA
      CON(L,K)=SCB(L)*CH(L,K)*HPI(L)+(1.-SCB(L))*CON(L,K)
      CONT(L,K)=0.0
      ENDDO
      ENDDO
C
C **  ADD REMAINING SEDIMENT SETTLING AND FLUX
C
C     IF(M.EQ.4)THEN
C      IF(ISTOPT(4).EQ.2) CALL CALSED2(ISTL_,0.5,CON )
C      NTSWVD=ISWVSD
C      IF(ISTOPT(4).EQ.13) CALL CALSED3(ISTL_,0.5,SED)
C      IF(ISTOPT(4).EQ.14.AND.N.GE.NTSWVD) CALL CALSED3(ISTL_,0.5,SED)
C      IF(ISTOPT(4).EQ.15) CALL CALSED3(ISTL_,0.5,SED)
C      IF(ISTOPT(4).EQ.16.AND.N.GE.NTSWVD) CALL CALSED3(ISTL_,0.5,SED)
C     ENDIF
C
C **  IF ISACAC NE 0 DO NOT INCLUDE FQC MASS SOURCES IN UPDATE
C
C ELSE ON TRANSPORT OPTION CHOICE
C
      ELSE
C
C BEGIN IF ON TIME LEVEL CHOICE FOR ISCDCA.NE.0
C
      IF(ISTL_.EQ.2)THEN
C
      DO K=1,KC
      RDZIC=DZIC(K)
      DO L=2,LA
CX      CH(L,K)=CONT(L,K)*H1P(L)
CX     &       +DELT*( (FUHU(L,K)-FUHU(L+1,K)
      CH(L,K)=CON1(L,K)*H1P(L)
     &       +DELT*( ( RDZIC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)
     &               +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &               +(FWU(L,K-1)-FWU(L,K))*RDZIC )
      ENDDO
      ENDDO
C
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
C
      DO K=1,KC
      RDZIC=DZIC(K)
      DO L=2,LA
CX      CH(L,K)=CONT(L,K)*H2P(L)
CX     &       +DELT*( (FUHU(L,K)-FUHU(L+1,K)
      CH(L,K)=CON1(L,K)*H2P(L)
     &       +DELT*( ( RDZIC*FQC(L,K)+FUHU(L,K)-FUHU(L+1,K)
     &               +FVHU(L,K)-FVHU(LNC(L),K))*DXYIP(L)
     &               +(FWU(L,K-1)-FWU(L,K))*RDZIC )
      ENDDO
      ENDDO
C
      IF(ISFCT(MVAR).GE.1)THEN
       DO K=1,KC
       DO L=2,LA
       CON2(L,K)=CON(L,K)
       ENDDO
       ENDDO
      ENDIF
C
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
C
      DO K=1,KC
      DO L=2,LA
      CON(L,K)=SCB(L)*CH(L,K)*HPI(L)+(1.-SCB(L))*CON(L,K)
      CONT(L,K)=0.0
      ENDDO
      ENDDO
C  
      ENDIF
C
C ENDIF ON TRANSPORT OPTION CHOICE
C
C**********************************************************************C
C
C **  CALCULATE LAST OUTFLOWING CONCENTRATION OR SPECIFY INFLOW 
C **  CONCENTRATION AT OPEN BOUNDARIES
C
C----------------------------------------------------------------------C
C
      DO K=1,KC
      DO LL=1,NCBS
      NSID=NCSERS(LL,M)
      L=LCBS(LL)
      LN=LNC(L)
C
      IF(VHDX2(LN,K).LT.0.)THEN
       IF(ISTL_.EQ.2)THEN
        CTMP=CON1(L,K)+DELT*(VHDX2(LN,K)*CON1(L,K)
     &      -FVHU(LN,K))*DXYIP(L)*HPI(L)
       ELSE
        IF(ISCDCA(MVAR).NE.2) CTMP=CON1(L,K)+DELT*(VHDX2(LN,K)*CON1(L,K)
     &      -FVHU(LN,K))*DXYIP(L)*HPI(L)
        IF(ISCDCA(MVAR).EQ.2) CTMP=0.5*(CON1(L,K)+CON(L,K))
     &     +0.5*(CON1(L,K)-CON(L,K))*H2P(L)*HPI(L)
     &     +DELT*(0.5*VHDX2(LN,K)*(CON1(L,K)+CON(L,K))
     &     -FVHU(LN,K))*DXYIP(L)*HPI(L)
        CON1(L,K)=CON(L,K)
       ENDIF
       CON(L,K)=CTMP
       CBSTMP=CBS(LL,1,M)+CSERT(1,NSID,M)
       IF(M.EQ.1.AND.CON(L,K).GT.CBSTMP) CON(L,K)=CBSTMP
       CLOS(LL,K,M)=CON(L,K)
       NLOS(LL,K,M)=N
      ELSE
       IF(ISUD.EQ.1) CON1(L,K)=CON(L,K)
       CBT=WTCI(K,1)*CBS(LL,1,M)+WTCI(K,2)*CBS(LL,2,M)+CSERT(K,NSID,M)
C      WRITE(6,6001)N,K,CBT
       NMNLO=N-NLOS(LL,K,M)
       IF(NMNLO.GE.NTSCRS(LL))THEN
        CON(L,K)=CBT
       ELSE
        CON(L,K)=CLOS(LL,K,M)
     &         +(CBT-CLOS(LL,K,M))*FLOAT(NMNLO)/FLOAT(NTSCRS(LL))
       ENDIF
      ENDIF
C
      ENDDO
      ENDDO
C
 6001 FORMAT('N,K,CBTS = ',2I10,F12.3)
C
C----------------------------------------------------------------------C
C
      DO K=1,KC
      DO LL=1,NCBW
      NSID=NCSERW(LL,M)
      L=LCBW(LL)      
C
      IF(UHDY2(L+1,K).LT.0.)THEN
       IF(ISTL_.EQ.2)THEN
        CTMP=CON1(L,K)+DELT*(UHDY2(L+1,K)*CON1(L,K)
     &      -FUHU(L+1,K))*DXYIP(L)*HPI(L)
       ELSE
        IF(ISCDCA(MVAR).NE.2) CTMP=CON1(L,K)
     &   +DELT*(UHDY2(L+1,K)*CON1(L,K)-FUHU(L+1,K))*DXYIP(L)*HPI(L)
        IF(ISCDCA(MVAR).EQ.2) CTMP=0.5*(CON1(L,K)+CON(L,K))
     &     +0.5*(CON1(L,K)-CON(L,K))*H2P(L)*HPI(L)
     &     +DELT*(0.5*UHDY2(L+1,K)*(CON1(L,K)+CON(L,K))
     &     -FUHU(L+1,K))*DXYIP(L)*HPI(L)
        CON1(L,K)=CON(L,K)
       ENDIF
       CON(L,K)=CTMP
       CBWTMP=CBW(LL,1,M)+CSERT(1,NSID,M)
       IF(M.EQ.1.AND.CON(L,K).GT.CBWTMP) CON(L,K)=CBWTMP
       CLOW(LL,K,M)=CON(L,K)
       NLOW(LL,K,M)=N
      ELSE
       IF(ISUD.EQ.1) CON1(L,K)=CON(L,K)
       CBT=WTCI(K,1)*CBW(LL,1,M)+WTCI(K,2)*CBW(LL,2,M)+CSERT(K,NSID,M)
       NMNLO=N-NLOW(LL,K,M)
       IF(NMNLO.GE.NTSCRW(LL))THEN
        CON(L,K)=CBT
       ELSE
        CON(L,K)=CLOW(LL,K,M)
     &         +(CBT-CLOW(LL,K,M))*FLOAT(NMNLO)/FLOAT(NTSCRW(LL))
       ENDIF
      ENDIF
C
      ENDDO
      ENDDO
C
C----------------------------------------------------------------------C
C
      DO K=1,KC
      DO LL=1,NCBE
      NSID=NCSERE(LL,M)
      L=LCBE(LL)      
C
      IF(UHDY2(L,K).GT.0.)THEN
       IF(ISTL_.EQ.2)THEN
        CTMP=CON1(L,K)+DELT*(FUHU(L,K)
     &      -UHDY2(L,K)*CON1(L,K))*DXYIP(L)*HPI(L)
       ELSE
        IF(ISCDCA(MVAR).NE.2) CTMP=CON1(L,K)+DELT*(FUHU(L,K)
     &      -UHDY2(L,K)*CON1(L,K))*DXYIP(L)*HPI(L)
        IF(ISCDCA(MVAR).EQ.2) CTMP=0.5*(CON1(L,K)+CON(L,K))
     &      +0.5*(CON1(L,K)-CON(L,K))*H2P(L)*HPI(L)+DELT*(FUHU(L,K)
     &      -0.5*UHDY2(L,K)*(CON1(L,K)+CON(L,K)))*DXYIP(L)*HPI(L)
        CON1(L,K)=CON(L,K)
       ENDIF
       CON(L,K)=CTMP
       CBETMP=CBE(LL,1,M)+CSERT(1,NSID,M)
       IF(M.EQ.1.AND.CON(L,K).GT.CBETMP) CON(L,K)=CBETMP
       CLOE(LL,K,M)=CON(L,K)
       NLOE(LL,K,M)=N
      ELSE
       IF(ISUD.EQ.1) CON1(L,K)=CON(L,K)
       CBT=WTCI(K,1)*CBE(LL,1,M)+WTCI(K,2)*CBE(LL,2,M)+CSERT(K,NSID,M)
       NMNLO=N-NLOE(LL,K,M)
       IF(NMNLO.GE.NTSCRE(LL))THEN
        CON(L,K)=CBT
       ELSE
        CON(L,K)=CLOE(LL,K,M)
     &         +(CBT-CLOE(LL,K,M))*FLOAT(NMNLO)/FLOAT(NTSCRE(LL))
       ENDIF
      ENDIF
C
      ENDDO
      ENDDO
C
C----------------------------------------------------------------------C
C
      DO K=1,KC
      DO LL=1,NCBN
      NSID=NCSERN(LL,M)
      L=LCBN(LL)
      LS=LSC(L)
C
      IF(VHDX2(L,K).GT.0.)THEN
       IF(ISTL_.EQ.2)THEN
        CTMP=CON1(L,K)+DELT*(FVHU(L,K)
     &      -VHDX2(L,K)*CON1(L,K))*DXYIP(L)*HPI(L)
       ELSE
        IF(ISCDCA(MVAR).NE.2) CTMP=CON1(L,K)+DELT*(FVHU(L,K)
     &      -VHDX2(L,K)*CON1(L,K))*DXYIP(L)*HPI(L)
        IF(ISCDCA(MVAR).EQ.2) CTMP=0.5*(CON1(L,K)+CON(L,K))
     &      +0.5*(CON1(L,K)-CON(L,K))*H2P(L)*HPI(L)+DELT*(FVHU(L,K)
     &      -0.5*VHDX2(L,K)*(CON1(L,K)+CON(L,K)))*DXYIP(L)*HPI(L)
        CON1(L,K)=CON(L,K)
       ENDIF
       CON(L,K)=CTMP
       CBNTMP=CBN(LL,1,M)+CSERT(1,NSID,M)
       IF(M.EQ.1.AND.CON(L,K).GT.CBNTMP) CON(L,K)=CBNTMP
       CLON(LL,K,M)=CON(L,K)
       NLON(LL,K,M)=N
      ELSE
       IF(ISUD.EQ.1) CON1(L,K)=CON(L,K)
       CBT=WTCI(K,1)*CBN(LL,1,M)+WTCI(K,2)*CBN(LL,2,M)+CSERT(K,NSID,M)
C      WRITE(6,6002)N,K,CBT
       NMNLO=N-NLON(LL,K,M)
       IF(NMNLO.GE.NTSCRN(LL))THEN
        CON(L,K)=CBT
       ELSE
        CON(L,K)=CLON(LL,K,M)
     &         +(CBT-CLON(LL,K,M))*FLOAT(NMNLO)/FLOAT(NTSCRN(LL))
       ENDIF
      ENDIF
C
      ENDDO
      ENDDO
C
 6002 FORMAT('N,K,CBTN = ',2I10,F12.3)
C
C**********************************************************************C
C
      RETURN
      END
