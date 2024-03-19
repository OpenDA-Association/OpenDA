      SUBROUTINE SSEDTOX(ISTLX,IS2TLX,CORDTX)  
C  
C CHANGE RECORD  
C  CHANGED ADD AND REMOVE BED LAYER ALGORITHM - SEE SECTIONS UNDER  
C  IBMECH=0,1, AND GE 2  
C  CHANGED ADD AND REMOVE BED LAYER ALGORITHM - SEE SECTIONS UNDER  
C   IBMECH=0,1, AND GE 2  
C  MODIFIED NONCOHESIVE RESUSPENSION FORMULATION TO BE CONSISTENT  
C   WITH ADD AND REMOVE BED LAYER MODIFICATION  
C  ADDED NONCOHESIVE BEDLOAD-SUSPENDED LOAD DISTRIBUTION FACTOR  
C   CALCULATED BY FUNCTION FSEDMODE.F  
C  ADDED HIDING FACTOR CORRECTION TO BAGNOLD FORM OF BED LOAD TRANSPORT  
C   CALCULATION  
C   CHANGED DIMENSIONS OF QSBDLDX AND QSBDLDY  
C  MODIFIED CALLS FUNCTIONS FHYDCN, FDSTRSE,AND FSTRSE TO ADDED USE  
C  STANDARD EXPONENTIAL FORM CONSTITUTIVE RELATIONSHIPS  
C  FIXED ERRORS IN FINITE STRAIN CONSOLIDATION (IBMECH.GE.2)  
C  ADDED BED LOAD OUTFLOW/RECIRCULATION BOUNDARY CONDITION OPTIONS  
C  AND BY PASS ARRAY RBPSBL  
C  INCREASED DIMENSIONS OF BEDLOAD FLUX SNDFBL  
C  CORRECTED CALCULATION OF SNDFBL  
C  MODIFIED FUNCTION FSBDLD CALL  
C  ADDED EXPOSURE AND HIDING FUNCTIONS PEXP AND PHID  
C  ADDED ADDITIONAL LOGIC TO PREVENT DIVIDE BY ZEROS  
C  CORRECTED SPELLING ERROR SNDDMX (WAS SNDDMAX)  
C  CORRECTED SPELLING ERROR SEDVRDT (WAS SEDVRT)  
C  ADDED BELV1, FIXED ERRORS IN GENERAL BED LOAD FUNCTION, AND  
C  ELIMINATED ERRORNEOUS RETURN BASED ON TOXIC PARAMETER VALUE  
C  MODIFIED TOXIC-ORGANIC CARBON PARTITIONING OPTIONS  
C  FIX SOME INCONSISTENCIES INVOLVING SEDB1,SNDB1,TOXB1. ADDED  
C  ADJUSTMENT TO WATER COLUMN CONCENTRATIONS WHEN MORPHOLOGICAL MODE  
C  IS ACTIVATED.  FIXED ERROR IN PORE WATER ADVECTION AND DIFFUSION  
C  SOLUTION  
C  ADDED BY PASS OF BED LOAD TRANSPORT FOR DRY CELLS  
C  ADDED ADJUSTMENT TO TOXIC PORE WATER ADVECTION AND DIFFUSION TO  
C  GUARANTEE MASS CONSERVATION, INCLUDING VARIABLES DERRB,TOXBBALO,  
C  TOXBBALN,TOXWBALO,TOXWBALN  
C  FIXED BED LOAD TRANSPORT OF SORBED CONTAMINANT. AND ADDED BED LOAD  
C  TOXIC FLUX TOXFBL(L,NT), BED LOAD TOXIC FLUX ON OUT FLOW BOUNDRY  
C  TOXBLB(NT) AND SED-TOX DEBUG FLAG ISDTXBUG  
C  MOVED TOXIC INITIALIZATIONS TO BEDINIT.FOR  
C  ADDED ROUNDOFF CONTROL TO SEDIMENT-WATER COLUMN EXCHANGE OF  
C  TOXICS.  MOVED LOCAL ARRAY SNDFBL TO GLOBAL COMMON  
C  MADE LOCAL ARRAYS TAUB(L),USTAR(L),UCELLCTR(L),VCELLCTR(L) OF FORMER  
C  SCALER VARIABLES OF SAME NAMES.  CALCULATED EACH ONCE AT START  
C  OF SEDIMENT TRANSPORT.  
C  REWROTE SEDIMENT BED FLOW AND RECIRCULATION BOUNDARY CONDITION  
C  IMPLEMENTATION IN BED LOAD TRANSPORT SECTION ADDED GLOBAL ARRAYS  
C  QSBDLOT AND QSBDLIN  
C  ADDED QMORPH(L), THE EQUIVALENT WATER COLUMN VOLUME SOURCE ASSOCIATE  
C  WITH CHANGE IN BED ELEVATION, FOR USE IN MASS BALANCE  
C
C  Merged SNL with DS-INTL
C
C **  SUBROUTINE SSEDTOX CALCULATES SETTLING AND WATER COLUMN-BED  
C **  EXCHANGE OF SEDIMENT AND SORBED TOXIC CONTAMINANTS  
C  
      USE GLOBAL  
      USE MPI

      ! *** EE BEGIN BLOCK  
	IMPLICIT NONE
	REAL::T1TMP, T2TMP, CORT,CORDTX,HDZBR,DSEDGMMI
	REAL::CSEDTAUS,CSEDRESS,CSEDTAUB,UTMP,VTMP,CURANG
	REAL::TAUBC,TAUB2,CSEDRESB,HDFUFXX,HDFUFYY,QSWNEG
	REAL::TMPEXP,QCELLCTRA,RSKSDD50,TMPVAL,QSWPOS
	INTEGER::ISTLX,IS2TLX,NVAL,L,K,NS,NX
	INTEGER::KTOPTP,KTOPM1,NT,LN,ITMP
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DELBED  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::QCELLAD1SQ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::QCELLAD1ZZ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TAUBSEDS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TAUBSNDS  

      IF(.NOT.ALLOCATED(DELBED))THEN
		ALLOCATE(DELBED(LCM))
		ALLOCATE(QCELLAD1SQ(LCM))
		ALLOCATE(QCELLAD1ZZ(LCM))
		ALLOCATE(TAUBSEDS(LCM))
		ALLOCATE(TAUBSNDS(LCM))
	    DELBED=0.0 
	    QCELLAD1SQ=0.0 
	    QCELLAD1ZZ=0.0 
	    TAUBSEDS=0.0 
	    TAUBSNDS=0.0 
	ENDIF
C
C *** EE BEGIN BLOCK  
C  
      IF(IS_TIMING)THEN  
        CALL CPU_TIME(T1TMP)  
      ENDIF  
C  
C *** EE END BLOCK  
C  
      ISTL=ISTLX  
      IS2TL=IS2TLX  
      CORT=CORDTX  
      DELT=DT2  
      S3TL=1.0  
      S2TL=0.0  
      ISUD=1  
      IF(ISTL.NE.3)THEN  
        DELT=DT  
        S3TL=0.0  
        S2TL=1.0  
        ISUD=0  
      ENDIF  
      IF(IS2TL.EQ.1)THEN  
        IF(ISDYNSTP.EQ.0)THEN  
          DELT=DT  
        ELSE  
          DELT=DTDYN  
        ENDIF  
        S3TL=1.0  
        S2TL=0.0  
        ISUD=1  
      ENDIF  
      IF(ISEDDT.GT.1) DELT=DTSED  

      DELTI=1./DELT  
      SEDMDGM=SQRT(SEDMDMX*SEDMDMN)  
      BEDEX=1.  
      NVAL=MOD(N,2)  

      FOURDPI=4./PI  
      DO L=2,LA  
        CTMPDRY(L)=1.  
      ENDDO  
C**********************************************************************C
C
C **  SET FLAGS FOR CORNER CELL BED STRESS CORRECTIONS
C *** PMC - THIS SECTIONS NOT USED - COMMENTED OUT
C
C      IF(ISCORTBC.GE.1) THEN
C
C **  SET FLAG FOR CELLS HAVING VOLUME SOURCE OR SINKS
C
C      DO L=1,LC
C	  ISSBCP(L)=0
C	ENDDO
C
C	DO L=2,LA
C	  IF(RSSBCE(L).GT.1.5)ISSBCP(L)=1
C	  IF(RSSBCW(L).GT.1.5)ISSBCP(L)=1
C	  IF(RSSBCN(L).GT.1.5)ISSBCP(L)=1
C	  IF(RSSBCS(L).GT.1.5)ISSBCP(L)=1
C	ENDDO
C
C      ENDIF
C
C**********************************************************************C
      IF(ISDTXBUG.EQ.1.AND.DEBUG)THEN  
        IF(N.EQ.1)THEN  
          OPEN(1,FILE='SSEDTOX0.DIA',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='SSEDTOX0.DIA',STATUS='UNKNOWN')  
          OPEN(11,FILE='SSEDTOX1.DIA',STATUS='UNKNOWN')  
          CLOSE(11,STATUS='DELETE')  
          OPEN(11,FILE='SSEDTOX1.DIA',STATUS='UNKNOWN')  
          OPEN(21,FILE='SSEDTOX2.DIA',STATUS='UNKNOWN')  
          CLOSE(21,STATUS='DELETE')  
          OPEN(21,FILE='SSEDTOX2.DIA',STATUS='UNKNOWN')  
          OPEN(31,FILE='SSEDTOX3.DIA',STATUS='UNKNOWN')  
          CLOSE(31,STATUS='DELETE')  
          OPEN(31,FILE='SSEDTOX3.DIA',STATUS='UNKNOWN')  
          OPEN(41,FILE='SSEDTOX4.DIA',STATUS='UNKNOWN')  
          CLOSE(41,STATUS='DELETE')  
          OPEN(41,FILE='SSEDTOX4.DIA',STATUS='UNKNOWN')  
        ELSE  
          OPEN(1,FILE='SSEDTOX0.DIA',POSITION='APPEND',STATUS='UNKNOWN')  
         OPEN(11,FILE='SSEDTOX1.DIA',POSITION='APPEND',STATUS='UNKNOWN')  
         OPEN(21,FILE='SSEDTOX2.DIA',POSITION='APPEND',STATUS='UNKNOWN')  
         OPEN(31,FILE='SSEDTOX3.DIA',POSITION='APPEND',STATUS='UNKNOWN')  
         OPEN(41,FILE='SSEDTOX4.DIA',POSITION='APPEND',STATUS='UNKNOWN')  
        ENDIF  
      ENDIF  
C  
C **  IF N=1 CALCULATE INITIAL SEDIMENT BED THICKNESS  
C **  MOVED TO SUBROUTINE BEDINIT  IN 8 AUGUST 2001 VERSION  
C       HBED(L,K)=0.  
C       SEDBALL(L,K)=0.  
C      TMPVAL=1./(1.-PORBED(L,K))  
C ** DIAGNOSTICS OF INITIALIZATION  
C **   UPDATE SEDIMENT PROCESSES  
C **  CALCULATE TOTAL SEDIMENT IN THE BED  
C  
      DO K=1,KB  
        DO L=1,LC  
          SEDBT(L,K)=0.  
          SNDBT(L,K)=0.  
          SEDBALL(L,K)=0.  
        ENDDO  
      ENDDO  
      IF(ISTRAN(6).GE.1.AND.IWRSP(1).LT.98)THEN  !avoids bed calculation when SEDZLJ is active  
        DO NS=1,NSED  
          DO K=1,KB  
            DO L=2,LA  
              SEDBT(L,K)=SEDBT(L,K)+SEDB(L,K,NS)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISTRAN(7).GE.1)THEN  
        DO NS=1,NSND  
          DO K=1,KB  
            DO L=2,LA  
              SNDBT(L,K)=SNDBT(L,K)+SNDB(L,K,NS)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
      DO K=1,KB  
        DO L=1,LC  
          SEDBALL(L,K)=SEDBT(L,K)+SNDBT(L,K)  
        ENDDO  
      ENDDO  
C  
C **  SET SEDIMENT VOLUME FRACTIONS  
C  
      IF(IWRSP(1).LT.98)THEN  !avoids bed calculation when SEDZLJ is active
	  DO K=1,KB  
          DO L=2,LA  
            BEDLINIT(L,K)=0.  
            BEDDINIT(L,K)=0.  
          ENDDO  
        ENDDO  
        DO NX=1,NSED+NSND  
          DO K=1,KB  
            DO L=2,LA  
              VFRBED(L,K,NX)=0.  
              VFRBED1(L,K,NX)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        IF(ISTRAN(6).GE.1)THEN  
          DO NS=1,NSED  
            DO K=1,KB  
              DO L=2,LA  
                VFRBED(L,K,NS)=SDEN(NS)*SEDB(L,K,NS)  
                VFRBED1(L,K,NS)=SDEN(NS)*SEDB1(L,K,NS)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(7).GE.1)THEN  
          DO NX=1,NSND  
            NS=NSED+NX  
            DO K=1,KB  
              DO L=2,LA  
                VFRBED(L,K,NS)=SDEN(NS)*SNDB(L,K,NX)  
                VFRBED1(L,K,NS)=SDEN(NS)*SNDB1(L,K,NX)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
	ENDIF
      IF(ISTRAN(6).GE.1.AND.IWRSP(1).LT.98)THEN  !avoids bed calculation when SEDZLJ is active 
        DO NS=1,NSED  
          DO K=1,KB  
            DO L=2,LA  
              BEDLINIT(L,K)=BEDLINIT(L,K)+VFRBED(L,K,NS)  
              BEDDINIT(L,K)=BEDDINIT(L,K)+VFRBED1(L,K,NS)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISTRAN(7).GE.1)THEN  
        DO NX=1,NSND  
          NS=NSED+NX  
          DO K=1,KB  
            DO L=2,LA  
              BEDLINIT(L,K)=BEDLINIT(L,K)+VFRBED(L,K,NS)  
              BEDDINIT(L,K)=BEDDINIT(L,K)+VFRBED1(L,K,NS)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISTRAN(6).GE.1.AND.IWRSP(1).LT.98)THEN  !avoids bed calculation when SEDZLJ is active
        DO NS=1,NSED  
          DO K=1,KB  
            DO L=2,LA  
              IF(BEDLINIT(L,K).GT.0.0)THEN  
                VFRBED(L,K,NS)=VFRBED(L,K,NS)/BEDLINIT(L,K)  
              ELSE  
                VFRBED(L,K,NS)=0.0  
              ENDIF  
              IF(BEDDINIT(L,K).GT.0.0)THEN  
                VFRBED1(L,K,NS)=VFRBED1(L,K,NS)/BEDDINIT(L,K)  
              ELSE  
                VFRBED1(L,K,NS)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISTRAN(7).GE.1)THEN  
        DO NX=1,NSND  
          NS=NSED+NX  
          DO K=1,KB  
            DO L=2,LA  
              IF(BEDLINIT(L,K).GT.0.0)THEN  
                VFRBED(L,K,NS)=VFRBED(L,K,NS)/BEDLINIT(L,K)  
              ELSE  
                VFRBED(L,K,NS)=0.0  
              ENDIF  
              IF(BEDDINIT(L,K).GT.0.0)THEN  
                VFRBED1(L,K,NS)=VFRBED1(L,K,NS)/BEDDINIT(L,K)  
              ELSE  
                VFRBED1(L,K,NS)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
      DO L=2,LA  
        QWBDTOP(L)=0.  
        QSBDTOP(L)=0.  
      ENDDO  
      DO K=1,KB  
        DO L=2,LA  
          FRACCOH(L,K)=0.0  
          FRACNON(L,K)=0.0  
        ENDDO  
      ENDDO  
      DO NS=1,NSED  
        DO K=1,KB  
          DO L=2,LA  
            IF(K.LE.KBT(L))THEN  
              FRACCOH(L,K)=FRACCOH(L,K)+VFRBED(L,KBT(L),NS)  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDDO  
      DO NX=1,NSND  
        NS=NX+NSED  
        DO K=1,KB  
          DO L=2,LA  
            IF(K.LE.KBT(L))THEN  
              FRACNON(L,K)=FRACNON(L,K)+VFRBED(L,KBT(L),NS)  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDDO  
C  
C **  SET COHESIVE BED CRITICAL STRESSES AND RESUSPENSION RATES  
C  
      IF(ISTRAN(6).GE.1.AND.IWRSP(1).LT.98)THEN  !avoids bed calculation when SEDZLJ is active
        ! PMC IF(IWRSP(1).EQ.0)THEN  
        ! PMC  DO K=1,KB  
        ! PMC    DO L=2,LA  
        ! PMC      TAURS(L,K)=TAUR(1)  
        ! PMC      WRSPS(L,K)=WRSPO(1)  
        ! PMC    ENDDO  
        ! PMC  ENDDO  
        ! PMCENDIF  
        ! PMCIF(IWRSPB(1).EQ.0)THEN  
        ! PMC  DO K=1,KB  
        ! PMC    DO L=2,LA  
        ! PMC      TAURB(L,K)=1.E6  
        ! PMC      WRSPB(L,K)=0.0  
        ! PMC    ENDDO  
        ! PMC  ENDDO  
        ! PMCENDIF
        IF(IWRSP(1).GE.1.AND.IWRSP(1).LT.98)THEN  !avoids bed calculation when SEDZLJ is active
          !DO K=1,KB  
            DO L=2,LA  
              K=KBT(L)  ! PMC - REMOVED LOOP AND ONLY COMPUTE THE TOP LAYER
              TAURS(L,K)=CSEDTAUS(BDENBED(L,K),TAUR(1),VDRRSPO(1),  
     &            VDRBED(L,K),VDRBED(L,K),IWRSP(1),L)  
              WRSPS(L,K)=CSEDRESS(BDENBED(L,K),WRSPO(1),VDRRSPO(1),  
     &            VDRBED(L,K),VDRBED(L,K),IWRSP(1))  
            ENDDO  
          !ENDDO  
        ENDIF  
        IF(IWRSPB(1).GE.1.AND.IWRSP(1).LT.98)THEN  !avoids bed calculation when SEDZLJ is active  
          !DO K=1,KB  
            DO L=2,LA
              K=KBT(L)  ! PMC - REMOVED LOOP AND ONLY COMPUTE THE TOP LAYER
              TAURB(L,K)=CSEDTAUB(BDENBED(L,K),TAUR(1),VDRRSPO(1),  
     &            VDRBED(L,K),VDRBED(L,K),IWRSPB(1))  
              WRSPB(L,K)=CSEDRESB(BDENBED(L,K),WRSPO(1),VDRRSPO(1),  
     &            VDRBED(L,K),VDRBED(L,K),IWRSPB(1))  
            ENDDO  
          !ENDDO  
        ENDIF  
	ENDIF  
C  
C **  IF N=1 AND ISTRAN(5)=1 CHECK INITIAL TOXIC CONCENTRATIONS IN  
C **  BED AND REINITILIZE IF NECESSARY  
C **  CALCULATE TOTAL PARTICULATE FRACTION OF EACH TOXIC IN THE BED  
C            NSP2(NT)=NSED+NSND  
C                  TOXPFB(L,K,NS,NT)=0.  
C              NS=1+NSED+NSND  
C              NS=2+NSED+NSND  
C                TOXPFTB(L,K,NT)=0.  
C                ELSE  
C                  TOXPFTB(L,K,NT)=1.  
C **  CONVERT MASS TOX/MASS SED INITIAL CONDITION TO TOTAL TOXIC  
C **  CONCENTRATION IN BED 0.001 CONVERTS TOXINTB UNITS OF MG/KG  
C **  TO TOXB UNITS OF OF MG/M**2  
C ** DIAGNOSTICS OF INITIALIZATION  
C             TMP1=-999.  
C             TMP2=-999.  
C  
C2222 FORMAT(2I5,7E13.4)  
C  
C **  SAVE OLD VALUES  
C  
      IF(ISTRAN(5).GE.1)THEN  
        DO NT=1,NTOX  
          DO K=1,KC  
            DO L=2,LA  
              TOXS(L,K,NT)=TOX(L,K,NT)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO K=1,KB  
            DO L=2,LA  
              TOXBS(L,K,NT)=TOXB(L,K,NT)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISTRAN(6).GE.1.AND.IWRSP(1).LT.98)THEN  !avoids bed calculation when SEDZLJ is active 
        DO NS=1,NSED  
          DO K=1,KC  
            DO L=2,LA  
              SEDS(L,K,NS)=SED(L,K,NS)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NS=1,NSED  
          DO K=1,KB  
            DO L=2,LA  
              SEDBS(L,K,NS)=SEDB(L,K,NS)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISTRAN(7).GE.1)THEN  
        DO NX=1,NSND  
          DO K=1,KC  
            DO L=2,LA  
              SNDS(L,K,NX)=SND(L,K,NX)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NX=1,NSND  
          DO K=1,KB  
            DO L=2,LA  
              SNDBS(L,K,NX)=SNDB(L,K,NX)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  SET MEAN D50  
C  
      IF(ISTRAN(7).GE.1)THEN  
        DO K=1,KB  
          DO L=2,LA  
            SEDDIA50(L,K)=0.  
            SNDBT(L,K)=0.  
          ENDDO  
        ENDDO  
        DO NX=1,NSND  
          NS=NSED+NX  
          DO K=1,KB  
            DO L=2,LA  
              SEDDIA50(L,K)=SEDDIA50(L,K)+SNDB(L,K,NX)*LOG(SEDDIA(NS))  
              SNDBT(L,K)=SNDBT(L,K)+SNDB(L,K,NX)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO K=1,KB  
          DO L=2,LA  
            IF(SNDBT(L,K).GT.0.)THEN  
              SEDDIA50(L,K)=EXP(SEDDIA50(L,K)/SNDBT(L,K))  
            ELSE  
              SEDDIA50(L,K)=30.*ZBR(L)  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  SET CELL CENTER BED STRESS FOR SEDIMENT RESUSPENSION AND DEPOSITIO  
C  
      IF(ISWAVE.GT.0.AND.IWRSP(1).LT.98)THEN  !avoids calculations if SDEZLJ is active
        DO L=2,LA  
          TAUBC=QQ(L,0)/CTURB2  
          UTMP=0.5*STCUV(L)*(U(L+1,1)+U(L,1))+1.E-12  
          VTMP=0.5*STCUV(L)*(V(LNC(L),1)+V(L,1))  
          CURANG=ATAN2(VTMP,UTMP)  
          TAUB2=TAUBC*TAUBC+0.5*(QQWV2(L)*QQWV2(L))  
     &        +FOURDPI*TAUBC*QQWV2(L)*COS(CURANG-WACCWE(L))  
          TAUB2=MAX(TAUB2,0.)  
          TAUB(L)=SQRT(TAUB2)  
          USTAR(L)=SQRT(TAUB(L))  
        ENDDO  
      ELSEIF(IWRSP(1).LT.98)THEN !avoids calculations if SDEZLJ is active
        DO L=2,LA  
          TAUBC=QQ(L,0)/CTURB2  
          TAUB(L)=TAUBC  
C  
C          TOPTMP=(LOG(2.))**2  
C          BOTTMP=(LOG(0.8*ZBR(L)/DROUGH))**2  
C  
          USTAR(L)=SQRT(TAUB(L))  
        ENDDO  
      ENDIF  
C
      DO L=2,LA
        LN=LNC(L)
        UCELLCTR(L)=0.5*(RSSBCW(L)*WCORWST(L)*U(L,1)
     &                  +RSSBCE(L)*WCOREST(L)*U(L+1,1))
        VCELLCTR(L)=0.5*(RSSBCS(L)*WCORSTH(L)*V(L,1)
     &                  +RSSBCN(L)*WCORNTH(L)*V(LN ,1))
        QCELLCTR(L)=SQRT(UCELLCTR(L)*UCELLCTR(L)
     &                  +VCELLCTR(L)*VCELLCTR(L))
        IF(QCELLCTR(L).GT.0.0) THEN
          UCELLCTR(L)=UCELLCTR(L)/QCELLCTR(L)
          VCELLCTR(L)=VCELLCTR(L)/QCELLCTR(L)
	    CBEDTOTAL(L)=TAUB(L)/(QCELLCTR(L)*QCELLCTR(L))
	  ELSE
          UCELLCTR(L)=0.
          VCELLCTR(L)=0.
	    CBEDTOTAL(L)=0.0
        ENDIF
      ENDDO
C
C **  CALCULATE CELL CENTER DEPTH DEVIATION FROM UNIFORM FLOW
C
      IF(ISBSDFUF.GE.1)THEN
        DO L=2,LA
          LN=LNC(L)
          HDFUFXX=0.5*(RSSBCW(L)*WCORWST(L)*HDFUFX(L)
     &                  +RSSBCE(L)*WCOREST(L)*HDFUFX(L+1))
          IF(HDFUFXX.LT.0.0)HDFUFXX=1.0
          HDFUFYY=0.5*(RSSBCS(L)*WCORSTH(L)*HDFUFY(L)
     &                  +RSSBCN(L)*WCORNTH(L)*HDFUFY(LN ))
          IF(HDFUFYY.LT.0.0)HDFUFYY=1.0
          HDFUF(L)=SQRT(HDFUFXX*HDFUFXX+HDFUFYY*HDFUFYY)
          HDFUF(L)=MIN(HDFUF(L),1.0)
        ENDDO
      ENDIF
C
      IF(KC.GT.1)THEN  
        TMPEXP=16./7.  
        DO L=2,LA  
          LN=LNC(L)  
	    UTMP=0.5*(RSSBCW(L)*WCORWST(L)*UHDYE(L)
     &             +RSSBCE(L)*WCOREST(L)*UHDYE(L+1))/(DYP(L)*HP(L))
          VTMP=0.5*(RSSBCS(L)*WCORSTH(L)*VHDXE(L)
     &             +RSSBCN(L)*WCORNTH(L)*VHDXE(LN ))/(DXP(L)*HP(L))
          QCELLCTRA=SQRT(UTMP*UTMP+VTMP*VTMP)  
          IF(QCELLCTR(L).GT.0.0) THEN  
            QCELLAD1SQ(L)=(QCELLCTRA/QCELLCTR(L))**2  
            QCELLAD1ZZ(L)=(QCELLCTRA/QCELLCTR(L))**TMPEXP  
          ELSE  
            QCELLAD1SQ(L)=1.  
            QCELLAD1ZZ(L)=1.  
          ENDIF  
        ENDDO  
      ELSE  
        DO L=2,LA  
          QCELLAD1SQ(L)=1.  
          QCELLAD1ZZ(L)=1.  
        ENDDO  
      ENDIF  
      DO L=2,LA  
        TAUBSEDS(L)=TAUBSED(L)  
        TAUBSNDS(L)=TAUBSND(L)  
      ENDDO  
      DO L=2,LA  
        TAUBSED(L)=TAUB(L)  
        TAUBSND(L)=TAUB(L)  
        USTARSED(L)=USTAR(L)  
        USTARSND(L)=USTAR(L)  
      ENDDO  
C  
C **  PARTITION BED STRESS BETWEEN TOTAL AND GRAIN STRESS  
C  
      IF(ISBEDSTR.EQ.1.OR.ISBEDSTR.EQ.2)THEN  
        TMPEXP=2./7.  
        RSKSDD50=1.0  
        TMPVAL=1./(COEFTSBL*VISMUDST)
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
C           TVAR3S(L)=0.375E+6*HP(L)*QCELLCTR(L)
            TVAR3S(L)=TMPVAL*HP(L)*QCELLCTR(L)
            TVAR3N(L)=RSKSDD50*SEDDIA50(L,KBT(L))*HPI(L)  
            HGDH(L)=0.0  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            IF(TVAR3S(L).GT.0.0)THEN  
              TVAR3S(L)=1./TVAR3S(L)  
            ENDIF  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            IF(TAUBSEDS(L).GT.0.0)THEN  
              TVAR3E(L)=QCELLCTR(L)/SQRT(TAUBSEDS(L))  
            ELSE  
              TVAR3E(L)=0.0  
            ENDIF  
          ENDIF  
        ENDDO  
C  
C      TVAR3E(L)=VELOCITY_MAGNITUDE/COHESIVE_BEDSTRESS  
C      TVAR3N(L)=D50/DEPTH  
C  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            TVAR3W(L)=TVAR3S(L)**0.333333  
            TVAR3E(L)=TVAR3E(L)**0.333333  
            TVAR3N(L)=TVAR3N(L)**0.333333  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            TVAR3S(L)=TVAR3S(L)**TMPEXP  
          ENDIF  
        ENDDO  
C  
C      TVAR3N(L)=(D50/DEPTH)**1/3  
C  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            IF(CBEDTOTAL(L).GT.0.0)THEN  
              K=KBT(L)  
C              HGDH(L)=0.025*QCELLAD1SQ(L)*(FRACCOH(L,K)*TVAR3W(L)
C     &                   *TVAR3E(L)+FRACNON(L,K)*TVAR3N(L))/CBEDTOTAL(L)  
              HGDH(L)=0.014*HDFUF(L)*QCELLAD1SQ(L)
     &                   *(FRACCOH(L,K)*TVAR3W(L)*TVAR3E(L)
     &                    +FRACNON(L,K)*TVAR3N(L))/CBEDTOTAL(L)
            ENDIF  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            HGDH(L)=HGDH(L)**0.75  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            HGDH(L)=MIN(HGDH(L),1.0)  
          ENDIF  
        ENDDO  
C  
C   CONVERT HGDH TO 1/HGDH  IE HDHG  
C  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            IF(HGDH(L).GT.0.0)THEN  
              HGDH(L)=1./HGDH(L)  
            ENDIF  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            IF(TAUB(L).GT.0.0)THEN  
              TAUBSED(L)=HGDH(L)**TMPEXP  
              TAUBSND(L)=HGDH(L)**0.333333  
              TVAR3W(L)=QCELLCTR(L)*QCELLCTR(L)  
            ENDIF  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            IF(TAUB(L).GT.0.0)THEN  
              TAUBSEDS(L)=TAUBSED(L)  
              TAUBSNDS(L)=TAUBSND(L)  
c          TAUBSED(L)=0.042*TVAR3S(L)*TAUBSED(L)*TVAR3W(L)*QCELLAD1ZZ(L)
c	     TAUBSND(L)=0.025*TVAR3N(L)*TAUBSND(L)*TVAR3W(L)*QCELLAD1SQ(L)
           TAUBSED(L)=0.026*TVAR3S(L)*TAUBSED(L)*TVAR3W(L)*QCELLAD1ZZ(L)
           TAUBSND(L)=0.014*TVAR3N(L)*TAUBSND(L)*TVAR3W(L)*QCELLAD1SQ(L)

            ENDIF  
          ENDIF  
        ENDDO  
C  
C **  IF ISBEDSTR=2, APPLY WEIGHTED AVERAGE TO BOTH SED AND SND  
C  
        IF(ISBEDSTR.EQ.2.OR.N.EQ.1)THEN  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              K=KBT(L)  
              TVAR3E(L)=FRACCOH(L,K)*TAUBSED(L)+FRACNON(L,K)*TAUBSND(L)  
            ENDIF  
          ENDDO  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              TAUBSED(L)=TVAR3E(L)  
              TAUBSND(L)=TVAR3E(L)  
            ENDIF  
          ENDDO  
        ENDIF  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            USTARSED(L)=SQRT(TAUBSED(L))  
            USTARSND(L)=SQRT(TAUBSND(L))  
          ENDIF  
        ENDDO  
      ENDIF  
C *** ENDIF ON GRAIN STRESS PARTITIONING FOR ISBEDSTR.EQ.1.OR.ISBEDSTR.EQ.2
C
C **  INDEPENDENTLY SET GRAIN STRESS
C
      IF(ISBEDSTR.EQ.3)THEN  
C
        DO L=2,LA 
          HDZBR=HP(L)/ZBRSED(L)
          TVAR3E(L)=0.16/( (LOG(HDZBR)-1.)**2 )         
        ENDDO
        DO L=2,LA 
          TAUBSED(L)=TVAR3E(L)*QCELLCTR(L)*QCELLCTR(L)
        ENDDO
        DO L=2,LA 
          TAUBSND(L)=TAUBSED(L)
        ENDDO
        DO L=2,LA 
          USTARSED(L)=SQRT(TAUBSED(L))
          USTARSND(L)=SQRT(TAUBSND(L))
        ENDDO
C
      ENDIF 
C
C**********************************************************************C
C 869 FORMAT(' I,J,HGDH = ',2I5,F10.3)  
      IF(IWRSP(1).LT.98)THEN !do not recalculate bed when SEDZLJ dynamics are active
	  DO L=2,LA  
          HBEDA(L)=0.0  
          DO K=1,KBT(L)  
            HBEDA(L)=HBEDA(L)+HBED(L,K)  
          END DO  
        ENDDO
	ENDIF
C  
C **  CALCULATE COHESIVE SEDIMENT SETTLING, DEPOSITION AND RESUSPENSION  
C  
      IF(ISTRAN(6).GE.1) CALL CALSED  
C  
C **  CALCULATE NONCOHESIVE SEDIMENT BEDLOAD TRANSPORT, SETTLING,  
C **  DEPOSITION AND RESUSPENSION  
C  
      IF(ISTRAN(7).GE.1) CALL CALSND  
C  
C **  CALCULATE PARENT TO ACTIVE LAYER SEDIMENT FLUX  
C  
      DO L=2,LA  
        QWATPA(L)=0.0  
        QSSDPA(L)=0.0  
      ENDDO  
      IF(ISNDAL.EQ.2)THEN  
C  
        IF(IALTYP.EQ.0)THEN  
          DO NS=1,NSED  
            DSEDGMM=1./(1.E6*SSG(NS))  
            DSEDGMMI=1.E6*SSG(NS)  
            DO L=2,LA  
              KTOPTP=KBT(L)  
              KTOPM1=KBT(L)-1  
              QSWPOS=(QSBDTOP(L)+QWBDTOP(L))/(1.+VDRBED1(L,KTOPM1))  
              QSWNEG=(QSBDTOP(L)+QWBDTOP(L))/(1.+VDRBED1(L,KTOPTP))  
              SEDFPA(L,NS)=DSEDGMMI*(VFRBED(L,KTOPM1,NS)*MAX(QSWPOS,0.)  
     &            +VFRBED(L,KTOPTP,NS)*MIN(QSWNEG,0.))  
              QSSDPA(L)=QSSDPA(L)+DSEDGMM*SEDFPA(L,NS)  
              QWATPA(L)=QWATPA(L)+DSEDGMM*  
     &            ( VDRBED(L,KTOPM1)*MAX(SEDFPA(L,NS),0.)  
     &            +VDRBED(L,KTOPTP)*MIN(SEDFPA(L,NS),0.))  
              SEDB(L,KTOPTP,NS)=SEDB(L,KTOPTP,NS)+DELT*SEDFPA(L,NS)  
              SEDB(L,KTOPM1,NS)=SEDB(L,KTOPM1,NS)-DELT*SEDFPA(L,NS)  
            ENDDO  
          ENDDO  
          DO NX=1,NSND  
            NS=NX+NSED  
            DSEDGMM=1./(1.E6*SSG(NS))  
            DSEDGMMI=1.E6*SSG(NS)  
            DO L=2,LA  
              KTOPTP=KBT(L)  
              KTOPM1=KBT(L)-1  
              QSWPOS=(QSBDTOP(L)+QWBDTOP(L))/(1.+VDRBED1(L,KTOPM1))  
              QSWNEG=(QSBDTOP(L)+QWBDTOP(L))/(1.+VDRBED1(L,KTOPTP))  
              SNDFPA(L,NX)=DSEDGMMI*(VFRBED(L,KTOPM1,NS)*MAX(QSWPOS,0.)  
     &            +VFRBED(L,KTOPTP,NS)*MIN(QSWNEG,0.))  
              QSSDPA(L)=QSSDPA(L)+DSEDGMM*SNDFPA(L,NX)  
              QWATPA(L)=QWATPA(L)+DSEDGMM*  
     &            ( VDRBED(L,KTOPM1)*MAX(SNDFPA(L,NX),0.)  
     &            +VDRBED(L,KTOPTP)*MIN(SNDFPA(L,NX),0.))  
              SNDB(L,KTOPTP,NX)=SNDB(L,KTOPTP,NX)+DELT*SNDFPA(L,NX)  
              SNDB(L,KTOPM1,NX)=SNDB(L,KTOPM1,NX)-DELT*SNDFPA(L,NX)  
            ENDDO  
          ENDDO  
        ELSE  
          DO NS=1,NSED  
            DSEDGMM=1./(1.E6*SSG(NS))  
            DSEDGMMI=1.E6*SSG(NS)  
            DO L=2,LA  
              KTOPTP=KBT(L)  
              KTOPM1=KBT(L)-1  
              SEDFPA(L,NS)=VFRBED(L,KTOPM1,NS)*MAX(QSBDTOP(L),0.)  
     &            +VFRBED(L,KTOPTP,NS)*MIN(QSBDTOP(L),0.)  
              QSSDPA(L)=QSSDPA(L)+SEDFPA(L,NS)  
              QWATPA(L)=QWATPA(L)+VDRBED(L,KTOPM1)*MAX(SEDFPA(L,NS),0.)  
     &            +VDRBED(L,KTOPTP)*MIN(SEDFPA(L,NS),0.)  
              SEDFPA(L,NS)=DSEDGMMI*SEDFPA(L,NS)  
              SEDB(L,KTOPTP,NS)=SEDB(L,KTOPTP,NS)+DELT*SEDFPA(L,NS)  
              SEDB(L,KTOPM1,NS)=SEDB(L,KTOPM1,NS)-DELT*SEDFPA(L,NS)  
            ENDDO  
          ENDDO  
          DO NX=1,NSND  
            NS=NX+NSED  
            DSEDGMM=1./(1.E6*SSG(NS))  
            DSEDGMMI=1.E6*SSG(NS)  
            DO L=2,LA  
              KTOPTP=KBT(L)  
              KTOPM1=KBT(L)-1  
              SNDFPA(L,NX)=VFRBED(L,KTOPM1,NS)*MAX(QSBDTOP(L),0.)  
     &            +VFRBED(L,KTOPTP,NS)*MIN(QSBDTOP(L),0.)  
              QSSDPA(L)=QSSDPA(L)+SNDFPA(L,NX)  
              QWATPA(L)=QWATPA(L)+VDRBED(L,KTOPM1)*MAX(SNDFPA(L,NX),0.)  
     &            +VDRBED(L,KTOPTP)*MIN(SNDFPA(L,NX),0.)  
              SNDFPA(L,NX)=DSEDGMMI*SNDFPA(L,NX)  
              SNDB(L,KTOPTP,NX)=SNDB(L,KTOPTP,NX)+DELT*SNDFPA(L,NX)  
              SNDB(L,KTOPM1,NX)=SNDB(L,KTOPM1,NX)-DELT*SNDFPA(L,NX)  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C8669 FORMAT('PA ERR ',I10,F10.5,8E14.6)  
C  
C **  UPDATE TOP BED LAYER THICKNESS AND VOID RATIO  
C **  FOR DEPOSITION-RESUSPENSION STEP  
C  
      DO L=2,LA  
        K=KBT(L)  
        HBED1(L,K)=HBED(L,K)  
        VDRBED1(L,K)=VDRBED(L,K)  
        QWTRBEDA(L)=QSBDTOP(L)  
        QWTRBEDA1(L)=QWBDTOP(L)  
        HBED(L,K)=HBED(L,K)-DELT*(QSBDTOP(L)+QWBDTOP(L))  
     &      +DELT*(QSSDPA(L)+QWATPA(L))  
        ! *** PMC BEGIN BLOCK
        IF(K.EQ.1)THEN  
          IF(HBED(L,K).LT.1.E-5)THEN  
            ! *** ZERO NEGATIVE THICKNESSES
            HBED(L,K)=0.0  
            VDRBED(L,K)=0.0  
            PORBED(L,K)=0.0  
            STDOCB(L,K)=0.0  
            STPOCB(L,K)=0.0  
          ENDIF
        ENDIF
        ! *** PMC END BLOCK  
        TMPVAL=HBED1(L,K)/(1.+VDRBED1(L,K))  
        TMPVAL=TMPVAL-DELT*(QSBDTOP(L)-QSSDPA(L))  
        IF(TMPVAL.GT.0.0) THEN  
          VDRBED(L,K)=(HBED(L,K)/TMPVAL)-1. 
          ! *** PMC BEGIN
          IF(K.EQ.1)THEN  
            IF(VDRBED(L,K).LT.0.01.OR.VDRBED(L,K).GT.99.)THEN  
              VDRBED(L,K)=0.0  ! *** LIMIT VOID RATIONS TO 0.01>=N<=.99
            ENDIF
          ENDIF  
          ! *** PMC END
        ELSE  
          VDRBED(L,K)=0.0  
        ENDIF  
      ENDDO  
C  
C **  UPDATE PARENT LAYER BED LAYER THICKNESS AND VOID RATIO  
C **  FOR DEPOSITION-RESUSPENSION STEP  
C  
      IF(ISNDAL.EQ.2)THEN  
        DO L=2,LA  
          K=KBT(L)-1  
          HBED1(L,K)=HBED(L,K)  
          VDRBED1(L,K)=VDRBED(L,K)  
          HBED(L,K)=HBED(L,K)-DELT*(QSSDPA(L)+QWATPA(L))  
          ! *** PMC BEGIN BLOCK
          IF(K.EQ.1)THEN  
            IF(HBED(L,K).LT.0.0)THEN  
              ! *** ZERO NEGATIVE THICKNESSES
              HBED(L,K)=0.0  
              VDRBED(L,K)=0.0  
              PORBED(L,K)=0.0  
              STDOCB(L,K)=0.0  
              STPOCB(L,K)=0.0  
            ENDIF
          ENDIF
          ! *** PMC END BLOCK  
          TMPVAL=HBED1(L,K)/(1.+VDRBED1(L,K))  
          TMPVAL=TMPVAL-DELT*QSSDPA(L)  
          IF(TMPVAL.GT.0.0) THEN  
            VDRBED(L,K)=(HBED(L,K)/TMPVAL)-1.  
          ELSE  
            VDRBED(L,K)=0.0  
          ENDIF  
        ENDDO  
      ENDIF  
      IF(ISNDAL.EQ.0.AND.KB.GT.1)THEN  
        DO K=1,KB  
          DO L=2,LA  
            IF(K.LT.KBT(L))THEN  
              HBED1(L,K)=S3TL*HBED(L,K)+S2TL*HBED1(L,K)  
              VDRBED1(L,K)=S3TL*VDRBED(L,K)+S2TL*VDRBED1(L,K)  
            ENDIF  
          ENDDO  
        ENDDO  
      ELSE  
        DO K=1,KB  
          DO L=2,LA  
            IF(K.LT.KBT(L)-1)THEN  
              HBED1(L,K)=S3TL*HBED(L,K)+S2TL*HBED1(L,K)  
              VDRBED1(L,K)=S3TL*VDRBED(L,K)+S2TL*VDRBED1(L,K)  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE TOXIC SETTLING, DEPOSITION AND RESUSPENSION  
C  
      IF(ISTRAN(5).GT.0)CALL CALTOX  
C  
C **  UPDATE TOP BED LAYER THICKNESS AND VOID RATIO  
C **  FOR DEPOSITION-RESUSPENSION STEP  
C **  PRESENTLY ACTIVE BEFORE THE WATER COLUMN-BED TOXICS EXCHANGE  
C **  CHECK PLACEMENT THERE AND HERE FOR  
C        VDRTMP=(TMPVAL/HBED(L,K))-1.  
C        VDRBED(L,K)=VDRTMP  
C **  UPDATE SEDIMENT BED LAYERING  
C  
      IF(IWRSP(1).LT.98)THEN !  added this to avoid bed calcs when SEDZLJ routines are active
      IF(KB.GT.1)CALL CALBLAY
      ENDIF
C  
C **  UPDATE SEDIMENT BED PHYSICAL PROPERTIES  
C  
      IF(IBMECH.EQ.9)THEN  
         CALL CALBED9  
      ELSE  
         CALL CALBED  
      ENDIF  
C  
C ++  CHANGE BED MORPHOLOGY  
C  
      IF(IMORPH.GT.0.OR.ISGWIT.EQ.2)THEN  ! *** DSLLC
        DO L=2,LA  
          TVAR3S(L)=HBEDA(L)  
          BELV1(L)=BELV(L)  
          HTMP(L)=HP(L)  
          H1P(L)=HP(L)  
          P1(L)=P(L)  
        ENDDO  
        DO L=2,LA  
          HBEDA(L)=0.0  
          DELBED(L)=TVAR3S(L)  
        ENDDO  
        DO K=1,KB  
          DO L=2,LA  
            IF(K.LE.KBT(L))THEN  
              HBEDA(L)=HBEDA(L)+HBED(L,K)  
            ENDIF  
          END DO  
        ENDDO  
        DO L=2,LA  
          IF(HBEDA(L).NE.DELBED(L))THEN  
            DELBED(L)=DELBED(L)-HBEDA(L)  
          ELSE  
            DELBED(L)=0.0  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          BELV(L)=ZELBEDA(L)+HBEDA(L)  
        ENDDO  
        IF(ISGWIT.EQ.2)THEN  ! *** DSLLC
          DO L=2,LA  
            HP(L)=HP(L)+DELBED(L)+DELT*QGW(L)*DXYIP(L)  
            P(L)=P(L)+G*DELT*QGW(L)*DXYIP(L)  
          ENDDO  
        ELSE  
          DO L=2,LA  
            HP(L)=HP(L)+DELBED(L)  
          ENDDO  
        ENDIF  
        DO L=2,LA  
          HPI(L)=1./HP(L)  
          QMORPH(L)=DELTI*DXYP(L)*(HP(L)-H1P(L))  
        ENDDO  
        ITMP=0  
        DO L=2,LA  
          IF(HP(L).LT.0.0)THEN  
            IF(ABS(H1P(L)).GT.HWET)THEN
              IF(MYRANK.EQ.0)THEN
              WRITE(8,2348)TIMEDAY,IL(L),JL(L),HBED1(L,KBT(L)),
     &            HBED(L,KBT(L)),BELV1(L),BELV(L),DELT 
            ENDIF
            ENDIF
            IF(ABS(H1P(L)).GE.HADJ)THEN  ! PMC-WAS HWET
              ITMP=1  
              IF(MYRANK.EQ.0)THEN
              WRITE(8,2345)IL(L),JL(L),HBED1(L,KBT(L)),HBED(L,KBT(L)),  
     &            BELV1(L),BELV(L),DELT,QSBDTOP(L),QWBDTOP(L),HBEDA(L)  
              WRITE(8,2347)L,KBT(L),(HBED(L,K),K=1,KBT(L))  
              ENDIF
            ELSE  
              HP(L)=0.9*HDRY  
            ENDIF  
          ENDIF  
        ENDDO  
        IF(ITMP.EQ.1)THEN  
          CALL RESTOUT(1)  
          IF(NDRYSTP.LT.0.AND.DEBUG.AND.MYRANK.EQ.0) THEN
            OPEN(1,FILE='DRYLOSS.OUT')
            CLOSE(1,STATUS='DELETE')
            OPEN(1,FILE='DRYLOSS.OUT')
            DO L=2,LA
	        IF(VDWASTE(L).GT.0.0)THEN
	          TMPVAL=VDWASTE(L)/DXYP(L)
                WRITE(1,1993)IL(L),JL(L),VDWASTE(L),TMPVAL,QDWASTE(L)
              ENDIF
            ENDDO
            CLOSE(1)
          ENDIF
          STOP  
        ENDIF  
      ENDIF  
 2345 FORMAT('NEG DEPTH DUE TO MORPH CHANGE', 2I5,12F12.5)
 2347 FORMAT('                             ', 2I5,12F12.5)
 2348 FORMAT('WITHIN TOLERANCE MORPH CHANGE NEG DEPTH',F10.5,2I5,5F12.5)
C2346 FORMAT('MORP ERR ',2I5,6E15.6)
 1993 FORMAT(2I6,4E14.6)
C  
C ++  ADJUST CONCENTRATIONS OF TRANSPORT VARIABLES IN RESPONSE TO  
C ++  CHANGE IN BED MORPHOLOGY  
C  
      IF(IMORPH.GT.0.OR.ISGWIT.EQ.2)THEN  ! *** DSLLC
        IF(ISTRAN(1).GT.0)THEN  
          DO K=1,KC  
            DO L=2,LA  
              SAL(L,K)=HTMP(L)*SAL(L,K)/HP(L)  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(2).GT.0)THEN  
          DO K=1,KC  
            DO L=2,LA  
              TEM(L,K)=HTMP(L)*TEM(L,K)/HP(L)  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(3).GT.0)THEN  
          DO K=1,KC  
            DO L=2,LA  
              DYE(L,K)=HTMP(L)*DYE(L,K)/HP(L)  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(4).GT.0)THEN  
          DO K=1,KC  
            DO L=2,LA  
              SFL(L,K)=HTMP(L)*SFL(L,K)/HP(L)  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(5).GT.0)THEN  
          DO NT=1,NTOX  
            DO K=1,KC  
              DO L=2,LA  
                TOX(L,K,NT)=HTMP(L)*TOX(L,K,NT)/HP(L)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(6).GT.0)THEN  
          DO NS=1,NSED  
            DO K=1,KC  
              DO L=2,LA  
                SED(L,K,NS)=HTMP(L)*SED(L,K,NS)/HP(L)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(7).GT.0)THEN  
          DO NS=1,NSND  
            DO K=1,KC  
              DO L=2,LA  
                SND(L,K,NS)=HTMP(L)*SND(L,K,NS)/HP(L)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  POREWATER ADVECTION AND DIFFUSION OF TOXICS  
C  

!{GeoSR, YSSONG, TOXIC, 101029
      IF(ISTOXB.EQ.1)THEN
       IF(ISTRAN(5).GT.0)CALL CALTOXB  
      ENDIF
C      IF(ISTRAN(5).GT.0)CALL CALTOXB  
! GeoSR, YSSONG, TOXIC, 101029}

C  
C **  TOXIC CONTAMINANT REACTIONS  
C  
      IF(ISTRAN(5).GE.1)CALL TOXCHEM  
C
C**********************************************************************C
C
C **  TOXIC CONTAMINANT OUTPUT TO FOOD CHAIN MODEL
C
      IF(ISTRAN(5).GE.1.AND.ISFDCH.GE.1)CALL FOODCHAIN(0)
C
C**********************************************************************C
C
C8800 FORMAT(I5,8E14.5)  
      CLOSE(1)  
      CLOSE(11)  
      CLOSE(21)  
      CLOSE(31)  
      CLOSE(41)  
C  
C *** EE BEGIN BLOCK  
C  
      CALL CPU_TIME(T2TMP)
      TSSED=TSSED+T2TMP-T1TMP  
C  
C *** EE END BLOCK  
C  
      RETURN  
      END  

