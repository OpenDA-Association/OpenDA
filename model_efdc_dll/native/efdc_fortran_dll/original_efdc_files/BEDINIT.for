      SUBROUTINE BEDINIT  
C  
C **  SUBROUTINE BEDINIT INITIALIZES SEDIMENT AND TOXIC VARIABLES  
C **  IT SEDIMENT BED FOR HOT AND COLD START CONDITIONS  
C CHANGE RECORD  
C  ADDED ADDITIONAL DIAGNOSTIC OUTPUT  
C  MOVED TOXIC INITIALIZATIONS FROM SSEDTOX  
C  
      USE GLOBAL  

 	IMPLICIT NONE
	INTEGER::K,L,NS,NX,NT,KTOPP1,IVAL,KTOPTP,IHOTSTRT
	REAL::CSEDTAUS,CSEDRESS,CSEDTAUB,CSEDRESB,TMPCVT
	REAL::SURF,FVOLSSD,FVOLSED,FVOLSND,TMP1

      REAL,ALLOCATABLE,DIMENSION(:)::FRACACT  
      REAL,ALLOCATABLE,DIMENSION(:)::FRACPAR  
      ALLOCATE(FRACACT(LCM))  
      ALLOCATE(FRACPAR(LCM))  
C  
C **  DETERMINE START UP MODE  
C  
      ! *** DSLLC BEGIN BLOCK
      ! *** ZERO LOCAL ARRAYS
      FRACACT=0.0
      FRACPAR=0.0
      ! *** DSLLC END BLOCK

      IHOTSTRT=0  
      IF(ISRESTI.NE.0)THEN  
        IF(ISCI(6).NE.0.OR.ISCI(7).NE.0)THEN  
          IHOTSTRT=1  
        ENDIF  
      ENDIF  
C  
C **  HOT START INITIALIZATION  
C  
      IF(IHOTSTRT.NE.0)THEN  
C  
C **  SET POROSITY  
C  
        DO K=1,KB  
          DO L=2,LA  
            PORBED(L,K)=VDRBED(L,K)/(1.+VDRBED(L,K))  
            PORBED1(L,K)=VDRBED1(L,K)/(1.+VDRBED1(L,K))  
          ENDDO  
        ENDDO  
C  
C **  SET BULK DENSITY  
C  
        DO K=1,KB  
          DO L=2,LA  
            SEDBT(L,K)=0.  
            SNDBT(L,K)=0.  
          ENDDO  
        ENDDO  
        IF(ISTRAN(6).GE.1)THEN  
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
          DO L=2,LA  
            IF(HBED(L,K).GT.0.)THEN  
              BDENBED(L,K)=1000.*PORBED(L,K)  
     &            +0.001*(SEDBT(L,K)+SNDBT(L,K))/HBED(L,K)  
            ELSE  
              BDENBED(L,K)=0.  
            ENDIF  
          ENDDO  
        ENDDO  
        DO K=1,KB  
          DO L=2,LA  
            SEDBT(L,K)=0.  
            SNDBT(L,K)=0.  
          ENDDO  
        ENDDO  
        IF(ISTRAN(6).GE.1)THEN  
          DO NS=1,NSED  
            DO K=1,KB  
              DO L=2,LA  
                SEDBT(L,K)=SEDBT(L,K)+SEDB1(L,K,NS)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(7).GE.1)THEN  
          DO NS=1,NSND  
            DO K=1,KB  
              DO L=2,LA  
                SNDBT(L,K)=SNDBT(L,K)+SNDB1(L,K,NS)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
        DO K=1,KB  
          DO L=2,LA  
            IF(HBED1(L,K).GT.0.)THEN  
              BDENBED1(L,K)=1000.*PORBED1(L,K)  
     &            +0.001*(SEDBT(L,K)+SNDBT(L,K))/HBED1(L,K)  
            ELSE  
              BDENBED1(L,K)=0.  
            ENDIF  
          ENDDO  
        ENDDO  
C  
C **  SET TOP BED LAYER  
C  
        DO L=2,LA  
          KBT(L)=1  
        ENDDO  
        DO K=1,KB  
          DO L=2,LA  
            IF(HBED(L,K).GT.0.)KBT(L)=K  
          ENDDO  
        ENDDO  
C  
C **  SET COHESIVE BED CRITICAL STRESSES AND RESUSPENSION RATES  
C  
        IF(ISTRAN(6).GE.1.AND.IWRSP(1)/=98)THEN  ! Avoids bed calculation when SEDZLJ is active  
          IF(IWRSP(1).EQ.0)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURS(L,K)=TAUR(1)  
                WRSPS(L,K)=WRSPO(1)  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(IWRSPB(1).EQ.0)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURB(L,K)=1.E6  
                WRSPB(L,K)=0.0  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(IWRSP(1).GE.1)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURS(L,K)=CSEDTAUS(BDENBED(L,K),TAUR(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSP(1),L)  
                WRSPS(L,K)=CSEDRESS(BDENBED(L,K),WRSPO(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSP(1))  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(IWRSPB(1).GE.1)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURB(L,K)=CSEDTAUB(BDENBED(L,K),TAUR(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSPB(1))  
                WRSPB(L,K)=CSEDRESB(BDENBED(L,K),WRSPO(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSPB(1))  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDIF  
C  
C **  SET SEDIMENT VOLUME FRACTIONS  
C  
        DO K=1,KB  
          DO L=2,LA  
            BEDLINIT(L,K)=0.  
            BEDDINIT(L,K)=0.  
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
        IF(ISTRAN(6).GE.1)THEN  
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
        IF(ISTRAN(6).GE.1)THEN  
          DO NS=1,NSED  
            DO K=1,KB  
              DO L=2,LA
                ! *** BEGIN DSLLC
                IF(BEDLINIT(L,K).GT.0.0)THEN
                  VFRBED(L,K,NS)=VFRBED(L,K,NS)/BEDLINIT(L,K) 
                ELSE  
                  VFRBED(L,K,NS)=0.0
                  BEDLINIT(L,K) =0.0  
                ENDIF  
                IF(BEDDINIT(L,K).GT.0.0)THEN  
                  VFRBED1(L,K,NS)=VFRBED1(L,K,NS)/BEDDINIT(L,K)  
                ELSE  
                  VFRBED1(L,K,NS)=0.0  
                  BEDDINIT(L,K) =0.0  
                ENDIF  
                ! *** END DSLLC
              ENDDO
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(7).GE.1)THEN  
          DO NX=1,NSND  
            NS=NSED+NX  
            DO K=1,KB  
              DO L=2,LA  
                ! *** BEGIN DSLLC
                IF(BEDLINIT(L,K).GT.0.0)THEN
                  VFRBED(L,K,NS)=VFRBED(L,K,NS)/BEDLINIT(L,K) 
                ELSE  
                  VFRBED(L,K,NS)=0.0
                  BEDLINIT(L,K) =0.0  
                ENDIF  
                IF(BEDDINIT(L,K).GT.0.0)THEN  
                  VFRBED1(L,K,NS)=VFRBED1(L,K,NS)/BEDDINIT(L,K)  
                ELSE  
                  VFRBED1(L,K,NS)=0.0  
                  BEDDINIT(L,K) =0.0  
                ENDIF  
                ! *** END DSLLC
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
C  
C**  INITIALIZE BED BOTTOM ELEVATION  
C  
        DO L=2,LA  
          HBEDA(L)=0.  
        ENDDO  
        DO L=2,LA  
          DO K=1,KBT(L)  
            HBEDA(L)=HBEDA(L)+HBED(L,K)  
          END DO  
        ENDDO  
        DO L=2,LA  
          ZELBEDA(L)=BELV(L)-HBEDA(L)  
        ENDDO  
C  
C**  INITIALIZE TOTAL SEDIMENT MASS PER UNIT AREA  
C  
        DO K=1,KB  
          DO L=2,LA  
            SEDBT(L,K)=0.  
            SNDBT(L,K)=0.  
          ENDDO  
        ENDDO  
        IF(ISTRAN(6).GE.1)THEN  
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
        GOTO 1000  
      ENDIF  
C  
C **  END HOT START INITIALIZATION  
C **  COLD START INITIALIZATION: IBMECH=0  
C  
      IF(IBMECH.EQ.0)THEN  
C  
C **  SET POROSITY AND VOID RATIO  
C  
        DO K=1,KB  
          DO L=2,LA  
            PORBED(L,K)=BEDPORC  
            PORBED1(L,K)=BEDPORC  
            VDRBED(L,K)=PORBED(L,K)/(1.-PORBED(L,K))  
            VDRBED1(L,K)=PORBED1(L,K)/(1.-PORBED1(L,K))  
            HBED(L,K)=0.  
            HBED1(L,K)=0.  
            KBT(L)=1  
          ENDDO  
        ENDDO  
C  
C **  UNIFORM SEDIMENT MASS PER UNIT AREA ALL CELLS, ALL BED LAYERS  
C **  CALCULATE LAYER THICKNESS AND BULK DENSITY  
C  
        IF(ISEDINT.LE.1)THEN  
          DO K=1,KB  
            DO L=2,LA  
              SEDBT(L,K)=0.  
              SNDBT(L,K)=0.  
            ENDDO  
          ENDDO  
          IF(ISTRAN(6).GE.1)THEN  
            DO NS=1,NSED  
              DO K=1,KB  
                DO L=2,LA  
                  HBED(L,K)=HBED(L,K)+SDEN(NS)*SEDB(L,K,NS)  
                  SEDBT(L,K)=SEDBT(L,K)+SEDB(L,K,NS)  
                ENDDO  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(ISTRAN(7).GE.1)THEN  
            DO NX=1,NSND  
              NS=NSED+NX  
              DO K=1,KB  
                DO L=2,LA  
                  HBED(L,K)=HBED(L,K)+SDEN(NS)*SNDB(L,K,NX)  
                  SNDBT(L,K)=SNDBT(L,K)+SNDB(L,K,NX)  
                ENDDO  
              ENDDO  
            ENDDO  
          ENDIF  
          DO K=1,KB  
            DO L=2,LA  
              HBED(L,K)=(1.+VDRBED(L,K))*HBED(L,K)  
              IF(HBED(L,K).GT.0.) KBT(L)=K  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(HBED(L,K).GT.0.)THEN  
                BDENBED(L,K)=1000.*PORBED(L,K)  
     &              +0.001*(SEDBT(L,K)+SNDBT(L,K))/HBED(L,K)  
              ELSE  
                BDENBED(L,K)=0.  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              HBED1(L,K)=HBED(L,K)  
              BDENBED1(L,K)=BDENBED(L,K)  
            ENDDO  
          ENDDO  
        ENDIF  
C  
C **  NONUNIFORM SEDIMENT MASS PER UNIT AREA ALL CELLS, ALL BED LAYERS  
C **  AND INITIAL CONDITIONS ARE IN MASS PER UNIT AREA  
C **  CALCULATE LAYER THICKNESS AND BULK DENSITY  
C  
        IF(ISEDINT.GE.2)THEN  
          IF(ISEDBINT.EQ.0)THEN  
            DO K=1,KB  
              DO L=2,LA  
                SEDBT(L,K)=0.  
                SNDBT(L,K)=0.  
              ENDDO  
            ENDDO  
            IF(ISTRAN(6).GE.1)THEN  
              DO NS=1,NSED  
                DO K=1,KB  
                  DO L=2,LA  
                    HBED(L,K)=HBED(L,K)+SDEN(NS)*SEDB(L,K,NS)  
                    SEDBT(L,K)=SEDBT(L,K)+SEDB(L,K,NS)  
                  ENDDO  
                ENDDO  
              ENDDO  
            ENDIF  
            IF(ISTRAN(7).GE.1)THEN  
              DO NX=1,NSND  
                NS=NSED+NX  
                DO K=1,KB  
                  DO L=2,LA  
                    HBED(L,K)=HBED(L,K)+SDEN(NS)*SNDB(L,K,NX)  
                    SNDBT(L,K)=SNDBT(L,K)+SNDB(L,K,NX)  
                  ENDDO  
                ENDDO  
              ENDDO  
            ENDIF  
            DO K=1,KB  
              DO L=2,LA  
                HBED(L,K)=(1.+VDRBED(L,K))*HBED(L,K)  
                IF(HBED(L,K).GT.0.) KBT(L)=K  
              ENDDO  
            ENDDO  
            DO K=1,KB  
              DO L=2,LA  
                IF(HBED(L,K).GT.0.)THEN  
                  BDENBED(L,K)=1000.*PORBED(L,K)  
     &                +0.001*(SEDBT(L,K)+SNDBT(L,K))/HBED(L,K)  
                ELSE  
                  BDENBED(L,K)=0.  
                ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KB  
              DO L=2,LA  
                HBED1(L,K)=HBED(L,K)  
                BDENBED1(L,K)=BDENBED(L,K)  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDIF  
C  
C **  NONUNIFORM SEDIMENT MASS PER UNIT AREA ALL CELLS, ALL BED LAYERS  
C **  AND INITIAL CONDITIONS ARE IN MASS FRACTION  
C **  CALCULATE LAYER THICKNESS AND BULK DENSITY  
C **  THIS OPTION REQUIRES INITIAL LAYER THICKNESSES  
C  
        IF(ISEDINT.GE.2.AND.IWRSP(1)/=98)THEN  ! Avoids bed calculation when SEDZLJ is active  
          IF(ISEDBINT.EQ.1)THEN  
            IF(IBEDLAYU.EQ.1)THEN  
              DO K=1,KB  
                DO L=2,LA  
                  BEDLINIT(L,K)=0.1*BEDLINIT(L,K)  
                ENDDO  
              ENDDO  
            ENDIF  
            DO K=1,KB  
              DO L=2,LA  
                HBED(L,K)=BEDLINIT(L,K)  
                HBED1(L,K)=BEDLINIT(L,K)  
                IF(HBED(L,K).GT.0.)KBT(L)=K  
              ENDDO  
            ENDDO  
            DO K=1,KB  
              DO L=2,LA  
                BDENBED(L,K)=0.  
              ENDDO  
            ENDDO  
            IF(ISTRAN(6).GE.1)THEN  
              DO NS=1,NSED  
                DO K=1,KB  
                  DO L=2,LA  
                    BDENBED(L,K)=BDENBED(L,K)  
     &                  +1000.*SSG(NS)*SEDB(L,K,NS)  
                  ENDDO  
                ENDDO  
              ENDDO  
            ENDIF  
            IF(ISTRAN(7).GE.1)THEN  
              DO NX=1,NSND  
                NS=NSED+NX  
                DO K=1,KB  
                  DO L=2,LA  
                    BDENBED(L,K)=BDENBED(L,K)  
     &                  +1000.*SSG(NS)*SNDB(L,K,NX)  
                  ENDDO  
                ENDDO  
              ENDDO  
            ENDIF  
            DO K=1,KB  
              DO L=2,LA  
                BDENBED(L,K)=1000.*PORBED(L,K)  
     &              +(1.-PORBED(L,K))*BDENBED(L,K)  
                BDENBED1(L,K)=BDENBED(L,K)  
              ENDDO  
            ENDDO  
            DO K=1,KB  
              DO L=2,LA  
                SEDBT(L,K)=1000.*HBED(L,K)*(BDENBED(L,K)  
     &              -1000.*PORBED(L,K))  
              ENDDO  
            ENDDO  
            IF(ISTRAN(6).GE.1)THEN  
              DO NS=1,NSED  
                DO K=1,KB  
                  DO L=2,LA  
                    SEDB(L,K,NS)=SEDB(L,K,NS)*SEDBT(L,K)  
                    SEDB1(L,K,NS)=SEDB(L,K,NS)  
                  ENDDO  
                ENDDO  
              ENDDO  
            ENDIF  
            IF(ISTRAN(7).GE.1)THEN  
              DO NX=1,NSND  
                DO K=1,KB  
                  DO L=2,LA  
                    SNDB(L,K,NX)=SNDB(L,K,NX)*SEDBT(L,K)  
                    SNDB1(L,K,NX)=SNDB(L,K,NX)  
                  ENDDO  
                ENDDO  
              ENDDO  
            ENDIF  
          ENDIF  
        ENDIF  
C  
C **  SET COHESIVE BED CRITICAL STRESSES AND RESUSPENSION RATES  
C  
        IF(ISTRAN(6).GE.1.AND.IWRSP(1)/=98)THEN  
          IF(IWRSP(1).EQ.0)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURS(L,K)=TAUR(1)  
                WRSPS(L,K)=WRSPO(1)  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(IWRSPB(1).EQ.0)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURB(L,K)=1.E6  
                WRSPB(L,K)=0.0  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(IWRSP(1).GE.1)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURS(L,K)=CSEDTAUS(BDENBED(L,K),TAUR(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSP(1),L)  
                WRSPS(L,K)=CSEDRESS(BDENBED(L,K),WRSPO(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSP(1))  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(IWRSPB(1).GE.1)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURB(L,K)=CSEDTAUB(BDENBED(L,K),TAUR(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSPB(1))  
                WRSPB(L,K)=CSEDRESB(BDENBED(L,K),WRSPO(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSPB(1))  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDIF  
C  
C **  SET SEDIMENT VOLUME FRACTIONS  
C  
        IF(IWRSP(1)/=98)THEN  ! Avoids bed calculation when SEDZLJ is active
	    DO K=1,KB  
            DO L=2,LA  
              BEDLINIT(L,K)=0.  
              BEDDINIT(L,K)=0.  
            ENDDO  
          ENDDO  
        ENDIF
	  IF(ISTRAN(6).GE.1.AND.IWRSP(1)/=98)THEN  ! Avoids bed calculation when SEDZLJ is active  
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
        IF(ISTRAN(6).GE.1.AND.IWRSP(1)/=98)THEN  ! Avoids bed calculation when SEDZLJ is active  
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
        IF(ISTRAN(6).GE.1)THEN  
          DO NS=1,NSED  
            DO K=1,KB  
              DO L=2,LA  
                ! *** BEGIN DSLLC
                IF(BEDLINIT(L,K).GT.0.0)THEN
                  VFRBED(L,K,NS)=VFRBED(L,K,NS)/BEDLINIT(L,K) 
                ELSE  
                  VFRBED(L,K,NS)=0.0
                  BEDLINIT(L,K) =0.0  
                ENDIF  
                IF(BEDDINIT(L,K).GT.0.0)THEN  
                  VFRBED1(L,K,NS)=VFRBED1(L,K,NS)/BEDDINIT(L,K)  
                ELSE  
                  VFRBED1(L,K,NS)=0.0  
                  BEDDINIT(L,K) =0.0  
                ENDIF  
                ! *** END DSLLC
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(7).GE.1)THEN  
          DO NX=1,NSND  
            NS=NSED+NX  
            DO K=1,KB  
              DO L=2,LA  
                ! *** BEGIN DSLLC
                IF(BEDLINIT(L,K).GT.0.0)THEN
                  VFRBED(L,K,NS)=VFRBED(L,K,NS)/BEDLINIT(L,K) 
                ELSE  
                  VFRBED(L,K,NS)=0.0
                  BEDLINIT(L,K) =0.0  
                ENDIF  
                IF(BEDDINIT(L,K).GT.0.0)THEN  
                  VFRBED1(L,K,NS)=VFRBED1(L,K,NS)/BEDDINIT(L,K)  
                ELSE  
                  VFRBED1(L,K,NS)=0.0  
                  BEDDINIT(L,K) =0.0  
                ENDIF  
                ! *** END DSLLC
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  END COLD START INITIALIZATION: IBMECH=0  
C **  COLD START INITIALIZATION: IBMECH.GE.1  
C  
      IF(IBMECH.GE.1.AND.IWRSP(1)/=98)THEN  ! Avoids bed mechanics when SEDZLJ is on
C  
C **  CONVERT AND INITIALIZE BED LAYER THICKNESS AND DEFINE  
C **  INITIAL TOP LAYER  
C  
        IF(IBEDLAYU.EQ.1)TMPCVT=0.001  
        IF(IBEDLAYU.EQ.2)TMPCVT=0.01  
        IF(IBEDLAYU.EQ.3)TMPCVT=1.0  
        IF(IBEDLAYU.GE.1)THEN  
          DO K=1,KB  
            DO L=2,LA  
              BEDLINIT(L,K)=TMPCVT*BEDLINIT(L,K)  
            ENDDO  
          ENDDO  
        ENDIF  
        DO L=2,LA  
          KBT(L)=1  
        ENDDO  
        DO K=1,KB  
          DO L=2,LA  
            HBED(L,K)=BEDLINIT(L,K)  
            HBED1(L,K)=BEDLINIT(L,K)  
            IF(HBED(L,K).GT.0.) KBT(L)=K  
          ENDDO  
        ENDDO  
C  
C **  CONVERT AND INITIALIZE BED BULK DENSITY  
C**   IBEDBDNU=0 BEDBINIT IS NOT BULK DENSITY  
C**   IBEDBDNU=1 BEDBINIT BULK DENSITY IN KG/M**3  
C**   IBEDBDNU=3 BEDBINIT BULK DENSITY IN GM/CM**3  
C  
        IF(IBEDBDNU.GE.1)THEN  
          IF(IBEDBDNU.EQ.2)THEN  
            DO K=1,KB  
              DO L=2,LA  
                BEDBINIT(L,K)=1000.*BEDBINIT(L,K)  
              ENDDO  
            ENDDO  
          ENDIF  
          DO K=1,KB  
            DO L=2,LA  
              BDENBED(L,K)=BEDBINIT(L,K)  
              BDENBED1(L,K)=BEDBINIT(L,K)  
            ENDDO  
          ENDDO  
        ENDIF  
C  
C **  CONVERT AND DRY DENSITY OF BED  
C **  IBEDDDNU=0,1 ACTUAL DRY DENSITY, =2  POROSITY, =3 VOID RATIO  
C  
        IF(IBEDDDNU.EQ.1)THEN  
          DO K=1,KB  
            DO L=2,LA  
              BEDDINIT(L,K)=1000.*BEDDINIT(L,K)  
            ENDDO  
          ENDDO  
        ENDIF  
C  
C **  CALCULATE POROSITY AND VOID RATIO  
C  
        IF(IBEDDDNU.LE.1)THEN  
          DO K=1,KB  
            DO L=2,LA  
              PORBED(L,K)=0.001*(BEDBINIT(L,K)-BEDDINIT(L,K))  
              VDRBED(L,K)=PORBED(L,K)/(1.-PORBED(L,K))  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(IBEDDDNU.EQ.2)THEN  
          DO K=1,KB  
            DO L=2,LA  
              PORBED(L,K)=BEDDINIT(L,K)  
              VDRBED(L,K)=PORBED(L,K)/(1.-PORBED(L,K))  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(IBEDDDNU.EQ.3)THEN  
          DO K=1,KB  
            DO L=2,LA  
              VDRBED(L,K)=BEDDINIT(L,K)  
              PORBED(L,K)=VDRBED(L,K)/(1.+VDRBED(L,K))  
            ENDDO  
          ENDDO  
        ENDIF  
        DO K=1,KB  
          DO L=2,LA  
            VDRBED1(L,K)=VDRBED(L,K)  
            PORBED1(L,K)=PORBED(L,K)  
          ENDDO  
        ENDDO  
C  
C **  INITIALIZE BED SEDIMENT FOR MASS FRACTION INPUT BY CACLUALTING  
C **  AND STORING TOTAL MASS OF SED/AREA IN BEDDINIT(L,K)  
C  
        DO K=1,KB  
          DO L=2,LA  
            BEDDINIT(L,K)=HBED(L,K)*(BDENBED(L,K)-1000.*PORBED(L,K))  
          ENDDO  
        ENDDO  
        IF(ISTRAN(6).GE.1)THEN  
          DO NS=1,NSED  
            IF(ISEDBU(NS).EQ.1)THEN  
              DO K=1,KB  
                DO L=2,LA  
                  SEDB(L,K,NS)=1000.*SEDBINIT(L,K,NS)*BEDDINIT(L,K)  
                  SEDB1(L,K,NS)=SEDB(L,K,NS)  
                ENDDO  
              ENDDO  
            ENDIF  
          ENDDO  
        ENDIF  
        IF(ISTRAN(7).GE.1)THEN  
          DO NS=1,NSND  
            IF(ISNDBU(NS).EQ.1)THEN  
              DO K=1,KB  
                DO L=2,LA  
                  SNDB(L,K,NS)=1000.*SNDBINIT(L,K,NS)*BEDDINIT(L,K)  
                  SNDB1(L,K,NS)=SNDB(L,K,NS)  
                ENDDO  
              ENDDO  
            ENDIF  
          ENDDO  
        ENDIF  
C  
C **  SET COHESIVE BED CRITICAL STRESSES AND RESUSPENSION RATES  
C  
        IF(ISTRAN(6).GE.1.AND.IWRSP(1)/=98)THEN  
          IF(IWRSP(1).EQ.0)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURS(L,K)=TAUR(1)  
                WRSPS(L,K)=WRSPO(1)  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(IWRSPB(1).EQ.0)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURB(L,K)=1.E6  
                WRSPB(L,K)=0.0  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(IWRSP(1).GE.1)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURS(L,K)=CSEDTAUS(BDENBED(L,K),TAUR(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSP(1),L)  
                WRSPS(L,K)=CSEDRESS(BDENBED(L,K),WRSPO(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSP(1))  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(IWRSPB(1).GE.1)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TAURB(L,K)=CSEDTAUB(BDENBED(L,K),TAUR(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSPB(1))  
                WRSPB(L,K)=CSEDRESB(BDENBED(L,K),WRSPO(1),VDRRSPO(1),  
     &              VDRBED(L,K),VDRBED(L,K),IWRSPB(1))  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDIF  
C  
C **  SET SEDIMENT VOLUME FRACTIONS  
C  
        DO K=1,KB  
          DO L=2,LA  
            BEDLINIT(L,K)=0.  
            BEDDINIT(L,K)=0.  
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
        IF(ISTRAN(6).GE.1)THEN  
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
        IF(ISTRAN(6).GE.1)THEN  
          DO NS=1,NSED  
            DO K=1,KB  
              DO L=2,LA  
                ! *** BEGIN DSLLC
                IF(BEDLINIT(L,K).GT.0.0)THEN
                  VFRBED(L,K,NS)=VFRBED(L,K,NS)/BEDLINIT(L,K) 
                ELSE  
                  VFRBED(L,K,NS)=0.0
                  BEDLINIT(L,K) =0.0  
                ENDIF  
                IF(BEDDINIT(L,K).GT.0.0)THEN  
                  VFRBED1(L,K,NS)=VFRBED1(L,K,NS)/BEDDINIT(L,K)  
                ELSE  
                  VFRBED1(L,K,NS)=0.0  
                  BEDDINIT(L,K) =0.0  
                ENDIF  
                ! *** END DSLLC
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(7).GE.1)THEN  
          DO NX=1,NSND  
            NS=NSED+NX  
            DO K=1,KB  
              DO L=2,LA  
                ! *** BEGIN DSLLC
                IF(BEDLINIT(L,K).GT.0.0)THEN
                  VFRBED(L,K,NS)=VFRBED(L,K,NS)/BEDLINIT(L,K) 
                ELSE  
                  VFRBED(L,K,NS)=0.0
                  BEDLINIT(L,K) =0.0  
                ENDIF  
                IF(BEDDINIT(L,K).GT.0.0)THEN  
                  VFRBED1(L,K,NS)=VFRBED1(L,K,NS)/BEDDINIT(L,K)  
                ELSE  
                  VFRBED1(L,K,NS)=0.0  
                  BEDDINIT(L,K) =0.0  
                ENDIF  
                ! *** END DSLLC
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  END COLD START INITIALIZATION: IBMECH.GE.1  
C**  INITIALIZE BED BOTTOM ELEVATION  
C  
	IF(IWRSP(1)/=98)THEN ! Skip if SEDZLJ dynamics are active
        DO L=2,LA  
          HBEDA(L)=0.  
        ENDDO  
        DO L=2,LA  
          DO K=1,KBT(L)  
            HBEDA(L)=HBEDA(L)+HBED(L,K)  
          END DO  
        ENDDO  
        DO L=2,LA  
          ZELBEDA(L)=BELV(L)-HBEDA(L)  
        ENDDO  
	ENDIF
C  
C**  INITIALIZE TOTAL SEDIMENT MASS PER UNIT AREA  
C  
      DO K=1,KB  
        DO L=2,LA  
          SEDBT(L,K)=0.  
          SNDBT(L,K)=0.  
        ENDDO  
      ENDDO  
      IF(ISTRAN(6).GE.1)THEN  
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
C  
C **  IF N=1 AND ISTRAN(5)=1 CHECK INITIAL TOXIC CONCENTRATIONS IN  
C **  BED AND REINITILIZE IF NECESSARY  
C  
      IF(ISTRAN(5).GE.1)THEN  
        IF(ISRESTI.EQ.0.OR.ISCI(5).EQ.0)THEN  
C  
C **  CALCULATE TOTAL SEDIMENT IN THE BED  
C  
          DO K=1,KB  
            DO L=1,LC  
              SEDBT(L,K)=0.  
              SNDBT(L,K)=0.  
              SEDBALL(L,K)=0.  
            ENDDO  
          ENDDO  
          IF(ISTRAN(6).GE.1)THEN  
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
C **  CALCULATE TOTAL PARTICULATE FRACTION OF EACH TOXIC IN THE BED  
C  
          DO NT=1,NTOX  
            NSP2(NT)=NSED+NSND  
            IF(ISTOC(NT).EQ.2) NSP2(NT)=NSP2(NT)+1  
            IF(ISTOC(NT).EQ.3) NSP2(NT)=NSP2(NT)+2  
          END DO  
          DO NT=1,NTOX  
            DO NS=1,NSP2(NT)  
              DO K=1,KB  
                DO L=2,LA  
                  TOXPFB(L,K,NS,NT)=0.  
                ENDDO  
              ENDDO  
            ENDDO  
          ENDDO  
          DO NT=1,NTOX  
            IF(ISTRAN(6).GE.1)THEN  
              DO NS=1,NSED  
                DO K=1,KB  
                  DO L=2,LA  
                    TOXPFB(L,K,NS,NT)=SEDB(L,K,NS)*TOXPARB(NS,NT)  
                  ENDDO  
                ENDDO  
              ENDDO  
            ENDIF  
            IF(ISTRAN(7).GE.1)THEN  
              DO NX=1,NSND  
                NS=NX+NSED  
                DO K=1,KB  
                  DO L=2,LA  
                    TOXPFB(L,K,NS,NT)=SNDB(L,K,NX)*TOXPARB(NS,NT)  
                  ENDDO  
                ENDDO  
              ENDDO  
            ENDIF  
            IF(ISTOC(NT).EQ.2)THEN  
              NS=1+NSED+NSND  
              DO K=1,KB  
                DO L=2,LA  
                  TOXPFB(L,K,NS,NT)=STDOCB(L,K)*TOXPARBC(1,NT)  
                ENDDO  
              ENDDO  
            ENDIF  
            IF(ISTOC(NT).EQ.3)THEN  
              NS=2+NSED+NSND  
              DO K=1,KB  
                DO L=2,LA  
                  TOXPFB(L,K,NS,NT)=STPOCB(L,K)*TOXPARBC(2,NT)  
                ENDDO  
              ENDDO  
            ENDIF  
          ENDDO  
          DO NT=1,NTOX  
            DO K=1,KB  
              DO L=2,LA  
                TOXPFTB(L,K,NT)=0.  
              ENDDO  
            ENDDO  
            DO NS=1,NSP2(NT)  
              DO K=1,KB  
                DO L=2,LA  
                  TOXPFTB(L,K,NT)=TOXPFTB(L,K,NT)+TOXPFB(L,K,NS,NT)  
                ENDDO  
              ENDDO  
            ENDDO  
          ENDDO  
          DO NT=1,NTOX  
            DO K=1,KB  
              DO L=2,LA  
                IF(SEDBALL(L,K).GT.0.0)THEN  
                  TOXPFTB(L,K,NT)=TOXPFTB(L,K,NT)  
     &                /(PORBED(L,K)*HBED(L,K)+TOXPFTB(L,K,NT))  
                ELSE  
                  TOXPFTB(L,K,NT)=1.  
                ENDIF  
              ENDDO  
            ENDDO  
          ENDDO  
C  
C **  CONVERT MASS TOX/MASS SED INITIAL CONDITION TO TOTAL TOXIC  
C **  CONCENTRATION IN BED 0.001 CONVERTS TOXINTB UNITS OF MG/KG  
C **  TO TOXB UNITS OF OF MG/M**2  
C  
          DO NT=1,NTOX  
            IF(ITXBDUT(NT).EQ.0)THEN  
              DO K=1,KB  
                DO L=2,LA  
                  TOXB(L,K,NT)=HBED(L,K)*TOXB(L,K,NT)  
                  TOXB1(L,K,NT)=TOXB(L,K,NT)  
                ENDDO  
              ENDDO  
            ENDIF  
            IF(ITXBDUT(NT).EQ.1)THEN  
              DO K=1,KB  
                DO L=2,LA  
                  TOXB(L,K,NT)=0.001*TOXB(L,K,NT)*(SEDBT(L,K)  
     &                +SNDBT(L,K))/TOXPFTB(L,K,NT)  
                  TOXB1(L,K,NT)=TOXB(L,K,NT)  
                ENDDO  
              ENDDO  
            ENDIF  
          ENDDO  
C  
C ** DIAGNOSTICS OF INITIALIZATION  
C  
          IF(ISDTXBUG.EQ.1)THEN  
            OPEN(2,FILE='TOXBED.DIA')  
            CLOSE(2,STATUS='DELETE')  
            OPEN(2,FILE='TOXBED.DIA')  
            DO L=2,LA  
C  
C             TMP1=-999.  
C             TMP2=-999.  
C  
              TMP1=TOXB(L,1,1)/(HBED(L,1)+1.E-12)  
              WRITE(2,2222)IL(L),JL(L),TOXPFTB(L,1,1),TOXB(L,1,1),  
     &            TMP1,TOX(L,1,1)  
            ENDDO  
            CLOSE(2)  
          ENDIF  
        ENDIF  
      ENDIF  
 2222 FORMAT(2I5,7E13.4)  
C  
C **  INITIALIZE FRACTION OF PARTICULATE ORGANIC CARBON IN BED  
C  
      IVAL=0  
      DO NT=1,NTOX  
        IF(ISTOC(NT).GE.2)IVAL=1  
      ENDDO  
      IF(IVAL.EQ.1.AND.ISTPOCB.EQ.4)THEN  
        CALL SETFPOCB(0)  
      ENDIF  
C  
C **  CALCULATE COHESIVE AND NONCOHESIVE VOID RATIOS  
C  
      DO K=1,KB  
        DO L=2,LA  
          IF(K.LE.KBT(L))THEN  
            FVOLSSD=1./(1.+VDRBED(L,K))  
            FVOLSED=0.0  
            DO NS=1,NSED  
              FVOLSED=FVOLSED+VFRBED(L,K,NS)  
            ENDDO  
            FVOLSND=0.0  
            DO NX=1,NSND  
              NS=NSED+NX  
              FVOLSND=FVOLSND+VFRBED(L,K,NS)  
            ENDDO  
            FVOLSED=FVOLSSD*FVOLSED  
            FVOLSND=FVOLSSD*FVOLSND  
            VDRBEDSND(L,K)=SNDVDRD
            IF(FVOLSED.GT.1.0E-18)THEN  
              VDRBEDSED(L,K)=((FVOLSED+FVOLSND)*VDRBED(L,K)-  
     &                         FVOLSND*SNDVDRD)/FVOLSED  
            ELSE
              VDRBEDSED(L,K)=0.0  
            ENDIF
          ELSE  
            VDRBEDSND(L,K)=0.0  
            VDRBEDSED(L,K)=0.0  
          ENDIF  
        ENDDO  
      ENDDO  
C  
C **  ADD ACTIVE ARMORING LAYER IF NOT PRESENT IN INITIAL OR RESTART  
C  
      IF(ISNDAL.EQ.2.AND.IALSTUP.GT.0)THEN  
        DO L=2,LA  
          KTOPTP=KBT(L)  
          KTOPP1=KBT(L)+1  
          FRACACT(L)=HBEDAL/HBED(L,KTOPTP)  
          FRACPAR(L)=(HBED(L,KTOPTP)-HBEDAL)/HBED(L,KTOPTP)  
          HBED(L,KTOPP1)=FRACACT(L)*HBED(L,KTOPTP)  
          HBED(L,KTOPTP)=FRACPAR(L)*HBED(L,KTOPTP)  
          PORBED(L,KTOPP1)=PORBED(L,KTOPTP)  
          PORBED1(L,KTOPP1)=PORBED1(L,KTOPTP)  
          VDRBED(L,KTOPP1)=VDRBED(L,KTOPTP)  
          VDRBED1(L,KTOPP1)=VDRBED1(L,KTOPTP)  
          BDENBED(L,KTOPP1)=BDENBED(L,KTOPTP)  
          BDENBED1(L,KTOPP1)=BDENBED1(L,KTOPTP)  
          SEDBT(L,KTOPP1)=FRACACT(L)*SEDBT(L,KTOPTP)  
          SEDBT(L,KTOPTP)=FRACPAR(L)*SEDBT(L,KTOPTP)  
          SNDBT(L,KTOPP1)=FRACACT(L)*SNDBT(L,KTOPTP)  
          SNDBT(L,KTOPTP)=FRACPAR(L)*SNDBT(L,KTOPTP)  
          STDOCB(L,KTOPP1)=STDOCB(L,KTOPTP)  
          STPOCB(L,KTOPP1)=STPOCB(L,KTOPTP)  
        ENDDO  
        DO NS=1,NSED  
          DO L=2,LA  
            KTOPTP=KBT(L)  
            KTOPP1=KBT(L)+1  
            SEDB(L,KTOPP1,NS)=FRACACT(L)*SEDB(L,KTOPTP,NS)  
            SEDB1(L,KTOPP1,NS)=FRACACT(L)*SEDB1(L,KTOPTP,NS)  
            SEDB(L,KTOPTP,NS)=FRACPAR(L)*SEDB(L,KTOPTP,NS)  
            SEDB1(L,KTOPTP,NS)=FRACPAR(L)*SEDB1(L,KTOPTP,NS)  
            STFPOCB(L,KTOPP1,NS)=STFPOCB(L,KTOPTP,NS)  
          ENDDO  
        ENDDO  
        DO NS=1,NSND  
          NX=NSED+NS  
          DO L=2,LA  
            KTOPTP=KBT(L)  
            KTOPP1=KBT(L)+1  
            SNDB(L,KTOPP1,NS)=FRACACT(L)*SNDB(L,KTOPTP,NS)  
            SNDB1(L,KTOPP1,NS)=FRACACT(L)*SNDB1(L,KTOPTP,NS)  
            SNDB(L,KTOPTP,NS)=FRACPAR(L)*SNDB(L,KTOPTP,NS)  
            SNDB1(L,KTOPTP,NS)=FRACPAR(L)*SNDB1(L,KTOPTP,NS)  
            STFPOCB(L,KTOPP1,NX)=STFPOCB(L,KTOPTP,NX)  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO L=2,LA  
            KTOPTP=KBT(L)  
            KTOPP1=KBT(L)+1  
            TOXB(L,KTOPP1,NT)=FRACACT(L)*TOXB(L,KTOPTP,NT)  
            TOXB1(L,KTOPP1,NT)=FRACACT(L)*TOXB1(L,KTOPTP,NT)  
            TOXB(L,KTOPTP,NT)=FRACPAR(L)*TOXB(L,KTOPTP,NT)  
            TOXB1(L,KTOPTP,NT)=FRACPAR(L)*TOXB1(L,KTOPTP,NT)  
            TOXPFTB(L,KTOPP1,NT)=TOXPFTB(L,KTOPTP,NT)  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO NS=1,NSED+NSND+2  
            DO L=2,LA  
              KTOPTP=KBT(L)  
              KTOPP1=KBT(L)+1  
              TOXPFB(L,K,NS,NT)=TOXPFB(L,K,NS,NT)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO L=2,LA  
          KBT(L)=KBT(L)+1  
        ENDDO  
      ENDIF  
C  
C**  WRITE DIAGNOSTIC FILES FOR BED INITIALIZATION  
C  
 1000 CONTINUE  
      IF(DEBUG)THEN
        OPEN(1,FILE='BEDINIT.SED')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDINIT.SED')  
        WRITE(1,111)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(SEDB(L,K,1),K=1,KB)  
          IF(NSED.GT.1) THEN  
            DO NX=2,NSED  
              WRITE(1,102)(SEDB(L,K,NX),K=1,KB)  
            END DO  
          ENDIF  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDINIT.SND')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDINIT.SND')  
        WRITE(1,112)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(SNDB(L,K,1),K=1,KB)  
          IF(NSND.GT.1)THEN  
            DO NX=2,NSND  
              WRITE(1,102)(SNDB(L,K,NX),K=1,KB)  
            END DO  
          ENDIF  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDINIT.VDR')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDINIT.VDR')  
        WRITE(1,113)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(VDRBED(L,K),K=1,KB)  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDINIT.POR')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDINIT.POR')  
        WRITE(1,114)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(PORBED(L,K),K=1,KB)  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDINIT.ZHB')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDINIT.ZHB')  
        WRITE(1,115)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),ZELBEDA(L),HBEDA(L),(HBED(L,K),K=1,KB)  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDINIT.BDN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDINIT.BDN')  
        WRITE(1,116)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(BDENBED(L,K),K=1,KB)  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDINIT.ELV')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDINIT.ELV')  
        WRITE(1,117)  
        DO L=2,LA  
          SURF=HP(L)+BELV(L)  
          WRITE(1,101)IL(L),JL(L),ZELBEDA(L),HBEDA(L),BELV(L),HP(L),SURF  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDINIT.TOX')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDINIT.TOX')  
        DO NT=1,NTOX  
          WRITE(1,118)NT  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),(TOXB(L,K,NT),K=1,KB)  
          ENDDO  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDINIT.VRS')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDINIT.VRS')  
        DO L=2,LA  
          WRITE(1,191)IL(L),JL(L),(VDRBED(L,K),K=1,KB)  
          WRITE(1,192) (VDRBEDSED(L,K),K=1,KB)  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDINITC.VVF')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDINITC.VVF')  
        OPEN(2,FILE='BEDINITF.VVF')  
        CLOSE(2,STATUS='DELETE')  
        OPEN(2,FILE='BEDINITF.VVF')  
        DO L=2,LA  
          K=KBT(L)  
          IF(HMP(L).GT.0.05)THEN  
          WRITE(1,191)IL(L),JL(L),VFRBED(L,K,1),PORBED(L,K),VDRBED(L,K),  
     &        VDRBEDSED(L,K)  
          ELSE  
          WRITE(2,191)IL(L),JL(L),VFRBED(L,K,1),PORBED(L,K),VDRBED(L,K),  
     &        VDRBEDSED(L,K)  
          ENDIF  
        ENDDO 
      ENDIF 
C  
C *** PMC BEGIN BLOCK  
C  
      ! *** SAVE STARTUP BED PARAMETERS FOR LATER  
      DO K=1,KB  
        DO L=2,LA  
          IF(K.LE.KBT(L))THEN  
            VDRBED2(L,K)=VDRBED(L,K)  
          ELSE  
            VDRBED2(L,K)=VDRBED(L,KBT(L))  
          ENDIF  
        ENDDO  
      ENDDO  

      DO L=2,LA  
        DO K=1,KB  
          BEDTHKSV(L,K)=HBED(L,K)  
          BEDPORSV(L,K)=PORBED(L,K)  
          BEDVDRSV(L,K)=VDRBED(L,K)  
          BEDBKDSV(L,K)=BDENBED(L,K)  
        ENDDO  
      ENDDO  
C  
C *** PMC END BLOCK  
C  
      CLOSE(1)  
      CLOSE(2)  
  191 FORMAT(2I5,18F10.3)  
  192 FORMAT(10X,18F10.3)  
  101 FORMAT(2I5,18E13.5)  
  102 FORMAT(10X,18E13.5)  
  111 FORMAT('   IL   JL    SEDBT(K=1,KB)')  
  112 FORMAT('   IL   JL    SNDBT(K=1,KB)')  
  113 FORMAT('   IL   JL    VRDBED(K=1,KB)')  
  114 FORMAT('   IL   JL    PORBED(K=1,KB)')  
  115 FORMAT('   IL   JL    ZBEDB        HBEDT        HBED(K=1,KB)')  
  116 FORMAT('   IL   JL    BDENBED(K=1,KB)')  
  117 FORMAT('   IL   JL    ZBEDB        HBEDT        BELV',  
     &    '        HWCOL        SELV')  
  118 FORMAT('   IL   JL    TOXB(K=1,KB,NT)  NT = ',I5)  
      RETURN  
      END  

