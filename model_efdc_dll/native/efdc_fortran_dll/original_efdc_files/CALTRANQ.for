      SUBROUTINE CALTRANQ (ISTL_,M,QCON,QCON1)  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALTRAN CALCULATES THE ADVECTIVE  
C **  TRANSPORT OF DISSOLVED OR SUSPENDED CONSITITUENT M LEADING TO  
C **  A NEW VALUE AT TIME LEVEL (N+1). THE VALUE OF ISTL INDICATES  
C **  THE NUMBER OF TIME LEVELS IN THE STEP  
C  
      USE GLOBAL  
      DIMENSION QCON(LCM,0:KCM),QCON1(LCM,0:KCM)  
C  
      BSMALL=1.0E-6  

      ! *** PMC
      IF(ISDYNSTP.EQ.0)THEN  
        DELT=DT2 
        DELTA=DT2  
        IF(ISCDCA(M).EQ.2) DELTA=DT  
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
        ISUD=1  
      END IF  
      ! *** PMC

C  
C **  ADVECTIVE FLUX CALCULATION  
C  
      IF(ISTL_.EQ.2) GOTO 300  
      IF(ISCDCA(M).EQ.0) GOTO 300  
      IF(ISCDCA(M).EQ.1) GOTO 400  
      IF(ISCDCA(M).EQ.2) GOTO 350  
C  
C **  CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE WITH ADVECTION  
C **  AVERAGED BETWEEN (N) AND (N+1) OR (N-1) AND (N+1) AND ADVECTED  
C **  AT (N) OR (N-1) IF ISTL EQUALS 2 OR 3 RESPECTIVELY  
C  
  300 CONTINUE  
      DO K=1,KS  
        DO L=2,LA  
          LS=LSC(L)  
          FUHU(L,K)=MAX(UHDY2(L,K),0.)*QCON1(L-1,K)  
     &        +MIN(UHDY2(L,K),0.)*QCON1(L,K)  
          FVHU(L,K)=MAX(VHDX2(L,K),0.)*QCON1(LS,K)  
     &        +MIN(VHDX2(L,K),0.)*QCON1(L,K)  
        ENDDO  
      ENDDO  
      DO K=0,KS  
        DO L=2,LA  
          FWU(L,K)=MAX(W2(L,K),0.)*QCON1(L,K)  
     &        +MIN(W2(L,K),0.)*QCON1(L,K+1)  
        ENDDO  
      ENDDO  
      GOTO 500  
C  
C **  CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE WITH ADVECTION  
C **  AVERAGED BETWEEN  (N-1) AND (N+1) AND ADVECTED FIELD AVERAGED  
C **  BETWEEN AT (N-1) AND (N) IF ISTL 3 ONLY  
C  
  350 CONTINUE  
      DO K=0,KC  
        DO L=2,LA  
          CONT(L,K)=0.5*(QCON(L,K)+QCON1(L,K))  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO L=2,LA  
          LS=LSC(L)  
          FUHU(L,K)=MAX(UHDY2(L,K),0.)*CONT(L-1,K)  
     &        +MIN(UHDY2(L,K),0.)*CONT(L,K)  
          FVHU(L,K)=MAX(VHDX2(L,K),0.)*CONT(LS,K)  
     &        +MIN(VHDX2(L,K),0.)*CONT(L,K)  
        ENDDO  
      ENDDO  
      DO K=0,KS  
        DO L=2,LA  
          FWU(L,K)=MAX(W2(L,K),0.)*CONT(L,K)  
     &        +MIN(W2(L,K),0.)*CONT(L,K+1)  
        ENDDO  
      ENDDO  
      GOTO 500  
C  
C **  CALCULATE ADVECTIVE FLUXES BY CENTRAL DIFFERENCE WITH TRANSPORT  
C **  AVERAGED BETWEEN (N+1) AND (N-1) AND TRANSPORTED FIELD AT (N)  
C  
  400 CONTINUE  
      DO K=1,KS  
        DO L=2,LA  
          LS=LSC(L)  
          FUHU(L,K)=0.5*UHDY2(L,K)*(QCON(L,K)+QCON(L-1,K))  
          FVHU(L,K)=0.5*VHDX2(L,K)*(QCON(L,K)+QCON(LS,K))  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO LL=1,NCBS  
          L=LCBS(LL)  
          LN=LNC(L)  
          IF(VHDX2(LN,K).LT.0.) FVHU(LN,K)=VHDX2(LN,K)*QCON1(LN,K)  
        ENDDO  
        DO LL=1,NCBW  
          L=LCBW(LL)  
          IF(UHDY2(L+1,K).LT.0.) FUHU(L+1,K)=UHDY2(L+1,K)*QCON1(L+1,K)  
        ENDDO  
        DO LL=1,NCBE  
          L=LCBE(LL)  
          IF(UHDY2(L,K).GT.0.) FUHU(L,K)=UHDY2(L,K)*QCON1(L-1,K)  
        ENDDO  
        DO LL=1,NCBN  
          L=LCBN(LL)  
          LS =LSC(L)  
          IF(VHDX2(L,K).GT.0.) FVHU(L,K)=VHDX2(L,K)*QCON1(LS,K)  
        ENDDO  
      ENDDO  
      DO K=0,KS  
        DO L=2,LA  
          FWU(L,K)=0.5*W2(L,K)*(QCON(L,K+1)+QCON(L,K))  
        ENDDO  
      ENDDO  
C  
C **  ADVECTION CALCULATION  
C  
  500 CONTINUE  
      IF(ISTL_.EQ.2)THEN  
        DO K=1,KS  
          DO L=2,LA  
            LN=LNC(L)  
            QCH(L,K)=QCON1(L,K)*H1P(L)  
     &          +DELT*((FUHU(L,K)-FUHU(L+1,K)  
     &          +FVHU(L,K)-FVHU(LN,K))*DXYIP(L)  
     &          +(FWU(L,K-1)-FWU(L,K))*DZIC(K))  
          ENDDO  
        ENDDO  
      ELSE  
        DO K=1,KS  
          DO L=2,LA  
            LN=LNC(L)  
            QCH(L,K)=QCON1(L,K)*H2P(L)  
     &          +DELT*((FUHU(L,K)-FUHU(L+1,K)  
     &          +FVHU(L,K)-FVHU(LN,K))*DXYIP(L)  
     &          +(FWU(L,K-1)-FWU(L,K))*DZIC(K))  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISUD.EQ.1)THEN  
        DO K=1,KS  
          DO L=2,LA
            ! *** HANDLED BELOW  
            !QCON1(L,K)=SCB(L)*QCON(L,K)+(1.-SCB(L))*QCON1(L,K)  
            QCON1(L,K)=QCON(L,K)  
          ENDDO  
        ENDDO  
      ENDIF  
      ! *** PMC-QCON SET BELOW BASED ON OPEN BC'S
      DO K=1,KS  
        DO L=2,LA  
          !QCON(L,K)=SCB(L)*QCH(L,K)*HPI(L)+(1.-SCB(L))*QCON(L,K)  
          QCON(L,K)=QCH(L,K)*HPI(L)  
        ENDDO  
      ENDDO  
C  
C **  ASSIGN OPEN BOUNDARY CELL VALUES NOT TO INTERFERE WITH FCT SCHEME  
C  
      IF(NBCSOP.GT.0)THEN
        ! *** SET OPEN BC'S USING ADJACENT CELLS
        DO K=1,KS  
          DO LL=1,NCBS  
            LN=LNC(L)  
            QCON(L,K)=QCON(LN,K)  
          ENDDO  
        ENDDO  
        DO K=1,KS  
          DO LL=1,NCBW  
            L=LCBW(LL)  
            QCON(L,K)=QCON(L+1,K)  
          ENDDO  
        ENDDO  
        DO K=1,KS  
          DO LL=1,NCBE  
            L=LCBE(LL)  
            QCON(L,K)=QCON(L-1,K)  
          ENDDO  
        ENDDO  
        DO K=1,KS  
          DO LL=1,NCBN  
            L=LCBN(LL)  
            LS=LSC(L)  
            QCON(L,K)=QCON(LS,K)  
          ENDDO  
        ENDDO  
      ENDIF
C
C *** Deleted the Anti-diffusive code for QQ & QQL (PMC)
C
      RETURN  
      END  

