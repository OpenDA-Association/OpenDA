      SUBROUTINE CALQQ2T (ISTL_)  
C  
C CHANGE RECORD  
C  FIXED DYNAMIC TIME STEPPING  
C **  SUBROUTINE CALQQ CALCULATES THE TURBULENT INTENSITY SQUARED AT  
C **  TIME LEVEL (N+1).  THE VALUE OF ISTL INDICATES THE NUMBER OF  
C **  TIME LEVELS INVOLVED  
C  
      USE GLOBAL  
      IF(ISDYNSTP.EQ.0)THEN  
        DELT=DT  
        DELTD2=0.5*DT  
        DELTI=1./DELT  
      ELSE  
        DELT=DTDYN  
        DELTD2=0.5*DTDYN  
        DELTI=1./DELT  
      END IF  
      S2TL=0.0  
      BSMALL=1.E-12  
      DO K=1,KS  
        DO L=2,LA  
          QQ2(L,K)=QQ(L,K)+QQ(L,K)  
          QQL2(L,K)=QQL(L,K)+QQL(L,K)  
        ENDDO  
      ENDDO  
C
      DO L=1,LC  
        UUU(L,KC)=0.  
        VVV(L,KC)=0.  
        FUHU(L,KC)=0.  
        FUHV(L,KC)=0.  
        FVHU(L,KC)=0.  
        FUHV(L,KC)=0.  
      ENDDO
C  
C **  CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE WITH TRANSPORT  
C **  AVERAGED BETWEEN (N) AND (N+1) AND TRANSPORTED FIELD AT (N) OR  
C **  TRANSPORT BETWEEN (N-1) AND (N+1) AND TRANSPORTED FIELD AT (N-1)  
C **  FOR ISTL EQUAL TO 2 AND 3 RESPECTIVELY  
C **  FUHQQ=FUHU, FVHQQ=FVHU, FUHQQL=FUHV, FVHQQL=FVHV  
C  
      IF(IDRYTBP.EQ.0)THEN  
        DO K=1,KC  
          DO L=2,LA  
            WB=0.5*DXYP(L)*(W2(L,K-1)+W2(L,K))  
            FWQQ(L,K)=MAX(WB,0.)*QQ(L,K-1)  
     &          +MIN(WB,0.)*QQ(L,K)  
            FWQQL(L,K)=MAX(WB,0.)*QQL(L,K-1)*H1P(L)  
     &          +MIN(WB,0.)*QQL(L,K)*H1P(L)  
          ENDDO  
        ENDDO  
      ELSE  
        DO K=1,KC  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              WB=0.5*DXYP(L)*(W2(L,K-1)+W2(L,K))  
              FWQQ(L,K)=MAX(WB,0.)*QQ(L,K-1)  
     &            +MIN(WB,0.)*QQ(L,K)  
              FWQQL(L,K)=MAX(WB,0.)*QQL(L,K-1)*H1P(L)  
     &            +MIN(WB,0.)*QQL(L,K)*H1P(L)  
            ELSE
              FWQQ(L,K)=0.  
              FWQQL(L,K)=0.  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF
C  
      IF(IDRYTBP.EQ.0)THEN  
        DO K=1,KS  
          DO L=2,LA  
            LS=LSC(L)  
            UHUW=0.5*(UHDY2(L,K)+UHDY2(L,K+1))  
            VHVW=0.5*(VHDX2(L,K)+VHDX2(L,K+1))  
            FUHU(L,K)=MAX(UHUW,0.)*QQ(L-1,K)  
     &          +MIN(UHUW,0.)*QQ(L,K)  
            FVHU(L,K)=MAX(VHVW,0.)*QQ(LS,K)  
     &          +MIN(VHVW,0.)*QQ(L,K)  
            FUHV(L,K)=MAX(UHUW,0.)*QQL(L-1,K)*H1P(L-1)  
     &          +MIN(UHUW,0.)*QQL(L,K)*H1P(L)  
            FVHV(L,K)=MAX(VHVW,0.)*QQL(LS,K)*H1P(LS)  
     &          +MIN(VHVW,0.)*QQL(L,K)*H1P(L)  
          ENDDO  
        ENDDO  
      ELSE  
        DO K=1,KS  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              LS=LSC(L)  
              UHUW=0.5*(UHDY2(L,K)+UHDY2(L,K+1))  
              VHVW=0.5*(VHDX2(L,K)+VHDX2(L,K+1))  
              FUHU(L,K)=MAX(UHUW,0.)*QQ(L-1,K)  
     &            +MIN(UHUW,0.)*QQ(L,K)  
              FVHU(L,K)=MAX(VHVW,0.)*QQ(LS,K)  
     &            +MIN(VHVW,0.)*QQ(L,K)  
              FUHV(L,K)=MAX(UHUW,0.)*QQL(L-1,K)*H1P(L-1)  
     &            +MIN(UHUW,0.)*QQL(L,K)*H1P(L)  
              FVHV(L,K)=MAX(VHVW,0.)*QQL(LS,K)*H1P(LS)  
     &            +MIN(VHVW,0.)*QQL(L,K)*H1P(L)
            ELSE  
              FUHU(L,K)=0.  
              FUHV(L,K)=0.  
              FVHU(L,K)=0.  
              FUHV(L,K)=0.  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE PRODUCTION, LOAD BOUNDARY CONDITIONS AND SOLVE  
C **  TRANSPORT EQUATIONS  
C **  FUHQQ=FUHU, FVHQQ=FVHU, FUHQQL=FUHV, FVHQQL=FVHV  
C **  CU1=CUQ, CU2=CUQL, UUU=QQH, VVV=QQLH  
C  
      DO K=1,KC  
        DO LL=1,NCBS  
          L=LCBS(LL)  
          LN=LNC(L)  
          IF(FVHU(LN,K).GT.0)THEN  
            FVHU(LN,K)=0.0  
            FVHV(LN,K)=0.0  
          ENDIF  
        ENDDO  
        DO LL=1,NCBW  
          L=LCBW(LL)  
          IF(FUHU(L+1,K).GT.0)THEN  
            FUHU(L+1,K)=0.0  
            FUHV(L+1,K)=0.0  
          ENDIF  
        ENDDO  
        DO LL=1,NCBE  
          L=LCBE(LL)  
          IF(FUHU(L,K).LT.0.)THEN  
            FUHU(L,K)=0.0  
            FUHV(L,K)=0.0  
          ENDIF  
        ENDDO  
        DO LL=1,NCBN  
          L=LCBN(LL)  
          IF(FVHU(L,K).LT.0.)THEN  
            FVHU(L,K)=0.0  
            FVHV(L,K)=0.0  
          ENDIF  
        ENDDO  
      ENDDO  
      IF(ISWAVE.LE.1.OR.ISWAVE.EQ.3)THEN  
        IF(IDRYTBP.EQ.0)THEN  
          DO K=1,KS  
            DO L=2,LA  
              LN=LNC(L)  
              UUU(L,K)=QQ(L,K)*H1P(L)  
     &            +DELT*(FUHU(L,K)-FUHU(L+1,K)+FVHU(L,K)-FVHU(LN,K)  
     &            +(FWQQ(L,K)-FWQQ(L,K+1))*DZIG(K))*DXYIP(L)  
              VVV(L,K)=QQL(L,K)*H1P(L)*H1P(L)  
     &            +DELT*(FUHV(L,K)-FUHV(L+1,K)+FVHV(L,K)-FVHV(LN,K)  
     &            +(FWQQL(L,K)-FWQQL(L,K+1))*DZIG(K))*DXYIP(L)  
            ENDDO  
          ENDDO  
          DO K=1,KS  
            DO L=2,LA  
              UUU(L,K)=MAX(UUU(L,K),0.)  
              VVV(L,K)=MAX(VVV(L,K),0.)  
            ENDDO  
          ENDDO  
          DO K=1,KS  
            DO L=2,LA  
              LN=LNC(L)  
              LS=LSC(L)  
              PQQB=AB(L,K)*GP*HP(L)*DZIG(K)*(B(L,K+1)-B(L,K))  
              PQQU=AV(L,K)*DZIGSD4(K)*(U(L+1,K+1)-U(L+1,K)+U(L,K+1)-  
     &            U(L,K))**2+AV(L,K)*DZIGSD4(K)*(V(LN,K+1)-V(LN,K)+V(
     &            L,K+1)-V(L,K))**2  
              PQQ=DELT*(PQQB+PQQU)  
              UUU(L,K)=UUU(L,K)+2.*PQQ  
              PQQL=DELT*H1P(L)*(CTE3*PQQB+CTE1*PQQU)  
              VVV(L,K)=VVV(L,K)+DML(L,K)*PQQL  
            ENDDO  
          ENDDO  
        ELSE  
          DO K=1,KS  
            DO L=2,LA  
              IF(LMASKDRY(L))THEN  
                LN=LNC(L)  
                UUU(L,K)=QQ(L,K)*H1P(L)  
     &              +DELT*(FUHU(L,K)-FUHU(L+1,K)+FVHU(L,K)-FVHU(LN,K)  
     &              +(FWQQ(L,K)-FWQQ(L,K+1))*DZIG(K))*DXYIP(L)  
                VVV(L,K)=QQL(L,K)*H1P(L)*H1P(L)  
     &              +DELT*(FUHV(L,K)-FUHV(L+1,K)+FVHV(L,K)-FVHV(LN,K)  
     &              +(FWQQL(L,K)-FWQQL(L,K+1))*DZIG(K))*DXYIP(L)  

                UUU(L,K)=MAX(UUU(L,K),0.)  
                VVV(L,K)=MAX(VVV(L,K),0.)  
              ELSE
                UUU(L,K)=0.0
                VVV(L,K)=0.0
              ENDIF  
            ENDDO  
          ENDDO  

          DO K=1,KS  
            DO L=2,LA  
              IF(LMASKDRY(L))THEN  
                LN=LNC(L)  
                LS=LSC(L)  
                PQQB=AB(L,K)*GP*HP(L)*DZIG(K)*(B(L,K+1)-B(L,K))  
                PQQU=AV(L,K)*DZIGSD4(K)*(U(L+1,K+1)-U(L+1,K)+U(L,K+1)  
     &              -U(L,K))**2+AV(L,K)*DZIGSD4(K)*(V(LN,K+1)-V(LN,K)
     &              +V(L,K+1)-V(L,K))**2  
                PQQ=DELT*(PQQB+PQQU)  
                UUU(L,K)=UUU(L,K)+2.*PQQ  
                PQQL=DELT*H1P(L)*(CTE3*PQQB+CTE1*PQQU)  
                VVV(L,K)=VVV(L,K)+DML(L,K)*PQQL  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C *** DSLLC BEGIN BLOCK  
C  
      IF(ISWAVE.EQ.2)THEN  
        IF(N.LT.NTSWV)THEN  
          TMPVAL=FLOAT(N)/FLOAT(NTSWV)  
          WVFACT=0.5-0.5*COS(PI*TMPVAL)  
        ELSE  
          WVFACT=1.0  
        ENDIF  
        DO K=1,KS  
          DO L=2,LA  
            TVAR1W(L,K)=WVDTKEM(K)*WVDISP(L,K)+WVDTKEP(K)*WVDISP(L,K+1)  
          ENDDO  
        ENDDO  
        DO K=1,KS  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN
              LN=LNC(L)  
              LS=LSC(L)  
              PQQB=AB(L,K)*GP*HP(L)*DZIG(K)*(B(L,K+1)-B(L,K))  
              PQQU=AV(L,K)*DZIGSD4(K)*  
     &          (U(L+1,K+1)-U(L+1,K)+U(L,K+1)-U(L,K))**2  
              PQQV=AV(L,K)*DZIGSD4(K)*  
     &          (V(LN,K+1)-V(LN,K)+V(L,K+1)-V(L,K))**2  
              PQQW= WVFACT*TVAR1W(L,K)  
              PQQ=DELT*(PQQU+PQQV+PQQB+PQQW)  
              FFTMP=MAX(FUHU(L,K)-FUHU(L+1,K)+FVHU(L,K)-FVHU(LN,K) +  
     &          (FWQQ(L,K)-FWQQ(L,K+1))*DZIG(K),0.)  
              UUU(L,K)=QQ(L,K)*H1P(L)+DELT*FFTMP*DXYIP(L) + 2.*PQQ  
              PQQL=DELT*H1P(L)*(CTE3*PQQB+CTE1*(PQQU+PQQV+PQQW))  
              FFTMP=MAX(FUHV(L,K)-FUHV(L+1,K)+FVHV(L,K)-FVHV(LN,K) +  
     &          (FWQQL(L,K)-FWQQL(L,K+1))*DZIG(K),0.)  
              VVV(L,K)=QQL(L,K)*H1P(L)*H1P(L)+DELT*FFTMP*DXYIP(L) +  
     &          DML(L,K)*PQQL  
            ENDIF   
          ENDDO  
        ENDDO  
      ENDIF  
C  
C *** DSLLC END BLOCK  
C  
      IF(KC.LE.2)THEN  
        IF(IDRYTBP.EQ.0)THEN  
          DO L=2,LA  
            CLQTMP=-DELT*CDZKK(1)*AQ(L,1)*HPI(L)  
            CUQTMP=-DELT*CDZKKP(1)*AQ(L,2)*HPI(L)  
            CMQTMP=1.-CLQTMP-CUQTMP  
     &          +2.*DELT*QQSQR(L,1)/(CTURBB1(L,1)*DML(L,1)*HP(L))  
            CMQLTMP=1.-CLQTMP-CUQTMP  
     &          +DELT*(QQSQR(L,1)/(CTURBB1(L,1)*DML(L,1)*HP(L)))*(1.  
     &          +CTE2*DML(L,1)*DML(L,1)*FPROX(1))  
            EQ=1./CMQTMP  
            EQL=1./CMQLTMP  
            CU1(L,1)=CUQTMP*EQ  
            CU2(L,1)=CUQTMP*EQL  
            UUU(L,1)=(UUU(L,1)-CLQTMP*HP(L)*QQ(L,0)  
     &          -CUQTMP*HP(L)*QQ(L,KC))*EQ  
            VVV(L,1)=VVV(L,1)*EQL  
          ENDDO  
        ELSE  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              CLQTMP=-DELT*CDZKK(1)*AQ(L,1)*HPI(L)  
              CUQTMP=-DELT*CDZKKP(1)*AQ(L,2)*HPI(L)  
              CMQTMP=1.-CLQTMP-CUQTMP  
     &            +2.*DELT*QQSQR(L,1)/(CTURBB1(L,1)*DML(L,1)*HP(L))  
              CMQLTMP=1.-CLQTMP-CUQTMP  
     &           +DELT*(QQSQR(L,1)/(CTURBB1(L,1)*DML(L,1)*HP(L)))*(1.  
     &            +CTE2*DML(L,1)*DML(L,1)*FPROX(1))  
              EQ=1./CMQTMP  
              EQL=1./CMQLTMP  
              CU1(L,1)=CUQTMP*EQ  
              CU2(L,1)=CUQTMP*EQL  
              UUU(L,1)=(UUU(L,1)-CLQTMP*HP(L)*QQ(L,0)  
     &            -CUQTMP*HP(L)*QQ(L,KC))*EQ  
              VVV(L,1)=VVV(L,1)*EQL  
            ENDIF  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(KC.GT.2)THEN  
        IF(IDRYTBP.EQ.0)THEN  
          DO L=2,LA  
            CLQTMP=-DELT*CDZKK(1)*AQ(L,1)*HPI(L)  
            CUQTMP=-DELT*CDZKKP(1)*AQ(L,2)*HPI(L)  
            CMQTMP=1.-CLQTMP-CUQTMP  
     &          +2.*DELT*QQSQR(L,1)/(CTURBB1(L,1)*DML(L,1)*HP(L))  
            CMQLTMP=1.-CLQTMP-CUQTMP  
     &          +DELT*(QQSQR(L,1)/(CTURBB1(L,1)*DML(L,1)*HP(L)))*(1.  
     &          +CTE2*DML(L,1)*DML(L,1)*FPROX(1))  
            EQ=1./CMQTMP  
            EQL=1./CMQLTMP  
            CU1(L,1)=CUQTMP*EQ  
            CU2(L,1)=CUQTMP*EQL  
            UUU(L,1)=(UUU(L,1)-CLQTMP*HP(L)*QQ(L,0))*EQ  
            VVV(L,1)=VVV(L,1)*EQL  
            CUQTMP=-DELT*CDZKKP(KS)*AQ(L,KC)*HPI(L)  
            UUU(L,KS)=UUU(L,KS)-CUQTMP*HP(L)*QQ(L,KC)  
          ENDDO  
          DO K=2,KS  
            DO L=2,LA  
              CLQTMP=-DELT*CDZKK(K)*AQ(L,K)*HPI(L)  
              CUQTMP=-DELT*CDZKKP(K)*AQ(L,K+1)*HPI(L)  
              CMQTMP=1.-CLQTMP-CUQTMP  
     &            +2.*DELT*QQSQR(L,K)/(CTURBB1(L,K)*DML(L,K)*HP(L))  
              CMQLTMP=1.-CLQTMP-CUQTMP  
     &           +DELT*(QQSQR(L,K)/(CTURBB1(L,K)*DML(L,K)*HP(L)))*(1.  
     &            +CTE2*DML(L,K)*DML(L,K)*FPROX(K))  
              EQ=1./(CMQTMP-CLQTMP*CU1(L,K-1))  
              EQL=1./(CMQLTMP-CLQTMP*CU2(L,K-1))  
              CU1(L,K)=CUQTMP*EQ  
              CU2(L,K)=CUQTMP*EQL  
              UUU(L,K)=(UUU(L,K)-CLQTMP*UUU(L,K-1))*EQ  
              VVV(L,K)=(VVV(L,K)-CLQTMP*VVV(L,K-1))*EQL  
            ENDDO  
          ENDDO  
          DO K=KS-1,1,-1  
            DO L=2,LA  
              UUU(L,K)=UUU(L,K)-CU1(L,K)*UUU(L,K+1)  
              VVV(L,K)=VVV(L,K)-CU2(L,K)*VVV(L,K+1)  
            ENDDO  
          ENDDO  
        ELSE  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              CLQTMP=-DELT*CDZKK(1)*AQ(L,1)*HPI(L)  
              CUQTMP=-DELT*CDZKKP(1)*AQ(L,2)*HPI(L)  
              CMQTMP=1.-CLQTMP-CUQTMP  
     &            +2.*DELT*QQSQR(L,1)/(CTURBB1(L,1)*DML(L,1)*HP(L))  
              CMQLTMP=1.-CLQTMP-CUQTMP  
     &           +DELT*(QQSQR(L,1)/(CTURBB1(L,1)*DML(L,1)*HP(L)))*(1.  
     &            +CTE2*DML(L,1)*DML(L,1)*FPROX(1))  
              EQ=1./CMQTMP  
              EQL=1./CMQLTMP  
              CU1(L,1)=CUQTMP*EQ  
              CU2(L,1)=CUQTMP*EQL  
              UUU(L,1)=(UUU(L,1)-CLQTMP*HP(L)*QQ(L,0))*EQ  
              VVV(L,1)=VVV(L,1)*EQL  
              CUQTMP=-DELT*CDZKKP(KS)*AQ(L,KC)*HPI(L)  
              UUU(L,KS)=UUU(L,KS)-CUQTMP*HP(L)*QQ(L,KC)  
            ENDIF  
          ENDDO  
          DO K=2,KS  
            DO L=2,LA  
              IF(LMASKDRY(L))THEN  
                CLQTMP=-DELT*CDZKK(K)*AQ(L,K)*HPI(L)  
                CUQTMP=-DELT*CDZKKP(K)*AQ(L,K+1)*HPI(L)  
                CMQTMP=1.-CLQTMP-CUQTMP  
     &              +2.*DELT*QQSQR(L,K)/(CTURBB1(L,K)*DML(L,K)*HP(L))  
                CMQLTMP=1.-CLQTMP-CUQTMP  
     &              +DELT*(QQSQR(L,K)/(CTURBB1(L,K)*DML(L,K)*HP(L))  
     &              )*(1.+CTE2*DML(L,K)*DML(L,K)*FPROX(K))  
                EQ=1./(CMQTMP-CLQTMP*CU1(L,K-1))  
                EQL=1./(CMQLTMP-CLQTMP*CU2(L,K-1))  
                CU1(L,K)=CUQTMP*EQ  
                CU2(L,K)=CUQTMP*EQL  
                UUU(L,K)=(UUU(L,K)-CLQTMP*UUU(L,K-1))*EQ  
                VVV(L,K)=(VVV(L,K)-CLQTMP*VVV(L,K-1))*EQL  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=KS-1,1,-1  
            DO L=2,LA  
              IF(LMASKDRY(L))THEN  
                UUU(L,K)=UUU(L,K)-CU1(L,K)*UUU(L,K+1)  
                VVV(L,K)=VVV(L,K)-CU2(L,K)*VVV(L,K+1)  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(IDRYTBP.EQ.0)THEN  
        DO K=1,KS  
          DO L=2,LA  
            QQ1(L,K)=QQ(L,K)  
            QQHDH=UUU(L,K)*HPI(L)  
            QQ(L,K)=MAX(QQHDH,QQMIN)  
          ENDDO  
        ENDDO  
C  
C **  ORIGINAL FORM MODIFED FOR DIMENSIONAL LENGHT SCALE TRANSPORT  
C  
        IF(ISTOPT(0).EQ.1)THEN  
          DO K=1,KS  
            DO L=2,LA  
              QQL1(L,K)=QQL(L,K)  
              QQHDH=VVV(L,K)*HPI(L)*HPI(L)  
              QQL(L,K)=MAX(QQHDH,QQLMIN)  
              DMLTMP=QQL(L,K)/QQ(L,K)  
              DMLTMP=MAX(DMLTMP,DMLMIN)  
              DELB=B(L,K)-B(L,K+1)  
              IF(DELB.GT.0.0)THEN  
                DMLMAX=0.53*SQRT(QQ(L,K)/(G*HP(L)*DZIG(K)*DELB))  
                DML(L,K)=MIN(DMLMAX,DMLTMP)  
              ELSE  
                DML(L,K)=DMLTMP  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDIF  
C  
C **  BUCHARD'S MODIFED CLOSURE FOR DIMENSIONAL LENGHT SCALE TRANSPORT  
C  
        IF(ISTOPT(0).EQ.2)THEN  
          DO K=1,KS  
            DO L=2,LA  
              QQL1(L,K)=QQL(L,K)  
              QQHDH=VVV(L,K)*HPI(L)*HPI(L)  
              QQL(L,K)=MAX(QQHDH,QQLMIN)  
              DMLTMP=QQL(L,K)/QQ(L,K)  
              DML(L,K)=MAX(DMLTMP,DMLMIN)  
            ENDDO  
          ENDDO  
        ENDIF  
      ELSE  
        DO K=1,KS  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              QQ1(L,K)=QQ(L,K)  
              QQHDH=UUU(L,K)*HPI(L)  
              QQ(L,K)=MAX(QQHDH,QQMIN)  
            ENDIF  
          ENDDO  
        ENDDO  
C  
C **  ORIGINAL FORM MODIFED FOR DIMENSIONAL LENGHT SCALE TRANSPORT  
C  
        IF(ISTOPT(0).EQ.1)THEN  
          DO K=1,KS  
            DO L=2,LA  
              IF(LMASKDRY(L))THEN  
                QQL1(L,K)=QQL(L,K)  
                QQHDH=VVV(L,K)*HPI(L)*HPI(L)  
                QQL(L,K)=MAX(QQHDH,QQLMIN)  
                DMLTMP=QQL(L,K)/QQ(L,K)  
                DMLTMP=MAX(DMLTMP,DMLMIN)  
                DELB=B(L,K)-B(L,K+1)  
                IF(DELB.GT.0.0)THEN  
                  DMLMAX=0.53*SQRT(QQ(L,K)/(G*HP(L)*DZIG(K)*DELB))  
                  DML(L,K)=MIN(DMLMAX,DMLTMP)  
                ELSE  
                  DML(L,K)=DMLTMP  
                ENDIF  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDIF  
C  
C **  BUCHARD'S MODIFED CLOSURE FOR DIMENSIONAL LENGHT SCALE TRANSPORT  
C  
        IF(ISTOPT(0).EQ.2)THEN  
          DO K=1,KS  
            DO L=2,LA  
              IF(LMASKDRY(L))THEN  
                QQL1(L,K)=QQL(L,K)  
                QQHDH=VVV(L,K)*HPI(L)*HPI(L)  
                QQL(L,K)=MAX(QQHDH,QQLMIN)  
                DMLTMP=QQL(L,K)/QQ(L,K)  
                DML(L,K)=MAX(DMLTMP,DMLMIN)  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C      QQMXSV=-1.E+12  
C      QQMNSV=1.E+12  
C      QQLMXSV=-1.E+12  
C      QQLMNSV=1.E+12  
C  
      DO K=1,KS  
        DO LL=1,NCBS  
          L=LCBS(LL)  
          LN=LNC(L)  
          QQ(L,K)=QQ(LN,K)  
          QQL(L,K)=QQL(LN,K)  
          DML(L,K)=DML(LN,K)  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO LL=1,NCBW  
          L=LCBW(LL)  
          QQ(L,K)=QQ(L+1,K)  
          QQL(L,K)=QQL(L+1,K)  
          DML(L,K)=DML(L+1,K)  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO LL=1,NCBE  
          L=LCBE(LL)  
          QQ(L,K)=QQ(L-1,K)  
          QQL(L,K)=QQL(L-1,K)  
          DML(L,K)=DML(L-1,K)  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO LL=1,NCBN  
          L=LCBN(LL)  
          LS=LSC(L)  
          QQ(L,K)=QQ(LS,K)  
          QQL(L,K)=QQL(LS,K)  
          DML(L,K)=DML(LS,K)  
        ENDDO  
      ENDDO  
C *** DSLLC BEGIN BLOCK
      DO K=1,KS  
        DO L=1,LC
          QQSQR(L,K)=SQRT(QQ(L,K))  ! *** DSLLC
        ENDDO  
      ENDDO  
C *** DSLLC END BLOCK
  110 FORMAT('    I    J   QQ BOT        QQ MID        QQ SURF',  
     &    '       PROD+ADV      1./DIAGON')  
  111 FORMAT(2I5,5E14.5)  
  600 FORMAT('N,QX,QN,QLX,QLN,CX,CN=',I5,6E12.4)  
  601 FORMAT('NEG QQ I,J,K,QQ=',3I5,E13.5)  
      RETURN  
      END  

