      SUBROUTINE CALQQ2 (ISTL_)  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALQQ2 CALCULATES THE TURBULENT INTENSITY SQUARED AT  
C **  TIME LEVEL (N+1).  THE VALUE OF ISTL INDICATES THE NUMBER OF  
C **  TIME LEVELS INVOLVED.  THIS VERSION USES A SEPARATE ADVECTIVE  
C **  TRANSPORT SUBROUTINE CALTRANQ  
C  
      USE GLOBAL  
      DELT=DT2  
      S3TL=1.0  
      S2TL=0.0  
      IF(ISTL_.EQ.2)THEN  
        DELT=DT  
        S3TL=0.0  
        S2TL=1.0  
      ENDIF  
      BSMALL=1.E-12  
C  
C **  MOVE UHDY2, VHDX2 AND W2 TO QQ TRANSPORT LOCATIONS  
C  
      DO K=1,KS  
        DO L=2,LA  
          U2(L,K)=0.5*(U2(L,K)+U2(L,K+1))  
          V2(L,K)=0.5*(V2(L,K)+V2(L,K+1))  
          UHDY2(L,K)=0.5*(UHDY2(L,K)+UHDY2(L,K+1))  
          VHDX2(L,K)=0.5*(VHDX2(L,K)+VHDX2(L,K+1))  
        ENDDO  
      ENDDO  
      DO K=0,KS  
        DO L=2,LA  
          W2(L,K)=0.5*(W2(L,K)+W2(L,K+1))  
        ENDDO  
      ENDDO  
C  
C **  ELIMINATE INFLOWS ACROSS OPEN BOUNDARIES  
C  
      DO K=1,KS  
        DO LL=1,NCBS  
          L=LCBS(LL)  
          LN=LNC(L)  
          IF(VHDX2(LN,K).GT.0.) VHDX2(LN,K)=0.  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO LL=1,NCBW  
          L=LCBW(LL)  
          IF(UHDY2(L+1,K).GT.0.) UHDY2(L+1,K)=0.  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO LL=1,NCBE  
          L=LCBE(LL)  
          IF(UHDY2(L,K).LT.0.) UHDY2(L,K)=0.  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO LL=1,NCBN  
          L=LCBN(LL)  
          IF(VHDX2(L,K).LT.0.) VHDX2(L,K)=0.  
        ENDDO  
      ENDDO  
C  
C **  CALL ADVECTIVE TRANSPORT SUBROUTINE FOR QQ AND QQL  
C  
      CALL CALTRANQ (ISTL_,0,QQ,QQ1)  
      CALL CALTRANQ (ISTL_,0,QQL,QQL1)  
      DO L=2,LA  
        W2(L,0)=0.  
        W2(L,KC)=0.  
      ENDDO  
C  
C **  CALCULATE PRODUCTION, LOAD BOUNDARY CONDITIONS AND SOLVE  
C **  TRANSPORT EQUATIONS  
C **  CU1=CUQ, CU2=CUQL, UUU=QQH, VVV=QQLH  
C *** DSLLC BEGIN BLOCK  
C  
      IF(ISWAVE.LE.1.OR.ISWAVE.EQ.3)THEN  
        DO K=1,KS  
          DO L=2,LA  
            LN=LNC(L)  
            PQQB=AB(L,K)*GP*HP(L)*DZIG(K)*(B(L,K+1)-B(L,K))  
            PQQU=AV(L,K)*DZIGSD4(K)*  
     &          (U(L+1,K+1)-U(L+1,K)+U(L,K+1)-U(L,K))**2  
            PQQV=AV(L,K)*DZIGSD4(K)*  
     &          (V(LN,K+1)-V(LN,K)+V(L,K+1)-V(L,K))**2  
            PQQ=DELT*(PQQB+PQQU+PQQV)  
            UUU(L,K)=QQ(L,K)*HP(L) + 2.*PQQ  
            PQQL=DELT*(CTE3*PQQB+CTE1*(PQQU+PQQV))  
            VVV(L,K)=QQL(L,K)*HP(L) + DML(L,K)*PQQL  
          ENDDO  
        ENDDO  
      ELSEIF(ISWAVE.EQ.2)THEN  
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
            LN=LNC(L)  
            PQQB=AB(L,K)*GP*HP(L)*DZIG(K)*(B(L,K+1)-B(L,K))  
            PQQU=AV(L,K)*DZIGSD4(K)*  
     &          (U(L+1,K+1)-U(L+1,K)+U(L,K+1)-U(L,K))**2  
            PQQV=AV(L,K)*DZIGSD4(K)*  
     &          (V(LN,K+1)-V(LN,K)+V(L,K+1)-V(L,K))**2  
            PQQW= WVFACT*TVAR1W(L,K)  
            PQQ=DELT*(PQQU+PQQV+PQQB+PQQW)  
            UUU(L,K)=QQ(L,K)*HP(L) + 2.*PQQ  
            PQQL=DELT*(CTE3*PQQB+CTE1*(PQQU+PQQV+PQQW))  
            VVV(L,K)=QQL(L,K)*HP(L) + DML(L,K)*PQQL  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C *** DSLLC END BLOCK  
C  
      DO L=2,LA  
        CLQTMP=-DELT*CDZKK(1)*AQ(L,1)*HPI(L)  
        CUQTMP=-DELT*CDZKKP(1)*AQ(L,2)*HPI(L)  
        CMQTMP=1.-CLQTMP-CUQTMP  
     &      +2.*DELT*QQSQR(L,1)/(CTURB*DML(L,1)*HP(L))  
        CMQLTMP=1.-CLQTMP-CUQTMP  
     &      +DELT*(QQSQR(L,1)/(CTURB*DML(L,1)*HP(L)))*(1.  
     &      +CTE2*DML(L,1)*DML(L,1)*FPROX(1))  
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
     &        +2.*DELT*QQSQR(L,K)/(CTURB*DML(L,K)*HP(L))  
          CMQLTMP=1.-CLQTMP-CUQTMP  
     &        +DELT*(QQSQR(L,K)/(CTURB*DML(L,K)*HP(L)))*(1.  
     &        +CTE2*DML(L,K)*DML(L,K)*FPROX(K))  
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
      DO K=1,KS  
        DO L=2,LA  
          QQ1(L,K)=S2TL*QQ1(L,K)+S3TL*QQ(L,K)  
          QQHDH=UUU(L,K)*HPI(L)  
          QQ(L,K)=MAX(QQHDH,QQMIN)  
          QQ(L,K)=SPB(L)*QQ(L,K)+(1.-SPB(L))*QQMIN  
          QQSQR(L,K)=SQRT(QQ(L,K))  ! *** DSLLC
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO L=2,LA  
          QQL1(L,K)=S2TL*QQL1(L,K)+S3TL*QQL(L,K)  
          QQHDH=VVV(L,K)*HPI(L)  
          QQL(L,K)=MAX(QQHDH,QQLMIN)  
          QQL(L,K)=SPB(L)*QQL(L,K)+(1.-SPB(L))*QQLMIN  
          DMLTMP=QQL(L,K)/QQ(L,K)  
          DMLTMP=MAX(DMLTMP,DMLMIN)  
          DELB=B(L,K)-B(L,K+1)  
          IF(DELB.GT.BSMALL)THEN  
            DMLMAX=CTE3*SQRT(QQ(L,K)/(G*HP(L)*DZIG(K)*DELB))  
            DML(L,K)=MIN(DMLMAX,DMLTMP)  
          ELSE  
            DML(L,K)=DMLTMP  
          ENDIF  
          DML(L,K)=SPB(L)*DML(L,K)+(1.-SPB(L))*DMLMIN  
        ENDDO  
      ENDDO  
      RETURN  
      END  

