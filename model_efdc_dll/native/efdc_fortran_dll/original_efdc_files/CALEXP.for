      SUBROUTINE CALEXP (ISTL_)  
C  
C **  SUBROUTINE CALEXP CALCULATES EXPLICIT MOMENTUM EQUATION TERMS  
C **  THIS SUBROUTINE IS CURRENT PRODUCTION VERSION  
C  
C **  THIS SUBROUTINE IS PART OF  EFDC-FULL VERSION 1.0a  
C  
C **  LAST MODIFIED BY JOHN HAMRICK ON 1 NOVEMBER 2001  
C  
C----------------------------------------------------------------------C  
C  
C CHANGE RECORD  
C DATE MODIFIED     BY                 DATE APPROVED    BY  
C  
C----------------------------------------------------------------------C  
C  
C**********************************************************************C  
C  
C **  THIS SUBROUTINE IS CURRENT PRODUCTION VERSION  
C CHANGE RECORD  
C  
      USE GLOBAL  

	IMPLICIT NONE
	INTEGER::LU,NS,ID,JD,KD,LD,K,L,LL,LW,LNW,LSE,LE
	INTEGER::ND,LF,NWR,IU,JU,KU,ISTL_,LN,LS
	REAL::QMF,QUMF,WU,VTMPATU,UTMPATV,UMAGTMP,VMAGTMP
	REAL::DZICK,DZICKC,WVFACT,RCDZF,WV,CACSUM,CFEFF,TMPVAL,DZPU,DZPV
	REAL::VHC,VHB,DELTD2,UHC,UHB
C  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::FUHJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::FVHJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::DZPC  
      IF(.NOT.ALLOCATED(DZPC))THEN
        ALLOCATE(FUHJ(LCM,KCM))  
        ALLOCATE(FVHJ(LCM,KCM))  
        ALLOCATE(DZPC(LCM,KCM))
        FUHJ=0.
        FVHJ=0.
        DZPC=0.
      ENDIF
C  
C**********************************************************************C  
C  
      DELT=DT2  
      DELTD2=DT  
      IF(ISTL_.EQ.2)THEN  
        DELT=DT  
        DELTD2=0.5*DT  
      ENDIF  
C  
      DELTI=1./DELT  
C  
      IF(N.EQ.1.AND.DEBUG)THEN  
        OPEN(1,FILE='MFLUX.DIA')  
        CLOSE(1,STATUS='DELETE')  
      ENDIF  
C  
      IF(N.LE.4.AND.DEBUG)THEN  
        OPEN(1,FILE='MFLUX.DIA',POSITION='APPEND')  
      ENDIF  
C  
C**********************************************************************C  
C  
C **  INITIALIZE EXTERNAL CORIOLIS-CURVATURE AND ADVECTIVE FLUX TERMS  
C  
C----------------------------------------------------------------------C  
C  
      ! *** DSLLC BEGIN BLOCK  
      DO L=1,LC  
        FCAXE(L)=0.  
        FCAYE(L)=0.  
C PMC        FCAX1E(L)=0.  
C PMC        FCAY1E(L)=0.  
        FXE(L)=0.  
        FYE(L)=0.  
      ENDDO  
C 
      IF(NQWR.GT.0)THEN
        DO K=1,KC  
          DO L=1,LC  
            FUHJ(L,K)=0.  
            FVHJ(L,K)=0.  
          ENDDO  
        ENDDO  
      ENDIF
      ! *** DSLLC END BLOCK  
C  
C**********************************************************************C  
C  
C **  SELECT ADVECTIVE FLUX FORM  
C  
C----------------------------------------------------------------------C  
C  
      IF(ISTL_.EQ.2) GOTO 200  
      IF(ISCDMA.EQ.0) GOTO 300  
      IF(ISCDMA.EQ.1) GOTO 400  
      IF(ISCDMA.EQ.2) GOTO 350  
C  
C**********************************************************************C  
C  
C **  TWO TIME LEVEL STEP  
C **  CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE WITH ADVECTION  
C **  AVERAGED BETWEEN (N) AND (N+1) AND ADVECTED FIELD AT N  
C  
  200 CONTINUE  
C  
C----------------------------------------------------------------------C  
C  
      DO K=1,KC  
        DO L=2,LA  
          UHDY2(L,K)=0.5*(UHDY1(L,K)+UHDY(L,K))  
          VHDX2(L,K)=0.5*(VHDX1(L,K)+VHDX(L,K))  
          W2(L,K)=W1(L,K)+W(L,K)  
        ENDDO  
      ENDDO  
C  
C----------------------------------------------------------------------C  
C  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L)  
          LS=LSC(L)  
          UHC=0.5*(UHDY2(L,K)+UHDY2(LS,K))  
          UHB=0.5*(UHDY2(L,K)+UHDY2(L+1,K))  
          VHC=0.5*(VHDX2(L,K)+VHDX2(L-1,K))  
          VHB=0.5*(VHDX2(L,K)+VHDX2(LN,K))  
C  
          FUHU(L,K)=MAX(UHB,0.)*U1(L,K)  
     &        +MIN(UHB,0.)*U1(L+1,K)  
          FVHU(L,K)=MAX(VHC,0.)*U1(LS,K)  
     &        +MIN(VHC,0.)*U1(L,K)  
          FUHV(L,K)=MAX(UHC,0.)*V1(L-1,K)  
     &        +MIN(UHC,0.)*V1(L,K)  
          FVHV(L,K)=MAX(VHB,0.)*V1(L,K)  
     &        +MIN(VHB,0.)*V1(LN,K)  
        ENDDO  
      ENDDO  
C  
C ADD RETURN FLOW MOMENTUM FLUX  
C  
      DO NWR=1,NQWR  
        IF(NQWRMFU(NWR).GT.0)THEN  
          IU=IQWRU(NWR)  
          JU=JQWRU(NWR)  
          KU=KQWRU(NWR)  
          LU=LIJ(IU,JU)  
          NS=NQWRSERQ(NWR)  
          QMF=QWR(NWR)+QWRSERT(NS)  
          QUMF=QMF*QMF/(H1P(LU)*DZC(KU)*DZC(KU)*BQWRMFU(NWR))  
          IF(NQWRMFU(NWR).EQ.1)  FUHJ(LU     ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.2)  FVHJ(LU     ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.3)  FUHJ(LU+1   ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.4)  FVHJ(LNC(LU),KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.-1) FUHJ(LU     ,KU)=QUMF  
          IF(NQWRMFU(NWR).EQ.-2) FVHJ(LU     ,KU)=QUMF  
          IF(NQWRMFU(NWR).EQ.-3) FUHJ(LU+1   ,KU)=QUMF  
          IF(NQWRMFU(NWR).EQ.-4) FVHJ(LNC(LU),KU)=QUMF  
        ENDIF  
        IF(NQWRMFD(NWR).GT.0)THEN  
          ID=IQWRD(NWR)  
          JD=JQWRD(NWR)  
          KD=KQWRD(NWR)  
          LD=LIJ(ID,JD)  
          NS=NQWRSERQ(NWR)  
          QMF=QWR(NWR)+QWRSERT(NS)  
          QUMF=QMF*QMF/(H1P(LD)*DZC(KD)*DZC(KD)*BQWRMFD(NWR))  
          IF(NQWRMFD(NWR).EQ.1)  FUHJ(LD     ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.2)  FVHJ(LD     ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.3)  FUHJ(LD+1   ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.4)  FVHJ(LNC(LD),KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.-1) FUHJ(LD     ,KD)=-QUMF  
          IF(NQWRMFD(NWR).EQ.-2) FVHJ(LD     ,KD)=-QUMF  
          IF(NQWRMFD(NWR).EQ.-3) FUHJ(LD+1   ,KD)=-QUMF  
          IF(NQWRMFD(NWR).EQ.-4) FVHJ(LNC(LD),KD)=-QUMF  
C         IF(N.LE.4.AND.DEBUG)THEN  
C           WRITE(1,1112)N,NWR,NS,ID,JD,KD,NQWRMFD(NWR),H1P(LD),QMF,  
C     &                  QUMF,FUHJ(LD,KD),FVHJ(LD,KD)  
C         ENDIF  
        ENDIF  
      ENDDO  
C  
C ** HARDWIRE FOR PEACH BOTTOM  
C  
C      DO K=1,KC  
C       FVHV(535,K)=700./H1P(535)  
C      ENDDO  
C  
C ** END HARDWIRE FOR PEACH BOTTOM  
C  
C----------------------------------------------------------------------C  
C  
      DO K=1,KS  
        DO L=2,LA  
          LS=LSC(L)
C          WU=0.5/(1.+SUB(L))*DXYU(L)*(W2(L,K)+SUB(L)*W2(L-1,K)) ! PMC 
C          WV=0.5/(1.+SVB(L))*DXYV(L)*(W2(L,K)+SVB(L)*W2(LS,K))  ! PMC
          WU=0.25*DXYU(L)*(W2(L,K)+W2(L-1,K))  
          WV=0.25*DXYV(L)*(W2(L,K)+W2(LS,K))  
          FWU(L,K)=MAX(WU,0.)*U1(L,K)  
     &        +MIN(WU,0.)*U1(L,K+1)  
          FWV(L,K)=MAX(WV,0.)*V1(L,K)  
     &        +MIN(WV,0.)*V1(L,K+1)  
        ENDDO  
      ENDDO  
C  
C----------------------------------------------------------------------C  
C  
      GOTO 500  
C  
C**********************************************************************C  
C  
C **  THREE TIME LEVEL (LEAP-FROG) STEP  
C **  WITH TRANSPORT AT (N) AND TRANSPORTED FIELD AT (N-1)  
C  
  300 CONTINUE  
C  
C----------------------------------------------------------------------C  
C  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L)  
          LS=LSC(L)  
          UHC=0.5*(UHDY(L,K)+UHDY(LS,K))  
          UHB=0.5*(UHDY(L,K)+UHDY(L+1,K))  
          VHC=0.5*(VHDX(L,K)+VHDX(L-1,K))  
          VHB=0.5*(VHDX(L,K)+VHDX(LN,K))  
          FUHU(L,K)=MAX(UHB,0.)*U1(L,K)  
     &        +MIN(UHB,0.)*U1(L+1,K)  
          FVHU(L,K)=MAX(VHC,0.)*U1(LS,K)  
     &        +MIN(VHC,0.)*U1(L,K)  
          FUHV(L,K)=MAX(UHC,0.)*V1(L-1,K)  
     &        +MIN(UHC,0.)*V1(L,K)  
          FVHV(L,K)=MAX(VHB,0.)*V1(L,K)  
     &        +MIN(VHB,0.)*V1(LN,K)  
        ENDDO  
      ENDDO  
C  
C ADD RETURN FLOW MOMENTUM FLUX  
C  
      DO NWR=1,NQWR  
        IF(NQWRMFU(NWR).GT.0)THEN  
          IU=IQWRU(NWR)  
          JU=JQWRU(NWR)  
          KU=KQWRU(NWR)  
          LU=LIJ(IU,JU)  
          NS=NQWRSERQ(NWR)  
          QMF=QWR(NWR)+QWRSERT(NS)  
          QUMF=QMF*QMF/(H1P(LU)*DZC(KU)*DZC(KU)*BQWRMFU(NWR))  
          IF(NQWRMFU(NWR).EQ.1)  FUHJ(LU     ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.2)  FVHJ(LU     ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.3)  FUHJ(LU+1   ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.4)  FVHJ(LNC(LU),KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.-1) FUHJ(LU     ,KU)=QUMF  
          IF(NQWRMFU(NWR).EQ.-2) FVHJ(LU     ,KU)=QUMF  
          IF(NQWRMFU(NWR).EQ.-3) FUHJ(LU+1   ,KU)=QUMF  
          IF(NQWRMFU(NWR).EQ.-4) FVHJ(LNC(LU),KU)=QUMF  
        ENDIF  
        IF(NQWRMFD(NWR).GT.0)THEN  
          ID=IQWRD(NWR)  
          JD=JQWRD(NWR)  
          KD=KQWRD(NWR)  
          LD=LIJ(ID,JD)  
          NS=NQWRSERQ(NWR)  
          QMF=QWR(NWR)+QWRSERT(NS)  
          QUMF=QMF*QMF/(H1P(LD)*DZC(KD)*DZC(KD)*BQWRMFD(NWR))  
          IF(NQWRMFD(NWR).EQ.1)  FUHJ(LD     ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.2)  FVHJ(LD     ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.3)  FUHJ(LD+1   ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.4)  FVHJ(LNC(LD),KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.-1) FUHJ(LD     ,KD)=-QUMF  
          IF(NQWRMFD(NWR).EQ.-2) FVHJ(LD     ,KD)=-QUMF  
          IF(NQWRMFD(NWR).EQ.-3) FUHJ(LD+1   ,KD)=-QUMF  
          IF(NQWRMFD(NWR).EQ.-4) FVHJ(LNC(LD),KD)=-QUMF  
        ENDIF  
      ENDDO  
C  
C----------------------------------------------------------------------C  
C  
      DO K=1,KS  
        DO L=2,LA  
          LS=LSC(L)  
          WU=0.5*DXYU(L)*(W(L,K)+W(L-1,K))  
          WV=0.5*DXYV(L)*(W(L,K)+W(LS,K))  
          FWU(L,K)=MAX(WU,0.)*U1(L,K)  
     &        +MIN(WU,0.)*U1(L,K+1)  
          FWV(L,K)=MAX(WV,0.)*V1(L,K)  
     &        +MIN(WV,0.)*V1(L,K+1)  
        ENDDO  
      ENDDO  
C  
C----------------------------------------------------------------------C  
C  
      GOTO 500  
C  
C**********************************************************************C  
C  
C **  THREE TIME LEVEL (LEAP-FROG) STEP  
C **  FIRST HALF STEP CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE  
C **  WITH TRANSPORT AT (N-1/2) AND TRANSPORTED FIELD AT (N-1)  
C **  SECOND HALF STEP CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE  
C **  WITH TRANSPORT AT (N+1/2) AND TRANSPORTED FIELD AT (N)  
C  
  350 CONTINUE  
C  
C----------------------------------------------------------------------C  
C  
      DO K=1,KC  
        DO L=2,LA  
          U2(L,K)=U1(L,K)+U(L,K)  
          V2(L,K)=V1(L,K)+V(L,K)  
        ENDDO  
      ENDDO  
C  
C----------------------------------------------------------------------C  
C  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L)  
          LS=LSC(L)  
          UHC=0.25*(UHDY(L,K)+UHDY(LS,K))  
          UHB=0.25*(UHDY(L,K)+UHDY(L+1,K))  
          VHC=0.25*(VHDX(L,K)+VHDX(L-1,K))  
          VHB=0.25*(VHDX(L,K)+VHDX(LN,K))  
          FUHU(L,K)=MAX(UHB,0.)*U2(L,K)  
     &        +MIN(UHB,0.)*U2(L+1,K)  
          FVHU(L,K)=MAX(VHC,0.)*U2(LS,K)  
     &        +MIN(VHC,0.)*U2(L,K)  
          FUHV(L,K)=MAX(UHC,0.)*V2(L-1,K)  
     &        +MIN(UHC,0.)*V2(L,K)  
          FVHV(L,K)=MAX(VHB,0.)*V2(L,K)  
     &        +MIN(VHB,0.)*V2(LN,K)  
        ENDDO  
      ENDDO  
C  
C----------------------------------------------------------------------C  
C  
      DO K=1,KS  
        DO L=2,LA  
          LS=LSC(L)  
          WU=0.25*DXYU(L)*(W(L,K)+W(L-1,K))  
          WV=0.25*DXYV(L)*(W(L,K)+W(LS,K))  
          FWU(L,K)=MAX(WU,0.)*U2(L,K)  
     &        +MIN(WU,0.)*U2(L,K+1)  
          FWV(L,K)=MAX(WV,0.)*V2(L,K)  
     &        +MIN(WV,0.)*V2(L,K+1)  
        ENDDO  
      ENDDO  
C  
C----------------------------------------------------------------------C  
C  
      GOTO 500  
C  
C**********************************************************************C  
C  
C **  THREE TIME LEVEL (LEAP-FROG) STEP  
C **  CALCULATE ADVECTIVE FLUXES BY CENTRAL DIFFERENCE WITH TRANSPORT  
C **  AT (N) AND TRANSPORTED FIELD AT (N)  
C  
  400 CONTINUE  
C  
C----------------------------------------------------------------------C  
C  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L)  
          LS=LSC(L)  
          FUHU(L,K)=0.25*(UHDY(L+1,K)+UHDY(L,K))*(U(L+1,K)+U(L,K))  
          FVHU(L,K)=0.25*(VHDX(L,K)+VHDX(L-1,K))*(U(L,K)+U(LS,K))  
          FUHV(L,K)=0.25*(UHDY(L,K)+UHDY(LS,K))*(V(L,K)+V(L-1,K))  
          FVHV(L,K)=0.25*(VHDX(L,K)+VHDX(LN,K))*(V(LN,K)+V(L,K))  
        ENDDO  
      ENDDO  
C  
C----------------------------------------------------------------------C  
C  
      DO K=1,KS  
        DO L=2,LA  
          LS=LSC(L)  
          FWU(L,K)=0.25*DXYU(L)*(W(L,K)+W(L-1,K))*(U(L,K+1)+U(L,K))  
          FWV(L,K)=0.25*DXYV(L)*(W(L,K)+W(LS,K))*(V(L,K+1)+V(L,K))  
        ENDDO  
      ENDDO  
C  
C**********************************************************************C  
C  
  500 CONTINUE  
C  
      IF(ITRICELL.GT.0)THEN
        DO K=1,KC  
          DO L=1,LA  
            FUHU(L,K)=STCUV(L)*FUHU(L,K)  
            FVHV(L,K)=STCUV(L)*FVHV(L,K)  
          ENDDO  
        ENDDO  
      ENDIF
C  
C**********************************************************************C  
C  
C **  CALCULATE CORIOLIS AND CURVATURE ACCELERATION COEFFICIENTS  
C  
C----------------------------------------------------------------------C  
C  
      CACSUM=0.0
      CFMAX=CF  
      IF(ISCURVATURE)THEN
        IF(ISDCCA.EQ.0)THEN  
C  
        
          DO K=1,KC  
            DO L=2,LA  
              LN=LNC(L)  
              CAC(L,K)=( FCORC(L)*DXYP(L)  
     &          +0.5*SNLT*(V(LN,K)+V(L,K))*DYDI(L)  
     &          -0.5*SNLT*(U(L+1,K)+U(L,K))*DXDJ(L) )*HP(L) 
              CACSUM=CACSUM+CAC(L,K) 
            ENDDO  
          ENDDO  
C  
        ELSE  
C  
          DO K=1,KC  
            DO L=2,LA  
              LN=LNC(L)  
              CAC(L,K)=( FCORC(L)*DXYP(L)  
     &          +0.5*SNLT*(V(LN,K)+V(L,K))*DYDI(L)  
     &          -0.5*SNLT*(U(L+1,K)+U(L,K))*DXDJ(L) )*HP(L)  
              CFEFF=ABS(CAC(L,K))*DXYIP(L)*HPI(L)  
              CFMAX=MAX(CFMAX,CFEFF)  
              CACSUM=CACSUM+CAC(L,K) 
            ENDDO  
          ENDDO  
C  
        ENDIF  
      ENDIF
C  
C      IF(N.EQ.2)THEN  
C       OPEN(1,FILE='CORC.DIA')  
C       CLOSE(2,STATUS='DELETE')  
C       OPEN(1,FILE='CORC.DIA')  
C       K=1  
C       DO L=2,LA  
C       LN=LNC(L)  
C       WRITE(1,1111)IL(L),JL(L),LN,V(LN,K),V(L,K),DYU(L+1),DYU(L),  
C     &        U(L+1,K),U(L,K),DXV(LN),DXV(L),HP(L),CAC(L,K)  
C       ENDDO  
C       CLOSE(1)  
C      ENDIF  
 1111 FORMAT(3I5,10E13.4)  
C  
C**********************************************************************C  
C  
C **  CALCULATE CORIOLIS-CURVATURE AND ADVECTIVE ACCELERATIONS  
C  
C----------------------------------------------------------------------C  
C  
      IF(CACSUM.GT.1.E-7)THEN
        DO K=1,KC  
          DO L=1,LC  
            FCAX1(L,K)=0.  
            FCAY1(L,K)=0.  
          ENDDO  
        ENDDO  
C  
C----------------------------------------------------------------------C  
C  
        DO K=1,KC  
          DO L=2,LA  
            LN=LNC(L)  
            LS=LSC(L)  
            LNW=LNWC(L)  
            LSE=LSEC(L)  
            FCAX(L,K)=ROLD*FCAX(L,K)  
     &        +0.25*RNEW*SCAX(L)*(CAC(L,K)*(V(LN,K)+V(L,K))  
     &        +CAC(L-1,K)*(V(LNW,K)+V(L-1,K)))  
            FCAY(L,K)=ROLD*FCAY(L,K)  
     &        +0.25*RNEW*SCAY(L)*(CAC(L,K)*(U(L+1,K)+U(L,K))  
     &        +CAC(LS,K)*(U(LSE,K)+U(LS,K)))  
          ENDDO  
        ENDDO
C  
C----------------------------------------------------------------------C  
C  
C **  MODIFICATION FOR TYPE 2 OPEN BOUNDARIES  
C  
        DO LL=1,NPBW  
          IF(ISPBW(LL).EQ.2)THEN  
            L=LPBW(LL)+1  
            LN=LNC(L)  
            DO K=1,KC  
              FCAX(L,K)=0.5*SCAX(L)*CAC(L,K)*(V(LN,K)+V(L,K))  
            ENDDO  
          ENDIF  
        ENDDO  
C  
        DO LL=1,NPBE  
          IF(ISPBE(LL).EQ.2)THEN  
            L=LPBE(LL)  
            LNW=LNWC(L)  
            DO K=1,KC  
              FCAX(L,K)=0.5*SCAX(L)*CAC(L-1,K)*(V(LNW,K)+V(L-1,K))  
            ENDDO  
          ENDIF  
        ENDDO  
C  
        DO LL=1,NPBS  
          IF(ISPBS(LL).EQ.2)THEN  
            L=LNC(LPBS(LL))  
            DO K=1,KC  
              FCAY(L,K)=0.5*SCAY(L)*CAC(L,K)*(U(L+1,K)+U(L,K))  
            ENDDO  
          ENDIF  
        ENDDO  
C  
        DO LL=1,NPBN  
          IF(ISPBN(LL).EQ.2)THEN  
            L=LPBN(LL)  
            LS=LSC(L)  
            LSE=LSEC(L)  
            DO K=1,KC  
              FCAY(L,K)=0.5*SCAY(L)*CAC(LS,K)*(U(LSE,K)+U(LS,K))  
            ENDDO  
          ENDIF  
        ENDDO  
      ENDIF
C  
C----------------------------------------------------------------------C  
C *** COMPUTE MAIN MOMENTUM TERMS
C  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L) 
          LS=LSC(L) 
          !HRUO(L)=SUBO(L)*DYU(L)*DXIU(L) 
          !HRXYU(L)=DXU(L)/DYU(L)          ! PMC - NOT USED
          FX(L,K)=(FUHU(L,K)-FUHU(L-1,K)+FVHU(LN,K)-FVHU(L,K)  
     &            +FUHJ(L,K) )  
          FY(L,K)=(FUHV(L+1,K)-FUHV(L,K)+FVHV(L,K)-FVHV(LS,K)  
     &            +FVHJ(L,K) )  
        ENDDO  
      ENDDO

      ! *** TREAT BC'S NEAR EDGES        PMC
      DO LL=1,NBCS
        ! *** BC CELL
        L=LBCS(LL)
        DO K=1,KC
          FX(L,K)=SAAX(L)*FX(L,K)
          FY(L,K)=SAAY(L)*FY(L,K)
        ENDDO

        ! *** EAST/WEST ADJACENT CELL
        L=LBERC(LL)
        DO K=1,KC
          FX(L,K)=SAAX(L)*FX(L,K)
        ENDDO

        ! *** NORTH/SOUTH ADJACENT CELL
        L=LBNRC(LL)
        DO K=1,KC
          FY(L,K)=SAAY(L)*FY(L,K)
        ENDDO
      ENDDO  
C  
C**********************************************************************C  
C  
C **  ADD VEGETATION DRAG TO HORIZONTAL ADVECTIVE ACCELERATIONS  
C  
C----------------------------------------------------------------------C  
C  
      IF(ISVEG.GE.1)THEN  
C  
        DO L=2,LA  
          FXVEGE(L)=0.  
          FYVEGE(L)=0.  
        ENDDO  
C  
        DO K=1,KC  
          DO L=2,LA  
            LW=L-1  
            LE=L+1  
            LS=LSC(L)  
            LN=LNC(L)  
            LNW=LNWC(L)  
            LSE=LSEC(L)  
            VTMPATU=0.25*(V1(L,K)+V1(LW,K)+V1(LN,K)+V1(LNW,K))  
            UTMPATV=0.25*(U1(L,K)+U1(LE,K)+U1(LS,K)+U1(LSE,K))  
            UMAGTMP=SQRT( U1(L,K)*U1(L,K)+VTMPATU*VTMPATU )  
            VMAGTMP=SQRT( UTMPATV*UTMPATV+V1(L,K)*V1(L,K) )  
            FXVEG(L,K)=UMAGTMP*SUB(L)*DXYU(L)*FXVEG(L,K)  
            FYVEG(L,K)=VMAGTMP*SVB(L)*DXYV(L)*FYVEG(L,K)  
            FXVEGE(L)=FXVEGE(L)+FXVEG(L,K)*DZC(K)  
            FYVEGE(L)=FYVEGE(L)+FYVEG(L,K)*DZC(K)  
          ENDDO  
        ENDDO  
C  
        DO K=1,KC  
          DO L=2,LA  
            FXVEG(L,K)=FXVEG(L,K)*U1(L,K)  
            FYVEG(L,K)=FYVEG(L,K)*V1(L,K)  
            FX(L,K)=FX(L,K)+FXVEG(L,K)-FXVEGE(L)*U1(L,K)  
            FY(L,K)=FY(L,K)+FYVEG(L,K)-FYVEGE(L)*V1(L,K)  
          ENDDO  
        ENDDO  
C  
        DO L=2,LA  
          FXVEGE(L)=DXYIU(L)*FXVEGE(L)/H1U(L)  
          FYVEGE(L)=DXYIV(L)*FYVEGE(L)/H1V(L)  
        ENDDO  
C  
      ENDIF  
C  
C  
C**********************************************************************C  
C  
C **  ADD HORIZONTAL MOMENTUM DIFFUSION TO ADVECTIVE ACCELERATIONS  
C  
C----------------------------------------------------------------------C  
C  
      IF(ISHDMF.GE.1)THEN  
        DO K=1,KC  
          DO L=2,LA  
            LS=LSC(L)  
            LN=LNC(L)  
            FX(L,K)=FX(L,K)-(FMDUX(L,K)+FMDUY(L,K))  
            FY(L,K)=FY(L,K)-(FMDVX(L,K)+FMDVY(L,K))  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C**********************************************************************C  
C  
C **  ADD BODY FORCE TO ADVECTIVE ACCELERATIONS  
C **  DISTRIBUTE UNIFORMLY OVER ALL LAYERS IF ISBODYF=1  
C **  DISTRIBUTE OVER SURFACE LAYER IF ISBODYF=2  
C  
C----------------------------------------------------------------------C  
C  
      IF(ISBODYF.EQ.1)THEN  
C  
        DO K=1,KC  
          DZICK=1./DZC(K)  
          DO L=2,LA  
            FX(L,K)=FX(L,K)-DYU(L)*HU(L)*FBODYFX(L)  
            FY(L,K)=FY(L,K)-DXV(L)*HV(L)*FBODYFY(L)  
          ENDDO  
        ENDDO  
C  
      ENDIF  
C  
      IF(ISBODYF.EQ.2)THEN  
C  
        DZICKC=1./DZC(KC)  
        DO L=2,LA  
          FX(L,KC)=FX(L,KC)-DZICKC*DYU(L)*HU(L)*FBODYFX(L)  
          FY(L,KC)=FY(L,KC)-DZICKC*DXV(L)*HV(L)*FBODYFY(L)  
        ENDDO  
C  
      ENDIF  
C  
C**********************************************************************C  
C  
C ** ADD EXPLICIT NONHYDROSTATIC PRESSURE  
C  
      IF(KC.GT.1.AND.ISPNHYDS.GE.1) THEN  
C  
        TMPVAL=2./(DZC(1)+DZC(2))  
        DO L=2,LA  
          DZPC(L,1)=TMPVAL*(PNHYDS(L,2)-PNHYDS(L,1))  
        ENDDO  
C  
        TMPVAL=2./(DZC(KC)+DZC(KC-1))  
        DO L=2,LA  
          DZPC(L,KC)=TMPVAL*(PNHYDS(L,KC)-PNHYDS(L,KC-1))  
        ENDDO  
  
        IF(KC.GE.3)THEN  
          DO K=2,KS  
            TMPVAL=2./(DZC(K+1)+2.*DZC(K)+DZC(K-1))  
            DO L=2,LA  
              DZPC(L,K)=TMPVAL*(PNHYDS(L,K+1)-PNHYDS(L,K-1))  
            ENDDO  
          ENDDO  
        ENDIF  
C  
        DO K=1,KC  
          DO L=2,LA  
            LS=LSC(L)  
            DZPU=0.5*(DZPC(L,K)+DZPC(L-1,K))  
            DZPV=0.5*(DZPC(L,K)+DZPC(LS ,K))  
            FX(L,K)=FX(L,K)+SUB(L)*DYU(L)*  
     &          ( HU(L)*(PNHYDS(L,K)-PNHYDS(L-1,K))  
     &          -( BELV(L)-BELV(L-1)+ZZ(K)*(HP(L)-HP(L-1)) )*DZPU )  
            FY(L,K)=FY(L,K)+SVB(L)*DXV(L)*  
     &          ( HV(L)*(PNHYDS(L,K)-PNHYDS(LS ,K))  
     &          -( BELV(L)-BELV(LS )+ZZ(K)*(HP(L)-HP(LS )) )*DZPV )  
          ENDDO  
        ENDDO  
C  
      ENDIF  
C  
C----------------------------------------------------------------------C  
C  
C **  ADD NET WAVE REYNOLDS STRESSES TO EXTERNAL ADVECTIVE ACCEL.  
C  
C *** DSLLC BEGIN BLOCK  
      IF(ISWAVE.EQ.2)THEN  
C  
        IF(N.LT.NTSWV)THEN  
          TMPVAL=FLOAT(N)/FLOAT(NTSWV)  
          WVFACT=0.5-0.5*COS(PI*TMPVAL)  
        ELSE  
          WVFACT=1.0  
        ENDIF  
C  
        DO K=1,KC  
          DO L=2,LA  
            FX(L,K)=FX(L,K)+WVFACT*SAAX(L)*FXWAVE(L,K)  
            FY(L,K)=FY(L,K)+WVFACT*SAAY(L)*FYWAVE(L,K)  
          ENDDO  
        ENDDO  
C  
      ENDIF  
C *** DSLLC END BLOCK  
C  
C**********************************************************************C  
C  
C **  CALCULATE EXTERNAL ACCELERATIONS  
C  
C----------------------------------------------------------------------C  
C  
      DO K=1,KC  
        DO L=2,LA  
          FCAXE(L)=FCAXE(L)+FCAX(L,K)*DZC(K)  
          FCAYE(L)=FCAYE(L)+FCAY(L,K)*DZC(K)  
          FXE(L)=FXE(L)+FX(L,K)*DZC(K)  
          FYE(L)=FYE(L)+FY(L,K)*DZC(K)  
        ENDDO  
      ENDDO  
C  
C**********************************************************************C  
C  
C **  COMPLETE CALCULATION OF INTERNAL ADVECTIVE ACCELERATIONS  
C  
C----------------------------------------------------------------------C  
C  
      IF(KC.GT.1)THEN
        DO K=1,KC  
          DO L=2,LA  
            FX(L,K)=FX(L,K)+SAAX(L)*(FWU(L,K)-FWU(L,K-1))*DZIC(K)  
            FY(L,K)=FY(L,K)+SAAY(L)*(FWV(L,K)-FWV(L,K-1))*DZIC(K)  
          ENDDO  
        ENDDO  
      ENDIF
C  
C**********************************************************************C  
C  
C **  CALCULATE EXPLICIT INTERNAL BUOYANCY FORCINGS CENTERED AT N FOR  
C **  THREE TIME LEVEL STEP AND AT (N+1/2) FOR TWO TIME LEVEL STEP  
C **  SBX=SBX*0.5*DYU & SBY=SBY*0.5*DXV  
C  
C----------------------------------------------------------------------C  
C  
      IF(BSC.GT.1.E-6)THEN
C
      IF(IINTPG.EQ.0)THEN  
C ***   ORIGINAL  
        DO K=1,KS  
          DO L=2,LA  
            LS=LSC(L)  
            FBBX(L,K)=ROLD*FBBX(L,K)+RNEW*SBX(L)*GP*HU(L)*  
     &          ( HU(L)*( (B(L,K+1)-B(L-1,K+1))*DZC(K+1)  
     &          +(B(L,K)-B(L-1,K))*DZC(K) )  
     &          -(B(L,K+1)-B(L,K)+B(L-1,K+1)-B(L-1,K))*  
     &          (BELV(L)-BELV(L-1)+Z(K)*(HP(L)-HP(L-1))) )  
            FBBY(L,K)=ROLD*FBBY(L,K)+RNEW*SBY(L)*GP*HV(L)*  
     &          ( HV(L)*( (B(L,K+1)-B(LS,K+1))*DZC(K+1)  
     &          +(B(L,K)-B(LS,K))*DZC(K) )  
     &          -(B(L,K+1)-B(L,K)+B(LS,K+1)-B(LS,K))*  
     &          (BELV(L)-BELV(LS)+Z(K)*(HP(L)-HP(LS))) )  
          ENDDO  
        ENDDO  
C  
      ENDIF  
C  
C     JACOBIAN  
C  
      IF(IINTPG.EQ.1)THEN  
C  
        K=1  
        DO L=2,LA  
          LS=LSC(L)  
          FBBX(L,K)=ROLD*FBBX(L,K)+RNEW*SBX(L)*GP*HU(L)*  
     &        ( 0.5*HU(L)*( (B(L,K+2)-B(L-1,K+2))*DZC(K+2)  
     &        +(B(L,K+1)-B(L-1,K+1))*DZC(K+1)  
     &        +(B(L,K  )-B(L-1,K  ))*DZC(K  )  
     &        +(B(L,K  )-B(L-1,K  ))*DZC(K  ) )  
     &        -0.5*(B(L,K+2)-B(L,K+1)+B(L-1,K+2)-B(L-1,K+1))*  
     &        (BELV(L)-BELV(L-1)+Z(K+1)*(HP(L)-HP(L-1)))  
     &        -0.5*(B(L,K  )-B(L,K  )+B(L-1,K  )-B(L-1,K  ))*  
     &        (BELV(L)-BELV(L-1)+Z(K-1)*(HP(L)-HP(L-1))) )
C  
          FBBY(L,K)=ROLD*FBBY(L,K)+RNEW*SBY(L)*GP*HV(L)*  
     &        ( 0.5*HV(L)*( (B(L,K+2)-B(LS ,K+2))*DZC(K+2)  
     &        +(B(L,K+1)-B(LS ,K+1))*DZC(K+1)  
     &        +(B(L,K  )-B(LS ,K  ))*DZC(K  )  
     &        +(B(L,K  )-B(LS ,K  ))*DZC(K  ) )  
     &        -0.5*(B(L,K+2)-B(L,K+1)+B(LS ,K+2)-B(LS ,K+1))*  
     &        (BELV(L)-BELV(LS)+Z(K+1)*(HP(L)-HP(LS)))  
     &        -0.5*(B(L,K  )-B(L,K  )+B(LS ,K  )-B(LS ,K  ))*  
     &        (BELV(L)-BELV(LS )+Z(K-1)*(HP(L)-HP(LS ))) )  
        ENDDO  
C  
        IF(KC.GT.2)THEN  
          K=KS  
          DO L=2,LA  
            LS=LSC(L)  
            FBBX(L,K)=ROLD*FBBX(L,K)+RNEW*SBX(L)*GP*HU(L)*  
     &          ( 0.5*HU(L)*( (B(L,K+1)-B(L-1,K+1))*DZC(K+1)  
     &          +(B(L,K+1)-B(L-1,K+1))*DZC(K+1)  
     &          +(B(L,K  )-B(L-1,K  ))*DZC(K  )  
     &          +(B(L,K-1)-B(L-1,K-1))*DZC(K-1) )  
     &          -0.5*(B(L,K+1)-B(L,K+1)+B(L-1,K+1)-B(L-1,K+1))*  
     &          (BELV(L)-BELV(L-1)+Z(K+1)*(HP(L)-HP(L-1)))  
     &          -0.5*(B(L,K  )-B(L,K-1)+B(L-1,K  )-B(L-1,K-1))*  
     &          (BELV(L)-BELV(L-1)+Z(K-1)*(HP(L)-HP(L-1))) )  
            FBBY(L,K)=ROLD*FBBY(L,K)+RNEW*SBY(L)*GP*HV(L)*  
     &          ( 0.5*HV(L)*( (B(L,K+1)-B(LS ,K+1))*DZC(K+1)  
     &          +(B(L,K+1)-B(LS ,K+1))*DZC(K+1)  
     &          +(B(L,K  )-B(LS ,K  ))*DZC(K  )  
     &          +(B(L,K-1)-B(LS ,K-1))*DZC(K-1) )  
     &          -0.5*(B(L,K+1)-B(L,K+1)+B(LS ,K+1)-B(LS ,K+1))*  
     &          (BELV(L)-BELV(LS)+Z(K+1)*(HP(L)-HP(LS)))  
     &          -0.5*(B(L,K  )-B(L,K-1)+B(LS ,K  )-B(LS ,K-1))*  
     &          (BELV(L)-BELV(LS )+Z(K-1)*(HP(L)-HP(LS ))) )  
          ENDDO  
        ENDIF  
C  
        IF(KC.GT.3)THEN  
          DO K=1,KS  
            DO L=2,LA  
              LS=LSC(L)  
              FBBX(L,K)=ROLD*FBBX(L,K)+RNEW*SBX(L)*GP*HU(L)*  
     &            ( 0.5*HU(L)*( (B(L,K+2)-B(L-1,K+2))*DZC(K+2)  
     &            +(B(L,K+1)-B(L-1,K+1))*DZC(K+1)  
     &            +(B(L,K  )-B(L-1,K  ))*DZC(K  )  
     &            +(B(L,K-1)-B(L-1,K-1))*DZC(K-1) )  
     &            -0.5*(B(L,K+2)-B(L,K+1)+B(L-1,K+2)-B(L-1,K+1))*  
     &            (BELV(L)-BELV(L-1)+Z(K+1)*(HP(L)-HP(L-1)))  
     &            -0.5*(B(L,K  )-B(L,K-1)+B(L-1,K  )-B(L-1,K-1))*  
     &            (BELV(L)-BELV(L-1)+Z(K-1)*(HP(L)-HP(L-1))) )  
              FBBY(L,K)=ROLD*FBBY(L,K)+RNEW*SBY(L)*GP*HV(L)*  
     &            ( 0.5*HV(L)*( (B(L,K+2)-B(LS ,K+2))*DZC(K+2)  
     &            +(B(L,K+1)-B(LS ,K+1))*DZC(K+1)  
     &            +(B(L,K  )-B(LS ,K  ))*DZC(K  )  
     &            +(B(L,K-1)-B(LS ,K-1))*DZC(K-1) )  
     &            -0.5*(B(L,K+2)-B(L,K+1)+B(LS ,K+2)-B(LS ,K+1))*  
     &            (BELV(L)-BELV(LS)+Z(K+1)*(HP(L)-HP(LS)))  
     &            -0.5*(B(L,K  )-B(L,K-1)+B(LS ,K  )-B(LS ,K-1))*  
     &            (BELV(L)-BELV(LS )+Z(K-1)*(HP(L)-HP(LS ))) )  
            ENDDO  
          ENDDO  
        ENDIF  
C  
      ENDIF  
C  
C     FINITE VOLUME  
C  
      IF(IINTPG.EQ.2)THEN  
C  
        DO K=1,KS  
          DO L=2,LA  
            LS=LSC(L)  
            FBBX(L,K)=ROLD*FBBX(L,K)  
     &          +RNEW*SBX(L)*GP*HU(L)*  
     &          ( ( HP(L)*B(L,K+1)-HP(L-1)*B(L-1,K+1) )*DZC(K+1)  
     &          +( HP(L)*B(L,K  )-HP(L-1)*B(L-1,K  ) )*DZC(K  ) )  
     &          -RNEW*SBX(L)*GP*(BELV(L)-BELV(L-1))*  
     &          ( HP(L)*B(L,K+1)-HP(L)*B(L,K)  
     &          +HP(L-1)*B(L-1,K+1)-HP(L-1)*B(L-1,K) )  
     &          -RNEW*SBX(L)*GP*(HP(L)-HP(L-1))*  
     &          ( HP(L)*ZZ(K+1)*B(L,K+1)-HP(L)*ZZ(K)*B(L,K)  
     &          +HP(L-1)*ZZ(K+1)*B(L-1,K+1)-HP(L-1)*ZZ(K)*B(L-1,K) )  
            FBBY(L,K)=ROLD*FBBY(L,K)  
     &          +RNEW*SBY(L)*GP*HV(L)*  
     &          ( ( HP(L)*B(L,K+1)-HP(LS )*B(LS ,K+1) )*DZC(K+1)  
     &          +( HP(L)*B(L,K  )-HP(LS )*B(LS ,K  ) )*DZC(K  ) )  
     &          -RNEW*SBY(L)*GP*(BELV(L)-BELV(LS ))*  
     &          ( HP(L)*B(L,K+1)-HP(L)*B(L,K)  
     &          +HP(LS)*B(LS ,K+1)-HP(LS)*B(LS ,K) )  
     &          -RNEW*SBY(L)*GP*(HP(L)-HP(LS ))*  
     &          ( HP(L)*ZZ(K+1)*B(L,K+1)-HP(L)*ZZ(K)*B(L,K)  
     &          +HP(LS)*ZZ(K+1)*B(LS ,K+1)-HP(LS)*ZZ(K)*B(LS ,K) )  
          ENDDO  
        ENDDO  
C  
      ENDIF  

      ENDIF  ! *** END OF BOUYANCY 

C  
C     IF(N.EQ.1)THEN  
C       OPEN(1,FILE='BUOY.DIA',STATUS='UNKNOWN')  
C       DO L=2,LA  
C        DO K=1,KS  
C        TMP3D(K)=SUBO(L)*FBBX(L,K)  
C        ENDDO  
C       WRITE(1,1111)IL(L),JL(L),(TMP3D(K),K=1,KS)  
C        DO K=1,KS  
C        TMP3D(K)=SVBO(L)*FBBY(L,K)  
C        ENDDO  
C       WRITE(1,1111)IL(L),JL(L),(TMP3D(K),K=1,KS)  
C       ENDDO  
C       CLOSE(1)  
C     ENDIF  
C  
C 1111 FORMAT(2I5,2X,8E12.4)  
C  
C**********************************************************************C  
C  
C **  CALCULATE EXPLICIT INTERNAL U AND V SHEAR EQUATION TERMS  
C  
C----------------------------------------------------------------------C  
C  
      IF(KC.GT.1)THEN
        DO L=1,LC
          DU(L,KC)=0.0
          DV(L,KC)=0.0
        ENDDO  
        DO K=1,KS  
          RCDZF=CDZF(K)  
          DO L=2,LA  
            !DXYIU(L)=1./(DXU(L)*DYU(L))  
            DU(L,K)=RCDZF*( H1U(L)*(U1(L,K+1)-U1(L,K))*DELTI  
     &        +DXYIU(L)*(FCAX(L,K+1)-FCAX(L,K)+FBBX(L,K)  
     &        +SNLT*(FX(L,K)-FX(L,K+1))) )  
            DV(L,K)=RCDZF*( H1V(L)*(V1(L,K+1)-V1(L,K))*DELTI  
     &        +DXYIV(L)*(FCAY(L,K)-FCAY(L,K+1)+FBBY(L,K)  
     &        +SNLT*(FY(L,K)-FY(L,K+1))) )  
          ENDDO  
        ENDDO  
      ENDIF
C  
      IF(ISTL_.EQ.2.AND.NWSER.GT.0)THEN  
C  
        DO ND=1,NDM  
          LF=2+(ND-1)*LDM  
          LL=LF+LDM-1  
          DO L=LF,LL  
            DU(L,KS)=DU(L,KS)-CDZU(KS)*TSX(L)  
            DV(L,KS)=DV(L,KS)-CDZU(KS)*TSY(L)  
          ENDDO  
        ENDDO  
C  
      ENDIF  
C  
C**********************************************************************C  
C  
      IF(N.LE.4)THEN  
        CLOSE(1)  
      ENDIF  
C  
 1112 FORMAT('N,NW,NS,I,J,K,NF,H,Q,QU,FUU,FVV=',/,2X,7I5,5E12.4)  
C  
C**********************************************************************C  
C  
      RETURN  
      END  
