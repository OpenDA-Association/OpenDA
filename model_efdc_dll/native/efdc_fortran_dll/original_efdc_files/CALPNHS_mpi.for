      SUBROUTINE CALPNHS_mpi
C  
C CHANGE RECORD  
C **  SUBROUTINE CALPNHS CALCULATES QUASI-NONHYDROSTATIC PRESSURE  
C  
      USE GLOBAL  
      USE MPI

      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::PNHYDSS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::FWJET  
      IF(.NOT.ALLOCATED(PNHYDSS))THEN
        ALLOCATE(PNHYDSS(LCM,KCM))
        ALLOCATE(FWJET(LCM,KCM))
        PNHYDSS=0.0 
        FWJET=0.0 
      ENDIF
C  
      IF(ISDYNSTP.EQ.0)THEN  
        DELT=DT  
        DELTD2=0.5*DT  
        DELTI=1./DELT  
      ELSE  
        DELT=DTDYN  
        DELTD2=0.5*DTDYN  
        DELTI=1./DELT  
      END IF  
      IF(N.EQ.1)THEN  
        DO K=0,KC  
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA  
            WZ1(L,K)=0. 
            FWJET(L,K)=0.  ! *** DSLLC 
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE THE PHYSICAL VERTICAL VELOCIY  
C  
      IF(NPROCS.GT.1)THEN
C        CALL BROADCAST_BOUNDARY(P,IC)
        CALL BROADCAST_BOUNDARY(DXIU,IC)
        CALL BROADCAST_BOUNDARY(DYIV,IC)
C        CALL BROADCAST_BOUNDARY_ARRAY(U,IC)
C        CALL BROADCAST_BOUNDARY_ARRAY(V,IC)
      ENDIF
C
!$OMP PARALLEL DO PRIVATE(LN,LS)
      DO L=LMPI2,LMPILA
        LN=LNC(L)  
        LS=LSC(L)  
        WZ(L,0)=DELTI*(BELV(L)-BELV1(L))  
        WZ(L,KC)=GI*( DELTI*(P(L)-P1(L))  
     &      +0.5*U(L+1,KC)*(P(L+1)-P(L))*DXIU(L+1)  
     &      +0.5*U(L,KC)*(P(L)-P(L-1))*DXIU(L)  
     &      +0.5*V(LN,KC)*(P(LN)-P(L))*DYIV(LN)  
     &      +0.5*V(L,KC)*(P(L)-P(LS))*DYIV(L) )  
      ENDDO  
      IF(KC.GT.2)THEN  
        DO K=1,KS  
!$OMP PARALLEL DO PRIVATE(LN,LS)
          DO L=LMPI2,LMPILA
            LN=LNC(L)  
            LS=LSC(L)  
            WZ(L,K)=W(L,K)+GI*ZZ(K)*( DELTI*(P(L)-P1(L))  
     &          +0.5*U(L+1,K)*(P(L+1)-P(L))*DXIU(L+1)  
     &          +0.5*U(L,K)*(P(L)-P(L-1))*DXIU(L)  
     &          +0.5*V(LN,K)*(P(LN)-P(L))*DYIV(LN)  
     &          +0.5*V(L,K)*(P(L)-P(LS))*DYIV(L) )  
     &          +(1.-ZZ(K))*( DELTI*(BELV(L)-BELV1(L))  
     &          +0.5*U(L+1,K)*(BELV(L+1)-BELV(L))*DXIU(L+1)  
     &          +0.5*U(L,K)*(BELV(L)-BELV(L-1))*DXIU(L)  
     &          +0.5*V(LN,K)*(BELV(LN)-BELV(L))*DYIV(LN)  
     &          +0.5*V(L,K)*(BELV(L)-BELV(LS))*DYIV(L) )  
          ENDDO  
        ENDDO  
      ENDIF  
C  
      IF(NPROCS.GT.1)THEN
        CALL BROADCAST_BOUNDARY_ARRAY(WZ,IC)
      ENDIF
C **  CALCULATE FLUXES  
C  
      DO K=1,KC  
!$OMP PARALLEL DO
        DO L=LMPI1,LMPILC
          PNHYDSS(L,K)=PNHYDS(L,K)  
          FUHU(L,K)=0.  
          FVHU(L,K)=0.  
          FWQQ(L,KC)=0.  
        ENDDO  
      ENDDO  
      DO K=1,KS  
!$OMP PARALLEL DO PRIVATE(LS)
        DO L=LMPI2,LMPILA
          LS=LSC(L)  
          UHUW=0.5*(UHDY(L,K)+UHDY(L,K+1))  
          VHVW=0.5*(VHDX(L,K)+VHDX(L,K+1))  
          FUHU(L,K)=MAX(UHUW,0.)*WZ(L-1,K)  
     &        +MIN(UHUW,0.)*WZ(L,K)  
          FVHU(L,K)=MAX(VHVW,0.)*WZ(LS,K)  
     &        +MIN(VHVW,0.)*WZ(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
!$OMP PARALLEL DO PRIVATE(WB)
        DO L=LMPI2,LMPILA
          WB=0.5*DXYP(L)*(W(L,K-1)+W(L,K))  
          FWQQ(L,K)=MAX(WB,0.)*WZ(L,K-1)  
     &        +MIN(WB,0.)*WZ(L,K)  
          FWJET(L,K)=0.  
        ENDDO  
      ENDDO  
C  
C **  ADD RETURN FLOW MOMENTUM FLUX  
C  
      DO NWR=1,NQWR  
        IF(NQWRMFU(NWR).GT.0)THEN  
          IU=IQWRU(NWR)  
          JU=JQWRU(NWR)  
          KU=KQWRU(NWR)  
          LU=LIJ(IU,JU)  
          NS=NQWRSERQ(NWR)  
          QMF=QWR(NWR)+QWRSERT(NS)  
          QUMF=QMF*QMF/(H1P(LU)*DZC(KU)*BQWRMFU(NWR))  
          IF(NQWRMFU(NWR).EQ.1)  FWJET(LU     ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.2)  FWJET(LU     ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.3)  FWJET(LU+1   ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.4)  FWJET(LNC(LU),KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.-1) FWJET(LU     ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.-2) FWJET(LU     ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.-3) FWJET(LU+1   ,KU)=-QUMF  
          IF(NQWRMFU(NWR).EQ.-4) FWJET(LNC(LU),KU)=-QUMF  
        ENDIF  
        IF(NQWRMFD(NWR).GT.0)THEN  
          ID=IQWRD(NWR)  
          JD=JQWRD(NWR)  
          KD=KQWRD(NWR)  
          LD=LIJ(ID,JD)  
          ADIFF=ABS(ANGWRMFD(NWR)-90.)  
          IF(ADIFF.LT.1.0)THEN  
            TMPANG=1.  
          ELSE  
            TMPANG=0.017453*ANGWRMFD(NWR)  
            TMPANG=SIN(TMPANG)  
          ENDIF  
          NS=NQWRSERQ(NWR)  
          QMF=QWR(NWR)+QWRSERT(NS)  
          QUMF=TMPANG*QMF*QMF/(H1P(LD)*DZC(KD)*BQWRMFD(NWR))  
          IF(NQWRMFD(NWR).EQ.1)  FWJET(LD     ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.2)  FWJET(LD     ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.3)  FWJET(LD+1   ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.4)  FWJET(LNC(LD),KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.-1) FWJET(LD     ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.-2) FWJET(LD     ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.-3) FWJET(LD+1   ,KD)=QUMF  
          IF(NQWRMFD(NWR).EQ.-4) FWJET(LNC(LD),KD)=QUMF  
        ENDIF  
      ENDDO  
C  
C **  CALCULATE QUASI-NONHYDROSTATIC PRESSURE  
C  
!$OMP PARALLEL DO PRIVATE(LN,TMPVAL)
      DO L=LMPI2,LMPILA
        LN=LNC(L)  
        TMPVAL=0.5*DZC(KC)/DXYP(L)  
        PNHYDS(L,KC)= 0.75*TMPVAL*( 
     &           DELTI*DXYP(L)*(HP(L)*WZ(L,KC)-H1P(L)*WZ1(L,KC))
     &          +FUHU(L+1,KC)-FUHU(L,KC)+FVHU(LN,KC)-FVHU(L,KC) )
     &               +0.25*TMPVAL*( 
     &           DELTI*DXYP(L)*(HP(L)*WZ(L,KS)-H1P(L)*WZ1(L,KS))
     &          +FUHU(L+1,KS)-FUHU(L,KS)+FVHU(LN,KS)-FVHU(L,KS) )
     &          -FWQQ(L,KC)
      ENDDO  
      DO K=KS,1,-1  
!$OMP PARALLEL DO PRIVATE(LN,TMPVAL)
        DO L=LMPI2,LMPILA
          LN=LNC(L)  
          TMPVAL=0.5*(DZC(K+1)+DZC(K))/DXYP(L)  
          PNHYDS(L,K)=PNHYDS(L,K+1)+FWQQ(L,K+1)-FWQQ(L,K)-FWJET(L,K)  
     &        +TMPVAL*( DELTI*DXYP(L)*(HP(L)*WZ(L,K)-H1P(L)*WZ1(L,K))  
     &        +FUHU(L+1,K)-FUHU(L,K)+FVHU(LN,K)-FVHU(L,K) )  
        ENDDO  
      ENDDO  
      DO K=0,KC  
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          WZ1(L,K)=WZ(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
!$OMP PARALLEL DO
        DO L=LMPI1,LMPILC
          PNHYDS(L,K)=0.5*(PNHYDSS(L,K)+PNHYDS(L,K))  
        ENDDO  
      ENDDO  
      IF(N.EQ.2.AND.DEBUG)THEN  
!####!!!    COLLECT_ZERO_ARRAY(PHNYDS)
        IF(MYRANK.EQ.0)THEN
           OPEN(1,FILE='PNHYDS.DIA')  
           DO L=2,LA  
             WRITE(1,888)IL(L),JL(L),(PNHYDS(L,K),K=1,KC)  
           ENDDO  
           CLOSE(1)  
        ENDIF
      ENDIF  
  888 FORMAT(2I5,10E14.5)  
      RETURN  
      END  

