      SUBROUTINE CALPUV9 (ISTL_)  
C  
C CHANGE RECORD  
C ** SUBROUTINE CALPUV9 CALCULATES THE EXTERNAL SOLUTION FOR P, UHDYE,  
C ** AND VHDXE, FOR FREE SURFACE FLOWS WITH PROVISIONS FOR WETTING  
C ** AND DRYING OF CELLS  
C  
      USE GLOBAL  

      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::QSUMTMP  

      IF(.NOT.ALLOCATED(QSUMTMP))THEN
        ALLOCATE(QSUMTMP(LCM))  
        QSUMTMP=0.
      ENDIF
C  
      IF(DEBUG)THEN
        IF(N.EQ.1.AND.ISDSOLV.EQ.1)THEN  
          OPEN(1,FILE='FUV1.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='EQCOEF1.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='EQTERM1.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='FP1.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
        ENDIF  
        IF(N.EQ.2.AND.ISDSOLV.EQ.1)THEN  
          OPEN(1,FILE='FUV2.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='EQCOEF2.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='EQTERM2.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='FP2.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
        ENDIF  
        IF(ISDSOLV.EQ.1)THEN  
          OPEN(1,FILE='FUV.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='EQCOEF.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='EQTERM.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='FP.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
        ENDIF
      ENDIF
C  
      DELT=DT2  
      DELTD2=DT  
      IF(ISTL_.EQ.2)THEN  
        DELT=DT  
        DELTD2=0.5*DT  
      ENDIF  
      DELTI=1./DELT  
C  
C **  CALCULATE EXTERNAL BUOYANCY INTEGRALS AT TIME LEVEL (N)  
C  
      IF(BSC.GT.1.E-6)THEN
        ! *** CALCULATE EXTERNAL BUOYANCY INTEGRALS AT TIME LEVEL (N)  
        CALL CALEBI  
C
        ! *** CALCULATE EXPLICIT EXTERNAL PRESSURE GRADIENTS  
        DO L=2,LA  
          !SBX(L)=0.5*SUB(L)*DYU(L)  
          FPGXE(L)=ROLD*FPGXE(L)+RNEW*(  
     &        -SBX(L)*HU(L)*GP*((BI2(L)+BI2(L-1))*(HP(L)-HP(L-1))  
     &        +2.0*HU(L)*(BI1(L)-BI1(L-1))    
     &        +(BE(L)+BE(L-1))*(BELV(L)-BELV(L-1))))  
        ENDDO  
        DO L=2,LA  
          LS=LSC(L)  
          !SBY(L)=0.5*SVB(L)*DXV(L)  
          FPGYE(L)=ROLD*FPGYE(L)+RNEW*(  
     &         -SBY(L)*HV(L)*GP*((BI2(L)+BI2(LS))*(HP(L)-HP(LS))  
     &        +2.0*HV(L)*(BI1(L)-BI1(LS)) 
     &        +(BE(L)+BE(LS))*(BELV(L)-BELV(LS))))  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE EXPLICIT EXTERNAL UHDYE AND VHDXE EQUATION TERMS  
C **  HRU=SUB*HMU*DYU/DXU & HRV=SVB*HMV*DXV/DYV  
C  
      IF(ISTL_.EQ.2)THEN  
        DO L=2,LA  
          HUTMP(L)=0.5*(HU(L)+H1U(L))  
          HVTMP(L)=0.5*(HV(L)+H1V(L))  
        ENDDO  
      ELSE  
        DO L=2,LA  
          HUTMP(L)=HU(L)  
          HVTMP(L)=HV(L)  
        ENDDO  
      ENDIF
C  
      DO L=2,LA  
        LS=LSC(L)  
        LN=LNC(L)  
        !DXYU(L)=DXU(L)*DYU(L)  
        !DXIU(L)=1./DXU(L)  
        !HRUO(L)=SUBO(L)*DYU(L)*DXIU(L)  
        FUHDYE(L)=UHDY1E(L)  
     &      -DELTD2*SUB(L)*HRUO(L)*HUTMP(L)*(P1(L)-P1(L-1))  
     &      +SUB(L)*DELT*DXIU(L)*(DXYU(L)*(TSX1(L)-RITB1*TBX1(L))  
     &      +FCAXE(L)+FPGXE(L)-SNLT*FXE(L))  
C
        !DXYV(L)=DXV(L)*DYV(L)  
        !DYIV(L)=1./DYV(L)  
        !HRVO(L)=SVBO(L)*DXV(L)*DYIV(L)
        FVHDXE(L)=VHDX1E(L)  
     &      -DELTD2*SVB(L)*HRVO(L)*HVTMP(L)*(P1(L)-P1(LS ))  
     &      +SVB(L)*DELT*DYIV(L)*(DXYV(L)*(TSY1(L)-RITB1*TBY1(L))  
     &      -FCAYE(L)+FPGYE(L)-SNLT*FYE(L))
      ENDDO  
C
      IF(ISDSOLV.GE.1.AND.DEBUG)THEN  
        OPEN(1,FILE='FUV.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        WRITE(1,1001)N,ISTL_  
        DO L=2,LA  
          WRITE(1,1001)IL(L),JL(L),UHDY1E(L),HRUO(L),HUTMP(L),P1(L),  
     &        P1(L-1),TSX1(L),TBX1(L),FCAXE(L),FPGXE(L),FXE(L)  
        ENDDO  
        CLOSE(1)  
        IF(N.EQ.1)THEN  
          OPEN(1,FILE='FUV1.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL_  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),UHDY1E(L),HRUO(L),HUTMP(L),P1(L),  
     &          P1(L-1),TSX1(L),TBX1(L),FCAXE(L),FPGXE(L),FXE(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
        IF(N.EQ.2)THEN  
          OPEN(1,FILE='FUV2.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL_  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),UHDY1E(L),HRUO(L),HUTMP(L),P1(L),  
     &          P1(L-1),TSX1(L),TBX1(L),FCAXE(L),FPGXE(L),FXE(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
      ENDIF  
C  
C **  SET IMPLICIT BOTTOM AND VEGETATION DRAG AS APPROPRIATE  
C  
      DO L=2,LA  
        RCX(L)=1.  
        RCY(L)=1.  
      ENDDO  
      RCX(1)=0.  
      RCY(1)=0.  
      RCX(LC)=0.  
      RCY(LC)=0.  
C  
C * SINGLE LAYER NO VEGETATION  
C  
      IF(KC.EQ.1.AND.ISVEG.EQ.0.AND.RITB.GT.0.)THEN  
        DO L=2,LA  
          RCX(L)=1./( 1.  
     &        +RITB*DELT*HUI(L)*STBX(L)*SQRT(VU(L)*VU(L)+U(L,1)*U(L,1)))  
          RCY(L)=1./( 1.  
     &        +RITB*DELT*HVI(L)*STBY(L)*SQRT(UV(L)*UV(L)+V(L,1)*V(L,1)))  
          FUHDYE(L)=FUHDYE(L)*RCX(L)  
          FVHDXE(L)=FVHDXE(L)*RCY(L)  
        ENDDO  
      ENDIF  
C  
C * SINGLE LAYER WITH VEGETATION  
C  
      IF(KC.EQ.1.AND.ISVEG.GE.1)THEN  
        DO L=2,LA  
          RCX(L)=1./( 1.  
     &        +RITB*DELT*HUI(L)*STBX(L)*SQRT(VU(L)*VU(L)+U(L,1)*U(L,1))  
     &        +DELT*FXVEGE(L) )  
          RCY(L)=1./( 1.  
     &        +RITB*DELT*HVI(L)*STBY(L)*SQRT(UV(L)*UV(L)+V(L,1)*V(L,1))  
     &        +DELT*FYVEGE(L) )  
          FUHDYE(L)=FUHDYE(L)*RCX(L)  
          FVHDXE(L)=FVHDXE(L)*RCY(L)  
        ENDDO  
      ENDIF  
C  
C * MULTIPLE LAYERS WITH VEGETATION  
C  
      IF(KC.GT.1.AND.ISVEG.GE.1)THEN  
        DO L=2,LA  
          RCX(L)=1./( 1.+DELT*FXVEGE(L) )  
          RCY(L)=1./( 1.+DELT*FYVEGE(L) )  
          FUHDYE(L)=FUHDYE(L)*RCX(L)  
          FVHDXE(L)=FVHDXE(L)*RCY(L)  
        ENDDO  
      ENDIF  
C
C **  ADJUST VOLUME SOURCE AND SINKS  
C  
      IF(ISGWIE.EQ.0)THEN  
        DO L=2,LA  
          IF(QSUME(L).LE.0.)THEN  
            IF(H1P(L).LE.HDRY)THEN  
              QSUMTMP(L)=0.  
            ELSE  
              QSUMTMP(L)=-(H1P(L)-HDRY)*DXYP(L)*DELTI  
              QSUMTMP(L)=MAX(QSUMTMP(L),QSUME(L))  
            ENDIF  
          ELSE  
            QSUMTMP(L)=QSUME(L)  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          DIFQVOL=QSUME(L)-QSUMTMP(L)  
          DO K=1,KC  
            QSUM(L,K)=QSUM(L,K)-DIFQVOL*DZC(K)  
          ENDDO  
          QSUME(L)=QSUMTMP(L)  
        ENDDO  
      ENDIF  
C  
C **  ADJUST SOURCES AND SINKS ESTIMATING SURFACE AND GROUNDWATER  
C **  AVAILABLE FOR EVAPOTRANSPIRATON AND INFILTRATION  
C  
      IF(ISGWIE.GE.1)THEN  
        DO L=2,LA  
          RIFTR(L)=0.  
          EVAPSW(L)=0.  
          EVAPGW(L)=0.  
          IF(H1P(L).GT.HDRY)THEN  
C  
C       APPLY MAXIMUM ET  
C  
            IF(EVAPCVT.LT.0.)THEN  
              SVPW=(10.**((0.7859+0.03477*TEM(L,KC))/  
     &            (1.+0.00412*TEM(L,KC))))  
              EVAPT(L)=CLEVAP(L)*0.7464E-3*WINDST(L)*(SVPW-VPA(L))
     &            /PATMT(L)  
            ENDIF  
            EVAPSW(L)=EVAPT(L)*DXYP(L)  
            RIFTR(L)=0.  
C  
C       CALCULATE DEPTH OF ACTIVE GROUNDWATER ELEV BELOW SURFACE  
C  
            DTAGW=BELV(L)-AGWELV(L)  
            IF(DTAGW.GT.0.0)THEN  
C  
C         INFLITRATION CAN OCCUR, CALCULATE LIMITING RATE TO BRING  
C         GW ELEV TO SOIL SURFACE  
C  
              RIFTRL=RNPOR*DTAGW*DELTI  
C  
C         SET RIFTRL TO MIN OF LIMITING RATE OR ACTUAL RATE  
C  
              RIFTRL=MIN(RIFTRM,RIFTRL)  
C  
C         ESTIMATE RATE BASED ON AVAILABLE SURFACE WATER  
C  
              RAVAIL=(H1P(L)-HDRY)*DELTI-EVAPT(L)  
C  
C         SET RIFTRL TO MIN OF AVAILABLE RATE OR LIMITING RATE  
C  
              RIFTRL=MIN(RAVAIL,RIFTRL)  
C  
C         CONVERT TO VOLUME FLOW UNITS  
C  
              RIFTR(L)=RIFTRL*DXYP(L)  
            ENDIF  
C  
C       ADJUST VOLUME OUTFLOWS OF WET CELLS  
C  
            IF(QSUME(L).LT.0.0)THEN  
              QSUMIET=RIFTR(L)+EVAPSW(L)  
              QEAVAIL=DXYP(L)*(H1P(L)-HDRY)*DELTI-QSUMIET  
              QEAVAIL=MAX(QEAVAIL,0.0)  
              QEAVAIL=-QEAVAIL  
              QSUMTMP(L)=MAX(QSUME(L),QEAVAIL)  
            ELSE  
              QSUMTMP(L)=QSUME(L)  
            ENDIF  
          ELSE  
            RIFTR(L)=0.  
            EVAPSW(L)=0.  
            QSUMTMP(L)=MAX(QSUME(L),0.0)  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          DIFQVOL=QSUME(L)-QSUMTMP(L)  
          DO K=1,KC  
            QSUM(L,K)=QSUM(L,K)-DIFQVOL*DZC(K)  
          ENDDO  
          QSUME(L)=QSUMTMP(L)  
        ENDDO  
      ENDIF  
C  
C **  SET OLD TIME LEVEL TERMS IN CONTINUITY EQUATION FOR  
C **  NON BOUNDARY POINTS  
C **  HRU=HMU*DYU/DXU & HRV=HMV*DXV/DYV  
C **  DXYIP=1/(DXP*DYP)  
C  
      DO L=2,LA  
        LN=LNC(L)  
        FP(L)=DELTI*DXYP(L)*P1(L)-0.5*G*(UHDY1E(L+1)-UHDY1E(L)  
     &                                  +VHDX1E(LN )-VHDX1E(L))  
      ENDDO  
C  
C **  ADVANCE EXTERNAL VARIABLES FOR THREE TIME LEVEL STEP  
C  
      IF(ISTL_.EQ.3)THEN  
        DO L=2,LA  
          UHDY2E(L)=UHDY1E(L)  
          VHDX2E(L)=VHDX1E(L)  
          UHDY1E(L)=UHDYE(L)  
          VHDX1E(L)=VHDXE(L)  
          U1V(L)=UV(L)  
          V1U(L)=VU(L)  
          P1(L)=P(L)  
          H1U(L)=HU(L)  
          H1V(L)=HV(L)  
          H1UI(L)=HUI(L)  
          H1VI(L)=HVI(L)  
          H2P(L)=H1P(L)  
          H1P(L)=HP(L)  
        ENDDO
        IF(ISGWIE.GE.1)THEN
	      DO L=2,LA  
	        AGWELV2(L)=AGWELV1(L)  
	        AGWELV1(L)=AGWELV(L)  
	      ENDDO  
        ENDIF  
      ENDIF
C  
C **  SET NEW TIME LEVEL TERMS IN CONTINUITY EQUATION INCLUDING  
C **  HOST-GUEST CHANNAL INTERACTION FOR NON BOUNDARY POINTS  
C  
      C1=0.5*G
      DO L=2,LA  
        LN=LNC(L)  
        ! ***  THE SUB & SVB SWITCHES ALREADY ACCOUNTED FOR
        FP(L)=FP(L)-C1*(FUHDYE(L+1)-FUHDYE(L)  
     &                 +FVHDXE(LN )-FVHDXE(L)  
     &                -2.0*QSUME(L) )  
      ENDDO  
C
      IF(ISGWIE.GE.1)THEN  
        DO L=2,LA  
          FP(L)=FP(L)-G*SPB(L)*(RIFTR(L)+EVAPSW(L))  
        ENDDO  
      ENDIF  
C
      IF(ISDSOLV.GE.1.AND.DEBUG)THEN  
        OPEN(1,FILE='FP.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        WRITE(1,1001)N,ISTL_  
        DO L=2,LA  
          WRITE(1,1001)IL(L),JL(L),FP1(L),FUHDYE(L),FUHDYE(L+1),  
     &        FVHDXE(L),FVHDXE(LNC(L)),QSUME(L),RIFTR(L),EVAPSW(L)  
        ENDDO  
        CLOSE(1)  
        IF(N.EQ.1)THEN  
          OPEN(1,FILE='FP1.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL_  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),FP1(L),FUHDYE(L),FUHDYE(L+1),  
     &          FVHDXE(L),FVHDXE(LNC(L)),QSUME(L),RIFTR(L),EVAPSW(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
        IF(N.EQ.2)THEN  
          OPEN(1,FILE='FP2.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL_  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),FP1(L),FUHDYE(L),FUHDYE(L+1),  
     &          FVHDXE(L),FVHDXE(LNC(L)),QSUME(L),RIFTR(L),EVAPSW(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
      ENDIF  
C
      C1=-0.5*DELTD2*G  
      DO L=2,LA  
        CS(L)=C1*SVB(L  )*HRVO(L  )*RCY(L  )*HVTMP(L  )  
        CW(L)=C1*SUB(L  )*HRUO(L  )*RCX(L  )*HUTMP(L  )  
        CE(L)=C1*SUB(L+1)*HRUO(L+1)*RCX(L+1)*HUTMP(L+1)  
      ENDDO  
      DO L=2,LA  
        LN=LNC(L)  
        CN(L)=C1*SVB(LN )*HRVO(LN )*RCY(LN )*HVTMP(LN )  
      ENDDO  
C
C *** APPLY THE OPEN BOUNDARY CONDITIONS
C
      IF(NBCSOP.GT.0) CALL SETOPENBC(DELT,DELTD2,DELTI,HUTMP,HVTMP)   
C  
      ! *** SET THE CENTER
      DO L=2,LA  
        CC(L)=DELTI*DXYP(L)-CS(L)-CW(L)-CE(L)-CN(L)  
      ENDDO  
C
      ! *** SCALE COEFFICIENTS IN EXTERNAL MODEL LINEAR EQUATION SYSTEM  
      CCMNM=1.E+18
      DO L=2,LA  
        CCMNM=MIN(CCMNM,CC(L))  
        FPTMP(L)=FP(L)  
      ENDDO  
      CCMNMI=1./CCMNM  
C
C *** APPLY THE OPEN BOUNDARY CONDITIONS FOR ADJACENT CELLS
C
      IF(NBCSOP.GT.0) CALL SETOPENBC2   
C
      CC(1)=1.  
      CC(LC)=1.  
C  
C **  SCALE BY MINIMUM DIAGONAL  
C  
      IF(IRVEC.EQ.9)THEN  
        DO L=2,LA  
          CCS(L)=CS(L)*CCMNMI  
          CCW(L)=CW(L)*CCMNMI  
          CCE(L)=CE(L)*CCMNMI  
          CCN(L)=CN(L)*CCMNMI  
          CCC(L)=CC(L)*CCMNMI  
          FPTMP(L)=FPTMP(L)*CCMNMI  
          CCCI(L)=1./CCC(L)  
        ENDDO  
      ENDIF  
C  
C **  SCALE TO NORMAL FORM  
C  
      IF(IRVEC.EQ.99)THEN  
        DO L=2,LA  
          CCS(L)=CS(L)/SQRT( CC(L)*CC(LSC(L)) )  
          CCW(L)=CW(L)/SQRT( CC(L)*CC(L-1   ) )  
          CCE(L)=CE(L)/SQRT( CC(L)*CC(L+1   ) )  
          CCN(L)=CN(L)/SQRT( CC(L)*CC(LNC(L)) )  
          CCC(L)=1.  
          FPTMP(L)=FPTMP(L)/SQRT( CC(L) )  
          P(L)=P(L)*SQRT( CC(L) )  
          CCCI(L)=1.  
        ENDDO  
      ENDIF  
C
      ! *** CALL THE PRECONDITIONED CONJUGATE GRADIENT SOLVER 
      CALL CONGRAD (ISTL_)  
C
      IF(IRVEC.EQ.99)THEN  
        DO L=2,LA  
          P(L)=P(L)/SQRT( CC(L) )  
        ENDDO  
      ENDIF  
C  
C ** DIAGNOSTICS  
C  
      IF(ISDSOLV.GE.1.AND.DEBUG)THEN  
        OPEN(1,FILE='EQCOEF.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        WRITE(1,1001)N,ISTL_  
        DO L=2,LA  
          SURFTMP=GI*P(L)  
          WRITE(1,1001)IL(L),JL(L),CS(L),CW(L),CC(L),CE(L),CN(L),  
     &        FP(L),SURFTMP  
        ENDDO  
        CLOSE(1)  
        IF(N.EQ.1)THEN  
          OPEN(1,FILE='EQCOEF1.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL_  
          DO L=2,LA  
            SURFTMP=GI*P(L)  
            WRITE(1,1001)IL(L),JL(L),CS(L),CW(L),CC(L),CE(L),CN(L),  
     &          FP(L),SURFTMP  
          ENDDO  
          CLOSE(1)  
        ENDIF  
        IF(N.EQ.2)THEN  
          OPEN(1,FILE='EQCOEF2.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL_  
          DO L=2,LA  
            SURFTMP=GI*P(L)  
            WRITE(1,1001)IL(L),JL(L),CS(L),CW(L),CC(L),CE(L),CN(L),  
     &          FP(L),SURFTMP  
          ENDDO  
          CLOSE(1)  
        ENDIF  
      ENDIF  
      IF(ISDSOLV.GE.1.AND.DEBUG)THEN  
        OPEN(1,FILE='EQTERM.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        WRITE(1,1001)N,ISTL_  
        DO L=2,LA  
          WRITE(1,1001)IL(L),JL(L),SUB(L),SVB(L),HRUO(L),  
     &        HRVO(L),HUTMP(L),HVTMP(L)  
        ENDDO  
        CLOSE(1)  
        IF(N.EQ.1)THEN  
          OPEN(1,FILE='EQTERM1.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL_  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),SUB(L),SVB(L),HRUO(L),  
     &          HRVO(L),HUTMP(L),HVTMP(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
        IF(N.EQ.2)THEN  
          OPEN(1,FILE='EQTERM2.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL_  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),SUB(L),SVB(L),HRUO(L),  
     &          HRVO(L),HUTMP(L),HVTMP(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
      ENDIF  
 1001 FORMAT(2I5,10(1X,E12.4))  
 1002 FORMAT(3I4,10(1X,E9.2))  
C  
C **  CALCULATE UHEX AND VHEX AND TOTAL DEPTHS AT TIME LEVEL (N+1)  
C **  HRU=SUB*DYU/DXU & HRV=SVB*DXV/DYV  
C  
      DO L=2,LA  
        LS=LSC(L)  
        UHDYE(L)=SUB(L)*( FUHDYE(L)  
     &      -DELTD2*HRUO(L)*RCX(L)*HUTMP(L)*(P(L)-P(L-1)) )  
        VHDXE(L)=SVB(L)*( FVHDXE(L)  
     &      -DELTD2*HRVO(L)*RCY(L)*HVTMP(L)*(P(L)-P(LS )) ) 
      ENDDO
C
      DO L=2,LA  
        UHE(L)=UHDYE(L)*DYIU(L)  
        VHE(L)=VHDXE(L)*DXIV(L)  
      ENDDO  
C  
C **  CALCULATE REVISED CELL DEPTHS BASED ON NEW HORIZONTAL  
C **  TRANSPORTS AT (N+1)  
C  
      IF(ISTL_.EQ.3)THEN  
        DO L=2,LA  
          LN=LNC(L)  
          HP(L)=H2P(L)+DELT*DXYIP(L)*(QSUME(L)  
     &        -0.5*(UHDYE(L+1)+UHDY2E(L+1)-UHDYE(L)-UHDY2E(L)  
     &             +VHDXE(LN )+VHDX2E(LN )-VHDXE(L)-VHDX2E(L)))
        ENDDO  
      ELSE  
        DO L=2,LA  
          LN=LNC(L)  
          HP(L)=H1P(L)+DELT*DXYIP(L)*(QSUME(L)  
     &        -0.5*(UHDYE(L+1)+UHDY1E(L+1)-UHDYE(L)-UHDY1E(L)  
     &             +VHDXE(LN )+VHDX1E(LN )-VHDXE(L)-VHDX1E(L)))  
        ENDDO
      ENDIF  
C
      IF(ISGWIE.GE.1)THEN  
        DO L=2,LA  
          HP(L)=HP(L)-DELT*DXYIP(L)*(RIFTR(L)+EVAPSW(L))  
        ENDDO  
      ENDIF
C
      ! *** APPLY OPEN BOUNDARYS 
      DO LL=1,NBCSOP
        L=LOBCS(LL)
        HP(L)=GI*P(L)-BELV(L)  
      ENDDO 
C  
C **  PERFORM FINAL UPDATES OF P,HU, AND HV  
C  
      DO L=2,LA  
        P(L)=G*(HP(L)+BELV(L))  
      ENDDO  
      DO L=2,LA  
        LS=LSC(L)  
        HU(L)=0.5*(DXYP(L)*HP(L)+DXYP(L-1)*HP(L-1))*DXYIU(L)  
        HV(L)=0.5*(DXYP(L)*HP(L)+DXYP(LS )*HP(LS ))*DXYIV(L)  
      ENDDO  
      DO L=2,LA  
        HPI(L)=1./HP(L)  
        HUI(L)=1./HU(L)  
        HVI(L)=1./HV(L)  
      ENDDO  
C  
C **  PERFORM UPDATE ON GROUNDWATER ELEVATION  
C  
      IF(ISGWIE.GE.1)THEN  
        DO L=2,LA  
          QSUM(L,KC)=QSUM(L,KC)-EVAPSW(L)  
          QSUM(L,1 )=QSUM(L,1 )-RIFTR(L)  
        ENDDO  
C  
C       INFILTRATION STEP  
C  
        RNPORI=1./RNPOR  
        IF(ISTL_.EQ.3)THEN  
          DO L=2,LA  
            AGWELV(L)=AGWELV2(L)+RNPORI*DELT*DXYIP(L)*RIFTR(L)  
          ENDDO  
        ELSE  
          DO L=2,LA  
            AGWELV(L)=AGWELV1(L)+RNPORI*DELT*DXYIP(L)*RIFTR(L)  
          ENDDO  
        ENDIF  
        DO L=2,LA  
          AGWELV(L)=MIN(AGWELV(L),BELV(L))  
        ENDDO  
C  
C       ET STEP  
C  
        DO L=2,LA  
          IF(EVAPCVT.LT.0.)THEN  
            SVPW=(10.**((0.7859+0.03477*TEM(L,KC))/  
     &          (1.+0.00412*TEM(L,KC))))  
           EVAPT(L)=CLEVAP(L)*0.7464E-3*WINDST(L)*(SVPW-VPA(L))/PATMT(L)  
          ENDIF  
          ETGWTMP=EVAPT(L)-EVAPSW(L)*DXYIP(L)  
          ETGWTMP=MAX(ETGWTMP,0.0)  
          ETGWAVL=RNPOR*DELTI*(AGWELV(L)-BELAGW(L))  
          ETGWAVL=MAX(ETGWAVL,0.0)  
          ETGWTMP=MIN(ETGWTMP,ETGWAVL)  
          EVAPGW(L)=ETGWTMP*DXYP(L)  
        ENDDO  
        DO L=2,LA  
          AGWELV(L)=AGWELV(L)-RNPORI*DELT*DXYIP(L)*EVAPGW(L)  
        ENDDO  
        DO L=2,LA  
          AGWELV(L)=MAX(AGWELV(L),BELAGW(L))  
        ENDDO  
      ENDIF  
C  
C **  CHECK FOR NEGATIVE DEPTHS  
C  
      IF(ISNEGH.GE.1)THEN  
        INEGFLG=0  
        DO L=2,LA  
          IF(HP(L).LT.0.)THEN  
            INEGFLG=1  
            LN=LNC(L)  
            WRITE (6,6060)IL(L),JL(L),HP(L),H1P(L),H2P(L)  
            WRITE (6,6061)IL(L),JL(L),HU(L),H1U(L)  
            WRITE (6,6062)IL(L),JL(L),HU(L+1),H1U(L+1)  
            WRITE (6,6063)IL(L),JL(L),HV(L),H1V(L)  
            WRITE (6,6064)IL(L),JL(L),HV(LN),H1V(LN)  
            WRITE (8,6060)IL(L),JL(L),HP(L),H1P(L),H2P(L)  
            WRITE (8,6061)IL(L),JL(L),HU(L),H1U(L)  
            WRITE (8,6062)IL(L),JL(L),HU(L+1),H1U(L+1)  
            WRITE (8,6063)IL(L),JL(L),HV(L),H1V(L)  
            WRITE (8,6064)IL(L),JL(L),HV(LN),H1V(LN)  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          IF(HU(L).LT.0.)THEN  
            INEGFLG=1  
            LN=LNC(L)  
            WRITE (6,6060)IL(L),JL(L),HP(L),H1P(L),H2P(L)  
            WRITE (6,6061)IL(L),JL(L),HU(L),H1U(L)  
            WRITE (6,6062)IL(L),JL(L),HU(L+1),H1U(L+1)  
            WRITE (6,6063)IL(L),JL(L),HV(L),H1V(L)  
            WRITE (6,6064)IL(L),JL(L),HV(LN),H1V(LN)  
            WRITE (8,6060)IL(L),JL(L),HP(L),H1P(L),H2P(L)  
            WRITE (8,6061)IL(L),JL(L),HU(L),H1U(L)  
            WRITE (8,6062)IL(L),JL(L),HU(L+1),H1U(L+1)  
            WRITE (8,6063)IL(L),JL(L),HV(L),H1V(L)  
            WRITE (8,6064)IL(L),JL(L),HV(LN),H1V(LN)  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          IF(HV(L).LT.0.)THEN  
            INEGFLG=1  
            LN=LNC(L)  
            WRITE (6,6060)IL(L),JL(L),HP(L),H1P(L),H2P(L)  
            WRITE (6,6061)IL(L),JL(L),HU(L),H1U(L)  
            WRITE (6,6062)IL(L),JL(L),HU(L+1),H1U(L+1)  
            WRITE (6,6063)IL(L),JL(L),HV(L),H1V(L)  
            WRITE (6,6064)IL(L),JL(L),HV(LN),H1V(LN)  
            WRITE (8,6060)IL(L),JL(L),HP(L),H1P(L),H2P(L)  
            WRITE (8,6061)IL(L),JL(L),HU(L),H1U(L)  
            WRITE (8,6062)IL(L),JL(L),HU(L+1),H1U(L+1)  
            WRITE (8,6063)IL(L),JL(L),HV(L),H1V(L)  
            WRITE (8,6064)IL(L),JL(L),HV(LN),H1V(LN)  
          ENDIF  
        ENDDO  
      ENDIF  
      IF(ISNEGH.EQ.2)THEN  
        IF(INEGFLG.EQ.1)THEN  
          CALL RESTOUT(1)  
          OPEN(1,FILE='EQCOEF.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='EQCOEF.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL_  
          DO L=2,LA  
            SURFTMP=GI*P(L)  
            WRITE(1,1001)IL(L),JL(L),CCS(L),CCW(L),CCC(L),CCE(L),CCN(L),  
     &          FPTMP(L),SURFTMP  
          ENDDO  
          CLOSE(1)  
          OPEN(1,FILE='EQTERM.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='EQTERM.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL_  
          DO L=2,LA  
            WRITE(1,1002)IL(L),JL(L),SUB(L),SVB(L),HRUO(L),  
     &          HRVO(L),HUTMP(L),HVTMP(L)  
          ENDDO  
          CLOSE(1)  
          OPEN(1,FILE='CFLMAX.OUT')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='CFLMAX.OUT')  
          DO L=2,LA  
            WRITE(1,1991)IL(L),JL(L),(CFLUUU(L,K),K=1,KC)  
            WRITE(1,1992)(CFLVVV(L,K),K=1,KC)  
            WRITE(1,1992)(CFLWWW(L,K),K=1,KC)  
            WRITE(1,1992)(CFLCAC(L,K),K=1,KC)  
          ENDDO  
          CLOSE(1)  
          STOP  
        ENDIF  
      ENDIF  
 1991 FORMAT(2I5,12F7.2)  
 1992 FORMAT(10X,12F7.2)  
 6060 FORMAT('  NEG DEPTH AT I,J =',2I4,'  HP,H1P,H2P =',3(2X,E12.4))  
 6061 FORMAT('  NEG DEPTH AT I,J =',2I4,'  HUW,H1UW =',2(2X,E12.4))  
 6062 FORMAT('  NEG DEPTH AT I,J =',2I4,'  HUE,H1UE =',2(2X,E12.4))  
 6063 FORMAT('  NEG DEPTH AT I,J =',2I4,'  HVS,H1VS =',2(2X,E12.4))  
 6064 FORMAT('  NEG DEPTH AT I,J =',2I4,'  HVN,H1VN =',2(2X,E12.4))  
 6910 FORMAT('  DRYING AT N,I,J =',I10,2I6,'  HP,H1P,H2P ='  
     &    ,3(2X,E12.4))  
 6911 FORMAT('  DRY W FACE N,I,J =',I10,2I6,' HU,H,H1 =',3(2X,E12.4))  
 6912 FORMAT('  DRY E FACE N,I,J =',I10,2I6,' HU,H,H1 =',3(2X,E12.4))  
 6913 FORMAT('  DRY S FACE N,I,J =',I10,2I6,' HV,H,H1 =',3(2X,E12.4))  
 6914 FORMAT('  DRY N FACE N,I,J =',I10,2I6,' HV,H,H1 =',3(2X,E12.4))  
 6920 FORMAT('  WETTING AT N,I,J =',I10,2I6,' HP,H1P,H2P ='  
     &    ,3(2X,E12.4))  
 6921 FORMAT('  WET S FACE N,I,J =',I10,2I6,' HV,H,H1 =',3(2X,E12.4))  
 6922 FORMAT('  WET W FACE N,I,J =',I10,2I6,' HU,H,H1 =',3(2X,E12.4))  
 6923 FORMAT('  WET E FACE N,I,J =',I10,2I6,' HU,H,H1 =',3(2X,E12.4))  
 6924 FORMAT('  WET N FACE N,I,J =',I10,2I6,' HV,H,H1 =',3(2X,E12.4))  
 6930 FORMAT('  WET BY VOL  N,I,J =',I10,2I6,' HP,H1P,H2P ='  
     &    ,3(2X,E12.4))  
 6940 FORMAT('  RESOLVE,  N,I,J =',I10,2I6,' HP,H1P,H2P ='  
     &    ,3(2X,E12.4))  
 6941 FORMAT('  RESOLVE,  N,I,J =',I10,2I6,' HUE,HP,H1P ='  
     &    ,3(2X,E12.4))  
 6942 FORMAT('  RESOLVE,  N,I,J =',I10,2I6,' HUW,HP,H1P ='  
     &    ,3(2X,E12.4))  
 6943 FORMAT('  RESOLVE,  N,I,J =',I10,2I6,' HVS,HP,H1P ='  
     &    ,3(2X,E12.4))  
 6944 FORMAT('  RESOLVE,  N,I,J =',I10,2I6,' HVN,HP,H1P ='  
     &    ,3(2X,E12.4))  
 6945 FORMAT('  RESOLVE NEG,  N,I,J =',I10,2I6,' HP,H1P,H2P ='  
     &    ,3(2X,E12.4))  
 6950 FORMAT('  RESOLVE, NEG DEP N,I,J =',I10,2I6,' HP,H1P,H2P ='  
     &    ,3(2X,E12.4))  
C     
C **  CALCULATE THE EXTERNAL DIVERGENCE  
C  
      IF(ISDIVEX.EQ.1)THEN  
        DIVEXMX=0.  
        DIVEXMN=1000000.  
        IF(ISTL_.EQ.3)THEN  
          DO L=2,LA  
            IF(SPB(L).NE.0)THEN  
              LN=LNC(L)  
              DIVEX=SPB(L)*(DXYP(L)*(HP(L)-H2P(L))*DELTI  
     &            +0.5*(UHDYE(L+1)+UHDY2E(L+1)-UHDYE(L)-UHDY2E(L)  
     &            +VHDXE(LN)+VHDX2E(LN)-VHDXE(L)-VHDX2E(L))-QSUME(L)  
     &            +RIFTR(L)+EVAPSW(L))  
              IF(DIVEX.GT.DIVEXMX)THEN  
                DIVEXMX=DIVEX  
                LMAX=L  
              ENDIF  
              IF(DIVEX.LT.DIVEXMN)THEN  
                DIVEXMN=DIVEX  
                LMIN=L  
              ENDIF  
            ENDIF  
          ENDDO  
        ELSE  
          DO L=2,LA  
            IF(SPB(L).NE.0)THEN  
              LN=LNC(L)  
              DIVEX=SPB(L)*(DXYP(L)*(HP(L)-H1P(L))*DELTI  
     &            +0.5*(UHDYE(L+1)+UHDY1E(L+1)-UHDYE(L)-UHDY1E(L)  
     &            +VHDXE(LN)+VHDX1E(LN)-VHDXE(L)-VHDX1E(L))-QSUME(L)  
     &            +RIFTR(L)+EVAPSW(L))  
              IF(DIVEX.GT.DIVEXMX)THEN  
                DIVEXMX=DIVEX  
                LMAX=L  
              ENDIF  
              IF(DIVEX.LT.DIVEXMN)THEN  
                DIVEXMN=DIVEX  
                LMIN=L  
              ENDIF  
            ENDIF  
          ENDDO  
        ENDIF  
        IMAX=IL(LMAX)  
        JMAX=JL(LMAX)  
        IMIN=IL(LMIN)  
        JMIN=JL(LMIN)  
        WRITE(6,6628)DIVEXMX,IMAX,JMAX  
        WRITE(6,6629)DIVEXMN,IMIN,JMIN  
      ENDIF  
  566 FORMAT('  I=',I5,3X,'J=',I5,3X,'HP=',F12.4)  
 6628 FORMAT('  DIVEXMX=',E13.5,5X,2I10)  
 6629 FORMAT('  DIVEXMN=',E13.5,5X,2I10)  
C  
C **  UPDATE ZERO DIMENSION VOLUME BALANCE  
C  
      IF(ISDRY.GE.1.AND.ISTL_.EQ.3)THEN  
        VOLADD=0.  
        DO L=2,LA  
          IF(SPB(L).NE.0)THEN  
            VOLADD=VOLADD+QSUME(L)-RIFTR(L)-EVAPSW(L)  
          ENDIF  
        ENDDO  
        VOLADD=VOLADD*DT  
        VOLZERD=VOLZERD+VOLADD  
        VETZERD=VETZERD+VOLADD+DT*EVAPSW(L)  
      ENDIF  
 5303 FORMAT(2X,F10.4,2X,F10.5,3(2X,E13.5))  
      RETURN  
      END  

