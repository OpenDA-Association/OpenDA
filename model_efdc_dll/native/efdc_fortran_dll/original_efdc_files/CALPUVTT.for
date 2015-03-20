C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C
      SUBROUTINE CALPUV2T_TT
C
C *** TT VERSION
C
C **  THIS SUBROUTINE IS PART OF  EFDC-FULL VERSION 1.0a 
C
C **  LAST MODIFIED BY JOHN HAMRICK ON 1 NOVEMBER 2001
C
C----------------------------------------------------------------------C
C
C CHANGE RECORD
C DATE MODIFIED     BY                 DATE APPROVED    BY
C 02/15/2002        John Hamrick       02/11/2002       John Hamrick
C  added alternate sor equation solver relax2t
C----------------------------------------------------------------------C  
C
C**********************************************************************C
C
C ** SUBROUTINE CALPUV2T CALCULATES THE EXTERNAL SOLUTION FOR P, UHDYE,
C ** AND VHDXE, FOR FREE SURFACE FLOWS WITH PROVISIONS FOR WETTING
C ** AND DRYING OF CELLS
C
C**********************************************************************C
C
C     FPGXE(L)  = PMCTESTX(1,L)
C     FPGYE(L)  = PMCTESTY(1,L)
C     FUHDYE(L) = PMCTESTX(2,L)
C     FVHDXE(L) = PMCTESTY(2,L)
C     FP(L)     = PMCTESTX(3,L)
C     P(L)      = PMCTESTY(3,L)
C     UHDYE(L)  = PMCTESTX(4,L)
C     VHDXE(L)  = PMCTESTY(4,L)
C     HP(L)     = PMCTESTX(5,L)
C
      USE GLOBAL  
      DIMENSION QSUMTMP(LCM)
      DIMENSION QCHANUT(NCHANM),QCHANVT(NCHANM)

      PARAMETER (LLCM=200)
      REAL, SAVE :: CCW1(LLCM),CCE1(LLCM),CCN1(LLCM),CCS1(LLCM)
      REAL, SAVE :: CCC1(LLCM)

      REAL, SAVE :: UHDY1ET(LLCM)
      REAL, SAVE :: VHDX1ET(LLCM)
      REAL, SAVE :: H1PT(LLCM)

      REAL*8   DTMP
      REAL*4   EPSILON

      LOGICAL HILOWX(LCM)
      LOGICAL HILOWY(LCM)
      REAL*4  DELTAHP
C
C**********************************************************************C
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
      ISTL=2  
C  
C**********************************************************************C  
C  
C **  CALCULATE EXTERNAL BUOYANCY INTEGRALS AT TIME LEVEL (N)  
C  
      !IF(BSC.GT.1.E-6) CALL CALEBI    ALREADY
C  
C**********************************************************************C  
C  
C **  CALCULATE EXPLICIT EXTERNAL PRESSURE GRADIENTS  
C **  SBX=SBX*0.5*DYU & SBY=SBY*0.5*DXV  
C **  SNLPX=SNLPX*GID2*DYU & SNLPY=SNLPY*GID2*DXV  
C  
C----------------------------------------------------------------------C  
C  
      IF(BSC.GT.1.E-6)THEN  
        DO L=2,LA  
          PMCTESTX(1,L)=-SBX(L)*HU(L)*((BI2(L)+BI2(L-1))*(HP(L)-HP(L-1))  
     &        +HU(L)*(BI1(L)-BI1(L-1))  
     &        +(BE(L)+BE(L-1))*(BELV(L)-BELV(L-1)))  
        ENDDO  
        DO L=2,LA  
          LS=LSC(L)  
          PMCTESTY(1,L)=-SBY(L)*HV(L)*((BI2(L)+BI2(LS))*(HP(L)-HP(LS))  
     &        +HV(L)*(BI1(L)-BI1(LS))  
     &        +(BE(L)+BE(LS))*(BELV(L)-BELV(LS)))  
        ENDDO  
      ENDIF  
C  
C**********************************************************************C  
C  
C **  CALCULATE EXPLICIT EXTERNAL UHDYE AND VHDXE EQUATION TERMS  
C **  HRU=SUB*HMU*DYU/DXU & HRV=SVB*HMV*DXV/DYV  
C  
C----------------------------------------------------------------------C  
C  
      DO L=2,LA  
        HUTMP(L)=HU(L)  
        HVTMP(L)=HV(L)  
      ENDDO  
C  
      DO L=2,LA  
        TVAR3S(L)=P(LSC(L))  
      ENDDO  
C  
      DO L=2,LA  
        PMCTESTX(2,L)=UHDYE(L)  
     &      -DELTD2*SUB(L)*HRUO(L)*HUTMP(L)*(P(L)-P(L-1))  
     &      +SUB(L)*DELT*DXIU(L)*(DXYU(L)*(TSX(L)-RITB1*TBX(L))  
     &      +FCAXE(L)+PMCTESTX(1,L)-SNLT*FXE(L))  
        PMCTESTY(2,L)=VHDXE(L)  
     &      -DELTD2*SVB(L)*HRVO(L)*HVTMP(L)*(P(L)-TVAR3S(L))  
     &      +SVB(L)*DELT*DYIV(L)*(DXYV(L)*(TSY(L)-RITB1*TBY(L))  
     &      -FCAYE(L)+PMCTESTY(1,L)-SNLT*FYE(L))  
      ENDDO  
C  
C**********************************************************************C  
C  
C **  SET IMPLICIT BOTTOM AND VEGETATION DRAG AS APPROPRIATE  
C  
C----------------------------------------------------------------------C  
C  
      DO L=2,LA  
        RCX(L)=1.  
        RCY(L)=1.  
      ENDDO  
C  
      RCX(1)=0.  
      RCY(1)=0.  
      RCX(LC)=0.  
      RCY(LC)=0.  
C  
C * SINGLE LAYER NO VEGETATION  
C  
      IF(KC.EQ.1.AND.ISVEG.EQ.0)THEN  
        DO L=2,LA  
          RCX(L)=1./( 1.  
     &        +RITB*DELT*HUI(L)*STBX(L)*SQRT(VU(L)*VU(L)+U(L,1)*U(L,1)))  
          RCY(L)=1./( 1.  
     &        +RITB*DELT*HVI(L)*STBY(L)*SQRT(UV(L)*UV(L)+V(L,1)*V(L,1)))  
          PMCTESTX(2,L)=PMCTESTX(2,L)*RCX(L)  
          PMCTESTY(2,L)=PMCTESTY(2,L)*RCY(L)  
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
          PMCTESTX(2,L)=PMCTESTX(2,L)*RCX(L)  
          PMCTESTY(2,L)=PMCTESTY(2,L)*RCY(L)  
        ENDDO  
      ENDIF  
C  
C * MULTIPLE LAYERS WITH VEGETATION  
C  
      IF(KC.GT.1.AND.ISVEG.GE.1)THEN  
        DO L=2,LA  
          RCX(L)=1./( 1.+DELT*FXVEGE(L) )  
          RCY(L)=1./( 1.+DELT*FYVEGE(L) )  
          PMCTESTX(2,L)=PMCTESTX(2,L)*RCX(L)  
          PMCTESTY(2,L)=PMCTESTY(2,L)*RCY(L)  
        ENDDO  
      ENDIF  
C  
C**********************************************************************C  
C  
C **  RESET BOUNDARY CONDITIONS SWITCHES  
C  
C----------------------------------------------------------------------C  
C  
      DO L=2,LA  
        SUB(L)=SUBO(L)  
        SVB(L)=SVBO(L)  
        SBX(L)=SBXO(L)  
        SBY(L)=SBYO(L)  
        SUB(L+1)=SUBO(L+1)  
        SBX(L+1)=SBXO(L+1)  
      ENDDO  
      DO L=2,LA  
        LN=LNC(L)  
        SVB(LN)=SVBO(LN)  
        SBY(LN)=SBYO(LN)  
      ENDDO  
C  
      DO L=1,LC  
        PMCTESTX(3,L)=0.  
        FP1(L)=0.  
      ENDDO  
C  
C**********************************************************************C  
C  
C **  SET OPEN BOUNDARY SURFACE ELEVATIONS  
C  
      IVAL=NPBW+NPBE+NPBS+NPBN  
      IF(IVAL.GT.0) CALL SETOBC2T(DELT,DELTD2,DELTI)  
C  
C**********************************************************************C  
C  
C **  ADJUST VOLUME SOURCE AND SINKS  
C  
C----------------------------------------------------------------------C  
C  
      IF(ISGWIE.EQ.0)THEN  
C  
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
C  
        DO L=2,LA  
          DIFQVOL=QSUME(L)-QSUMTMP(L)  
          DO K=1,KC  
            QSUM(L,K)=QSUM(L,K)-DIFQVOL*DZC(K)  
          ENDDO  
          QSUME(L)=QSUMTMP(L)  
        ENDDO  
C  
      ENDIF  
C  
C**********************************************************************C  
C  
C **  ADJUST SOURCES AND SINKS ESTIMATING SURFACE AND GROUNDWATER  
C **  AVAILABLE FOR EVAPOTRANSPIRATON AND INFILTRATION  
C  
C----------------------------------------------------------------------C  
C  
      IF(ISGWIE.GE.1)THEN  
C  
        DO L=2,LA  
          RIFTR(L)=0.  
          EVAPSW(L)=0.  
          EVAPGW(L)=0.  
          IF(H1P(L).GT.HDRY)THEN  
C       APPLY MAXIMUM ET  
            IF(EVAPCVT.LT.0.)THEN  
              SVPW=(10.**((0.7859+0.03477*TEM(L,KC))/  
     &            (1.+0.00412*TEM(L,KC))))  
              EVAPT(L)=CLEVAP(L)*0.7464E-3*WINDST(L)*(SVPW-VPA(L))
     &            /PATMT(L)  
            ENDIF  
            EVAPSW(L)=EVAPT(L)*DXYP(L)  
            RIFTR(L)=0.  
C       CALCULATE DEPTH OF ACTIVE GROUNDWATER ELEV BELOW SURFACE  
            DTAGW=BELV(L)-AGWELV(L)  
            IF(DTAGW.GT.0.0)THEN  
C         INFLITRATION CAN OCCUR, CALCULATE LIMITING RATE TO BRING  
C         GW ELEV TO SOIL SURFACE  
              RIFTRL=RNPOR*DTAGW*DELTI  
C         SET RIFTRL TO MIN OF LIMITING RATE OR ACTUAL RATE  
              RIFTRL=MIN(RIFTRM,RIFTRL)  
C         ESTIMATE RATE BASED ON AVAILABLE SURFACE WATER  
              RAVAIL=(H1P(L)-HDRY)*DELTI-EVAPT(L)  
C         SET RIFTRL TO MIN OF AVAILABLE RATE OR LIMITING RATE  
              RIFTRL=MIN(RAVAIL,RIFTRL)  
C         CONVERT TO VOLUME FLOW UNITS  
              RIFTR(L)=RIFTRL*DXYP(L)  
            ENDIF  
C       ADJUST VOLUME OUTFLOWS OF WET CELLS  
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
C  
        DO L=2,LA  
          DIFQVOL=QSUME(L)-QSUMTMP(L)  
          DO K=1,KC  
            QSUM(L,K)=QSUM(L,K)-DIFQVOL*DZC(K)  
          ENDDO  
          QSUME(L)=QSUMTMP(L)  
        ENDDO  
C  
      ENDIF  
C  
C**********************************************************************C  
C  
C **  ADVANCE EXTERNAL VARIABLES  
C  
C----------------------------------------------------------------------C  
C  
      DO L=1,LC 
        PMCTESTX(6,L)=UHDYE(L)  
        PMCTESTY(6,L)=VHDXE(L)
        PMCTESTX(7,L)=HP(L)  
      ENDDO  
C
C ROI      DO L=2,LA  
C ROI        UHDY2E(L)=UHDY1E(L)  
C ROI        VHDX2E(L)=VHDX1E(L)  
C ROI        UHDY1E(L)=UHDYE(L)  
C ROI        VHDX1E(L)=VHDXE(L)  
C ROI        U1V(L)=UV(L)  
C ROI        V1U(L)=VU(L)  
C ROI        P1(L)=P(L)  
C ROI        H1U(L)=HU(L)  
C ROI        H1V(L)=HV(L)  
C ROI        H1UI(L)=HUI(L)  
C ROI        H1VI(L)=HVI(L)  
C ROI        !H2P(L)=H1P(L)    pmc
C ROI        H1P(L)=HP(L)  
C ROI        AGWELV2(L)=AGWELV1(L)  
C ROI        AGWELV1(L)=AGWELV(L)  
C ROI      ENDDO  
C  
C**********************************************************************C  
C  
C **  SET OLD TIME LEVEL TERMS IN CONTINUITY EQUATION FOR  
C **  NON BOUNDARY POINTS  
C **  HRU=HMU*DYU/DXU & HRV=HMV*DXV/DYV  
C **  DXYIP=1/(DXP*DYP)  
C  
C----------------------------------------------------------------------C  
C  
      DO L=2,LA  
        TVAR3N(L)=VHDXE(LNC(L))  
      ENDDO  
      DO L=2,LA  
        FP1(L)=FP1(L)+SPB(L)*( DELTI*DXYP(L)*P(L)  
     &      -0.5*G*(UHDYE(L+1)-UHDYE(L)  
     &      +TVAR3N(L)-VHDXE(L)) )  
      ENDDO  
C  
C**********************************************************************C  
C  
C **  SET NEW TIME LEVEL TERMS IN CONTINUITY EQUATION INCLUDING  
C **  HOST-GUEST CHANNAL INTERACTION FOR NON BOUNDARY POINTS  
C  
C----------------------------------------------------------------------C  
C  
      DO L=2,LA  
        LN=LNC(L)  
        TVAR3N(L)=SVB(LN )*PMCTESTY(2,LN)  
      ENDDO  
      DO L=2,LA  
        PMCTESTX(3,L)=FP1(L)-0.5*G*SPB(L)*  
     &      ( SUB(L+1)*PMCTESTX(2,L+1)-SUB(L)*PMCTESTX(2,L)  
     &      +TVAR3N(L)-SVB(L)*PMCTESTY(2,L)  
     &      -2.0*QSUME(L) )  
      ENDDO  
C  
      IF(ISGWIE.GE.1)THEN  
        DO L=2,LA  
          PMCTESTX(3,L)=PMCTESTX(3,L)-G*SPB(L)*(RIFTR(L)+EVAPSW(L))  
        ENDDO  
      ENDIF  
C  
      CCMNM=1.E+18  
      DO L=2,LA  
        IF(SPB(L).GT.0.)THEN  
          C1=-0.5*DELTD2*G*SPB(L)  
          CS(L)=C1*SVB(L  )*HRVO(L  )*RCY(L  )*HVTMP(L  )  
          CW(L)=C1*SUB(L  )*HRUO(L  )*RCX(L  )*HUTMP(L  )  
          CE(L)=C1*SUB(L+1)*HRUO(L+1)*RCX(L+1)*HUTMP(L+1)  
        ENDIF  
      ENDDO  
      DO L=2,LA  
        IF(SPB(L).GT.0.)THEN  
          LN=LNC(L)  
          C1=-0.5*DELTD2*G*SPB(L)  
          CN(L)=C1*SVB(LN )*HRVO(LN )*RCY(LN )*HVTMP(LN )  
        ENDIF  
      ENDDO  
      DO L=2,LA  
        IF(SPB(L).GT.0.)THEN  
          CC(L)=SPB(L)*(DELTI*DXYP(L)-CS(L)-CW(L)-CE(L)-CN(L))  
        ENDIF  
      ENDDO  
      DO L=2,LA  
        CCMNM=MIN(CCMNM,CC(L))  
        FPTMP(L)=PMCTESTX(3,L)  
      ENDDO    
      CCMNMI=1./CCMNM  
C
      DO LL=1,NPBW  
        IF(ISPBW(LL).EQ.0)THEN  
          L=LPBW(LL)  
          CW(L+1)=0.  
        ENDIF  
      ENDDO  
C  
      DO LL=1,NPBE  
        IF(ISPBE(LL).EQ.0)THEN  
          L=LPBE(LL)  
          CE(L-1)=0.  
        ENDIF  
      ENDDO  
C  
      DO LL=1,NPBS  
        IF(ISPBS(LL).EQ.0)THEN  
          L=LPBS(LL)  
          LN=LNC(L)  
          CS(LN)=0.  
        ENDIF  
      ENDDO  
C  
      DO LL=1,NPBN  
        IF(ISPBN(LL).EQ.0)THEN  
          L=LPBN(LL)  
          LS=LSC(L)  
          CN(LS)=0.  
        ENDIF  
      ENDDO  
C  
      CC(1)=1.  
      CC(LC)=1.  
C  
C **  SCALE BY MINIMUM DIAGONAL  
C  
      IF(IRVEC.EQ.9.OR.IRVEC.EQ.9999)THEN  
C  
        DO L=2,LA  
          CCS(L)=CS(L)*CCMNMI  
          CCW(L)=CW(L)*CCMNMI  
          CCE(L)=CE(L)*CCMNMI  
          CCN(L)=CN(L)*CCMNMI  
          CCC(L)=CC(L)*CCMNMI  
          FPTMP(L)=FPTMP(L)*CCMNMI  
          CCCI(L)=1./CCC(L)  
        ENDDO  
C  
      ENDIF  
C
C *** SAVE FOR TESTING  PMC
      DO L=2,LA  
        CCS1(L)=CCS(L)
        CCW1(L)=CCW(L) 
        CCE1(L)=CCE(L)  
        CCN1(L)=CCN(L)  
        CCC1(L)=CCC(L)  
      ENDDO  

C  ** IRVEC = 9999 CALLS A NEW RED-BLACK ORDERED SOR  
C     SCHEME WHICH USING CHEBYCHEV ACCELERATION. THIS  
C     WORKS AND WAS IMPLEMENT FOR TESTING ONLY. SHOULD NOT  
C     BE USED FOR APPLICATIONS INVOVLING NS CELL MAPPINGS  
C  
      DO L=2,LA  
        TVAR3S(L)=P(L)  
      ENDDO  
      IF(ISAVEC.EQ.0) THEN  
        IF(IRVEC.EQ.9) CALL CONGRAD (ISTL)  
        IF(IRVEC.EQ.9999) CALL RELAX2T  
      END IF  
      DO L=2,LA  
        PMCTESTY(3,L)=P(L)
        P(L)=TVAR3S(L)
      ENDDO  
C  
C**********************************************************************C  
C  
C **  CALCULATE UHEX AND VHEX AND TOTAL DEPTHS AT TIME LEVEL (N+1)  
C **  HRU=SUB*DYU/DXU & HRV=SVB*DXV/DYV  
C  
C----------------------------------------------------------------------C  
C  
      DO L=2,LA  
        TVAR3S(L)=PMCTESTY(3,LSC(L))  
      ENDDO  
      DO L=2,LA  
        !UHDYE(L)=SUB(L)*(FUHDYE(L)-DELTD2*HRUO(L)*RCX(L)*HUTMP(L)*(P(L)-P(L-1)) )  
        PMCTESTX(4,L)=SUB(L)*( PMCTESTX(2,L) - 
     &   DELTD2*HRUO(L)*RCX(L)*HUTMP(L)*(PMCTESTY(3,L)-PMCTESTY(3,L-1)))
        !VHDXE(L)=SVB(L)*(FVHDXE(L)-DELTD2*HRVO(L)*RCY(L)*HVTMP(L)*(P(L)-TVAR3S(L)) )  
        PMCTESTY(4,L)=SVB(L)*( PMCTESTY(2,L) - 
     &   DELTD2*HRVO(L)*RCY(L)*HVTMP(L)*(PMCTESTY(3,L)-TVAR3S(L)) )  
      ENDDO  
C NA      DO L=2,LA  
C NA       UHE(L)=UHDYE(L)*DYIU(L)  
C NA       VHE(L)=VHDXE(L)*DXIV(L)  
C NA     ENDDO  
C  
C**********************************************************************C  
C  
C **  CALCULATE REVISED CELL DEPTHS BASED ON NEW HORIZONTAL  
C **  TRANSPORTS AT (N+1)  
C  
C----------------------------------------------------------------------C  
C  
      DO L=2,LA  
        LN=LNC(L)  
        !TVAR3N(L)=VHDXE(LN)+VHDX1E(LN)  
        TVAR3N(L)=PMCTESTY(4,LN)+PMCTESTY(6,LN)  
      ENDDO  
      DO L=2,LA  
        !TVAR3C(L)=H1P(L)+DELT*DXYIP(L)*(QSUME(L)  
        !     -0.5*(UHDYE(L+1)+UHDY1E(L+1)-UHDYE(L)-UHDY1E(L)  
        !     +TVAR3N(L)-VHDXE(L)-VHDX1E(L)))  
        if(.false.)then
        TVAR3C(L)=PMCTESTX(7,L)+DELT*DXYIP(L)*(QSUME(L)  
     &      -0.5*(PMCTESTX(4,L+1)+PMCTESTX(6,L+1)
     &           -PMCTESTX(4,L  )-PMCTESTX(6,L  )  
     &      +TVAR3N(L)-PMCTESTY(4,L)-PMCTESTY(6,L)))  
        else
        LN=LNC(L)  
        PMCTESTX(5,L)=PMCTESTX(7,L)+DELT*DXYIP(L)*(QSUME(L)  
     &      -0.5*(PMCTESTX(4,L+1)+PMCTESTX(6,L+1)
     &           -PMCTESTX(4,L  )-PMCTESTX(6,L  )  
     &           +PMCTESTY(4,LN )+PMCTESTY(6,LN )
     &           -PMCTESTY(4,L  )-PMCTESTY(6,L  )))  
        endif
      ENDDO  
      IF(ISGWIE.GE.1)THEN  
        DO L=2,LA  
          TVAR3C(L)=TVAR3C(L)-DELT*DXYIP(L)*(RIFTR(L)+EVAPSW(L))  
        ENDDO  
      ENDIF  
      DO L=2,LA  
        !HP(L)=SPB(L)*TVAR3C(L)+(1.-SPB(L))*(GI*P(L)-BELV(L))  
        if(.false.)then
        PMCTESTX(5,L)=SPB(L)*TVAR3C(L)+
     &               (1.-SPB(L))*(GI*PMCTESTY(3,L)-BELV(L))  
        endif
      ENDDO  
C  
C**********************************************************************C  
C  
C **  PERFORM FINAL UPDATES OF P,HU, AND HV  
C  
      DO L=2,LA  
        !P(L)=G*(HP(L)+BELV(L))  
        PMCTESTY(3,L)=G*(PMCTESTX(5,L)+BELV(L))  
      ENDDO  
C  
C**********************************************************************C  
C
C **  PERFORM UPDATE ON GROUNDWATER ELEVATION  
C  
C----------------------------------------------------------------------C  
C  
      IF(ISGWIE.GE.1)THEN  
  
      ENDIF  

      RETURN
C  
C**********************************************************************C  
C
      ENTRY NOW_CHECK

      ! *** NOW CHECK IF PARAMTERS ARE CLOSE
      EPSILON=1.E-7
      DO L=2,LA  
        IF(ABS(FPGXE(L)-PMCTESTX(1,L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
C
        IF(ABS(FPGYE(L)-PMCTESTY(1,L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
C     
        IF(ABS(FUHDYE(L)-PMCTESTX(2,L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
C     
        IF(ABS(FVHDXE(L)-PMCTESTY(2,L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
C 
        IF(ABS(CCS1(L)-CCS(L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
        IF(ABS(CCW1(L)-CCW(L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
        IF(ABS(CCE1(L)-CCE(L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
        IF(ABS(CCN1(L)-CCN(L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
        IF(ABS(CCC1(L)-CCC(L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
C
        IF(ABS(FP(L)-PMCTESTX(3,L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
C     
        IF(ABS(P(L)-PMCTESTY(3,L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
C     
        IF(ABS(UHDYE(L)-PMCTESTX(4,L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
C     
        IF(ABS(VHDXE(L)-PMCTESTY(4,L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
C     
        IF(ABS(HP(L)-PMCTESTX(5,L)).GT.EPSILON)THEN
          IPMC=0
        ENDIF
      ENDDO
        
      RETURN  
      END  
