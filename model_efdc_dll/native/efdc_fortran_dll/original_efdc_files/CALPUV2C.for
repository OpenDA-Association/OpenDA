      SUBROUTINE CALPUV2C  
C  
C **  PREVIOUS NAME WAS CALPUV2TC  
C CHANGE RECORD  
C  MODIFIED DRYING AND WETTING SCHEME. THE OLD FORMULATION REMAINS  
C  SEE (ISDRY.GT.0.AND.ISDRY.LT.98). THE NEW FORMULATION IS ACTIVATED  
C  BY (ISDRY.EQ.99). ALSO ADDED OPTION TO WASTE WATER FROM ESSENTIALLY  
C  DRY CELLS HAVING WATER DEPTHS GREATER THAN HDRY.  IE THE HIGH AND  
C  WET CELLS BLOCKED BY DRY CELLS. THIS IS ACTIVED BY A NEGATIVE VALUE  
C  OF NDRYSTP PARAMETER IS THE EFDC.INP FILE  
C  ADDED SAVE OF OLD VALUES OF HORIZONTAL FLOW FACE SWITCHES SUB1 & SVB1  
C  AND TRANSPORT BYPASS MASK, IMASKDRY FOR DRY CELLS. ADD VARIABLE  
C  IDRYDWN TO MARK WASTING FROM BLOCKED CELLS  
C  ADDED QDWASTE(L) TO SAVE SOURCE EQUIVALENT OF VOLUME LOSS RATE  
C  FOR REDUCING DEPTH OF HIGH/DRY CELLS.  ALSO ADDED CONCENTRATION  
C  ADJUSTMENT  
C ** SUBROUTINE CALPUV2TC CALCULATES THE EXTERNAL SOLUTION FOR P, UHDYE,  
C ** AND VHDXE, FOR FREE SURFACE FLOWS WITH PROVISIONS FOR WETTING  
C ** AND DRYING OF CELLS  
C  
      USE GLOBAL  

      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:)::IACTIVE  
      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:)::IQDRYDWN  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::QCHANUT  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::QCHANVT  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::QSUMTMP 
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DIFQVOL
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SUB1
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SVB1
      IF(.NOT.ALLOCATED(IACTIVE))THEN
        ALLOCATE(IACTIVE(NCHANM))  
        ALLOCATE(IQDRYDWN(LCM))  
        ALLOCATE(QCHANUT(NCHANM))  
        ALLOCATE(QCHANVT(NCHANM))  
        ALLOCATE(QSUMTMP(LCM))  
        ALLOCATE(DIFQVOL(LCM))  
        ALLOCATE(SUB1(LCM))  
        ALLOCATE(SVB1(LCM))
        IACTIVE=0 
        IQDRYDWN=0
        QCHANUT=0.
        QCHANVT=0.
        QSUMTMP=0.
        DIFQVOL=0.
        SUB1=0.
        SVB1=0.
      ENDIF
C  
      IF(N.EQ.1.AND.DEBUG)THEN  
        OPEN(1,FILE='MODCHAN.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
      ENDIF  
      IF(N.EQ.1.AND.ISDSOLV.EQ.1.AND.DEBUG)THEN  
        OPEN(1,FILE='FUV1.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='EQCOEF1.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='EQTERM1.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='FP1.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
      ENDIF  
      IF(N.EQ.2.AND.ISDSOLV.EQ.1.AND.DEBUG)THEN  
        OPEN(1,FILE='FUV2.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='EQCOEF2.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='EQTERM2.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='FP2.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
      ENDIF  
      IF(ISDSOLV.EQ.1.AND.DEBUG)THEN  
        OPEN(1,FILE='FUV.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='EQCOEF.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='EQTERM.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='FP.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
      ENDIF  
      IF(ISDYNSTP.EQ.0)THEN  
        DELT=DT  
        DELTD2=0.5*DT  
        DELTI=1./DELT  
      ELSE  
        DELT=DTDYN  
        DELTD2=0.5*DTDYN  
        DELTI=1./DELT  
      ENDIF  
      ISTL=2  
      RLAMN=QCHERR  
      RLAMO=1.-RLAMN  
C  
C **  SET SWITCHES FOR DRYING AND WETTING  
C  
      ITERHP=0  
      NCORDRY=0  
      ICORDRY=0  
      NEWDRY=0  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_LC(1,ithds)
         LL=jse_LC(2,ithds)
c
      DO L=LF,LL
        IQDRYDWN(L)=0  
        ISCDRY(L)=0
        SUB1(L)=SUB(L)
        SVB1(L)=SVB(L)
      ENDDO  
c
      enddo
C  
C **  INITIALIZE SUBGRID SCALE CHANNEL INTERACTIONS  
C  
      IF(MDCHH.GE.1)THEN  
        DO NMD=1,MDCHH  
          QCHANUT(NMD)=QCHANU(NMD)  
          QCHANVT(NMD)=QCHANV(NMD)  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE EXTERNAL BUOYANCY INTEGRALS AT TIME LEVEL (N)  
C  
      IF(BSC.GT.1.E-6)THEN
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        CALL CALEBI0(LF,LL)
c
      enddo
      ENDIF
C
        ! *** CALCULATE EXPLICIT EXTERNAL PRESSURE GRADIENTS  
!$OMP PARALLEL DO PRIVATE(LF,LL,LS)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      IF(BSC.GT.1.E-6)THEN
        DO L=LF,LL
          !SBX(L)=0.5*SUB(L)*DYU(L)  
          FPGXE(L)=-SBX(L)*HU(L)*GP*((BI2(L)+BI2(L-1))*(HP(L)-HP(L-1))  
     &        +2.0*HU(L)*(BI1(L)-BI1(L-1))    
     &        +(BE(L)+BE(L-1))*(BELV(L)-BELV(L-1)))  
          LS=LSC(L)  
          !SBY(L)=0.5*SVB(L)*DXV(L)  
          FPGYE(L)=-SBY(L)*HV(L)*GP*((BI2(L)+BI2(LS))*(HP(L)-HP(LS))  
     &        +2.0*HV(L)*(BI1(L)-BI1(LS)) 
     &        +(BE(L)+BE(LS))*(BELV(L)-BELV(LS)))  
        ENDDO  
      ENDIF
c
c     enddo
C  
C **  CALCULATE EXPLICIT EXTERNAL UHDYE AND VHDXE EQUATION TERMS  
C **  HRU=SUB*HMU*DYU/DXU & HRV=SVB*HMV*DXV/DYV  
C  
c!$OMP PARALLEL DO PRIVATE(LF,LL,LS)
c     do ithds=0,nthds-1
c        LF=jse(1,ithds)
c        LL=jse(2,ithds)
c 
      DO L=LF,LL
        H2P(L)=HP(L)  ! *** DSLLC SINGLE LINE
        LS=LSC(L)  
        !DXYU(L)=DXU(L)*DYU(L)  
        !DXIU(L)=1./DXU(L)  
        !HRUO(L)=SUBO(L)*DYU(L)*DXIU(L)  
        FUHDYE(L)=UHDYE(L)  
     &      -DELTD2*SUB(L)*HRUO(L)*HU(L)*(P(L)-P(L-1))  
     &      +SUB(L)*DELT*DXIU(L)*(DXYU(L)*(TSX(L)-RITB1*TBX(L))  
     &      +FCAXE(L)+FPGXE(L)-SNLT*FXE(L))  
C
        !DXYV(L)=DXV(L)*DYV(L)  
        !DYIV(L)=1./DYV(L)  
        !HRVO(L)=SVBO(L)*DXV(L)*DYIV(L)
        FVHDXE(L)=VHDXE(L)  
     &      -DELTD2*SVB(L)*HRVO(L)*HV(L)*(P(L)-P(LS))  
     &      +SVB(L)*DELT*DYIV(L)*(DXYV(L)*(TSY(L)-RITB1*TBY(L))  
     &      -FCAYE(L)+FPGYE(L)-SNLT*FYE(L)) 
      ENDDO  
c
      enddo
      IF(ISDSOLV.GE.1.AND.DEBUG)THEN  
        OPEN(1,FILE='FUV.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        WRITE(1,1001)N,ISTL  
        DO L=2,LA  
          WRITE(1,1001)IL(L),JL(L),UHDY1E(L),HRUO(L),HU(L),P1(L),  
     &        P1(L-1),TSX1(L),TBX1(L),FCAXE(L),FPGXE(L),FXE(L)  
        ENDDO  
        CLOSE(1)  
        IF(N.EQ.1)THEN  
          OPEN(1,FILE='FUV1.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),UHDY1E(L),HRUO(L),HU(L),P1(L),  
     &          P1(L-1),TSX1(L),TBX1(L),FCAXE(L),FPGXE(L),FXE(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
        IF(N.EQ.2)THEN  
          OPEN(1,FILE='FUV2.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),UHDY1E(L),HRUO(L),HU(L),P1(L),  
     &          P1(L-1),TSX1(L),TBX1(L),FCAXE(L),FPGXE(L),FXE(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
      ENDIF  
C  
C **  SET IMPLICIT BOTTOM AND VEGETATION DRAG AS APPROPRIATE  
C   
      RCX(1)=0.  
      RCY(1)=0.  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c 
      DO L=LF,LL
        RCX(L)=1.  
        RCY(L)=1.  
      ENDDO  
c
      enddo
      RCX(LC)=0.  
      RCY(LC)=0.  
C  
C * SINGLE LAYER NO VEGETATION  
C  
      IF(KC.EQ.1)THEN
        IF(ISVEG.EQ.0.AND.RITB.GT.0.)THEN  
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
	    IF(ISVEG.GE.1)THEN  
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
C **  RESET BOUNDARY CONDITIONS SWITCHES  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c 
      DO L=LF,LL
        SUB(L)=SUBO(L)  
        SVB(L)=SVBO(L)  
        SBX(L)=SBXO(L)  
        SBY(L)=SBYO(L)  
c       SUB(L+1)=SUBO(L+1)  
c       SBX(L+1)=SBXO(L+1)  
      ENDDO  
c
      enddo
        SUB(LC)=SUBO(LC)  
        SBX(LC)=SBXO(LC)  
        SVB(1)=SVBO(1)  
        SBY(1)=SBYO(1)  
        SVB(LC)=SVBO(LC)  
        SBY(LC)=SBYO(LC)  
c     DO L=2,LA  
c       LN=LNC(L)  
c       SVB(LN)=SVBO(LN)  
c       SBY(LN)=SBYO(LN)  
c     ENDDO  
C  
C **  ADJUST VOLUME SOURCE AND SINKS  
C  
      IF(ISGWIE.EQ.0)THEN  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c 
        DO L=LF,LL
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
          DIFQVOL(L)=QSUME(L)-QSUMTMP(L)  
          QSUME(L)=QSUMTMP(L)  
        ENDDO  
          DO K=1,KC  
        DO L=LF,LL
            QSUM(L,K)=QSUM(L,K)-DIFQVOL(L)*DZC(K)  
          ENDDO  
        ENDDO  
c
      enddo
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
          DIFQVOL(L)=QSUME(L)-QSUMTMP(L)  
          QSUME(L)=QSUMTMP(L)  
        ENDDO  
        DO L=2,LA  
          DO K=1,KC  
            QSUM(L,K)=QSUM(L,K)-DIFQVOL(L)*DZC(K)  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  ADVANCE EXTERNAL VARIABLES  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL,LN)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c 
      DO L=LF,LL
        UHDY1E(L)=UHDYE(L)  
        VHDX1E(L)=VHDXE(L)  
        P1(L)=P(L)  
        H1U(L)=HU(L)  
        H1V(L)=HV(L)  
        H1UI(L)=HUI(L)  
        H1VI(L)=HVI(L)  
C PMC        H2P(L)=H1P(L)  
        H1P(L)=HP(L)
      ENDDO
C
      IF(ISGWIE.GE.1)THEN
        DO L=LF,LL
          AGWELV2(L)=AGWELV1(L)  
          AGWELV1(L)=AGWELV(L)  
        ENDDO
      ENDIF  
C  
C **  SET OLD TIME LEVEL TERMS IN CONTINUITY EQUATION FOR NON BOUNDARY POINTS  
C **  HRU=HMU*DYU/DXU & HRV=HMV*DXV/DYV  
C **  DXYIP=1/(DXP*DYP)  
C  
C *** DSLLC BEGIN BLOCK
      DO L=LF,LL
        LN=LNC(L)
        FP1(L)=DELTI*DXYP(L)*P(L)-0.5*G*(UHDYE(L+1)-UHDYE(L)
     &                                  +VHDXE(LN )-VHDXE(L))
      ENDDO  
c
      enddo
C  
C **  SET NEW TIME LEVEL TERMS IN CONTINUITY EQUATION INCLUDING  
C **  HOST-GUEST CHANNAL INTERACTION FOR NON BOUNDARY POINTS  
C **  REENTER AT 1000 FOR WETTING-DRYING CORRECTION AND CHANNEL  
C **  INTERACTION  
C  
 1000 CONTINUE  
      C1=0.5*G
!$OMP PARALLEL DO PRIVATE(LF,LL,LN)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c 
      DO L=LF,LL
        LN=LNC(L)
        ! ***  THE SUB & SVB SWITCHES ALREADY ACCOUNTED FOR
        FP(L)=FP1(L)-C1*(FUHDYE(L+1)-FUHDYE(L)  
     &                  +FVHDXE(LN )-FVHDXE(L)
     &              -2.0*QSUME(L) )
      ENDDO  
c
      enddo
C
      IF(ISGWIE.GE.1)THEN  
        DO L=2,LA  
          FP(L)=FP(L)-G*SPB(L)*(RIFTR(L)+EVAPSW(L))  
        ENDDO  
      ENDIF  
C
      C1=-0.5*DELTD2*G  
!$OMP PARALLEL DO PRIVATE(LF,LL,LN)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c 
      DO L=LF,LL
        CS(L)=C1*SVB(L  )*HRVO(L  )*RCY(L  )*HV(L  )  
        CW(L)=C1*SUB(L  )*HRUO(L  )*RCX(L  )*HU(L  )  
        CE(L)=C1*SUB(L+1)*HRUO(L+1)*RCX(L+1)*HU(L+1)  
        LN=LNC(L)  
        CN(L)=C1*SVB(LN )*HRVO(LN )*RCY(LN )*HV(LN )  
      ENDDO  
c
      enddo
C
C *** APPLY THE OPEN BOUNDARY CONDITIONS
C
      IF(NBCSOP.GT.0) CALL SETOPENBC(DELT,DELTD2,DELTI,HU,HV)    
C  
      ! *** SET THE CENTER
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c 
      DO L=LF,LL
        CC(L)=DELTI*DXYP(L)-CS(L)-CW(L)-CE(L)-CN(L)  
      ENDDO  
c
      enddo
C
C **  INSERT IMPLICT SUB-GRID SCALE CHANNEL INTERACTIONS  
C  
      IF(MDCHH.GE.1)CALL SUBCHAN(QCHANUT,QCHANVT,IACTIVE,RLAMN,RLAMO,  
     &    DELT,IACTALL)  
C  
      ! *** SCALE COEFFICIENTS IN EXTERNAL MODEL LINEAR EQUATION SYSTEM  
      CCMNM=1.E+18  
!$OMP PARALLEL DO PRIVATE(LF,LL) REDUCTION(min:CCMNM)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c 
      DO L=LF,LL
        CCMNM=MIN(CCMNM,CC(L))  
        FPTMP(L)=FP(L)  
      ENDDO  
c
      enddo
      CCMNMI=1./CCMNM  

C
C *** APPLY THE OPEN BOUNDARY CONDITIONS FOR ADJACENT CELLS
C
      IF(NBCSOP.GT.0) CALL SETOPENBC2   
C
      IF(ISDSOLV.GE.1.AND.DEBUG)THEN  
        OPEN(1,FILE='FP.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        WRITE(1,1001)N,ISTL  
        DO L=2,LA  
          WRITE(1,1001)IL(L),JL(L),FP1(L),FUHDYE(L),FUHDYE(L+1),  
     &        FVHDXE(L),FVHDXE(LNC(L)),QSUME(L),RIFTR(L),EVAPSW(L)  
        ENDDO  
        CLOSE(1)  
        IF(N.EQ.1)THEN  
          OPEN(1,FILE='FP1.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),FP1(L),FUHDYE(L),FUHDYE(L+1),  
     &          FVHDXE(L),FVHDXE(LNC(L)),QSUME(L),RIFTR(L),EVAPSW(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
        IF(N.EQ.2)THEN  
          OPEN(1,FILE='FP2.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),FP1(L),FUHDYE(L),FUHDYE(L+1),  
     &          FVHDXE(L),FVHDXE(LNC(L)),QSUME(L),RIFTR(L),EVAPSW(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
      ENDIF  
C
      CC(1)=1.  
      CC(LC)=1.  
C  
C **  SCALE BY MINIMUM DIAGONAL  
C  
      IF(IRVEC.EQ.9)THEN  
!$OMP PARALLEL DO PRIVATE(LF,LL) 
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c 
        DO L=LF,LL
          CCS(L)=CS(L)*CCMNMI  
          CCW(L)=CW(L)*CCMNMI  
          CCE(L)=CE(L)*CCMNMI  
          CCN(L)=CN(L)*CCMNMI  
          CCC(L)=CC(L)*CCMNMI  
          FPTMP(L)=FPTMP(L)*CCMNMI  
          CCCI(L)=1./CCC(L)  
        ENDDO  
c
      enddo   
        IF(MDCHH.GE.1)THEN  
          DO NMD=1,MDCHH  
            CCCCHH(NMD)=CCCCHH(NMD)*CCMNMI  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  CALL EQUATION SOLVER  
C  
      IF(MDCHH.EQ.0) CALL CONGRAD(ISTL)  
      IF(MDCHH.GE.1) CALL CONGRADC(ISTL)  
C  
C ** DIAGNOSTICS  
C  
      IF(ISDSOLV.GE.1.AND.DEBUG)THEN  
        OPEN(1,FILE='EQCOEF.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        WRITE(1,1001)N,ISTL  
        DO L=2,LA  
          SURFTMP=GI*P(L)  
          WRITE(1,1001)IL(L),JL(L),CS(L),CW(L),CC(L),CE(L),CN(L),  
     &        FP(L),SURFTMP  
        ENDDO  
        IF(MDCHH.GE.1)THEN  
          DO NMD=1,MDCHH  
            WRITE(1,1001)NMD,MDCHTYP(NMD),CCCCHH(NMD),CCCCHU(NMD),  
     &          CCCCHV(NMD),QCHANUT(NMD),QCHANVT(NMD)  
          ENDDO  
        ENDIF  
        CLOSE(1)  
        IF(N.EQ.1)THEN  
          OPEN(1,FILE='EQCOEF1.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL  
          DO L=2,LA  
            SURFTMP=GI*P(L)  
            WRITE(1,1001)IL(L),JL(L),CS(L),CW(L),CC(L),CE(L),CN(L),  
     &          FP(L),SURFTMP  
          ENDDO  
          IF(MDCHH.GE.1)THEN  
            DO NMD=1,MDCHH  
              WRITE(1,1001)NMD,MDCHTYP(NMD),CCCCHH(NMD),CCCCHU(NMD),  
     &            CCCCHV(NMD),QCHANUT(NMD),QCHANVT(NMD)  
            ENDDO  
          ENDIF  
          CLOSE(1)  
        ENDIF  
        IF(N.EQ.2)THEN  
          OPEN(1,FILE='EQCOEF2.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL  
          DO L=2,LA  
            SURFTMP=GI*P(L)  
            WRITE(1,1001)IL(L),JL(L),CS(L),CW(L),CC(L),CE(L),CN(L),  
     &          FP(L),SURFTMP  
          ENDDO  
          IF(MDCHH.GE.1)THEN  
            DO NMD=1,MDCHH  
              WRITE(1,1001)NMD,MDCHTYP(NMD),CCCCHH(NMD),CCCCHU(NMD),  
     &            CCCCHV(NMD),QCHANUT(NMD),QCHANVT(NMD)  
            ENDDO  
          ENDIF  
          CLOSE(1)  
        ENDIF  
      ENDIF  
      IF(ISDSOLV.GE.1.AND.DEBUG)THEN  
        OPEN(1,FILE='EQTERM.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        WRITE(1,1001)N,ISTL  
        DO L=2,LA  
          WRITE(1,1001)IL(L),JL(L),SUB(L),SVB(L),HRUO(L),  
     &        HRVO(L),HU(L),HV(L)  
        ENDDO  
        CLOSE(1)  
        IF(N.EQ.1)THEN  
          OPEN(1,FILE='EQTERM1.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),SUB(L),SVB(L),HRUO(L),  
     &          HRVO(L),HU(L),HV(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
        IF(N.EQ.2)THEN  
          OPEN(1,FILE='EQTERM2.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,1001)N,ISTL  
          DO L=2,LA  
            WRITE(1,1001)IL(L),JL(L),SUB(L),SVB(L),HRUO(L),  
     &          HRVO(L),HU(L),HV(L)  
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
!$OMP PARALLEL DO PRIVATE(LF,LL,LS)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO L=LF,LL
        LS=LSC(L)  
        UHDYE(L)=SUB(L)*( FUHDYE(L)  
     &      -DELTD2*HRUO(L)*RCX(L)*HU(L)*(P(L)-P(L-1)) )  
        VHDXE(L)=SVB(L)*( FVHDXE(L)  
     &      -DELTD2*HRVO(L)*RCY(L)*HV(L)*(P(L)-P(LS )) )  
      ENDDO  
      DO L=2,LA  
        UHE(L)=UHDYE(L)*DYIU(L)  
        VHE(L)=VHDXE(L)*DXIV(L)  
      ENDDO  
c
      enddo
C  
C **  CALCULATE NEW SUB-GRID SCALE CHANNEL EXCHANGE FLOWS  
C  
      IF(MDCHH.GE.1)THEN  
        DO NMD=1,MDCHH  
          IF (IACTIVE(NMD).GT.0)THEN  
            LHOST=LMDCHH(NMD)  
            LCHNU=LMDCHU(NMD)  
            LCHNV=LMDCHV(NMD)  
            IF(MDCHTYP(NMD).EQ.1)THEN  
C  
C             QCHANU(NMD)=0.  
C  
              QCHANU(NMD)=CCCCHU(NMD)*QCHANUT(NMD)  
     &            -RLAMN*CCCCHU(NMD)*CCCCHV(NMD)*(P(LHOST)-P(LCHNU))  
     &            -RLAMO*CCCCHU(NMD)*CCCCHV(NMD)*(P1(LHOST)-P1(LCHNU))  
              QCHANUN(NMD)=QCHANUT(NMD)  
              QCHANV(NMD)=0.  
              QCHANVN(NMD)=QCHANVT(NMD)  
            ENDIF  
            IF(MDCHTYP(NMD).EQ.2)THEN  
C  
C             QCHANV(NMD)=0.  
C  
              QCHANV(NMD)=CCCCHU(NMD)*QCHANVT(NMD)  
     &            -RLAMN*CCCCHU(NMD)*CCCCHV(NMD)*(P(LHOST)-P(LCHNV))  
     &            -RLAMO*CCCCHU(NMD)*CCCCHV(NMD)*(P1(LHOST)-P1(LCHNV))  
              QCHANVN(NMD)=QCHANVT(NMD)  
              QCHANU(NMD)=0.  
              QCHANUN(NMD)=QCHANUT(NMD)  
            ENDIF  
          ELSE  
            QCHANV(NMD)=0.  
            QCHANVN(NMD)=0.  
            QCHANU(NMD)=0.  
            QCHANUN(NMD)=0.  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE REVISED CELL DEPTHS BASED ON NEW HORIZONTAL  
C **  TRANSPORTS AT (N+1)  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL,LN)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO L=LF,LL
        LN=LNC(L)  
        HP(L)=H1P(L)+DELTD2*DXYIP(L)*(2.*QSUME(L) !+QSUM1E(L) PMC
     &           -(UHDYE(L+1)+UHDY1E(L+1)-UHDYE(L)-UHDY1E(L)  
     &            +VHDXE(LN) +VHDX1E(LN )-VHDXE(L)-VHDX1E(L)))  
      ENDDO  
c
      enddo
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
C **  ADD CHANNEL INTERACTION EXCHANGES  
C  
      IF(MDCHH.GE.1)THEN  
        DO NMD=1,MDCHH  
          IF(IACTIVE(NMD).GT.0)THEN  
            LHOST=LMDCHH(NMD)  
            LCHNU=LMDCHU(NMD)  
            LCHNV=LMDCHV(NMD)  
            IF(MDCHTYP(NMD).EQ.1)THEN  
              TMPVAL=DELT*(RLAMN*QCHANU(NMD)+RLAMO*QCHANUT(NMD))  
              HP(LHOST)=HP(LHOST)+TMPVAL*DXYIP(LHOST)  
              HP(LCHNU)=HP(LCHNU)-TMPVAL*DXYIP(LCHNU)  
            ENDIF  
            IF(MDCHTYP(NMD).EQ.2)THEN  
              TMPVAL=DELT*(RLAMN*QCHANV(NMD)+RLAMO*QCHANVT(NMD))  
              HP(LHOST)=HP(LHOST)+TMPVAL*DXYIP(LHOST)  
              HP(LCHNV)=HP(LCHNV)-TMPVAL*DXYIP(LCHNV)  
            ENDIF  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  PERFORM INTERMEDIATE UPDATES OF P
C  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO L=LF,LL
        P(L)=G*(HP(L)+BELV(L))  
      ENDDO  
c
      enddo
C  
C **  CHECK FOR DRYING AND RESOLVE EQUATIONS IF NECESSARY  
C  
      IF(ISDRY.GT.0.AND.ISDRY.LT.98)THEN  
        ICORDRY=0  
!$OMP PARALLEL DO PRIVATE(LF,LL) REDUCTION(+:ICORDRY)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO L=LF,LL-1
          IF(HP(L).LE.HDRY)THEN  
            IF(ISCDRY(L).EQ.0)THEN  
              ISCDRY(L)=1  
              ICORDRY=ICORDRY+1  
            ENDIF  
            SUB(L)=0.  
            SVB(L)=0.  
            SBX(L)=0.  
            SBY(L)=0.  
            SUB(L+1)=0.  
            SBX(L+1)=0.  
          ENDIF  
        ENDDO  
c
      enddo
      do ithds=0,nthds-1
         LL=jse(2,ithds)
c
           L=LL
          IF(HP(L).LE.HDRY)THEN
            IF(ISCDRY(L).EQ.0)THEN
              ISCDRY(L)=1
              ICORDRY=ICORDRY+1
            ENDIF
            SUB(L)=0.
            SVB(L)=0.
            SBX(L)=0.
            SBY(L)=0.
            SUB(L+1)=0.
            SBX(L+1)=0.
          ENDIF
c
      enddo

        DO L=2,LA  
          IF(HP(L).LE.HDRY)THEN  
            LN=LNC(L)  
            IF(SVB(LN).NE.0.) SVB(LN)=0.  
            IF(SBY(LN).NE.0.) SBY(LN)=0.  
          ENDIF  
        ENDDO  
        IF(ICORDRY.GT.0)THEN  
          NCORDRY=NCORDRY+1  
          GOTO 1000  
        ENDIF  
      ENDIF  
 6960 FORMAT(' NCORDRY =', I5)  
 6961 FORMAT(' UNSTABLE, NCORDRY =', I5)  
 9999 CONTINUE  
C  
C **  CHECK FOR DRYING AND RESOLVE EQUATIONS IF NECESSARY  
C  
      IF(ISDRY.EQ.99)THEN  
        HDRY2=2.*HDRY  
        ICORDRY=0  
        DO L=2,LA  
          LS=LSC(L)  
          LN=LNC(L)  
          IF(HP(L).LE.HDRY)THEN  
            SUBW=SUB(L)  
            SUBE=SUB(L+1)  
            SVBS=SVB(L)  
            SVBN=SVB(LN)  
            DHPDT=DELTI*(HP(L)-H1P(L))  
            ! *** ALLOW RE-WETTING
            IF(DHPDT.GT.0.0)THEN  
              SUB(L)=0.0  
              SUB(L+1)=0.0  
              SVB(L)=0.0  
              SVB(LN)=0.0  
              SBX(L)=0.0  
              SBX(L+1)=0.0  
              SBY(L)=0.0  
              SBY(LN)=0.0  
              IF(SUBO(L).GT.0.5)THEN  
                IF(UHDYE(L).GT.0.0.AND.HP(L-1).GT.HDRY2)THEN 
                  SUB(L)=1.  
                  SBX(L)=1.  
                ENDIF  
              ENDIF  
              IF(SUBO(L+1).GT.0.5)THEN  
                IF(UHDYE(L+1).LT.0.0.AND.HP(L+1).GT.HDRY2)THEN
                  SUB(L+1)=1.  
                  SBX(L+1)=1.  
                ENDIF  
              ENDIF  
              IF(SVBO(L).GT.0.5)THEN  
                IF(VHDXE(L).GT.0.0.AND.HP(LS).GT.HDRY2)THEN
                  SVB(L)=1.  
                  SBY(L)=1.  
                ENDIF  
              ENDIF  
              IF(SVBO(LN).GT.0.5)THEN  
                IF(VHDXE(LN).LT.0.0.AND.HP(LN).GT.HDRY2)THEN
                  SVB(LN)=1.  
                  SBY(LN)=1.  
                ENDIF  
              ENDIF  
              RDRY=SUB(L)+SUB(L+1)+SVB(L)+SVB(LN)  
              IF(RDRY.LT.0.5)THEN  
                ISCDRY(L)=1  
              ELSE  
                ISCDRY(L)=0  
              ENDIF  
              TMPVAL=ABS(SUB(L)-SUBW)  
              IF(TMPVAL.GT.0.5)THEN
                ICORDRY=1  
              ELSE
                TMPVAL=ABS(SUB(L+1)-SUBE)  
                IF(TMPVAL.GT.0.5)THEN
                  ICORDRY=1  
                ELSE
                  TMPVAL=ABS(SVB(L)-SVBS)  
                  IF(TMPVAL.GT.0.5)THEN
                    ICORDRY=1  
                  ELSE
                    TMPVAL=ABS(SVB(LN)-SVBN)  
                    IF(TMPVAL.GT.0.5)THEN ICORDRY=1  
                  ENDIF
                ENDIF
              ENDIF
            ELSE  
              SUB(L)=0.0  
              SUB(L+1)=0.0  
              SVB(L)=0.0  
              SVB(LN)=0.0  
              SBX(L)=0.0  
              SBX(L+1)=0.0  
              SBY(L)=0.0  
              SBY(LN)=0.0  
              IF(ISCDRY(L).EQ.0)THEN  
                ISCDRY(L)=1  
                ICORDRY=1  
              ENDIF  
            ENDIF  
          ENDIF  
        ENDDO  
        IF(ICORDRY.EQ.1)THEN  
          NCORDRY=NCORDRY+1  
          GOTO 1000  
        ENDIF  
      ENDIF  

C      WRITE(8,6960)NCORDRY  
C**********************************************************************C
C
C **  COUNT THE NUMBER TO TIME STEPS A CELL IS DRY, AND IF IT HAS BEEN
C **  DRY FOR MORE THAN ABS(NDRYSTP), AND ITS BOTTOM ELEVATION IS HIGHER
C **  THAN THE SURROUNDING DRY CELLS, THEN REDUCE ITS DEPTH BELOW THE 
C **  DRYING DEPTH IF NECESSARY.  SAVE VOLUME REDUCTION RATE AS QDWASTE
C **  DEFINED AS POSITIVE OUT. THEN ADJUST CONCENTRATIONS
C
      IF(ISDRY.GT.0) THEN
        IF(NDRYSTP.LT.0) THEN
	    NTMP=ABS(NDRYSTP)
	    DO L=2,LA
	      LN=LNC(L)
	      LS=LSC(L)
            QDWASTE(L)=0.
            IQDRYDWN(L)=0
	      RDRY=SUB(L)+SUB(L+1)+SVB(L)+SVB(LN)
	      IF(RDRY.GT.0.5)NATDRY(L)=0
            IF(RDRY.LT.0.5)NATDRY(L)=NATDRY(L)+1
	      IF(NATDRY(L).GT.NTMP)THEN	
	        IF(HP(L).GE.HDRY)THEN
	          BELVAVG=0.0
	          RVAL=0.0
	          IF(HP(L+1).LT.HDRY.AND.SUBO(L+1).GT.0.5)THEN
	            BELVAVG=BELVAVG+BELV(L+1)
                  RVAL=RVAL+1.
	          ENDIF
	          IF(HP(L-1).LT.HDRY.AND.SUBO(L).GT.0.5)THEN
	            BELVAVG=BELVAVG+BELV(L-1)
                  RVAL=RVAL+1.
	          ENDIF
	          IF(HP(LN).LT.HDRY.AND.SVBO(LN).GT.0.5)THEN
	            BELVAVG=BELVAVG+BELV(LN)
                  RVAL=RVAL+1.
	          ENDIF
	          IF(HP(LS).LT.HDRY.AND.SVBO(L).GT.0.5)THEN
	            BELVAVG=BELVAVG+BELV(LS)
                  RVAL=RVAL+1.
	          ENDIF
	          IF(BELV(L).GE.BELVAVG)THEN
	            HOLDTMP=HP(L)
			        IQDRYDWN(L)=1
	            HP(L)=0.90*HDRY
	            NATDRY(L)=0
	            QDWASTE(L)=DELTI*DXYP(L)*(HOLDTMP-HP(L))
	            VDWASTE(L)=VDWASTE(L)+DXYP(L)*(HOLDTMP-HP(L))
	            TMPVAL=HOLDTMP/HP(L)

                  ! *** PMC COMMENTED OUT-CONC NOT MASS SO NOT NEEDED
	            !DO K=1,KC
	            !  SAL(L,K)=TMPVAL*SAL(L,K)
	            !  TEM(L,K)=TMPVAL*SAL(L,K)
	            !  DYE(L,K)=TMPVAL*SAL(L,K)
	            !  DO NT=1,NTOX
	            !    TOX(L,K,NT)=TMPVAL*TOX(L,K,NT)
	            !  ENDDO
	            !  DO NS=1,NSED
	            !    SED(L,K,NS)=TMPVAL*SED(L,K,NS)
	            !  ENDDO
	            !  DO NX=1,NSND
	            !    SND(L,K,NX)=TMPVAL*SND(L,K,NX)
	            !  ENDDO
	            !ENDDO
	          ENDIF
              END IF
	      ENDIF
	      IF(QDWASTE(L).GT.0.0)THEN
	        TMPVAL=QDWASTE(L)/DXYP(L)
C	        WRITE(8,8888)IL(L),JL(L),TIME,RDRY,HOLDTMP,HP(L),
C     &             QDWASTE(L),TMPVAL
C	        WRITE(6,8888)IL(L),JL(L),TIME,RDRY,HOLDTMP,HP(L),
C     &             QDWASTE(L),TMPVAL
	      ENDIF
          ENDDO
	  END IF
	END IF	      
C
C 8888 FORMAT(' QDW ',2I6,6E14.6)
C
C**********************************************************************C
C  
C **  PERFORM FINAL UPDATES OF P,HU, AND HV  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL,LS)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO L=LF,LL
        P(L)=G*(HP(L)+BELV(L))  
        LS=LSC(L)
        HU(L)=0.5*(DXYP(L)*HP(L)+DXYP(L-1)*HP(L-1))*DXYIU(L)  
        HV(L)=0.5*(DXYP(L)*HP(L)+DXYP(LS )*HP(LS ))*DXYIV(L)  
        H1P(L)=H2P(L)  ! *** DSLLC, UPDATE THE LAST DEPTH TO ACTUAL PREVIOUS  
        HPI(L)=1./HP(L)  
        HUI(L)=1./HU(L)  
        HVI(L)=1./HV(L)  
      ENDDO  
c
      enddo
C  
C **  SET TRANSPORT MASK FOR DRY CELLS  
C  
      IF(ISDRY.GT.0)THEN  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO L=LF,LL
          IMASKDRY(L)=0  
          LMASKDRY(L)=.TRUE.  
        END DO  
c
      enddo
        IF(IDRYTBP.EQ.1)THEN  
          DO L=2,LA  
            LN=LNC(L)  
            IUW=0  
            IUE=0  
            IVS=0  
            IVN=0  
            IF(SUB1(L).LT.0.5.AND.SUB(L).LT.0.5)IUE=1  
            IF(SUB1(L+1).LT.0.5.AND.SUB(L+1).LT.0.5)IUW=1  
            IF(SVB1(L).LT.0.5.AND.SVB(L).LT.0.5)IVS=1  
            IF(SVB1(LN).LT.0.5.AND.SVB(LN).LT.0.5)IVN=1  
            IFACE=IUW+IUE+IVS+IVN  
            IF(IFACE.EQ.4)THEN  
              IMASKDRY(L)=1  
              LMASKDRY(L)=.FALSE.  
              IF(H1P(L).EQ.HP(L))IMASKDRY(L)=2  
            END IF  
            IF(IQDRYDWN(L).EQ.1)THEN  
              IMASKDRY(L)=0  
              LMASKDRY(L)=.TRUE.  
            ENDIF  
          END DO  
        END IF  
      END IF  
C  
C **  OUTPUT DIAGNOSTICS FOR 2 GRID INTERATCTION  
C  
      IF(MDCHH.GT.0.AND.DEBUG)THEN  
        IF(MDCHHD.GT.0)THEN  
          IVAL=MOD(N,MDCHHD2)  
          IF(IVAL.EQ.0)THEN  
            IF(IACTALL.GT.0)THEN  
              IF(DEBUG)OPEN(1,FILE='MODCHAN.OUT',POSITION='APPEND')  
              DO NMD=1,MDCHH  
                WRITE(1,8000)  
                LHOST=LMDCHH(NMD)  
                IHOST=IL(LHOST)  
                JHOST=JL(LHOST)  
                LCHNU=LMDCHU(NMD)  
                LCHNV=LMDCHV(NMD)  
C  
C         X-DIRECTION CHANNEL  
C  
                IF(MDCHTYP(NMD).EQ.1)THEN  
                  ICHNU=IL(LCHNU)  
                  JCHNU=JL(LCHNU)  
                  SRFCHAN=HP(LCHNU)+BELV(LCHNU)  
                  SRFHOST=HP(LHOST)+BELV(LHOST)  
                  SRFCHAN1=H1P(LCHNU)+BELV(LCHNU)  
                  SRFHOST1=H1P(LHOST)+BELV(LHOST)  
                  WRITE(1,8001)N,NMD,MDCHTYP(NMD),ICHNU,JCHNU,
     &               ISCDRY(LCHNU),SRFCHAN,HP(LCHNU),SRFCHAN1,H1P(LCHNU)  
                  WRITE(1,8002)IHOST,JHOST,ISCDRY(LHOST),  
     &                 SRFHOST,HP(LHOST),SRFHOST1,H1P(LHOST)  
                  WRITE(1,8003)QCHANU(NMD),QCHANUT(NMD),CCCCHU(NMD)
     &                ,CCCCHV(NMD)  
                ENDIF  
C  
C         Y-DIRECTION CHANNEL  
C  
                IF(MDCHTYP(NMD).EQ.2)THEN  
                  ICHNV=IL(LCHNV)  
                  JCHNV=JL(LCHNV)  
                  SRFCHAN=HP(LCHNV)+BELV(LCHNV)  
                  SRFHOST=HP(LHOST)+BELV(LHOST)  
                  SRFCHAN1=H1P(LCHNV)+BELV(LCHNV)  
                  SRFHOST1=H1P(LHOST)+BELV(LHOST)  
                  WRITE(1,8001)N,NMD,MDCHTYP(NMD),ICHNV,JCHNV,
     &               ISCDRY(LCHNV),SRFCHAN,HP(LCHNV),SRFCHAN1,H1P(LCHNV)  
                  WRITE(1,8002)IHOST,JHOST,ISCDRY(LHOST),  
     &                SRFHOST,HP(LHOST),SRFHOST1,H1P(LHOST)  
                  WRITE(1,8003)QCHANV(NMD),QCHANVT(NMD),CCCCHU(NMD)
     &                ,CCCCHV(NMD)  
                ENDIF  
                WRITE(1,8004)  
              ENDDO  
              CLOSE(1)  
            ENDIF  
          ENDIF  
        ENDIF  
      ENDIF  
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
        IF(ISTL.EQ.3)THEN  
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
      IF(N.EQ.NTS.AND.DEBUG)THEN  
        IF(MDCHH.GT.0)THEN  
          DO NMD=1,MDCHH  
            WRITE(8,8000)  
            LHOST=LMDCHH(NMD)  
            IHOST=IL(LHOST)  
            JHOST=JL(LHOST)  
            LCHNU=LMDCHU(NMD)  
            LCHNV=LMDCHV(NMD)  
C  
C         X-DIRECTION CHANNEL  
C  
            IF(MDCHTYP(NMD).EQ.1)THEN  
              ICHNU=IL(LCHNU)  
              JCHNU=JL(LCHNU)  
              SRFCHAN=HP(LCHNU)+BELV(LCHNU)  
              SRFHOST=HP(LHOST)+BELV(LHOST)  
              SRFCHAN1=H1P(LCHNU)+BELV(LCHNU)  
              SRFHOST1=H1P(LHOST)+BELV(LHOST)  
              WRITE(8,8001)N,NMD,MDCHTYP(NMD),ICHNU,JCHNU,ISCDRY(LCHNU),  
     &            SRFCHAN,HP(LCHNU),P1(LCHNU),H1P(LCHNU)  
              WRITE(8,8002)IHOST,JHOST,ISCDRY(LHOST),  
     &            SRFHOST,HP(LHOST),P1(LHOST),H1P(LHOST)  
              WRITE(8,8003)QCHANU(NMD),QCHANUT(NMD),CCCCHU(NMD),
     &            CCCCHV(NMD)  
            ENDIF  
C  
C         Y-DIRECTION CHANNEL  
C  
            IF(MDCHTYP(NMD).EQ.2)THEN  
              ICHNV=IL(LCHNV)  
              JCHNV=JL(LCHNV)  
              SRFCHAN=HP(LCHNV)+BELV(LCHNV)  
              SRFHOST=HP(LHOST)+BELV(LHOST)  
              SRFCHAN1=H1P(LCHNV)+BELV(LCHNV)  
              SRFHOST1=H1P(LHOST)+BELV(LHOST)  
              WRITE(8,8001)N,NMD,MDCHTYP(NMD),ICHNV,JCHNV,ISCDRY(LCHNV),  
     &            SRFCHAN,HP(LCHNV),SRFCHAN1,H1P(LCHNV)  
              WRITE(8,8002)IHOST,JHOST,ISCDRY(LHOST),  
     &            SRFHOST,HP(LHOST),SRFHOST1,H1P(LHOST)  
              WRITE(8,8003)QCHANV(NMD),QCHANVT(NMD),CCCCHU(NMD),
     &            CCCCHV(NMD)  
            ENDIF  
            WRITE(8,8004)  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  CHECK FOR NEGATIVE DEPTHS  
C  
      IF(ISNEGH.GE.1)CALL NEGDEP(QCHANUT,QCHANVT,2)  
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
 8001 FORMAT(I7,5I5,4E13.4)  
 8002 FORMAT(17X,3I5,4E13.4)  
 8003 FORMAT(32X,4E13.4)  
 8000 FORMAT('    N    NMD  MTYP   I    J  IDRY      P           H',  
     &    '           P1           H1')  
 8004 FORMAT('                                     QCHANU',  
     &    '       QCHANUT      CCCCHU       CCCCHV ')  
C  
C **  CALCULATE THE EXTERNAL DIVERGENCE  
C  
      IF(ISDIVEX.EQ.1)THEN  
        DIVEXMX=0.  
        DIVEXMN=1000000.  
        DO L=2,LA  
          IF(SPB(L).NE.0)THEN  
            LN=LNC(L)  
            DIVEX=SPB(L)*(DXYP(L)*(HP(L)-H1P(L))*DELTI  
     &          +0.5*(UHDYE(L+1)+UHDY1E(L+1)-UHDYE(L)-UHDY1E(L)  
     &          +VHDXE(LN)+VHDX1E(LN)-VHDXE(L)-VHDX1E(L))-QSUME(L)  
     &          +RIFTR(L)+EVAPSW(L))  
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
      ISTL=2  
      IF(ISDRY.GE.1.AND.ISTL.EQ.3)THEN  
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

