      SUBROUTINE CALIMP2T  
C  
C **  SUBROUTINE CALEXP CALCULATES IMPLICIT MOMENTUM EQUATION  
C **  CORIOLIS AND CURVATURE TERMS FOR 1/2 STEP PREDICTOR  
C CHANGE RECORD  
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
      ENDIF  
      DO K=1,KC  
        TVAR1W(1,K)=0.  
        TVAR1S(1,K)=0.  
        TVAR1E(1,K)=0.  
        TVAR1N(1,K)=0.  
        TVAR2W(1,K)=0.  
        TVAR2S(1,K)=0.  
        TVAR2E(1,K)=0.  
        TVAR2N(1,K)=0.  
        TVAR1W(LC,K)=0.  
        TVAR1S(LC,K)=0.  
        TVAR1E(LC,K)=0.  
        TVAR1N(LC,K)=0.  
        TVAR2W(LC,K)=0.  
        TVAR2S(LC,K)=0.  
        TVAR2E(LC,K)=0.  
        TVAR2N(LC,K)=0.  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          TVAR1W(L,K)=0.  
          TVAR1S(L,K)=0.  
          TVAR1E(L,K)=0.  
          TVAR1N(L,K)=0.  
          TVAR2W(L,K)=0.  
          TVAR2S(L,K)=0.  
          TVAR2E(L,K)=0.  
          TVAR2N(L,K)=0.  
        ENDDO  
      ENDDO  
      TVAR3W(1)=0.  
      TVAR3S(1)=0.  
      TVAR3E(1)=0.  
      TVAR3N(1)=0.  
      TVAR3W(LC)=0.  
      TVAR3S(LC)=0.  
      TVAR3E(LC)=0.  
      TVAR3N(LC)=0.  
      DO ND=1,NDM  
        LF=2+(ND-1)*LDM  
        LL=LF+LDM-1  
        DO L=LF,LL  
          TVAR3W(L)=0.  
          TVAR3S(L)=0.  
          TVAR3E(L)=0.  
          TVAR3N(L)=0.  
        ENDDO  
      ENDDO  
C  
C **  INITIALIZE EXTERNAL CORIOLIS-CURVATURE AND ADVECTIVE FLUX TERMS  
C  
      DO L=1,LC
        FCAXE(L)=0.  
        FCAYE(L)=0.  
C PMC          FCAX1E(L)=0.  
C PMC          FCAY1E(L)=0.  
        FXE(L)=0.  
        FYE(L)=0.  
      ENDDO  
C  
C **  CALCULATE TIME LEVEL TIME LEVEL N INTERNAL STRESSES  
C **  AND WAVE INDUCED CURRENT FORCINGS  
C **  CALCULATE FRACTIONAL STEP WITH STRESSES AND WV FORCES  
C  
      ! *** Surface & Bottom Stresses
      DO ND=1,NDM  
        LF=2+(ND-1)*LDM  
        LL=LF+LDM-1  
        DO L=LF,LL  
          TVAR2W(L,1)=TBX(L)  
          TVAR2S(L,1)=TBY(L)  
          TVAR2E(L,KC)=TSX(L)  
          TVAR2N(L,KC)=TSY(L)  
        ENDDO  
      ENDDO  
      ! *** Vertical Eddy Viscosity Impacts
      DO K=1,KS  
        TMPVAL=DZIG(K)  
        DO L=2,LA  
          TVAR2E(L,K)=TMPVAL*(U(L,K+1)-U(L,K))/AVUI(L,K)  
          TVAR2N(L,K)=TMPVAL*(V(L,K+1)-V(L,K))/AVVI(L,K)  
        ENDDO  
      ENDDO  

      DO K=2,KC  
        DO L=2,LA  
          TVAR2W(L,K)=TVAR2E(L,K-1) 
          TVAR2S(L,K)=TVAR2N(L,K-1)  
        ENDDO  
      ENDDO
  
      DO K=1,KC  
        DO L=2,LA  
          TVAR1W(L,K)=DYIU(L)*UHDY(L,K)  
          TVAR1S(L,K)=DXIV(L)*VHDX(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        TMPVAL=DELTD2*DZIC(K)  
        DO L=2,LA  
          ! *** Vertical Shear in the X direction
          TVAR1W(L,K)=TVAR1W(L,K)+TMPVAL*(TVAR2E(L,K)-TVAR2W(L,K))  
          ! *** Vertical Shear in the Y direction
          TVAR1S(L,K)=TVAR1S(L,K)+TMPVAL*(TVAR2N(L,K)-TVAR2S(L,K))  
        ENDDO  
      ENDDO  
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
        WVFACT=DELTD2*WVFACT  
C
        DO K=1,KC  
          DO L=2,LA  
            TVAR1W(L,K)=TVAR1W(L,K)+WVFACT*SAAX(L)*DXYIU(L)*FXWAVE(L,K)
            TVAR1S(L,K)=TVAR1S(L,K)+WVFACT*SAAY(L)*DXYIV(L)*FYWAVE(L,K)  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C *** DSLLC END BLOCK  
C  
      DO K=1,KC  
        DO L=2,LA  
          TVAR1W(L,K)=SUB(L)*TVAR1W(L,K)  
          TVAR1S(L,K)=SVB(L)*TVAR1S(L,K)  
        ENDDO  
      ENDDO  
C  
C **  ADD PRESSURE GRADIENT TERMS  
C **  HRU=SUB*HMU*DYU/DXU & HRV=SVB*HMV*DXV/DYV  
C  
      DO ND=1,NDM  
        LF=2+(ND-1)*LDM  
        LL=LF+LDM-1  
        DO L=LF,LL  
          TVAR1E(L,KC)=DZC(KC)*B(L,KC)  
        ENDDO  
      ENDDO  
      DO K=KS,1,-1  
        DO L=2,LA  
          TVAR1E(L,K)=TVAR1E(L,K+1)+DZC(K)*B(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          TVAR1E(L,K)=TVAR1E(L,K)-0.5*DZC(K)*B(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          TVAR1N(L,K)=TVAR1E(L,K)+0.5*(Z(K)+Z(K-1))*B(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          TVAR1N(L,K)=GP*TVAR1N(L,K)  
          TVAR1E(L,K)=GP*TVAR1E(L,K)  
        ENDDO  
      ENDDO  
      DO ND=1,NDM  
        LF=2+(ND-1)*LDM  
        LL=LF+LDM-1  
        DO L=LF,LL  
          TVAR3W(L)=DELTD2*SUB(L)*HU(L)*DXIU(L)  
          TVAR3S(L)=DELTD2*SVB(L)*HV(L)*DYIV(L)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          LS=LSC(L)  
          TVAR1W(L,K)=TVAR1W(L,K)-TVAR3W(L)*( P(L)-P(L-1) )  
          TVAR1S(L,K)=TVAR1S(L,K)-TVAR3S(L)*( P(L)-P(LS ) )  
        ENDDO  
      ENDDO  
      DO ND=1,NDM  
        LF=2+(ND-1)*LDM  
        LL=LF+LDM-1  
        DO L=LF,LL  
          TVAR3W(L)=DELT*SUB(L)*HU(L)*DXYIU(L)*SBX(L)  ! *** PMC H1U --> HU
          TVAR3S(L)=DELT*SVB(L)*HV(L)*DXYIV(L)*SBY(L)  ! *** PMC H1V --> HV
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          LS=LSC(L)  
          TVAR1W(L,K)=TVAR1W(L,K)-TVAR3W(L)*(  
     &        +HU(L)*(TVAR1E(L,K)-TVAR1E(L-1,K))  ! *** PMC H1U --> HU
     &        +0.5*GP*(B(L,K)+B(L-1,K))*(BELV(L)-BELV(L-1))  
     &        +0.5*(TVAR1N(L,K)+TVAR1N(L-1,K))*(HP(L)-HP(L-1)) )  
          TVAR1S(L,K)=TVAR1S(L,K)-TVAR3S(L)*(  
     &        +HV(L)*(TVAR1E(L,K)-TVAR1E(LS ,K))   ! *** PMC H1V --> HV
     &        +0.5*GP*(B(L,K)+B(LS ,K))*(BELV(L)-BELV(LS ))  
     &        +0.5*(TVAR1N(L,K)+TVAR1N(LS ,K))*(HP(L)-HP(LS )) )  
        ENDDO  
      ENDDO  
C  
C **  ADVECTION STEP FOR X MOMENTUM EQUATION  
C **  SELECT ADVECTIVE FLUX FORM  
C  
      DO K=1,KC  
        UHDY2(1,K)=0.  
        VHDX2(1,K)=0.  
        UHDY2(LC,K)=0.  
        VHDX2(LC,K)=0.  
        FUHU(1,K)=0.  
        FUHU(LC,K)=0.  
        FVHU(1,K)=0.  
        FVHU(LC,K)=0.  
      ENDDO  
      DO K=0,KC  
        W2(1,K)=0.  
        FWU(1,K)=0.  
        W2(LC,K)=0.  
        FWU(LC,K)=0.  
      ENDDO  
      DO ND=1,NDM  
        LF=2+(ND-1)*LDM  
        LL=LF+LDM-1  
        DO L=LF,LL  
          W2(L,0)=0.  
          FWU(L,0)=0.  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LC  
          UHDY2(L,K)=0.  
          VHDX2(L,K)=0.  
          W2(L,K)=0.  
          FUHU(L,K)=0.  
          FVHU(L,K)=0.  
          FWU(L,K)=0.  
        ENDDO  
      ENDDO  
C  
C **  ADVECTIVE FLUX HALF STEP  
C **  CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE WITH ADVECTION  
C **  AT N AND ADVECTED FIELD AT N  
C **  UHDY2(L,K) IS DYU*U ON POS X U(L) MOM CV FACE (AT B OR P POINT)  
C **  VHDX2(L,K) IS DXV*V ON NEG Y U(L) MOM CV FACE (AT CORNER POINT)  
C **  W2(L,K)    IS DXYP*W/H ON TOP U(L) MOM CV FACE  
C ** SMOLARKIEZC AND MARGOLIN FORM  
C ** STANDARD FORM  
C  
      DO K=1,KC  
        DO L=2,LA  
C PMC         UHDY2(L,K)=0.25*STCUV(L)*( (UHDY(L  ,K)+UHDY(L  ,K)
          UHDY2(L,K)=0.25*( (UHDY(L  ,K)+UHDY(L  ,K)) 
     &        +(UHDY(L+1,K)+UHDY(L+1,K)) )  
          VHDX2(L,K)=0.25*( (VHDX(L  ,K)+VHDX(L  ,K))  
     &        +(VHDX(L-1,K)+VHDX(L-1,K)) )  
        ENDDO  
      ENDDO
      ! *** DSLLC BEGIN BLOCK  
      IF(ITRICELL.GT.0)THEN
        DO K=1,KC  
          DO L=2,LA  
            UHDY2(L,K)=UHDY2(L,K)*STCUV(L)
            VHDX2(L,K)=VHDX2(L,K)*STCUV(L)
          ENDDO  
        ENDDO
      ENDIF
      ! *** DSLLC END BLOCK  
      DO K=1,KS  
        DO L=2,LA  
          W2(L,K)=0.25*( DXYP(L  )*(W(L  ,K)+W(L  ,K))  
     &        +DXYP(L-1)*(W(L-1,K)+W(L-1,K)) )  
        ENDDO  
      ENDDO  
C  
C ** SMOLARKIEZC AND MARGOLIN FORM  
C ** STANDARD FORM  
C  
      DO K=1,KC  
        DO L=2,LA  
          FUHU(L,K)=MAX(UHDY2(L,K),0.)*U(L     ,K)  
     &        +MIN(UHDY2(L,K),0.)*U(L+1   ,K)  
          FVHU(L,K)=MAX(VHDX2(L,K),0.)*U(LSC(L),K)  
     &        +MIN(VHDX2(L,K),0.)*U(L     ,K)  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO L=2,LA  
          FWU(L,K)=MAX(W2(L,K),0.)*U(L,K)  
     &        +MIN(W2(L,K),0.)*U(L,K+1)  
        ENDDO  
      ENDDO  
C  
C **  ADVANCE U ADVECTION 1/2 TIME STEP  
C  
      DO K=1,KC  
        DO L=2,LA  
          FX(L,K)=FUHU(L,K)-FUHU(L-1,K)+FVHU(LNC(L),K)-FVHU(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          FX(L,K)=FX(L,K)+(FWU(L,K)-FWU(L,K-1))*DZIC(K)  
        ENDDO  
      ENDDO  
      TMPVAL=DELTD2*SNLT  
      DO K=1,KC  
        DO L=2,LA  
          TVAR1W(L,K)=TVAR1W(L,K)-TMPVAL*SUB(L)*DXYIU(L)*SAAX(L)*FX(L,K)  
        ENDDO  
      ENDDO  
C  
C **  ADVECTION STEP FOR Y MOMENTUM EQUATION  
C **  SELECT ADVECTIVE FLUX FORM  
C  
      DO K=1,KC  
        UHDY2(1,K)=0.  
        VHDX2(1,K)=0.  
        UHDY2(LC,K)=0.  
        VHDX2(LC,K)=0.  
        FUHV(1,K)=0.  
        FUHV(LC,K)=0.  
        FVHV(1,K)=0.  
        FVHV(LC,K)=0.  
      ENDDO  
      DO K=0,KC  
        W2(1,K)=0.  
        FWV(1,K)=0.  
        W2(LC,K)=0.  
        FWV(LC,K)=0.  
      ENDDO  
      DO ND=1,NDM  
        LF=2+(ND-1)*LDM  
        LL=LF+LDM-1  
        DO L=LF,LL  
          W2(L,0)=0.  
          FWV(L,0)=0.  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LC  
          UHDY2(L,K)=0.  
          VHDX2(L,K)=0.  
          W2(L,K)=0.  
          FUHV(L,K)=0.  
          FVHV(L,K)=0.  
          FWV(L,K)=0.  
        ENDDO  
      ENDDO  
C  
C **  TWO TIME LEVEL STEP  
C **  CALCULATE ADVECTIVE FLUXES BY UPWIND DIFFERENCE WITH ADVECTION  
C **  AVERAGED BETWEEN (N) AND (N+1) AND ADVECTED FIELD AT N  
C **  UHDY2(L,K) IS DYU*U ON NEG X V(L) MOM CV FACE (CORNER POINT)  
C **  VHDX2(L,K) IS DXV*V ON POS Y V(L) MOM CV FACE (B 0R P POINT)  
C **  W2(L,K)    IS DXYP*W/H ON TOP V MOM CV FACE  
C ** SMOLARKIEWCZ AND MARGOLIN FORM  
C ** STANDARD FORM  
C  
      DO K=1,KC  
        DO L=2,LA  
          LS=LSC(L)  
          LN=LNC(L)  
          UHDY2(L,K)=0.25*( (UHDY(L ,K)+UHDY(L ,K))  
     &        +(UHDY(LS,K)+UHDY(LS,K)) )  
          VHDX2(L,K)=0.25*STCUV(L)*( (VHDX(L ,K)+VHDX(L ,K))  
     &        +(VHDX(LN,K)+VHDX(LN,K)) )  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO L=2,LA  
          LS=LSC(L)  
          W2(L,K)=0.25*( DXYP(L )*(W(L ,K)+W(L ,K))  
     &        +DXYP(LS)*(W(LS,K)+W(LS,K)) )  
        ENDDO  
      ENDDO  
C  
C ** SMOLARKIEWCZ AND MARGOLIN FORM  
C ** STANDARD FORM  
C  
      DO K=1,KC  
        DO L=2,LA  
          FUHV(L,K)=MAX(UHDY2(L,K),0.)*V(L-1   ,K)  
     &        +MIN(UHDY2(L,K),0.)*V(L     ,K)  
          FVHV(L,K)=MAX(VHDX2(L,K),0.)*V(L     ,K)  
     &        +MIN(VHDX2(L,K),0.)*V(LNC(L),K)  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO L=2,LA  
          FWV(L,K)=MAX(W2(L,K),0.)*V(L,K)  
     &        +MIN(W2(L,K),0.)*V(L,K+1)  
        ENDDO  
      ENDDO  
C  
C **  ADVANCE V ADVECTION 1/2 TIME STEP  
C  
      DO K=1,KC  
        DO L=2,LA  
          FY(L,K)=FUHV(L+1,K)-FUHV(L,K)+FVHV(L,K)-FVHV(LSC(L),K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          FY(L,K)=FY(L,K)+(FWV(L,K)-FWV(L,K-1))*DZIC(K)  
        ENDDO  
      ENDDO  
      TMPVAL=DELTD2*SNLT  
      DO K=1,KC  
        DO L=2,LA  
          TVAR1S(L,K)=TVAR1S(L,K)-TMPVAL*SVB(L)*DXYIV(L)*SAAY(L)*FY(L,K)  
        ENDDO  
      ENDDO  
C  
C **  ADVANCE CONTINUITY EQUATION 1/2 STEP  
C **  INITIALIZE MID TIME LEVEL VELOCITY  
C **  BEGIN CORIOLIS AND CURVATURE ACCELERATION ITERATION  
C **  CALCULATE CORIOLIS AND CURVATURE PARAMETER  
C  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L)  
          CAC(L,K)=( FCORC(L)*DXYP(L)  
     &        +0.5*SNLT*(V(LN ,K)+V(L,K))*DYDI(L)  
     &        -0.5*SNLT*(U(L+1,K)+U(L,K))*DXDJ(L) )*HP(L)  
        ENDDO  
      ENDDO  
C  
C **  CALCULATE CORIOLIS-CURVATURE AND ADVECTIVE ACCELERATIONS  
C  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L)  
          LS=LSC(L)  
          LNW=LNWC(L)  
          LSE=LSEC(L)  
          FCAX(L,K)=0.25*SCAX(L)*(CAC(L,K)*(V(LN,K)+V(L,K))  
     &        +CAC(L-1,K)*(V(LNW,K)+V(L-1,K)))  
          FCAY(L,K)=0.25*SCAY(L)*(CAC(L,K)*(U(L+1,K)+U(L,K))  
     &        +CAC(LS,K)*(U(LSE,K)+U(LS,K)))  
        ENDDO  
      ENDDO  
C  
C **  CALCULATE MID TIME LEVEL TRANSPORT AND VELOCTIY  
C  
      DO K=1,KC  
        DO L=2,LA  
          TVAR1W(L,K)=TVAR1W(L,K)+DELTD2*SUB(L)*DXYIU(L)*FCAX(L,K)  
          TVAR1S(L,K)=TVAR1S(L,K)-DELTD2*SVB(L)*DXYIV(L)*FCAY(L,K)  
        ENDDO  
      ENDDO  
C  
C **  ADD HORIZONTAL MOMENTUN DIFFUSION TO ADVECTIVE ACCELERATIONS  
C **  CALCULATE EXTERNAL ACCELERATIONS  
C  
      DO K=1,KC  
        DO L=2,LA  
          FXE(L)=FXE(L)+DYU(L)*TVAR1W(L,K)*DZC(K)  
          FYE(L)=FYE(L)+DXV(L)*TVAR1S(L,K)*DZC(K)  
        ENDDO  
      ENDDO  
C  
C **  ADD REMAINING SURFACE AND BOTTOM STRESS  
C  
      DO L=2,LA  
        FXE(L)=FXE(L)+DELTD2*SUB(L)*DYU(L)*(TSX(L)-TBX1(L))  ! *** PMC WHY TBX1 NOT TBX ?
        FYE(L)=FYE(L)+DELTD2*SVB(L)*DXV(L)*(TSY(L)-TBY1(L))  
      ENDDO  
C  
C **  CALCULATE EXPLICIT INTERNAL U AND V SHEAR EQUATION TERMS  
C **  CALCULATE EXPLICIT INTERNAL BUOYANCY FORCINGS CENTERED AT N FOR  
C **  THREE TIME LEVEL STEP AND AT (N+1/2) FOR TWO TIME LEVEL STEP  
C **  SBX=SBX*0.5*DYU & SBY=SBY*0.5*DXV  
C  
      DO K=1,KS  
        DO L=2,LA  
          LS=LSC(L)  
          FBBX(L,K)=SBX(L)*GP*HU(L)*  
     &        ( HU(L)*( (B(L,K+1)-B(L-1,K+1))*DZC(K+1)  
     &        +(B(L,K)-B(L-1,K))*DZC(K) )  
     &        -(B(L,K+1)-B(L,K)+B(L-1,K+1)-B(L-1,K))*  
     &        (BELV(L)-BELV(L-1)+Z(K)*(HP(L)-HP(L-1))) )  
          FBBY(L,K)=SBY(L)*GP*HV(L)*  
     &        ( HV(L)*( (B(L,K+1)-B(LS,K+1))*DZC(K+1)  
     &        +(B(L,K)-B(LS,K))*DZC(K) )  
     &        -(B(L,K+1)-B(L,K)+B(LS,K+1)-B(LS,K))*  
     &        (BELV(L)-BELV(LS)+Z(K)*(HP(L)-HP(LS))) )  
        ENDDO  
      ENDDO  
 1111 FORMAT(2I5,2X,8E12.4)  
C  
C **  CALCULATE EXPLICIT INTERNAL U AND V SHEAR EQUATION TERMS  
C      ELSE  
C  
      DO K=1,KS  
        RCDZF=CDZF(K)  
        DO L=2,LA  
          DU(L,K)=RCDZF*( 2.*(TVAR1W(L,K+1)-TVAR1W(L,K))*DELTI  
     &        +DXYIU(L)*FBBX(L,K) )  
          DV(L,K)=RCDZF*( 2.*(TVAR1S(L,K+1)-TVAR1S(L,K))*DELTI  
     &        +DXYIV(L)*FBBY(L,K) )  
        ENDDO  
      ENDDO  

      IF(NWSER.GT.0)THEN
        DO L=2,LA
          DU(L,KS)=DU(L,KS)-CDZU(KS)*TSX(L)  
          DV(L,KS)=DV(L,KS)-CDZU(KS)*TSY(L)  
        ENDDO  
      ENDIF
C
      RETURN  
      END  

