      SUBROUTINE WAVESXY  
C  
C CHANGE RECORD  
C  
      USE GLOBAL  
C  
C **  INPUT WAVE INFORMATION  
C *** DSLLC BEGIN BLOCK  
C  
      IF(JSWAVE.EQ.1) GOTO 100  
      JSWRPH=1  
      DO L=1,LC  
        HMPW(L)=0.  
        HMCW(L)=0.  
        HMUW(L)=0.  
        HMVW(L)=0.  
        WVWHA(L)=0.  
        WVKHP(L)=0.  
        WVKHC(L)=0.  
        WVKHU(L)=0.  
        WVKHV(L)=0.  
        WVTMP1(L)=0.  
        WVTMP2(L)=0.  
        WVTMP3(L)=0.  
        WVTMP4(L)=0.  
        UWVMAG(L)=0.  
        VWVMAG(L)=0.  
        WVENEP(L)=0.  
        UWVSQ(L)=0.  
        QQWC(L)=1.E-12  
        QQWCR(L)=1.E-12  
        QQWV1(L)=1.E-12  
        QQWV2(L)=1.E-12  
        QQWV3(L)=1.E-12  
        WACCWE(L)=0.  
      ENDDO  
      DO K=1,KC  
        DO L=1,LC  
          WVHUU(L,K)=0.  
          WVHVV(L,K)=0.  
          WVHUV(L,K)=0.  
          WVPP(L,K)=0.  
          WVPU(L,K)=0.  
          WVPV(L,K)=0.  
          WVDISP(L,K)=0.  
          FXWAVE(L,K)=0.  
          FYWAVE(L,K)=0.  
        ENDDO  
      ENDDO  
      PRINT *,'WAVE: WAVE.INP'
      OPEN(1,FILE='WAVE.INP',STATUS='UNKNOWN')  
      DO NSKIP=1,31  
        READ(1,1,IOSTAT=ISO)  
        IF(ISO.GT.0) GOTO 1081  
      ENDDO  
      READ(1,*,IOSTAT=ISO)NWVDAT,WVPRD,ISWCBL,ISWRSR,ISWRSI,NWUPDT,  
     &    NTSWV,WVDISV,WVDISH,WVLSH,WVLSX,ISWVSD,ISDZBR  
      IF(ISO.GT.0) GOTO 1082  
      WVFRQ=2.*PI/WVPRD  
      NWCUNT=NWUPDT-1  
      JSWAVE=1  
      RSWRSR=FLOAT(ISWRSR)  
      RSWRSI=FLOAT(ISWRSI)  
      DO NWV=1,NWVDAT  
        READ(1,*,IOSTAT=ISO)IWV,JWV,ENETMP,SXXTMP,SYYTMP,SXYTMP,  
     &      DISPTMP,WANGLE  
        IF(ISO.GT.0) GOTO 1083  
        L=LIJ(IWV,JWV)  
        ! INPUT UNITS ARE M3/S2  
        WVENEP(L)=ENETMP  
        ! INPUT UNITS KG/S^2  DIVIDE BY DENSITY SO FINAL UNITS ARE M3/S2  
        WVHUU(L,KC)=SXXTMP/1000.  
        WVHVV(L,KC)=SYYTMP/1000.  
        WVHUV(L,KC)=SXYTMP/1000.  
        ! INPUT UNITS ARE M3/S3  
        WVDISP(L,KC)=DISPTMP  
        ! INPUT UNITS DEGREES, CONVERT TO RADIANS  
        WACCWE(L)=WANGLE/180.*PI  
      ENDDO  
C  
C **  DETERMINE ORTIBAL AMPLITUDES  
C  
      DO L=2,LA  
        LN=LNC(L)  
        LE=L+1  
        WVWHA(L)=SQRT(2.*WVENEP(L)/G)  
        HMPW(L)=HMP(L)+WVWHA(L)  
      ENDDO  
C  
C PMC COMPUTE THE DERIVED WATER SURFACES  
C  
      DO L=2,LA  
        LS=LSC(L)  
        LW=L-1  
        LSW=LSWC(L)  
        ! HEIGHT @ CORNER  
        HMCW(L)=0.25*(HMPW(L)+HMPW(LW)+HMPW(LS)+HMPW(LSW))  
        ! HEIGHT U AND V FACE  
        HMUW(L)=0.5*(DXYP(L)*HMPW(L)+DXYP(LW)*HMPW(LW))  
     &      /(DXU(L)*DYU(L))  
        HMVW(L)=0.5*(DXYP(L)*HMPW(L)+DXYP(LS)*HMPW(LS))  
     &      /(DXV(L)*DYV(L))  
      ENDDO  
      CLOSE(1)  
C  
C **  INITIALIZE VERTICAL DISTRIBUTION OF WAVE DISSIPATION AS SOURCE  
C **  TO VERTICAL TKE CLOSURE  
C  
      IF(KC.EQ.2)THEN  
        WVDTKEM(1)=WVDISV  
        WVDTKEP(1)=WVDISV  
      ENDIF  
      IF(KC.EQ.3)THEN  
        WVDTKEM(1)=WVDISV  
        WVDTKEP(1)=0.5*WVDISV  
        WVDTKEM(2)=0.5*WVDISV  
        WVDTKEP(2)=WVDISV  
      ENDIF  
      IF(KC.GE.4)THEN  
        WVDTKEM(1)=WVDISV  
        WVDTKEP(1)=0.5*WVDISV  
        WVDTKEM(KS)=0.5*WVDISV  
        WVDTKEP(KS)=WVDISV  
        DO K=2,KS-1  
          WVDTKEM(K)=0.5*WVDISV  
          WVDTKEP(K)=0.5*WVDISV  
        ENDDO  
      ENDIF  
C  
C **  GENERATE WAVE TABLE  
C  
      HMXTMP=0.  
      DO L=2,LA  
        HMXTMP=MAX(HMXTMP,HMPW(L))  
      ENDDO  
      FKHMAX=1.5*GI*WVFRQ*WVFRQ*HMXTMP  
      RKHTMP=0.001  
   10 CONTINUE  
      FKHTMP=RKHTMP*TANH(RKHTMP)  
      IF(FKHTMP.LT.FKHMAX)THEN  
        RKHTMP=2.*RKHTMP  
        GOTO 10  
      ELSE  
        DKH=RKHTMP/1000.  
      ENDIF  
      RKHTAB(1)=0.  
      FUNKH(1)=0.  
      DO NKH=2,1001  
        RKHTAB(NKH)=RKHTAB(NKH-1)+DKH  
        FUNKH(NKH)=RKHTAB(NKH)*TANH(RKHTAB(NKH))  
      ENDDO  
      IF(DEBUG)THEN
        OPEN(1,FILE='WVTAB.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='WVTAB.OUT',STATUS='UNKNOWN')  
        DO NKH=1,1001  
          WRITE(1,111)RKHTAB(NKH),FUNKH(NKH)  
        ENDDO  
        CLOSE(1)  
      ENDIF
      GOTO 100  
 1081 WRITE(6,1091)  
      STOP  
 1082 WRITE(6,1092)  
      STOP  
 1083 WRITE(6,1093) NWV  
      STOP  
 1084 WRITE(6,1094) NWV  
      STOP  
    1 FORMAT(120X)  
 1091 FORMAT('  READ ERROR ON FILE WAVE.INP , HEADER')  
 1092 FORMAT('  READ ERROR ON FILE WAVE.INP , 1ST DATA')  
 1093 FORMAT('  READ ERROR ON FILE WAVE.INP , 2ND DATA, NWV = ',I5)  
 1094 FORMAT('  READ ERROR ON FILE WAVE.INP , 3RD DATA, NWV = ',I5)  
  111 FORMAT(2E14.4)  
C  
C **  INITIALIZE OR UPDATE WAVE FIELD  
C  
  100 CONTINUE  
      NWCUNT=NWCUNT+1  
      IF(NWCUNT.LT.NWUPDT) RETURN  
      NWCUNT=0  
      GDFRQ=G/WVFRQ  
C  
C **  DISTRIBUTE WVHUU, WVHVV, AND WVDISP OVER DEPTH  
C **  COMPUTE CELL CORNER QUANTITY WVHUV  
C  
      DO L=1,LC  
        HFFDG=GI*WVFRQ*WVFRQ*HMPW(L)  
        WVKHP(L)=VALKH(HFFDG)  
      ENDDO  
C  
C *** DISTRIBUTE VALUES ACROSS KC  
C  
      DO L=2,LA  
        RKHM1=WVKHP(L)  
        RKHM2=2.*WVKHP(L)  
        SINH2=SINH(2.*WVKHP(L))  
        COSH3=COSH(WVKHP(L))  
        RATIO=0.5+(WVKHP(L)/SINH2)  
        DO K=1,KC  
          ZTOP=Z(K)  
          ZBOT=Z(K-1)  
          ! *** START Moved to after ZTOP is defined (2009_09_03)
          SINHTOP=SINH(RKHM2*ZTOP)  
          SINHBOT=SINH(RKHM2*ZBOT)  
          COSHTOP=COSH(RKHM1*ZTOP)  
          COSHBOT=COSH(RKHM1*ZBOT)  
          ! *** END Moved to after ZTOP is defined (2009_09_03)
          TMPVAL=(RKHM2*(ZTOP-ZBOT)+SINHTOP-SINHBOT)/(RKHM2+SINH2)  
          ! *** APPLY FACTOR  
          WVHUU(L,K)=TMPVAL*WVHUU(L,KC)  
          WVHVV(L,K)=TMPVAL*WVHVV(L,KC)  
          WVHUV(L,K)=TMPVAL*WVHUV(L,KC)  
          WVDISP(L,K)=TMPVAL*WVDISP(L,KC)  
          TMPP1=-0.5*(ZTOP-ZBOT)+(ZTOP*COSHTOP-ZBOT*COSHBOT)/COSH3  
          TMPP2=(RATIO-1.)*(SINHTOP-SINHBOT-2.*(ZTOP-ZBOT))  
     &        /(SINH2-2.)  
          ! *** LIMIT RANGE WHEN WVKHP~0.72  
          IF(ABS(TMPP1).GT.0.5)THEN  
            TMPP1=SIGN(0.5,TMPP1)  
          ENDIF  
          IF(ABS(TMPP2).GT.0.5)THEN  
            TMPP2=SIGN(0.5,TMPP2)  
          ENDIF  
          WVPP(L,K)=WVENEP(L)*(TMPP1+TMPP2)  
        ENDDO  
      ENDDO  
C  
C **  INITIALIZE WAVE-CURRENT BOUNDARY LAYER MODEL CALCULATING  
C **  THE WAVE TURBULENT INTENSITY, QQWV  
C **  AND SQUARED HORIZONTAL WAVE OBRITAL VELOCITY MAGNITUDE  
C  
      DO L=2,LA  
        AEXTMP=WVWHA(L)/SINH(WVKHP(L))  
        ! *** SQUARED HORIZONTAL WAVE OBRITAL VELOCITY MAGNITUDE  
        UWVSQ(L)=AEXTMP*AEXTMP*WVFRQ*WVFRQ  
        IF(UWVSQ(L).GT.1.E-7)THEN  
          IF(ZBR(L).LE.0.)THEN  
            ! ** TURBULENT SMOOTH WAVE BOUNDARY LAYER  
            IF(ISMUD.GE.1)THEN  
              VISMUDD=CSEDVIS(SED(L,1,1))  
            ELSE  
              VISMUDD=1.E-6  
            ENDIF  
            REYWAVE=UWVSQ(L)*AEXTMP/VISMUDD  
            CDTMP=0.012/(REYWAVE**0.123)  
          ELSE  
            ! ** TURBULENT ROUGH WAVE BOUNDARY LAYER  
            IF(WVWHA(L).GT.0.0)THEN  
              TMPVAL=30.*ZBR(L)/AEXTMP  
              TMPVAL=5.5*(TMPVAL**0.2)-6.3  
              CDTMP=0.5*EXP(TMPVAL)  
            ELSE  
              CDTMP=0.0  
            ENDIF  
          ENDIF  
          QQWV1(L)=CDTMP*UWVSQ(L)*UWVSQ(L)  
          ZBRE(L)=ZBR(L)  
          IF(QQ(L,0).GT.0.)THEN  
            TMPVAL=UWVSQ(L)*SQRT( AEXTMP/(30.*ZBR(L)) )  
            USTARC=SQRT(QQ(L,0)/CTURB2)  
            TMPVAL=TMPVAL/USTARC  
            ZBRE(L)=ZBR(L)*(1.+0.19*TMPVAL)  
          ENDIF  
          CDRGTMP=(30.*ZBRE(L)/AEXTMP)**0.2  
          CDRGTMP=5.57*CDRGTMP-6.13  
          CDRGTMP=EXP(CDRGTMP)  
          CDRGTMP=MIN(CDRGTMP,0.22)  
          TMPVAL=0.5*CDRGTMP*UWVSQ(L)  
          QQWV2(L)=CTURB2*TMPVAL  
        ELSE  
          QQWV1(L)=QQLMIN  
          QQWV2(L)=QQLMIN  
        ENDIF  
      ENDDO  
      IF(ISRESTI.NE.0)THEN  
        PRINT *,'WAVE: WVQWCP.INP'
        OPEN(1,FILE='WVQWCP.INP',STATUS='UNKNOWN')  
        DO L=2,LA  
          READ(1,*)IDUM,JDUM,QQWV1(L),QQWV2(L),QQWV2(L),QQWC(L),QQWCR(L)  
        ENDDO  
      ENDIF  
C  
C **  COMPUTE CELL FACE QUANTITIES WVPU,WVPV  
C  
      TMPVAL=0.5*WVFRQ*WVFRQ  
      DO L=2,LA  
        HFFDG=GI*WVFRQ*WVFRQ*HMUW(L)  
        WVKHU(L)=VALKH(HFFDG)  
        HFFDG=GI*WVFRQ*WVFRQ*HMVW(L)  
        WVKHV(L)=VALKH(HFFDG)  
      ENDDO  
      DO L=2,LA  
        LS=LSC(L)  
        WVTMP1(L)=SINH(WVKHU(L))  
        WVWHAUT=(WVWHA(L)+SUB(L)*WVWHA(L-1))/(1.+SUB(L))  
        WVTMP2(L)=TMPVAL*WVWHAUT*WVWHAUT  
     &      /(WVTMP1(L)*WVTMP1(L))  
        WVWHAVT=(WVWHA(L)+SVB(L)*WVWHA(LS ))/(1.+SVB(L))  
        WVTMP3(L)=SINH(WVKHV(L))  
        WVTMP4(L)=TMPVAL*WVWHAVT*WVWHAVT  
     &      /(WVTMP3(L)*WVTMP3(L))  
      ENDDO  
      DO K=1,KC  
        ZTOP=Z(K)  
        ZBOT=Z(K-1)  
        DO L=2,LA  
          SNHTOPU=SINH(WVKHU(L)*ZTOP)  
          SNHBOTU=SINH(WVKHU(L)*ZBOT)  
          SNHTOPV=SINH(WVKHV(L)*ZTOP)  
          SNHBOTV=SINH(WVKHV(L)*ZBOT)  
          TMPPU=(1.-ZTOP)*SNHTOPU*(ZTOP*WVTMP1(L)-SNHTOPU)  
     &        -(1.-ZBOT)*SNHBOTU*(ZBOT*WVTMP1(L)-SNHBOTU)  
          TMPPV=(1.-ZTOP)*SNHTOPV*(ZTOP*WVTMP3(L)-SNHTOPV)  
     &        -(1.-ZBOT)*SNHBOTV*(ZBOT*WVTMP3(L)-SNHBOTV)  
          WVPU(L,K)=WVTMP2(L)*TMPPU  
          WVPV(L,K)=WVTMP4(L)*TMPPV  
        ENDDO  
      ENDDO  
C  
C **  CALCULATE THE NET X AND  Y WAVE REYNOLDS STRESS FORCINGS  
C  
      DO K=1,KC  
        DZITMP=1./DZC(K)  
        DO L=2,LA  
          LS=LSC(L)  
          LN=LNC(L)  
          LNW=LNWC(L)  
          LSE=LSEC(L)  
          FXWAVE(L,K)=DZITMP*SUB(L)*SPB(L)  
     &          *( RSWRSI*(DYU(L)*(WVPP(L,K)-WVPP(L-1,K))  
     &          +DYU(L)*WVPU(L,K)*(HMPW(L)-HMPW(L-1)))  
     &          +RSWRSR*(DYP(L)*WVHUU(L,K)-DYP(L-1)*WVHUU(L-1,K)  
     &          +0.5*(DXV(LN )+DXV(LNW))*WVHUV(LN,K)      !
     &          -0.5*(DXV(L  )+DXV(L-1))*WVHUV(L ,K)) )  
          FYWAVE(L,K)=DZITMP*SVB(L)*SPB(L)  
     &          *( RSWRSI*(DXV(L)*(WVPP(L,K)-WVPP(LS ,K))  
     &          +DXV(L)*WVPV(L,K)*(HMPW(L)-HMPW(LS )))  
     &          +RSWRSR*(DXP(L)*WVHVV(L,K)-DXP(LS )*WVHVV(LS ,K)  
     &          +0.5*(DYU(L+1)+DYU(LSE))*WVHUV(L+1,K)     !
     &          -0.5*(DYU(L  )+DYU(LS ))*WVHUV(L  ,K)) )  
        ENDDO  
      ENDDO  
      IF(ISPGNS.GE.2)THEN  
        DO K=1,KC  
          DO NPNS=1,NPNSBP  
            L=LIJ(ISPNS(NPNS),JSPNS(NPNS))  
            FXWAVE(L,K)=0.  
            FYWAVE(L,K)=0.  
            L=LIJ(INPNS(NPNS),JNPNS(NPNS))  
            FXWAVE(L,K)=0.  
            FYWAVE(L,K)=0.  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C *** DSLLC END BLOCK  
C  
      RETURN  
      END  

