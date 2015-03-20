C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C
      SUBROUTINE SETOBC2T(DE_T,D_LTD2,DE_TI)  
C  
C     TT Version (Only used with TT versions of CALPUV)
C
C CHANGE RECORD  
C
C ** SUBROUTINE SETOBC2T SETS OPEN BOUNDARY CONDITIONS FOR CALPUV2T AND  
C ** CALPUV2C  
C  
      USE GLOBAL  
C
C**********************************************************************C
C
C **  SET OPEN BOUNDARY SURFACE ELEVATIONS 
C
C----------------------------------------------------------------------C
C
      IF(ISDYNSTP.EQ.0)THEN
        TN=DT*FLOAT(N)+TCON*TBEGIN
      ELSE
        TN=TIMESEC
      ENDIF
C
      DO M=1,MTIDE
      TM=MOD(TN,TCP(M))
      TM=PI2*TM/TCP(M)
      CCCOS(M)=COS(TM)
      SSSIN(M)=SIN(TM)
      ENDDO
C
C----------------------------------------------------------------------C
C
      DO LL=1,NPBW
      L=LPBW(LL)
       CC(L)=DE_TI*DXYP(L)
       CS(L)=0.
       CW(L)=0.
       CE(L)=0.
       CN(L)=0.
      FP1(L)=PSERT(NPSERW(LL))
      DO M=1,MTIDE
      TC=CCCOS(M)
      TS=SSSIN(M)
      FP1(L)=FP1(L)+PCBW(LL,M)*TC+PSBW(LL,M)*TS
      ENDDO
      CET=0.5*D_LTD2*G*HRUO(L+1)*RCX(L+1)*HUTMP(L+1)
      IF(ISPBW(LL).GE.1)THEN
       TMP=D_LTD2*SQRT(G*HMU(L+1))*DXIU(L+1)
       CC(L)=CET*(1.+TMP)/TMP
       CE(L)=-CET
       FP1(L)=CET*(2.*FP1(L)
     & -SQRT(G*HMU(L+1))*FUHDYE(L+1)*DYIU(L+1)*HUI(L+1))/TMP
      ELSE
       FP1(L+1)=CET*FP1(L)
       FP1(L)=CC(L)*FP1(L)
      ENDIF
      ENDDO
C
C----------------------------------------------------------------------C
C
      DO LL=1,NPBE
      L=LPBE(LL)
       CC(L)=DE_TI*DXYP(L)
       CS(L)=0.
       CW(L)=0.
       CE(L)=0.
       CN(L)=0.      
      FP1(L)=PSERT(NPSERE(LL))
      DO M=1,MTIDE
      TC=CCCOS(M)
      TS=SSSIN(M)
      FP1(L)=FP1(L)+PCBE(LL,M)*TC+PSBE(LL,M)*TS
      ENDDO
      CWT=0.5*D_LTD2*G*HRUO(L  )*RCX(L)*HUTMP(L  )
      IF(ISPBE(LL).GE.1)THEN
       TMP=D_LTD2*SQRT(G*HMU(L))*DXIU(L)
       CC(L)=CWT*(1.+TMP)/TMP
       CW(L)=-CWT
       FP1(L)=CWT*(2.*FP1(L)
     & +SQRT(G*HMU(L))*FUHDYE(L)*DYIU(L)*HUI(L))/TMP
      ELSE
       FP1(L-1)=CWT*FP1(L)
       FP1(L)=CC(L)*FP1(L)
      ENDIF
      ENDDO
C
C----------------------------------------------------------------------C
C
      DO LL=1,NPBS
      L=LPBS(LL)
       CC(L)=DE_TI*DXYP(L)
       CS(L)=0.
       CW(L)=0.
       CE(L)=0.
       CN(L)=0.
      LN=LNC(L)
      FP1(L)=PSERT(NPSERS(LL))
      DO M=1,MTIDE
      TC=CCCOS(M)
      TS=SSSIN(M)
      FP1(L)=FP1(L)+PCBS(LL,M)*TC+PSBS(LL,M)*TS
      ENDDO
      CNT=0.5*D_LTD2*G*HRVO(LN )*RCY(LN)*HVTMP(LN )
      IF(ISPBS(LL).GE.1)THEN
       TMP=D_LTD2*SQRT(G*HMV(LN))*DYIV(LN)
       CC(L)=CNT*(1.+TMP)/TMP
       CN(L)=-CNT
       FP1(L)=CNT*(2.*FP1(L)
     & -SQRT(G*HMV(LN))*FVHDXE(LN)*DXIV(LN)*HVI(LN))/TMP
      ELSE
       FP1(LN)=CNT*FP1(L)
       FP1(L)=CC(L)*FP1(L)
      ENDIF
      ENDDO
C
C----------------------------------------------------------------------C
C
      DO LL=1,NPBN
      L=LPBN(LL)
       CC(L)=DE_TI*DXYP(L)
       CS(L)=0.
       CW(L)=0.
       CE(L)=0.
       CN(L)=0.
      LS=LSC(L)
      FP1(L)=PSERT(NPSERN(LL))
      DO M=1,MTIDE
      TC=CCCOS(M)
      TS=SSSIN(M)
      FP1(L)=FP1(L)+PCBN(LL,M)*TC+PSBN(LL,M)*TS
      ENDDO
      CST=0.5*D_LTD2*G*HRVO(L  )*RCY(L)*HVTMP(L  )
      IF(ISPBN(LL).GE.1)THEN
       TMP=D_LTD2*SQRT(G*HMV(L))*DYIV(L)
       CC(L)=CST*(1.+TMP)/TMP
       CS(L)=-CST
       FP1(L)=CST*(2.*FP1(L)
     & +SQRT(G*HMV(L))*FVHDXE(L)*DXIV(L)*HVI(L))/TMP
      ELSE
       FP1(LS)=CST*FP1(L)
       FP1(L)=CC(L)*FP1(L)
      ENDIF
      ENDDO
      RETURN  
      END  

