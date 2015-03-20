      SUBROUTINE SETOPENBC(DE_T,D_LTD2,DE_TI,HUT,HVT)  
C  
C CHANGE RECORD  
C ** SUBROUTINE SETOBC SETS OPEN BOUNDARY CONDITIONS FOR
C    CALPUV2T & CALPUV2C   AND  CALPUV9 & CALPUV9C
C  
C *** MODIFIED BY PAUL M. CRAIG TO ADDRESS MOVING CALL IN CALPUV2#
C      
      USE GLOBAL  
C
      DIMENSION HUT(LCM),HVT(LCM)
C  
C **  SET OPEN BOUNDARY SURFACE ELEVATIONS  
C  
      IF(ISDYNSTP.EQ.0)THEN  
        TN=DT*FLOAT(N)+TCON*TBEGIN  
      ELSE  
        TN=TIMESEC  
      ENDIF  
      DO M=1,MTIDE  
        TM=MOD(TN,TCP(M))  
        TM=PI2*TM/TCP(M)  
        CCCOS(M)=COS(TM)  
        SSSIN(M)=SIN(TM)  
      ENDDO  

      ! *** WEST OPEN BOUNDARY
      DO LL=1,NPBW  
        L=LPBW(LL)  
        CS(L)=0.  
        CW(L)=0.  
        CE(L)=0.  
        CN(L)=0.  
        FP(L)=PSERT(NPSERW(LL))  
        DO M=1,MTIDE  
          TC=CCCOS(M)  
          TS=SSSIN(M)  
          FP(L)=FP(L)+PCBW(LL,M)*TC+PSBW(LL,M)*TS  
        ENDDO  
        ! *** INACTIVAVE BC'S WHEN ELEVATIONS DROP BELOW BOTTOM+HDRY  
        FP1G=FP(L)/G-0.95*HDRY  
        IF(FP1G.LT.BELV(L).OR.FP1G.LT.BELV(L+1))THEN  
          FP(L)=(BELV(L)+0.95*HDRY)*G  
          CET=0.  
        ELSE  
          CET=0.5*D_LTD2*G*HRUO(L+1)*RCX(L+1)*HUT(L+1)  
        ENDIF  
  
        IF(ISPBW(LL).GE.1)THEN  
          TMP=D_LTD2*SQRT(G*HUT(L+1))*DXIU(L+1)  
          CE(L)=-CET  
          FP(L)=CET*(2.*FP(L)  
     &        -SQRT(G*HUT(L+1))*FUHDYE(L+1)*DYIU(L+1)/HUT(L+1))/TMP  
        ELSE  
          FP(L+1)=FP(L+1)+CET*FP(L)
          FP(L)=DE_TI*DXYP(L)*FP(L)
        ENDIF  
      ENDDO  

      ! *** EAST OPEN BOUNDARY
      DO LL=1,NPBE  
        L=LPBE(LL)  
        CS(L)=0.  
        CW(L)=0.  
        CE(L)=0.  
        CN(L)=0.  
        
        FP(L)=PSERT(NPSERE(LL))  
        DO M=1,MTIDE  
          TC=CCCOS(M)  
          TS=SSSIN(M)  
          FP(L)=FP(L)+PCBE(LL,M)*TC+PSBE(LL,M)*TS  
        ENDDO  
        ! *** INACTIVAVE BC'S WHEN ELEVATIONS DROP BELOW BOTTOM+HDRY  
        FP1G=FP(L)/G-0.95*HDRY  
        IF(FP1G.LT.BELV(L).OR.FP1G.LT.BELV(L-1))THEN  
          FP(L)=(BELV(L)+0.95*HDRY)*G  
          CWT=0.  
        ELSE  
          CWT=0.5*D_LTD2*G*HRUO(L)*RCX(L)*HUT(L)  
        ENDIF  
        IF(ISPBE(LL).GE.1)THEN  
          TMP=D_LTD2*SQRT(G*HUT(L))*DXIU(L)  
          CC(L)=CWT*(1.+TMP)/TMP  
          CW(L)=-CWT  
          FP(L)=CWT*(2.*FP(L)  
     &        +SQRT(G*HUT(L))*FUHDYE(L)*DYIU(L)/HUT(L))/TMP  
        ELSE  
          FP(L-1)=FP(L-1)+CWT*FP(L)  
          FP(L)=DE_TI*DXYP(L)*FP(L)  
        ENDIF  
      ENDDO  

      ! *** SOUTH OPEN BOUNDARY
      DO LL=1,NPBS  
        L=LPBS(LL)  
        LN=LNC(L)  
        CS(L)=0.  
        CW(L)=0.  
        CE(L)=0.  
        CN(L)=0.  
        FP(L)=PSERT(NPSERS(LL))  
        DO M=1,MTIDE  
          TC=CCCOS(M)  
          TS=SSSIN(M)  
          FP(L)=FP(L)+PCBS(LL,M)*TC+PSBS(LL,M)*TS  
        ENDDO  
        ! *** INACTIVAVE BC'S WHEN ELEVATIONS DROP BELOW BOTTOM+HDRY  
        FP1G=FP(L)/G-0.95*HDRY  
        IF(FP1G.LT.BELV(L).OR.FP1G.LT.BELV(LN))THEN  
          FP(L)=(BELV(L)+0.95*HDRY)*G  
          CNT=0.  
        ELSE  
          CNT=0.5*D_LTD2*G*HRVO(LN)*RCY(LN)*HVT(LN)  
        ENDIF  
        IF(ISPBS(LL).GE.1)THEN  
          TMP=D_LTD2*SQRT(G*HVT(LN))*DYIV(LN)  
          CC(L)=CNT*(1.+TMP)/TMP  
          CN(L)=-CNT  
          FP(L)=CNT*(2.*FP(L)  
     &        -SQRT(G*HVT(LN))*FVHDXE(LN)*DXIV(LN)/HVT(LN))/TMP  
        ELSE  
          FP(LN)=FP(LN)+CNT*FP(L)  
          FP(L)=DE_TI*DXYP(L)*FP(L)  
        ENDIF  
      ENDDO  

      ! *** NORTH OPEN BOUNDARY
      DO LL=1,NPBN  
        L=LPBN(LL)  
        LS=LSC(L)  
        CS(L)=0.  
        CW(L)=0.  
        CE(L)=0.  
        CN(L)=0.  
        FP(L)=PSERT(NPSERN(LL))  
        DO M=1,MTIDE  
          TC=CCCOS(M)  
          TS=SSSIN(M)  
          FP(L)=FP(L)+PCBN(LL,M)*TC+PSBN(LL,M)*TS  
        ENDDO  
        ! *** INACTIVAVE BC'S WHEN ELEVATIONS DROP BELOW BOTTOM+HDRY  
        FP1G=FP(L)/G-0.95*HDRY  
        IF(FP1G.LT.BELV(L).OR.FP1G.LT.BELV(LS))THEN  
          FP(L)=(BELV(L)+0.95*HDRY)*G  
          CST=0.  
        ELSE  
          CST=0.5*D_LTD2*G*HRVO(L)*RCY(L)*HVT(L)  
        ENDIF  
        IF(ISPBN(LL).GE.1)THEN  
          TMP=D_LTD2*SQRT(G*HVT(L))*DYIV(L)  
          CC(L)=CST*(1.+TMP)/TMP  
          CS(L)=-CST  
          FP(L)=CST*(2.*FP(L)  
     &        +SQRT(G*HVT(L))*FVHDXE(L)*DXIV(L)/HVT(L))/TMP  
        ELSE  
          FP(LS)=FP(LS)+CST*FP(L)  
          FP(L)=DE_TI*DXYP(L)*FP(L)  
        ENDIF  
      ENDDO  

      RETURN  
      END  

