      SUBROUTINE CALAVB2 (ISTL_)  
C  
C **  SUBROUTINE CALAV CALCULATES VERTICAL VISCOSITY AND DIFFUSIVITY  
C **  USING GLAPERIN ET AL'S MODIFICATION OF THE MELLOR-YAMADA MODEL  
C **  (NOTE AV, AB, AND AQ ARE ACTUALLY DIVIDED BY H)  
C **  IF ISGA=1 VALUES ARE GEOMETRIC AVERAGES WITH THE PREVIOUS VALUES  
C CHANGE RECORD  
C
      USE GLOBAL 

	IMPLICIT NONE
C
      !REAL*8 HPD(LCM),DMLD(LCM,KCM),BD(LCM,KCM),DZIGD(KCM)
      !REAL*8 CTURBB1D(LCM,KCM),CTURBB2D(LCM,KCM),QQD(LCM,KCM)
      !REAL*8 CTURBD,CTURB2BD,GPD
      !REAL*8 RIQMIN,RAVBTMP,DELBTMP,RITMP,BFUN,RIQ
      !REAL*8 TMPVAL,TMPVAL1,SBTOP,SVTOP,TMPVAL2,SVTOP2,SVBOT
      !REAL*8 SFAV,SFAB,SBBOT,TMPVAL3
      !REAL*8 ATURB1,ATURB2,TURBC1
      !REAL*8 AVTMP,ABTMP

      REAL*8 RIQMIN,DELBTMP,RITMP,BFUN,RIQ
      REAL*8 TMPVAL,TMPVAL1,SBTOP,SVTOP,TMPVAL2,SVTOP2,SVBOT
      REAL*8 SFAV,SFAB,SBBOT,TMPVAL3
      REAL*8 ATURB1,ATURB2,TURBC1
      REAL*8 AVTMP,ABTMP,BBTC,DELBSQ
C
      REAL*8 TMP1,AQTMP
C
      INTEGER K,L,LS,ISTL_
C
      DATA ATURB1,ATURB2,TURBC1/0.92,0.74,0.08/  
C  
C   SHTOP    =      0.4939  
C   SHBOT    =     34.6764  
C   SMTOP1   =      0.3933  
C   SMTOP2   =      7.8464  
C   SMBOT1   =     34.6764  
C   SMBOT2   =      6.1272  
C   RLIMIT   =      0.0233  
C   SHMIN    =      0.0934  
C   SMMIN    =      0.1099  
C   SHMAX    =      5.2073  
C   SMMAX    =      4.9639  
C  
      RIQMIN=-0.023  
C
      DO K=1,KS  
        DO L=2,LA  
          CTURBB1(L,K)=CTURB  
          CTURBB2(L,K)=CTURB2B   ! PMC ONLY NEEDS TO BE ASSIGNED ONCE SINCE NEVER CHANGED
        ENDDO  
      ENDDO  
C
      DO K=1,KS    ! PMC SKIP IF BSC.LE.1.E-6
        DO L=2,LA  
          DELBTMP=(B(L,K+1)-B(L,K))*DZIG(K)  
          RITMP=-GP*HP(L)*DELBTMP/QQ(L,K)  
          IF(RITMP.GT.0.)THEN  
            RIQ=DML(L,K)*DML(L,K)*RITMP  
            BFUN=EXP(-3.11*RIQ)  
            CTURBB1(L,K)=CTURB/(BFUN+1.E-12)  

            ! *** Original Code
            IF(BBT(L,K).GT.0.)THEN   ! *** PMC BBT is never set, so this is never used
              TMPVAL=DELBTMP*DELBTMP/(RITMP*BBT(L,K))  
              CTURBB2(L,K)=CTURB2B/(1.+0.61*(1.-BFUN)*TMPVAL)  
            ENDIF  
          ENDIF  
        ENDDO
      ENDDO  

      DO K=1,KS  
        DO L=2,LA  
          RIQ=-GP*HP(L)*DML(L,K)*DML(L,K)*DZIG(K)  
     &            *(B(L,K+1)-B(L,K))/QQ(L,K)  
          RIQ=MAX(RIQ,RIQMIN)  
          TMPVAL1=1.-( 6.*ATURB1/CTURBB1(L,K) )  
          SBTOP=ATURB2*TMPVAL1  
          SBBOT=3.*ATURB2*( 6.*ATURB1+CTURBB2(L,K) )  
          SVTOP=ATURB1*( TMPVAL1-3.*TURBC1)  
          TMPVAL2=TMPVAL1*( CTURBB2(L,K)-3.*ATURB2 )  
          TMPVAL3=-3.*TURBC1*( CTURBB2(L,K)+6.*ATURB1)  
          SVTOP2=3.*ATURB2*(TMPVAL2+TMPVAL3)/SVTOP  
          SVBOT=9.*ATURB1*ATURB2  
          SFAV=SVTOP*(1.+SVTOP2*RIQ)/((1.+SVBOT*RIQ)*(1.+SBBOT*RIQ))  
          SFAB=SBTOP/(1.+SBBOT*RIQ)  
          ABTMP=AVCON*SFAB*DML(L,K)*HP(L)*QQSQR(L,K)+ABO
          AVTMP=AVCON*SFAV*DML(L,K)*HP(L)*QQSQR(L,K)+AVO
C
          IF(ISFAVB.EQ.0)THEN  
            AV(L,K)=AVTMP*HPI(L)
            AB(L,K)=SCB(L)*ABTMP*HPI(L)
          ELSEIF(ISFAVB.EQ.1)THEN  
            AV(L,K)=0.5*(AV(L,K)+AVTMP*HPI(L))  
            AB(L,K)=SCB(L)*0.5*(AB(L,K)+ABTMP*HPI(L))  
          ELSEIF(ISFAVB.EQ.2)THEN  
            AV(L,K)=SQRT(AV(L,K)*AVTMP*HPI(L))  
            AB(L,K)=SCB(L)*SQRT(AB(L,K)*ABTMP*HPI(L))  
          ENDIF
        ENDDO        
      ENDDO

      IF(ISLOG.GE.1)THEN  
        AVMAX=AVO  
        ABMAX=ABO  
        AVMIN=10.  
        ABMIN=10.  
        DO K=1,KS  
          DO L=2,LA  
            AVMAX=MAX(AVMAX,AV(L,K))  
            ABMAX=MAX(ABMAX,AB(L,K))  
            AVMIN=MIN(AVMIN,AV(L,K))  
            ABMIN=MIN(ABMIN,AB(L,K))  
          ENDDO
        ENDDO
      ENDIF

      ! *** NOW APPLY MAXIMUM, IF REQURIED
      IF(ISAVBMX.GE.1)THEN  
        DO K=1,KS  
          DO L=2,LA  
            AVTMP=AVMX*HPI(L)  
            ABTMP=ABMX*HPI(L)  
            AV(L,K)=MIN(AV(L,K),AVTMP)  
            AB(L,K)=MIN(AB(L,K),ABTMP)  
          ENDDO  
        ENDDO  
      ENDIF
  
      DO K=1,KS  
        DO L=2,LA  
          LS=LSC(L)
          AVUI(L,K)=2./(AV(L,K)+AV(L-1,K))  
          AVVI(L,K)=2./(AV(L,K)+AV(LS,K))  
        ENDDO  
      ENDDO  

      IF(ISTL_.EQ.3)THEN  
        DO K=2,KS  
          DO L=2,LA  
            AQ(L,K)=0.205*(AV(L,K-1)+AV(L,K))  
          ENDDO  
        ENDDO  
        DO L=2,LA  
          AQ(L,1)=0.205*AV(L,1)  
          AQ(L,KC)=0.205*AV(L,KS)  
        ENDDO  
      ELSE  
        DO K=2,KS  
          DO L=2,LA  
            AQTMP=0.205*(AV(L,K-1)+AV(L,K))  
            AQ(L,K)=AQTMP  
          ENDDO  
        ENDDO  
        DO L=2,LA  
          AQTMP=0.205*AV(L,1)  
          AQ(L,1)=AQTMP  
          AQTMP=0.205*AV(L,KS)  
          AQ(L,KC)=AQTMP  
        ENDDO  
      ENDIF  

      RETURN  
      END  

