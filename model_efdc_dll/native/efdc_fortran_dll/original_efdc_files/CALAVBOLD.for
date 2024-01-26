      SUBROUTINE CALAVBOLD (ISTL_)  
C 
C *** OLD STANDARD
C 
C **  SUBROUTINE CALAV CALCULATES VERTICAL VISCOSITY AND DIFFUSIVITY  
C **  USING GLAPERIN ET AL'S MODIFICATION OF THE MELLOR-YAMADA MODEL  
C **  (NOTE AV, AB, AND AQ ARE ACTUALLY DIVIDED BY H)  
C **  IF ISGA=1 VALUES ARE GEOMETRIC AVERAGES WITH THE PREVIOUS VALUES  
C CHANGE RECORD  
C  ADDED DRYCELL BYPASS AND CONSISTENT INITIALIZATION OF DRY VALUES  
C  
      USE GLOBAL  
	IMPLICIT NONE
	REAL::QQIMAX,RIQMIN,RIQMAX,RIQ
	REAL::SFAV,SFAB,ABTMP,AVTMP
	INTEGER::K,L,LS,ISTL_
C   SMTOP2   =      7.8464  
C   SMBOT1   =     34.6764  
C   SMBOT2   =      6.1272  
C   RLIMIT   =      0.0233  
C   SHMIN    =      0.0934  
C   SMMIN    =      0.1099  
C   SHMAX    =      5.2073  
C   SMMAX    =      4.9639  
C  
      QQIMAX=1./QQMIN  
      AVMAX=AVO  
      ABMAX=ABO  
      AVMIN=10.  
      ABMIN=10.  
      RIQMIN=-0.023  
      RIQMAX=0.28  
      IF(IDRYTBP.NE.0)THEN
      DO K=1,KC
        DO L=1,LC
          IF(IMASKDRY(L).EQ.1)THEN  
            AV(L,K)=AVO*HPI(L)  
            AB(L,K)=ABO*HPI(L)  
          ENDIF  
        ENDDO  
      ENDDO  
      ENDIF
      IF(ISFAVB.EQ.0)THEN  
        DO K=1,KS  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              QQI(L)=1./QQ(L,K)  
              QQI(L)=MIN(QQI(L),QQIMAX)  
            ENDIF  
          ENDDO  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              RIQ=-GP*HP(L)*DML(L,K)*DML(L,K)*DZIG(K)  
     &            *(B(L,K+1)-B(L,K))*QQI(L)  
              RIQ=MAX(RIQ,RIQMIN)  
              RIQ=MIN(RIQ,RIQMAX)  
C  
C      SFAV=0.4*(1.+8.*RIQ)/((1.+36.*RIQ)*(1.+6.*RIQ))  
C      SFAB=0.5/(1.+36.*RIQ)  
C  
              SFAV=0.3933*(1.+7.8464*RIQ)/((1.+34.6764*RIQ)*(1.+
     &            6.1272*RIQ))  
              SFAB=0.4939/(1.+34.6764*RIQ)  
             AB(L,K)=AVCON*SFAB*DML(L,K)*HP(L)*QQSQR(L,K)+ABO  
             AV(L,K)=AVCON*SFAV*DML(L,K)*HP(L)*QQSQR(L,K)+AVO  
              AVMAX=MAX(AVMAX,AV(L,K))  
              ABMAX=MAX(ABMAX,AB(L,K))  
              AVMIN=MIN(AVMIN,AV(L,K))  
              ABMIN=MIN(ABMIN,AB(L,K))  
              AV(L,K)=AV(L,K)*HPI(L)  
              AB(L,K)=SCB(L)*AB(L,K)*HPI(L)  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISFAVB.EQ.1)THEN  
      IF(IDRYTBP.EQ.0)THEN
        DO K=1,KS
          DO L=2,LA
              QQI(L)=1./QQ(L,K)
              QQI(L)=MIN(QQI(L),QQIMAX)
              RIQ=-GP*HP(L)*DML(L,K)*DML(L,K)*DZIG(K)
     &            *(B(L,K+1)-B(L,K))*QQI(L)
              RIQ=MAX(RIQ,RIQMIN)
              RIQ=MIN(RIQ,RIQMAX)
C
C      SFAV=0.4*(1.+8.*RIQ)/((1.+36.*RIQ)*(1.+6.*RIQ))
C      SFAB=0.5/(1.+36.*RIQ)
C
              SFAV=0.3933*(1.+7.8464*RIQ)/((1.+34.6764*RIQ)*(1.+
     &            6.1272*RIQ))
              SFAB=0.4939/(1.+34.6764*RIQ)
              ABTMP=AVCON*SFAB*DML(L,K)*HP(L)*QQSQR(L,K)+ABO
              AVTMP=AVCON*SFAV*DML(L,K)*HP(L)*QQSQR(L,K)+AVO
              AVMAX=MAX(AVMAX,AVTMP)
              ABMAX=MAX(ABMAX,ABTMP)
              AVMIN=MIN(AVMIN,AVTMP)
              ABMIN=MIN(ABMIN,ABTMP)
              AV(L,K)=0.5*(AV(L,K)+AVTMP*HPI(L))
              AB(L,K)=SCB(L)*0.5*(AB(L,K)+ABTMP*HPI(L))
          ENDDO
        ENDDO
c
      ELSE
        DO K=1,KS
          DO L=2,LA
            IF(LMASKDRY(L))THEN  
              QQI(L)=1./QQ(L,K)  
              QQI(L)=MIN(QQI(L),QQIMAX)  
            ENDIF  
          ENDDO
          DO L=2,LA
            IF(LMASKDRY(L))THEN  
              RIQ=-GP*HP(L)*DML(L,K)*DML(L,K)*DZIG(K)  
     &            *(B(L,K+1)-B(L,K))*QQI(L)  
              RIQ=MAX(RIQ,RIQMIN)  
              RIQ=MIN(RIQ,RIQMAX)  
C  
C      SFAV=0.4*(1.+8.*RIQ)/((1.+36.*RIQ)*(1.+6.*RIQ))  
C      SFAB=0.5/(1.+36.*RIQ)  
C  
              SFAV=0.3933*(1.+7.8464*RIQ)/((1.+34.6764*RIQ)*(1.+
     &            6.1272*RIQ))  
              SFAB=0.4939/(1.+34.6764*RIQ)  
              ABTMP=AVCON*SFAB*DML(L,K)*HP(L)*QQSQR(L,K)+ABO  
              AVTMP=AVCON*SFAV*DML(L,K)*HP(L)*QQSQR(L,K)+AVO  
              AVMAX=MAX(AVMAX,AVTMP)  
              ABMAX=MAX(ABMAX,ABTMP)  
              AVMIN=MIN(AVMIN,AVTMP)  
              ABMIN=MIN(ABMIN,ABTMP)  
              AV(L,K)=0.5*(AV(L,K)+AVTMP*HPI(L))  
              AB(L,K)=SCB(L)*0.5*(AB(L,K)+ABTMP*HPI(L))  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF
      ENDIF
      IF(ISFAVB.EQ.2)THEN  
        DO K=1,KS  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              QQI(L)=1./QQ(L,K)  
              QQI(L)=MIN(QQI(L),QQIMAX)  
            ENDIF  
          ENDDO  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              RIQ=-GP*HP(L)*DML(L,K)*DML(L,K)*DZIG(K)  
     &            *(B(L,K+1)-B(L,K))*QQI(L)  
              RIQ=MAX(RIQ,RIQMIN)  
              RIQ=MIN(RIQ,RIQMAX)  
C  
C      SFAV=0.4*(1.+8.*RIQ)/((1.+36.*RIQ)*(1.+6.*RIQ))  
C      SFAB=0.5/(1.+36.*RIQ)  
C  
              SFAV=0.3933*(1.+7.8464*RIQ)/((1.+34.6764*RIQ)*(1.+
     &            6.1272*RIQ))  
              SFAB=0.4939/(1.+34.6764*RIQ)  
              ABTMP=AVCON*SFAB*DML(L,K)*HP(L)*QQSQR(L,K)+ABO  
              AVTMP=AVCON*SFAV*DML(L,K)*HP(L)*QQSQR(L,K)+AVO  
              AVMAX=MAX(AVMAX,AVTMP)  
              ABMAX=MAX(ABMAX,ABTMP)  
              AVMIN=MIN(AVMIN,AVTMP)  
              ABMIN=MIN(ABMIN,ABTMP)  
              AV(L,K)=SQRT(AV(L,K)*AVTMP*HPI(L))  
              AB(L,K)=SCB(L)*SQRT(AB(L,K)*ABTMP*HPI(L))  
            ENDIF  
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
      DO K=2,KS  
        DO L=2,LA  
          AQ(L,K)=0.205*(AV(L,K-1)+AV(L,K))  
        ENDDO  
      ENDDO  
      DO L=2,LA  
        AQ(L,1)=0.205*AV(L,1)  
        AQ(L,KC)=0.205*AV(L,KS)  
      ENDDO  
      RETURN  
      END  

