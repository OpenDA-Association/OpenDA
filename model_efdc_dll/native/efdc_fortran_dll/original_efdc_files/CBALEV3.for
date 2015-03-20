      SUBROUTINE CBALEV3  
C  
C CHANGE RECORD  
C **  SUBROUTINES CBALEV CALCULATE GLOBAL VOLUME, MASS, MOMENTUM,  
C **  AND ENERGY BALANCES  
C  
      USE GLOBAL  
C  
C **  ACCUMULATE INTERNAL SOURCES AND SINKS  
C  
      DO L=2,LA  
        VOLOUTE=VOLOUTE-QSUME(L)  
      ENDDO  
      DO K=1,KC  
        DO LL=1,NQSIJ  
          L=LQS(LL)  
          PPEOUTE=PPEOUTE-QSS(K,LL)*G*( 0.5*(BELV(L)+BELV(L-1))  
     &        +0.125*(HP(L)+H2P(L)+HP(L-1)+H2P(L-1))*(Z(K)+Z(K-1)) )  
        ENDDO  
      ENDDO  
      IF(ISTRAN(1).GE.1)THEN  
        DO K=1,KC  
          DO L=2,LC  
            CONT(L,K)=SAL(L,K)  
          ENDDO  
        ENDDO  
        DO NS=1,NQSIJ  
          L=LQS(NS)  
          NQSTMP=NQSERQ(NS)  
          NCSTMP=NCSERQ(NS,1)  
          DO K=1,KC  
            SALOUTE=SALOUTE  
     &          -MAX(QSS(K,NS),0.)*CQS(K,NS,1)  
     &          -MIN(QSS(K,NS),0.)*SAL(L,K)  
     &          -MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,1)  
     &          -MIN(QSERCELL(K,NS),0.)*SAL(L,K)  
          ENDDO  
        ENDDO  
        DO NCTL=1,NQCTL  
          RQWD=1.  
          IU=IQCTLU(NCTL)  
          JU=JQCTLU(NCTL)  
          LU=LIJ(IU,JU)  
          ID=IQCTLD(NCTL)  
          JD=JQCTLD(NCTL)  
          IF(ID.EQ.0.AND.JD.EQ.0)THEN  
            LD=LC  
            RQWD=0.  
          ELSE  
            LD=LIJ(ID,JD)  
          ENDIF  
          DO K=1,KC  
            SALOUTE=SALOUTE+QCTLT(K,NCTL)*CONT(LU,K)  
     &          -RQWD*QCTLT(K,NCTL)*CONT(LU,K)  
          ENDDO  
        ENDDO  
        DO NWR=1,NQWR  
          IU=IQWRU(NWR)  
          JU=JQWRU(NWR)  
          KU=KQWRU(NWR)  
          ID=IQWRD(NWR)  
          JD=JQWRD(NWR)  
          KD=KQWRD(NWR)  
          LU=LIJ(IU,JU)  
          LD=LIJ(ID,JD)  
          NQSTMP=NQWRSERQ(NWR)  
          NCSTMP=NQWRSERQ(NWR)  
          SALOUTE=SALOUTE+  
     &        ( (QWR(NWR)+QWRSERT(NQSTMP))*CONT(LU,KU) )  
          IF(LD.NE.1.OR.LD.NE.LC)THEN  
            SALOUTE=SALOUTE-  
     &          ( QWR(NWR)*(CONT(LU,KU)+CQWR(NWR,1))  
     &          +QSERCELL(K,NS)*(CONT(LU,KU)+CQWRSERT(NCSTMP,1)) )  
          ENDIF  
        ENDDO  
      ENDIF  
      IF(ISTRAN(3).GE.1)THEN  
        DO K=1,KC  
          DO L=2,LC  
            CONT(L,K)=DYE(L,K)  
          ENDDO  
        ENDDO  
        DO NS=1,NQSIJ  
          L=LQS(NS)  
          NQSTMP=NQSERQ(NS)  
          NCSTMP=NCSERQ(NS,1)  
          DO K=1,KC  
            DYEOUTE=DYEOUTE  
     &          -MAX(QSS(K,NS),0.)*CQS(K,NS,3)  
     &          -MIN(QSS(K,NS),0.)*DYE(L,K)  
     &          -MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,3)  
     &          -MIN(QSERCELL(K,NS),0.)*DYE(L,K)  
          ENDDO  
        ENDDO  
        DO NCTL=1,NQCTL  
          RQWD=1.  
          IU=IQCTLU(NCTL)  
          JU=JQCTLU(NCTL)  
          LU=LIJ(IU,JU)  
          ID=IQCTLD(NCTL)  
          JD=JQCTLD(NCTL)  
          IF(ID.EQ.0.AND.JD.EQ.0)THEN  
            LD=LC  
            RQWD=0.  
          ELSE  
            LD=LIJ(ID,JD)  
          ENDIF  
          DO K=1,KC  
            DYEOUTE=DYEOUTE+QCTLT(K,NCTL)*CONT(LU,K)  
     &          -RQWD*QCTLT(K,NCTL)*CONT(LU,K)  
          ENDDO  
        ENDDO  
        DO NWR=1,NQWR  
          IU=IQWRU(NWR)  
          JU=JQWRU(NWR)  
          KU=KQWRU(NWR)  
          ID=IQWRD(NWR)  
          JD=JQWRD(NWR)  
          KD=KQWRD(NWR)  
          LU=LIJ(IU,JU)  
          LD=LIJ(ID,JD)  
          NQSTMP=NQWRSERQ(NWR)  
          NCSTMP=NQWRSERQ(NWR)  
          DYEOUTE=DYEOUTE+  
     &        ( (QWR(NWR)+QWRSERT(NQSTMP))*CONT(LU,KU) )  
          IF(LD.NE.1.OR.LD.NE.LC)THEN  
            DYEOUTE=DYEOUTE-  
     &          ( QWR(NWR)*(CONT(LU,KU)+CQWR(NWR,3))  
     &          +QWRSERT(NQSTMP)*(CONT(LU,KU)+CQWRSERT(NCSTMP,3)) )  
          ENDIF  
        ENDDO  
      ENDIF  
      RETURN  
      END  

