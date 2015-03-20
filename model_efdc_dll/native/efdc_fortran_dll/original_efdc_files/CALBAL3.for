      SUBROUTINE CALBAL3  
C  
C CHANGE RECORD  
C **  SUBROUTINES CALBAL CALCULATE GLOBAL VOLUME, MASS, MOMENTUM,  
C **  AND ENERGY BALANCES  
C  
      USE GLOBAL  

	IMPLICIT NONE
	INTEGER::L,K,LL,NS,NQSTMP,NCSTMP,NCTL,IU,JU,LU
	INTEGER::ID,JD,LD,NWR,KU,KD
	REAL::RQWD
C  
C **  ACCUMULATE INTERNAL SOURCES AND SINKS  
C  
      DO L=2,LA  
        VOLOUT=VOLOUT-QSUME(L)  
      ENDDO  
      DO K=1,KC  
        DO LL=1,NQSIJ  
          L=LQS(LL)  
          PPEOUT=PPEOUT-QSS(K,LL)*G*( 0.5*(BELV(L)+BELV(L-1))  
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
            SALOUT=SALOUT  
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
            SALOUT=SALOUT+QCTLT(K,NCTL)*CONT(LU,K)  
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
          SALOUT=SALOUT+  
     &        ( (QWR(NWR)+QWRSERT(NQSTMP))*CONT(LU,KU) )  
          IF(LD.NE.1.OR.LD.NE.LC)THEN  
            SALOUT=SALOUT-  
     &          ( QWR(NWR)*(CONT(LU,KU)+CQWR(NWR,1))  
     &          +QWRSERT(NQSTMP)*(CONT(LU,KU)+CQWRSERT(NCSTMP,1)) )  
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
          NCSTMP=NCSERQ(NS,3)  
          DO K=1,KC  
            DYEOUT=DYEOUT  
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
            DYEOUT=DYEOUT+QCTLT(K,NCTL)*CONT(LU,K)  
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
          DYEOUT=DYEOUT+  
     &        ( (QWR(NWR)+QWRSERT(NQSTMP))*CONT(LU,KU) )  
          IF(LD.NE.1.OR.LD.NE.LC)THEN  
            DYEOUT=DYEOUT-  
     &          ( QWR(NWR)*(CONT(LU,KU)+CQWR(NWR,3))  
     &          +QWRSERT(NQSTMP)*(CONT(LU,KU)+CQWRSERT(NCSTMP,3)) )  
          ENDIF  
        ENDDO  
      ENDIF  
      RETURN  
      END  

