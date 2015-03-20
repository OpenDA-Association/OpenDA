      SUBROUTINE BAL2T3A  
C  
C CHANGE RECORD  
C  SUBROUTINE ADDED FOR 2 TIME-LEVEL BALANCES INCLUDING SED,SND,TOX  
C  MODIFIED SND MASS BALANCE WITH RESPECT TO BED LOAD OUTFLOW  
C  ADDED QDWASTE TO WATER MASS BALANCE  
C **  SUBROUTINES CALBAL CALCULATE GLOBAL VOLUME, MASS, MOMENTUM,  
C **  AND ENERGY BALANCES  
C  
      USE GLOBAL  

 	IMPLICIT NONE
	INTEGER::LD,K,L,NSX,NS,NWR,NCTL,ID,JD,KU,NT,M,JU,LU,KD,LL,NQSTMP
	INTEGER::IU,NCSTMP
	INTEGER::LF,ithds
	REAL::RQWD

      IF(ISDYNSTP.EQ.0)THEN  
        DELT=DT  
      ELSE  
        DELT=DTDYN  
      END IF  
C  
C **  ACCUMULATE INTERNAL SOURCES AND SINKS  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL) REDUCTION(-:VOLOUT,WVOLOUT)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO L=LF,LL
        VOLOUT=VOLOUT-DELT*(QSUME(L)-QDWASTE(L))
c     ENDDO  
c     DO L=2,LA  
        WVOLOUT=WVOLOUT-DELT*(QSUME(L)-QDWASTE(L))  
      ENDDO  
c
      enddo

      DO K=1,KC  
        DO LL=1,NQSIJ  
          L=LQS(LL)  
          PPEOUT=PPEOUT-DELT*QSS(K,LL)*G*( 0.5*(BELV(L)+BELV(L-1))  
     &        +0.125*(HP(L)+H2P(L)+HP(L-1)+H2P(L-1))*(Z(K)+Z(K-1)) )  
        ENDDO  
      ENDDO  
      IF(ISTRAN(1).GE.1)THEN  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_2_LC(1,ithds)
         LL=jse_2_LC(2,ithds)
c
        DO K=1,KC  
          DO L=LF,LL
            CONT(L,K)=SAL1(L,K)  
          ENDDO  
        ENDDO  
c
      enddo

        DO NS=1,NQSIJ  
          L=LQS(NS)  
          NQSTMP=NQSERQ(NS)  
          NCSTMP=NCSERQ(NS,1)  
          DO K=1,KC  
            SALOUT=SALOUT  
     &          -DELT*MAX(QSS(K,NS),0.)*CQS(K,NS,1)  
     &          -DELT*MIN(QSS(K,NS),0.)*SAL1(L,K)  
     &          -DELT*MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,1)  
     &          -DELT*MIN(QSERCELL(K,NS),0.)*SAL1(L,K)  
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
            DO K=1,KC  
              SALOUT=SALOUT+DELT*QCTLT(K,NCTL)*CONT(LU,K)  
            ENDDO  
          ENDIF  
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
     &        DELT*( (QWR(NWR)+QWRSERT(NQSTMP))*CONT(LU,KU) )  
          IF(LD.NE.1.OR.LD.NE.LC)THEN  
            SALOUT=SALOUT-  
     &          DELT*( QWR(NWR)*(CONT(LU,KU)+CQWR(NWR,1))  
     &          +QWRSERT(NQSTMP)*(CONT(LU,KU)+CQWRSERT(NCSTMP,1)) )  
          ENDIF  
        ENDDO  
      ENDIF  
      IF(ISTRAN(3).GE.1)THEN  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_2_LC(1,ithds)
         LL=jse_2_LC(2,ithds)
c
        DO K=1,KC  
          DO L=LF,LL
            CONT(L,K)=DYE1(L,K)  
          ENDDO  
        ENDDO  
c
      enddo

        DO NS=1,NQSIJ  
          L=LQS(NS)  
          NQSTMP=NQSERQ(NS)  
          NCSTMP=NCSERQ(NS,3)  
          DO K=1,KC  
            DYEOUT=DYEOUT  
     &          -DELT*MAX(QSS(K,NS),0.)*CQS(K,NS,3)  
     &          -DELT*MIN(QSS(K,NS),0.)*DYE(L,K)  
     &          -DELT*MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,3)  
     &          -DELT*MIN(QSERCELL(K,NS),0.)*DYE1(L,K)  
            DYEOUT2T=DYEOUT2T  
     &          -DELT*MAX(QSS(K,NS),0.)*CQS(K,NS,3)  
     &          -DELT*MIN(QSS(K,NS),0.)*DYE(L,K)  
     &          -DELT*MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,3)  
     &          -DELT*MIN(QSERCELL(K,NS),0.)*DYE1(L,K)  
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
            DO K=1,KC  
              DYEOUT=DYEOUT+DELT*QCTLT(K,NCTL)*CONT(LU,K)  
              DYEOUT2T=DYEOUT2T+DELT*QCTLT(K,NCTL)*CONT(LU,K)  
            ENDDO  
          ENDIF  
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
     &        DELT*( (QWR(NWR)+QWRSERT(NQSTMP))*CONT(LU,KU) )  
          DYEOUT2T=DYEOUT2T+  
     &        DELT*( (QWR(NWR)+QWRSERT(NQSTMP))*CONT(LU,KU) )  
          IF(LD.NE.1.OR.LD.NE.LC)THEN  
            DYEOUT=DYEOUT-  
     &          DELT*( QWR(NWR)*(CONT(LU,KU)+CQWR(NWR,3))  
     &          +QWRSERT(NQSTMP)*(CONT(LU,KU)+CQWRSERT(NCSTMP,3)) )  
            DYEOUT2T=DYEOUT2T-  
     &          DELT*( QWR(NWR)*(CONT(LU,KU)+CQWR(NWR,3))  
     &          +QWRSERT(NQSTMP)*(CONT(LU,KU)+CQWRSERT(NCSTMP,3)) )  
          ENDIF  
        ENDDO  
      ENDIF  
      IF(ISTRAN(5).GE.1)THEN  
        DO NT=1,NTOX  
          M=MSVTOX(NT)  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_2_LC(1,ithds)
         LL=jse_2_LC(2,ithds)
c
          DO K=1,KC  
            DO L=LF,LL
              CONT(L,K)=TOX1(L,K,NT)  
            ENDDO  
          ENDDO  
c
      enddo
C  
C  TOXOUT2T(NT) IS NET TOXIC MASS GOING OUT OF DOMAIN DUE  
C  TO WATER COLUMN VOLUME SOURCES AND SINKS  
C  
          DO NS=1,NQSIJ  
            L=LQS(NS)  
            NQSTMP=NQSERQ(NS)  
            NCSTMP=NCSERQ(NS,M)  
            DO K=1,KC  
              TOXOUT2T(NT)=TOXOUT2T(NT)  
     &            -DELT*MAX(QSS(K,NS),0.)*CQS(K,NS,M)  
     &            -DELT*MIN(QSS(K,NS),0.)*TOX1(L,K,NT)  
     &            -DELT*MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,M)  
     &            -DELT*MIN(QSERCELL(K,NS),0.)*TOX1(L,K,NT)  
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
              DO K=1,KC  
              TOXOUT2T(NT)=TOXOUT2T(NT)+DELT*QCTLT(K,NCTL)*TOX1(LU,K,NT)  
              ENDDO  
            ENDIF  
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
            TOXOUT2T(NT)=TOXOUT2T(NT)+  
     &          DELT*( (QWR(NWR)+QWRSERT(NQSTMP))*CONT(LU,KU) )  
            IF(LD.NE.1.OR.LD.NE.LC)THEN  
              TOXOUT2T(NT)=TOXOUT2T(NT)-  
     &            DELT*( QWR(NWR)*(CONT(LU,KU)+CQWR(NWR,M))  
     &            +QWRSERT(NQSTMP)*(CONT(LU,KU)+CQWRSERT(NCSTMP,M)) )  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISTRAN(6).GE.1)THEN  
        DO NSX=1,NSED  
          M=MSVSED(NSX)  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_2_LC(1,ithds)
         LL=jse_2_LC(2,ithds)
c
          DO K=1,KC  
            DO L=LF,LL
              CONT(L,K)=SED1(L,K,NSX)  
            ENDDO  
          ENDDO  
c
      enddo
C  
C SEDOUT2T(NSX) IS IS NET COHESIVE MASS GOING OUT OF DOMAIN DUE  
C   TO WATER COLUMN VOLUME SOURCES AND SINKS  
C  
          DO NS=1,NQSIJ  
            L=LQS(NS)  
            NQSTMP=NQSERQ(NS)  
            NCSTMP=NCSERQ(NS,M)  
            DO K=1,KC  
              SEDOUT2T(NSX)=SEDOUT2T(NSX)  
     &            -DELT*MAX(QSS(K,NS),0.)*CQS(K,NS,M)  
     &            -DELT*MIN(QSS(K,NS),0.)*SED1(L,K,NSX)  
     &            -DELT*MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,M)  
     &            -DELT*MIN(QSERCELL(K,NS),0.)*SED1(L,K,NSX)  
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
              DO K=1,KC  
               SEDOUT2T(NSX)=SEDOUT2T(NSX)+DELT*QCTLT(K,NCTL)*CONT(LU,K)  
              ENDDO  
            ENDIF  
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
            SEDOUT2T(NSX)=SEDOUT2T(NSX)+  
     &          DELT*( (QWR(NWR)+QWRSERT(NQSTMP))*CONT(LU,KU) )  
            IF(LD.NE.1.OR.LD.NE.LC)THEN  
              SEDOUT2T(NSX)=SEDOUT2T(NSX)-  
     &            DELT*( QWR(NWR)*(CONT(LU,KU)+CQWR(NWR,M))  
     &            +QWRSERT(NQSTMP)*(CONT(LU,KU)+CQWRSERT(NCSTMP,M)) )  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISTRAN(7).GE.1)THEN  
        DO NSX=1,NSND  
          M=MSVSND(NSX)  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_2_LC(1,ithds)
         LL=jse_2_LC(2,ithds)
c
          DO K=1,KC  
            DO L=LF,LL
              CONT(L,K)=SND1(L,K,NSX)  
            ENDDO  
          ENDDO  
c
      enddo
C  
C  SNDOUT2T(NSX) IS NET NONCOHESIVE MASS GOING OUT OF DOMAIN DUE  
C  TO WATER COLUMN VOLUME SOURCES AND SINKS  
C  
          DO NS=1,NQSIJ  
            L=LQS(NS)  
            NQSTMP=NQSERQ(NS)  
            NCSTMP=NCSERQ(NS,M)  
            DO K=1,KC  
              SNDOUT2T(NSX)=SNDOUT2T(NSX)  
     &            -DELT*MAX(QSS(K,NS),0.)*CQS(K,NS,M)  
     &            -DELT*MIN(QSS(K,NS),0.)*SND1(L,K,NSX)  
     &            -DELT*MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,M)  
     &            -DELT*MIN(QSERCELL(K,NS),0.)*SND1(L,K,NSX)  
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
              DO K=1,KC  
               SNDOUT2T(NSX)=SNDOUT2T(NSX)+DELT*QCTLT(K,NCTL)*CONT(LU,K)  
              ENDDO  
            ENDIF  
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
            SNDOUT2T(NSX)=SNDOUT2T(NSX)+  
     &          DELT*( (QWR(NWR)+QWRSERT(NQSTMP))*CONT(LU,KU) )  
            IF(LD.NE.1.OR.LD.NE.LC)THEN  
              SNDOUT2T(NSX)=SNDOUT2T(NSX)-  
     &            DELT*( QWR(NWR)*(CONT(LU,KU)+CQWR(NWR,M))  
     &            +QWRSERT(NQSTMP)*(CONT(LU,KU)+CQWRSERT(NCSTMP,M)) )  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
  800 FORMAT('N,NS,SNDFBL2T,DEL',2I5,2E14.5)  
      RETURN  
      END  

