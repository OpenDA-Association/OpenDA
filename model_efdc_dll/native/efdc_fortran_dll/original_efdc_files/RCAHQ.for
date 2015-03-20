      SUBROUTINE RCAHQ  
C  
C CHANGE RECORD  
C **  SUBROUTINE FOR INTERFACING RCA MODEL  
C **  MODIFIED FROM WCA2A PROUDCTION VERSION  
C **  WITH WITHDRAWL-RETURN FLOW OPTION DEACTIVATED BY CNWR  
C  
C *** PMC  THIS ROUTINE USES HMP, THE STATIC IC DEPTH.  SHOULDN'T IT USE HP?
      USE GLOBAL  

      REAL,ALLOCATABLE,DIMENSION(:)::DZRCA  
      REAL,ALLOCATABLE,DIMENSION(:)::DZZRCA  
      REAL,ALLOCATABLE,DIMENSION(:)::QINRCA  
      REAL,ALLOCATABLE,DIMENSION(:,:)::QINTFL  
      REAL,ALLOCATABLE,DIMENSION(:,:)::TMPARA  

      ALLOCATE(DZRCA(KCM+1))  
      ALLOCATE(DZZRCA(KCM+1))  
      ALLOCATE(QINRCA(NQSIJM))  
      ALLOCATE(QINTFL(NQINFLM,KCM))  
      ALLOCATE(TMPARA(ICM,JCM))  
      DZRCA=0.
      DZZRCA=0.
      QINRCA=0.
      QINTFL=0.
      TMPARA=0.
C  
C **  WRITE TIME INVARIANT FILES ON FIRST ENTRY  
C  
      IF(JSWASP.EQ.0) GOTO 1000  
      JSWASP=0  
      OPEN(1,FILE='EFDC.RCA',STATUS='UNKNOWN')  
C  
C **  READ I,J LOCATION OF THE DUMP CELL (AN ACTIVE WATER CELL)  
C  
      DO NSKIP=1,4  
        READ(1,100)  
      ENDDO  
      READ(1,*)ISDRCA,IDMPCL,JDMPCL  
      CLOSE(1)  
C  
C **  WRITE I,J INDICES DEFINING FLOWS BETWEEN ARBITARY CELLS  
C **  (POSTIVE FLOW DIRECTION DEFINED FROM FIRST TO SECOND I,J PAIR)  
C  
      OPEN(1,FILE='FLWMAP.INP',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='FLWMAP.INP',STATUS='UNKNOWN')  
      NINTFL=0  
      IF(NQSIJ.GT.0)THEN  
        DO NS=1,NQSIJ  
          NINTFL=NINTFL+1  
          WRITE(1,101)IDMPCL,JDMPCL,IQS(NS),JQS(NS)  
        ENDDO  
      ENDIF  
      IF(NQCTL.GT.0)THEN  
        DO NCTL=1,NQCTL  
          NINTFL=NINTFL+1  
          IF(IQCTLD(NCTL).GT.0)THEN  
            WRITE(1,101) IQCTLU(NCTL),JQCTLU(NCTL),  
     &          IQCTLD(NCTL),JQCTLD(NCTL)  
          ELSE  
            WRITE(1,101)IQCTLU(NCTL),JQCTLU(NCTL),  
     &          IDMPCL,JDMPCL  
          ENDIF  
        ENDDO  
      ENDIF  
      IF(NQWR.GT.0)THEN  
        DO NWR=1,NQWR  
          NINTFL=NINTFL+1  
          WRITE(1,101)IQWRU(NWR),JQWRU(NWR),IQWRD(NWR),JQWRD(NWR)  
        ENDDO  
      ENDIF  
      IF(MDCHH.GT.0)THEN  
        DO NMD=1,MDCHH  
          IF(IMDCHU(NMD).GT.1)THEN  
            NINTFL=NINTFL+1  
            WRITE(1,101)IMDCHU(NMD),JMDCHU(NMD),IMDCHH(NMD),JMDCHH(NMD)  
          ENDIF  
        ENDDO  
        DO NMD=1,MDCHH  
          IF(IMDCHV(NMD).GT.1)THEN  
            NINTFL=NINTFL+1  
            WRITE(1,101)IMDCHV(NMD),JMDCHV(NMD),IMDCHH(NMD),JMDCHH(NMD)  
          ENDIF  
        ENDDO  
      ENDIF  
      CLOSE(1)  
      OPEN(1,FILE='EFDCRCA.LOG',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='EFDCRCA.LOG',STATUS='UNKNOWN')  
      ICRCA1=IDMPCL  
      ICRCA2=JDMPCL  
      ICRCA3=ISDRCA  
      NCRCA1=NINTFL  
      WRITE(1,102)IC,JC,KC  
      WRITE(1,103)NINTFL  
      WRITE(1,104)IDMPCL,JDMPCL  
      TIMTMP=TCON*TBEGIN/86400.  
      WRITE(1,105)TIMTMP  
      CLOSE(1)  
      OPEN(1,FILE='INFLWIJ.DAT',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='INFLWIJ.DAT',STATUS='UNKNOWN')  
      WRITE(1,110)  
      WRITE(1,111)  
      IF(NQSIJ.GE.1)THEN  
        DO NS=1,NQSIJ  
          WRITE(1,112)NS,IQS(NS),JQS(NS)  
        ENDDO  
      ENDIF  
      CLOSE(1)  
C  
C **  WRITE GRID GEOMETRY, INCLUDING INITIAL DEPTH  
C  
      OPEN(1,FILE='GCM_GEOM',FORM='UNFORMATTED',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='GCM_GEOM',FORM='UNFORMATTED',STATUS='UNKNOWN')  
      DO L=1,LC  
        HTMP(L)=HMP(L)  
      ENDDO  
      DZRCA(KC+1)=0.  
      DZZRCA(KC+1)=0.  
      DZZRCA(1)=0.  
      DO K=1,KC  
        KK=KC+1-K  
        DZRCA(KK)=DZC(K)  
      ENDDO  
      IF(KC.GT.1)THEN  
        DO K=1,KS  
          KK=KS+2-K  
          DZZRCA(KK)=DZG(K)  
        ENDDO  
      ENDIF  
      WRITE(1)DZRCA,DZZRCA  
      SQRTMP=SQRT(0.5)  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=1.E-6  
        ENDDO  
      ENDDO  
      DO J=1,JC  
        DO I=1,IC  
          IF(IJCT(I,J).GE.1.AND.IJCT(I,J).LE.5)THEN  
            TMPARA(I,J)=HMP(LIJ(I,J))  
          ENDIF  
        ENDDO  
      ENDDO  
      WRITE(1)TMPARA  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=DX  
        ENDDO  
      ENDDO  
      DO J=1,JC  
        DO I=1,IC  
          IF(IJCT(I,J).GE.1.AND.IJCT(I,J).LE.4)THEN  
            TMPARA(I,J)=SQRTMP*DXP(LIJ(I,J))  
          ENDIF  
          IF(IJCT(I,J).EQ.5)THEN  
            TMPARA(I,J)=DXP(LIJ(I,J))  
          ENDIF  
        ENDDO  
      ENDDO  
      WRITE(1)TMPARA  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=DY  
        ENDDO  
      ENDDO  
      DO J=1,JC  
        DO I=1,IC  
          IF(IJCT(I,J).GE.1.AND.IJCT(I,J).LE.4)THEN  
            TMPARA(I,J)=SQRTMP*DYP(LIJ(I,J))  
          ENDIF  
          IF(IJCT(I,J).EQ.5)THEN  
            TMPARA(I,J)=DYP(LIJ(I,J))  
          ENDIF  
        ENDDO  
      ENDDO  
      WRITE(1)TMPARA  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
      DO J=1,JC  
        DO I=1,IC  
          IF(IJCT(I,J).GE.1.AND.IJCT(I,J).LE.5)THEN  
            TMPARA(I,J)=1.  
          ENDIF  
        ENDDO  
      ENDDO  
      WRITE(1)TMPARA  
      CLOSE(1)  
      OPEN(1,FILE='EFDCHYD.INP',FORM='UNFORMATTED',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      IF(ICRCA3.EQ.1)THEN  
        OPEN(1,FILE='EFDCHYD.ASC',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
      ENDIF  
      OPEN(1,FILE='INFLOW.DAT',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='HYDRLGY.INP',FORM='UNFORMATTED',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      IF(ICRCA3.EQ.1)THEN  
        OPEN(1,FILE='HYDRLGY.ASC',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
      ENDIF  
      OPEN(1,FILE='INTFLW.INP',FORM='UNFORMATTED',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      IF(ICRCA3.EQ.1)THEN  
        OPEN(1,FILE='INTFLW.ASC',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
      ENDIF  
  100 FORMAT(120X)  
  101 FORMAT(4I10)  
  102 FORMAT(' NROW,NCOL,NLAYR = ',3I10/)  
  103 FORMAT(' NO INTERNAL FLOWS, NINTFL (LINES) IN FLWMAP.INP = ',  
     &    I10/)  
  104 FORMAT(' ROW, COLUMN INDICES OF DUMP CELL = ',2I10/)  
  105 FORMAT(' SIMULATION STARTING TIME IN DAYS = ',F12.6/)  
  106 FORMAT(' TIME IN DAYS AT MIDDLE OF AVERAGING PERIOD = ',F12.6/)  
  110 FORMAT(' LOCATION OF INFLOWS ',/)  
  111 FORMAT(' INFLOW #   ROW INDEX   COLUMN INDEX ',/)  
  112 FORMAT(2X,I5,7X,I5,7X,I5)  
  120 FORMAT(F12.6,13F12.4)  
 1000 CONTINUE  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=(DT*FLOAT(N)+TCON*TBEGIN)/86400.  
      ELSE  
        TIME=TIMESEC/86400.  
      ENDIF  
      NMID=N-(NTSMMT/2)  
      TIMMID=(DT*FLOAT(NMID)+TCON*TBEGIN)/86400.  
C  
C **  WRITE TIME AT END OF AVERAGING PERIOD TO EFDCRCA.LOG  
C  
      OPEN(1,FILE='EFDCRCA.LOG',STATUS='UNKNOWN',POSITION='APPEND')  
      WRITE(1,106)TIMMID  
      CLOSE(1)  
C  
C **  WRITE INFLOWS AT END OF AVERAGING PERIOD TO INFLOW.DAT  
C  
      OPEN(1,FILE='INFLOW.DAT',STATUS='UNKNOWN',POSITION='APPEND')  
      DO NS=1,NQSIJ  
        QINRCA(NS)=0.  
      ENDDO  
      DO NS=1,NQSIJ  
        NQSTMP=NQSERQ(NS)  
        IF(NQSTMP.GT.0)THEN  
          DO K=1,KC  
            QINRCA(NS)=QINRCA(NS)+QSRTLPP(K,NQSTMP)+MAX(QSS(K,NS),0.)  
          ENDDO  
        ELSE  
          DO K=1,KC  
            QINRCA(NS)=QINRCA(NS)+MAX(QSS(K,NS),0.)  
          ENDDO  
        ENDIF  
      ENDDO  
      WRITE(1,120)TIME,(QINRCA(NS),NS=1,NQSIJ)  
      CLOSE(1)  
C  
C **  WRITE INTERNAL FLOWS TO INTFLW.INP  
C  
      OPEN(1,FILE='INTFLW.INP',FORM='UNFORMATTED',STATUS='UNKNOWN'  
     &    ,POSITION='APPEND')  
      IF(ICRCA3.EQ.1)THEN  
        OPEN(2,FILE='INTFLW.ASC',STATUS='UNKNOWN',POSITION='APPEND')  
        WRITE(2,106)TIME  
      ENDIF  
      DO K=1,KC  
        DO NS=1,NCRCA1  
          QINTFL(NS,K)=0.  
        ENDDO  
      ENDDO  
      NINTFL=0  
      IF(NQSIJ.GT.0)THEN  
        DO NS=1,NQSIJ  
          NINTFL=NINTFL+1  
          NQSTMP=NQSERQ(NS)  
          IF(NQSTMP.GT.0)THEN  
            DO K=1,KC  
              QINTFL(NINTFL,K)=QINTFL(NINTFL,K)+QSRTLPN(K,NQSTMP)  
     &            +MIN(QSS(K,NS),0.)  
            ENDDO  
          ELSE  
            DO K=1,KC  
              QINTFL(NINTFL,K)=QINTFL(NINTFL,K)+MIN(QSS(K,NS),0.)  
            ENDDO  
          ENDIF  
        ENDDO  
      ENDIF  
      IF(NQCTL.GT.0)THEN  
        DO NCTL=1,NQCTL  
          NINTFL=NINTFL+1  
          DO K=1,KC  
            QINTFL(NINTFL,K)=QINTFL(NINTFL,K)+QCTLTLP(K,NCTL)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(NQWR.GT.0)THEN  
        DO NWR=1,NQWR  
          NQSTMP=NQWRSERQ(NWR)  
          NINTFL=NINTFL+1  
          DO K=1,KC  
            IF(K.EQ.KQWRU(NWR))THEN  
              QINTFL(NINTFL,K)=QINTFL(NINTFL,K)+QWRSERTLP(NQSTMP)  
     &            +QWR(NWR)  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(MDCHH.GT.0)THEN  
        DO NMD=1,MDCHH  
          IF(IMDCHU(NMD).GT.1)THEN  
            NINTFL=NINTFL+1  
            DO K=1,KC  
              QINTFL(NINTFL,K)=QINTFL(NINTFL,K)+QCHNULP(NMD)*DZC(K)  
            ENDDO  
          ENDIF  
        ENDDO  
        DO NMD=1,MDCHH  
          IF(IMDCHV(NMD).GT.1)THEN  
            NINTFL=NINTFL+1  
            DO K=1,KC  
              QINTFL(NINTFL,K)=QINTFL(NINTFL,K)+QCHNULP(NMD)*DZC(K)  
            ENDDO  
          ENDIF  
        ENDDO  
      ENDIF  
      WRITE(1)QINTFL  
      IF(ICRCA3.EQ.1)THEN  
        WRITE(2,213)  
        DO NS=1,NINTFL  
          WRITE(2,211)NS,(QINTFL(NS,K),K=1,KC)  
        ENDDO  
      ENDIF  
      CLOSE(1)  
      IF(ICRCA3.EQ.1) CLOSE(2)  
C  
C **  WRITE TRANSPORTS TO EFDCHDY.INP  
C  
      OPEN(1,FILE='EFDCHYD.INP',FORM='UNFORMATTED',STATUS='UNKNOWN'  
     &    ,POSITION='APPEND')  
      WRITE(1)TIMMID  
      IF(ICRCA3.EQ.1)THEN  
        OPEN(2,FILE='EFDCHYD.ASC',STATUS='UNKNOWN',POSITION='APPEND')  
        WRITE(2,106) TIMMID  
      ENDIF  
      TAVGTMP=FLOAT(NTSMMT)*DT  
C  
C **  WRITE QX  
C  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
      DO KK=1,KC  
        K=KC+1-KK  
        DO L=2,LA  
          TMPVAL=DZC(K)*DYU(L)*UHLPF(L,K)  
          TMPARA(IL(L),JL(L))=TMPVAL  
          TVAR1E(L,K)=TMPVAL  
        ENDDO  
        WRITE(1)TMPARA  
      ENDDO  
      IF(ICRCA3.EQ.1)THEN  
        WRITE(2,201)  
        DO L=2,LA  
          WRITE(2,200)L,IL(L),JL(L),(TVAR1E(L,K),K=1,KC)  
        ENDDO  
        WRITE(2,210)  
      ENDIF  
C  
C **  WRITE QY  
C  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
      DO KK=1,KC  
        K=KC+1-KK  
        DO L=2,LA  
          TMPVAL=DZC(K)*DXV(L)*VHLPF(L,K)  
          TMPARA(IL(L),JL(L))=TMPVAL  
          TVAR1E(L,K)=TMPVAL  
        ENDDO  
        WRITE(1)TMPARA  
      ENDDO  
      IF(ICRCA3.EQ.1)THEN  
        WRITE(2,202)  
        DO L=2,LA  
          WRITE(2,200)L,IL(L),JL(L),(TVAR1E(L,K),K=1,KC)  
        ENDDO  
        WRITE(2,210)  
      ENDIF  
C  
C **  LOAD NET DEPTH INTEGRATED INFLOWS INTO TVAR3E AND  
C **  OUT FLOWS INTO TVAR3N  
C  
      DO L=2,LA  
        LN=LNC(L)  
        TVAR3E(L)=0.  
        TVAR3N(L)=0.  
        DO K=1,KC  
          TVAR3E(L)=TVAR3E(L)+DZC(K)*( DYU(L)*UHLPF(L,K)  
     &        +DXV(L)*VHLPF(L,K) )  
          TVAR3N(L)=TVAR3N(L)+DZC(K)*( DYU(L+1)*UHLPF(L+1,K)  
     &        +DXV(LN )*VHLPF(LN ,K) )  
        ENDDO  
      ENDDO  
      IF(MDCHH.GE.1)THEN  
        DO NMD=1,MDCHH  
          IF(MDCHTYP(NMD).EQ.1)THEN  
            TVAR3E(LMDCHH(NMD))=TVAR3E(LMDCHH(NMD))  
     &          +QCHNULP(NMD)  
            TVAR3N(LMDCHU(NMD))=TVAR3N(LMDCHU(NMD))  
     &          +QCHNULP(NMD)  
          ENDIF  
          IF(MDCHTYP(NMD).EQ.2)THEN  
            TVAR3E(LMDCHH(NMD))=TVAR3E(LMDCHH(NMD))  
     &          +QCHNVLP(NMD)  
            TVAR3N(LMDCHV(NMD))=TVAR3N(LMDCHV(NMD))  
     &          +QCHNVLP(NMD)  
          ENDIF  
          IF(MDCHTYP(NMD).EQ.3)THEN  
            TVAR3E(LMDCHH(NMD))=TVAR3E(LMDCHH(NMD))  
     &          +QCHNULP(NMD)+QCHNVLP(NMD)  
            TVAR3N(LMDCHU(NMD))=TVAR3N(LMDCHU(NMD))  
     &          +QCHNULP(NMD)+QCHNVLP(NMD)  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  WRITE QZ  
C  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
      IF(KC.GT.1)THEN  
        DO K=1,KS  
          DO L=2,LA  
            LN=LNC(L)  
            WLPF(L,K)=SWB(L)*( WLPF(L,K-1)  
     &          +DZC(K)*(DYU(L)*UHLPF(L,K)+DXV(L)*VHLPF(L,K))*DXYIP(L)  
     &          -TVAR3E(L)*DXYIP(L)  
     &          -DZC(K)*(DYU(L+1)*UHLPF(L+1,K)  
     &          +DXV(LN )*VHLPF(LN ,K))*DXYIP(L)  
     &          +TVAR3N(L)*DXYIP(L) )  
     &          +SWB(L)*( QSUMLPF(L,K)-DZC(K)*QSUMELPF(L) )*DXYIP(L)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(KC.GT.1)THEN  
        DO KK=0,KC  
          K=KC+1-KK  
          DO L=2,LA  
            TMPARA(IL(L),JL(L))=DXYP(L)*WLPF(L,K)  
          ENDDO  
          WRITE(1)TMPARA  
          IF(ICRCA3.EQ.1.AND.K.GE.1)THEN  
            DO L=2,LA  
              TVAR1E(L,K)=DXYP(L)*WLPF(L,K)  
            ENDDO  
          ENDIF  
        ENDDO  
      ENDIF  
      IF(ICRCA3.EQ.1.AND.KC.GT.1)THEN  
        WRITE(2,203)  
        DO L=2,LA  
          WRITE(2,200)L,IL(L),JL(L),(TVAR1E(L,K),K=1,KS)  
        ENDDO  
        WRITE(2,210)  
      ENDIF  
C  
C **  WRITE AHX  
C  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
C  
C **  WRITE AHY  
C  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
C  
C **  WRITE AZ  
C  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
      IF(KC.GT.1)THEN  
        DO KK=0,KC  
          K=KC+1-KK  
          IF(K.GE.1.AND.K.LE.KS)THEN  
            DO L=2,LA  
              TMPARA(IL(L),JL(L))=HP(L)*AB(L,K)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              TMPARA(IL(L),JL(L))=0.  
            ENDDO  
          ENDIF  
          WRITE(1)TMPARA  
          IF(ICRCA3.EQ.1.AND.K.GE.1)THEN  
            DO L=2,LA  
              TVAR1S(L,K)=HP(L)*AB(L,K)  
            ENDDO  
          ENDIF  
        ENDDO  
      ENDIF  
      IF(ICRCA3.EQ.1.AND.KC.GT.1)THEN  
        WRITE(2,206)  
        DO L=2,LA  
          WRITE(2,200)L,IL(L),JL(L),(TVAR1S(L,K),K=1,KS)  
        ENDDO  
        WRITE(2,210)  
      ENDIF  
C  
C **  WRITE WATER SURF ELV AT START OF AVG INTERVAL  
C  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
      DO L=2,LA  
        TMPARA(IL(L),JL(L))=HTMP(L)-HMP(L)  
        TVAR3W(L)=HTMP(L)-HMP(L)  
      ENDDO  
      WRITE(1)TMPARA  
      IF(ICRCA3.EQ.1)THEN  
        WRITE(2,215)  
        DO L=2,LA  
          WRITE(2,200)L,IL(L),JL(L),TVAR3W(L)  
        ENDDO  
        WRITE(2,210)  
      ENDIF  
C  
C **  CALCULATE TOTAL DEPTH AT END OF AVG INTERVAL  
C **  HTMP IS NEW DEPTH, TMPARA IS DETA  
C  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
C  
C **  UPDATE FOR TRANSPORTS, INTERNAL CHANNEL EXCHANGES AND  
C **  HYDROLOGY  
C  
      DO L=2,LA  
        TMPARA(IL(L),JL(L))=TAVGTMP*SPB(L)*DXYIP(L)*( TVAR3E(L)  
     &      -TVAR3N(L)+RAINLPF(L)-EVPSLPF(L)-RINFLPF(L) )  
      ENDDO  
C  
C **  UPDATE FOR EXTERNAL INFLOWS  
C  
      DO NS=1,NQSIJ  
        L=LIJ(IQS(NS),JQS(NS))  
        TMPARA(IQS(NS),JQS(NS))=TMPARA(IQS(NS),JQS(NS))  
     &      +TAVGTMP*SPB(L)*DXYIP(L)*QINRCA(NS)  
      ENDDO  
C  
C **  UPDATE FOR NON CHANNEL EXCHANGE INTERNAL FLOWS  
C  
      NINTFL=0  
      IF(NQSIJ.GT.0)THEN  
        DO NS=1,NQSIJ  
          L=LIJ(IQS(NS),JQS(NS))  
          NINTFL=NINTFL+1  
          DO K=1,KC  
            TMPARA(IQS(NS),JQS(NS))=  
     &          TMPARA(IQS(NS),JQS(NS))  
     &          -TAVGTMP*SPB(L)*DXYIP(L)*QINTFL(NINTFL,K)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(NQCTL.GT.0)THEN  
        DO NCTL=1,NQCTL  
          LU=LIJ(IQCTLU(NCTL),JQCTLU(NCTL))  
          LD=LIJ(IQCTLD(NCTL),JQCTLD(NCTL))  
          NINTFL=NINTFL+1  
          DO K=1,KC  
            TMPARA(IQCTLU(NCTL),JQCTLU(NCTL))=  
     &          TMPARA(IQCTLU(NCTL),JQCTLU(NCTL))  
     &          -TAVGTMP*SPB(LU)*DXYIP(LU)*QINTFL(NINTFL,K)  
            TMPARA(IQCTLD(NCTL),JQCTLD(NCTL))=  
     &          TMPARA(IQCTLD(NCTL),JQCTLD(NCTL))  
     &          +TAVGTMP*SPB(LD)*DXYIP(LD)*QINTFL(NINTFL,K)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(NQWR.GT.0)THEN  
        DO NWR=1,NQWR  
          LU=LIJ(IQWRU(NWR),JQWRU(NWR))  
          LD=LIJ(IQWRD(NWR),JQWRD(NWR))  
          NINTFL=NINTFL+1  
          DO K=1,KC  
            IF(K.EQ.KQWRU(NWR))THEN  
              TMPARA(IQWRU(NWR),JQWRU(NWR))=  
     &            TMPARA(IQWRU(NWR),JQWRU(NWR))  
     &            -TAVGTMP*SPB(LU)*DXYIP(LU)*QINTFL(NINTFL,KQWRU(NWR))  
              TMPARA(IQWRD(NWR),JQWRD(NWR))=  
     &            TMPARA(IQWRD(NWR),JQWRD(NWR))  
     &            +TAVGTMP*SPB(LD)*DXYIP(LD)*QINTFL(NINTFL,KQWRD(NWR))  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  COMPLETE UPDATE  
C  
      DO L=2,LA  
        HTMP(L)=(1.-SPB(L))*HP(L)+SPB(L)*( HTMP(L)  
     &      +TMPARA(IL(L),JL(L)) )  
        TVAR3S(L)=HTMP(L)-HMP(L)  
      ENDDO  
      WRITE(1)TMPARA  
      IF(ICRCA3.EQ.1)THEN  
        WRITE(2,216)  
        DO L=2,LA  
          WRITE(2,200)L,IL(L),JL(L),TMPARA(IL(L),JL(L))  
        ENDDO  
        WRITE(2,210)  
      ENDIF  
      IF(ICRCA3.EQ.1)THEN  
        WRITE(2,207)  
        DO L=2,LA  
          WRITE(2,200)L,IL(L),JL(L),TVAR3W(L),HTMP(L),  
     &        TMPARA(IL(L),JL(L)),TVAR3S(L)  
        ENDDO  
        WRITE(2,210)  
      ENDIF  
C  
C **  WRITE SALINITY  
C  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
      DO KK=1,KC  
        K=KC+1-KK  
        DO L=2,LA  
          TMPARA(IL(L),JL(L))=SALLPF(L,K)  
        ENDDO  
        WRITE(1)TMPARA  
      ENDDO  
      IF(ICRCA3.EQ.1)THEN  
        WRITE(2,208)  
        DO L=2,LA  
          WRITE(2,200)L,IL(L),JL(L),(SALLPF(L,K),K=1,KC)  
        ENDDO  
        WRITE(2,210)  
      ENDIF  
C  
C **  WRITE TEMPERATURE  
C  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
      DO KK=1,KC  
        K=KC+1-KK  
        DO L=2,LA  
          TMPARA(IL(L),JL(L))=TEMLPF(L,K)  
        ENDDO  
        WRITE(1)TMPARA  
      ENDDO  
      IF(ICRCA3.EQ.1)THEN  
        WRITE(2,209)  
        DO L=2,LA  
          WRITE(2,200)L,IL(L),JL(L),(TEMLPF(L,K),K=1,KC)  
        ENDDO  
        WRITE(2,210)  
      ENDIF  
      CLOSE(1)  
      IF(ICRCA3.EQ.1) CLOSE(2)  
C  
C **  WRITE HYDROLOGY  
C  
      OPEN(1,FILE='HYDRLGY.INP',FORM='UNFORMATTED',STATUS='UNKNOWN'  
     &    ,POSITION='APPEND')  
      WRITE(1)TIMMID  
      DO J=1,JC  
        DO I=1,IC  
          TMPARA(I,J)=0.  
        ENDDO  
      ENDDO  
      DO L=2,LA  
        TMPARA(IL(L),JL(L))=RAINLPF(L)  
      ENDDO  
      WRITE(1)TMPARA  
      DO L=2,LA  
        TMPARA(IL(L),JL(L))=EVPSLPF(L)  
      ENDDO  
      WRITE(1)TMPARA  
      DO L=2,LA  
        TMPARA(IL(L),JL(L))=RINFLPF(L)  
      ENDDO  
      WRITE(1)TMPARA  
      CLOSE(1)  
      IF(ICRCA3.EQ.1)THEN  
        OPEN(2,FILE='HYDRLGY.ASC',STATUS='UNKNOWN',POSITION='APPEND')  
        WRITE(2,106)TIME  
        WRITE(2,212)  
        DO L=2,LA  
          WRITE(2,200)L,IL(L),JL(L),RAINLPF(L),EVPSLPF(L),EVPGLPF(L),  
     &        RINFLPF(L),GWLPF(L)  
        ENDDO  
        CLOSE(2)  
      ENDIF  
  200 FORMAT(3I5,6F15.6)  
  201 FORMAT(' L,I(ROW),J(COL),QX(I,J,K),K=1,KC ',/)  
  202 FORMAT(' L,I(ROW),J(COL),QY(I,J,K),K=1,KC ',/)  
  203 FORMAT(' L,I(ROW),J(COL),QZ(I,J,K),K=1,KS ',/)  
  204 FORMAT(' L,I(ROW),J(COL),AX(I,J,K),K=1,KC ',/)  
  205 FORMAT(' L,I(ROW),J(COL),AY(I,J,K),K=1,KC ',/)  
  206 FORMAT(' L,I(ROW),J(COL),AZ(I,J,K),K=1,KS ',/)  
  207 FORMAT(' L,I(ROW),J(COL),SELS(I,J),SELE(I,J),DSEL(I,J) ',/)  
  208 FORMAT(' L,I(ROW),J(COL),SAL(I,J,K),K=1,KC ',/)  
  209 FORMAT(' L,I(ROW),J(COL),TEM(I,J,K),K=1,KC ',/)  
  210 FORMAT(//)  
  211 FORMAT(I5,2X,6E15.6)  
  212 FORMAT(' L,I(ROW),J(ROW),RAINLPF(I,J),EVPSLPF(I,J),EVPGLPF(I,J),  
     &    RINFLPF(I,J),GWLPF(I,J) ',/)  
  213 FORMAT(' NQINTFL,QINTFL ',/)  
  215 FORMAT(' L,I(ROW),J(COL),SURFELV START AVG INTERVAL',/)  
  216 FORMAT(' L,I(ROW),J(COL),DEL SURFELV OVER INTERVAL',/)  
      RETURN  
      END  

