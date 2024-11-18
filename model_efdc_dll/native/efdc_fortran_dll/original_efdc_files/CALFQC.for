      SUBROUTINE CALFQC(ISTL_,IS2TL_,MVAR,MO,CON,CON1,FQCPAD,QSUMPAD,  
     &    QSUMNAD)  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALFQC CALCULATES MASS SOURCES AND SINKS ASSOCIATED  
C **  WITH CONSTANT AND TIME SERIES INFLOWS AND OUTFLOWS; CONTROL  
C **  STRUCTURE INFLOWS AND OUTLOWS; WITHDRAWAL AND RETURN STRUCTURE  
C **  OUTFLOWS; AND  EMBEDED CHANNEL INFLOWS AND OUTFLOWS  
C  
      USE GLOBAL  

	INTEGER::L,K,ID,JD,KD,NWR,IU,JU,KU,LU,NS
	INTEGER::LD,NMD,NJP

      DIMENSION CON(LCM,KCM),CON1(LCM,KCM),FQCPAD(0:LCM1,KCM),
     &          QSUMNAD(0:LCM1,KCM),QSUMPAD(0:LCM1,KCM)
 
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::CONQ  
      REAL QVKTMP,QUKTMP

      L = 0
      QVKTMP = 0.0
      QUKTMP = 0.0

      IF(.NOT.ALLOCATED(CONQ))THEN
        ALLOCATE(CONQ(LCM,KCM))
        CONQ=0.0 
      ENDIF
C
      M=MO
      
      ! *** SELECTIVE ZEROING
      IF(KC.GT.1)THEN
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_LC(1,ithds)
         LL=jse_LC(2,ithds)
c

        IF(NGWSER.GT.0.OR.ISGWIT.NE.0)THEN
          DO L=LF,LL
            FQC(L,1)=0.  
          ENDDO  
        ENDIF

        ! *** ZERO EVAP/RAINFALL
        IF(MVAR.EQ.2)THEN        
          DO L=LF,LL
            FQC(L,KC)=0.  
          ENDDO  
          IF(ISADAC(MVAR).GE.2)THEN
            DO L=LF,LL
              FQCPAD(L,KC)=0.  
            ENDDO  
          ENDIF
          IF(ISADAC(MVAR).GT.0)THEN
            DO L=LF,LL
              QSUMPAD(L,KC)=0.  
            ENDDO  
          ENDIF
        ENDIF
c
      enddo
        
        ! *** ZERO ALL DEFINED BC'S
          DO K=1,KC
        DO NS=1,NBCS
          L=LBCS(NS)
            FQC(L,K)=0.  
            FQCPAD(L,K)=0  
            QSUMPAD(L,K)=0.  
            !QSUMNAD(L,K)=0.  
          ENDDO
        ENDDO

      ELSE
        FQC=0.
        IF(ISADAC(MVAR).GE.2)FQCPAD=0.
        QSUMPAD=0.
        !QSUMNAD=0.
      ENDIF
C
      IF(MVAR.EQ.8.AND.IWQPSL.NE.2) GOTO 1500  
C  
C **  INITIALIZE VOLUMETRIC SOURCE-SINK FLUXES AND AUXILLARY VARIABLES  
C  
      ! *** 3TL STANDARD & WATER QUALITY
      IF(ISTL_.EQ.3.OR.MVAR.EQ.8)THEN  
        IF(NGWSER.GT.0.OR.ISGWIT.NE.0)THEN
          DO L=1,LC  
            CONQ(L,1)=CON(L,1)  
          ENDDO  
        ENDIF

        ! *** ZERO EVAP/RAINFALL
        IF(MVAR.EQ.2)THEN        
          DO L=1,LC  
            CONQ(L,KC)=CON(L,KC)
          ENDDO  
        ENDIF
        
        ! *** INITIALIZE ALL DEFINED BC'S
        DO NS=1,NBCS
          L=LBCS(NS)
          DO K=1,KC
            CONQ(L,K)=CON(L,K)  
          ENDDO  
        ENDDO  
      ENDIF  
C
      ! *** 3TL CORRECTION STEP
      IF(ISTL_.EQ.2.AND.IS2TL_.EQ.0)THEN  
        IF(NGWSER.GT.0.OR.ISGWIT.NE.0)THEN
          DO L=1,LC  
            CONQ(L,1)=0.5*(CON(L,1)+CON1(L,1))  
          ENDDO  
        ENDIF

        ! *** ZERO EVAP/RAINFALL
        IF(MVAR.EQ.2)THEN        
          DO L=1,LC  
            CONQ(L,KC)=0.5*(CON(L,KC)+CON1(L,KC))  
          ENDDO  
        ENDIF
        
        ! *** INITIALIZE ALL DEFINED BC'S
        DO NS=1,NBCS
          L=LBCS(NS)
          DO K=1,KC
            CONQ(L,K)=0.5*(CON(L,K)+CON1(L,K))  
          ENDDO  
        ENDDO  

      ENDIF 
       
      ! *** 2TL STANDARD
      IF(ISTL_.EQ.2.AND.IS2TL_.EQ.1)THEN  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_LC(1,ithds)
         LL=jse_LC(2,ithds)
c
        IF(NGWSER.GT.0.OR.ISGWIT.NE.0)THEN
          DO L=LF,LL
            CONQ(L,1)=0.5*(3.*CON(L,1)-CON1(L,1))  
          ENDDO  
        ENDIF

        ! *** ZERO EVAP/RAINFALL
        IF(MVAR.EQ.2)THEN        
          DO L=LF,LL
            CONQ(L,KC)=0.5*(3.*CON(L,KC)-CON1(L,KC))  
          ENDDO  
        ENDIF
c
      enddo
        
        ! *** INITIALIZE ALL DEFINED BC'S
          DO K=1,KC
        DO NS=1,NBCS
          L=LBCS(NS)
            CONQ(L,K)=0.5*(3.*CON(L,K)-CON1(L,K))  
          ENDDO  
        ENDDO  

      ENDIF  
C
      IF(MVAR.EQ.4) GOTO 1000  
C
      IF(MVAR.EQ.8)THEN
        M=4+NTOX+NSED+NSND+MO
      ENDIF
C
C *********************************************************************C
C
C *** STANDARD VOLUMETRICS SOURCE SINK LOCATIONS (2TL)  
C
      IF(ISTL_.EQ.2.AND.IS2TL_.EQ.1)THEN  
        ! *** FLOW BOUNDARY CELLS (2TL)  
        DO NS=1,NQSIJ  
          L=LQS(NS)  
          NQSTMP=NQSERQ(NS)  
          NCSTMP=NCSERQ(NS,M)  
          DO K=1,KC  
            FQC(L,K)=FQC(L,K)  
     &          +MAX(QSS(K,NS),0.)*CQS(K,NS,M)  
     &          +MIN(QSS(K,NS),0.)*CONQ(L,K)  
     &          +MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,M)  
     &          +MIN(QSERCELL(K,NS),0.)*CONQ(L,K)  
            FQCPAD(L,K)=FQCPAD(L,K)  
     &          +MAX(QSS(K,NS),0.)*CQS(K,NS,M)  
     &          +MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,M)  
            QSUMPAD(L,K)=QSUMPAD(L,K)  
     &          +MAX(QSS(K,NS),0.)+MAX(QSERCELL(K,NS),0.)  
C            QSUMNAD(L,K)=QSUMNAD(L,K)  
C     &          +MIN(QSS(K,NS),0.)+MIN(QSERCELL(K,NS),0.)  
          ENDDO  
        ENDDO  

        ! ***  JET-PLUME VOLUMETRICS SOURCE SINK LOCATIONS (2TL)  
        IF(NQJPIJ.GT.0)THEN  
          DO NJP=1,NQJPIJ  
            IF(ICALJP(NJP).EQ.1)THEN  
              RPORTS=FLOAT(NPORTJP(NJP))  
              LJP=LIJ(IQJP(NJP),JQJP(NJP))  
              KTMP=KEFFJP(NJP)  
              ! ***  QVJPTMP=TIME SERIES DISCHARGE FROM JET-PLUME  
              QVJPTMP=0.  
              DO K=1,KC  
                QVJPTMP=QVJPTMP+QSERT(K,NQSERJP(NJP))
              ENDDO  

              ! QCJPTMP=ENTRAINMENT FLUX  
              QCJPTMP=0.  
              QVJPENT=0.  
              ! REMOVE ENTRAINMENT FLUX AND CALCULATE TOTAL ENTRAIMENT FLUX  
              DO K=1,KC  
                FQC(LJP,K)=FQC(LJP,K)-RPORTS*QJPENT(K,NJP)*CONQ(LJP,K)  
                QCJPTMP=QCJPTMP+QJPENT(K,NJP)*CONQ(LJP,K)  
                QVJPENT=QVJPENT+QJPENT(K,NJP)  
C                QSUMNAD(LJP,K)=QSUMNAD(LJP,K)-RPORTS*QJPENT(K,NJP)  
              ENDDO  

              ! PLACE JET FLUX AND ENTRAINMENT FLUX IS EFFECTIVE LAYER  
              FQC(LJP,KTMP)=FQC(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QQCJP(NJP)*CQCJP(1,NJP,M)  
     &            +RPORTS*QVJPTMP*CSERT(1,NCSERJP(NJP,M),M)  
              FQCPAD(LJP,KTMP)=FQCPAD(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QQCJP(NJP)*CQCJP(1,NJP,M)  
     &            +RPORTS*QVJPTMP*CSERT(1,NCSERJP(NJP,M),M)  
              QSUMPAD(LJP,KTMP)=QSUMPAD(LJP,KTMP)+RPORTS*QVJPENT  
     &            +RPORTS*QQCJP(NJP)+RPORTS*QVJPTMP  
            ENDIF  
            IF(ICALJP(NJP).EQ.2)THEN  
              RPORTS=FLOAT(NPORTJP(NJP))  
              LJP=LIJ(IQJP(NJP),JQJP(NJP))  
              KTMP=KEFFJP(NJP)  
              NS=NQWRSERJP(NJP)  
              LU=LIJ(IUPCJP(NJP),JUPCJP(NJP))  
              KU=KUPCJP(NJP)  
              CONUP=CONQ(LU,KU)  
              QCJPTMP=0.  
              QVJPENT=0.  

              ! REMOVE ENTRAIMENT FLUX AND CALCULATE TOTAL ENTRAINMENT  
              DO K=1,KC  
                FQC(LJP,K)=FQC(LJP,K)-RPORTS*QJPENT(K,NJP)*CONQ(LJP,K)  
                QCJPTMP=QCJPTMP+QJPENT(K,NJP)*CONQ(LJP,K)  
                QVJPENT=QVJPENT+QJPENT(K,NJP)  
C                QSUMNAD(LJP,K)=QSUMNAD(LJP,K)-RPORTS*QJPENT(K,NJP)  
              ENDDO  

              !  PLACE ENTRAINMENT, CONSTANT AND TIME SERIES FLUXES IN EFFECTIVE CELL  
              FQC(LJP,KTMP)=FQC(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QWRCJP(NJP)*(CWRCJP(NJP,M)+CONUP)  
     &            +RPORTS*QWRSERT(NS)*(CQWRSERT(NS,M)+CONUP)  
              FQCPAD(LJP,KTMP)=FQCPAD(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QWRCJP(NJP)*(CWRCJP(NJP,M)+CONUP)  
     &            +RPORTS*QWRSERT(NS)*(CQWRSERT(NS,M)+CONUP)  
              QSUMPAD(LJP,KTMP)=QSUMPAD(LJP,KTMP)+RPORTS*QVJPENT  
     &            +RPORTS*QWRCJP(NJP)+RPORTS*QWRSERT(NS)  

              ! REMOVAL WITHDRAWAL FROM UPSTREAM CELL  
              FQC(LU,KU)=FQC(LU,KU)  
     &            -RPORTS*QWRCJP(NJP)*CONUP  
     &            -RPORTS*QWRSERT(NS)*CONUP  
C              QSUMNAD(LU,KU)=QSUMNAD(LU,KU)  
C     &            -RPORTS*QWRCJP(NJP)-RPORTS*QWRSERT(NS)  
            ENDIF  
          ENDDO  
        ENDIF  

        ! *** CONTROL STRUCTURES (2TL)  
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
            FQC(LU,K)=FQC(LU,K)  
     &          -QCTLT(K,NCTL)*CONQ(LU,K)  
            FQC(LD,K)=FQC(LD,K)  
     &          +RQWD*QCTLT(K,NCTL)*CONQ(LU,K)  
            FQCPAD(LD,K)=FQCPAD(LD,K)  
     &          +RQWD*QCTLT(K,NCTL)*CONQ(LU,K)  
            QSUMPAD(L,K)=QSUMPAD(L,K)  
     &          +RQWD*QCTLT(K,NCTL)  
C            QSUMNAD(L,K)=QSUMNAD(L,K)  
C     &          -QCTLT(K,NCTL)  
          ENDDO  
        ENDDO  

        ! *** WITHDRAWAL CONCENTRATION RISE RETURN (2TL)  
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
          FQC(LU,KU)=FQC(LU,KU)  
     &        -(QWR(NWR)+QWRSERT(NQSTMP))*CONQ(LU,KU)  
          FQC(LD,KD)=FQC(LD,KD)  
     &        +QWR(NWR)*(CONQ(LU,KU)+CQWR(NWR,M))  
     &        +QWRSERT(NQSTMP)*(CONQ(LU,KU)+CQWRSERT(NCSTMP,M))  
          FQCPAD(LD,KD)=FQCPAD(LD,KD)  
     &        +QWR(NWR)*(CONQ(LU,KU)+CQWR(NWR,M))  
     &        +QWRSERT(NQSTMP)*(CONQ(LU,KU)+CQWRSERT(NCSTMP,M))  
          QSUMPAD(LD,KD)=QSUMPAD(LD,KD)  
     &        +QWR(NWR)+QWRSERT(NQSTMP)  
C          QSUMNAD(LU,KU)=QSUMNAD(LU,KU)  
C     &        -(QWR(NWR)+QWRSERT(NQSTMP))  
        ENDDO  

        ! *** SUBGRID SCALE CHANNEL EXCHANGE (2TL)  
        IF(MDCHH.GE.1)THEN  
          DO K=1,KC  
            DO NMD=1,MDCHH  
              LMDCHHT=LMDCHH(NMD)  
              LMDCHUT=LMDCHU(NMD)  
              LMDCHVT=LMDCHV(NMD)  
              IF(MDCHTYP(NMD).EQ.1)THEN  
                QUKTMP=QCHANU(NMD)*DZC(K)  
                QVKTMP=0.  
              ENDIF  
              IF(MDCHTYP(NMD).EQ.2)THEN  
                QVKTMP=QCHANV(NMD)*DZC(K)  
                QUKTMP=0.  
              ENDIF  
              IF(MDCHTYP(NMD).EQ.3)THEN  
                QUKTMP=QCHANU(NMD)*DZC(K)  
                QVKTMP=QCHANV(NMD)*DZC(K)  
              ENDIF  
              FQC(LMDCHHT,K)=FQC(LMDCHHT,K)  
     &            +MAX(QUKTMP,0.)*CONQ(LMDCHUT,K)  
     &            +MIN(QUKTMP,0.)*CONQ(LMDCHHT,K)  
     &            +MAX(QVKTMP,0.)*CONQ(LMDCHVT,K)  
     &            +MIN(QVKTMP,0.)*CONQ(LMDCHHT,K)  
              FQC(LMDCHUT,K)=FQC(LMDCHUT,K)  
     &            -MAX(QUKTMP,0.)*CONQ(LMDCHUT,K)  
     &            -MIN(QUKTMP,0.)*CONQ(LMDCHHT,K)  
              FQC(LMDCHVT,K)=FQC(LMDCHVT,K)  
     &            -MAX(QVKTMP,0.)*CONQ(LMDCHVT,K)  
     &            -MIN(QVKTMP,0.)*CONQ(LMDCHHT,K)  
            ENDDO  
          ENDDO  
        ENDIF  

        ! *** GROUNDWATER, EVAP, RAINFALL (2TL)  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        IF(ISGWIE.NE.0)THEN  
          DO L=LF,LL
            FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)  
          ENDDO  
        ENDIF  
        
        ! *** ZONED SEEPAGE (2TL)  
        IF(ISGWIT.EQ.3)THEN  
          DO L=LF,LL
            IF(H1P(L).GT.HDRY)THEN  
              FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)
            ENDIF 
          ENDDO  
        ENDIF  
        
        ! *** TEMPERATURE ADJUSTMENTS FOR RAINFALL & EVAPORATION
        IF(M.EQ.2)THEN 
           
          IF(ISTOPT(2).EQ.0.OR.ISTOPT(2).EQ.3)THEN  
            DO L=LF,LL
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TEMO*DXYP(L)  
            ENDDO  
          ENDIF  
          IF(ISTOPT(2).EQ.1.OR.ISTOPT(2).EQ.2.OR.ISTOPT(2).EQ.4)THEN  
            DO L=LF,LL
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)  
              FQCPAD(L,KC)=FQCPAD(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)
              QSUMPAD(L,KC)=QSUMPAD(L,KC)+RAINT(L)*DXYP(L)
            ENDDO  
          ENDIF  
        ENDIF  
        IF(M.EQ.2)THEN  
          IF(ISTOPT(2).EQ.0)THEN  
            DO L=LF,LL
              FQC(L,KC)=FQC(L,KC)-EVAPSW(L)*CONQ(L,KC)  
            ENDDO  
          ENDIF  
        ENDIF  
c
      enddo
      ENDIF 
C
C *********************************************************************C
C
C *** 3TL CORRECTOR VOLUMETRICS SOURCE SINK LOCATIONS
C
      IF(ISTL_.EQ.2.AND.IS2TL_.EQ.0)THEN  
        ! *** FLOW BOUNDARY CELLS (3TL CORRECTOR)
        DO NS=1,NQSIJ  
          L=LQS(NS)  
          NQSTMP=NQSERQ(NS)  
          NCSTMP=NCSERQ(NS,M)  
          DO K=1,KC  
            FQC(L,K)=FQC(L,K)  
     &          +MAX(QSS(K,NS),0.)*CQS(K,NS,M)  
     &          +MIN(QSS(K,NS),0.)*CONQ(L,K)  
     &          +MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,M)  
     &          +MIN(QSERCELL(K,NS),0.)*CONQ(L,K)  
            FQCPAD(L,K)=FQCPAD(L,K)  
     &          +MAX(QSS(K,NS),0.)*CQS(K,NS,M)  
     &          +MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,M)  
            QSUMPAD(L,K)=QSUMPAD(L,K)  
     &          +MAX(QSS(K,NS),0.)+MAX(QSERCELL(K,NS),0.)  
C            QSUMNAD(L,K)=QSUMNAD(L,K)  
C     &          +MIN(QSS(K,NS),0.)+MIN(QSERCELL(K,NS),0.)  
          ENDDO  
        ENDDO  

        ! *** JET-PLUME VOLUMETRICS SOURCE SINK LOCATIONS (3TL CORRECTOR)  
        IF(NQJPIJ.GT.0)THEN  
          DO NJP=1,NQJPIJ  
            IF(ICALJP(NJP).EQ.1)THEN  
              RPORTS=FLOAT(NPORTJP(NJP))  
              LJP=LIJ(IQJP(NJP),JQJP(NJP))  
              KTMP=KEFFJP(NJP)  

              ! QVJPTMP=TIME SERIES DISCHARGE FROM JET-PLUME  
              QVJPTMP=0.  
              DO K=1,KC  
                QVJPTMP=QVJPTMP+QSERT(K,NQSERJP(NJP))
              ENDDO  
  
              ! QCJPTMP=ENTRAINMENT FLUX  
              QCJPTMP=0.  
              QVJPENT=0.  

              ! REMOVE ENTRAINMENT FLUX AND CALCULATE TOTAL ENTRAIMENT FLUX  
              DO K=1,KC  
                FQC(LJP,K)=FQC(LJP,K)-RPORTS*QJPENT(K,NJP)*CONQ(LJP,K)  
                QCJPTMP=QCJPTMP+QJPENT(K,NJP)*CONQ(LJP,K)  
                QVJPENT=QVJPENT+QJPENT(K,NJP)  
C                QSUMNAD(LJP,K)=QSUMNAD(LJP,K)-RPORTS*QJPENT(K,NJP)  
              ENDDO  

              ! PLACE JET FLUX AND ENTRAINMENT FLUX IS EFFECTIVE LAYER  
              FQC(LJP,KTMP)=FQC(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QQCJP(NJP)*CQCJP(1,NJP,M)  
     &            +RPORTS*QVJPTMP*CSERT(1,NCSERJP(NJP,M),M)  
              FQCPAD(LJP,KTMP)=FQCPAD(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QQCJP(NJP)*CQCJP(1,NJP,M)  
     &            +RPORTS*QVJPTMP*CSERT(1,NCSERJP(NJP,M),M)  
              QSUMPAD(LJP,KTMP)=QSUMPAD(LJP,KTMP)+RPORTS*QVJPENT  
     &            +RPORTS*QQCJP(NJP)+RPORTS*QVJPTMP  
            ENDIF  
            IF(ICALJP(NJP).EQ.2)THEN  
              RPORTS=FLOAT(NPORTJP(NJP))  
              LJP=LIJ(IQJP(NJP),JQJP(NJP))  
              KTMP=KEFFJP(NJP)  
              NS=NQWRSERJP(NJP)  
              LU=LIJ(IUPCJP(NJP),JUPCJP(NJP))  
              KU=KUPCJP(NJP)  
              CONUP=CONQ(LU,KU)  
              QCJPTMP=0.  
              QVJPENT=0.  

              ! REMOVE ENTRAIMENT FLUX AND CALCULATE TOTAL ENTRAINMENT  
              DO K=1,KC  
                FQC(LJP,K)=FQC(LJP,K)-RPORTS*QJPENT(K,NJP)*CONQ(LJP,K)  
                QCJPTMP=QCJPTMP+QJPENT(K,NJP)*CONQ(LJP,K)  
                QVJPENT=QVJPENT+QJPENT(K,NJP)  
C                QSUMNAD(LJP,K)=QSUMNAD(LJP,K)-RPORTS*QJPENT(K,NJP)  
              ENDDO  

              !  PLACE ENTRAINMENT, CONSTANT AND TIME SERIES FLUXES IN EFFECTIVE CELL  
              FQC(LJP,KTMP)=FQC(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QWRCJP(NJP)*(CWRCJP(NJP,M)+CONUP)  
     &            +RPORTS*QWRSERT(NS)*(CQWRSERT(NS,M)+CONUP)  
              FQCPAD(LJP,KTMP)=FQCPAD(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QWRCJP(NJP)*(CWRCJP(NJP,M)+CONUP)  
     &            +RPORTS*QWRSERT(NS)*(CQWRSERT(NS,M)+CONUP)  
              QSUMPAD(LJP,KTMP)=QSUMPAD(LJP,KTMP)+RPORTS*QVJPENT  
     &            +RPORTS*QWRCJP(NJP)+RPORTS*QWRSERT(NS)  

              ! REMOVAL WITHDRAWAL FROM UPSTREAM CELL  
              FQC(LU,KU)=FQC(LU,KU)  
     &            -RPORTS*QWRCJP(NJP)*CONUP  
     &            -RPORTS*QWRSERT(NS)*CONUP  
C              QSUMNAD(LU,KU)=QSUMNAD(LU,KU)  
C     &            -RPORTS*QWRCJP(NJP)-RPORTS*QWRSERT(NS)  
            ENDIF  
          ENDDO  
        ENDIF  

        ! *** CONTROL STRUCTURES (3TL CORRECTOR)  
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
            FQC(LU,K)=FQC(LU,K)  
     &          -QCTLT(K,NCTL)*CONQ(LU,K)  
            FQC(LD,K)=FQC(LD,K)  
     &          +RQWD*QCTLT(K,NCTL)*CONQ(LU,K)  
            FQCPAD(LD,K)=FQCPAD(LD,K)  
     &          +RQWD*QCTLT(K,NCTL)*CONQ(LU,K)  
            QSUMPAD(L,K)=QSUMPAD(L,K)  
     &          +RQWD*QCTLT(K,NCTL)  
C            QSUMNAD(L,K)=QSUMNAD(L,K)  
C     &          -QCTLT(K,NCTL)  
          ENDDO  
        ENDDO  

        ! ***  WITHDRAWAL CONCENTRATION RISE RETURN (3TL CORRECTOR)  
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
          FQC(LU,KU)=FQC(LU,KU)  
     &        -(QWR(NWR)+QWRSERT(NQSTMP))*CONQ(LU,KU)  
          FQC(LD,KD)=FQC(LD,KD)  
     &        +QWR(NWR)*(CONQ(LU,KU)+CQWR(NWR,M))  
     &        +QWRSERT(NQSTMP)*(CONQ(LU,KU)+CQWRSERT(NCSTMP,M))  
          FQCPAD(LD,KD)=FQCPAD(LD,KD)  
     &        +QWR(NWR)*(CONQ(LU,KU)+CQWR(NWR,M))  
     &        +QWRSERT(NQSTMP)*(CONQ(LU,KU)+CQWRSERT(NCSTMP,M))  
          QSUMPAD(LD,KD)=QSUMPAD(LD,KD)  
     &        +QWR(NWR)+QWRSERT(NQSTMP)  
C          QSUMNAD(LU,KU)=QSUMNAD(LU,KU)  
C     &        -(QWR(NWR)+QWRSERT(NQSTMP))  
        ENDDO  

        ! ***  SUBGRID SCALE CHANNEL EXCHANGE (3TL CORRECTOR)  
        IF(MDCHH.GE.1)THEN  
          DO K=1,KC  
            DO NMD=1,MDCHH  
              LMDCHHT=LMDCHH(NMD)  
              LMDCHUT=LMDCHU(NMD)  
              LMDCHVT=LMDCHV(NMD)  
              IF(MDCHTYP(NMD).EQ.1)THEN  
                QUKTMP=QCHANU(NMD)*DZC(K)  
                QVKTMP=0.  
              ENDIF  
              IF(MDCHTYP(NMD).EQ.2)THEN  
                QVKTMP=QCHANV(NMD)*DZC(K)  
                QUKTMP=0.  
              ENDIF  
              IF(MDCHTYP(NMD).EQ.3)THEN  
                QUKTMP=QCHANU(NMD)*DZC(K)  
                QVKTMP=QCHANV(NMD)*DZC(K)  
              ENDIF  
              FQC(LMDCHHT,K)=FQC(LMDCHHT,K)  
     &            +MAX(QUKTMP,0.)*CONQ(LMDCHUT,K)  
     &            +MIN(QUKTMP,0.)*CONQ(LMDCHHT,K)  
     &            +MAX(QVKTMP,0.)*CONQ(LMDCHVT,K)  
     &            +MIN(QVKTMP,0.)*CONQ(LMDCHHT,K)  
              FQC(LMDCHUT,K)=FQC(LMDCHUT,K)  
     &            -MAX(QUKTMP,0.)*CONQ(LMDCHUT,K)  
     &            -MIN(QUKTMP,0.)*CONQ(LMDCHHT,K)  
              FQC(LMDCHVT,K)=FQC(LMDCHVT,K)  
     &            -MAX(QVKTMP,0.)*CONQ(LMDCHVT,K)  
     &            -MIN(QVKTMP,0.)*CONQ(LMDCHHT,K)  
            ENDDO  
          ENDDO  
        ENDIF  

        ! ***  GROUNDWATER, EVAP, RAINFALL (3TL CORRECTOR)  
        IF(ISGWIE.NE.0)THEN  
          DO L=2,LA  
            FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)  
          ENDDO  
        ENDIF  
        
        ! *** ZONED SEEPAGE (3TL)  
        IF(ISGWIT.EQ.3)THEN  
          DO L=2,LA
            IF(H1P(L).GT.HDRY)THEN  
              FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)
            ENDIF 
          ENDDO  
        ENDIF  
        
        ! *** TEMPERATURE ADJUSTMENTS FOR RAINFALL & EVAPORATION
        IF(M.EQ.2)THEN  
          IF(ISTOPT(2).EQ.0.OR.ISTOPT(2).EQ.3)THEN  
            DO L=2,LA  
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TEMO*DXYP(L)  
            ENDDO  
          ENDIF  
          IF(ISTOPT(2).EQ.1.OR.ISTOPT(2).EQ.2.OR.ISTOPT(2).EQ.4)THEN  
            DO L=2,LA  
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)  
              FQCPAD(L,KC)=FQCPAD(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)  
              QSUMPAD(L,KC)=QSUMPAD(L,KC)+RAINT(L)*DXYP(L)  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(M.EQ.2)THEN  
          IF(ISTOPT(2).EQ.0)THEN  
            DO L=2,LA  
              FQC(L,KC)=FQC(L,KC)-EVAPSW(L)*CONQ(L,KC)  
            ENDDO  
          ENDIF  
        ENDIF  
      ENDIF  
C *********************************************************************C
C  
C **  STANDARD VOLUMETRICS SOURCE SINK LOCATIONS (3TL)  
C  
      IF(ISTL_.EQ.3)THEN  
        ! *** FLOW BOUNDARY CELLS (3TL)
        DO NS=1,NQSIJ  
          L=LQS(NS)  
          NQSTMP=NQSERQ(NS)  
          NCSTMP=NCSERQ(NS,M)  
          DO K=1,KC  
            FQC(L,K)=FQC(L,K)  
     &          +MAX(QSS(K,NS),0.)*CQS(K,NS,M)  
     &          +MIN(QSS(K,NS),0.)*CONQ(L,K)  
     &          +MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,M)  
     &          +MIN(QSERCELL(K,NS),0.)*CONQ(L,K)  
            FQCPAD(L,K)=FQCPAD(L,K)  
     &          +MAX(QSS(K,NS),0.)*CQS(K,NS,M)  
     &          +MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,M)  
            QSUMPAD(L,K)=QSUMPAD(L,K)  
     &          +MAX(QSS(K,NS),0.)+MAX(QSERCELL(K,NS),0.)  
C            QSUMNAD(L,K)=QSUMNAD(L,K)  
C     &          +MIN(QSS(K,NS),0.)+MIN(QSERCELL(K,NS),0.)  
          ENDDO  
        ENDDO  

        ! ***  JET-PLUME VOLUMETRICS SOURCE SINK LOCATIONS (3TL)  
        IF(NQJPIJ.GT.0)THEN  
          DO NJP=1,NQJPIJ  
            IF(ICALJP(NJP).EQ.1)THEN  
              RPORTS=FLOAT(NPORTJP(NJP))  
              LJP=LIJ(IQJP(NJP),JQJP(NJP))  
              KTMP=KEFFJP(NJP)  

              ! QVJPTMP=TIME SERIES DISCHARGE FROM JET-PLUME  
              QVJPTMP=0.  
              DO K=1,KC  
                QVJPTMP=QVJPTMP+QSERT(K,NQSERJP(NJP))
              ENDDO  

              ! QCJPTMP=ENTRAINMENT FLUX  
              QCJPTMP=0.  
              QVJPENT=0.  

              ! REMOVE ENTRAINMENT FLUX AND CALCULATE TOTAL ENTRAIMENT FLUX  
              DO K=1,KC  
                FQC(LJP,K)=FQC(LJP,K)-RPORTS*QJPENT(K,NJP)*CONQ(LJP,K)  
                QCJPTMP=QCJPTMP+QJPENT(K,NJP)*CONQ(LJP,K)  
                QVJPENT=QVJPENT+QJPENT(K,NJP)  
C                QSUMNAD(LJP,K)=QSUMNAD(LJP,K)-RPORTS*QJPENT(K,NJP)  
              ENDDO  

              ! PLACE JET FLUX AND ENTRAINMENT FLUX IS EFFECTIVE LAYER  
              FQC(LJP,KTMP)=FQC(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QQCJP(NJP)*CQCJP(1,NJP,M)  
     &            +RPORTS*QVJPTMP*CSERT(1,NCSERJP(NJP,M),M)  
              FQCPAD(LJP,KTMP)=FQCPAD(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QQCJP(NJP)*CQCJP(1,NJP,M)  
     &            +RPORTS*QVJPTMP*CSERT(1,NCSERJP(NJP,M),M)  
              QSUMPAD(LJP,KTMP)=QSUMPAD(LJP,KTMP)+RPORTS*QVJPENT  
     &            +RPORTS*QQCJP(NJP)+RPORTS*QVJPTMP  
            ENDIF  
            IF(ICALJP(NJP).EQ.2)THEN  
              RPORTS=FLOAT(NPORTJP(NJP))  
              LJP=LIJ(IQJP(NJP),JQJP(NJP))  
              KTMP=KEFFJP(NJP)  
              NS=NQWRSERJP(NJP)  
              LU=LIJ(IUPCJP(NJP),JUPCJP(NJP))  
              KU=KUPCJP(NJP)  
              CONUP=CONQ(LU,KU)  
              QCJPTMP=0.  
              QVJPENT=0.  

              ! REMOVE ENTRAIMENT FLUX AND CALCULATE TOTAL ENTRAINMENT  
              DO K=1,KC  
                FQC(LJP,K)=FQC(LJP,K)-RPORTS*QJPENT(K,NJP)*CONQ(LJP,K)  
                QCJPTMP=QCJPTMP+QJPENT(K,NJP)*CONQ(LJP,K)  
                QVJPENT=QVJPENT+QJPENT(K,NJP)  
C                QSUMNAD(LJP,K)=QSUMNAD(LJP,K)-RPORTS*QJPENT(K,NJP)  
              ENDDO  

              ! PLACE ENTRAINMENT, CONSTANT AND TIME SERIES FLUXES IN EFFECTIVE CELL  
              FQC(LJP,KTMP)=FQC(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QWRCJP(NJP)*(CWRCJP(NJP,M)+CONUP)  
     &            +RPORTS*QWRSERT(NS)*(CQWRSERT(NS,M)+CONUP)  
              FQCPAD(LJP,KTMP)=FQCPAD(LJP,KTMP)+RPORTS*QCJPTMP  
     &            +RPORTS*QWRCJP(NJP)*(CWRCJP(NJP,M)+CONUP)  
     &            +RPORTS*QWRSERT(NS)*(CQWRSERT(NS,M)+CONUP)  
              QSUMPAD(LJP,KTMP)=QSUMPAD(LJP,KTMP)+RPORTS*QVJPENT  
     &            +RPORTS*QWRCJP(NJP)+RPORTS*QWRSERT(NS)  

              ! REMOVAL WITHDRAWAL FROM UPSTREAM CELL  
              FQC(LU,KU)=FQC(LU,KU)  
     &            -RPORTS*QWRCJP(NJP)*CONUP  
     &            -RPORTS*QWRSERT(NS)*CONUP  
C              QSUMNAD(LU,KU)=QSUMNAD(LU,KU)  
C     &            -RPORTS*QWRCJP(NJP)-RPORTS*QWRSERT(NS)  
            ENDIF  
          ENDDO  
        ENDIF  

        ! ***  CONTROL STRUCTURES (3TL)  
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
            FQC(LU,K)=FQC(LU,K)  
     &          -QCTLT(K,NCTL)*CONQ(LU,K)  
            FQC(LD,K)=FQC(LD,K)  
     &          +RQWD*QCTLT(K,NCTL)*CONQ(LU,K)  
            FQCPAD(LD,K)=FQCPAD(LD,K)  
     &          +RQWD*QCTLT(K,NCTL)*CONQ(LU,K)  
            QSUMPAD(L,K)=QSUMPAD(L,K)  
     &          +RQWD*QCTLT(K,NCTL)  
C            QSUMNAD(L,K)=QSUMNAD(L,K)  
C     &          -QCTLT(K,NCTL)  
          ENDDO  
        ENDDO  

        ! ***  WITHDRAWAL CONCENTRATION RISE RETURN (3TL)  
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
          FQC(LU,KU)=FQC(LU,KU)  
     &        -(QWR(NWR)+QWRSERT(NQSTMP))*CONQ(LU,KU)  
          FQC(LD,KD)=FQC(LD,KD)  
     &        +QWR(NWR)*(CONQ(LU,KU)+CQWR(NWR,M))  
     &        +QWRSERT(NQSTMP)*(CONQ(LU,KU)+CQWRSERT(NCSTMP,M))  
          FQCPAD(LD,KD)=FQCPAD(LD,KD)  
     &        +QWR(NWR)*(CONQ(LU,KU)+CQWR(NWR,M))  
     &        +QWRSERT(NQSTMP)*(CONQ(LU,KU)+CQWRSERT(NCSTMP,M))  
          QSUMPAD(LD,KD)=QSUMPAD(LD,KD)  
     &        +QWR(NWR)+QWRSERT(NQSTMP)  
C          QSUMNAD(LU,KU)=QSUMNAD(LU,KU)  
C     &        -(QWR(NWR)+QWRSERT(NQSTMP))  
        ENDDO  

        ! ***  SUBGRID SCALE CHANNEL EXCHANGE (3TL)  
        IF(MDCHH.GE.1)THEN  
          DO K=1,KC  
            DO NMD=1,MDCHH  
              LMDCHHT=LMDCHH(NMD)  
              LMDCHUT=LMDCHU(NMD)  
              LMDCHVT=LMDCHV(NMD)  
              IF(MDCHTYP(NMD).EQ.1)THEN  
                QUKTMP=QCHANU(NMD)*DZC(K)  
                QVKTMP=0.  
              ENDIF  
              IF(MDCHTYP(NMD).EQ.2)THEN  
                QVKTMP=QCHANV(NMD)*DZC(K)  
                QUKTMP=0.  
              ENDIF  
              IF(MDCHTYP(NMD).EQ.3)THEN  
                QUKTMP=QCHANU(NMD)*DZC(K)  
                QVKTMP=QCHANV(NMD)*DZC(K)  
              ENDIF  
              FQC(LMDCHHT,K)=FQC(LMDCHHT,K)  
     &            +MAX(QUKTMP,0.)*CONQ(LMDCHUT,K)  
     &            +MIN(QUKTMP,0.)*CONQ(LMDCHHT,K)  
     &            +MAX(QVKTMP,0.)*CONQ(LMDCHVT,K)  
     &            +MIN(QVKTMP,0.)*CONQ(LMDCHHT,K)  
              FQC(LMDCHUT,K)=FQC(LMDCHUT,K)  
     &            -MAX(QUKTMP,0.)*CONQ(LMDCHUT,K)  
     &            -MIN(QUKTMP,0.)*CONQ(LMDCHHT,K)  
              FQC(LMDCHVT,K)=FQC(LMDCHVT,K)  
     &            -MAX(QVKTMP,0.)*CONQ(LMDCHVT,K)  
     &            -MIN(QVKTMP,0.)*CONQ(LMDCHHT,K)  
            ENDDO  
          ENDDO  
        ENDIF  

        ! ***  GROUNDWATER, EVAP, RAINFALL (3TL)  
        IF(ISGWIE.NE.0)THEN  
          DO L=2,LA  
            FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)  
          ENDDO  
        ENDIF  
        
        ! *** ZONED SEEPAGE (3TL)  
        IF(ISGWIT.EQ.3)THEN  
          DO L=2,LA
            IF(H1P(L).GT.HDRY)THEN  
              FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)
            ENDIF 
          ENDDO  
        ENDIF  
        
        ! *** TEMPERATURE ADJUSTMENTS FOR RAINFALL & EVAPORATION
        IF(M.EQ.2)THEN  
          IF(ISTOPT(2).EQ.0.OR.ISTOPT(2).EQ.3)THEN  
            DO L=2,LA  
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TEMO*DXYP(L)  
            ENDDO  
          ENDIF  
          IF(ISTOPT(2).EQ.1.OR.ISTOPT(2).EQ.2.OR.ISTOPT(2).EQ.4)THEN  
            DO L=2,LA  
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)  
              FQCPAD(L,KC)=FQCPAD(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)  
              QSUMPAD(L,KC)=QSUMPAD(L,KC)+RAINT(L)*DXYP(L)  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(M.EQ.2)THEN  
          IF(ISTOPT(2).EQ.0)THEN  
            DO L=2,LA  
              FQC(L,KC)=FQC(L,KC)-EVAPSW(L)*CONQ(L,KC)  
            ENDDO  
          ENDIF  
        ENDIF  
      ENDIF  
      GOTO 2000  
C
C *** SHELL FISH LARVAE SECTION
C
 1000 CONTINUE  
      DO NS=1,NQSIJ  
        L=LQS(NS)  
        NQSTMP=NQSERQ(NS)  
        NCSTMP=NCSERQ(NS,M)  
        DO K=1,KC  
          FQC(L,K)=FQC(L,K)  
     &        +MAX(QSS(K,NS),0.)*CQS(K,NS,M)  
     &        +MIN(QSS(K,NS),0.)*CONQ(L,K)  
     &        +MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,M)  
     &        +MIN(QSERCELL(K,NS),0.)*CONQ(L,K)  
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
          FQC(LU,K)=FQC(LU,K)  
     &        -QCTLT(K,NCTL)*CONQ(LU,K)  
          FQC(LD,K)=FQC(LD,K)  
     &        +RQWD*QCTLT(K,NCTL)*CONQ(LU,K)  
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
        FQC(LU,KU)=FQC(LU,KU)  
     &      -(QWR(NWR)+QWRSERT(NQSTMP))*CONQ(LU,KU)  
        FQC(LD,KD)=FQC(LD,KD)  
     &      +QWR(NWR)*(SFLKILL*CONQ(LU,KU)+CQWR(NWR,M))  
     &      +QWRSERT(NQSTMP)*(SFLKILL*CONQ(LU,KU)  
     &      +CQWRSERT(NCSTMP,M))  
      ENDDO  
      IF(MDCHH.GE.1)THEN  
        DO K=1,KC  
          DO NMD=1,MDCHH  
            LMDCHHT=LMDCHH(NMD)  
            LMDCHUT=LMDCHU(NMD)  
            LMDCHVT=LMDCHV(NMD)  
            IF(MDCHTYP(NMD).EQ.1)THEN  
              QUKTMP=QCHANU(NMD)*DZC(K)  
              QVKTMP=0.  
            ENDIF  
            IF(MDCHTYP(NMD).EQ.2)THEN  
              QVKTMP=QCHANV(NMD)*DZC(K)  
              QUKTMP=0.  
            ENDIF  
            IF(MDCHTYP(NMD).EQ.3)THEN  
              QUKTMP=QCHANU(NMD)*DZC(K)  
              QVKTMP=QCHANV(NMD)*DZC(K)  
            ENDIF  
            FQC(LMDCHHT,K)=FQC(LMDCHHT,K)  
     &          +MAX(QUKTMP,0.)*CONQ(LMDCHUT,K)  
     &          +MIN(QUKTMP,0.)*CONQ(LMDCHHT,K)  
     &          +MAX(QVKTMP,0.)*CONQ(LMDCHVT,K)  
     &          +MIN(QVKTMP,0.)*CONQ(LMDCHHT,K)  
            FQC(LMDCHUT,K)=FQC(LMDCHUT,K)  
     &          -MAX(QUKTMP,0.)*CONQ(LMDCHUT,K)  
     &          -MIN(QUKTMP,0.)*CONQ(LMDCHHT,K)  
            FQC(LMDCHVT,K)=FQC(LMDCHVT,K)  
     &          -MAX(QVKTMP,0.)*CONQ(LMDCHVT,K)  
     &          -MIN(QVKTMP,0.)*CONQ(LMDCHHT,K)  
          ENDDO  
        ENDDO  
      ENDIF  
      GOTO 2000  
C
C *** WATER QUALITY ONLY (IWQPSL=1,0)
C      
 1500 CONTINUE  

      ! *** FLOW BOUNDARIES
      DO NS=1,NQSIJ  
        L=LQS(NS)  
        NQSTMP=NQSERQ(NS)  
        DO K=1,KC  
          FQC(L,K)=FQC(L,K)  
     &        +MIN(QSS(K,NS),0.)*CON1(L,K)  
     &        +MIN(QSERCELL(K,NS),0.)*CON1(L,K)  
          QSUMPAD(L,K)=QSUMPAD(L,K)  
     &        +MAX(QSS(K,NS),0.)+MAX(QSERCELL(K,NS),0.)  
C          QSUMNAD(L,K)=QSUMNAD(L,K)  
C     &        +MIN(QSS(K,NS),0.)+MIN(QSERCELL(K,NS),0.)  
        ENDDO  
      ENDDO  
      
      ! *** HYDRAULIC STRUCTURES
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
          FQC(LU,K)=FQC(LU,K)  
     &        -QCTLT(K,NCTL)*CON1(LU,K)  
          FQC(LD,K)=FQC(LD,K)  
     &        +RQWD*QCTLT(K,NCTL)*CON1(LU,K)  
          FQCPAD(LD,K)=FQCPAD(LD,K)  
     &        +RQWD*QCTLT(K,NCTL)*CON1(LU,K)  
          QSUMPAD(L,K)=QSUMPAD(L,K)  
     &        +RQWD*QCTLT(K,NCTL)  
C          QSUMNAD(L,K)=QSUMNAD(L,K)  
C     &        -QCTLT(K,NCTL)  
        ENDDO  
      ENDDO
      
      ! *** WITHDRAWAL/RETURN
      IF(MVAR.EQ.8)THEN
        M=4+NTOX+NSED+NSND+MO
      ELSE
        M=MO
      ENDIF
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
        FQC(LU,KU)=FQC(LU,KU)  
     &      -(QWR(NWR)+QWRSERT(NQSTMP))*CON1(LU,KU)  
        FQC(LD,KD)=FQC(LD,KD)  
     &      +QWR(NWR)*(CON1(LU,KU)+CQWR(NWR,M))  
     &      +QWRSERT(NQSTMP)*(CON1(LU,KU)+CQWRSERT(NCSTMP,M))  
        FQCPAD(LD,KD)=FQCPAD(LD,KD)  
     &      +QWR(NWR)*(CON1(LU,KU)+CQWR(NWR,M))  
     &      +QWRSERT(NQSTMP)*(CON1(LU,KU)+CQWRSERT(NCSTMP,M))  
        QSUMPAD(LD,KD)=QSUMPAD(LD,KD)  
     &      +QWR(NWR)+QWRSERT(NQSTMP)  
C        QSUMNAD(LU,KU)=QSUMNAD(LU,KU)  
C     &      -(QWR(NWR)+QWRSERT(NQSTMP))  
      ENDDO  
      
      ! *** SUBGRID CHANNEL INTERACTIONS
      IF(MDCHH.GE.1)THEN  
        DO K=1,KC  
          DO NMD=1,MDCHH  
            LMDCHHT=LMDCHH(NMD)  
            LMDCHUT=LMDCHU(NMD)  
            LMDCHVT=LMDCHV(NMD)  
            IF(MDCHTYP(NMD).EQ.1)THEN  
              QUKTMP=QCHANU(NMD)*DZC(K)  
              QVKTMP=0.  
            ENDIF  
            IF(MDCHTYP(NMD).EQ.2)THEN  
              QVKTMP=QCHANV(NMD)*DZC(K)  
              QUKTMP=0.  
            ENDIF  
            IF(MDCHTYP(NMD).EQ.3)THEN  
              QUKTMP=QCHANU(NMD)*DZC(K)  
              QVKTMP=QCHANV(NMD)*DZC(K)  
            ENDIF  
            FQC(LMDCHHT,K)=FQC(LMDCHHT,K)  
     &          +MAX(QUKTMP,0.)*CON1(LMDCHUT,K)  
     &          +MIN(QUKTMP,0.)*CON1(LMDCHHT,K)  
     &          +MAX(QVKTMP,0.)*CON1(LMDCHVT,K)  
     &          +MIN(QVKTMP,0.)*CON1(LMDCHHT,K)  
            FQC(LMDCHUT,K)=FQC(LMDCHUT,K)  
     &          -MAX(QUKTMP,0.)*CON1(LMDCHUT,K)  
     &          -MIN(QUKTMP,0.)*CON1(LMDCHHT,K)  
            FQC(LMDCHVT,K)=FQC(LMDCHVT,K)  
     &          -MAX(QVKTMP,0.)*CON1(LMDCHVT,K)  
     &          -MIN(QVKTMP,0.)*CON1(LMDCHHT,K)  
          ENDDO  
        ENDDO  
      ENDIF  
      
      ! *** GROUNDWATER, EVAP, RAINFALL (2TL)  
      IF(ISGWIE.NE.0)THEN  
        DO L=2,LA  
          FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)  
        ENDDO  
      ENDIF  
      
      ! *** ZONED SEEPAGE (2TL)  
      IF(ISGWIT.EQ.3)THEN  
        DO L=2,LA
          IF(H1P(L).GT.HDRY)THEN  
            FQC(L,1)=FQC(L,1)-RIFTR(L)*CON1(L,1)
          ENDIF 
        ENDDO  
      ENDIF  
      
 2000 CONTINUE  
      RETURN  
      END  

