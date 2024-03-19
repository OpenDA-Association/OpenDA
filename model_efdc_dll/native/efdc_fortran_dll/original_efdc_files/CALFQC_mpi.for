      SUBROUTINE CALFQC_mpi(ISTL_,IS2TL_,MVAR,MO,CON,CON1)!,FQCPAD,QSUMPAD,
!     &    QSUMNAD)
C
C CHANGE RECORD
C **  SUBROUTINE CALFQC CALCULATES MASS SOURCES AND SINKS ASSOCIATED
C **  WITH CONSTANT AND TIME SERIES INFLOWS AND OUTFLOWS; CONTROL
C **  STRUCTURE INFLOWS AND OUTLOWS; WITHDRAWAL AND RETURN STRUCTURE
C **  OUTFLOWS; AND  EMBEDED CHANNEL INFLOWS AND OUTFLOWS
C
      USE GLOBAL
      USE MPI

      INTEGER::L,K,ID,JD,KD,NWR,IU,JU,KU,LU,NS
      INTEGER::LD,NMD,NJP

      DIMENSION CON(LCM,KCM),CON1(LCM,KCM)!,FQCPAD(LCM,KCM),
!     &          QSUMNAD(LCM,KCM),QSUMPAD(LCM,KCM)

      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::CONQ
      REAL QVKTMP
      REAL QUKTMP
      QVKTMP=0.0
      QUKTMP=0.0
      L=0
      IF(.NOT.ALLOCATED(CONQ))THEN
        ALLOCATE(CONQ(LCM,KCM))
        CONQ=0.0
      ENDIF
C
      M=MO
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FQCPAD   )
        call collect_in_zero_array(QSUMPAD  )
        call collect_in_zero_array(QSUMNAD  )
        call collect_in_zero_array(FQC   )
        if(myrank.eq.0) print*, n,'0FQC     = ', sum(abs(dble(FQC )))
        if(myrank.eq.0) print*, n,'0FQCPAD  = ', sum(abs(dble(FQCPAD )))
        if(myrank.eq.0) print*, n,'0QSUMPAD = ', sum(abs(dble(QSUMPAD)))
        if(myrank.eq.0) print*, n,'0QSUMNAD = ', sum(abs(dble(QSUMNAD)))
      endif
C
      ! *** SELECTIVE ZEROING
      S4TIME=MPI_TIC()
      IF(KC.GT.1)THEN
        IF(NGWSER.GT.0.OR.ISGWIT.NE.0)THEN
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            FQC(L,1)=0.
          ENDDO
        ENDIF

        ! *** ZERO EVAP/RAINFALL
        IF(MVAR.EQ.2)THEN
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            FQC(L,KC)=0.
          ENDDO
          IF(ISADAC(MVAR).GE.2)THEN
!$OMP PARALLEL DO
            DO L=LMPI1,LMPILC
              FQCPAD(L,KC)=0.
            ENDDO
          ENDIF
          IF(ISADAC(MVAR).GT.0)THEN
!$OMP PARALLEL DO
            DO L=LMPI1,LMPILC
              QSUMPAD(L,KC)=0.
            ENDDO
          ENDIF
        ENDIF

        ! *** ZERO ALL DEFINED BC'S
!$OMP PARALLEL DO PRIVATE(L)
        DO NS=1,NBCS
          L=LBCS(NS)
          IF(ISDOMAIN(L))THEN
            FQC(L,1:KC)=0.
            FQCPAD(L,1:KC)=0
            QSUMPAD(L,1:KC)=0.
          ENDIF
        ENDDO

      ELSE
        FQC=0.
        IF(ISADAC(MVAR).GE.2)FQCPAD=0.
        QSUMPAD=0.
        !QSUMNAD=0.
      ENDIF
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FQCPAD   )
        call collect_in_zero_array(QSUMPAD  )
        call collect_in_zero_array(QSUMNAD  )
        call collect_in_zero_array(FQC   )
        if(myrank.eq.0) print*, n,'0FQC     = ', sum(abs(dble(FQC )))
        if(myrank.eq.0) print*, n,'0FQCPAD  = ', sum(abs(dble(FQCPAD )))
        if(myrank.eq.0) print*, n,'0QSUMPAD = ', sum(abs(dble(QSUMPAD)))
        if(myrank.eq.0) print*, n,'0QSUMNAD = ', sum(abs(dble(QSUMNAD)))
      endif

      MPI_WTIMES(1101)=MPI_WTIMES(1101)+MPI_TOC(S4TIME)
C
      IF(MVAR.EQ.8.AND.IWQPSL.NE.2) GOTO 1500
C
C **  INITIALIZE VOLUMETRIC SOURCE-SINK FLUXES AND AUXILLARY VARIABLES
C
C
      S4TIME=MPI_TIC()
      ! *** 3TL STANDARD & WATER QUALITY
      IF(ISTL_.EQ.3.OR.MVAR.EQ.8)THEN
        IF(NGWSER.GT.0.OR.ISGWIT.NE.0)THEN
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            CONQ(L,1)=CON(L,1)
          ENDDO
        ENDIF

        ! *** ZERO EVAP/RAINFALL
        IF(MVAR.EQ.2)THEN
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            CONQ(L,KC)=CON(L,KC)
          ENDDO
        ENDIF

        ! *** INITIALIZE ALL DEFINED BC'S
!$OMP PARALLEL DO PRIVATE(L)
        DO NS=1,NBCS
          L=LBCS(NS)
          IF(ISDOMAIN(L))THEN
            CONQ(L,1:KC)=CON(L,1:KC)
          ENDIF
        ENDDO
      ENDIF
      MPI_WTIMES(1102)=MPI_WTIMES(1102)+MPI_TOC(S4TIME)
C
      S4TIME=MPI_TIC()
      ! *** 3TL CORRECTION STEP
      IF(ISTL_.EQ.2.AND.IS2TL_.EQ.0)THEN
        IF(NGWSER.GT.0.OR.ISGWIT.NE.0)THEN
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            CONQ(L,1)=0.5*(CON(L,1)+CON1(L,1))
          ENDDO
        ENDIF

        ! *** ZERO EVAP/RAINFALL
        IF(MVAR.EQ.2)THEN
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            CONQ(L,KC)=0.5*(CON(L,KC)+CON1(L,KC))
          ENDDO
        ENDIF

        ! *** INITIALIZE ALL DEFINED BC'S
!$OMP PARALLEL DO PRIVATE(L)
        DO NS=1,NBCS
          L=LBCS(NS)
          IF(ISDOMAIN(L))THEN
            CONQ(L,1:KC)=0.5*(CON(L,1:KC)+CON1(L,1:KC))
          ENDIF
        ENDDO

      ENDIF

      ! *** 2TL STANDARD
      IF(ISTL_.EQ.2.AND.IS2TL_.EQ.1)THEN
        IF(NGWSER.GT.0.OR.ISGWIT.NE.0)THEN
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            CONQ(L,1)=0.5*(3.*CON(L,1)-CON1(L,1))
          ENDDO
        ENDIF

        ! *** ZERO EVAP/RAINFALL
        IF(MVAR.EQ.2)THEN
!$OMP PARALLEL DO
          DO L=LMPI1,LMPILC
            CONQ(L,KC)=0.5*(3.*CON(L,KC)-CON1(L,KC))
          ENDDO
        ENDIF

        ! *** INITIALIZE ALL DEFINED BC'S
!$OMP PARALLEL DO PRIVATE(L)
        DO NS=1,NBCS
          L=LBCS(NS)
          IF(ISDOMAIN(L))THEN
            CONQ(L,1:KC)=0.5*(3.*CON(L,1:KC)-CON1(L,1:KC))
          ENDIF
        ENDDO

      ENDIF
      MPI_WTIMES(1103)=MPI_WTIMES(1103)+MPI_TOC(S4TIME)
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
C
        S4TIME=MPI_TIC()
        ! *** FLOW BOUNDARY CELLS (2TL)
CC!$OMP PARALLEL DO PRIVATE(L,NQSTMP,NCSTMP)
        DO NS=1,NQSIJ
          L=LQS(NS)
          NQSTMP=NQSERQ(NS)
          NCSTMP=NCSERQ(NS,M)
          IF(ISDOMAIN(LQS(NS)))THEN
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
          ENDIF
        ENDDO
        MPI_WTIMES(1104)=MPI_WTIMES(1104)+MPI_TOC(S4TIME)
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FQCPAD   )
        call collect_in_zero_array(QSUMPAD  )
        call collect_in_zero_array(QSUMNAD  )
        call collect_in_zero_array(FQC   )
        if(myrank.eq.0) print*, n,'11FQC   = ', sum(abs(dble(FQC )))
        if(myrank.eq.0) print*, n,'11FQCD  = ', sum(abs(dble(FQCPAD )))
        if(myrank.eq.0) print*, n,'11QSUAD = ', sum(abs(dble(QSUMPAD)))
        if(myrank.eq.0) print*, n,'11QSUAD = ', sum(abs(dble(QSUMNAD)))
      endif
C
        S4TIME=MPI_TIC()
        ! ***  JET-PLUME VOLUMETRICS SOURCE SINK LOCATIONS (2TL)
        IF(NQJPIJ.GT.0)THEN
          CALL broadcast_boundary_array(FQC,ic)
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
        CALL broadcast_boundary_array(CONQ,ic)
        CALL broadcast_boundary_array(FQC,ic)
        CALL broadcast_boundary_array(FQCPAD,ic)
        CALL broadcast_boundary_array(QSUMPAD,ic)
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
        MPI_WTIMES(1105)=MPI_WTIMES(1105)+MPI_TOC(S4TIME)
C

      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FQCPAD   )
        call collect_in_zero_array(QSUMPAD  )
        call collect_in_zero_array(QSUMNAD  )
        call collect_in_zero_array(FQC   )
        if(myrank.eq.0) print*, n,'12FQC   = ', sum(abs(dble(FQC )))
        if(myrank.eq.0) print*, n,'12FQD  = ', sum(abs(dble(FQCPAD )))
        if(myrank.eq.0) print*, n,'12QSAD = ', sum(abs(dble(QSUMPAD)))
        if(myrank.eq.0) print*, n,'12QSAD = ', sum(abs(dble(QSUMNAD)))
      endif

        S4TIME=MPI_TIC()
        ! *** CONTROL STRUCTURES (2TL)
        CALL broadcast_boundary_array(CONQ,ic)
        CALL broadcast_boundary_array(FQC,ic)
        CALL broadcast_boundary_array(FQCPAD,ic)
        CALL broadcast_boundary_array(QSUMPAD,ic)
C!$OMP PARALLEL DO PRIVATE(RQWD,IU,JU,LU,ID,JD,LD)
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
            QSUMPAD(LD,K)=QSUMPAD(LD,K)
     &          +RQWD*QCTLT(K,NCTL)
C            QSUMNAD(L,K)=QSUMNAD(L,K)
C     &          -QCTLT(K,NCTL)
          ENDDO
        ENDDO
        MPI_WTIMES(1106)=MPI_WTIMES(1106)+MPI_TOC(S4TIME)
C
        CALL broadcast_boundary_array(CONQ,ic)
        CALL broadcast_boundary_array(FQC,ic)
        CALL broadcast_boundary_array(FQCPAD,ic)
        CALL broadcast_boundary_array(QSUMPAD,ic)
        S4TIME=MPI_TIC()
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
        MPI_WTIMES(1107)=MPI_WTIMES(1107)+MPI_TOC(S4TIME)
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FQCPAD   )
        call collect_in_zero_array(QSUMPAD  )
        call collect_in_zero_array(QSUMNAD  )
        if(myrank.eq.0) print*, n,'13FQCPAD = ', sum(abs(dble(FQCPAD )))
        if(myrank.eq.0) print*, n,'13QSUMPAD= ', sum(abs(dble(QSUMPAD)))
        if(myrank.eq.0) print*, n,'13QSUMNAD= ', sum(abs(dble(QSUMNAD)))
        if(myrank.eq.0) print*, n,'13QSUMNAD= ', nqwr,NQCTL
        DO NWR=1,NQWR
          IU=IQWRU(NWR)
          JU=JQWRU(NWR)
          KU=KQWRU(NWR)
          ID=IQWRD(NWR)
          JD=JQWRD(NWR)
          KD=KQWRD(NWR)
          LU=LIJ(IU,JU)
          LD=LIJ(ID,JD)
          if(myrank.eq.0) print*, qsumpad(ld,kd)
        enddo
      endif
c
        S4TIME=MPI_TIC()
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
        MPI_WTIMES(1108)=MPI_WTIMES(1108)+MPI_TOC(S4TIME)
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FQCPAD   )
        call collect_in_zero_array(QSUMPAD  )
        call collect_in_zero_array(QSUMNAD  )
        call collect_in_zero_array(FQC   )
        if(myrank.eq.0) print*, n,'15FQC   = ', sum(abs(dble(FQC )))
        if(myrank.eq.0) print*, n,'15FQD  = ', sum(abs(dble(FQCPAD )))
        if(myrank.eq.0) print*, n,'15QSAD = ', sum(abs(dble(QSUMPAD)))
        if(myrank.eq.0) print*, n,'15QSAD = ', sum(abs(dble(QSUMNAD)))
      endif
C
        S4TIME=MPI_TIC()
        ! *** GROUNDWATER, EVAP, RAINFALL (2TL)
        IF(ISGWIE.NE.0)THEN
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)
          ENDDO
        ENDIF
        MPI_WTIMES(1109)=MPI_WTIMES(1109)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
        ! *** ZONED SEEPAGE (2TL)
        IF(ISGWIT.EQ.3)THEN
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(H1P(L).GT.HDRY)THEN
              FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)
            ENDIF
          ENDDO
        ENDIF
        MPI_WTIMES(1110)=MPI_WTIMES(1110)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
        ! *** TEMPERATURE ADJUSTMENTS FOR RAINFALL & EVAPORATION
        IF(M.EQ.2)THEN
          IF(ISTOPT(2).EQ.0.OR.ISTOPT(2).EQ.3)THEN
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TEMO*DXYP(L)
            ENDDO
          ENDIF
          IF(ISTOPT(2).EQ.1.OR.ISTOPT(2).EQ.2.OR.ISTOPT(2).EQ.4)THEN
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)
              FQCPAD(L,KC)=FQCPAD(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)
              QSUMPAD(L,KC)=QSUMPAD(L,KC)+RAINT(L)*DXYP(L)
            ENDDO
          ENDIF
        ENDIF
        IF(M.EQ.2)THEN
          IF(ISTOPT(2).EQ.0)THEN
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              FQC(L,KC)=FQC(L,KC)-EVAPSW(L)*CONQ(L,KC)
            ENDDO
          ENDIF
        ENDIF
        MPI_WTIMES(1111)=MPI_WTIMES(1111)+MPI_TOC(S4TIME)
      ENDIF
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FQCPAD   )
        call collect_in_zero_array(QSUMPAD  )
        call collect_in_zero_array(QSUMNAD  )
        call collect_in_zero_array(FQC   )
        if(myrank.eq.0) print*, n,'2FQC   = ', sum(abs(dble(FQC )))
        if(myrank.eq.0) print*, n,'2FQCPAD  = ', sum(abs(dble(FQCPAD )))
        if(myrank.eq.0) print*, n,'2QSUMPAD = ', sum(abs(dble(QSUMPAD)))
        if(myrank.eq.0) print*, n,'2QSUMNAD = ', sum(abs(dble(QSUMNAD)))
      endif

C *********************************************************************C
C
C *** 3TL CORRECTOR VOLUMETRICS SOURCE SINK LOCATIONS
C
      IF(ISTL_.EQ.2.AND.IS2TL_.EQ.0)THEN
C
        S4TIME=MPI_TIC()
        ! *** FLOW BOUNDARY CELLS (3TL CORRECTOR)
        DO NS=1,NQSIJ
          L=LQS(NS)
          NQSTMP=NQSERQ(NS)
          NCSTMP=NCSERQ(NS,M)
          IF(ISDOMAIN(LQS(NS)))THEN
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
          ENDIF
        ENDDO
        MPI_WTIMES(1112)=MPI_WTIMES(1112)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
        CALL broadcast_boundary_array(CONQ,ic)
        CALL broadcast_boundary_array(FQC,ic)
        CALL broadcast_boundary_array(FQCPAD,ic)
        CALL broadcast_boundary_array(QSUMPAD,ic)
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
        MPI_WTIMES(1113)=MPI_WTIMES(1113)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
        CALL broadcast_boundary_array(CONQ,ic)
        CALL broadcast_boundary_array(FQC,ic)
        CALL broadcast_boundary_array(FQCPAD,ic)
        CALL broadcast_boundary_array(QSUMPAD,ic)
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
        MPI_WTIMES(1114)=MPI_WTIMES(1114)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
        CALL broadcast_boundary_array(CONQ,ic)
        CALL broadcast_boundary_array(FQC,ic)
        CALL broadcast_boundary_array(FQCPAD,ic)
        CALL broadcast_boundary_array(QSUMPAD,ic)
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
        MPI_WTIMES(1115)=MPI_WTIMES(1115)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
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
        MPI_WTIMES(1116)=MPI_WTIMES(1116)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
        ! ***  GROUNDWATER, EVAP, RAINFALL (3TL CORRECTOR)
        IF(ISGWIE.NE.0)THEN
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)
          ENDDO
        ENDIF
        MPI_WTIMES(1117)=MPI_WTIMES(1117)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
        ! *** ZONED SEEPAGE (3TL)
        IF(ISGWIT.EQ.3)THEN
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(H1P(L).GT.HDRY)THEN
              FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)
            ENDIF
          ENDDO
        ENDIF
        MPI_WTIMES(1118)=MPI_WTIMES(1118)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
        ! *** TEMPERATURE ADJUSTMENTS FOR RAINFALL & EVAPORATION
        IF(M.EQ.2)THEN
          IF(ISTOPT(2).EQ.0.OR.ISTOPT(2).EQ.3)THEN
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TEMO*DXYP(L)
            ENDDO
          ENDIF
          IF(ISTOPT(2).EQ.1.OR.ISTOPT(2).EQ.2.OR.ISTOPT(2).EQ.4)THEN
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)
              FQCPAD(L,KC)=FQCPAD(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)
              QSUMPAD(L,KC)=QSUMPAD(L,KC)+RAINT(L)*DXYP(L)
            ENDDO
          ENDIF
        ENDIF
        IF(M.EQ.2)THEN
          IF(ISTOPT(2).EQ.0)THEN
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              FQC(L,KC)=FQC(L,KC)-EVAPSW(L)*CONQ(L,KC)
            ENDDO
          ENDIF
        ENDIF
        MPI_WTIMES(1119)=MPI_WTIMES(1119)+MPI_TOC(S4TIME)
C
      ENDIF
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FQCPAD   )
        call collect_in_zero_array(QSUMPAD  )
        call collect_in_zero_array(QSUMNAD  )
        if(myrank.eq.0) print*, n,'3FQCPAD  = ', sum(abs(dble(FQCPAD )))
        if(myrank.eq.0) print*, n,'3QSUMPAD = ', sum(abs(dble(QSUMPAD)))
        if(myrank.eq.0) print*, n,'3QSUMNAD = ', sum(abs(dble(QSUMNAD)))
      endif

C *********************************************************************C
C
C **  STANDARD VOLUMETRICS SOURCE SINK LOCATIONS (3TL)
C
      IF(ISTL_.EQ.3)THEN
C
        S4TIME=MPI_TIC()
        ! *** FLOW BOUNDARY CELLS (3TL)
        DO NS=1,NQSIJ
          L=LQS(NS)
          NQSTMP=NQSERQ(NS)
          NCSTMP=NCSERQ(NS,M)
          IF(ISDOMAIN(LQS(NS)))THEN
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
          ENDIF
        ENDDO
        MPI_WTIMES(1120)=MPI_WTIMES(1120)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
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
        MPI_WTIMES(1121)=MPI_WTIMES(1121)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
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
        MPI_WTIMES(1122)=MPI_WTIMES(1122)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
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
        MPI_WTIMES(1123)=MPI_WTIMES(1123)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
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
        MPI_WTIMES(1124)=MPI_WTIMES(1124)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
        ! ***  GROUNDWATER, EVAP, RAINFALL (3TL)
        IF(ISGWIE.NE.0)THEN
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)
          ENDDO
        ENDIF
        MPI_WTIMES(1125)=MPI_WTIMES(1125)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
        ! *** ZONED SEEPAGE (3TL)
        IF(ISGWIT.EQ.3)THEN
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(H1P(L).GT.HDRY)THEN
              FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)
            ENDIF
          ENDDO
        ENDIF
        MPI_WTIMES(1126)=MPI_WTIMES(1126)+MPI_TOC(S4TIME)
C
        S4TIME=MPI_TIC()
        ! *** TEMPERATURE ADJUSTMENTS FOR RAINFALL & EVAPORATION
        IF(M.EQ.2)THEN
          IF(ISTOPT(2).EQ.0.OR.ISTOPT(2).EQ.3)THEN
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TEMO*DXYP(L)
            ENDDO
          ENDIF
          IF(ISTOPT(2).EQ.1.OR.ISTOPT(2).EQ.2.OR.ISTOPT(2).EQ.4)THEN
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              FQC(L,KC)=FQC(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)
              FQCPAD(L,KC)=FQCPAD(L,KC)+RAINT(L)*TATMT(L)*DXYP(L)
              QSUMPAD(L,KC)=QSUMPAD(L,KC)+RAINT(L)*DXYP(L)
            ENDDO
          ENDIF
        ENDIF
        IF(M.EQ.2)THEN
          IF(ISTOPT(2).EQ.0)THEN
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              FQC(L,KC)=FQC(L,KC)-EVAPSW(L)*CONQ(L,KC)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
      MPI_WTIMES(1127)=MPI_WTIMES(1127)+MPI_TOC(S4TIME)
C
      GOTO 2000
C
C *** SHELL FISH LARVAE SECTION
C
 1000 CONTINUE
C
      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FQCPAD   )
        call collect_in_zero_array(QSUMPAD  )
        call collect_in_zero_array(QSUMNAD  )
        if(myrank.eq.0) print*, n,'4FQCPAD  = ', sum(abs(dble(FQCPAD )))
        if(myrank.eq.0) print*, n,'4QSUMPAD = ', sum(abs(dble(QSUMPAD)))
        if(myrank.eq.0) print*, n,'4QSUMNAD = ', sum(abs(dble(QSUMNAD)))
      endif

      S4TIME=MPI_TIC()
      DO NS=1,NQSIJ
        L=LQS(NS)
        NQSTMP=NQSERQ(NS)
        NCSTMP=NCSERQ(NS,M)
        IF(ISDOMAIN(LQS(NS)))THEN
        DO K=1,KC
          FQC(L,K)=FQC(L,K)
     &        +MAX(QSS(K,NS),0.)*CQS(K,NS,M)
     &        +MIN(QSS(K,NS),0.)*CONQ(L,K)
     &        +MAX(QSERCELL(K,NS),0.)*CSERT(K,NCSTMP,M)
     &        +MIN(QSERCELL(K,NS),0.)*CONQ(L,K)
        ENDDO
        ENDIF
      ENDDO
      MPI_WTIMES(1128)=MPI_WTIMES(1128)+MPI_TOC(S4TIME)
C
      S4TIME=MPI_TIC()
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
      MPI_WTIMES(1129)=MPI_WTIMES(1129)+MPI_TOC(S4TIME)
C
      S4TIME=MPI_TIC()
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
      MPI_WTIMES(1130)=MPI_WTIMES(1130)+MPI_TOC(S4TIME)
C
      S4TIME=MPI_TIC()
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
      MPI_WTIMES(1131)=MPI_WTIMES(1131)+MPI_TOC(S4TIME)
C
      GOTO 2000
C
C *** WATER QUALITY ONLY (IWQPSL=1,0)
C
 1500 CONTINUE
C
      S4TIME=MPI_TIC()
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
      MPI_WTIMES(1132)=MPI_WTIMES(1132)+MPI_TOC(S4TIME)
C
      S4TIME=MPI_TIC()
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
      MPI_WTIMES(1133)=MPI_WTIMES(1133)+MPI_TOC(S4TIME)
C
      S4TIME=MPI_TIC()
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
      MPI_WTIMES(1134)=MPI_WTIMES(1134)+MPI_TOC(S4TIME)
C
      S4TIME=MPI_TIC()
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
      MPI_WTIMES(1135)=MPI_WTIMES(1135)+MPI_TOC(S4TIME)
C
      S4TIME=MPI_TIC()
      ! *** GROUNDWATER, EVAP, RAINFALL (2TL)
      IF(ISGWIE.NE.0)THEN
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          FQC(L,1)=FQC(L,1)-RIFTR(L)*CONQ(L,1)
        ENDDO
      ENDIF
      MPI_WTIMES(1136)=MPI_WTIMES(1136)+MPI_TOC(S4TIME)
C
      S4TIME=MPI_TIC()
      ! *** ZONED SEEPAGE (2TL)
      IF(ISGWIT.EQ.3)THEN
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          IF(H1P(L).GT.HDRY)THEN
            FQC(L,1)=FQC(L,1)-RIFTR(L)*CON1(L,1)
          ENDIF
        ENDDO
      ENDIF
      MPI_WTIMES(1137)=MPI_WTIMES(1137)+MPI_TOC(S4TIME)
C
 2000 CONTINUE

      if(PRINT_SUM.AND.MVAR.eq.2)then
        call collect_in_zero_array(FQCPAD   )
        call collect_in_zero_array(QSUMPAD  )
        call collect_in_zero_array(QSUMNAD  )
        if(myrank.eq.0) print*, n,'5FQCPAD  = ', sum(abs(dble(FQCPAD )))
        if(myrank.eq.0) print*, n,'5QSUMPAD = ', sum(abs(dble(QSUMPAD)))
        if(myrank.eq.0) print*, n,'5QSUMNAD = ', sum(abs(dble(QSUMNAD)))
      endif

      RETURN
      END

