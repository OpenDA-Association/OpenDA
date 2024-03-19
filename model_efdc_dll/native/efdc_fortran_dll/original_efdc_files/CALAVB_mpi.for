      SUBROUTINE CALAVB_mpi (ISTL_)
C
C **  SUBROUTINE CALAV CALCULATES VERTICAL VISCOSITY AND DIFFUSIVITY
C **  USING GLAPERIN ET AL'S MODIFICATION OF THE MELLOR-YAMADA MODEL
C **  (NOTE AV, AB, AND AQ ARE ACTUALLY DIVIDED BY H)
C **  IF ISGA=1 VALUES ARE GEOMETRIC AVERAGES WITH THE PREVIOUS VALUES
C CHANGE RECORD
C  ADDED DRYCELL BYPASS AND CONSISTENT INITIALIZATION OF DRY VALUES
C
      USE GLOBAL
      USE MPI
      IMPLICIT NONE
      INTEGER::L,K,LS,ISTL_
      REAL::QQIMAX,RIQMIN,RIQMAX,RIQ,SFAV,SFAB,ABTMP,AVTMP
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
      QQIMAX=1./QQMIN
      AVMAX=AVO
      ABMAX=ABO
      AVMIN=10.
      ABMIN=10.
      RIQMIN=-0.023
      RIQMAX=0.28

      S1TIME=MPI_TIC()
C      DO K=1,KC
!$OMP PARALLEL DO
        DO L=LMPI1,LMPILC
          IF(IMASKDRY(L).EQ.1)THEN
            AV(L,1:KC)=AVO*HPI(L)
            AB(L,1:KC)=ABO*HPI(L)
          ENDIF
        ENDDO
C      ENDDO
      MPI_WTIMES(801)=MPI_WTIMES(801)+MPI_TOC(S1TIME)

      IF(ISFAVB.EQ.0)THEN
        S1TIME=MPI_TIC()
        DO K=1,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              QQI(L)=1./QQ(L,K)
              QQI(L)=MIN(QQI(L),QQIMAX)
            ENDIF
          ENDDO
!$OMP PARALLEL DO PRIVATE(RIQ,SFAV,SFAB,AVMAX,ABMAX,AVMIN,ABMIN)
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              RIQ=-GP*HP(L)*DML(L,K)*DML(L,K)*DZIG(K)
     &            *(B(L,K+1)-B(L,K))*QQI(L)
              RIQ=MAX(RIQ,RIQMIN)
              RIQ=MIN(RIQ,RIQMAX)
              SFAV=0.3920*(1.+8.6736*RIQ)/((1.+30.192*RIQ)*(1.+
     &            6.1272*RIQ))
              SFAB=0.4939/(1.+30.192*RIQ)
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
        MPI_WTIMES(802)=MPI_WTIMES(802)+MPI_TOC(S1TIME)
      ENDIF

      IF(ISFAVB.EQ.1)THEN
        S1TIME=MPI_TIC()
        DO K=1,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              QQI(L)=1./QQ(L,K)
              QQI(L)=MIN(QQI(L),QQIMAX)
            ENDIF
          ENDDO
!$OMP PARALLEL DO PRIVATE(RIQ,SFAV,SFAB,ABTMP,AVTMP,AVMAX,
!$OMP+     ABMAX,AVMIN,ABMIN)
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              RIQ=-GP*HP(L)*DML(L,K)*DML(L,K)*DZIG(K)
     &            *(B(L,K+1)-B(L,K))*QQI(L)
              RIQ=MAX(RIQ,RIQMIN)
              RIQ=MIN(RIQ,RIQMAX)
              SFAV=0.3920*(1.+8.6736*RIQ)/((1.+30.192*RIQ)*(1.+
     &            6.1272*RIQ))
              SFAB=0.4939/(1.+30.192*RIQ)
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
        MPI_WTIMES(803)=MPI_WTIMES(803)+MPI_TOC(S1TIME)
      ENDIF
      IF(ISFAVB.EQ.2)THEN
        S1TIME=MPI_TIC()
        DO K=1,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              QQI(L)=1./QQ(L,K)
              QQI(L)=MIN(QQI(L),QQIMAX)
            ENDIF
          ENDDO
!$OMP PARALLEL DO PRIVATE(RIQ,SFAV,SFAB,ABTMP,AVTMP,
!$OMP+     AVMAX,ABMAX,AVMIN,ABMIN)
          DO L=LMPI2,LMPILA
            IF(LMASKDRY(L))THEN
              RIQ=-GP*HP(L)*DML(L,K)*DML(L,K)*DZIG(K)
     &            *(B(L,K+1)-B(L,K))*QQI(L)
              RIQ=MAX(RIQ,RIQMIN)
              RIQ=MIN(RIQ,RIQMAX)
              SFAV=0.3920*(1.+8.6736*RIQ)/((1.+30.192*RIQ)*(1.+
     &            6.1272*RIQ))
              SFAB=0.4939/(1.+30.192*RIQ)
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
        MPI_WTIMES(804)=MPI_WTIMES(804)+MPI_TOC(S1TIME)
      ENDIF
      S1TIME=MPI_TIC()
      IF(ISAVBMX.GE.1)THEN
        DO K=1,KS
!$OMP PARALLEL DO PRIVATE(AVTMP,ABTMP)
          DO L=LMPI2,LMPILA
            AVTMP=AVMX*HPI(L)
            ABTMP=ABMX*HPI(L)
            AV(L,K)=MIN(AV(L,K),AVTMP)
            AB(L,K)=MIN(AB(L,K),ABTMP)
          ENDDO
        ENDDO
      ENDIF
      MPI_WTIMES(805)=MPI_WTIMES(805)+MPI_TOC(S1TIME)

      S1TIME=MPI_TIC()
      CALL broadcast_boundary_array(AV,ic)
      CALL broadcast_boundary_array(AB,ic)
      MPI_WTIMES(809)=MPI_WTIMES(809)+MPI_TOC(S1TIME)

      if(PRINT_SUM)then
        call collect_in_zero_array(B )
        call collect_in_zero_array(QQSQR )
        call collect_in_zero_array(AV )
        call collect_in_zero_array(AB )
        IF(MYRANK.EQ.0) PRINT*, 'B = ', sum(abs(dble(B)))
        IF(MYRANK.EQ.0) PRINT*, 'QQSQR = ', sum(abs(dble(QQSQR)))
        IF(MYRANK.EQ.0) PRINT*, 'AV = ', sum(abs(dble(AV)))
        IF(MYRANK.EQ.0) PRINT*, 'AB = ', sum(abs(dble(AB)))
      endif

      S1TIME=MPI_TIC()
      DO K=1,KS
!$OMP PARALLEL DO PRIVATE(LS)
        DO L=LMPI2,LMPILA
          LS=LSC(L)
          AVUI(L,K)=(1.+SUB(L))/(AV(L,K)+SUB(L)*AV(L-1,K))
          AVVI(L,K)=(1.+SVB(L))/(AV(L,K)+SVB(L)*AV(LS,K))
        ENDDO
      ENDDO
      MPI_WTIMES(806)=MPI_WTIMES(806)+MPI_TOC(S1TIME)
      S1TIME=MPI_TIC()
      DO K=2,KS
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          AQ(L,K)=0.205*(AV(L,K-1)+AV(L,K))
        ENDDO
      ENDDO
      MPI_WTIMES(807)=MPI_WTIMES(807)+MPI_TOC(S1TIME)
      S1TIME=MPI_TIC()
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        AQ(L,1)=0.205*AV(L,1)
      ENDDO
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        AQ(L,KC)=0.205*AV(L,KS)
      ENDDO
      MPI_WTIMES(808)=MPI_WTIMES(808)+MPI_TOC(S1TIME)

      S1TIME=MPI_TIC()
      CALL broadcast_boundary_array(AQ,ic)
      CALL broadcast_boundary_array(AVUI,ic)
      CALL broadcast_boundary_array(AVVI,ic)
      MPI_WTIMES(810)=MPI_WTIMES(810)+MPI_TOC(S1TIME)


      if(PRINT_SUM)then
        call collect_in_zero_array(AVUI )
        call collect_in_zero_array(AVVI )
        IF(MYRANK.EQ.0) PRINT*, 'avb_AVUI = ', sum(abs(dble(AVUI)))
        IF(MYRANK.EQ.0) PRINT*, 'avb_AVVI = ', sum(abs(dble(AVVI)))
      endif

      RETURN
      END

