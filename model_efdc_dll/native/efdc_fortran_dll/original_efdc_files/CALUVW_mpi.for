      SUBROUTINE CALUVW_mpi (ISTL_,IS2TL_)
C
C CHANGE RECORD
C **  CALCULATE THE INTERNAL SOLUTION AT TIME LEVEL (N+1)
C **  THE VALUE OF ISTL INDICATES THE NUMBER OF TIME LEVELS IN THE STEP
C
      USE GLOBAL
      USE MPI
      REAL DTCFL
      DTCFL=0.0
C
      IF(ISDYNSTP.EQ.0)THEN
        DELT=DT2
        DELTD2=DT
        IF(ISTL_.EQ.2)THEN
          DELT=DT
          DELTD2=0.5*DT
        ENDIF
        DELTI=1./DELT
      ELSE
        DELT=DTDYN
        DELTD2=0.5*DTDYN
        DELTI=1./DELT
      ENDIF
      IF(KC.EQ.1) GOTO 30
C
C **  CALCULATE BOTTOM FRICTION COEFFICIENT
C
      IF(ISTL_.EQ.3)THEN
        S1TIME=MPI_TIC()
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          RCX(L)=AVCON1/H1U(L)+STBX(L)*SQRT(U1(L,1)*U1(L,1)
     &        +V1U(L)*V1U(L))
          RCY(L)=AVCON1/H1V(L)+STBY(L)*SQRT(U1V(L)*U1V(L)
     &        +V1(L,1)*V1(L,1))
        ENDDO
        MPI_WTIMES(101)=MPI_WTIMES(101)+MPI_TOC(S1TIME)
C
C       LF=2+(ND-1)*LDM
C
      ELSE
        S1TIME=MPI_TIC()
        IF(AVCON1.LT.0.00001)THEN
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            ! *** FOR 2TL U1 & U AND V1 & V ARE THE SAME
            ! *** THESE ARE ONLY DIFFERENCE FOR 3TL ISTL=2 TRAP CORRECTION STEP
            RCX(L)=STBX(L)*SQRT(SQRT(U1(L,1)*U1(L,1)+V1U(L)*V1U(L))
     &             *SQRT(U(L,1)*U(L,1)+VU(L)*VU(L)))
            RCY(L)=STBY(L)*SQRT(SQRT(U1V(L)*U1V(L)+V1(L,1)*V1(L,1))
     &             *SQRT(UV(L)*UV(L)+V(L,1)*V(L,1)))
          ENDDO
        ELSE
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            RCX(L)=AVCON1/SQRT(H1U(L)*HU(L))+STBX(L)
     &             *SQRT(SQRT(U1(L,1)*U1(L,1)+V1U(L)*V1U(L))
     &             *SQRT(U(L,1)*U(L,1)+VU(L)*VU(L)))
            RCY(L)=AVCON1/SQRT(H1V(L)*HV(L))+STBY(L)
     &             *SQRT(SQRT(U1V(L)*U1V(L)+V1(L,1)*V1(L,1))
     &             *SQRT(UV(L)*UV(L)+V(L,1)*V(L,1)))
          ENDDO
        ENDIF
        MPI_WTIMES(102)=MPI_WTIMES(102)+MPI_TOC(S1TIME)
C
C       LF=2+(ND-1)*LDM
C
      ENDIF
C
      if(PRINT_SUM)then
        call collect_in_zero(RCX )
        call collect_in_zero(RCY )
        call collect_in_zero(HU )
        call collect_in_zero(HV )
        call collect_in_zero_array(AVUI )
        call collect_in_zero_array(AVVI )
        IF(MYRANK.EQ.0) PRINT*, 'RCX = ', sum(abs(dble(RCX)))
        IF(MYRANK.EQ.0) PRINT*, 'RCY = ', sum(abs(dble(RCY)))
        IF(MYRANK.EQ.0) PRINT*, 'HU = ', sum(abs(dble(HU)))
        IF(MYRANK.EQ.0) PRINT*, 'HV = ', sum(abs(dble(HV)))
        IF(MYRANK.EQ.0) PRINT*, 'AVUI = ', sum(abs(dble(AVUI)))
        IF(MYRANK.EQ.0) PRINT*, 'AVVI = ', sum(abs(dble(AVVI)))
      endif
C **  CALCULATE THE U AND V SHEARS
C
      S1TIME=MPI_TIC()
      RCDZM=CDZM(1)*DELTI
      RCDZU=CDZU(1)
      RCDZL=CDZL(1)
!$OMP PARALLEL DO PRIVATE(CMU,CMV,EU,EV)
      DO L=LMPI2,LMPILA
        CMU=1.+RCDZM*HU(L)*AVUI(L,1)
        CMV=1.+RCDZM*HV(L)*AVVI(L,1)
        EU=1./CMU
        EV=1./CMV
        CU1(L,1)=RCDZU*EU
        CU2(L,1)=RCDZU*EV
        DU(L,1)=(DU(L,1)-RCDZL*RCX(L)*UHE(L)*HUI(L))*EU
        DV(L,1)=(DV(L,1)-RCDZL*RCY(L)*VHE(L)*HVI(L))*EV
        UUU(L,1)=EU
        VVV(L,1)=EV
      ENDDO
      MPI_WTIMES(103)=MPI_WTIMES(103)+MPI_TOC(S1TIME)

      S1TIME=MPI_TIC()
      DO K=2,KS
        RCDZM=CDZM(K)*DELTI
        RCDZU=CDZU(K)
        RCDZL=CDZL(K)
!$OMP PARALLEL DO PRIVATE(CMU,CMV,EU,EV)
        DO L=LMPI2,LMPILA
          CMU=1.+RCDZM*HU(L)*AVUI(L,K)
          CMV=1.+RCDZM*HV(L)*AVVI(L,K)
          EU=1./(CMU-RCDZL*CU1(L,K-1))
          EV=1./(CMV-RCDZL*CU2(L,K-1))
          CU1(L,K)=RCDZU*EU
          CU2(L,K)=RCDZU*EV
          DU(L,K)=(DU(L,K)-RCDZL*DU(L,K-1))*EU
          DV(L,K)=(DV(L,K)-RCDZL*DV(L,K-1))*EV
          UUU(L,K)=-RCDZL*UUU(L,K-1)*EU
          VVV(L,K)=-RCDZL*VVV(L,K-1)*EV
        ENDDO
      ENDDO
      MPI_WTIMES(104)=MPI_WTIMES(104)+MPI_TOC(S1TIME)

      S1TIME=MPI_TIC()
      DO K=KS-1,1,-1
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          DU(L,K)=DU(L,K)-CU1(L,K)*DU(L,K+1)
          DV(L,K)=DV(L,K)-CU2(L,K)*DV(L,K+1)
          UUU(L,K)=UUU(L,K)-CU1(L,K)*UUU(L,K+1)
          VVV(L,K)=VVV(L,K)-CU2(L,K)*VVV(L,K+1)
        ENDDO
      ENDDO
      MPI_WTIMES(105)=MPI_WTIMES(105)+MPI_TOC(S1TIME)

      S1TIME=MPI_TIC()
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        AAU(L)=0.
        AAV(L)=0.
        BBU(L)=1.
        BBV(L)=1.
      ENDDO
      MPI_WTIMES(106)=MPI_WTIMES(106)+MPI_TOC(S1TIME)

      S1TIME=MPI_TIC()
      DO K=1,KS
        RCDZR=CDZR(K)
!$OMP PARALLEL DO PRIVATE(CRU,CRV)
        DO L=LMPI2,LMPILA
          CRU=RCDZR*RCX(L)*AVUI(L,K)
          CRV=RCDZR*RCY(L)*AVVI(L,K)
          AAU(L)=AAU(L)+CRU*DU(L,K)
          AAV(L)=AAV(L)+CRV*DV(L,K)
          BBU(L)=BBU(L)+CRU*UUU(L,K)
          BBV(L)=BBV(L)+CRV*VVV(L,K)
        ENDDO
      ENDDO
      MPI_WTIMES(107)=MPI_WTIMES(107)+MPI_TOC(S1TIME)

      S1TIME=MPI_TIC()
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        AAU(L)=AAU(L)/BBU(L)
        AAV(L)=AAV(L)/BBV(L)
      ENDDO
      MPI_WTIMES(108)=MPI_WTIMES(108)+MPI_TOC(S1TIME)

      S1TIME=MPI_TIC()
      DO K=1,KS
        RDZG=DZG(K)
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          DU(L,K)=RDZG*HU(L)*AVUI(L,K)*(DU(L,K)-AAU(L)*UUU(L,K))
          DV(L,K)=RDZG*HV(L)*AVVI(L,K)*(DV(L,K)-AAV(L)*VVV(L,K))
        ENDDO
      ENDDO
      MPI_WTIMES(109)=MPI_WTIMES(109)+MPI_TOC(S1TIME)
C
C **  CALCULATED U AND V
C **  DUSUM+UHE=UHE, DVSUM+VHE=VHE
C
      if(PRINT_SUM)then
        call collect_in_zero_array(UHDY )
        call collect_in_zero_array(VHDX )
        call collect_in_zero_array(U )
        call collect_in_zero_array(V )
        call collect_in_zero_array(W )
        IF(MYRANK.EQ.0) PRINT*, 'AUVW_UHDY = ', sum(abs(dble(UHDY)))
        IF(MYRANK.EQ.0) PRINT*, 'AUVW_VHDX = ', sum(abs(dble(VHDX)))
        IF(MYRANK.EQ.0) PRINT*, 'AUVW_U  = ', sum(abs(dble(U )))
        IF(MYRANK.EQ.0) PRINT*, 'AUVW_V  = ', sum(abs(dble(V )))
        IF(MYRANK.EQ.0) PRINT*, 'AUVW_W  = ', sum(abs(dble(W )))
      endif

      if(PRINT_SUM)then
        call collect_in_zero_array(DU )
        call collect_in_zero_array(DV )
        call collect_in_zero_array(UUU )
        call collect_in_zero_array(VVV )
        IF(MYRANK.EQ.0) PRINT*, 'AUVW_DU = ', sum(abs(dble(DU)))
        IF(MYRANK.EQ.0) PRINT*, 'AUVW_DV = ', sum(abs(dble(DV)))
        IF(MYRANK.EQ.0) PRINT*, 'AUVW_UUU  = ', sum(abs(dble(UUU )))
        IF(MYRANK.EQ.0) PRINT*, 'AUVW_VVV  = ', sum(abs(dble(VVV )))
      endif
      S1TIME=MPI_TIC()
      DO K=1,KS
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        UHE(L)=UHE(L)+CDZD(K)*DU(L,K)
        VHE(L)=VHE(L)+CDZD(K)*DV(L,K)
      ENDDO
      ENDDO
      MPI_WTIMES(110)=MPI_WTIMES(110)+MPI_TOC(S1TIME)
      S1TIME=MPI_TIC()
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        UHDY(L,KC)=UHE(L)*SUB(L)
        VHDX(L,KC)=VHE(L)*SVB(L)
      ENDDO
      MPI_WTIMES(111)=MPI_WTIMES(111)+MPI_TOC(S1TIME)
      S1TIME=MPI_TIC()
      DO K=KS,1,-1
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        UHDY(L,K)=UHDY(L,K+1)-DU(L,K)*SUB(L)
        VHDX(L,K)=VHDX(L,K+1)-DV(L,K)*SVB(L)
      ENDDO
      ENDDO
      MPI_WTIMES(112)=MPI_WTIMES(112)+MPI_TOC(S1TIME)
      S1TIME=MPI_TIC()
      DO K=1,KC
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        U(L,K)=UHDY(L,K)*HUI(L)
        V(L,K)=VHDX(L,K)*HVI(L)
        UHDY(L,K)=UHDY(L,K)*DYU(L)
        VHDX(L,K)=VHDX(L,K)*DXV(L)
      ENDDO
      ENDDO
      MPI_WTIMES(113)=MPI_WTIMES(113)+MPI_TOC(S1TIME)
C
      S1TIME=MPI_TIC()
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        TVAR3E(L)=SUM(UHDY(L,1:KC)*DZC(1:KC))-UHDYE(L)
        TVAR3N(L)=SUM(VHDX(L,1:KC)*DZC(1:KC))-VHDXE(L)
      ENDDO
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        UHDY(L,1:KC)=UHDY(L,1:KC)-TVAR3E(L)*DZIC(1:KC)
        VHDX(L,1:KC)=VHDX(L,1:KC)-TVAR3N(L)*DZIC(1:KC)
      ENDDO
      MPI_WTIMES(115)=MPI_WTIMES(115)+MPI_TOC(S1TIME)
      S1TIME=MPI_TIC()
C
C **  RESET VELOCITIES
C
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        UHE(L)=0.
        VHE(L)=0.
      ENDDO
      DO K=1,KC
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        UHE(L)=UHE(L)+UHDY(L,K)*DZC(K)
        VHE(L)=VHE(L)+VHDX(L,K)*DZC(K)
      ENDDO
      ENDDO
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        U(L,:)=UHDY(L,:)*HUI(L)
        V(L,:)=VHDX(L,:)*HVI(L)
      ENDDO
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        U(L,:)=U(L,:)*DYIU(L)
        V(L,:)=V(L,:)*DXIV(L)
      ENDDO
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        UHE(L)=UHE(L)*DYIU(L)
        VHE(L)=VHE(L)*DXIV(L)
      ENDDO
      MPI_WTIMES(116)=MPI_WTIMES(116)+MPI_TOC(S1TIME)
C
C **  UNCOMMENT BELOW TO WRITE CONTINUITY DIAGNOSITCS
C
C6661 FORMAT(' I,J,UHDYERMX = ',2I5,E14.5)
C6662 FORMAT(' I,J,UHDYERMN = ',2I5,E14.5)
C6663 FORMAT(' I,J,VHDYERMX = ',2I5,E14.5)
C6664 FORMAT(' I,J,VHDYERMX = ',2I5,E14.5)
C
C **  CALCULATE W
C
      IF(ISTL_.EQ.3)THEN
        S1TIME=MPI_TIC()
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          TVAR3E(L)=UHDYE(L+1   )
          TVAR3N(L)=VHDXE(LNC(L))
          TVAR3W(L)=UHDY2E(L+1   )
          TVAR3S(L)=VHDX2E(LNC(L))
        ENDDO
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            TVAR1E(L,K)=UHDY(L+1   ,K)
            TVAR1N(L,K)=VHDX(LNC(L),K)
            TVAR1W(L,K)=UHDY2(L+1   ,K)
            TVAR1S(L,K)=VHDX2(LNC(L),K)
          ENDDO
        ENDDO
        DO K=1,KS
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            W(L,K)=W(L,K-1) - 0.5*DZC(K)*DXYIP(L)*
     &          (TVAR1E(L,K)-UHDY(L,K)-TVAR3E(L)+UHDYE(L)
     &          +TVAR1W(L,K)-UHDY2(L,K)-TVAR3W(L)+UHDY2E(L)
     &          +TVAR1N(L,K)-VHDX(L,K)-TVAR3N(L)+VHDXE(L)
     &          +TVAR1S(L,K)-VHDX2(L,K)-TVAR3S(L)+VHDX2E(L))
     &          +(QSUM(L,K)-DZC(K)*QSUME(L))*DXYIP(L)
          ENDDO
        ENDDO
        MPI_WTIMES(117)=MPI_WTIMES(117)+MPI_TOC(S1TIME)
      ELSEIF(ISTL_.EQ.2)THEN
        S1TIME=MPI_TIC()
        call broadcast_boundary_array(UHDY, ic)
        call broadcast_boundary_array(UHDY1,ic)
        call broadcast_boundary_array(VHDX, ic)
        call broadcast_boundary_array(VHDX1,ic)
        call broadcast_boundary(UHDYE ,ic)
        call broadcast_boundary(UHDY1E,ic)
        call broadcast_boundary(VHDXE ,ic)
        call broadcast_boundary(VHDX1E,ic)
        DO K=1,KS
!$OMP PARALLEL DO PRIVATE(LN,LE)
          DO L=LMPI2,LMPILA
            LN=LNC(L)
            LE=L+1
            W(L,K)=W(L,K-1) - 0.5*DZC(K)*DXYIP(L)*
     &          ( UHDY(LE,K)- UHDY(L,K)- UHDYE(LE)+UHDYE(L)
     &          +UHDY1(LE,K)-UHDY1(L,K)-UHDY1E(LE)+UHDY1E(L)
     &          + VHDX(LN,K)- VHDX(L,K)- VHDXE(LN)+VHDXE(L)
     &          +VHDX1(LN,K)-VHDX1(L,K)-VHDX1E(LN)+VHDX1E(L))
     &          +(QSUM(L,K)-DZC(K)*QSUME(L) )*DXYIP(L)
          ENDDO
        ENDDO
        MPI_WTIMES(118)=MPI_WTIMES(118)+MPI_TOC(S1TIME)
      ENDIF
C
      ! *** APPLY OPEN BOUNDARYS
      S1TIME=MPI_TIC()
      DO LL=1,NBCSOP
        L=LOBCS(LL)
        DO K=1,KS
          W(L,K)=0.0
        ENDDO
      ENDDO
      MPI_WTIMES(119)=MPI_WTIMES(119)+MPI_TOC(S1TIME)

C 601 FORMAT(' IMAX,JMAX,QWSFMAX = ',2I5,E14.5)
C 602 FORMAT(' IMIN,JMIN,QWSFMIN = ',2I5,E14.5)
C 603 FORMAT(' TOTAL SURF Q ERR = ',E14.5)
C
      S1TIME=MPI_TIC()
      call broadcast_boundary_array(W,ic)
      MPI_WTIMES(140)=MPI_WTIMES(140)+MPI_TOC(S1TIME)
C
C **  CALCULATE U AND V ON OPEN BOUNDARIES
C
   30 CONTINUE
C
      if(PRINT_SUM)then
        call collect_in_zero_array(UHDY )
        call collect_in_zero_array(VHDX )
        call collect_in_zero_array(U )
        call collect_in_zero_array(V )
        call collect_in_zero_array(W )
        IF(MYRANK.EQ.0) PRINT*, '0UVW_UHDY = ', sum(abs(dble(UHDY)))
        IF(MYRANK.EQ.0) PRINT*, '0UVW_VHDX = ', sum(abs(dble(VHDX)))
        IF(MYRANK.EQ.0) PRINT*, '0UVW_U  = ', sum(abs(dble(U )))
        IF(MYRANK.EQ.0) PRINT*, '0UVW_V  = ', sum(abs(dble(V )))
        IF(MYRANK.EQ.0) PRINT*, '0UVW_W  = ', sum(abs(dble(W )))
      endif
      S1TIME=MPI_TIC()
      DO K=1,KC
        DO LL=1,NCBS
          L=LCBS(LL)
          LN=LNC(L)
          LNN=LNC(LN)
          IF(LN.NE.LC)THEN
            VHDX(LN,K)=VHDX(LNN,K)-VHDXE(LNN)+VHDXE(LN)
            V(LN,K)=VHDX(LN,K)/(HV(LN)*DXV(LN))
          ELSE
            VHDX(LN,K)=0.
            V(LN,K)=0.
          ENDIF
        ENDDO
      ENDDO
      DO K=1,KC
        DO LL=1,NCBW
          L=LCBW(LL)
          LP=L+1
          LPP=L+2
          IF(LP.NE.LC)THEN
            UHDY(LP,K)=UHDY(LPP,K)-UHDYE(LPP)+UHDYE(LP)
            U(LP,K)=UHDY(LP,K)/(HU(LP)*DYU(LP))
          ELSE
            UHDY(LP,K)=0.
            U(LP,K)=0.
          ENDIF
        ENDDO
      ENDDO
      DO K=1,KC
        DO LL=1,NCBE
          L=LCBE(LL)
          UHDY(L,K)=UHDY(L-1,K)-UHDYE(L-1)+UHDYE(L)
          U(L,K)=UHDY(L,K)/(HU(L)*DYU(L))
        ENDDO
      ENDDO
      DO K=1,KC
        DO LL=1,NCBN
          L=LCBN(LL)
          LS=LSC(L)
          VHDX(L,K)=VHDX(LS,K)-VHDXE(LS)+VHDXE(L)
          V(L,K)=VHDX(L,K)/(HV(L)*DXV(L))
        ENDDO
      ENDDO
      MPI_WTIMES(120)=MPI_WTIMES(120)+MPI_TOC(S1TIME)
C
C **  CALCULATE AVERAGE CELL FACE TRANSPORTS FOR SALT, TEMPERATURE AND
C **  SEDIMENT TRANSPORT AND PLACE IN UHDY2, VHDX2 AND W2
C
      if(PRINT_SUM)then
        call collect_in_zero_array(UHDY1 )
        call collect_in_zero_array(VHDX1 )
        call collect_in_zero_array(U1 )
        call collect_in_zero_array(V1 )
        call collect_in_zero_array(W1 )
        call collect_in_zero_array(UHDY )
        call collect_in_zero_array(VHDX )
        call collect_in_zero_array(U )
        call collect_in_zero_array(V )
        call collect_in_zero_array(W )
        IF(MYRANK.EQ.0) PRINT*, '1UVW_UHDY = ', sum(abs(dble(UHDY)))
        IF(MYRANK.EQ.0) PRINT*, '1UVW_VHDX = ', sum(abs(dble(VHDX)))
        IF(MYRANK.EQ.0) PRINT*, '1UVW_U  = ', sum(abs(dble(U )))
        IF(MYRANK.EQ.0) PRINT*, '1UVW_V  = ', sum(abs(dble(V )))
        IF(MYRANK.EQ.0) PRINT*, '1UVW_W  = ', sum(abs(dble(W )))
        IF(MYRANK.EQ.0) PRINT*, '1UVW_UHDY1 = ', sum(abs(dble(UHDY1)))
        IF(MYRANK.EQ.0) PRINT*, '1UVW_VHDX1 = ', sum(abs(dble(VHDX1)))
        IF(MYRANK.EQ.0) PRINT*, '1UVW_U1  = ', sum(abs(dble(U1 )))
        IF(MYRANK.EQ.0) PRINT*, '1UVW_V1  = ', sum(abs(dble(V1 )))
        IF(MYRANK.EQ.0) PRINT*, '1UVW_W1  = ', sum(abs(dble(W1 )))
      endif
      IF(ISTL_.EQ.2)THEN
        S1TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            UHDY2(L,K)=0.5*(UHDY(L,K)+UHDY1(L,K))
            VHDX2(L,K)=0.5*(VHDX(L,K)+VHDX1(L,K))
            U2(L,K)=0.5*(U(L,K)+U1(L,K))
            V2(L,K)=0.5*(V(L,K)+V1(L,K))
            W2(L,K)=0.5*(W(L,K)+W1(L,K))
          ENDDO
        ENDDO
        MPI_WTIMES(121)=MPI_WTIMES(121)+MPI_TOC(S1TIME)
      ELSE
        S1TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            UHDY2(L,K)=0.5*(UHDY(L,K)+UHDY2(L,K))
            VHDX2(L,K)=0.5*(VHDX(L,K)+VHDX2(L,K))
            U2(L,K)=0.5*(U(L,K)+U2(L,K))
            V2(L,K)=0.5*(V(L,K)+V2(L,K))
            W2(L,K)=0.5*(W(L,K)+W2(L,K))
          ENDDO
        ENDDO
        MPI_WTIMES(122)=MPI_WTIMES(122)+MPI_TOC(S1TIME)
      ENDIF
C
      if(PRINT_SUM)then
        call collect_in_zero_array(UHDY2 )
        call collect_in_zero_array(VHDX2 )
        call collect_in_zero_array(U2 )
        call collect_in_zero_array(V2 )
        call collect_in_zero_array(W2 )
        IF(MYRANK.EQ.0) PRINT*, '2UVW_UHDY2 = ', sum(abs(dble(UHDY2)))
        IF(MYRANK.EQ.0) PRINT*, '2UVW_VHDX2 = ', sum(abs(dble(VHDX2)))
        IF(MYRANK.EQ.0) PRINT*, '2UVW_U2  = ', sum(abs(dble(U2 )))
        IF(MYRANK.EQ.0) PRINT*, '2UVW_V2  = ', sum(abs(dble(V2 )))
        IF(MYRANK.EQ.0) PRINT*, '2UVW_W2  = ', sum(abs(dble(W2 )))
      endif
C
      IF(ISWVSD.GE.1)THEN
        S1TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            UHDY2(L,K)=UHDY2(L,K)+DYU(L)*UVPT(L,K)
            VHDX2(L,K)=VHDX2(L,K)+DXV(L)*VVPT(L,K)
            U2(L,K)=U2(L,K)+UVPT(L,K)/HMU(L)
            V2(L,K)=V2(L,K)+VVPT(L,K)/HMV(L)  ! *** Scott James
            W2(L,K)=W2(L,K)+WVPT(L,K)
          ENDDO
        ENDDO
        MPI_WTIMES(123)=MPI_WTIMES(123)+MPI_TOC(S1TIME)
      ENDIF
C
      if(PRINT_SUM)then
        call collect_in_zero_array(UHDY2 )
        call collect_in_zero_array(VHDX2 )
        call collect_in_zero_array(U2 )
        call collect_in_zero_array(V2 )
        call collect_in_zero_array(W2 )
        IF(MYRANK.EQ.0) PRINT*, '3UVW_UHDY2 = ', sum(abs(dble(UHDY2)))
        IF(MYRANK.EQ.0) PRINT*, '3UVW_VHDX2 = ', sum(abs(dble(VHDX2)))
        IF(MYRANK.EQ.0) PRINT*, '3UVW_U2  = ', sum(abs(dble(U2 )))
        IF(MYRANK.EQ.0) PRINT*, '3UVW_V2  = ', sum(abs(dble(V2 )))
        IF(MYRANK.EQ.0) PRINT*, '3UVW_W2  = ', sum(abs(dble(W2 )))
      endif
C
C **  ADDITIONAL 3D CONTINUITY ADJUSTED ADDED BELOW
C
      IF(KC.GT.1)THEN
        S1TIME=MPI_TIC()
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          TVAR3E(L)=SUM(UHDY2(L,:)*DZC(:))
          TVAR3N(L)=SUM(VHDX2(L,:)*DZC(:))
        ENDDO
        MPI_WTIMES(124)=MPI_WTIMES(124)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        call broadcast_boundary(TVAR3E,ic)
        call broadcast_boundary(TVAR3N,ic)
        MPI_WTIMES(141)=MPI_WTIMES(141)+MPI_TOC(S1TIME)
        IF(ISGWIE.GE.1)THEN
        IF(ISTL_.EQ.3)THEN
          S1TIME=MPI_TIC()
!$OMP PARALLEL DO PRIVATE(HPPTMP)
          DO L=LMPI2,LMPILA
            HPPTMP=H2P(L)+DELT*DXYIP(L)*( QSUME(L)
     &          -TVAR3E(L+1)+TVAR3E(L)
     &          -TVAR3N(LNC(L)) +TVAR3N(L) )
     &          -DELT*DXYIP(L)*(RIFTR(L)+EVAPSW(L))
            HP(L)=SPB(L)*HPPTMP+(1.-SPB(L))*(GI*P(L)-BELV(L))
            HPI(L)=1./HP(L)
          ENDDO
          MPI_WTIMES(125)=MPI_WTIMES(125)+MPI_TOC(S1TIME)
        ELSE
          S1TIME=MPI_TIC()
!$OMP PARALLEL DO PRIVATE(HPPTMP)
          DO L=LMPI2,LMPILA
            HPPTMP=H1P(L)+DELT*DXYIP(L)*( QSUME(L)
     &          -TVAR3E(L+1)+TVAR3E(L)
     &          -TVAR3N(LNC(L)) +TVAR3N(L) )
     &          -DELT*DXYIP(L)*(RIFTR(L)+EVAPSW(L))
            HP(L)=SPB(L)*HPPTMP+(1.-SPB(L))*(GI*P(L)-BELV(L))
            HPI(L)=1./HP(L)
          ENDDO
          MPI_WTIMES(126)=MPI_WTIMES(126)+MPI_TOC(S1TIME)
        ENDIF
        ELSE
        IF(ISTL_.EQ.3)THEN
          S1TIME=MPI_TIC()
!$OMP PARALLEL DO PRIVATE(HPPTMP)
          DO L=LMPI2,LMPILA
            HPPTMP=H2P(L)+DELT*DXYIP(L)*( QSUME(L)
     &          -TVAR3E(L+1)+TVAR3E(L)
     &          -TVAR3N(LNC(L)) +TVAR3N(L) )
            HP(L)=SPB(L)*HPPTMP+(1.-SPB(L))*(GI*P(L)-BELV(L))
            HPI(L)=1./HP(L)
          ENDDO
          MPI_WTIMES(127)=MPI_WTIMES(127)+MPI_TOC(S1TIME)
        ELSE
          S1TIME=MPI_TIC()
!$OMP PARALLEL DO PRIVATE(HPPTMP)
          DO L=LMPI2,LMPILA
            HPPTMP=H1P(L)+DELT*DXYIP(L)*( QSUME(L)
     &          -TVAR3E(L+1)+TVAR3E(L)
     &          -TVAR3N(LNC(L)) +TVAR3N(L) )
            HP(L)=SPB(L)*HPPTMP+(1.-SPB(L))*(GI*P(L)-BELV(L))
            HPI(L)=1./HP(L)
          ENDDO
          MPI_WTIMES(128)=MPI_WTIMES(128)+MPI_TOC(S1TIME)
        ENDIF
        ENDIF
        IF(MDCHH.GE.1)THEN
          S1TIME=MPI_TIC()
          RLAMN=QCHERR
          RLAMO=1.-RLAMN
          DO NMD=1,MDCHH
            LHOST=LMDCHH(NMD)
            LCHNU=LMDCHU(NMD)
            LCHNV=LMDCHV(NMD)
            IF(MDCHTYP(NMD).EQ.1)THEN
              TMPVAL=DELT*(RLAMN*QCHANU(NMD)+RLAMO*QCHANUN(NMD))
              HP(LHOST)=HP(LHOST)+TMPVAL*DXYIP(LHOST)
              HP(LCHNU)=HP(LCHNU)-TMPVAL*DXYIP(LCHNU)
              HPI(LHOST)=1./HP(LHOST)
              HPI(LCHNU)=1./HP(LCHNU)
            ENDIF
            IF(MDCHTYP(NMD).EQ.2)THEN
              TMPVAL=DELT*(RLAMN*QCHANV(NMD)+RLAMO*QCHANVN(NMD))
              HP(LHOST)=HP(LHOST)+TMPVAL*DXYIP(LHOST)
              HP(LCHNV)=HP(LCHNV)-TMPVAL*DXYIP(LCHNV)
              HPI(LHOST)=1./HP(LHOST)
              HPI(LCHNV)=1./HP(LCHNV)
            ENDIF
          ENDDO
        ENDIF
        MPI_WTIMES(129)=MPI_WTIMES(129)+MPI_TOC(S1TIME)
      ENDIF
C
      S1TIME=MPI_TIC()
      call broadcast_boundary(HP,IC)
      call broadcast_boundary(HPI,IC)
      MPI_WTIMES(142)=MPI_WTIMES(142)+MPI_TOC(S1TIME)
C
C **  ACCUMULTATE MAX COURANT NUMBERS
C
C *** DSLLC BEGIN BLOCK
      IF(ISINWV.EQ.1.OR.ISNEGH.GT.0)THEN
        S1TIME=MPI_TIC()
        DO K=1,KC
!$OMP PARALLEL DO PRIVATE(CFLUUUT,CFLVVVT,CFLWWWT,CFLCACT)
          DO L=LMPI2,LMPILA
            CFLUUUT=DELT*ABS(DXIU(L)*U(L,K))
            CFLUUU(L,K)=MAX(CFLUUUT,CFLUUU(L,K))
            CFLVVVT=DELT*ABS(DYIV(L)*V(L,K))
            CFLVVV(L,K)=MAX(CFLVVVT,CFLVVV(L,K))
            CFLWWWT=DELT*ABS(HPI(L)*DZIG(K)*W(L,K))
            CFLWWW(L,K)=MAX(CFLWWWT,CFLWWW(L,K))
            CFLCACT=DELT*ABS(CAC(L,K)*DXYIP(L)*HPI(L))
            CFLCAC(L,K)=MAX(CFLCACT,CFLCAC(L,K))
          ENDDO
        ENDDO
        MPI_WTIMES(130)=MPI_WTIMES(130)+MPI_TOC(S1TIME)
      ENDIF
C *** DSLLC END BLOCK
C
C ** CALCULATE NONHYDROSTATIC PRESSURE
C
      S1TIME=MPI_TIC()
      IF(KC.GT.1.AND.ISPNHYDS.GE.1) CALL CALPNHS_mpi
      MPI_WTIMES(131)=MPI_WTIMES(131)+MPI_TOC(S1TIME)
C
C **  WRITE TO DIAGNOSTIC FILE CFL.OUT WITH DIAGNOSTICS OF MAXIMUM
C **  TIME STEP
C **  SEDIMENT TRANSPORT AND PLACE IN UHDY2, VHDX2 AND W2
C
!      IF(ISCFL.GE.1.AND.ISTL_.EQ.3.AND.DEBUG)THEN
!      IF(ISCFL.GE.1.AND.DEBUG)THEN
      IF(ISCFL.GE.1)THEN
        S1TIME=MPI_TIC()
        IF(MYRANK.EQ.0)THEN
        OPEN(1,FILE='CFL.OUT',STATUS='UNKNOWN',POSITION='APPEND')
        ENDIF
        IF(ISCFLM.GE.1.AND.N.EQ.1)THEN
          IF(MYRANK.EQ.0)THEN
          OPEN(2,FILE='CFLMP.OUT',STATUS='UNKNOWN')
          CLOSE(2,STATUS='DELETE')
          ENDIF
          DO L=1,LC
            ICFLMP(L)=0
          ENDDO
        ENDIF
        MPI_WTIMES(132)=MPI_WTIMES(132)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        DTCFL=1.E+18
        K=1
!$OMP PARALLEL DO PRIVATE(LN,UWTMP,UETMP,VSTMP,VNTMP,WBTMP,WTTMP,DTMAXI,DTMAX
!$OMP+  ICFL,JCFL,KCFL) FIRSTPRIVATE(DTCFL)
        DO L=LMPI2,LMPILA
          LN=LNC(L)
          UWTMP=ABS(DXIU(L  )*U2(L  ,K))
          UETMP=ABS(DXIU(L+1)*U2(L+1,K))
          VSTMP=ABS(DYIV(L  )*V2(L  ,K))
          VNTMP=ABS(DYIV(LN )*U2(LN ,K))
          WBTMP=0.
          WTTMP=ABS(HPI(L)*DZIC(K)*W2(L,K))
          DTMAXI=MAX(UWTMP,UETMP)+MAX(VSTMP,VNTMP)+MAX(WBTMP,WTTMP)
     &        +1.0E-12
          DTMAX=0.5/DTMAXI
          IF(DTMAX.LT.DTCFL)THEN
            DTCFL=DTMAX
            ICFL=IL(L)
            JCFL=JL(L)
            KCFL=K
          ENDIF
        ENDDO
        MPI_WTIMES(133)=MPI_WTIMES(133)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(KC.GT.1)THEN
          K=KC
!$OMP PARALLEL DO PRIVATE(LN,UWTMP,UETMP,VSTMP,VNTMP,WBTMP,WTTMP,DTMAXI,DTMAX
!$OMP+  ICFL,JCFL,KCFL) FIRSTPRIVATE(DTCFL)
          DO L=LMPI2,LMPILA
            LN=LNC(L)
            UWTMP=ABS(DXIU(L  )*U2(L  ,K))
            UETMP=ABS(DXIU(L+1)*U2(L+1,K))
            VSTMP=ABS(DYIV(L  )*V2(L  ,K))
            VNTMP=ABS(DYIV(LN )*U2(LN ,K))
            WTTMP=0.
            WBTMP=ABS(HPI(L)*DZIC(K)*W2(L,K-1))
            DTMAXI=MAX(UWTMP,UETMP)+MAX(VSTMP,VNTMP)+MAX(WBTMP,WTTMP)
     &          +1.0E-12
            DTMAX=0.5/DTMAXI
            IF(DTMAX.LT.DTCFL)THEN
              DTCFL=DTMAX
              ICFL=IL(L)
              JCFL=JL(L)
              KCFL=K
            ENDIF
          ENDDO
        ENDIF
        MPI_WTIMES(134)=MPI_WTIMES(134)+MPI_TOC(S1TIME)
        S1TIME=MPI_TIC()
        IF(KC.GT.2)THEN
          DO K=2,KS
!$OMP PARALLEL DO PRIVATE(LN,UWTMP,UETMP,VSTMP,VNTMP,WBTMP,WTTMP,DTMAXI,DTMAX
!$OMP+  ICFL,JCFL,KCFL) FIRSTPRIVATE(DTCFL)
            DO L=LMPI2,LMPILA
              LN=LNC(L)
              UWTMP=ABS(DXIU(L  )*U2(L  ,K))
              UETMP=ABS(DXIU(L+1)*U2(L+1,K))
              VSTMP=ABS(DYIV(L  )*V2(L  ,K))
              VNTMP=ABS(DYIV(LN )*U2(LN ,K))
              WBTMP=ABS(HPI(L)*DZIC(K)*W2(L,K-1))
              WTTMP=ABS(HPI(L)*DZIC(K)*W2(L,K  ))
              DTMAXI=MAX(UWTMP,UETMP)+MAX(VSTMP,VNTMP)+MAX(WBTMP,WTTMP)
     &            +1.0E-12
              DTMAX=0.5/DTMAXI
              IF(DTMAX.LT.DTCFL)THEN
                DTCFL=DTMAX
                ICFL=IL(L)
                JCFL=JL(L)
                KCFL=K
              ENDIF
            ENDDO
          ENDDO
        ENDIF
        MPI_WTIMES(135)=MPI_WTIMES(135)+MPI_TOC(S1TIME)

        IF(.FALSE.)THEN
        S1TIME=MPI_TIC()
        IVAL=MOD(N,ISCFL)
        IDTCFL=NINT(DTCFL)
        MPI_WTIMES(136)=MPI_WTIMES(136)+MPI_TOC(S1TIME)

        S1TIME=MPI_TIC()
        IF(MYRANK.EQ.0)THEN
        IF(ISCFL.EQ.1) WRITE(1,1212)DTCFL,N,ICFL,JCFL,KCFL
        IF(ISCFL.GE.2.AND.IVAL.EQ.0 ) WRITE(1,1213)IDTCFL
        ENDIF
        MPI_WTIMES(137)=MPI_WTIMES(137)+MPI_TOC(S1TIME)

        S1TIME=MPI_TIC()
        IF(ISCFLM.GE.1 )THEN
          LTMP=LIJ(ICFL,JCFL)
          ICFLMP(LTMP)=ICFLMP(LTMP)+1
        ENDIF
        IF(ISCFLM.GE.1.AND.N.EQ.NTS)THEN
          IF(MYRANK.EQ.0)THEN
          OPEN(2,FILE='CFLMP.OUT',STATUS='UNKNOWN')
          TMPVALN=1./FLOAT(NTS)
          DO L=2,LA
            TMPVAL=TMPVALN*FLOAT(ICFLMP(L))
            WRITE(2,1214)IL(L),JL(L),ICFLMP(L),TMPVAL
          ENDDO
          CLOSE(2)
          ENDIF
        ENDIF
        MPI_WTIMES(138)=MPI_WTIMES(138)+MPI_TOC(S1TIME)
        ENDIF
        IF(MYRANK.EQ.0) CLOSE(1)

      ENDIF
 1212 FORMAT(' MAX TIME STEP =',F10.2,' SEC FOR N,I,J,K =',I8,3I5)
 1213 FORMAT(I4)
 1214 FORMAT(2I5,I12,F10.2)
      RETURN
      END

