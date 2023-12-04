      SUBROUTINE CALWQC_mpi(ISTL_,IS2TL_)
C
C CHANGE RECORD
C **  SUBROUTINE CALWQC CALCULATES THE CONCENTRATION OF DISSOLVED AND
C **  SUSPENDED WATER QUALITY CONSTITUTENTS AT TIME LEVEL (N+1).
C **  CALLED ONLY ON ODD THREE TIME LEVEL STEPS
C
      USE GLOBAL
      USE MPI

      LOGICAL WQC_MPI
C
      WQC_MPI=.TRUE.
C
      DELT=DT2
      IF(IS2TIM.GE.1) THEN
        IF(ISDYNSTP.EQ.0)THEN
          DELT=DT
          ISUD=0
        ELSE
          DELT=DTDYN
          ISUD=0
        END IF
      ENDIF
C
C **  UPDATED TIME SERIES CONCENTRATION BOUNDARY CONDITIONS
C **  3D ADVECTI0N TRANSPORT CALCULATION
C
CGEO        DO NW=0,NWQV
CGEO          call collect_in_zero_array(WQV(:,:,NW))
CGEO        ENDDO
CGEO        IF(MYRANK.EQ.0)THEN
CGEO          PRINT*, n,'h1WQV  = ', sum(abs(dble(WQV)))
CGEO        ENDIF
      S2TIME=MPI_TIC()
      DO NW=1,NWQV
        IF(ISTRWQ(NW).EQ.1)THEN
          CALL CALTRAN_mpi(ISTL_,IS2TL_,8,NW,WQV(1,1,NW),WQV(1,1,NW))
        ENDIF
      ENDDO
      MPI_WTIMES(721)=MPI_WTIMES(721)+MPI_TOC(S2TIME)
C
CGEO        DO NW=0,NWQV
CGEO          call collect_in_zero_array(WQV(:,:,NW))
CGEO        ENDDO
CGEO        IF(MYRANK.EQ.0)THEN
CGEO          PRINT*, n,'h2WQV  = ', sum(abs(dble(WQV)))
CGEO        ENDIF
      S2TIME=MPI_TIC()
      DO NSP=1,NXSP
       CALL CALTRAN_mpi(ISTL_,IS2TL_,8,NSP+NWQV,
     &                  WQVX(1,1,NSP),WQVX(1,1,NSP))
      ENDDO
      MPI_WTIMES(722)=MPI_WTIMES(722)+MPI_TOC(S2TIME)
CGEO      DO NSP=1,NXSP; call collect_in_zero_array(WQVX(:,:,NSP)); ENDDO
CGEO      DO NSP=1,NXSP
CGEO      IF(MYRANK.EQ.0) PRINT*, 'a2',nsp,sum(abs(dble(WQVX(:,:,NSP))))
CGEO      ENDDO
C
C **  CALLS TO SOURCE-SINK CALCULATIONS
C **  BYPASS OR INITIALIZE VERTICAL DIFFUSION CALCULATION
C
      IF(KC.EQ.1) GOTO 2000
      S2TIME=MPI_TIC()
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA
        HWQI(L)=1./HWQ(L)
      ENDDO
      MPI_WTIMES(723)=MPI_WTIMES(723)+MPI_TOC(S2TIME)
C
C **  VERTICAL DIFFUSION CALCULATION LEVEL 1
C
      IF(ISWQLVL.EQ.1)THEN
        S2TIME=MPI_TIC()
        RCDZKK=-DELT*CDZKK(1)
        DO NW=1,NWQV
!$OMP PARALLEL DO PRIVATE(CCUBTMP,CCMBTMP,EEB)
          DO L=LMPI2,LMPILA
            CCUBTMP=RCDZKK*HWQI(L)*AB(L,1)
            CCMBTMP=1.-CCUBTMP
            EEB=1./CCMBTMP
            CU1(L,1)=CCUBTMP*EEB
            WQV(L,1,NW)=WQV(L,1,NW)*EEB
          ENDDO
        ENDDO
        MPI_WTIMES(724)=MPI_WTIMES(724)+MPI_TOC(S2TIME)
C
        S2TIME=MPI_TIC()
        DO NW=1,NWQV
          DO K=2,KS
            RCDZKMK=-DELT*CDZKMK(K)
            RCDZKK=-DELT*CDZKK(K)
!$OMP PARALLEL DO PRIVATE(CCLBTMP,CCUBTMP,CCMBTMP,EEB)
            DO L=LMPI2,LMPILA
              CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
              CCUBTMP=RCDZKK*HWQI(L)*AB(L,K)
              CCMBTMP=1.-CCLBTMP-CCUBTMP
              EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
              CU1(L,K)=CCUBTMP*EEB
              WQV(L,K,NW)=(WQV(L,K,NW)-CCLBTMP*WQV(L,K-1,NW))*EEB
            ENDDO
          ENDDO
        ENDDO
        MPI_WTIMES(725)=MPI_WTIMES(725)+MPI_TOC(S2TIME)
C
        S2TIME=MPI_TIC()
        K=KC
        RCDZKMK=-DELT*CDZKMK(K)
        DO NW=1,NWQV
!$OMP PARALLEL DO PRIVATE(CCLBTMP,CCMBTMP,EEB)
          DO L=LMPI2,LMPILA
            CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
            CCMBTMP=1.-CCLBTMP
            EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
            WQV(L,K,NW)=(WQV(L,K,NW)-CCLBTMP*WQV(L,K-1,NW))*EEB
          ENDDO
        ENDDO
        DO NW=1,NWQV
          DO K=KC-1,1,-1
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              WQV(L,K,NW)=WQV(L,K,NW)-CU1(L,K)*WQV(L,K+1,NW)
            ENDDO
          ENDDO
        ENDDO
        MPI_WTIMES(726)=MPI_WTIMES(726)+MPI_TOC(S2TIME)
C
C **  VERTICAL DIFFUSION CALCULATION LEVEL 2
C
      ELSEIF(ISWQLVL.EQ.2)THEN
C
        S2TIME=MPI_TIC()
        RCDZKK=-DELT*CDZKK(1)
        DO NW=1,NWQV
!$OMP PARALLEL DO PRIVATE(CCUBTMP,CCMBTMP,EEB)
          DO L=LMPI2,LMPILA
            CCUBTMP=RCDZKK*HWQI(L)*AB(L,1)
            CCMBTMP=1.-CCUBTMP
            EEB=1./CCMBTMP
            CU1(L,1)=CCUBTMP*EEB
            WQV(L,1,NW)=WQV(L,1,NW)*EEB
          ENDDO
        ENDDO
        MPI_WTIMES(727)=MPI_WTIMES(727)+MPI_TOC(S2TIME)
C
        S2TIME=MPI_TIC()
        DO NW=1,NWQV
          DO K=2,KS
            RCDZKMK=-DELT*CDZKMK(K)
            RCDZKK=-DELT*CDZKK(K)
!$OMP PARALLEL DO PRIVATE(CCLBTMP,CCUBTMP,CCMBTMP,EEB)
            DO L=LMPI2,LMPILA
              CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
              CCUBTMP=RCDZKK*HWQI(L)*AB(L,K)
              CCMBTMP=1.-CCLBTMP-CCUBTMP
              EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
              CU1(L,K)=CCUBTMP*EEB
              WQV(L,K,NW)=(WQV(L,K,NW)-CCLBTMP*WQV(L,K-1,NW))*EEB
            ENDDO
          ENDDO
        ENDDO
        MPI_WTIMES(728)=MPI_WTIMES(728)+MPI_TOC(S2TIME)
C
        S2TIME=MPI_TIC()
        K=KC
        RCDZKMK=-DELT*CDZKMK(K)
        DO NW=1,NWQV
!$OMP PARALLEL DO PRIVATE(CCLBTMP,CCMBTMP,EEB)
          DO L=LMPI2,LMPILA
            CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
            CCMBTMP=1.-CCLBTMP
            EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
            WQV(L,K,NW)=(WQV(L,K,NW)-CCLBTMP*WQV(L,K-1,NW))*EEB
          ENDDO
        ENDDO
       DO NW=1,NWQV
          DO K=KC-1,1,-1
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              WQV(L,K,NW)=WQV(L,K,NW)-CU1(L,K)*WQV(L,K+1,NW)
            ENDDO
          ENDDO
        ENDDO
        MPI_WTIMES(729)=MPI_WTIMES(729)+MPI_TOC(S2TIME)
C
C **  VERTICAL DIFFUSION CALCULATION LEVEL 3
C
      ELSEIF(ISWQLVL.EQ.3)THEN
        S2TIME=MPI_TIC()
        IF(.FALSE.)THEN
        RCDZKK=-DELT*CDZKK(1)
        DO ND=1,NDM
          LF=2+(ND-1)*LDM
          LL=LF+LDM-1
          DO L=LF,LL
            CCUBTMP=RCDZKK*HWQI(L)*AB(L,1)
            CCMBTMP=1.-CCUBTMP
            EEB=1./CCMBTMP
            CU1(L,1)=CCUBTMP*EEB
            WQV(L,1, 1)=WQV(L,1, 1)*EEB
            WQV(L,1, 2)=WQV(L,1, 2)*EEB
            WQV(L,1, 3)=WQV(L,1, 3)*EEB
            WQV(L,1, 4)=WQV(L,1, 4)*EEB
            WQV(L,1, 5)=WQV(L,1, 5)*EEB
            WQV(L,1, 6)=WQV(L,1, 6)*EEB
            WQV(L,1, 7)=WQV(L,1, 7)*EEB
            WQV(L,1, 8)=WQV(L,1, 8)*EEB
            WQV(L,1, 9)=WQV(L,1, 9)*EEB
            WQV(L,1,10)=WQV(L,1,10)*EEB
            WQV(L,1,11)=WQV(L,1,11)*EEB
            WQV(L,1,12)=WQV(L,1,12)*EEB
            WQV(L,1,13)=WQV(L,1,13)*EEB
            WQV(L,1,14)=WQV(L,1,14)*EEB
            WQV(L,1,15)=WQV(L,1,15)*EEB
            WQV(L,1,16)=WQV(L,1,16)*EEB
            WQV(L,1,17)=WQV(L,1,17)*EEB
            WQV(L,1,18)=WQV(L,1,18)*EEB
            WQV(L,1,19)=WQV(L,1,19)*EEB
            WQV(L,1,20)=WQV(L,1,20)*EEB
            WQV(L,1,21)=WQV(L,1,21)*EEB
          ENDDO
          !{ GEOSR X-species : jgcho 2015.11.09
          if (NXSP.gt.0) then
            DO L=LF,LL
              CCUBTMP=RCDZKK*HWQI(L)*AB(L,1)
              CCMBTMP=1.-CCUBTMP
              EEB=1./CCMBTMP
              CU1(L,1)=CCUBTMP*EEB
              DO nsp=1,NXSP
                WQVX(L,1,nsp)=WQVX(L,1,nsp)*EEB
              ENDDO
            enddo
          endif
          !} GEOSR X-species : jgcho 2015.11.09
        ENDDO
        ELSE
        RCDZKK=-DELT*CDZKK(1)
        DO NW=1,NWQV
!$OMP PARALLEL DO PRIVATE(CCUBTMP,CCMBTMP,EEB)
          DO L=LMPI2,LMPILA
            CCUBTMP=RCDZKK*HWQI(L)*AB(L,1)
            CCMBTMP=1.-CCUBTMP
            EEB=1./CCMBTMP
            CU1(L,1)=CCUBTMP*EEB
            WQV(L,1,NW)=WQV(L,1,NW)*EEB
          ENDDO
        ENDDO
        IF(NXSP.GT.0)THEN
        DO NSP=1,NXSP
!$OMP PARALLEL DO PRIVATE(CCUBTMP,CCMBTMP,EEB)
          DO L=LMPI2,LMPILA
            CCUBTMP=RCDZKK*HWQI(L)*AB(L,1)
            CCMBTMP=1.-CCUBTMP
            EEB=1./CCMBTMP
            CU1(L,1)=CCUBTMP*EEB
            WQVX(L,1,NSP)=WQVX(L,1,NSP)*EEB
          ENDDO
        ENDDO
        ENDIF
        ENDIF
        MPI_WTIMES(730)=MPI_WTIMES(730)+MPI_TOC(S2TIME)
C
        IF(.FALSE.)THEN
        DO ND=1,NDM
          LF=2+(ND-1)*LDM
          LL=LF+LDM-1
          DO K=2,KS
            RCDZKMK=-DELT*CDZKMK(K)
            RCDZKK=-DELT*CDZKK(K)
            DO L=LF,LL
              CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
              CCUBTMP=RCDZKK*HWQI(L)*AB(L,K)
              CCMBTMP=1.-CCLBTMP-CCUBTMP
              EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
              CU1(L,K)=CCUBTMP*EEB
              WQV(L,K, 1)=(WQV(L,K, 1)-CCLBTMP*WQV(L,K-1, 1))*EEB
              WQV(L,K, 2)=(WQV(L,K, 2)-CCLBTMP*WQV(L,K-1, 2))*EEB
              WQV(L,K, 3)=(WQV(L,K, 3)-CCLBTMP*WQV(L,K-1, 3))*EEB
              WQV(L,K, 4)=(WQV(L,K, 4)-CCLBTMP*WQV(L,K-1, 4))*EEB
              WQV(L,K, 5)=(WQV(L,K, 5)-CCLBTMP*WQV(L,K-1, 5))*EEB
              WQV(L,K, 6)=(WQV(L,K, 6)-CCLBTMP*WQV(L,K-1, 6))*EEB
              WQV(L,K, 7)=(WQV(L,K, 7)-CCLBTMP*WQV(L,K-1, 7))*EEB
              WQV(L,K, 8)=(WQV(L,K, 8)-CCLBTMP*WQV(L,K-1, 8))*EEB
              WQV(L,K, 9)=(WQV(L,K, 9)-CCLBTMP*WQV(L,K-1, 9))*EEB
              WQV(L,K,10)=(WQV(L,K,10)-CCLBTMP*WQV(L,K-1,10))*EEB
              WQV(L,K,11)=(WQV(L,K,11)-CCLBTMP*WQV(L,K-1,11))*EEB
              WQV(L,K,12)=(WQV(L,K,12)-CCLBTMP*WQV(L,K-1,12))*EEB
              WQV(L,K,13)=(WQV(L,K,13)-CCLBTMP*WQV(L,K-1,13))*EEB
              WQV(L,K,14)=(WQV(L,K,14)-CCLBTMP*WQV(L,K-1,14))*EEB
              WQV(L,K,15)=(WQV(L,K,15)-CCLBTMP*WQV(L,K-1,15))*EEB
              WQV(L,K,16)=(WQV(L,K,16)-CCLBTMP*WQV(L,K-1,16))*EEB
              WQV(L,K,17)=(WQV(L,K,17)-CCLBTMP*WQV(L,K-1,17))*EEB
              WQV(L,K,18)=(WQV(L,K,18)-CCLBTMP*WQV(L,K-1,18))*EEB
              WQV(L,K,19)=(WQV(L,K,19)-CCLBTMP*WQV(L,K-1,19))*EEB
              WQV(L,K,20)=(WQV(L,K,20)-CCLBTMP*WQV(L,K-1,20))*EEB
              WQV(L,K,21)=(WQV(L,K,21)-CCLBTMP*WQV(L,K-1,21))*EEB
            ENDDO
          ENDDO
          !{ GEOSR X-species : jgcho 2015.11.09
          if (NXSP.gt.0) then
            DO K=2,KS
              RCDZKMK=-DELT*CDZKMK(K)
              RCDZKK=-DELT*CDZKK(K)
              DO L=LF,LL
                CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
                CCUBTMP=RCDZKK*HWQI(L)*AB(L,K)
                CCMBTMP=1.-CCLBTMP-CCUBTMP
                EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
                CU1(L,K)=CCUBTMP*EEB
                DO nsp=1,NXSP
                  WQVX(L,K,nsp)=(WQVX(L,K,nsp)-CCLBTMP*WQVX(L,K-1,nsp))
     &                  *EEB
                ENDDO
              enddo
            enddo
          endif
          !} GEOSR X-species : jgcho 2015.11.09
        ENDDO
        ELSE
        S2TIME=MPI_TIC()
C        DO NW=1,NWQV
          DO K=2,KS
            RCDZKMK=-DELT*CDZKMK(K)
            RCDZKK=-DELT*CDZKK(K)
!$OMP PARALLEL DO PRIVATE(CCLBTMP,CCUBTMP,CCMBTMP,EEB)
            DO L=LMPI2,LMPILA
              CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
              CCUBTMP=RCDZKK*HWQI(L)*AB(L,K)
              CCMBTMP=1.-CCLBTMP-CCUBTMP
              EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
              CU1(L,K)=CCUBTMP*EEB
              WQV(L,K,1:NWQV)=(WQV(L,K,1:NWQV)-CCLBTMP
     &                        *WQV(L,K-1,1:NWQV))*EEB
            ENDDO
          ENDDO
C        ENDDO
        MPI_WTIMES(731)=MPI_WTIMES(731)+MPI_TOC(S2TIME)
        S2TIME=MPI_TIC()
        IF(NXSP.GT.0)THEN
C        DO NSP=1,NXSP
          DO K=2,KS
            RCDZKMK=-DELT*CDZKMK(K)
            RCDZKK=-DELT*CDZKK(K)
!$OMP PARALLEL DO PRIVATE(CCLBTMP,CCUBTMP,CCMBTMP,EEB)
            DO L=LMPI2,LMPILA
              CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
              CCUBTMP=RCDZKK*HWQI(L)*AB(L,K)
              CCMBTMP=1.-CCLBTMP-CCUBTMP
              EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
              CU1(L,K)=CCUBTMP*EEB
              WQVX(L,K,1:NXSP)=(WQVX(L,K,1:NXSP)-CCLBTMP
     &                         *WQVX(L,K-1,1:NXSP))*EEB
            ENDDO
          ENDDO
C        ENDDO
        ENDIF
        MPI_WTIMES(732)=MPI_WTIMES(732)+MPI_TOC(S2TIME)
        ENDIF
C
        IF(.FALSE.)THEN
        K=KC
        RCDZKMK=-DELT*CDZKMK(K)
        DO ND=1,NDM
          LF=2+(ND-1)*LDM
          LL=LF+LDM-1
          DO L=LF,LL
            CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
            CCMBTMP=1.-CCLBTMP
            EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
            WQV(L,K, 1)=(WQV(L,K, 1)-CCLBTMP*WQV(L,K-1, 1))*EEB
            WQV(L,K, 2)=(WQV(L,K, 2)-CCLBTMP*WQV(L,K-1, 2))*EEB
            WQV(L,K, 3)=(WQV(L,K, 3)-CCLBTMP*WQV(L,K-1, 3))*EEB
            WQV(L,K, 4)=(WQV(L,K, 4)-CCLBTMP*WQV(L,K-1, 4))*EEB
            WQV(L,K, 5)=(WQV(L,K, 5)-CCLBTMP*WQV(L,K-1, 5))*EEB
            WQV(L,K, 6)=(WQV(L,K, 6)-CCLBTMP*WQV(L,K-1, 6))*EEB
            WQV(L,K, 7)=(WQV(L,K, 7)-CCLBTMP*WQV(L,K-1, 7))*EEB
            WQV(L,K, 8)=(WQV(L,K, 8)-CCLBTMP*WQV(L,K-1, 8))*EEB
            WQV(L,K, 9)=(WQV(L,K, 9)-CCLBTMP*WQV(L,K-1, 9))*EEB
            WQV(L,K,10)=(WQV(L,K,10)-CCLBTMP*WQV(L,K-1,10))*EEB
            WQV(L,K,11)=(WQV(L,K,11)-CCLBTMP*WQV(L,K-1,11))*EEB
            WQV(L,K,12)=(WQV(L,K,12)-CCLBTMP*WQV(L,K-1,12))*EEB
            WQV(L,K,13)=(WQV(L,K,13)-CCLBTMP*WQV(L,K-1,13))*EEB
            WQV(L,K,14)=(WQV(L,K,14)-CCLBTMP*WQV(L,K-1,14))*EEB
            WQV(L,K,15)=(WQV(L,K,15)-CCLBTMP*WQV(L,K-1,15))*EEB
            WQV(L,K,16)=(WQV(L,K,16)-CCLBTMP*WQV(L,K-1,16))*EEB
            WQV(L,K,17)=(WQV(L,K,17)-CCLBTMP*WQV(L,K-1,17))*EEB
            WQV(L,K,18)=(WQV(L,K,18)-CCLBTMP*WQV(L,K-1,18))*EEB
            WQV(L,K,19)=(WQV(L,K,19)-CCLBTMP*WQV(L,K-1,19))*EEB
            WQV(L,K,20)=(WQV(L,K,20)-CCLBTMP*WQV(L,K-1,20))*EEB
            WQV(L,K,21)=(WQV(L,K,21)-CCLBTMP*WQV(L,K-1,21))*EEB
          ENDDO
          !{ GEOSR X-species : jgcho 2015.11.09
          if (NXSP.gt.0) then
            DO L=LF,LL
              CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
              CCMBTMP=1.-CCLBTMP
              EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
              DO nsp=1,NXSP
                WQVX(L,K,nsp)=(WQVX(L,K,nsp)-CCLBTMP*WQVX(L,K-1,nsp))
     &                *EEB
              ENDDO
            enddo
          endif
          !} GEOSR X-species : jgcho 2015.11.09
        ENDDO
        ELSE
        S2TIME=MPI_TIC()
        K=KC
        RCDZKMK=-DELT*CDZKMK(K)
C        DO NW=1,NWQV
!$OMP PARALLEL DO PRIVATE(CCLBTMP,CCMBTMP,EEB)
          DO L=LMPI2,LMPILA
            CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
            CCMBTMP=1.-CCLBTMP
            EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
            WQV(L,K,1:NWQV)=(WQV(L,K,1:NWQV)-CCLBTMP
     &                      *WQV(L,K-1,1:NWQV))*EEB
          ENDDO
C        ENDDO
        MPI_WTIMES(733)=MPI_WTIMES(733)+MPI_TOC(S2TIME)
        S2TIME=MPI_TIC()
        IF(NXSP.GT.0)THEN
C        DO NSP=1,NXSP
!$OMP PARALLEL DO PRIVATE(CCLBTMP,CCMBTMP,EEB)
          DO L=LMPI2,LMPILA
            CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)
            CCMBTMP=1.-CCLBTMP
            EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))
            WQVX(L,K,1:NXSP)=(WQVX(L,K,1:NXSP)-CCLBTMP
     &                       *WQVX(L,K-1,1:NXSP))*EEB
          ENDDO
C        ENDDO
        ENDIF
        ENDIF
        MPI_WTIMES(734)=MPI_WTIMES(734)+MPI_TOC(S2TIME)
C
        IF(.FALSE.)THEN
        DO ND=1,NDM
          LF=2+(ND-1)*LDM
          LL=LF+LDM-1
          DO K=KC-1,1,-1
            DO L=LF,LL
              WQV(L,K, 1)=WQV(L,K, 1)-CU1(L,K)*WQV(L,K+1, 1)
              WQV(L,K, 2)=WQV(L,K, 2)-CU1(L,K)*WQV(L,K+1, 2)
              WQV(L,K, 3)=WQV(L,K, 3)-CU1(L,K)*WQV(L,K+1, 3)
              WQV(L,K, 4)=WQV(L,K, 4)-CU1(L,K)*WQV(L,K+1, 4)
              WQV(L,K, 5)=WQV(L,K, 5)-CU1(L,K)*WQV(L,K+1, 5)
              WQV(L,K, 6)=WQV(L,K, 6)-CU1(L,K)*WQV(L,K+1, 6)
              WQV(L,K, 7)=WQV(L,K, 7)-CU1(L,K)*WQV(L,K+1, 7)
              WQV(L,K, 8)=WQV(L,K, 8)-CU1(L,K)*WQV(L,K+1, 8)
              WQV(L,K, 9)=WQV(L,K, 9)-CU1(L,K)*WQV(L,K+1, 9)
              WQV(L,K,10)=WQV(L,K,10)-CU1(L,K)*WQV(L,K+1,10)
              WQV(L,K,11)=WQV(L,K,11)-CU1(L,K)*WQV(L,K+1,11)
              WQV(L,K,12)=WQV(L,K,12)-CU1(L,K)*WQV(L,K+1,12)
              WQV(L,K,13)=WQV(L,K,13)-CU1(L,K)*WQV(L,K+1,13)
              WQV(L,K,14)=WQV(L,K,14)-CU1(L,K)*WQV(L,K+1,14)
              WQV(L,K,15)=WQV(L,K,15)-CU1(L,K)*WQV(L,K+1,15)
              WQV(L,K,16)=WQV(L,K,16)-CU1(L,K)*WQV(L,K+1,16)
              WQV(L,K,17)=WQV(L,K,17)-CU1(L,K)*WQV(L,K+1,17)
              WQV(L,K,18)=WQV(L,K,18)-CU1(L,K)*WQV(L,K+1,18)
              WQV(L,K,19)=WQV(L,K,19)-CU1(L,K)*WQV(L,K+1,19)
              WQV(L,K,20)=WQV(L,K,20)-CU1(L,K)*WQV(L,K+1,20)
              WQV(L,K,21)=WQV(L,K,21)-CU1(L,K)*WQV(L,K+1,21)
            ENDDO
          ENDDO
          !{ GEOSR X-species : jgcho 2015.11.09
          if (NXSP.gt.0) then
            DO K=KC-1,1,-1
              DO L=LF,LL
                DO nsp=1,NXSP
                  WQVX(L,K,nsp)=WQVX(L,K,nsp)-CU1(L,K)*WQVX(L,K+1,nsp)
                ENDDO
              enddo
            enddo
          endif
          !} GEOSR X-species : jgcho 2015.11.09
        ENDDO
        ELSE
        S2TIME=MPI_TIC()
C        DO NW=1,NWQV
          DO K=KC-1,1,-1
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              WQV(L,K,1:NWQV)=WQV(L,K,1:NWQV)-CU1(L,K)*WQV(L,K+1,1:NWQV)
            ENDDO
          ENDDO
C        ENDDO
        MPI_WTIMES(735)=MPI_WTIMES(735)+MPI_TOC(S2TIME)
        S2TIME=MPI_TIC()
        IF(NXSP.GT.0)THEN
C        DO NSP=1,NXSP
          DO K=KC-1,1,-1
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              WQVX(L,K,1:NXSP)=WQVX(L,K,1:NXSP)-CU1(L,K)
     &                        *WQVX(L,K+1,1:NXSP)
            ENDDO
          ENDDO
C        ENDDO
        ENDIF
        MPI_WTIMES(736)=MPI_WTIMES(736)+MPI_TOC(S2TIME)
        ENDIF
      ENDIF
 2000 CONTINUE
      RETURN
      END

