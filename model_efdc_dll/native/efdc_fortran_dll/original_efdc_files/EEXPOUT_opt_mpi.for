      SUBROUTINE EEXPOUT_opt_mpi(JSEXPLORER)

      !----------------------------------------------------------------

      ! **  SUBROUTINE EEXPOUT WRITES UNFORMATTED OUTPUT FILES:
      ! **    EE_WC     - WATER COLUMN AND TOP LAYER OF SEDIMENTS
      ! **    EE_BED    - SEDIMENT BED LAYER INFORMATION
      ! **    EE_WQ     - WATER QUALITY INFORMATION FOR THE WATER COLUMN
      ! **    EE_SD     - SEDIMENT DIAGENSIS INFORMATION
      ! **    EE_ARRAYS - GENERAL/USER DEFINED ARRAY DUMP. LINKED TO
      ! **                EFDC_EXPLORER FOR DISPLAY
      ! **    EE_SEDZLJ - SEDIMENT BED DATA FOR SEDZLJ SUB-MODEL

      !----------------------------------------------------------------

      ! *** Notes:

      USE GLOBAL
      USE MPI

      INTEGER*4 VER
      CHARACTER*8 ARRAYNAME
      INTEGER*4 IWQ(40), NACTIVE
      INTEGER*4 JSEXPLORER,NS,NW,MW,NSEDSTEPS,NSXD
      INTEGER*4 L,K,ISYS,NT,NX,N1
      REAL*4  TMPVAL,WQ
      REAL*4  ZERO, SHEAR
c      REAL    SHEAR_1D(LCM),HBED_1D(LCM),BDENBED_1D(LCM),PORBED_1D(LCM)
c      REAL    SEDB_1D(LCM,NSED),SED_VFRBED_1D(LCM,NSED)
c      REAL    SNDB_1D(LCM,NSND),SND_VFRBED_1D(LCM,NSND)
c      INTEGER N1_1D(LCM)

      INTEGER NP1
      INTEGER COUNTCELL(LA)

      SAVE IWQ
      SAVE NSEDSTEPS

      IF(.NOT.ALLOCATED(SEDB_1D))THEN
         ALLOCATE(SEDB_1D(LCM,NSED))
         ALLOCATE(SED_VFRBED_1D(LCM,NSED))
         ALLOCATE(SNDB_1D(LCM,NSND))
         ALLOCATE(SND_VFRBED_1D(LCM,NSND))
         SEDB_1D      =0.
         SED_VFRBED_1D=0.
         SNDB_1D      =0.
         SND_VFRBED_1D=0.
      ENDIF

      IF(ISDYNSTP.EQ.0)THEN
        DELT=DT
      ELSE
        DELT=DTDYN
      ENDIF
      NACTIVE=LA-1

!{GEOSR, OIL, CWCHO, 101121
      S1TIME=MPI_TIC()
      IF (IDTOX.GE.4440) THEN
      ISTRAN(5)=1
      NTOX=1
       DO L=2,LA
       DO K=1,KC
          COUNTCELL(L)=0
          OILCONC=0.0
          DO NP1=1,NPD
          IF(L==LLA(NP1)) THEN
           COUNTCELL(L)=COUNTCELL(L)+1
          ENDIF
         ENDDO
         OILCONC(L,K,1)=OILMASS/REAL(NPD)*REAL(COUNTCELL(L))
         OILCONC(L,K,1)=OILCONC(L,K,1)/(DXP(L)*DYP(L)*HP(L))*1000. ! [mg/L]
         TOX(L,K,1)=OILCONC(L,K,1)
         ENDDO
         ENDDO
      ENDIF
      MPI_WTIMES(991)=MPI_WTIMES(992)+MPI_TOC(S1TIME)
!}
      IF(JSEXPLORER.eq.0)THEN
      IF(ISSPH(8).GE.1)THEN
         call collect_in_zero_int(KBT)
         call collect_in_zero(TAUBSED)
         call collect_in_zero(TAUBSND)
         call collect_in_zero(TAUB)
         DO K=0,KCM
            call collect_in_zero(QQ(:,K))
         ENDDO

         call collect_in_zero(RSSBCE)
         call collect_in_zero(RSSBCW)
         call collect_in_zero(RSSBCN)
         call collect_in_zero(RSSBCS)
         call collect_in_zero(TBX)
         call collect_in_zero(TBY)

         call collect_in_zero(WVWHA)
         call collect_in_zero(WVFRQL)
         call collect_in_zero(WACCWE)

         call collect_in_zero_array(SAL)
         call collect_in_zero_array(TEM)
         call collect_in_zero(TEMB)
         call collect_in_zero_array(DYE)
         call collect_in_zero_array(SFL)

         DO NT=1,NTXM
           call collect_in_zero_array_kbm(TOXB(:,:,NT))
           call collect_in_zero_array(TOX(:,:,NT))
         ENDDO

         call collect_in_zero(BELV)
         call collect_in_zero_array_kbm(HBED)
         call collect_in_zero_array_kbm(BDENBED)
         call collect_in_zero_array_kbm(PORBED)

         DO NS=1,NSED
           call collect_in_zero_array_kbm(SEDB(:,:,NS))
           call collect_in_zero_array_kbm(VFRBED(:,:,NS))
           call collect_in_zero_array(SED(:,:,NS))
         ENDDO

         DO NX=1,NSND
           call collect_in_zero_array_kbm(SNDB(:,:,NX))
           call collect_in_zero_array_kbm(VFRBED(:,:,NX+NSED))
           call collect_in_zero_array(SND(:,:,NX))
           call collect_in_zero(CQBEDLOADX(:,NX))
           call collect_in_zero(CQBEDLOADY(:,NX))
         ENDDO

      ENDIF

      IF(ISTRAN(6).GT.0.AND.NSEDFLUME.GT.0)THEN
        call collect_in_zero_r8(TAU)
        call collect_in_zero_r8(D50AVG)
        call collect_in_zero_r8(ETOTO)

        DO NT=1,NSCM
          call collect_in_zero_r8(CBL(1,:,NT))
          call collect_in_zero_r8(CBL(2,:,NT))
          call collect_in_zero_r8(XBLFLUX(:,NT))
          call collect_in_zero_r8(YBLFLUX(:,NT))
          DO K=1,KB
            call collect_in_zero_r8(PER(NT,K,:))
          ENDDO
        ENDDO
        DO K=1,KB
          call collect_in_zero_int(LAYER(K,:))
          call collect_in_zero_r8(TSED(K,:))
          call collect_in_zero_r8(BULKDENS(K,:))
        ENDDO
      ENDIF

      IF(ISBEXP.GE.1)THEN
        call collect_in_zero_int(KBT)
        call collect_in_zero_array_kbm(HBED)
        call collect_in_zero_array_kbm(BDENBED)
        call collect_in_zero_array_kbm(PORBED)

        DO NT=1,NTOX
          call collect_in_zero_array_kbm(TOXB(:,:,NT))
        ENDDO

        DO NS=1,NSED
          call collect_in_zero_array_kbm(SEDB(:,:,NS))
          call collect_in_zero_array_kbm(VFRBED(:,:,NS))
        ENDDO

        DO NX=1,NSND
          call collect_in_zero_array_kbm(SNDB(:,:,NX))
          call collect_in_zero_array_kbm(VFRBED(:,:,NX+NSED))
        ENDDO
      ENDIF

      IF(ISINWV.EQ.2)THEN
         call collect_in_zero_array(FXWAVE)
         call collect_in_zero_array(FYWAVE)

         call collect_in_zero(HP)
         call collect_in_zero_array(AH)
         call collect_in_zero_array(AV)
         DO K=0,KCM
          call collect_in_zero(QQ(:,K))
         ENDDO

         call collect_in_zero_array(FMDUX)
         call collect_in_zero_array(FMDUY)
         call collect_in_zero_array(FMDVY)
         call collect_in_zero_array(FMDVX)
         call collect_in_zero_array(U)
         call collect_in_zero_array(V)

         call collect_in_zero(UHDYE)
         call collect_in_zero(VHDXE)

         call collect_in_zero(FXE)
         call collect_in_zero(FYE)
         call collect_in_zero(DXIU)
         call collect_in_zero(DYIV)
         call collect_in_zero(AHC(:,1))
         call collect_in_zero(AHC(:,2))

         call collect_in_zero_array(AHU)
         call collect_in_zero_array(AMCU)
         call collect_in_zero_array(AMCV)
         call collect_in_zero_array(AMSU)
      ENDIF

      DO NW=0,NWQV
        call collect_in_zero_array(WQV(:,:,NW))
      ENDDO
      DO NSP=1,NXSP
        call collect_in_zero_array(WQVX(:,:,NSP))
      ENDDO

      IF(PRINT_SUM)THEN
      IF(MYRANK.EQ.0)THEN
         PRINT*,n,'TAUBSED ',sum(abs(dble(TAUBSED)))
         PRINT*,n,'TAUBSND ',sum(abs(dble(TAUBSND)))
         PRINT*,n,'TAUB ',sum(abs(dble(TAUB)))
         PRINT*,n,'RSSBCE ',sum(abs(dble(RSSBCE)))
         PRINT*,n,'RSSBCW ',sum(abs(dble(RSSBCW)))
         PRINT*,n,'RSSBCN ',sum(abs(dble(RSSBCN)))
         PRINT*,n,'RSSBCS ',sum(abs(dble(RSSBCS)))
         PRINT*,n,'TBX ',sum(abs(dble(TBX)))
         PRINT*,n,'TBY ',sum(abs(dble(TBY)))
         PRINT*,n,'WVWHA ',sum(abs(dble(WVWHA)))
         PRINT*,n,'WVFRQL ',sum(abs(dble(WVFRQL)))
         PRINT*,n,'WACCWE ',sum(abs(dble(WACCWE)))
         PRINT*,n,'SAL ',sum(abs(dble(SAL)))
         PRINT*,n,'TEM ',sum(abs(dble(TEM)))
         PRINT*,n,'TEMB ',sum(abs(dble(TEMB)))
         PRINT*,n,'DYE ',sum(abs(dble(DYE)))
         PRINT*,n,'SFL ',sum(abs(dble(SFL)))
         PRINT*,n,'TOXB ',sum(abs(dble(TOXB)))
         PRINT*,n,'TOX ',sum(abs(dble(TOX)))
         PRINT*,n,'HBED ',sum(abs(dble(HBED)))
         PRINT*,n,'BDENBED ',sum(abs(dble(BDENBED)))
         PRINT*,n,'PORBED ',sum(abs(dble(PORBED)))
         PRINT*,n,'KBT ',sum(abs(dble(KBT)))
         PRINT*,n,'SEDB ',sum(abs(dble(SEDB)))
         PRINT*,n,'VFRBED ',sum(abs(dble(VFRBED)))
         PRINT*,n,'SNDB ',sum(abs(dble(SNDB)))
         PRINT*,n,'VFRBED ',sum(abs(dble(VFRBED)))
         PRINT*,n,'CQBEDLOADX ',sum(abs(dble(CQBEDLOADX)))
         PRINT*,n,'CQBEDLOADY ',sum(abs(dble(CQBEDLOADY)))
         PRINT*,n,'WQV ',sum(abs(dble(WQV)))
         PRINT*,n,'WQVX ',sum(abs(dble(WQVX)))
      ENDIF
      ENDIF
      ENDIF

C **  INITIAL CALL
      S1TIME=MPI_TIC()
      IF(JSEXPLORER.EQ.1.AND.MYRANK.EQ.0)THEN
        OPEN(95,FILE='EE_WC.OUT',STATUS='UNKNOWN',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
        CLOSE(95,STATUS='DELETE')
        OPEN(95,FILE='EE_WC.OUT',STATUS='UNKNOWN',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
        VER=106
        WRITE(95)VER
        WRITE(95)ISTRAN(1),ISTRAN(2),ISTRAN(3),ISTRAN(4)
        WRITE(95)ISTRAN(5),ISTRAN(6),ISTRAN(7)
        WRITE(95)NSED,NSND,KB,KC,NTOX
        NSXD=NSED+NSND
        DO NS=1,NSXD
          WRITE(95)SEDDIA(NS)
        ENDDO
        CLOSE(95,STATUS='KEEP')

        IF(ISTRAN(6).GT.0.AND.NSEDFLUME.GT.0)THEN
          OPEN(95,FILE='EE_SEDZLJ.OUT',STATUS='UNKNOWN',
     &           ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
          CLOSE(95,STATUS='DELETE')
          OPEN(95,FILE='EE_SEDZLJ.OUT',STATUS='UNKNOWN',
     &           ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
          VER=100
          WRITE(95)VER
          WRITE(95)ITBM,NSICM
          CLOSE(95,STATUS='KEEP')
        ENDIF

        IF(ISBEXP.GE.1)THEN
          IF(ISTRAN(6).GE.1.OR.ISTRAN(7).GE.1)THEN
            OPEN(10,FILE='EE_BED.OUT',STATUS='UNKNOWN',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
            CLOSE(10,STATUS='DELETE')
            OPEN(10,FILE='EE_BED.OUT',STATUS='UNKNOWN',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
            VER=106
            WRITE(10)VER
            WRITE(10)ISTRAN(1),ISTRAN(2),ISTRAN(3),ISTRAN(4)
            WRITE(10)ISTRAN(5),ISTRAN(6),ISTRAN(7)
            WRITE(10)NSED,NSND,KB,KC,NTOX
            DO NS=1,NSXD
              WRITE(10)SEDDIA(NS)
            ENDDO
            CLOSE(10,STATUS='KEEP')
          ENDIF
        ENDIF

        IF(ISTRAN(8).GT.0)THEN
          OPEN(95,FILE='EE_WQ.OUT',STATUS='UNKNOWN',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
          CLOSE(95,STATUS='DELETE')
          OPEN(95,FILE='EE_WQ.OUT',STATUS='UNKNOWN',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
          VER=100
          WRITE(95)VER
          WRITE(95)NWQV
          WRITE(95)(ISTRWQ(NW),NW=1,NWQV)
          IWQ=0
          DO MW=1,NWQV
            IWQ(MW)=ISTRWQ(MW)
          ENDDO
          WRITE(95)(IWQ(NW),NW=1,NWQV)
          CLOSE(95,STATUS='KEEP')
!{ GEOSR X-species : jgcho 2015.10.14
                 if (NXSP.gt.0) then !{ GEOSR X-species : jgcho 2015.10.15
          OPEN(95,FILE='EE_WQX.OUT',STATUS='UNKNOWN',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
          CLOSE(95,STATUS='DELETE')
          OPEN(95,FILE='EE_WQX.OUT',STATUS='UNKNOWN',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
          VER=100
          WRITE(95)VER
          WRITE(95)NXSP,LA,KC
          CLOSE(95,STATUS='KEEP')
                 endif ! if (NXSP.gt.0) then !{ GEOSR X-species : jgcho 2015.10.15
!} GEOSR X-species : jgcho 2015.10.14

          ! *** SAVE SEDIMENT DIAGENESIS RESULTS
          IF(ISSDBIN.LT.0)THEN
            OPEN(95,FILE='EE_SD.OUT',STATUS='UNKNOWN',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
            CLOSE(95,STATUS='DELETE')
            OPEN(95,FILE='EE_SD.OUT',STATUS='UNKNOWN',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
            VER=100
            WRITE(95)VER
            WRITE(95)NACTIVE
            CLOSE(95,STATUS='KEEP')
            NSEDSTEPS=-1
          ENDIF
        ENDIF
      ELSEIF(JSEXPLORER.EQ.-1)THEN
        ! *** FORCE ALL OUTPUT
        NSEDSTEPS=32000
      ENDIF
      MPI_WTIMES(992)=MPI_WTIMES(992)+MPI_TOC(S1TIME)

C *** WRITE SNAPSHOT
      S1TIME=MPI_TIC()
      IF(ISDYNSTP.EQ.0)THEN
        EETIME=DT*FLOAT(N)+TCON*TBEGIN
      ELSE
        EETIME=TIMESEC
      ENDIF
      IF(JSEXPLORER.EQ.1)EETIME=TCON*TBEGIN
      EETIME=EETIME/86400.

      IF(ISSPH(8).GE.1.AND.MYRANK.EQ.0)THEN
        OPEN(95,FILE='EE_WC.OUT',STATUS='OLD',
     &         POSITION='APPEND',FORM='UNFORMATTED')
        WRITE(95)EETIME,NACTIVE
        IF(.FALSE.)THEN

        DO L=2,LA
          N1=KBT(L)
          IF(ISTRAN(6).GT.0.OR.ISTRAN(7).GT.0)THEN
            IF(ISBEDSTR.GE.1.AND.NSEDFLUME.EQ.0)THEN
              WRITE(95)TAUBSED(L)
              IF(ISBEDSTR.EQ.1)THEN
                WRITE(95)TAUBSND(L)
              ENDIF
            ELSE
              WRITE(95)TAUB(L)
            ENDIF
          ELSE
            SHEAR=MAX(QQ(L,0),QQMIN)/CTURB2
            WRITE(95)SHEAR
          ENDIF
          IF(ISWAVE.GE.1)THEN
            ! *** Shear due to Current Only
            SHEAR = (RSSBCE(L)*TBX(L+1   )+RSSBCW(L)*TBX(L))**2
     &             +(RSSBCN(L)*TBY(LNC(L))+RSSBCS(L)*TBY(L))**2
            SHEAR=0.5*SQRT(SHEAR)
            WRITE(95)SHEAR
            IF(ISWAVE.EQ.3)THEN
              WRITE(95)WVWHA(L),WVFRQL(L),WACCWE(L)
            ENDIF
          ENDIF
          IF(ISTRAN(1).EQ.1)WRITE(95)(SAL(L,K),K=1,KC)
          IF(ISTRAN(2).EQ.1)THEN
            WRITE(95)(TEM(L,K),K=1,KC)
            IF(TBEDIT.GT.0.)WRITE(95)TEMB(L)
          ENDIF
          IF(ISTRAN(3).EQ.1)WRITE(95,ERR=999,IOSTAT=ISYS)
     &                               (DYE(L,K),K=1,KC)
          IF(ISTRAN(4).EQ.1)WRITE(95)(SFL(L,K),K=1,KC)
          IF(ISTRAN(5).EQ.1)THEN
            WRITE(95)(TOXB(L,N1,NT),NT=1,NTOX)
            WRITE(95)((TOX(L,K,NT),K=1,KC),NT=1,NTOX)
          ENDIF
          IF(ISTRAN(6).EQ.1.OR.ISTRAN(7).GE.1)THEN
            WRITE(95)N1,BELV(L),HBED(L,N1),BDENBED(L,N1),PORBED(L,N1)
            IF(ISTRAN(6).EQ.1)THEN
              WRITE(95)(SEDB(L,N1,NS),VFRBED(L,N1,NS),NS=1,NSED)
              WRITE(95)((SED(L,K,NS),K=1,KC),NS=1,NSED)
            ENDIF
            IF(ISTRAN(7).EQ.1)THEN
              WRITE(95)(SNDB(L,N1,NX),VFRBED(L,N1,NX+NSED),NX=1,NSND)
              WRITE(95)((SND(L,K,NX),K=1,KC),NX=1,NSND)
              IF(ISBDLDBC.GT.0)THEN
                WRITE(95)(CQBEDLOADX(L,NX),CQBEDLOADY(L,NX),NX=1,NSND)
              ENDIF
            ENDIF
          ENDIF
        ENDDO

        ELSE

          IF(ISTRAN(6).GT.0.OR.ISTRAN(7).GT.0)THEN
            IF(ISBEDSTR.GE.1.AND.NSEDFLUME.EQ.0)THEN
              WRITE(95) TAUBSED
              IF(ISBEDSTR.EQ.1)THEN
                WRITE(95) TAUBSND
              ENDIF
            ELSE
              WRITE(95) TAUB
            ENDIF
          ENDIF

          IF(ISWAVE.GE.1)THEN
            DO L=2,LA
              SHEAR_1D(L) = (RSSBCE(L)*TBX(L+1   )+RSSBCW(L)*TBX(L))**2
     &                     +(RSSBCN(L)*TBY(LNC(L))+RSSBCS(L)*TBY(L))**2
              SHEAR_1D(L)=0.5*SQRT(SHEAR_1D(L))
            ENDDO
            WRITE(95) SHEAR_1D
            IF(ISWAVE.EQ.3)THEN
              WRITE(95) WVWHA
              WRITE(95) WVFRQL
              WRITE(95) WACCWE
            ENDIF
          ENDIF

          IF(ISTRAN(1).EQ.1) WRITE(95) SAL
          IF(ISTRAN(2).EQ.1)THEN
            WRITE(95) TEM
            IF(TBEDIT.GT.0.) WRITE(95) TEMB
          ENDIF

          IF(ISTRAN(3).EQ.1) WRITE(95) DYE
          IF(ISTRAN(4).EQ.1) WRITE(95) SFL
          IF(ISTRAN(5).EQ.1)THEN
            WRITE(95) TOXB
            WRITE(95) TOX
          ENDIF

          IF(ISTRAN(6).EQ.1.OR.ISTRAN(7).GE.1)THEN
            DO L=2,LA
               N1=KBT(L)
               N1_1D(L)=N1
               HBED_1D(L)=HBED(L,N1)
               BDENBED_1D(L)=BDENBED(L,N1)
               PORBED_1D(L)=PORBED(L,N1)
            ENDDO
            WRITE(95) N1_1D
            WRITE(95) BELV
            WRITE(95) HBED_1D
            WRITE(95) BDENBED_1D
            WRITE(95) PORBED_1D

            IF(ISTRAN(6).EQ.1)THEN
              DO NS=1,NSED
              DO L=2,LA
                N1=KBT(L)
                SEDB_1D(L,NS)=SEDB(L,N1,NS)
                SED_VFRBED_1D(L,NS)=VFRBED(L,N1,NS)
              ENDDO
              ENDDO
              WRITE(95) SEDB_1D
              WRITE(95) SED_VFRBED_1D
              WRITE(95) SED
            ENDIF

            IF(ISTRAN(7).EQ.1)THEN
              DO NX=1,NSND
                DO L=2,LA
                  N1=KBT(L)
                  SNDB_1D(L,NX)=SNDB(L,N1,NX)
                  SND_VFRBED_1D(L,NX)=VFRBED(L,N1,NX+NSED)
                ENDDO
              ENDDO
              WRITE(95) SNDB_1D
              WRITE(95) SND_VFRBED_1D
              WRITE(95) SND
              IF(ISBDLDBC.GT.0)THEN
                WRITE(95) CQBEDLOADX
                WRITE(95) CQBEDLOADY
              ENDIF
            ENDIF

          ENDIF

        ENDIF

        CALL FLUSH(95)
        CLOSE(95,STATUS='KEEP')
      ENDIF
      MPI_WTIMES(993)=MPI_WTIMES(993)+MPI_TOC(S1TIME)

      ! *** OUTPUT THE SEDZLJ VARIABLES
      S1TIME=MPI_TIC()
      IF(ISTRAN(6).GT.0.AND.NSEDFLUME.GT.0.AND.MYRANK.EQ.0)THEN
        OPEN(95,FILE='EE_SEDZLJ.OUT',STATUS='OLD',
     &         POSITION='APPEND',FORM='UNFORMATTED')

        WRITE(95)EETIME,NACTIVE

        DO L=2,LA
          WRITE(95) REAL(TAU(L))            !TAU(LCM)      - Shear Stress in dynes/cm^2
          WRITE(95) REAL(D50AVG(L))         !D50AVG(LCM)   - Average particle size of bed surface (microns)
          WRITE(95) REAL(ETOTO(L))          !ETOTO(LCM)    - Total erosion in the cell
          DO NT=1,NSCM
            WRITE(95) REAL(CBL(1,L,NT))     !CBL(NSCM,LCM) - This is the bedload concentration in g/cm^3 of each size class
            WRITE(95) REAL(XBLFLUX(L,NT))   !XBLFLUX(LCM,NSCM) - Bedload flux in X direction (g/s)
            WRITE(95) REAL(YBLFLUX(L,NT))   !YBLFLUX(LCM,NSCM) - Bedload flux in Y direction (g/s)
            DO K=1,KB
              WRITE(95) REAL(PER(NT,K,L))   !PER(NSCM,KB,LCM) - This is the mass percentage of each size class in a layer
            ENDDO
          ENDDO
          DO K=1,KB
            WRITE(95) LAYER(K,L)            !LAYER(KB,LCM) - This is = 1 when a bed layer (KB index) exists with mass
            WRITE(95) REAL(TSED(K,L))       !TSED(KB,LCM)  - This is the mass in g/cm^2 in each layer
            WRITE(95) REAL(BULKDENS(K,L))   !BULKDENS(KB,LCM) - Dry Bulk density of each layer (g/cm^3)
          ENDDO
        ENDDO

        IF(MYRANK.EQ.0) CALL FLUSH(95)
        IF(MYRANK.EQ.0) CLOSE(95,STATUS='KEEP')
      ENDIF
      MPI_WTIMES(994)=MPI_WTIMES(994)+MPI_TOC(S1TIME)

C *** NOW OUTPUT ALL THE BEDINFO TO A SINGLE FILE
      S1TIME=MPI_TIC()
      IF(ISBEXP.GE.1.AND.MYRANK.EQ.0)THEN
        IF(ISTRAN(6).GE.1.OR.ISTRAN(7).GE.1.AND.KB.GT.1)THEN
          OPEN(87,FILE='EE_BED.OUT',STATUS='UNKNOWN',POSITION='APPEND'
     &           ,FORM='UNFORMATTED')
          WRITE(87)EETIME,NACTIVE
          DO L=2,LA
            WRITE(87)KBT(L)
          ENDDO
          DO L=2,LA
            DO K=1,KB
              WRITE(87)HBED(L,K),BDENBED(L,K),PORBED(L,K)
              IF(ISTRAN(6).GE.1)THEN
                DO NS=1,NSED
                  WRITE(87)SEDB(L,K,NS),VFRBED(L,K,NS)
                ENDDO
              ENDIF
              IF(ISTRAN(7).GE.1)THEN
                DO NX=1,NSND
                  NS=NSED+NX
                  WRITE(87)SNDB(L,K,NX),VFRBED(L,K,NS)
                ENDDO
              ENDIF
              IF(ISTRAN(5).GE.1)THEN
                DO NT=1,NTOX
                  WRITE(87)TOXB(L,K,NT)
                ENDDO
              ENDIF
            ENDDO
          ENDDO
          CALL FLUSH(87)
          CLOSE(87,STATUS='KEEP')
        ENDIF
      ENDIF
      MPI_WTIMES(995)=MPI_WTIMES(995)+MPI_TOC(S1TIME)

C *** INTERNAL ARRAYS
      S1TIME=MPI_TIC()
      IF(ISINWV.EQ.2.AND.JSEXPLORER.LE.0.AND.MYRANK.EQ.0)THEN
        ZERO=0.0
        IF(N.LT.(2*NTSPTC/NPSPH(8)))THEN
          OPEN(95,FILE='EE_ARRAYS.OUT',STATUS='UNKNOWN')
            CLOSE(95,STATUS='DELETE')
          OPEN(95,FILE='EE_ARRAYS.OUT',STATUS='UNKNOWN',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
          VER=100
          WRITE(95)VER
          WRITE(95)3    ! # OF TIME VARYING ARRAYS

          ! FLAGS: ARRAY TYPE, TIME VARIABLE
          ! ARRAY TYPE:    0 = L            DIM'D
          !                1 = L,KC         DIM'D
          !                2 = L,0:KC       DIM'D
          !                3 = L,KB         DIM'D
          !                4 = L,KC,NCLASS  DIM'D
          ! TIME VARIABLE: 0 = NOT CHANGING
          !                1 = TIME VARYING

          !WRITE(95)0,0
          !ARRAYNAME='SUB'
          !WRITE(95)ARRAYNAME
          !DO L=2,LA
          !  WRITE(95)SUB(L)
          !ENDDO

          !WRITE(95)0,0
          !ARRAYNAME='SVB'
          !WRITE(95)ARRAYNAME
          !DO L=2,LA
          !  WRITE(95)SVB(L)
          !ENDDO

          WRITE(95)1,0
          ARRAYNAME='FXWAVE'
          WRITE(95)ARRAYNAME
          DO L=2,LA
            DO K=1,KC
              WRITE(95)FXWAVE(L,K)
            ENDDO
          ENDDO

          WRITE(95)1,0
          ARRAYNAME='FYWAVE'
          WRITE(95)ARRAYNAME
          DO L=2,LA
            DO K=1,KC
              WRITE(95)FYWAVE(L,K)
            ENDDO
          ENDDO

        ELSE
          OPEN(95,FILE='EE_ARRAYS.OUT',STATUS='UNKNOWN',
     &            POSITION='APPEND',FORM='UNFORMATTED')
        ENDIF

        IF(.TRUE.)THEN

          WRITE(95)1,1
          ARRAYNAME='AH'
          WRITE(95)ARRAYNAME
          DO L=2,LA
            DO K=1,KC
              WRITE(95)AH(L,K)
            ENDDO
          ENDDO

          WRITE(95)1,1
          ARRAYNAME='AV'
          WRITE(95)ARRAYNAME
          DO L=2,LA
            DO K=1,KC
              WRITE(95)(AV(L,K)*HP(L))
            ENDDO
          ENDDO

          WRITE(95)2,1
          ARRAYNAME='QQ'
          WRITE(95)ARRAYNAME
          DO L=2,LA
            DO K=0,KC
              WRITE(95)QQ(L,K)
            ENDDO
          ENDDO

          IF(.FALSE.)THEN
            ! *** FMDUX FMDUY FMDVY FMDVX
            WRITE(95)1,1
            ARRAYNAME='FMDUX'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              DO K=1,KC
                WRITE(95)FMDUX(L,K)
              ENDDO
            ENDDO

            WRITE(95)1,1
            ARRAYNAME='FMDUY'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              DO K=1,KC
                WRITE(95)FMDUY(L,K)
              ENDDO
            ENDDO
            WRITE(95)1,1
            ARRAYNAME='FMDVY'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              DO K=1,KC
                WRITE(95)FMDVY(L,K)
              ENDDO
            ENDDO
            WRITE(95)1,1
            ARRAYNAME='FMDVX'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              DO K=1,KC
                WRITE(95)FMDVX(L,K)
              ENDDO
            ENDDO

            WRITE(95)1,1
            ARRAYNAME='U'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              DO K=1,KC
                WRITE(95)U(L,K)
              ENDDO
            ENDDO
            WRITE(95)1,1
            ARRAYNAME='V'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              DO K=1,KC
                WRITE(95)V(L,K)
              ENDDO
            ENDDO

            WRITE(95)0,1
            ARRAYNAME='UHDYE'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              WRITE(95)UHDYE(L)
            ENDDO

            WRITE(95)0,1
            ARRAYNAME='VHDXE'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              WRITE(95)VHDXE(L)
            ENDDO

            WRITE(95)0,1
            ARRAYNAME='FXE'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              TMPVAL=FXE(L)*DELT*DXIU(L)
              WRITE(95)TMPVAL
            ENDDO

            WRITE(95)0,1
            ARRAYNAME='FYE'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              TMPVAL=FYE(L)*DELT*DYIV(L)
              WRITE(95)TMPVAL
            ENDDO

            WRITE(95)0,1
            ARRAYNAME='FUHX'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              TMPVAL=AHC(L,1)*DELT*DXIU(L)
              WRITE(95)TMPVAL
            ENDDO

            WRITE(95)0,1
            ARRAYNAME='FVHY'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              TMPVAL=AHC(L,2)*DELT*DYIV(L)
              WRITE(95)TMPVAL
            ENDDO

            WRITE(95)1,1
            ARRAYNAME='FUHU'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              DO K=1,KC
                WRITE(95)AHU(L,K)
              ENDDO
            ENDDO
            WRITE(95)1,1
            ARRAYNAME='FVHU'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              DO K=1,KC
                WRITE(95)AMCU(L,K)
              ENDDO
            ENDDO
            WRITE(95)1,1
            ARRAYNAME='FVHV'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              DO K=1,KC
                WRITE(95)AMCV(L,K)
              ENDDO
            ENDDO
            WRITE(95)1,1
            ARRAYNAME='FUHV'
            WRITE(95)ARRAYNAME
            DO L=2,LA
              DO K=1,KC
                WRITE(95)AMSU(L,K)
              ENDDO
            ENDDO

          ENDIF

        !WRITE(95)0,1
        !ARRAYNAME='TATMT'
        !WRITE(95)ARRAYNAME
        !DO L=2,LA
        !  WRITE(95)TATMT(L)
        !ENDDO
        ENDIF
C
        CALL FLUSH(95)
        CLOSE(95,STATUS='KEEP')

      ENDIF
      MPI_WTIMES(996)=MPI_WTIMES(996)+MPI_TOC(S1TIME)

C *** WATER QUALITY
      IF(ISTRAN(8).GT.0.AND.MYRANK.EQ.0)THEN
        !  1) CHC - cyanobacteria
        !  2) CHD - diatom algae
        !  3) CHG - green algae
        !  4) ROC - refractory particulate organic carbon
        !  5) LOC - labile particulate organic carbon
        !  6) DOC - dissolved organic carbon
        !  7) ROP - refractory particulate organic phosphorus
        !  8) LOP - labile particulate organic phosphorus
        !  9) DOP - dissolved organic phosphorus
        ! 10) P4D - total phosphate
        ! 11) RON - refractory particulate organic nitrogen 22) macroalgae
        ! 12) LON - labile particulate organic nitrogen
        ! 13) DON - dissolved organic nitrogen
        ! 14) NHX - ammonia nitrogen
        ! 15) NOX - nitrate nitrogen
        ! 16) SUU - particulate biogenic silica
        ! 17) SAA - dissolved available silica
        ! 18) COD - chemical oxygen demand
        ! 19) DOX - dissolved oxygen
        ! 20) TAM - total active metal
        ! 21) FCB - fecal coliform bacteria
        S1TIME=MPI_TIC()
        OPEN(95,FILE='EE_WQ.OUT',STATUS='UNKNOWN',POSITION='APPEND',
     &          FORM='UNFORMATTED')
        WRITE(95)EETIME
        IF(.FALSE.)THEN
          DO L=2,LA
            DO K=1,KC
              DO NW=1,NWQV
                IF(IWQ(NW).GT.0)THEN
                  WQ=WQV(L,K,NW)
                  WRITE(95)WQ
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ELSE
          DO NW=1,NWQV
            IF(IWQ(NW).GT.0)THEN
              WRITE(95) WQV(:,:,NW)
            ENDIF
         ENDDO
        ENDIF
        CALL FLUSH(95)
        CLOSE(95,STATUS='KEEP')
        MPI_WTIMES(997)=MPI_WTIMES(997)+MPI_TOC(S1TIME)
!{ GEOSR X-species : jgcho 2015.10.14
        S1TIME=MPI_TIC()
        if (NXSP.gt.0) then !{ GEOSR X-species : jgcho 2015.10.15
        OPEN(95,FILE='EE_WQX.OUT',STATUS='UNKNOWN',POSITION='APPEND',
     &          FORM='UNFORMATTED')
        WRITE(95)EETIME,N
        IF(.FALSE.)THEN
        DO NSP=1,NXSP
          DO K=1,KC
            DO L=2,LA
              WQ=WQVX(L,K,NSP)
              WRITE(95)WQ
            ENDDO
          ENDDO
        ENDDO
        ELSE
        DO NSP=1,NXSP
          WRITE(95) WQVX(:,:,NSP)
        ENDDO
        ENDIF
        CALL FLUSH(95)
        CLOSE(95,STATUS='KEEP')
        endif !if (NXSP.gt.0) then !{ GEOSR X-species : jgcho 2015.10.15
        MPI_WTIMES(998)=MPI_WTIMES(998)+MPI_TOC(S1TIME)
!} GEOSR X-species : jgcho 2015.09.18
        ! *** SAVE SEDIMENT DIAGENESIS RESULTS
        S1TIME=MPI_TIC()
        IF(IWQBEN.GT.0.AND.ISSDBIN.LT.0)THEN
          ! *** IF JSEXPLORER=1 THEN WRITE THE ARRAYS (I.E. IC'S)
          NSEDSTEPS=NSEDSTEPS+1
          IF(NSEDSTEPS.GE.ABS(ISSDBIN).OR.JSEXPLORER.EQ.1)THEN
            OPEN(95,FILE='EE_SD.OUT',STATUS='UNKNOWN',POSITION='APPEND',
     &           FORM='UNFORMATTED')
            WRITE(95)EETIME
            DO L=2,LA

              !   SMPON = Conc. Particulate Org. Nitrogen   in G-class 1, 2 & 3  (g/m3) dim(LA,NSMGM)
              !   SMPOP = Conc. Particulate Org. Phosphorus in G-class 1, 2 & 3  (g/m3) dim(LA,NSMGM)
              !   SMPOC = Conc. Particulate Org. Carbon     in G-class 1, 2 & 3  (g/m3) dim(LA,NSMGM)

              ! *** DEPOSITION FLUXES
              ! SMDFN(LL,?) = Sediment Flux To The Sediment Bed From PON Into G1, G2, & G3
              ! SMDFP(LL,?) = Sediment Flux To The Sediment Bed From POP Into G1, G2, & G3
              ! SMDFC(LL,?) = Sediment Flux To The Sediment Bed From POC Into G1, G2, & G3

              !  SM1NH4 = Conc. NH4-N in layer 1 (g/m3)  dim(LA)
              !  SM2NH4 = Conc. NH4-N in layer 2 (g/m3)
              !  SM1NO3 = Conc. NO3-N in layer 1 (g/m3)
              !  SM2NO3 = Conc. NO3-N in layer 2 (g/m3)
              !  SM1PO4 = Conc. PO4-P in layer 1 (g/m3)
              !  SM2PO4 = Conc. PO4-P in layer 2 (g/m3)
              !  SM1H2S = Conc. Sulfide (H2S) in layer 1 (g/m3)
              !  SM2H2S = Conc. Sulfide (H2S) in layer 2 (g/m3)
              !   SMPSI = Conc. Particulate biogenic silica in layer 2 (g/m3)
              !   SM1SI = Conc. Dissolved available silica in layer 1 (g/m3)
              !   SM2SI = Conc. Dissolved available silica in layer 2 (g/m3)
              !   SMBST = Accumulated benthic stress (days)
              !     SMT = Sediment temperature (degC)

              ! *** SEDIMENT OXYGEN DEMANDS
              !  SMCSOD = CARBONACEOUS SOD
              !  SMNSOD = NITROGENOUS SOD

              ! *** BENTHIC FLUXES
              !  WQBFNH4 = AMMONIUM FLUX
              !  WQBFNO3 = NITRATE FLUX
              !   WQBFO2 = O2 SEDIMENT FLUX (SOD)
              !  WQBFCOD = COD FLUX
              ! WQBFPO4D = PO4 FLUX
              !  WQBFSAD = SILICA FLUX

              WRITE(95)(SMPON(L,K),K=1,3)
              WRITE(95)(SMPOP(L,K),K=1,3)
              WRITE(95)(SMPOC(L,K),K=1,3)
              WRITE(95)(SMDFN(L,K),K=1,3)
              WRITE(95)(SMDFP(L,K),K=1,3)
              WRITE(95)(SMDFC(L,K),K=1,3)
              WRITE(95)SM1NH4(L),SM2NH4(L)
              WRITE(95)SM1NO3(L),SM2NO3(L)
              WRITE(95)SM1PO4(L),SM2PO4(L)
              WRITE(95)SM1H2S(L),SM2H2S(L)
              WRITE(95)SM1SI(L), SM2SI(L)
              WRITE(95)SMPSI(L)
              WRITE(95)SMBST(L),SMT(L)
              WRITE(95)SMCSOD(L),SMNSOD(L)
              WRITE(95)WQBFNH4(L),WQBFNO3(L),WQBFO2(L),WQBFCOD(L),
     &               WQBFPO4D(L),WQBFSAD(L)
            ENDDO
            CALL FLUSH(95)
            CLOSE(95,STATUS='KEEP')
            NSEDSTEPS=0
          ENDIF
        ENDIF
      ENDIF
      MPI_WTIMES(999)=MPI_WTIMES(999)+MPI_TOC(S1TIME)

      RETURN

  999 STOP ' Error writing SNAPSHOT file'
      END
