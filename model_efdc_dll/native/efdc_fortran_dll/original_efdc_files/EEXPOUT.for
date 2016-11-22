      SUBROUTINE EEXPOUT(JSEXPLORER)

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

      INTEGER*4 VER
      CHARACTER*8 ARRAYNAME
      INTEGER*4 IWQ(40), NACTIVE
      INTEGER*4 JSEXPLORER,NS,NW,MW,NSEDSTEPS,NSXD
      INTEGER*4 L,K,ISYS,NT,NX,N1
      REAL*4  TMPVAL,WQ
      REAL*4  ZERO, SHEAR
      
	INTEGER NP1
	INTEGER COUNTCELL(LA)

      SAVE IWQ
      SAVE NSEDSTEPS

      IF(ISDYNSTP.EQ.0)THEN  
        DELT=DT  
      ELSE  
        DELT=DTDYN  
      ENDIF  
      NACTIVE=LA-1

!{GEOSR, OIL, CWCHO, 101121
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
!}


C **  INITIAL CALL
      IF(JSEXPLORER.EQ.1)THEN
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

C *** WRITE SNAPSHOT
      IF(ISDYNSTP.EQ.0)THEN
        EETIME=DT*FLOAT(N)+TCON*TBEGIN
      ELSE
        EETIME=TIMESEC
      ENDIF
      IF(JSEXPLORER.EQ.1)EETIME=TCON*TBEGIN
      EETIME=EETIME/86400.

      IF(ISSPH(8).GE.1)THEN
        OPEN(95,FILE='EE_WC.OUT',STATUS='OLD',
     &         POSITION='APPEND',FORM='UNFORMATTED')
        WRITE(95)EETIME,NACTIVE
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
        CALL FLUSH(95)
        CLOSE(95,STATUS='KEEP')
      ENDIF

      ! *** OUTPUT THE SEDZLJ VARIABLES
      IF(ISTRAN(6).GT.0.AND.NSEDFLUME.GT.0)THEN
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
                
        CALL FLUSH(95)
        CLOSE(95,STATUS='KEEP')
      ENDIF
      
C *** NOW OUTPUT ALL THE BEDINFO TO A SINGLE FILE
      IF(ISBEXP.GE.1)THEN
        IF(ISTRAN(6).GE.1.OR.ISTRAN(7).GE.1.AND.KB.GT.1)THEN
          OPEN(10,FILE='EE_BED.OUT',STATUS='UNKNOWN',POSITION='APPEND'
     &           ,FORM='UNFORMATTED')
          WRITE(10)EETIME,NACTIVE
          DO L=2,LA
            WRITE(10)KBT(L)
          ENDDO
          DO L=2,LA
            DO K=1,KB   
              WRITE(10)HBED(L,K),BDENBED(L,K),PORBED(L,K)
              IF(ISTRAN(6).GE.1)THEN
                DO NS=1,NSED
                  WRITE(10)SEDB(L,K,NS),VFRBED(L,K,NS)
                ENDDO
              ENDIF
              IF(ISTRAN(7).GE.1)THEN
                DO NX=1,NSND
                  NS=NSED+NX
                  WRITE(10)SNDB(L,K,NX),VFRBED(L,K,NS)
                ENDDO
              ENDIF
              IF(ISTRAN(5).GE.1)THEN
                DO NT=1,NTOX
                  WRITE(10)TOXB(L,K,NT)
                ENDDO
              ENDIF
            ENDDO
          ENDDO
          CALL FLUSH(10)
          CLOSE(10,STATUS='KEEP')
        ENDIF
      ENDIF

C *** INTERNAL ARRAYS
      IF(ISINWV.EQ.2.AND.JSEXPLORER.LE.0)THEN
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

C *** WATER QUALITY
      IF(ISTRAN(8).GT.0)THEN
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
        OPEN(95,FILE='EE_WQ.OUT',STATUS='UNKNOWN',POSITION='APPEND',
     &          FORM='UNFORMATTED')
        WRITE(95)EETIME
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
        CALL FLUSH(95)
        CLOSE(95,STATUS='KEEP')

!{ GEOSR X-species : jgcho 2015.10.14
                 if (NXSP.gt.0) then !{ GEOSR X-species : jgcho 2015.10.15
      OPEN(95,FILE='EE_WQX.OUT',STATUS='UNKNOWN',POSITION='APPEND',
     &          FORM='UNFORMATTED')
        WRITE(95)EETIME,N
        do nsp=1,NXSP
          do K=1,KC
            do L=2,LA
              WQ=WQVX(L,K,nsp)
              WRITE(95)WQ
            ENDDO
          ENDDO
        ENDDO
        CALL FLUSH(95)
        CLOSE(95,STATUS='KEEP')
                 endif !if (NXSP.gt.0) then !{ GEOSR X-species : jgcho 2015.10.15
!} GEOSR X-species : jgcho 2015.09.18
        ! *** SAVE SEDIMENT DIAGENESIS RESULTS
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

      RETURN

  999 STOP ' Error writing SNAPSHOT file'
      END
