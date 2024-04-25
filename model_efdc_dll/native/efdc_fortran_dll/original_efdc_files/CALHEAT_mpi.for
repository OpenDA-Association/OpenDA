      SUBROUTINE CALHEAT_mpi(ISTL_)
C
C   Subroutine CALHEAT takes the information from the atmospheric boundary
C   file and the wind forcing file and calculates the net heat flux across
C   the water surface boundary.  The heat flux is then used to update the
C   water temperature either in the surface cells, or distributed across
C   the cells in the vertical and into the bottom.  The subroutine has
C   three options these are:
C
C    ISOPT(2)=1: Full surface and internal heat transfer calculation
C                using meteorologic data from input stream.
C                IASWRAD=0:  ADSORB SW SOLR RAD TO ALL LAYERS AND BED
C                IASWRAD=1:  ADSORB SW SOLR RAD TO TO SURFACE LAYER
C    ISOPT(2)=2: Transient equilibrium surface heat transfer calculation
C                using external equilibrium temperature and heat transfer
C                coefficient data from the meteorologic input data.
C    ISOPT(2)=3: Equilibrium surface heat transfer calculation using constant
C                equilibrium temperature and heat transfer coefficients
C                HEQT and TEMO read in through input stream.
C    ISOPT(2)=4: Equilibrium surface heat transfer calculation using algorithm
C                from CE-QUAL-W2.
C
C   The heat flux terms are derived from a paper by Rosati
C   and Miyakoda (1988) entitled "A General Circulation Model for Upper Ocean
C   Simulation".  The heat flux is prescribed by term for the following
C   influxes and outfluxes:
C
C     - Short Wave Incoming Radiation (+)
C     - Net Long Wave Radiation (+/-)
C     - Sensible Heat Flux (convection -)
C     - Latent Heat Flux (evaporation +/-)
C
C   Two formulations of the Latent Heat Flux are provided.  The first is from
C   the Rosati and Miyakoda paper, the second is an alternate formulation by
C   Brady, Graves, and Geyer (1969).  The second formulation was taken from
C   "Hydrodynamics and Transport for Water Quality Modeling" (Martin and
C   McCutcheon, 1999).  The Rosati and Miyakoda formulation will have zero
C   evaporative cooling or heating if wind speed goes to zero.  The Brady,
C   Graves, and Geyer formulation provides for a minimum evaporative cooling
C   under zero wind speed.
C
C
C VARIABLE LIST:
C
C   CLOUDT  = Cloud cover (0 to 10)
C   HCON    = Sensible heat flux (W/m2)
C   HLAT    = Latent heat flux (W/m2)
C   HLWBR   = Net longwave radiation (atmospheric long wave plus back
C             radiation, W/m2)
C   SOLSWRT = Short wave incoming radiation (W/m2)
C   SVPW    = Saturation vapor pressure in mb based upon the water surface
C             temperature
C   TATMT   = Temperature of air above water surface (deg C)
C   TEM     = Water temperature in cell (deg C)
C   VPA     = Vapor pressure of air at near surface air temperature (mb)
C   WINDST  = Wind speed at 10 meters over cell surface (m/s)
C
C MODIFICATION HISTORY:
C
C   Date       Author             Comments
C   ---------- ------------------ ---------------------------------------------
C   06/01/1992 John M. Hamrick    Orignial author
C   11/07/2000 Steven Peene       Cleaned code, provided more detailed
C                                 descriptions, added alternate formulation
C                                 for latent heat flux, separated out
C                                 individual heat flux terms
C   11/01/2005 Paul M. Craig      Added Option 4, to use the Equilibrium Temperature
C                                 algorithym from CE-QUAL-W2.  Also added the sub-option
C                                 under this option to couple or decouple the bottom temperature
C                                 to the water column temperatures.
C                                 Added the ability to input spatially variable bed temps and
C                                 thermally active bed thicknesses.
C                                 Also cleaned up the code and added structure.
C
C CHANGE RECORD
C **  SUBROUTINE CALHEAT CALCULATES SURFACE AND INTERNAL HEAT SOURCES
C **  AND SINKS IN THE HEAT (TEM) TRANSPORT EQUATION
C
      USE GLOBAL
      USE MPI
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::NETRAD
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TBEDTHK
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::HDEP
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RADBOT
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::FLUXTB
      !REAL,SAVE ::    PTIME
      !REAL,SAVE ::    PMCTOL
      REAL            K_ABOVE
      INTEGER                           ::NRANK
      REAL TSSS_ABOVE
      REAL WQCHLS_ABOVE
      REAL POMS_ABOVE
      REAL CSHE
      TSSS_ABOVE=0.0
      WQCHLS_ABOVE=0.0
      POMS_ABOVE=0.0
      CSHE=0.0
C
      IF(.NOT.ALLOCATED(NETRAD))THEN
        ALLOCATE(NETRAD(LCM,KCM))
        ALLOCATE(TBEDTHK(LCM))
        ALLOCATE(HDEP(LCM))
        ALLOCATE(RADBOT(LCM))
        ALLOCATE(FLUXTB(LCM))
          RADBOT=0.0 !SCJ
          FLUXTB=0.0 !SCJ

        ! *** Ininitialze Heat Exchange Terms
        IF(MYRANK.EQ.0) PRINT *,'CALHEAT: INITIALIZING'
        CALL HEAT_EXCHANGE
        !PMCTOL=0.1
        NETRAD=0.
        HDEP=0.
        TBEDTHK=0.
        IF((ISTOPT(2).EQ.1.AND.IASWRAD.EQ.0).OR.ISTOPT(2).EQ.4)THEN
          IF(DABEDT.GT.0.)THEN
            IF(MYRANK.EQ.0)
     &      PRINT *,'CALHEAT: SETTING CONSTANT THICKNESS TO:',DABEDT
            DO L=2,LA
              TBEDTHK(L)=DABEDT
            ENDDO
          ELSE
            ! *** READ IN THE SPATIALLY VARYING INIT T AND BED THICKNESS (DABEDT)
            IF(MYRANK.EQ.0)
     &      PRINT *,'CALHEAT: READ IN THE SPATIALLY VARYING INIT T AND
     & BED THICKNESS: TEMB.INP'
            DO L=2,LA
              TBEDTHK(L)=ABS(DABEDT)
              IF(ISCI(2).EQ.0)TEMB(L)=ABS(TBEDIT)
            ENDDO
            OPEN(1001,FILE='TEMB.INP',ERR=1000,STATUS='OLD')
            DO IS=1,4
              READ(1001,*)
            ENDDO
            DO L1 = 2, LA
              READ(1001,*,END=1000) I,J,T1,T2
              L=LIJ(I,J)
              IF(ISCI(2).EQ.0)TEMB(L)=T1
              TBEDTHK(L)=T2
            ENDDO
 1000       CLOSE(1001)
          ENDIF
        ENDIF
        IF(PRINT_SUM)THEN
        call collect_in_zero_array(TEM)
        IF(MYRANK.EQ.0)THEN
          PRINT*, n,'1HEAT  = ', sum(abs(dble(TEM)))
        ENDIF
        ENDIF
!{GeoSR, YSSONG, ICE COVER, 1111031
C        IF(ISTOPT(2).EQ.1.OR.ISTOPT(2).EQ.4)THEN
C          DO L=2,LA
C            PSHADE(L)=1.0
C          ENDDO
C          IF(USESHADE)THEN
C            ! *** READ IN THE SPATIALLY VARYING INIT T AND BED THICKNESS (DABEDT)
C            PRINT *,
C     *        'CALHEAT: READ IN SPATIALLY VARYING SHADE: PSHADE.INP'
C            OPEN(1001,FILE='PSHADE.INP',ERR=1010,STATUS='OLD')
C            DO IS=1,4
C              READ(1001,*)
C            ENDDO
C            DO L1 = 2, LA
C              READ(1001,*,END=1010) I,J,T1
C              L=LIJ(I,J)
C              PSHADE(L)=T1
C            ENDDO
C 1010       CLOSE(1001)
C          ELSE
C            PRINT *,'CALHEAT: SETTING CONSTANT SHADE TO: 1.0 (NO SHADE)'
C          ENDIF
C        ENDIF
!}

        IF(DEBUG.AND.MYRANK.EQ.0)THEN
          PRINT *,'CALHEAT: Bed Temp(L=2):', TEMB(2)
          OPEN(77,file='calheat.dia',status='unknown')
          CLOSE(77,status='DELETE')
          OPEN(77,file='calheat.dia',status='NEW')
          WRITE(77,998)'TIMEDAY','SRON','ET','TD_C','TA_C','TDEW_F',
     &                  'TAIR_F','FW'
  998     FORMAT(A11,8A9)
        ENDIF
      ENDIF

!{GeoSR, YSSONG, ICE COVER, 1111031
      S2TIME=MPI_TIC()
      IF(USESHADE)THEN
        IF(ISDYNSTP.EQ.0)THEN
          TIMTMP=(DT*FLOAT(N)+TCON*TBEGIN)/86400.
        ELSE
          TIMTMP=TIMESEC/86400.
        ENDIF

        IF(TIMTMP .GE. SHDDAY)THEN
          IF(ISTOPT(2).EQ.1.OR.ISTOPT(2).EQ.4)THEN
            DO L=2,LA
              PSHADE(L)=1.0
            ENDDO

            IF(N.EQ.1)THEN
              ! *** READ IN THE SPATIALLY VARYING INIT T (READ IN SPATIALLY VARYING SHADE: PSHADE.INP')
              OPEN(1001,FILE='PSHADE.INP',STATUS='UNKNOWN')
C
C SKIP OVER ALL COMMENT CARDS AT BEGINNING OF FILE:
C
              DO NDUM=1,3
                READ(1001,*)
              ENDDO
C
C SEQUENTIALLY READ ICE COVER FILE UNTIL THE APPROPRIATE
            ENDIF

C TIME IS FOUND:
C   SHDAY   = CURRENT DAY AT WHICH ICE COVER IS IN EFFECT
C   SHDDAY = NEXT DAY AT WHICH ICE COVER CHANGES (PASSED TO MAIN PROG
C
   10       READ(1001, *, END=15) SHDDAY,NDATASHD
            IF(SHDDAY .GT. TIMTMP) GOTO 20
            SHDAY = SHDDAY
            DO NDUM=1,NDATASHD
              READ(1001,*,END=15) I,J,PSHADE0
              L=LIJ(I,J)
              PSHADE(L)=PSHADE0
            ENDDO
            GOTO 10
C
C UNEXPECTED END-OF-FILE ENCOUNTERED:
C
   15       WRITE(2,16)
   16       FORMAT(//,' ************* WARNING *************',/,
     &    ' END-OF-FILE ENCOUNTERED IN FILE: ', A20,/,/
     &    ' ICE COVER PSHADE SET TO VALUES CORRESPONDING ',
     &    ' TO LAST DAY IN FILE.',/)
            SHDDAY=(TCON*TBEGIN + NTC*TIDALP)/86400.0  ! *** PMC SINGLE LINE
   20       CONTINUE
            BACKSPACE(1001)
          ENDIF
        ENDIF
      ENDIF
      MPI_WTIMES(751)=MPI_WTIMES(751)+MPI_TOC(S2TIME)
      IF(PRINT_SUM)THEN
      call collect_in_zero_array(TEM)
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'2HEAT  = ', sum(abs(dble(TEM)))
      ENDIF
      ENDIF
!}
C
      ! *** DSLLC BEGIN CHANGE
CPMC      DELT=DT2
CPMC      IF(ISTL_.EQ.2)THEN
CPMC        DELT=DT
CPMC      ENDIF
CPMC      DELT=DT2
      IF(ISTL_.EQ.2)THEN
        IF(ISDYNSTP.EQ.0)THEN
          DELT=DT
        ELSE
          DELT=DTDYN
        END IF
      ELSE
        DELT=DT2
      ENDIF
      ! *** DSLLC END CHANGE

      ! *** OVERWRITE THE INPUT SOLAR RAD WITH A COMPUTED ONE
      IF(COMPUTESOLRAD)THEN
        S2TIME=MPI_TIC()
        CALL SHORT_WAVE_RADIATION(WINDST(2),RHA(2),TATMT(2),CLOUDT(2),
     &                            PATMT(2),SRO,SRON)
        MPI_WTIMES(752)=MPI_WTIMES(752)+MPI_TOC(S2TIME)
        S2TIME=MPI_TIC()
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          ! *** USE COMPUTED SRO
          SOLSWRT(L)=SRON
        ENDDO
        MPI_WTIMES(753)=MPI_WTIMES(753)+MPI_TOC(S2TIME)
      ENDIF
      IF(PRINT_SUM)THEN
      call collect_in_zero_array(TEM)
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'3HEAT  = ', sum(abs(dble(TEM)))
      ENDIF
      ENDIF
      IF(USESHADE)THEN
        S2TIME=MPI_TIC()
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          ! *** APPLY PSHADE FACTORS
          SOLSWRT(L)=SOLSWRT(L)*PSHADE(L)
        ENDDO
        MPI_WTIMES(754)=MPI_WTIMES(754)+MPI_TOC(S2TIME)
      ENDIF
      IF(ISTOPT(2).EQ.1)THEN
        ! *** FULL HEAT BALANCE WITH ATMOSPHERIC LINKAGE

        ! *** SET UP MIN DEPTH
        S2TIME=MPI_TIC()
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          HDEP(L)=MAX(HP(L),0.)
        ENDDO
        MPI_WTIMES(755)=MPI_WTIMES(755)+MPI_TOC(S2TIME)

        ! NET HEAT FLUX = RSN+RAN-RB-RE-RC  (WATT/M2)
        S2TIME=MPI_TIC()
!$OMP PARALLEL DO PRIVATE(SVPW,CLDFAC,RAN,FW,RE,RC,RB)
        DO L=LMPI2,LMPILA
          SVPW=(10.**((0.7859+0.03477*TEM(L,KC))/
     &          (1.+0.00412*TEM(L,KC))))
          ! *** Net atmospheric radiation (Diffusive)
          CLDFAC=1.0+0.0017*CLOUDT(L)**2
          ! ** .5153153831e-12 = 1000.0/3600.0*9.37E-6*2.0411E-7*0.97
          RAN=0.51531538e-12*(TATMT(L)+273.15)**6*CLDFAC
          ! *** Evaporation
          FW=9.2+0.46*WINDST(L)**2;
          RE=FW*(SVPW-VPA(L));
          ! *** Conduction
          RC=0.47*FW*(TEM(L,KC)-TATMT(L))
          ! *** Longwave back radiation
          ! ***  5.443E-8 = 5.67E-8 * 0.97
          RB=5.443E-8*(TEM(L,KC)+273.15)**4
          NETRAD(L,KC)=RAN-RB-RE-RC
!{GeoSR, 2015.01.15 JHLEE, NEGATIVE WATER TEMPERATURE PROBLEM
          IF(ISICE.EQ.1)THEN
            IF(TEM(L,KC).LT.0.0)THEN
              NETRAD(L,KC)=0.0
            ENDIF
          ENDIF
!} GeoSR, 2015.01.15 JHLEE, NEGATIVE WATER TEMPERATURE PROBLEM
        ENDDO
        MPI_WTIMES(756)=MPI_WTIMES(756)+MPI_TOC(S2TIME)
        ! *** NET SHORTWAVE SOLAR RADIATION
        IF(IASWRAD.EQ.0.)THEN
          ! *** ADSORB SW SOLR RAD TO ALL LAYERS AND BED

          ! *** SURFACE LAYER
          TFAST=SWRATNF*(Z(KC)-1.)
          TFAST1=SWRATNF*(Z(KC-1)-1.)
          TSLOW=SWRATNS*(Z(KC)-1.)
          TSLOW1=SWRATNS*(Z(KC-1)-1.)
          S2TIME=MPI_TIC()
          IF(FSWRATF.LT.1.)THEN
!$OMP PARALLEL DO PRIVATE(RSN)
            DO L=LMPI2,LMPILA
              RSN=SOLSWRT(L)*
     &           (    FSWRATF*(EXP(TFAST*HDEP(L))-EXP(TFAST1*HDEP(L)))
     &          +(1.-FSWRATF)*(EXP(TSLOW*HDEP(L))-EXP(TSLOW1*HDEP(L))))
              NETRAD(L,KC)=NETRAD(L,KC)+RSN
            ENDDO
          ELSE
!$OMP PARALLEL DO PRIVATE(RSN)
            DO L=LMPI2,LMPILA
              RSN=SOLSWRT(L)*(1.-EXP(TFAST1*HDEP(L)))
              NETRAD(L,KC)=NETRAD(L,KC)+RSN
            ENDDO
          ENDIF
          MPI_WTIMES(757)=MPI_WTIMES(757)+MPI_TOC(S2TIME)
          ! *** ALL REMAINING LAYERS
          S2TIME=MPI_TIC()
          IF(KC.GT.1)THEN
            DO K=1,KS
              TFAST=SWRATNF*(Z(K)-1.)
              TFAST1=SWRATNF*(Z(K-1)-1.)
              C2=DELT*DZIC(K)*0.2393E-6
              IF(FSWRATF.LT.1.)THEN
                TSLOW=SWRATNS*(Z(K)-1.)
                TSLOW1=SWRATNS*(Z(K-1)-1.)
!$OMP PARALLEL DO PRIVATE(RSN)
                DO L=LMPI2,LMPILA
                  RSN=SOLSWRT(L)*
     &             (    FSWRATF*(EXP(TFAST*HDEP(L))-EXP(TFAST1*HDEP(L)))
     &           +(1.-FSWRATF)*(EXP(TSLOW*HDEP(L))-EXP(TSLOW1*HDEP(L))))
                  NETRAD(L,K)=RSN
                ENDDO
              ELSE
!$OMP PARALLEL DO PRIVATE(RSN)
                DO L=LMPI2,LMPILA
                  RSN=SOLSWRT(L)*
     &               (EXP(TFAST*HDEP(L))-EXP(TFAST1*HDEP(L)))
                  NETRAD(L,K)=RSN
                ENDDO
              ENDIF
            ENDDO
          ENDIF
          MPI_WTIMES(758)=MPI_WTIMES(758)+MPI_TOC(S2TIME)
          ! *** Distribute heat flux to the bed for each grid cell.
          S2TIME=MPI_TIC()
          TFAST=SWRATNF*(Z(0)-1.)
          IF(FSWRATF.LT.1.)THEN
            TSLOW=SWRATNS*(Z(0)-1.)
!$OMP PARALLEL DO PRIVATE(UBED,VBED,USPD,TMPVAL)
            DO L=LMPI2,LMPILA
              UBED=0.5*( U(L,1)+U(L+1,1) )
              VBED=0.5*( V(L,1)+V(LNC(L),1) )
              USPD=SQRT( UBED*UBED+VBED*VBED )
              TMPVAL=(HTBED1*USPD+HTBED2)*(TEM(L,1)-TEMB(L))
              NETRAD(L,1)=NETRAD(L,1)-TMPVAL/0.2393E-6
              ! *** UPDATE BOTTOM
              IF(TBEDIT.GT.0.)THEN
                TEMB(L)=TEMB(L) + (TMPVAL + 0.2393E-6*SOLSWRT(L)*
     &               (FSWRATF *EXP(TFAST*HDEP(L))
     &           +(1.-FSWRATF)*EXP(TSLOW*HDEP(L))))*DELT/TBEDTHK(L)
              ENDIF
            ENDDO
          ELSE
!$OMP PARALLEL DO PRIVATE(UBED,VBED,USPD,TMPVAL)
            DO L=LMPI2,LMPILA
              UBED=0.5*( U(L,1)+U(L+1,1) )
              VBED=0.5*( V(L,1)+V(LNC(L),1) )
              USPD=SQRT( UBED*UBED+VBED*VBED )
              TMPVAL=(HTBED1*USPD+HTBED2)*(TEM(L,1)-TEMB(L))
              NETRAD(L,1)=NETRAD(L,1)-TMPVAL/0.2393E-6

              ! *** UPDATE BOTTOM
              IF(TBEDIT.GT.0.)THEN
                TEMB(L)=TEMB(L) + (TMPVAL + 0.2393E-6*SOLSWRT(L)*
     &                  FSWRATF*EXP(TFAST*HDEP(L)))*DELT/TBEDTHK(L)
              ENDIF
            ENDDO
          ENDIF
          MPI_WTIMES(759)=MPI_WTIMES(759)+MPI_TOC(S2TIME)
          IF(PRINT_SUM)THEN
          call collect_in_zero_array(TEM)
          IF(MYRANK.EQ.0)THEN
            PRINT*, n,'4HEAT  = ', sum(abs(dble(TEM)))
          ENDIF
          ENDIF
          ! *** NOW FINALIZE THE TEMPERATURE
          S2TIME=MPI_TIC()
          DO K=1,KC
            ! *** RHO = 1000.0  Density (kg / m^3)
            ! *** CP  = 4179.0  Specific Heat (J / kg / degC)
            ! *** 0.2393E-6 = 1/RHO/CP
            C1=DELT*DZIC(K)*0.2393E-6
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              TEM(L,K)=TEM(L,K)+HPI(L)*C1*NETRAD(L,K)
            ENDDO
          ENDDO
          IF(ISDRY.GT.0.AND.ISTOPT(2).EQ.1)THEN
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              IF(IMASKDRY(L).EQ.1.) TEMB(L)=TATMT(L)
            ENDDO
          ENDIF
          MPI_WTIMES(760)=MPI_WTIMES(760)+MPI_TOC(S2TIME)
          IF(PRINT_SUM)THEN
          call collect_in_zero_array(TEM)
          IF(MYRANK.EQ.0)THEN
            PRINT*, n,'5HEAT  = ', sum(abs(dble(TEM)))
          ENDIF
          ENDIF

        ELSE   ! IF(IASWRAD.EQ.1)THEN

          ! *** ADSORB SW SOLR RAD TO TO SURFACE LAYER
          S2TIME=MPI_TIC()
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            NETRAD(L,KC)=NETRAD(L,KC)+SOLSWRT(L)
          ENDDO
          ! *** NOW FINALIZE THE TEMPERATURE
          C1=DELT*DZIC(KC)*0.2393E-6
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            TEM(L,KC)=TEM(L,KC)+HPI(L)*C1*NETRAD(L,KC)
          ENDDO
          MPI_WTIMES(761)=MPI_WTIMES(761)+MPI_TOC(S2TIME)
          IF(PRINT_SUM)THEN
          call collect_in_zero_array(TEM)
          IF(MYRANK.EQ.0)THEN
            PRINT*, n,'6HEAT  = ', sum(abs(dble(TEM)))
          ENDIF
          ENDIF

        ENDIF

      ELSEIF(ISTOPT(2).EQ.2)THEN

        ! *** IMPLEMENT EXTERNALLY SPECIFIED EQUILIBRIUM TEMPERATURE FORMULATION
        S2TIME=MPI_TIC()
        TMPKC=DELT/DZC(KC)
        DO ND=1,NDM
          LF=2+(ND-1)*LDM
          LL=LF+LDM-1
          DO L=LF,LL
! [ GEOSR 2010.5.13
            TEM(L,KC)=TEM(L,KC)-TMPKC*CLOUDT(L)*HPI(L)*(TEM(L,KC)
     &          -TATMT(L))
c            TEM(L,KC)=TEM(L,KC)-TMPKC*SOLSWRT(L)*HPI(L)*(TEM(L,KC)
c     &          -TATMT(L))
! GEOSR 2010.5.13 ]
          ENDDO
        ENDDO
        MPI_WTIMES(762)=MPI_WTIMES(762)+MPI_TOC(S2TIME)
        IF(PRINT_SUM)THEN
        call collect_in_zero_array(TEM)
        IF(MYRANK.EQ.0)THEN
          PRINT*, n,'7HEAT  = ', sum(abs(dble(TEM)))
        ENDIF
        ENDIF

      ELSEIF(ISTOPT(2).EQ.3)THEN

        ! *** IMPLEMENT CONSTANT COEFFICIENT EQUILIBRIUM TEMPERATURE FORMULATION
        S2TIME=MPI_TIC()
        DTHEQT=DELT*HEQT*FLOAT(KC)
        DO ND=1,NDM
          LF=2+(ND-1)*LDM
          LL=LF+LDM-1
          DO L=LF,LL
            TEM(L,KC)=TEM(L,KC)-DTHEQT*HPI(L)*(TEM(L,KC)-TEMO)
          ENDDO
        ENDDO
        MPI_WTIMES(763)=MPI_WTIMES(763)+MPI_TOC(S2TIME)
        IF(PRINT_SUM)THEN
        call collect_in_zero_array(TEM)
        IF(MYRANK.EQ.0)THEN
          PRINT*, n,'8HEAT  = ', sum(abs(dble(TEM)))
        ENDIF
        ENDIF

      ELSEIF(ISTOPT(2).EQ.4)THEN
        IF(PRINT_SUM)THEN
        call collect_in_zero_array(TEM)
        IF(MYRANK.EQ.0)THEN
          PRINT*, n,'9HEAT  = ', sum(abs(dble(TEM)))
        ENDIF
        ENDIF
        ! *** IMPLEMENT W2 EQUILIBRIUM TEMPERATURE FORMULATION
        S2TIME=MPI_TIC()
        IF(.NOT.COMPUTESOLRAD)THEN
          ! *** MUST MAKE AT LEAST ONE CALL TO THIS TO INITIALIZE VARIABLES
          CALL SHORT_WAVE_RADIATION(WINDST(2),RHA(2),TATMT(2),CLOUDT(2),
     &                            PATMT(2),SRO,SRON)
        ENDIF
        MPI_WTIMES(764)=MPI_WTIMES(764)+MPI_TOC(S2TIME)
        ! *** SWRATNF - Background/Clear Water Extinction Coefficient
        ! *** SWRATNS - Light Extinction for TSS (1/m per g/m3)
        ! *** FSWRATF - Fraction of Solar Rad Absobed in the Surface Layer
        ! *** HTBED2  - Bottom Heat Exchange Coefficient (W/m2/s)
        S2TIME=MPI_TIC()
        IF(PRINT_SUM)THEN
        call collect_in_zero_array(TEM)
        IF(MYRANK.EQ.0)THEN
          PRINT*, n,'AHEAT  = ', sum(abs(dble(TEM)))
        ENDIF
        ENDIF
        PSHADE_OLD=-1.
        DO NRANK=0,NPROCS-1
           IF(MYRANK.EQ.NRANK)THEN
           DO L=LMPI2,LMPILA
             IF(PSHADE_OLD.NE.PSHADE(L))THEN
               IF(SOLSWRT(L).gt.0.01.OR.PSHADE_OLD.LT.-.99)then
                 CALL EQUILIBRIUM_TEMPERATURE(SOLSWRT(L),ET,CSHE)
               ENDIF
               PSHADE_OLD=PSHADE(L)
             ENDIF
             ! *** SURFACE HEAT FLUX
             THICK  =HP(L)*DZC(KC)
             TFLUX = CSHE*(ET-TEM(L,KC))/THICK*DELT
             TEM(L,KC) = TEM(L,KC)+TFLUX
             ! *** BEGIN PMC
             ! *** TEMPORARY FIX UNTIL BUILD IN ICE SUB-MODEL INTO THE HEAT SUB-MODEL (COOK INLET)
             IF(ISTRAN(1)>0)THEN
               IF( TEM(L,KC)<-1.3 )THEN
                 TEM(L,KC) = -1.3*(SAL(L,KC)/35.)
               ENDIF
             ELSE
               IF( TEM(L,KC)<0.1 )THEN
                 TEM(L,KC) = 0.1
               ENDIF
             ENDIF
             ! *** END PMC
             ! *** BOTTOM HEAT FLUX
             THICK = HP(L)*DZC(1)
             TFLUX = HTBED2*(TEMB(L)-TEM(L,1))*DELT
             TEM(L,1) = TEM(L,1)+TFLUX/THICK
             FLUXTB(L)=TFLUX
           ENDDO
           ENDIF
           CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
           IF(NRANK.LT.NPROCS-1)THEN
           IF(MYRANK==NRANK)THEN
            CALL MPI_ISEND( PSHADE_OLD,1,MPI_REAL,MYRANK+1,87,
     &      MPI_COMM_WORLD,IREQ1,IERR)
            CALL MPI_WAIT(IREQ1,STATUS1,IERR)
           ELSEIF(MYRANK==NRANK+1)THEN
            CALL MPI_IRECV( PSHADE_OLD,1,MPI_REAL,MYRANK-1,87,
     &      MPI_COMM_WORLD,IREQ2,IERR)
            CALL MPI_WAIT(IREQ2,STATUS2,IERR)
           ENDIF
           IF(MYRANK==NRANK)THEN
            CALL MPI_ISEND( ET,1,MPI_REAL,MYRANK+1,87,
     &      MPI_COMM_WORLD,IREQ1,IERR)
            CALL MPI_WAIT(IREQ1,STATUS1,IERR)
           ELSEIF(MYRANK==NRANK+1)THEN
            CALL MPI_IRECV( ET,1,MPI_REAL,MYRANK-1,87,
     &      MPI_COMM_WORLD,IREQ2,IERR)
            CALL MPI_WAIT(IREQ2,STATUS2,IERR)
           ENDIF
           IF(MYRANK==NRANK)THEN
            CALL MPI_ISEND( CSHE,1,MPI_REAL,MYRANK+1,87,
     &      MPI_COMM_WORLD,IREQ1,IERR)
            CALL MPI_WAIT(IREQ1,STATUS1,IERR)
           ELSEIF(MYRANK==NRANK+1)THEN
            CALL MPI_IRECV( CSHE,1,MPI_REAL,MYRANK-1,87,
     &      MPI_COMM_WORLD,IREQ2,IERR)
            CALL MPI_WAIT(IREQ2,STATUS2,IERR)
           ENDIF
           ENDIF
        ENDDO
        MPI_WTIMES(765)=MPI_WTIMES(765)+MPI_TOC(S2TIME)
        IF(PRINT_SUM)THEN
        call collect_in_zero_array(TEM)
        call collect_in_zero(FLUXTB)
        call collect_in_zero(TEMB)
        call collect_in_zero(SOLSWRT)
        call collect_in_zero(PSHADE)
        IF(MYRANK.EQ.0)THEN
          PRINT*, n,'1FLUXTB = ', sum(abs(dble(FLUXTB)))
          PRINT*, n,'1TEMB   = ', sum(abs(dble(TEMB)))
          PRINT*, n,'1SOLSWRT= ', sum(abs(dble(SOLSWRT)))
          PRINT*, n,'1PSHADE = ', sum(abs(dble(PSHADE)))
          PRINT*, n,'BHEAT   = ', sum(abs(dble(TEM)))
        ENDIF
        ENDIF
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
        CALL MPI_BCAST(SOLSWRT(2),1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
        ! *** Distribute Solar Radiation Across Water Column
        IF(SOLSWRT(2).GT.0.1)THEN
          S2TIME=MPI_TIC()
!$OMP PARALLEL DO PRIVATE(K,TSS_ABOVE,
!$OMP+      WQCHL_ABOVE,POM_ABOVE,GAMMA,TOP,EXPTOP,
!$OMP+      K_ABOVE,SRON,BOT,EXPBOT) 
!$OMP+      FIRSTPRIVATE(TSSS_ABOVE,WQCHLS_ABOVE,POMS_ABOVE)
          DO L=LMPI2,LMPILA
            K=KC
            IF(ISTRAN(6).GT.0.OR.ISTRAN(7).GT.0)THEN
              TSSS_ABOVE=SNDT(L,K)+SEDT(L,K)
              TSS_ABOVE=TSSS_ABOVE
            ELSE
              TSS_ABOVE=0.0
            ENDIF
            IF(ISTRAN(8).GT.0)THEN
              ! *** Water Quality is Active so account for Chlorophyll and POM
              ! *** If using WQ then use the WQ Coefficients
              WQCHLS_ABOVE=WQCHL(L,K)
              WQCHL_ABOVE=WQCHLS_ABOVE
              POMS_ABOVE=WQV(L,K,4)+WQV(L,K,5)
              POM_ABOVE =POMS_ABOVE
              GAMMA = WQKEB(1) + WQKETSS*TSS_ABOVE   +
     &                           WQKECHL*WQCHL_ABOVE +
     &                           WQKEPOM*POM_ABOVE
            ELSE
              GAMMA = SWRATNF + SWRATNS*TSS_ABOVE
            ENDIF

            TOP=GAMMA*HP(L)*(Z(K-1)-1.)
            EXPTOP=EXP(TOP)
            K_ABOVE=1.

            ! *** ENSURE AT LEAST THE FSWRATF FRACTION OF SRO IS ATTENUATED
            IF((1.-EXPTOP).GT.FSWRATF)THEN
              SRON=SOLSWRT(L)*EXPTOP
            ELSE
              SRON=SOLSWRT(L)*(1.0-FSWRATF)
            ENDIF
            EXPBOT=0.0
            DO K = KS,1,-1
              ! *** Net Extinction Coefficient
              K_ABOVE=K_ABOVE+1.
              IF(ISTRAN(6).GT.0.OR.ISTRAN(7).GT.0)THEN
                TSSS_ABOVE=TSSS_ABOVE+SNDT(L,K)+SEDT(L,K)
                TSS_ABOVE=TSSS_ABOVE/K_ABOVE
              ENDIF
              IF(ISTRAN(8).GT.0)THEN
                ! *** Water Quality is Active so account for Chlorophyll
                ! *** If using WQ then use the WQ Coefficients
                POMS_ABOVE=POMS_ABOVE+WQV(L,K,4)+WQV(L,K,5)
                POM_ABOVE=POMS_ABOVE/K_ABOVE

                WQCHLS_ABOVE=WQCHLS_ABOVE+WQCHL(L,K)
                WQCHL_ABOVE=WQCHLS_ABOVE/K_ABOVE
                GAMMA = WQKEB(1) + WQKETSS*TSS_ABOVE   +
     &                             WQKECHL*WQCHL_ABOVE +
     &                             WQKEPOM*POM_ABOVE
              ELSE
                GAMMA = SWRATNF + SWRATNS*TSS_ABOVE
              ENDIF

              BOT=GAMMA*HP(L)*(Z(K-1)-1.)

              ! *** Compute Net Energy
              EXPBOT=EXP(BOT)
              NETRAD(L,K)=SRON*(EXPTOP-EXPBOT)
              TOP=BOT
              EXPTOP=EXPBOT
            ENDDO
            RADBOT(L)=EXPBOT*SRON
          ENDDO
          MPI_WTIMES(766)=MPI_WTIMES(766)+MPI_TOC(S2TIME)
          ! *** NOW FINALIZE THE TEMPERATURE
          S2TIME=MPI_TIC()
          DO K=1,KS
            ! *** RHO = 1000.0  Density (kg / m^3)
            ! *** CP  = 4179.0  Specific Heat (J / kg / degC)
            ! *** 0.2393E-6 = 1/RHO/CP --> Conversion from Watts
            C1=DELT*DZIC(K)*0.2393E-6
C!$OMP PARALLEL DO PRIVATE(TEMO)
            DO L=LMPI2,LMPILA
              TEMO=TEM(L,K)+HPI(L)*C1*NETRAD(L,K)
              !IF(ABS(TEM(L,K)-TEMO).GT.PMCTOL)THEN
              !  IPMC=0
              !ENDIF
              TEM(L,K)=TEMO
            ENDDO
          ENDDO
          MPI_WTIMES(767)=MPI_WTIMES(767)+MPI_TOC(S2TIME)
          CALL MPI_BCAST(TEMO,1,MPI_REAL,NPROCS-1,MPI_COMM_WORLD,IERR)
        ENDIF

        ! *** UPDATE BOTTOM
        S2TIME=MPI_TIC()
        IF(TBEDIT.GT.0.)THEN
          IF(SOLSWRT(2).GT.0.01.AND.BETAF.GT.0.)THEN
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              TEMB(L)=TEMB(L) +
     &                (0.2393E-6*RADBOT(L)*DELT*BETAF - FLUXTB(L))
     &                      /TBEDTHK(L)
            ENDDO
          ELSE
!$OMP PARALLEL DO
            DO L=LMPI2,LMPILA
              TEMB(L)=TEMB(L) - FLUXTB(L)/TBEDTHK(L)
            ENDDO
          ENDIF
        ENDIF
        MPI_WTIMES(768)=MPI_WTIMES(768)+MPI_TOC(S2TIME)
      ENDIF
      IF(PRINT_SUM)THEN
      call collect_in_zero_array(TEM)
      call collect_in_zero(FLUXTB)
      call collect_in_zero(TEMB)
      call collect_in_zero(SOLSWRT)
      call collect_in_zero(PSHADE)
      IF(MYRANK.EQ.0)THEN
        PRINT*, n,'2FLUXTB = ', sum(abs(dble(FLUXTB)))
        PRINT*, n,'2TEMB   = ', sum(abs(dble(TEMB)))
        PRINT*, n,'2SOLSWRT= ', sum(abs(dble(SOLSWRT)))
        PRINT*, n,'2PSHADE = ', sum(abs(dble(PSHADE)))
        PRINT*, n,'CHEAT   = ', sum(abs(dble(TEM)))
      ENDIF
      ENDIF
      ! *** APPLY DRY CELL CORRECTIONS
      S2TIME=MPI_TIC()
      IF(ISDRY.GT.0)THEN
        DO K=1,KC
!$OMP PARALLEL DO
          DO L=LMPI2,LMPILA
            IF(.NOT.LMASKDRY(L))THEN
              TEM(L,K)=TATMT(L)
              ! *** BEGIN PMC
              ! *** TEMPORARY FIX UNTIL BUILD IN ICE SUB-MODEL INTO THE HEAT SUB-MODEL (COOK INLET)
              IF(ISTRAN(1)>0)THEN
                IF( TEM(L,K)<-1.3 )THEN
                  TEM(L,K) = -1.3*(SAL(L,K)/35.)
                ENDIF
              ELSE
                IF( TEM(L,K)<0.1 )THEN
                  TEM(L,K) = 0.1
                ENDIF
              ENDIF
              ! *** END PMC
            ENDIF
          ENDDO
        ENDDO
      ENDIF
        IF(PRINT_SUM)THEN
        call collect_in_zero_array(TEM)
        IF(MYRANK.EQ.0)THEN
          PRINT*, n,'DHEAT  = ', sum(abs(dble(TEM)))
        ENDIF
        ENDIF
      MPI_WTIMES(769)=MPI_WTIMES(769)+MPI_TOC(S2TIME)

C 600 FORMAT(4I5,2E12.4)

      RETURN
      END
