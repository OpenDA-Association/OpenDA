      SUBROUTINE CALHEAT(ISTL_)  
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
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::NETRAD  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TBEDTHK
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::HDEP
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RADBOT
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::FLUXTB
      !REAL,SAVE ::    PTIME
      !REAL,SAVE ::    PMCTOL
      REAL            K_ABOVE
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
        PRINT *,'CALHEAT: INITIALIZING'
        CALL HEAT_EXCHANGE

        !PMCTOL=0.1
        NETRAD=0.
        HDEP=0.
        TBEDTHK=0.
        IF((ISTOPT(2).EQ.1.AND.IASWRAD.EQ.0).OR.ISTOPT(2).EQ.4)THEN 
          IF(DABEDT.GT.0.)THEN
            PRINT *,'CALHEAT: SETTING CONSTANT THICKNESS TO:',DABEDT
            DO L=2,LA
              TBEDTHK(L)=DABEDT
            ENDDO
          ELSE
            ! *** READ IN THE SPATIALLY VARYING INIT T AND BED THICKNESS (DABEDT) 
            PRINT *,'CALHEAT: READ IN THE SPATIALLY VARYING INIT T AND
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

        IF(DEBUG)THEN
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
        CALL SHORT_WAVE_RADIATION(WINDST(2),RHA(2),TATMT(2),CLOUDT(2),
     &                            PATMT(2),SRO,SRON)
        DO L=2,LA
          ! *** USE COMPUTED SRO
          SOLSWRT(L)=SRON
        ENDDO
      ENDIF

      IF(USESHADE)THEN
        DO L=2,LA
          ! *** APPLY PSHADE FACTORS
          SOLSWRT(L)=SOLSWRT(L)*PSHADE(L)
        ENDDO
      ENDIF

      IF(ISTOPT(2).EQ.1)THEN  
        ! *** FULL HEAT BALANCE WITH ATMOSPHERIC LINKAGE
!
!$OMP PARALLEL DO PRIVATE(LF,LL,
!$OMP& SVPW,CLDFAC,RAN,FW,RE,RC,
!$OMP& RB,TFAST,TFAST1,TSLOW,TSLOW1,
!$OMP& RSN,C2,UBED,VBED,USPD,TMPVAL,
!$OMP& C1)
                do ithds=0,nthds-1
                  LF=jse(1,ithds)
                  LL=jse(2,ithds)

        ! NET HEAT FLUX = RSN+RAN-RB-RE-RC  (WATT/M2)
        DO L=LF,LL
          ! *** SET UP MIN DEPTH
          HDEP(L)=MAX(HP(L),0.)  
        

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

        ! *** NET SHORTWAVE SOLAR RADIATION
        IF(IASWRAD.EQ.0.)THEN  
          ! *** ADSORB SW SOLR RAD TO ALL LAYERS AND BED  

          ! *** SURFACE LAYER
          TFAST=SWRATNF*(Z(KC)-1.)
          TFAST1=SWRATNF*(Z(KC-1)-1.)
          TSLOW=SWRATNS*(Z(KC)-1.)
          TSLOW1=SWRATNS*(Z(KC-1)-1.)
          IF(FSWRATF.LT.1.)THEN
            DO L=LF,LL
              RSN=SOLSWRT(L)*  
     &           (    FSWRATF*(EXP(TFAST*HDEP(L))-EXP(TFAST1*HDEP(L)))  
     &          +(1.-FSWRATF)*(EXP(TSLOW*HDEP(L))-EXP(TSLOW1*HDEP(L))))    
              NETRAD(L,KC)=NETRAD(L,KC)+RSN
            ENDDO
          ELSE
            DO L=LF,LL
              RSN=SOLSWRT(L)*(1.-EXP(TFAST1*HDEP(L)))  
              NETRAD(L,KC)=NETRAD(L,KC)+RSN
            ENDDO
          ENDIF
  
          ! *** ALL REMAINING LAYERS
          IF(KC.GT.1)THEN  
            DO K=1,KS  
              TFAST=SWRATNF*(Z(K)-1.)
              TFAST1=SWRATNF*(Z(K-1)-1.)
              C2=DELT*DZIC(K)*0.2393E-6
              IF(FSWRATF.LT.1.)THEN
                TSLOW=SWRATNS*(Z(K)-1.)
                TSLOW1=SWRATNS*(Z(K-1)-1.)
                DO L=LF,LL
                  RSN=SOLSWRT(L)*   
     &             (    FSWRATF*(EXP(TFAST*HDEP(L))-EXP(TFAST1*HDEP(L)))  
     &           +(1.-FSWRATF)*(EXP(TSLOW*HDEP(L))-EXP(TSLOW1*HDEP(L))))    
                  NETRAD(L,K)=RSN
                ENDDO  
              ELSE
                DO L=LF,LL
                  RSN=SOLSWRT(L)*
     &               (EXP(TFAST*HDEP(L))-EXP(TFAST1*HDEP(L)))
                  NETRAD(L,K)=RSN
                ENDDO  
              ENDIF
            ENDDO  
          ENDIF  

          ! *** Distribute heat flux to the bed for each grid cell.
          TFAST=SWRATNF*(Z(0)-1.)
          IF(FSWRATF.LT.1.)THEN
            TSLOW=SWRATNS*(Z(0)-1.)
            DO L=LF,LL
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
            DO L=LF,LL
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

          ! *** NOW FINALIZE THE TEMPERATURE 
          DO K=1,KC  
            ! *** RHO = 1000.0  Density (kg / m^3)
            ! *** CP  = 4179.0  Specific Heat (J / kg / degC)
            ! *** 0.2393E-6 = 1/RHO/CP
            C1=DELT*DZIC(K)*0.2393E-6
            DO L=LF,LL
              TEM(L,K)=TEM(L,K)+HPI(L)*C1*NETRAD(L,K)  
            ENDDO  
          ENDDO  

          IF(ISDRY.GT.0.AND.ISTOPT(2).EQ.1)THEN  
            DO L=LF,LL
              IF(IMASKDRY(L).EQ.1.) TEMB(L)=TATMT(L)  
            ENDDO  
          ENDIF  
  
        ELSE   ! IF(IASWRAD.EQ.1)THEN  
          
          C1=DELT*DZIC(KC)*0.2393E-6
          DO L=LF,LL
            ! *** ADSORB SW SOLR RAD TO TO SURFACE LAYER
            NETRAD(L,KC)=NETRAD(L,KC)+SOLSWRT(L)
            ! *** NOW FINALIZE THE TEMPERATURE 
            TEM(L,KC)=TEM(L,KC)+HPI(L)*C1*NETRAD(L,KC)  
          ENDDO  
        ENDIF  
!                
                enddo
!$OMP END PARALLEL DO
      ELSEIF(ISTOPT(2).EQ.2)THEN  
        ! *** IMPLEMENT EXTERNALLY SPECIFIED EQUILIBRIUM TEMPERATURE FORMULATION  
        TMPKC=DELT/DZC(KC)  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
          DO L=LF,LL  
! [ GEOSR 2010.5.13
c            TEM(L,KC)=TEM(L,KC)-TMPKC*CLOUDT(L)*HPI(L)*(TEM(L,KC)
c     &          -TATMT(L))  
            TEM(L,KC)=TEM(L,KC)-TMPKC*SOLSWRT(L)*HPI(L)*(TEM(L,KC)
     &          -TATMT(L))  
! GEOSR 2010.5.13 ]
          ENDDO  
        ENDDO  
!$OMP END PARALLEL DO
      ELSEIF(ISTOPT(2).EQ.3)THEN  
        ! *** IMPLEMENT CONSTANT COEFFICIENT EQUILIBRIUM TEMPERATURE FORMULATION  
        DTHEQT=DELT*HEQT*FLOAT(KC)  
        DO ND=1,NDM  
          LF=2+(ND-1)*LDM  
          LL=LF+LDM-1  
          DO L=LF,LL  
            TEM(L,KC)=TEM(L,KC)-DTHEQT*HPI(L)*(TEM(L,KC)-TEMO)  
          ENDDO  
        ENDDO  

      ELSEIF(ISTOPT(2).EQ.4)THEN  
        ! *** IMPLEMENT W2 EQUILIBRIUM TEMPERATURE FORMULATION 
        IF(.NOT.COMPUTESOLRAD)THEN
          ! *** MUST MAKE AT LEAST ONE CALL TO THIS TO INITIALIZE VARIABLES
          CALL SHORT_WAVE_RADIATION(WINDST(2),RHA(2),TATMT(2),CLOUDT(2),
     &                            PATMT(2),SRO,SRON)
        ENDIF
        ! *** SWRATNF - Background/Clear Water Extinction Coefficient
        ! *** SWRATNS - Light Extinction for TSS (1/m per g/m3)
        ! *** FSWRATF - Fraction of Solar Rad Absobed in the Surface Layer
        ! *** HTBED2  - Bottom Heat Exchange Coefficient (W/m2/s)
        PSHADE_OLD=-1.0
        DO L=2,LA
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

        ! *** Distribute Solar Radiation Across Water Column
        IF(SOLSWRT(2).GT.0.1)THEN
          DO L=2,LA
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

          ! *** NOW FINALIZE THE TEMPERATURE 
          DO K=1,KS
            ! *** RHO = 1000.0  Density (kg / m^3)  
            ! *** CP  = 4179.0  Specific Heat (J / kg / degC)
            ! *** 0.2393E-6 = 1/RHO/CP --> Conversion from Watts
            C1=DELT*DZIC(K)*0.2393E-6
            DO L=2,LA  
              TEMO=TEM(L,K)+HPI(L)*C1*NETRAD(L,K)
              !IF(ABS(TEM(L,K)-TEMO).GT.PMCTOL)THEN
              !  IPMC=0
              !ENDIF
              TEM(L,K)=TEMO
            ENDDO  
          ENDDO  
          
          !DO L=2,LA   ! *** PMC PMC PMC
          !  DO K=1,KS
          !    IF(HP(L).LT.1.0.AND.(TEM(L,K)-PMCTOL).GT.TEM(L,K+1)) THEN
          !      IPMC = 0
          !    ENDIF
          !  ENDDO
          !ENDDO

        ENDIF

        ! *** UPDATE BOTTOM
        IF(TBEDIT.GT.0.)THEN
          IF(SOLSWRT(2).GT.0.01.AND.BETAF.GT.0.)THEN
            DO L=2,LA
              TEMB(L)=TEMB(L) + 
     &                (0.2393E-6*RADBOT(L)*DELT*BETAF - FLUXTB(L))
     &                      /TBEDTHK(L)
            ENDDO
          ELSE
            DO L=2,LA
              TEMB(L)=TEMB(L) - FLUXTB(L)/TBEDTHK(L)
            ENDDO
          ENDIF
        ENDIF            
      ENDIF  

      ! *** APPLY DRY CELL CORRECTIONS
      IF(ISDRY.GT.0)THEN  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO K=1,KC  
          DO L=LF,LL
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
c
      enddo
      ENDIF  

  600 FORMAT(4I5,2E12.4)  

      RETURN  
      END  

************************************************************************
**           S U B R O U T I N E   H E A T  E X C H A N G E           **
**                                                                    **
**                      FROM CE-QUAL-W2 (VER 3.1)                     **
**                                                                    **
************************************************************************

      SUBROUTINE HEAT_EXCHANGE

      USE GLOBAL
      
******* Tupe declaration

        REAL      JDAY
        INTEGER*4 IDAY

******* Allocate/Dimension declaration

        Real, SAVE ::  MPS_TO_MPH, W_M2_TO_BTU_FT2_DAY 
        Real, SAVE ::  FLUX_BR_TO_FLUX_SI, BTU_FT2_DAY_TO_W_M2 
        Real, SAVE ::  REFL 
        Real, SAVE ::  AFW,BFW,CFW, BCONV
        Real, SAVE ::  TDEW_F, TAIR_F, WIND_2M
        Real, SAVE ::  TIMENEXT


******* Data declaration

        DATA MPS_TO_MPH          /2.23714/, 
     .       W_M2_TO_BTU_FT2_DAY /7.60796/,
     .       FLUX_BR_TO_FLUX_SI  /0.23659/
        DATA BTU_FT2_DAY_TO_W_M2 /0.1314/
        DATA AFW /9.2/, BFW /0.46/, CFW /2.0/, BCONV /1.520411/
        DATA REFL/0.06/  ! *** SHORTWAVE RADIATION RELFECTED OFF SURFACE  

******* Function declaration

        DEG_F(X) = X*1.8+32.0
        DEG_C(X) = (X-32.0)*5.0/9.0

        TIMENEXT=TIMEDAY+1./24.
      RETURN

************************************************************************
**                 S H O R T  W A V E  R A D I A T I O N              **
************************************************************************

      ENTRY SHORT_WAVE_RADIATION(WSPD,TD,TAIR,CLD,ATMPR,SRO,SRON)

******* Input Conversions
        IF(TD.LT.1.1.AND.IRELH(NASER).EQ.1)THEN
          ! *** TD IS RELATIVE HUMIDITY.  CONVERT TO DEW POINT
          ! *** Jensen et al. (1990) ASCE Manual No. 70 (see pages 176 & 177)

          ! *** Ambient vapor pressure in kPa 
          VaporP=TD* 0.611*EXP(17.27*TAIR/(TAIR+237.3)) 
 
          ! *** Compute dewpoint temperature (Tdew) in C
          Tdew=(116.9+237.3*LOG(VaporP))/(16.78-LOG(VaporP))

          ! *** Compute wet bulb temperature (Tw)
          !PSR=ATMPR/10.  ! ***  Convert millibars to kPa
          !Gamma=0.00066*PSR
          !Delta=4098.*VaporP/(Tdew+237.3)**2
          !Tw=((Gamma*TAIR)+(Delta*Tdew))/(Gamma+Delta)
        ELSE
          TDEW=TD
        ENDIF

        CLD10 = CLD*10.  ! *** CONVERT FROM 0-1 (EFDC) TO 0-10 (W2)
        IF(WINDH<=0.0)WINDH = 10.0     ! *** FIXING THE WIND SPEED MEASUREMENT HEIGHT

******* British units

        TDEW_F   = DEG_F(TDEW)
        TAIR_F   = DEG_F(TAIR)
        WIND_MPH = WSPD*MPS_TO_MPH
        WIND_2M  = WIND_MPH*(LOG(2.0/0.003)/LOG(WINDH/0.003))

******* Shortwave Radiation

        STANDARD = 15.0*INT(DS_LONG/15.0)

        ! *** Day of the Year
        THOUR    = (TIMEDAY-INT(TIMEDAY))*24.0
        IDAY     =  TIMEDAY-INT(TIMEDAY/365.)*365.
        IDAY     =  IDAY+INT(INT(TIMEDAY/365.)/4.)
        JDAY     = REAL(IDAY)
        PMC1     = (2.*PI*(JDAY-1.))/365.
        EQTNEW   =  0.170*SIN(4.*PI*(JDAY-80.)/373.)-
     &              0.129*SIN(2.*PI*(JDAY-8.)/355.)
        H     = 0.2618*(THOUR-(DS_LONG-STANDARD)*0.066667+EQTNEW-12.0)
        DECL =  0.006918-0.399912*COS(PMC1)  +0.070257*SIN(PMC1)-
     &                   0.006758*COS(2*PMC1)+0.000907*SIN(2*PMC1)-
     &                   0.002697*COS(3*PMC1)+0.001480*SIN(3*PMC1)
        SINAL = SIN(DS_LAT*.017453)*SIN(DECL)+COS(DS_LAT*.017453)
     .          *COS(DECL)*COS(H)
        A0    = 57.2958*ASIN(SINAL)
        IF (A0.GT.0.0) THEN
          SRO  = 2.044*A0+0.1296*A0**2-1.941E-3*A0**3+7.591E-6*A0**4
          SRO  = (1.0-0.0065*CLD10**2)*SRO*24.0
          SRO  = SRO*BTU_FT2_DAY_TO_W_M2
          SRON = SRO*(1.0-REFL) ! *** Adjust for surface reflection
        ELSE
          SRO  = 0.0
          SRON = 0.0
        END IF
      RETURN


************************************************************************
**             E Q U I L I B R I U M  T E M P E R A T U R E           **
************************************************************************

      ENTRY EQUILIBRIUM_TEMPERATURE(SRON,ET,CSHE)

******* British units
        
        ! *** SRON Should already be adjusted for Shading & Reflection
        SRO_BR   = SRON*W_M2_TO_BTU_FT2_DAY 

******* Equilibrium temperature and heat exchange coefficient
        
        ET    = TDEW_F
        TSTAR = (ET+TDEW_F)*0.5
        BETA  = 0.255-(8.5E-3*TSTAR)+(2.04E-4*TSTAR*TSTAR)
        FW    = W_M2_TO_BTU_FT2_DAY*AFW+BCONV*BFW*WIND_2M**CFW
        CSHE  = 15.7+(0.26+BETA)*FW
        RA    = 3.1872E-08*(TAIR_F+459.67)**4
        ETP   = (SRO_BR+RA-1801.0)/CSHE+(CSHE-15.7)
     .            *(0.26*TAIR_F+BETA*TDEW_F)/(CSHE*(0.26+BETA))
        J     = 0
        DO WHILE (ABS(ETP-ET).GT.0.05.AND.J.LT.10)
          ET    = ETP
          TSTAR = (ET+TDEW_F)*0.5
          BETA  = 0.255-(8.5E-3*TSTAR)+(2.04E-4*TSTAR*TSTAR)
          CSHE  = 15.7+(0.26+BETA)*FW
          ETP   = (SRO_BR+RA-1801.0)/CSHE+(CSHE-15.7)
     .            *(0.26*TAIR_F+BETA*TDEW_F)/(CSHE*(0.26+BETA))
          J     = J+1
        END DO

******* SI units

        ! *** RHO = 1000.0  Density (kg / m^3)
        ! *** CP  = 4179.0  Specific Heat (J / kg / degC)
        ! *** 0.2393E-6 = 1/RHO/CP
        ET   = DEG_C(ET)
        CSHE = CSHE*FLUX_BR_TO_FLUX_SI*0.2393E-6

        IF(DEBUG)THEN
          IF(TIMEDAY.GT.TIMENEXT)THEN
            TD_C=DEG_C(TDEW_F)
            TA_C=DEG_C(TAIR_F)
            WRITE(77,999)TIMEDAY,SRON,ET,TD_C,TA_C,TDEW_F,TAIR_F,FW,CSHE
  999       FORMAT(F11.4,8F9.2,E12.4)
            TIMENEXT = TIMENEXT+1./24.
          ENDIF
        ENDIF
      RETURN

      END

