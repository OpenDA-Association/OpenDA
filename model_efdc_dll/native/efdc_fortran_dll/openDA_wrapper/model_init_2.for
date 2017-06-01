


































































































      SUBROUTINE model_init_2

C     Second part of initialization after INPUT(TITLE) is called
C     and before the RESTART subroutines are called
C     Inserted from EFDC.for [lines:193-939]

C     Actions:

C **  SET TIME RELATED PARAMETERS  
C       THE PARAMETER NTC=NUMBER OF TIME CYCLES, CONTROLS  
C       THE LENGTH OF RUN (NUMBER OF TIME STEPS)  

C **  SET CONTROLS FOR WRITING TO INSTANTANEOUS 2D SCALAR CONTOURING  
C       AND 2D VELOCITY VECTOR PLOTTING FILES  
C       SCALAR FIELD CONTOURING IN HORIZONTAL PLANES: SUBROUTINE SALPLTH  

C **  SEDIMENT BED PROPERTIES CONTOURING IN HORIZONTAL  
C       PLANES: SUBROUTINE SBEDPLT  

C **  FREE SURFACE ELEVATION OR PRESSURE CONTOURING IN HORIZONTAL  
C       PLANES: SUBROUTINE SURFPLT  

C **  VELOCITY VECTOR PLOTTING IN HORIZONTAL PLANES: SUBROUTINE VELPLTH  

C **  SCALAR FIELD CONTOURING IN VERTICAL PLANES: SUBROUTINE SALPLTV  

C **  NORMAL VELOCITY CONTOURING AND TANGENTIAL VELOCITY VECTOR  
C       PLOTTING IN VERTICAL PALNES: SUBROUTINE VELPLTV  

C **  THREE-DIMENSIONAL HDF FORMAT GRAPHICS FILES: SUBROUTINE OUT3D  
C  

  
C **  SET CONTROLS FOR WRITING TO FILTERED, AVERAGED OR RESIDUAL  
C       2D SCALAR CONTOURING AND 2D VELOCITY VECTOR PLOTTING FILES  
C       RESIDUAL SALINITY, TEMPERATURE, DYE AND SEDIMENT CONCENTRATION  
C       CONTOURING IN HORIZONTAL: SUBROUTINE RSALPLTH  

C **  RESIDUAL VELOCITY VECTOR PLOTTING IN HORIZONTAL PLANES:  
C       SUBROUTINE RVELPLTH  
  
C **  RESIDUAL SURFACE ELEVATION PLOTTING IN HORIZONTAL PLANES:  
C       SUBROUTINE RVELPLTH  
  
C **  RESIDUAL SCALAR FIELD CONTOURING IN VERTICAL  
C       PLANES: SUBROUTINE RSALPLTV  
  
C **  RESIDUAL NORMAL AND TANGENTIAL VELOCITY CONTOURING AND AND  
C       TANGENTIAL VELOCITY VECTOR PLOTTING IN VERTICAL PLANES:  
C       SUBROUTINE RVELPLTV  
  
C **  SET CONTROLS FOR WRITING TO DRIFTER, HARMONIC ANALYSIS,  
C       RESIDUAL TRANSPORT, AND BLANCE OUTPUT FILES  

C **  SET CONTROL FOR CALCULATION OF LAGRANGIAN MEAN VELOCITIY FIELDS  
C       BY PARTICLE TRACKING  

C **  SET SOME CONSTANTS  

C **  SET CONSTANTS FOR M2 TIDAL CYCLE HARMONIC ANALYSIS  

C **  SET WEIGHTS FOR SALINITY AND TEMPERATURE BOUNDARY INTERPOLATION  

C **  INITIALIZE ARRAYS  

C **  READ IN XLON AND YLAT OR UTME AND UTMN OF CELL CENTERS OF  
C       CURVILINEAR PORTION OF THE  GRID  

C **  WIMS COMMUNICATION

C **  SET CORNER CELL STRESS CORRECTION

C **  READ SPATIAL AVERAGING MAP FOR FOOD CHAIN MODEL OUTPUT

C **  READ IN COUNTER CLOCKWISE ANGLE FROM EAST SPECIFYING  
C **  PRINCIPAL FLOOD FLOW DIRECTION  

C **  SET BOUNDARY CONDITION SWITCHES  

C **  CALCUATE CURVATURE METRICS (NEW ADDITION)  

C **  DYDI  

C **  DXDJ  

      use omp_lib
      USE GLOBAL



































      USE model_extra_global


      IMPLICIT NONE



      REAL,ALLOCATABLE,DIMENSION(:,:)::SHOTS
































      INTEGER :: I, J, K, L, M, LL, LF,  LT, NT
      INTEGER :: LS, LN
      INTEGER :: IS, NS
      INTEGER :: ISNAP, TSHIFT
      INTEGER :: NTMP, NSHOTS
      INTEGER :: NFDCHIJ
      INTEGER :: ITMPVAL
      INTeger :: ithds
      REAL :: T0, T3, T4, T9
      REAL :: WTQCLT, TQCLT
      REAL :: DET, DETTMP 
      REAL :: DDYDDDX, DDXDDDY, DYUP1, DYUM1
      REAL :: DXVLN, DXVLS
      REAL :: XLNUTME, YLNUTME, YLTUTMN
      REAL :: CCUE, CCVE, CCUN, CCVN
      REAL :: TMPVAL, TMPCOR
      REAL :: ANG, ANG1, ANG2,  ANGTMP1, ANGTMP2
      REAL :: TA1, TA2
      REAL :: ZERO, FORCSUM


















       LF=2
       LL=LF+LDM-1
       if(LL.ne.LA) stop 99
       LL=LA
       write(6,*) nthds,LF,LL 
       call pbm_cut(jse,LF,LL,nthds)
       do ithds=0,nthds-1
         lf=jse(1,ithds)
         ll=jse(2,ithds)
         write(6,*) ithds,lf,ll,ll-lf+1
       enddo
         write(6,*)
       do ithds=0,nthds-1
         jse_LC(1,ithds)=jse(1,ithds)
         jse_LC(2,ithds)=jse(2,ithds)
       enddo
         jse_LC(1,0)=1
         jse_LC(2,nthds-1)=LC
       do ithds=0,nthds-1
         lf=jse_LC(1,ithds)
         ll=jse_LC(2,ithds)
c        write(6,*) ithds,lf,ll,ll-lf+1
       enddo
       do ithds=0,nthds-1
         jse_2_LC(1,ithds)=jse(1,ithds)
         jse_2_LC(2,ithds)=jse(2,ithds)
       enddo
         jse_2_LC(2,nthds-1)=LC
       do ithds=0,nthds-1
         lf=jse_LC(1,ithds)
         ll=jse_LC(2,ithds)
c        write(6,*) ithds,lf,ll,ll-lf+1
       enddo

C  
C **  CALL SUBROUTINE TO ADJUST, CONVERT AND SMOOTH DEPTH FIELD  
C  
C      IF(NSHMAX.GE.1) CALL DEPSMTH     PMC
C  
C **  SET TIME RELATED PARAMETERS  
C **  THE PARAMETER NTC=NUMBER OF TIME CYCLES, CONTROLS  
C **  THE LENGTH OF RUN (NUMBER OF TIME STEPS)  
C  
c      TCYCLE=0.0  
      TLRPD=0.0  
      THDMT=0.0  
      TVDIF=0.0  
      TCGRS=0.0  
      TSSED=0.0  ! *** EE SINGLE LINE 
      TSSEDZLJ=0.0 
      TCONG=0.0  
      TSADV=0.0  
      TRELAXV=0.0  
      TPUV=0.0  
      TCEXP=0.0  
      TAVB=0.0  
      TUVW=0.0  
      TQQQ=0.0  
      TQCLT=0.0  
      TWQADV=0.0  
      TWQDIF=0.0  
      TWQKIN=0.0  
      TWQSED=0.0  
      WTQCLT=0.0  
      CFMAX=CF  
      PI=3.1415926535898  
      NBAN=49  

      ! ***  NTC:     NUMBER OF REFERENCE TIME PERIODS IN RUN
      ! ***  NTSPTC:  NUMBER OF TIME STEPS PER REFERENCE TIME PERIOD
      ! ***  TCON:    CONVERSION MULTIPLIER TO CHANGE TBEGIN TO SECONDS
      ! ***  TBEGIN:  TIME ORIGIN OF RUN
      ! ***  TIDALP:  REFERENCE TIME PERIOD IN SEC (IE 44714.16S OR 86400S)
      TPN=FLOAT(NTSPTC)  
      NTS=NTC*NTSPTC/NFLTMT 
      NLTS=NTSPTC*NLTC           ! *** # Transition Step to Completely linear
      NTTS=NTSPTC*NTTC           ! *** # Transition Step to Fully Non-linear
      NTTS=NTTS+NLTS             ! *** Total # of Steps to End of Transition
      SNLT=0.  
      NCTBC=1  
      NPRINT=1  
      NTSPP=NTCPP*NTSPTC/NFLTMT  
      !NTSNB=NTCNB*NTSPTC         ! *** No Bouyancy (Not used)
      NTSVB=NTCVB*NTSPTC         ! *** Variable Bouyancy
      ITRMAX=0  
      ITRMIN=1000  
      ERRMAX=1E-9  
      ERRMIN=1000.  
      NMMT=1  
      NBAL=1  
      NBALE=1  
      NBALO=1  
      NBUD=1  
      NHAR=1  
      NTSPTC2=2*NTSPTC/NFLTMT  
      NDISP=NTS-NTSPTC+2  
      NSHOWR=0  
      NSHOWC=0  
      DO NS=1,NASER  
        MATLAST(NS)=1  
      ENDDO  
      DO NS=1,NWSER  
        MWTLAST(NS)=1  
      ENDDO  
      DO NS=1,NPSER  
        MPTLAST(NS)=1  
      ENDDO  
      DO NS=1,NQSER  
        MQTLAST(NS)=1  
      ENDDO  
      DO NS=1,NQWRSR  
        MQWRTLST(NS)=1  
      ENDDO  
      NTMP=4+NSED+NSND+NTOX  
      DO NC=1,NTMP  
        DO NN=1,NCSER(NC)  
          MCTLAST(NN,NC)=1  
        ENDDO  
      ENDDO  
! { GEOSR JGCHO 20111.10.27
      DO NS=1,NQCTL  
        MTIDELAST(NS)=1  
        MGTLAST(NS)=1  
      ENDDO  
! } GEOSR JGCHO 20111.10.27
      MSFTLST=1  
C  
C **  SET CONTROLS FOR WRITING TO INSTANTANEOUS 2D SCALAR CONTOURING  
C **  AND 2D VELOCITY VECTOR PLOTTING FILES  
C **  SCALAR FIELD CONTOURING IN HORIZONTAL PLANES: SUBROUTINE SALPLTH  
C  
      DO N=1,7  
        IF(ISSPH(N).EQ.0)THEN  
          NCSPH(N)=0  
          JSSPH(N)=0  
        ENDIF  
        IF(ISSPH(N).EQ.2)THEN  
          NCSPH(N)=NTS-(NTSPTC-(NTSPTC/NPSPH(N)))/NFLTMT  
          JSSPH(N)=1  
        ENDIF  
        IF(ISSPH(N).EQ.1)THEN  
          NCSPH(N)=NTSPTC/NPSPH(N)/NFLTMT  
          JSSPH(N)=1  
        ENDIF  
      ENDDO  
      IF(ISSPH(8).EQ.1)NCSPH(8)=NTSPTC/NPSPH(8)/NFLTMT  
C  
C **  SEDIMENT BED PROPERTIES CONTOURING IN HORIZONTAL  
C **  PLANES: SUBROUTINE SBEDPLT  
C  
      IF(ISBPH.EQ.0)THEN  
        NCBPH=0  
        JSBPH=0  
      ENDIF  
      IF(ISBPH.EQ.2)THEN  
        NCBPH=NTS-(NTSPTC-(NTSPTC/NPBPH))/NFLTMT  
        JSBPH=1  
      ENDIF  
      IF(ISBPH.EQ.1)THEN  
        NCBPH=NTSPTC/NPBPH/NFLTMT  
        JSBPH=1  
      ENDIF  
C  
C **  FREE SURFACE ELEVATION OR PRESSURE CONTOURING IN HORIZONTAL  
C **  PLANES: SUBROUTINE SURFPLT  
C  
      ! *** DSLLC BEGIN BLOCK
      NSNAPSHOTS=0  
      NSHOTS=0

      ! ***  TCON:    CONVERSION MULTIPLIER TO CHANGE TBEGIN TO SECONDS
      ! ***  TBEGIN:  TIME ORIGIN OF RUN
      ! ***  TIDALP:  REFERENCE TIME PERIOD IN SEC (IE 44714.16S OR 86400S)
      IF(ISPPH.EQ.0)THEN  
        NCPPH=0  
        JSPPH=0
      ELSEIF(ISPPH.EQ.2)THEN  
        NCPPH=NTS-(NTSPTC-(NTSPTC/NPPPH))/NFLTMT  
        JSPPH=1  
        NSNAPSHOTS=2
        SNAPSHOTS(1)=(TBEGIN*TCON+TIDALP*NTC)/86400.-0.001
        SNAPSHOTS(2)=SNAPSHOTS(1)+0.001
      ELSE   ! *** IF(ISPPH.EQ.1)THEN  
        NCPPH=NTSPTC/NPPPH/NFLTMT  
        JSPPH=1

        DELSNAP=TIDALP/NPPPH/86400.
        T1=TBEGIN*TCON/86400.
        T2=(TBEGIN*TCON+TIDALP*NTC)/86400.
        T0=T1
        T9=T2
        NN=(T2-T1)/DELSNAP+2

        ! *** HIGH FREQUENCY SNAPSHOTS
        IF(ISPPH.EQ.100)THEN
          PRINT *, 'HIGH FREQ SNAPSHOTS USED'
          NS=0
          OPEN(1,FILE='SNAPSHOTS.INP',STATUS='UNKNOWN',ERR=999)  
          DO NS=1,4  
            READ(1,1111)  
          ENDDO  
          READ(1,*)NSHOTS
          ALLOCATE(SHOTS(NSHOTS+1,3))  
          DO NS=1,NSHOTS
            READ(1,*)(SHOTS(NS,K),K=1,3)
            SHOTS(NS,3)=SHOTS(NS,3)/24.
            SHOTS(NS,2)=SHOTS(NS,2)/24.
            SHOTS(NS,1)=SHOTS(NS,1)-SHOTS(NS,2)/2.
          ENDDO
  999     NSHOTS=NS-1
          IF(NSHOTS.LT.0)NSHOTS=0
          CLOSE(1)

          SHOTS(NSHOTS+1,1)=T2

          DO NS=1,NSHOTS
            NN = NN+SHOTS(NS,2)/SHOTS(NS,3) + 1
          ENDDO
        ENDIF

        ! *** Reallocate the SNAPSHOTS array
        DEALLOCATE(SNAPSHOTS)
        ALLOCATE(SNAPSHOTS(NN))

        ! *** BUILD THE SNAPSHOT DATES
        ISNAP=1
        IF(NSHOTS.GT.0)THEN
          T4=SHOTS(ISNAP,1)+SHOTS(ISNAP,2)  ! *** ENDING TIME FOR HIGH FREQ
          DO WHILE (T4.LT.T0)
            ISNAP=ISNAP+1
            IF(ISNAP.GT.NSHOTS)THEN
              NSHOTS=0
              EXIT
            ENDIF
            T4=SHOTS(ISNAP,1)+SHOTS(ISNAP,2) 
          ENDDO
        ENDIF

        DO WHILE (T1.LT.T2)
          T1=T1+DELSNAP
          IF(ISNAP.LE.NSHOTS)THEN
            IF(T1.GT.SHOTS(ISNAP,1))THEN
              ! *** ENSURE NO OVERLAPPING PERIODS
              T3=SHOTS(ISNAP,1)
              IF(NSNAPSHOTS.GE.1)THEN
                DO WHILE (T3.LT.SNAPSHOTS(NSNAPSHOTS))
                  T3=T3+SHOTS(ISNAP,3)
                ENDDO
              ENDIF

              T4=SHOTS(ISNAP,1)+SHOTS(ISNAP,2)  ! *** ENDING TIME FOR HIGH FREQ
              IF(T4.GT.T0.AND.T3.LT.T9)THEN
                ! *** VALID PERIOD, SO BUILD HF SNAPSHOTS               

                ! *** CHECK 1ST AND LAST HF PERIOD
                IF(T4.GT.T9)T4=T9
                DO WHILE (T3.LE.T0)
                  T3=T3+SHOTS(ISNAP,3)
                ENDDO

                ! *** BUILD HF SNAPSHOTS
                DO WHILE (T3.LE.T4)
                  NSNAPSHOTS=NSNAPSHOTS+1
                  SNAPSHOTS(NSNAPSHOTS)=T3
                  T3=T3+SHOTS(ISNAP,3)
                ENDDO
              ENDIF
              ISNAP=ISNAP+1  ! *** INCREMENT TO THE NEXT PERIOD

              ! *** Synch up the regular intervals
              IF(NSNAPSHOTS.GT.0)THEN
                DO WHILE (T1.LT.SNAPSHOTS(NSNAPSHOTS))
                  T1=T1+DELSNAP
                ENDDO
              ENDIF

              IF(T1.LT.SHOTS(ISNAP,1).AND.T1.GE.T0.AND.T1.LE.T9)THEN
                NSNAPSHOTS=NSNAPSHOTS+1
                SNAPSHOTS(NSNAPSHOTS)=T1
              ENDIF
            ELSEIF(T1.GE.T0.AND.T1.LE.T9)THEN
              NSNAPSHOTS=NSNAPSHOTS+1
              SNAPSHOTS(NSNAPSHOTS)=T1
            ENDIF
          ELSEIF(T1.GE.T0.AND.T1.LE.T9)THEN
            NSNAPSHOTS=NSNAPSHOTS+1
            SNAPSHOTS(NSNAPSHOTS)=T1
          ENDIF  
        ENDDO
      ENDIF
      PRINT *, 'NSNAPSHOTS=',  NSNAPSHOTS
      WRITE(7,*)'NSNAPSHOTS=',  NSNAPSHOTS 
      DO I=1,NSNAPSHOTS
        WRITE(7,*)'SNAPSHOT: ',I,SNAPSHOTS(I)
      ENDDO
      IF(NSNAPSHOTS.GT.2)THEN
        IF(SNAPSHOTS(NSNAPSHOTS).LT.T2)SNAPSHOTS(NSNAPSHOTS+1)=T2+.001
      ENDIF

      NSNAPSHOTS=1
      ! *** DSLLC END BLOCK
C  
C **  VELOCITY VECTOR PLOTTING IN HORIZONTAL PLANES: SUBROUTINE VELPLTH  
C  
      IF(ISVPH.EQ.0)THEN  
        NCVPH=0  
        JSVPH=0  
      ENDIF  
      IF(ISVPH.EQ.2)THEN  
        NCVPH=NTS-(NTSPTC-(NTSPTC/NPVPH))/NFLTMT  
        JSVPH=1  
      ENDIF  
      IF(ISVPH.EQ.1)THEN  
        NCVPH=NTSPTC/NPVPH/NFLTMT  
        JSVPH=1  
      ENDIF  
C  
C **  SCALAR FIELD CONTOURING IN VERTICAL PLANES: SUBROUTINE SALPLTV  
C  
      DO N=1,7  
        IF(ISSPV(N).EQ.0)THEN  
          NCSPV(N)=0  
          JSSPV(N)=0  
        ENDIF  
        IF(ISSPV(N).EQ.2)THEN  
          NCSPV(N)=NTS-(NTSPTC-(NTSPTC/NPSPV(N)))/NFLTMT  
          JSSPV(N)=1  
          DO IS=1,ISECSPV  
            CCTITLE(20+IS)=CCTITLE(10+IS)  
            CCTITLE(30+IS)=CCTITLE(10+IS)  
            CCTITLE(40+IS)=CCTITLE(10+IS)  
            CCTITLE(50+IS)=CCTITLE(10+IS)  
          ENDDO  
        ENDIF  
        IF(ISSPV(N).EQ.1)THEN  
          NCSPV(N)=NTSPTC/NPSPV(N)/NFLTMT  
          JSSPV(N)=1  
          DO IS=1,ISECSPV  
            CCTITLE(20+IS)=CCTITLE(10+IS)  
            CCTITLE(30+IS)=CCTITLE(10+IS)  
            CCTITLE(40+IS)=CCTITLE(10+IS)  
            CCTITLE(50+IS)=CCTITLE(10+IS)  
          ENDDO  
        ENDIF  
      ENDDO  
C  
C **  NORMAL VELOCITY CONTOURING AND TANGENTIAL VELOCITY VECTOR  
C **  PLOTTING IN VERTICAL PALNES: SUBROUTINE VELPLTV  
C  
      IF(ISVPV.EQ.0)THEN  
        NCVPV=0  
        JSVPV=0  
      ENDIF  
      IF(ISVPV.EQ.2)THEN  
        NCVPV=NTS-(NTSPTC-(NTSPTC/NPVPV))/NFLTMT  
        JSVPV=1  
        DO IS=1,ISECVPV  
          CVTITLE(20+IS)=CVTITLE(10+IS)  
          CVTITLE(30+IS)=CVTITLE(10+IS)  
          CVTITLE(40+IS)=CVTITLE(10+IS)  
          CVTITLE(50+IS)=CVTITLE(10+IS)  
          CVTITLE(60+IS)=CVTITLE(10+IS)  
          CVTITLE(70+IS)=CVTITLE(10+IS)  
          CVTITLE(80+IS)=CVTITLE(10+IS)  
          CVTITLE(90+IS)=CVTITLE(10+IS)  
        ENDDO  
      ENDIF  
      IF(ISVPV.EQ.1)THEN  
        NCVPV=NTSPTC/NPVPV/NFLTMT  
        JSVPV=1  
        DO IS=1,ISECVPV  
          CVTITLE(20+IS)=CVTITLE(10+IS)  
          CVTITLE(30+IS)=CVTITLE(10+IS)  
          CVTITLE(40+IS)=CVTITLE(10+IS)  
          CVTITLE(50+IS)=CVTITLE(10+IS)  
          CVTITLE(60+IS)=CVTITLE(10+IS)  
          CVTITLE(70+IS)=CVTITLE(10+IS)  
          CVTITLE(80+IS)=CVTITLE(10+IS)  
          CVTITLE(90+IS)=CVTITLE(10+IS)  
        ENDDO  
      ELSE  
        NCVPV=0  
        JSVPV=0  
      ENDIF  
C  
C **  THREE-DIMENSIONAL HDF FORMAT GRAPHICS FILES: SUBROUTINE OUT3D  
C  
      IF(IS3DO.EQ.1)THEN  
        NC3DO=NTS-(NTSPTC-(NTSPTC/NP3DO))/NFLTMT  
      ENDIF  
C  
C **  SET CONTROLS FOR WRITING TO FILTERED, AVERAGED OR RESIDUAL  
C **  2D SCALAR CONTOURING AND 2D VELOCITY VECTOR PLOTTING FILES  
C **  RESIDUAL SALINITY, TEMPERATURE, DYE AND SEDIMENT CONCENTRATION  
C **  CONTOURING IN HORIZONTAL: SUBROUTINE RSALPLTH  
C  
      DO N=1,7  
        IF(ISRSPH(N).GE.1) JSRSPH(N)=1  
      ENDDO  
C  
C **  RESIDUAL VELOCITY VECTOR PLOTTING IN HORIZONTAL PLANES:  
C **  SUBROUTINE RVELPLTH  
C  
      IF(ISRVPH.GE.1) JSRVPH=1  
C  
C **  RESIDUAL SURFACE ELEVATION PLOTTING IN HORIZONTAL PLANES:  
C **  SUBROUTINE RVELPLTH  
C  
      IF(ISRPPH.GE.1) JSRPPH=1  
C  
C **  RESIDUAL SCALAR FIELD CONTOURING IN VERTICAL  
C **  PLANES: SUBROUTINE RSALPLTV  
C  
      DO N=1,7  
        IF(ISRSPV(N).GE.1) JSRSPV(N)=1  
      ENDDO  
C  
C **  RESIDUAL NORMAL AND TANGENTIAL VELOCITY CONTOURING AND AND  
C **  TANGENTIAL VELOCITY VECTOR PLOTTING IN VERTICAL PLANES:  
C **  SUBROUTINE RVELPLTV  
C  
      IF(ISRVPV.GE.1) JSRVPV=1  
C  
C **  SET CONTROLS FOR WRITING TO DRIFTER, HARMONIC ANALYSIS,  
C **  RESIDUAL TRANSPORT, AND BLANCE OUTPUT FILES  
C  
      JSPD=1  
      NCPD=1  
      JSLSHA=1  
      IF(ISLSHA.EQ.1)THEN  
        LSLSHA=0  
        NCLSHA=NTS-NTCLSHA*NTSPTC  
      ENDIF  
      IF(ISRESTR.EQ.1) JSRESTR=1  
      JSWASP=0  
      IF(ISWASP.GE.1) JSWASP=1  
      IF(ISBAL.GE.1)THEN  
        JSBAL=1  
        JSBALO=1  
        JSBALE=1  
      ENDIF  
      JSSBAL=1  
C  
C **  SET CONTROL FOR CALCULATION OF LAGRANGIAN MEAN VELOCITIY FIELDS  
C **  BY PARTICLE TRACKING  
C  
      IF(ISLRPD.GE.1)THEN  
        NLRPDRT(1)=NTS-NTSPTC-(NTSPTC-(NTSPTC/MLRPDRT))/NFLTMT  
        DO M=2,MLRPDRT  
          NLRPDRT(M)=NLRPDRT(M-1)+(NTSPTC/MLRPDRT)  
        ENDDO  
        JSLRPD=1  
      ELSE  
        NLRPDRT(1)=NTS+2  
        JSLRPD=0  
      ENDIF  
C  
C **  SET SOME CONSTANTS  
C  
      JSTBXY=0  
      CTURB2=CTURB**0.667  
      CTURB3=CTURB**0.333  
      KS=KC-1  
      IF(KS.EQ.0) KS=1  
      DZI=FLOAT(KC)  
      DZ=1./DZI  
      DZS=DZ*DZ  
      DT=TIDALP*FLOAT(NFLTMT)/FLOAT(NTSPTC)  
      DTI=1./DT  
      DT2=2.*DT  
      DTMIN=DT  
      AVCON1=2.*(1.-AVCON)*DZI*AVO  
      G=9.81  
      GPO=G*BSC  
      GI=1./G  
      GID2=.5*GI  
      PI=3.1415926535898  
      PI2=2.*PI  
      TCVP=0.0625*TIDALP/PI  
C  
C **  SET CONSTANTS FOR M2 TIDAL CYCLE HARMONIC ANALYSIS  
C  
      IF(ISHTA.GT.0)THEN  
        AC=0.  
        AS=0.  
        ACS=0.  
        TSHIFT=(TBEGIN*TCON/DT)+FLOAT(NTC-2)*NTSPTC  
        DO N=1,NTSPTC  
          TNT=FLOAT(N)+TSHIFT  
          NP=NTSPTC+N  
          WC(N)=COS(2.*PI*TNT/TPN)  
          WS(N)=SIN(2.*PI*TNT/TPN)  
          WC(NP)=WC(N)  
          WS(NP)=WS(N)  
          AC=AC + 2.*WC(N)*WC(N)  
          AS=AS + 2.*WS(N)*WS(N)  
          ACS=0.  
          WC2(N)=COS(4.*PI*TNT/TPN)  
          WS2(N)=SIN(4.*PI*TNT/TPN)  
          WC2(NP)=WC2(N)  
          WS2(NP)=WC2(N)  
          AC2=AC2 + 2.*WC2(N)*WC2(N)  
          AS2=AS2 + 2.*WS2(N)*WS2(N)  
          ACS2=0.  
        ENDDO  
        DET=AC*AS-ACS*ACS  
        AS=AS/DET  
        AC=AC/DET  
        ACS=ACS/DET  
        DET=AC2*AS2-ACS2*ACS2  
        AS2=AS2/DET  
        AC2=AC2/DET  
        ACS2=ACS2/DET  
      ENDIF  
C  
C **  SET WEIGHTS FOR SALINITY AND TEMPERATURE BOUNDARY INTERPOLATION  
C  
      IF(KC.GT.1)THEN  
        DO K=1,KC  
          WTCI(K,1)=FLOAT(K-KC)/FLOAT(1-KC)  
          WTCI(K,2)=FLOAT(K-1)/FLOAT(KC-1)  
        ENDDO  
      ELSE  
        WTCI(1,1)=0.5  
        WTCI(1,2)=0.5  
      ENDIF  
C  
C **  INITIALIZE ARRAYS  
C  
      CALL AINIT  
C  
C **  READ IN XLON AND YLAT OR UTME AND UTMN OF CELL CENTERS OF  
C **  CURVILINEAR PORTION OF THE  GRID  
C  
      IF(ISCLO.EQ.1)THEN  
        OPEN(1,FILE='LXLY.INP',STATUS='UNKNOWN')  
        DO NS=1,4  
          READ(1,1111)  
        ENDDO  
 1111 FORMAT(80X)  
        IF(ISCORV.EQ.1)THEN  
          DO LL=1,LVC  
            READ(1,*,ERR=3000)I,J,XLNUTME,YLTUTMN,CCUE,CCVE,CCUN,
     &                   CCVN,TMPVAL,TMPCOR  
            L=LIJ(I,J)  
            DLON(L)=XLNUTME  
            DLAT(L)=YLTUTMN  
            ANG1=ATAN2(CCUN,CCUE)  
            ANG2=ATAN2(-CCVE,CCVN)  
            ANG=0.5*(ANG1+ANG2)  
            IF(SIGN(1.,ANG1).NE.SIGN(1.,ANG2))THEN
               IF(ABS(ANG1).GT.(1.57).OR.ABS(ANG2).GT.(1.57)) THEN
                 ! *** HANDLE THE DISCONTINUITY AT THE 180 DEGREES ANGLE
                 ANG = ANG + acos(-1.0) !better way to do Pi, suggested by SCJ
              ENDIF
            END IF
            CUE(L)=COS(ANG)  
            CVE(L)=-SIN(ANG)  
            CUN(L)=SIN(ANG)  
            CVN(L)=COS(ANG)  
            WINDSTKA(L)=TMPVAL  
            FCORC(L)=TMPCOR  
            DETTMP=1./( CUE(L)*CVN(L)-CUN(L)*CVE(L) )  
            IF(DETTMP.EQ.0.0)THEN  
              WRITE(6,6262)  
              WRITE(6,6263)IL(L),JL(L)  
              STOP  
            ENDIF  
          ENDDO  
        ELSE  
          DO LL=1,LVC  
            READ(1,*,ERR=3000)I,J,XLNUTME,YLTUTMN,CCUE,CCVE,CCUN,
     &          CCVN,TMPVAL  
            L=LIJ(I,J)  
            DLON(L)=XLNUTME  
            DLAT(L)=YLTUTMN  
            ANG1=ATAN2(CCUN,CCUE)  
            ANG2=ATAN2(-CCVE,CCVN)  
            ANG=0.5*(ANG1+ANG2)  
            TA1 = SIGN(1.,ANG1)
            TA2 = SIGN(1.,ANG2)
            IF(SIGN(1.,ANG1).NE.SIGN(1.,ANG2))THEN
               IF(ABS(ANG1).GT.(1.57).OR.ABS(ANG2).GT.(1.57)) THEN
                 ! *** HANDLE THE DISCONTINUITY AT THE 180 DEGREES ANGLE
                 ANG = ANG + acos(-1.0) !better way to do Pi, suggested by SCJ
              ENDIF
            END IF
            CUE(L)=COS(ANG)  
            CVE(L)=-SIN(ANG)  
            CUN(L)=SIN(ANG)  
            CVN(L)=COS(ANG)  
            WINDSTKA(L)=TMPVAL  
            FCORC(L)=CF  
            DETTMP=1./( CUE(L)*CVN(L)-CUN(L)*CVE(L) )  
            IF(DETTMP.EQ.0.0)THEN  
              WRITE(6,6262)  
              WRITE(6,6263)IL(L),JL(L)  
              STOP  
            ENDIF  
          ENDDO  
        ENDIF  
 6262 FORMAT('  SINGULAR INVERSE TRANSFORM FROM E,N TO CURV X,Y')  
 6263 FORMAT('  I,J =',2I10/)  
        CLOSE(1)  
      ENDIF
C *** DSLLC BEGIN BLOCK
      ISCURVATURE=.FALSE.
      FORCSUM=0.
      DO L=2,LA
        FORCSUM=FORCSUM+FCORC(L)
      ENDDO
      IF(FORCSUM.GT.1.0E-6)ISCURVATURE=.TRUE.
C *** DSLLC END BLOCK
      FCORC(1)=FCORC(2)  
      FCORC(LC)=FCORC(LA)  
      GOTO 3002  
 3000 WRITE(6,3001)  
 3001 FORMAT('  READ ERROR FOR FILE LXLY.INP ')  
      STOP  
 3002 CONTINUE  

      ZERO=0.  
      IF(DEBUG)THEN
        OPEN(1,FILE='LIJMAP.OUT',STATUS='UNKNOWN')  
        DO L=2,LA  
          WRITE(1,1113)L,IL(L),JL(L),ZERO  
        ENDDO  
        CLOSE(1)  
      ENDIF
 1112 FORMAT (2I5,2F12.4,6F12.7)  
 1113 FORMAT (3I5,F10.2)  

!{GeoSR, TOXIC, YSSONG, 101030, 101125
C
C **  WIMS COMMUNICATION
C

	  IF(IDTOX.GT.0.AND.IDTOX.LT.4440)THEN  ! ONLY FOR TOXIC MODULE (CWCHO)	  
        CALL READWIMS2
        NQSERQ(NQSIJ)=NQSER
        IQS(NQSIJ)=ITX
	  JQS(NQSIJ)=JTX
	  ENDIF
! GeoSR}


C
C **  SET CORNER CELL STRESS CORRECTION
C
      DO L=2,LA
	  FSCORTBCV(L)=0.0
	ENDDO
C
      IF(ISCORTBC.GE.1)THEN
        DO L=2,LA
	    FSCORTBCV(L)=FSCORTBC
	  ENDDO
	ENDIF
C
      IF(ISCORTBC.EQ.2.AND.DEBUG)THEN
        OPEN(1,FILE='CORNERC.INP')
        DO NS=1,4
          READ(1,1111)
        ENDDO
        READ(1,*)NTMP
	  DO NT=1,NTMP
	    READ(1,*)I,J,TMPVAL
	    L=LIJ(I,J)
	    FSCORTBCV(L)=TMPVAL
	  ENDDO
        CLOSE(1)
      ENDIF
C            
C **  READ SPATIAL AVERAGING MAP FOR FOOD CHAIN MODEL OUTPUT
C
      IF(ISFDCH.EQ.1)THEN 
	  DO L=1,LC
	    MFDCHZ(L)=0
	  ENDDO
        OPEN(1,FILE='FOODCHAIN.INP')
        DO NS=1,4
          READ(1,1111)
        ENDDO
        READ(1,*)NFDCHIJ
	  DO LT=1,NFDCHIJ
	    READ(1,*)I,J,ITMPVAL
	    L=LIJ(I,J)
	    MFDCHZ(L)=ITMPVAL
	  ENDDO
        CLOSE(1)
      ENDIF
C  
C **  READ IN COUNTER CLOCKWISE ANGLE FROM EAST SPECIFYING  
C **  PRINCIPAL FLOOD FLOW DIRECTION  
C  
      IF(ISTRAN(4).GE.1.AND.ISSFLFE.GE.1)THEN  
        OPEN(1,FILE='FLDANG.INP',STATUS='UNKNOWN')  
        DO LL=2,LA  
          READ(1,*,ERR=3130)I,J,ANGTMP1,ANGTMP2  
          L=LIJ(I,J)  
          ACCWFLD(L,1)=0.0174533*ANGTMP1  
          ACCWFLD(L,2)=0.0174533*ANGTMP2  
        ENDDO  
        CLOSE(1)  
      ENDIF  
      GOTO 3132  
 3130 WRITE(6,3131)  
 3131 FORMAT('  READ ERROR FOR FILE FLDANG.INP ')  
      STOP  
 3132 CONTINUE  
C  
C **  SET BOUNDARY CONDITION SWITCHES  
C  
      CALL SETBCS  
C  
C **  CALCUATE CURVATURE METRICS (NEW ADDITION)  
C  
      DO L=1,LC  
        DYDI(L)=0.  
        DXDJ(L)=0.  
      ENDDO  
C  
C ** DYDI  
C  
      TMPVAL=0.
      DO L=2,LA  
        I=IL(L)  
        J=JL(L)
        ! *** DSLLC - CHANGED CELL TYPE 5 TO 8  
        IF(IJCT(I-1,J).GE.1.AND.IJCT(I-1,J).LE.8)THEN  
          IF(IJCT(I+1,J).GE.1.AND.IJCT(I+1,J).LE.8)THEN  
            DYDI(L)=DYU(L+1)-DYU(L)  
          ELSE  
            DDYDDDX=2.*(DYP(L)-DYP(L-1))/(DXP(L)+DXP(L-1))  
            DYUP1=DYP(L)+0.5*DDYDDDX*DXP(L)  
            DYDI(L)=DYUP1-DYU(L)  
          END IF  
        ELSE  
          IF(IJCT(I+1,J).GE.1.AND.IJCT(I+1,J).LE.8)THEN  
            DDYDDDX=2.*(DYP(L+1)-DYP(L))/(DXP(L+1)+DXP(L))  
            DYUM1=DYP(L)-0.5*DDYDDDX*DXP(L)  
            DYDI(L)=DYU(L)-DYUM1  
          ELSE  
            DYDI(L)=0.0  
          END IF  
        END IF
        IF(DYDI(L).GT.1.E-7)THEN
          TMPVAL=1.
        ENDIF
      ENDDO  
C  
C ** DXDJ  
C  
      DO L=2,LA  
        LN=LNC(L)  
        LS=LSC(L)  
        I=IL(L)  
        J=JL(L)  
        ! *** DSLLC - CHANGED CELL TYPE 5 TO 8  
        IF(IJCT(I,J-1).GE.1.AND.IJCT(I,J-1).LE.8)THEN  
          IF(IJCT(I,J+1).GE.1.AND.IJCT(I,J+1).LE.8)THEN  
            DXDJ(L)=DXV(LN)-DXV(L)  
          ELSE  
            DDXDDDY=2.*(DXP(L)-DXP(LS))/(DYP(L)+DYP(LS))  
            DXVLN=DXP(L)+0.5*DDXDDDY*DYP(L)  
            DXDJ(L)=DXVLN-DXV(L)  
          END IF  
        ELSE  
          IF(IJCT(I,J+1).GE.1.AND.IJCT(I,J+1).LE.8)THEN  
            DDXDDDY=2.*(DXP(LN)-DXP(L))/(DYP(LN)+DYP(L))  
            DXVLS=DXP(L)-0.5*DDXDDDY*DYP(L)  
            DXDJ(L)=DXV(L)-DXVLS  
          ELSE  
            DXDJ(L)=0.0  
          END IF  
        END IF  
        IF(DXDJ(L).GT.1.E-7)THEN
          TMPVAL=1.
        ENDIF
      ENDDO  
      IF(TMPVAL.GT.0.5)ISCURVATURE=.TRUE.




























































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































      end subroutine model_init_2
