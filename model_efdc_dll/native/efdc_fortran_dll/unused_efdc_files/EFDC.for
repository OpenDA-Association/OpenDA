C  
C **********************************************************************  
C  
      PROGRAM EFDC  
C  
C **  WELCOME TO THE ENVIRONMENTAL FLUID DYNAMICS COMPUTER CODE SERIES  
C **  DEVELOPED BY JOHN M. HAMRICK.  THE EFDC CODE WAS ORGINALLY  
C **  DEVELOPED AT VIRGINIA INSTITUTE OF MARINE SCIENCE  
C **  /SCHOOL OF MARINE SCIENCE, THE COLLEGE OF  
C **  WILLIAM AND MARY, GLOUCESTER POINT, VA 23062  
C **  THIS SOURCE FILE IS A DIRECT RELEASE BY THE DEVELOPER  
C **  AND DIFFERS SIGNIFICANTLY FROM PRE 1 MARCH 96 VIMS RELEASES OF  
C **  EFDC AND POST 1 MARCH 96 VIMS RELEASES OF HEM3D (THE NAMED  
C **  CURRENTLY USED BY VIMS FOR THE VERSION OF EFDC EXISTING AT  
C **  THE TIME OF MY DEPARTURE) WITH RESPECT TO ERROR FIXES AND  
C **  APPLICATION CAPABILITIES  
C **  EFDC IS CURRENTLY MAINTAINED BY TETRA TECH, INC., WITH PRIMARY  
C **  SUPPORT FORM THE US ENVIRONMENTAL PROTECTION AGENCY  
C **  ENVIRONMENTAL FLUID DYNAMICS CODE AND EFDC ARE  
C **  TRADEMARKS OF JOHN M. HAMRICK, PH.D., P.E.  
C **  EFDC SOLVES THE 3D REYNOLDS AVERAGED NAVIER-STOKES  
C **  EQUATIONS (WITH HYDROSTATIC AND BOUSINESSQ APPROXIMATIONS) AND  
C **  TRANSPORT EQUATIONS FOR TURBULENT INTENSITY, TURBULENT  
C **  INTENSITYXLENGHT SCALE, SALINITY (OR WATER VAPOR CONTENT),  
C **  TEMPERATURE, AN INERT TRACER (CALLED DYE), A DYNAMICALLY ACTIVE  
C **  SUSPENDED SETTLING PARTICLE FIELD (CALLED SEDIMENT).  A FREE  
C **  SURFACE OR RIGID LID IS PRESENT ON THE VERTICAL BOUNDARY Z=1  
C **  IN THE SIGMA STRETCHED VERTICAL COORDINATE.  THE HORIZONTAL  
C **  COORDINATE SYSTEM IS CURVILINEAR AND ORTHOGONAL.  
C **  THE NUMERICAL SOLUTION SCHEME IS ON A SPATIALLY STAGGERED MAC  
C **  OR C GRID AND THE TIME INTEGRATION USES A THREE TIME LEVEL  
C **  LEAPFROG INTEGRATION WITH PERIODIC TRAPEZOIDAL CORRECTIONS TO  
C **  SUPPRESS THE COMPUTATIONAL MODE AND REDUCE NOISE.  
C **  SPATIAL SOLUTION OF THE EXTERNAL MODE FOR THE FREE SURFACE  
C **  ELEVATION OR KINEMATIC PRESSURE UNDER THE RIGID LID IS BY  
C **  RED-BLACK SUCCESSIVE OVER RELAXATION (RB SOR) OR CONJUGATE  
C **  GRADIENT SOLUTION OF A PSEUDO-HEMHOLTZ EQUATION.  THE INTERNAL  
C **  SOLUTION IS IMPLICIT FOR THE VERTICAL SHEAR OR VELOCITY STRUCTURE.  
C **  A NUMBER OF OPTIONS ARE AVAILABLE FOR REPRESENTING THE ADVECTIVE  
C **  TRANSPORT TERMS IN THE MOMENTUM AND SCALAR TRANSPORT EQUATIONS.  
C **  PRIMARY DOCUMENTATION INCLUDES:  
C     HAMRICK, J. M., 1992A:  A THREE-DIMENSIONAL ENVIRONMENTAL  
C     FLUID DYNAMICS COMPUTER CODE: THEORETICAL AND COMPUTATIONAL  
C     ASPECTS. THE COLLEGE OF WILLIAM AND MARY, VIRGINIA INSTITUTE  
C     OF MARINE SCIENCE, SPECIAL REPORT 317, 63 PP.  
C     HAMRICK, J. M., 1996A:  USERS MANUAL FOR THE ENVIRONMENTAL  
C     FLUID DYNAMIC COMPUTER CODE. THE COLLEGE OF WILLIAM AND MARY,  
C     VIRGINIA INSTITUTE OF MARINE SCIENCE, SPECIAL REPORT 328, 224 PP.  
C     PARK, K., A. Y. KUO, J. SHEN, AND J. M. HAMRICK, 1995:  
C     A THREE-DIMENSIONAL HYDRODYNAMIC-EUTROPHICATION MODEL (HEM3D):  
C     DESCRIPTION OF WATER QUALITY AND SEDIMENT PROCESSES SUBMODELS.  
C     THE COLLEGE OF WILLIAM AND MARY, VIRGINIA INSTITUTE OF MARINE  
C     SCIENCE. SPECIAL REPORT 327, 113 PP.  
C     TETRA TECH, INC., 1999B: THEORETICAL AND COMPUTATIONAL ASPECTS  
C     OF SEDIMENT AND CONTAMINANT TRANSPORT IN THE EFDC MODEL.  
C     A REPORT TO THE U.S. ENVIRONMENTAL PROTECTION AGENCY,  
C     TETRA TECH, INC., FAIRFAX, VA.  
C **  ADDITIONAL REFERENCES TO MODEL APPLICATIONS ARE AVAILABLE  
C **  FROM THE DEVELOPER  
C **  CHANGES MADE TO THIS CODE BY UNAUTHORIZED PERSONS WILL BE  
C **  SUPPORTED ON A COST REIMBURSED BASIS ONLY.  SUPPORT IS AVAILABLE  
C **  FROM JOHN M. HAMRICK, 2520 WEST WHITTAKER CLOSE  
C **  WILLIAMSBURG, VA, TEL. 804-258-0608, FAX 804-258-9698  
C **  EMAIL: HAM@VISI.NET  
C **  THE AUTHOR AND TETRA TECH, INC.  ASSUME NO LIABILITY FOR USE  
C **  OF THIS CODE FOR ENVIRONMENTAL AND ENGINEERING STUDIES.  
C **  
C **  THIS CODE HAS BEEN COMPLIED ON A WINDOWS XP SYSTEM USING         PMC
C **  THE COMPAQ FORTRAN 6.5 AND INTEL'S FORTRAN COMPILER              PMC
C **  THIS VERSION PROVIDES AUTOMATIC MEMORY ALLOCATION USING F90'S    PMC
C **  DYNAMIC MEMORY ALLOCATION FUNCTIONS                              PMC
C **
C **  THE FOLLOWING FILES MAY BE NECESSARY TO RUN THIS CODE:  
C **      EFDC.INP  
C **      CELL.INP  
C **      CELLLT.INP  LONG TERM OPTION DISABLED SO NO LONGER NEEDED    PMC
C **      DXDY.INP  
C **      LXLY.INP  
C **      SALT.INP OR RESTART.INP OR RESTRAN.INP  
C **      TEMP.INP OR RESTART.INP OR RESTRAN.INP  
C **      ASER.INP  
C **      WSER.INP  
C **      QSER.INP  
C **      PSER.INP  
C **      SSER.INP  
C **      TSER.INP  
C **      DSER.INP  
C **      SDSER.INP  
C **      SNSER.INP  
C **      TXSER.INP  
C **      SFSER.INP  
C **      TXSER.INP  
C **      QCTL.INP  
C **      MASK.INP  
C **      SHOW.INP  
C **      VEGE.INP  
C **      MODDXDY.INP  
C **      MODCHAN.INP  
C **      GWATER.INP  
C  
C *** EE BEGIN BLOCK  
      USE GLOBAL
C 
C *** WASP7 Linkage     
CPMC      COMMON/WASPHYDRO/  IHL_HANDLE,ibegin,idays,ad,adcoeff,abwmax,abwmx
CPMC	INCLUDE 'hydrolink_set.INT'
CPMC      CHARACTER*256 errstring
C
      CHARACTER*80 TITLE  
C
      REAL,ALLOCATABLE,DIMENSION(:,:)::SHOTS
C  
C *** EE BEGIN BLOCK  
C  
      REAL*4 CPUTIME(2)  
      REAL*4 TIME_END,TIME_START
      REAL*8 T1,T2,DELSNAP
      INTEGER COUNT
      LOGICAL PAUSEIT
	CHARACTER*20 BUFFER
C
![ykchoi 10.04.26. for linux version
!      INTERFACE TO INTEGER FUNCTION ATEXIT  
!     &    [C,ALIAS:'_atexit'](FUN)  
!      EXTERNAL FUN  
!      END  
!      EXTERNAL QUIT  
!ykchoi]
C  
C *** EE END BLOCK  
C **  SET IEEE EXCEPTION TRAPS (SUN SYSTEMS ONLY)  
C *** EE BEGIN BLOCK  
C     IS_TIMING NEEDS TO BE BASED ON A FLAG IN EFDC.INP AS IN EFDC_DS  
C     BUT WILL USE THIS AS A DEFAULT FOR NOW  
C  
      IS_TIMING=.TRUE.  
      TIME_END=SECNDS(0.0)  

      !CALL CPU_TIME(TIME_START)        ! *** Intel Version 11.066 Bug - Work Around

      COUNT = NARGS()
      PAUSEIT = .TRUE. !SCJ set to .FALSE. to turn off spacebar trap for calibration or sensitivity runs

      ! *** GET THE COMMAND LINE ARGUMENTS, IF ANY
      IF(COUNT.GT.0)THEN
        CALL GETARG(1, BUFFER, iStatus)
        IF (Buffer(1:4).EQ.'-NOP'.OR.Buffer(1:4).EQ.'-nop') THEN
          PAUSEIT=.FALSE.
        ENDIF
      ENDIF
C  
C *** EE END BLOCK  
C  
      CALL WELCOME  
C  
      IF(PAUSEIT)THEN
        WRITE(*,'(A)')'SETTING EXCEPTION TRAPS(TAP SPACEBAR TO QUIT)'  
![ykchoi 10.04.26. for linux version
!        IF(ATEXIT(QUIT).NE.0)STOP'CAN''T TRAP EXCEPTIONS'  
!ykchoi]
      ENDIF
C  
C **  OPEN OUTPUT FILES  
C  
      OPEN(7,FILE='EFDC.OUT',STATUS='UNKNOWN')  
      OPEN(8,FILE='EFDCLOG.OUT',STATUS='UNKNOWN')  
      OPEN(9,FILE='TIME.LOG',STATUS='UNKNOWN') 
      CLOSE(7,STATUS='DELETE')  
      CLOSE(8,STATUS='DELETE')  
      CLOSE(9,STATUS='DELETE')  
      OPEN(7,FILE='EFDC.OUT',STATUS='UNKNOWN')  
      write(7,*) 'Modified by GEOSR (NH007_20120418_fixedweir)' !GEOSR 2012. 4.18
      OPEN(8,FILE='EFDCLOG.OUT',STATUS='UNKNOWN')  
      OPEN(9,FILE='TIME.LOG',STATUS='UNKNOWN')  

      OPEN(1,FILE='DRYWET.LOG',STATUS='UNKNOWN') 
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='VSFP.OUT',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='SEDIAG.OUT',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='CFL.OUT',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='NEGSEDSND.OUT',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='ERROR.LOG',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')
C  
C **  CALL INPUT SUBROUTINE  
C  
      CALL VARINIT  
      CALL INPUT(TITLE)  
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
                 ANG = ANG + acos(-1.0)										!better way to do Pi, suggested by SCJ
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
                 ANG = ANG + acos(-1.0)										!better way to do Pi, suggested by SCJ
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
C  
C **  READ RESTART CONDITIONS OR INITIALIZE SCALAR FIELDS  
C     ISRESTI.EQ.10 READS AND OLD RESTART FILE GENERATED BY  
C     PRE SEPTEMBER 8, 1992 VERSIONS OF EFDC.FOR  
C  
      IF(ISRESTI.GE.1)THEN  
        IF(ISRESTI.EQ.1) CALL RESTIN1  
        IF(ISRESTI.EQ.2) CALL RESTIN2  
        IF(ISRESTI.EQ.10) CALL RESTIN10  
      ENDIF  
      IF(ISRESTI.EQ.-1) CALL RESTIN1  
C  
C **  INTIALIZE SALINITY FIELD IF NOT READ IN FROM RESTART FILE  
C  
      IF(ISTRAN(1).GE.1.AND.(ISRESTI.EQ.0                  .OR.  
     &                      (ISRESTI.GE.1.AND.ISCI(1).EQ.0).OR.
     &                      (ISTOPT(1).GT.1)))THEN  ! *** PMC SINGLE LINE - FORCE IC
        IF(ISTOPT(1).GE.1)THEN  
          NREST=0  
          DO K=1,KC  
            DO L=2,LA  
              SAL(L,K)=SALINIT(L,K)  
              SAL1(L,K)=SALINIT(L,K)  
            ENDDO  
          ENDDO  

          DO K=1,KC  
            DO LL=1,NCBS  
              L=LCBS(LL)  
              CLOS(LL,K,1)=SALINIT(L,K)  
              NLOS(LL,K,1)=0  
              IF(NCSERS(LL,1).EQ.0)THEN  
                SAL(L,K)=WTCI(K,1)*CBS(LL,1,1)+WTCI(K,2)*CBS(LL,2,1)  
                SAL1(L,K)=SAL(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              L=LCBW(LL)  
              CLOW(LL,K,1)=SALINIT(L,K)  
              NLOW(LL,K,1)=0  
              IF(NCSERW(LL,1).EQ.0)THEN  
                SAL(L,K)=WTCI(K,1)*CBW(LL,1,1)+WTCI(K,2)*CBW(LL,2,1)  
                SAL1(L,K)=SAL(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              L=LCBE(LL)  
              CLOE(LL,K,1)=SALINIT(L,K)  
              NLOE(LL,K,1)=0  
              IF(NCSERE(LL,1).EQ.0)THEN  
                SAL(L,K)=WTCI(K,1)*CBE(LL,1,1)+WTCI(K,2)*CBE(LL,2,1)  
                SAL1(L,K)=SAL(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              L=LCBN(LL)  
              CLON(LL,K,1)=SALINIT(L,K)  
              NLON(LL,K,1)=0  
              IF(NCSERN(LL,1).EQ.0)THEN  
                SAL(L,K)=WTCI(K,1)*CBN(LL,1,1)+WTCI(K,2)*CBN(LL,2,1)  
                SAL1(L,K)=SAL(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  

        ENDIF  
      ENDIF  
 9101 FORMAT(I5)  
 9102 FORMAT(3I5,12F8.2)  
C  
C **  INTIALIZE TEMP FIELD IF NOT READ IN FROM RESTART FILE  
C  
      IF(ISTRAN(2).GE.1.AND.(ISRESTI.EQ.0                  .OR.  
     &                      (ISRESTI.GE.1.AND.ISCI(2).EQ.0).OR.
     &                      (ISTOPT(2).GT.9)))THEN  ! *** PMC SINGLE LINE - FORCE IC
          ! *** SPATIALLY VARYING TEMPERATURE FIELD
          NREST=0  
          DO K=1,KC  
            DO L=2,LA  
              TEM(L,K)=TEMINIT(L,K)  
              TEM1(L,K)=TEM(L,K)  
            ENDDO  
          ENDDO  

          DO K=1,KC  
            DO LL=1,NCBS  
              L=LCBS(LL)  
              CLOS(LL,K,2)=TEMINIT(L,K)  
              NLOS(LL,K,2)=0  
              IF(NCSERS(LL,2).EQ.0)THEN  
                TEM(L,K)=WTCI(K,1)*CBS(LL,1,2)+WTCI(K,2)*CBS(LL,2,2)  
                TEM1(L,K)=TEM(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              L=LCBW(LL)  
              CLOW(LL,K,2)=TEMINIT(L,K)  
              NLOW(LL,K,2)=0  
              IF(NCSERW(LL,2).EQ.0)THEN  
                TEM(L,K)=WTCI(K,1)*CBW(LL,1,2)+WTCI(K,2)*CBW(LL,2,2)  
                TEM1(L,K)=TEM(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              L=LCBE(LL)  
              CLOE(LL,K,2)=TEMINIT(L,K)  
              NLOE(LL,K,2)=0  
              IF(NCSERE(LL,2).EQ.0)THEN  
                TEM(L,K)=WTCI(K,1)*CBE(LL,1,2)+WTCI(K,2)*CBE(LL,2,2)  
                TEM1(L,K)=TEM(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              L=LCBN(LL)  
              CLON(LL,K,2)=TEMINIT(L,K)  
              NLON(LL,K,2)=0  
              IF(NCSERN(LL,2).EQ.0)THEN  
                TEM(L,K)=WTCI(K,1)*CBN(LL,1,2)+WTCI(K,2)*CBN(LL,2,2)  
                TEM1(L,K)=TEM(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  

      ENDIF  
C  
C **  INTIALIZE TEMPERATURE BC IF NOT READ IN FROM RESTART FILE  
C     AND CONSTANT INTIAL CONDITION IS USED  
C  
      IF(ISRESTI.EQ.0.AND.ISTRAN(2).GE.1)THEN  
        IF(ISTOPT(2).EQ.0)THEN  
          ! *** CONSTANT TEMPERATURE FIELD
          M=2  
          DO K=1,KC  
            DO LL=1,NCBS  
              CLOS(LL,K,M)=TEMO  
              NLOS(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              CLOW(LL,K,M)=TEMO  
              NLOW(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              CLOE(LL,K,M)=TEMO  
              NLOE(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              CLON(LL,K,M)=TEMO  
              NLON(LL,K,M)=0  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  

      ! *** RESET IC OPTION, IF USED
      IF(ISTOPT(2).GT.9)ISTOPT(2)=ISTOPT(2)-10 ! PMC SINGLE LINE
C  
C **  INTIALIZE DYE FIELD IF NOT READ IN FROM RESTART FILE  
C  
      IF(ISTRAN(3).GE.1.AND.(ISRESTI.EQ.0                  .OR.  
     &                      (ISRESTI.GE.1.AND.ISCI(3).EQ.0).OR.
     &                      (ISTOPT(3).GT.1)))THEN  ! *** PMC SINGLE LINE - FORCE IC
        IF(ISTOPT(3).GE.1)THEN  
          ! *** SPATIALLY VARIABLE DYE FIELD
          NREST=0  
          DO K=1,KC  
            DO L=2,LA  
              DYE(L,K)=DYEINIT(L,K)  
              DYE1(L,K)=DYE(L,K)  
            ENDDO  
          ENDDO  

          DO K=1,KC  
            DO LL=1,NCBS  
              L=LCBS(LL)  
              CLOS(LL,K,3)=DYEINIT(L,K)  
              NLOS(LL,K,3)=0  
              IF(NCSERS(LL,3).EQ.0)THEN  
                DYE(L,K)=WTCI(K,1)*CBS(LL,1,3)+WTCI(K,2)*CBS(LL,2,3)  
                DYE1(L,K)=DYE(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              L=LCBW(LL)  
              CLOW(LL,K,3)=DYEINIT(L,K)  
              NLOW(LL,K,3)=0  
              IF(NCSERW(LL,3).EQ.0)THEN  
                DYE(L,K)=WTCI(K,1)*CBW(LL,1,3)+WTCI(K,2)*CBW(LL,2,3)  
                DYE1(L,K)=DYE(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              L=LCBE(LL)  
              CLOE(LL,K,3)=DYEINIT(L,K)  
              NLOE(LL,K,3)=0  
              IF(NCSERE(LL,3).EQ.0)THEN  
                DYE(L,K)=WTCI(K,1)*CBE(LL,1,3)+WTCI(K,2)*CBE(LL,2,3)  
                DYE1(L,K)=DYE(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              L=LCBN(LL)  
              CLON(LL,K,3)=DYEINIT(L,K)  
              NLON(LL,K,3)=0  
              IF(NCSERN(LL,3).EQ.0)THEN  
                DYE(L,K)=WTCI(K,1)*CBN(LL,1,3)+WTCI(K,2)*CBN(LL,2,3)  
                DYE1(L,K)=DYE(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  

        ENDIF  
      ENDIF  
C  
C **  INTIALIZE DYE BC IF NOT READ IN FROM RESTART FILE  
C **  AND CONSTANT INITIAL CONDITIONS ARE USED  
C  
      IF((ISRESTI.EQ.0.AND.ISTRAN(3).GE.1).OR.
     &   (ISRESTI.GE.1.AND.ISCI(3).EQ.0))THEN     ! *** PMC SINGLE LINE
        IF(ISTOPT(3).EQ.0)THEN  
          M=3  
          DO K=1,KC  
            DO LL=1,NCBS  
              CLOS(LL,K,M)=0.  
              NLOS(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              CLOW(LL,K,M)=0.  
              NLOW(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              CLOE(LL,K,M)=0.  
              NLOE(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              CLON(LL,K,M)=0.  
              NLON(LL,K,M)=0  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE TOX AND BC IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(5).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(5).EQ.1)THEN  
        DO NT=1,NTOX  
          IF(ITXINT(NT).EQ.1.OR.ITXINT(NT).EQ.3)THEN  
            M=4+NT  
            DO K=1,KC  
              DO L=2,LA  
                TOX(L,K,NT)=TOXINIT(L,K,NT)  
                TOX1(L,K,NT)=TOX(L,K,NT)  
              ENDDO  
            ENDDO  

            DO K=1,KC  
              DO LL=1,NCBS  
               L=LCBS(LL)  
               CLOS(LL,K,M)=TOXINIT(L,K,NT)  
               NLOS(LL,K,M)=0  
               IF(NCSERS(LL,M).EQ.0)THEN  
               TOX(L,K,NT)=WTCI(K,1)*CBS(LL,1,M)+WTCI(K,2)*CBS(LL,2,M)  
               TOX1(L,K,NT)=TOX(L,K,NT)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
               L=LCBW(LL)  
               CLOW(LL,K,M)=TOXINIT(L,K,NT)  
               NLOW(LL,K,M)=0  
               IF(NCSERW(LL,M).EQ.0)THEN  
               TOX(L,K,NT)=WTCI(K,1)*CBW(LL,1,M)+WTCI(K,2)*CBW(LL,2,M)  
               TOX1(L,K,NT)=TOX(L,K,NT)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
               L=LCBE(LL)  
               CLOE(LL,K,M)=TOXINIT(L,K,NT)  
               NLOE(LL,K,M)=0  
               IF(NCSERE(LL,3).EQ.0)THEN  
               TOX(L,K,NT)=WTCI(K,1)*CBE(LL,1,M)+WTCI(K,2)*CBE(LL,2,M)  
               TOX1(L,K,NT)=TOX(L,K,NT)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
               L=LCBN(LL)  
               CLON(LL,K,M)=TOXINIT(L,K,NT)  
               NLON(LL,K,M)=0  
               IF(NCSERN(LL,M).EQ.0)THEN  
               TOX(L,K,NT)=WTCI(K,1)*CBN(LL,1,M)+WTCI(K,2)*CBN(LL,2,M)  
               TOX1(L,K,NT)=TOX(L,K,NT)  
               ENDIF  
              ENDDO  
            ENDDO  

          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  INTIALIZE TOX BC IF NOT READ IN FROM RESTART FILE  
C **  AND CONSTANT INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(5).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(5).EQ.1)THEN  
        DO NT=1,NTOX  
          IF(ITXINT(NT).EQ.0.OR.ITXINT(NT).EQ.2)THEN  
            M=4+NT  
            DO K=1,KC  
              DO LL=1,NCBS  
                CLOS(LL,K,M)=TOXINTW(NT)  
                NLOS(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
                CLOW(LL,K,M)=TOXINTW(NT)  
                NLOW(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
                CLOE(LL,K,M)=TOXINTW(NT)  
                NLOE(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
                CLON(LL,K,M)=TOXINTW(NT)  
                NLON(LL,K,M)=0  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  INTIALIZE TOX BED IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(5).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(5).EQ.1)THEN  
        DO NT=1,NTOX  
          IF(ITXINT(NT).EQ.2.OR.ITXINT(NT).EQ.3)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TOXB(L,K,NT)=TOXBINIT(L,K,NT)  
                TOXB1(L,K,NT)=TOXB(L,K,NT)  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  INTIALIZE SED AND BC IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(6).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(6).EQ.1)THEN  
        IF(ISEDINT.EQ.1.OR.ISEDINT.EQ.3)THEN  
          DO NS=1,NSED  
            M=4+NTOX+NS  
            DO K=1,KC  
              DO L=2,LA  
                SED(L,K,NS)=SEDINIT(L,K,NS)  
                SED1(L,K,NS)=SED(L,K,NS)  
              ENDDO  
            ENDDO  

            DO K=1,KC  
              DO LL=1,NCBS  
               L=LCBS(LL)  
               CLOS(LL,K,M)=SEDINIT(L,K,NS)  
               NLOS(LL,K,M)=0  
               IF(NCSERS(LL,M).EQ.0)THEN  
               SED(L,K,NS)=WTCI(K,1)*CBS(LL,1,M)+WTCI(K,2)*CBS(LL,2,M)  
               SED1(L,K,NS)=SED(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
               L=LCBW(LL)  
               CLOW(LL,K,M)=SEDINIT(L,K,NS)  
               NLOW(LL,K,M)=0  
               IF(NCSERW(LL,M).EQ.0)THEN  
               SED(L,K,NS)=WTCI(K,1)*CBW(LL,1,M)+WTCI(K,2)*CBW(LL,2,M)  
               SED1(L,K,NS)=SED(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
               L=LCBE(LL)  
               CLOE(LL,K,M)=SEDINIT(L,K,NS)  
               NLOE(LL,K,M)=0  
               IF(NCSERE(LL,3).EQ.0)THEN  
               SED(L,K,NS)=WTCI(K,1)*CBE(LL,1,M)+WTCI(K,2)*CBE(LL,2,M)  
               SED1(L,K,NS)=SED(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
               L=LCBN(LL)  
               CLON(LL,K,M)=SEDINIT(L,K,NS)  
               NLON(LL,K,M)=0  
               IF(NCSERN(LL,M).EQ.0)THEN  
               SED(L,K,NS)=WTCI(K,1)*CBN(LL,1,M)+WTCI(K,2)*CBN(LL,2,M)  
               SED1(L,K,NS)=SED(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  

          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE SED BC IF NOT READ IN FROM RESTART FILE AND  
C **  CONSTANT INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(6).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(6).EQ.1)THEN  
        IF(ISEDINT.EQ.0.OR.ISEDINT.EQ.2)THEN  
          DO NS=1,NSED  
            M=4+NTOX+NS  
            DO K=1,KC  
              DO LL=1,NCBS  
                CLOS(LL,K,M)=SEDO(NS)  
                NLOS(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
                CLOW(LL,K,M)=SEDO(NS)  
                NLOW(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
                CLOE(LL,K,M)=SEDO(NS)  
                NLOE(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
                CLON(LL,K,M)=SEDO(NS)  
                NLON(LL,K,M)=0  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE SED BED IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(6).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(6).EQ.1)THEN  
        IF(ISEDINT.EQ.2.OR.ISEDINT.EQ.3)THEN  
          DO NS=1,NSED  
            DO K=1,KB  
              DO L=2,LA  
                SEDB(L,K,NS)=SEDBINIT(L,K,NS)  
                SEDB1(L,K,NS)=SEDB(L,K,NS)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE SND AND BC IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(7).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(7).EQ.1)THEN  
        IF(ISEDINT.EQ.1.OR.ISEDINT.EQ.3)THEN  
          DO NS=1,NSND  
            M=4+NTOX+NSED+NS  
            DO K=1,KC  
              DO L=2,LA  
                SND(L,K,NS)=SNDINIT(L,K,NS)  
                SND1(L,K,NS)=SND(L,K,NS)  
              ENDDO  
            ENDDO  

            DO K=1,KC  
              DO LL=1,NCBS  
               L=LCBS(LL)  
               CLOS(LL,K,M)=SNDINIT(L,K,NS)  
               NLOS(LL,K,M)=0  
               IF(NCSERS(LL,M).EQ.0)THEN  
               SND(L,K,NS)=WTCI(K,1)*CBS(LL,1,M)+WTCI(K,2)*CBS(LL,2,M)  
               SND1(L,K,NS)=SND(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
               L=LCBW(LL)  
               CLOW(LL,K,M)=SNDINIT(L,K,NS)  
               NLOW(LL,K,M)=0  
               IF(NCSERW(LL,M).EQ.0)THEN  
               SND(L,K,NS)=WTCI(K,1)*CBW(LL,1,M)+WTCI(K,2)*CBW(LL,2,M)  
               SND1(L,K,NS)=SND(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
               L=LCBE(LL)  
               CLOE(LL,K,M)=SNDINIT(L,K,NS)  
               NLOE(LL,K,M)=0  
               IF(NCSERE(LL,3).EQ.0)THEN  
               SND(L,K,NS)=WTCI(K,1)*CBE(LL,1,M)+WTCI(K,2)*CBE(LL,2,M)  
               SND1(L,K,NS)=SND(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
               L=LCBN(LL)  
               CLON(LL,K,M)=SNDINIT(L,K,NS)  
               NLON(LL,K,M)=0  
               IF(NCSERN(LL,M).EQ.0)THEN  
               SND(L,K,NS)=WTCI(K,1)*CBN(LL,1,M)+WTCI(K,2)*CBN(LL,2,M)  
               SND1(L,K,NS)=SED(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  

          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE SND BC IF NOT READ IN FROM RESTART FILE AND  
C **  CONSTANT INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(7).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(7).EQ.1)THEN  
        IF(ISEDINT.EQ.0.OR.ISEDINT.EQ.2)THEN  
          DO NX=1,NSND  
            NS=NSED+NX  
            M=4+NTOX+NSED+NX  
            DO K=1,KC  
              DO LL=1,NCBS  
                CLOS(LL,K,M)=SEDO(NS)  
                NLOS(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
                CLOW(LL,K,M)=SEDO(NS)  
                NLOW(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
                CLOE(LL,K,M)=SEDO(NS)  
                NLOE(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
                CLON(LL,K,M)=SEDO(NS)  
                NLON(LL,K,M)=0  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE SND BED IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(7).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(7).EQ.1)THEN  
        IF(ISEDINT.EQ.2.OR.ISEDINT.EQ.3)THEN  
          DO NX=1,NSND  
            DO K=1,KB  
              DO L=2,LA  
                SNDB(L,K,NX)=SNDBINIT(L,K,NX)  
                SNDB1(L,K,NX)=SNDB(L,K,NX)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INITIALIZE SEDIMENT BED  
C  
      IF(ISTRAN(6).GE.1.OR.ISTRAN(7).GE.1) CALL BEDINIT  
C  
C **  INITIALIZE BUOYANCY AND EQUATION OF STATE  
C  
      CALL CALBUOY  
C  
C **  INITIALIZE SFL IF(ISRESTI.EQ.0.AND ISTRAN(4).GE.1)  
C  
      IF(ISRESTI.EQ.0.AND.ISTRAN(4).GE.1)THEN  
        IF(ISTOPT(4).EQ.11)THEN  
          DO K=1,KC  
            DO L=1,LC  
              SFL(L,K)=SAL(L,K)  
              SFL2(L,K)=SAL(L,K)  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBS  
              L=LCBS(LL)  
              CLOS(LL,K,5)=SALINIT(L,K)  
              NLOS(LL,K,5)=0  
              SFL(L,K)=WTCI(K,1)*CBS(LL,1,5)+WTCI(K,2)*CBS(LL,2,5)  
              SFL2(L,K)=SFL(L,K)  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              L=LCBW(LL)  
              CLOW(LL,K,5)=SALINIT(L,K)  
              NLOW(LL,K,5)=0  
              SFL(L,K)=WTCI(K,1)*CBW(LL,1,5)+WTCI(K,2)*CBW(LL,2,5)  
              SFL2(L,K)=SFL(L,K)  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              L=LCBE(LL)  
              CLOE(LL,K,5)=SALINIT(L,K)  
              NLOE(LL,K,5)=0  
              SFL(L,K)=WTCI(K,1)*CBE(LL,1,5)+WTCI(K,2)*CBE(LL,2,5)  
              SFL2(L,K)=SFL(L,K)  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              L=LCBN(LL)  
              CLON(LL,K,5)=SALINIT(L,K)  
              NLON(LL,K,5)=0  
              SFL(L,K)=WTCI(K,1)*CBN(LL,1,5)+WTCI(K,2)*CBN(LL,2,5)  
              SFL2(L,K)=SFL(L,K)  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  ACTIVATE DYE TRACER CONTINUITY CHECK  
C  
      IF(ISMMC.EQ.1)THEN  
        DO K=1,KC  
          DO L=1,LC  
            DYE(L,K)=1.  
            DYE1(L,K)=1.  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO LL=1,NCBS  
            CLOS(LL,K,3)=1.  
            NLOS(LL,K,3)=0  
          ENDDO  
          DO LL=1,NCBW  
            CLOW(LL,K,3)=1.  
            NLOW(LL,K,3)=0  
          ENDDO  
          DO LL=1,NCBE  
            CLOE(LL,K,3)=1.  
            NLOE(LL,K,3)=0  
          ENDDO  
          DO LL=1,NCBN  
            CLON(LL,K,3)=1.  
            NLON(LL,K,3)=0  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  MASK CELLS TO BE CONVERTED FROM WATER TO LAND  
C **  CALL MOVED TO SETBCS ON 23 JAN 2004  
C **  SET VERTICAL GRID DEPENDENT ARRAYS AND HARDWIRE DIMENSIONLESS  
C **  MIXING LENGTH  
C  
      DO K=1,KC  
        DZIC(K)=1./DZC(K)  
      ENDDO  
      DZIG(0)=0.  
      DZIG(KC)=0.  
      DO K=1,KS  
        DZG(K)=0.5*(DZC(K)+DZC(K+1))  
        DZIG(K)=1./DZG(K)  
        DZIGSD4(K)=0.25*DZIG(K)*DZIG(K)  
        CDZU(K)=-DZC(K)/(DZC(K)+DZC(K+1))  
        CDZL(K)=-DZC(K+1)/(DZC(K)+DZC(K+1))  
        CDZF(K)=DZC(K)*DZC(K+1)/(DZC(K)+DZC(K+1))  
        CDZM(K)=0.5*DZC(K)*DZC(K+1)  
      ENDDO  
      CDZR(1)=DZC(1)-1.  
      CDZD(1)=DZC(1)  
      DO K=2,KS  
        CDZR(K)=DZC(K)+CDZR(K-1)  
        CDZD(K)=DZC(K)+CDZD(K-1)  
      ENDDO  
      DO K=1,KS  
        CDZR(K)=CDZR(K)*DZG(K)*CDZL(1)  
      ENDDO  
      CDZKMK(1)=0.  
      DO K=2,KC  
        CDZKMK(K)=DZIG(K-1)*DZIC(K)  
      ENDDO  
      DO K=1,KS  
        CDZKK(K)=DZIC(K)*DZIG(K)  
        CDZKKP(K)=DZIG(K)*DZIC(K+1)  
      ENDDO  
      CDZKK(KC)=0.  
      Z(0)=0.  
      IF(KC.GT.1)THEN  
        DO K=1,KS  
          Z(K)=Z(K-1)+DZC(K)     ! *** TOP OF LAYER Z
          ZZ(K)=Z(K)-0.5*DZC(K)  ! *** MID LAYER Z
          FPROX(K)=(1./(VKC*Z(K))**2)+0.25*(1./(VKC*(1.-Z(K)))**2)/CTE2  
        ENDDO  
      ENDIF  
      Z(KC)=Z(KS)+DZC(KC)  
      ZZ(KC)=Z(KC)-0.5*DZC(KC)  
      IF(ISRESTI.EQ.0)THEN  
        DO K=0,KC  
          DO L=1,LC  
            DML(L,K)=VKC*Z(K)*(1.-Z(K))  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  INITIALIZE UNSTRETCHING PROCEDURE  
C  
      
      DZPC=(SELVMAX-BELVMIN)/FLOAT(KPC)  
      IF(DZPC.NE.0.)THEN
        ZP(0)=BELVMIN  
        DO KP=1,KPC  
          ZP(KP)=ZP(KP-1)+DZPC  
        ENDDO  
        DO KP=1,KPC  
          ZZP(KP)=0.5*(ZP(KP)+ZP(KP-1))  
        ENDDO  
        DO L=2,LA  
          TMP=(BELV(L)-BELVMIN)/DZPC  
          KPB(L)=NINT(0.5+TMP)  
        ENDDO  
      ENDIF
C  
C **  CALCULATE CONSTANT HORIZONTAL SPATIAL ARRAYS  
C  
      DO L=2,LA  
        DXYU(L)=DXU(L)*DYU(L)  
        DXYV(L)=DXV(L)*DYV(L)  
        DXYP(L)=STCAP(L)*DXP(L)*DYP(L)  
        DXIU(L)=1./DXU(L)  
        DYIU(L)=1./DYU(L)  
        DXIV(L)=1./DXV(L)  
        DYIV(L)=1./DYV(L)  
        DXYIP(L)=1./(STCAP(L)*DXP(L)*DYP(L))  
        DXYIU(L)=1./(DXU(L)*DYU(L))  
        DXYIV(L)=1./(DXV(L)*DYV(L))  
        HRU(L)=SUB(L)*HMU(L)*DYU(L)*DXIU(L)  
        HRV(L)=SVB(L)*HMV(L)*DXV(L)*DYIV(L)  
        HRUO(L)=SUBO(L)*DYU(L)*DXIU(L)  
        HRVO(L)=SVBO(L)*DXV(L)*DYIV(L)
        !HRXYU(L)=DXU(L)/DYU(L)    ! PMC - NOT USED
        !HRXYV(L)=DXV(L)/DYV(L)    ! PMC - NOT USED
C        SBX(L)=0.5*SBX(L)*DYU(L)  ! TT ORIGINAL
C        SBY(L)=0.5*SBY(L)*DXV(L)  ! TT ORIGINAL
        SBX(L)=0.5*SUB(L)*DYU(L)  !PMC 
        SBY(L)=0.5*SVB(L)*DXV(L)  !PMC
        SBXO(L)=SBX(L)  
        SBYO(L)=SBY(L)  
        SNLPX(L)=GID2*SNLPX(L)*DYU(L)  
        SNLPY(L)=GID2*SNLPY(L)*DXV(L)  
      ENDDO  

      ! *** DSLLC BEGIN SEEPAGE
      IF(ISGWIT.EQ.3)THEN
        DO L=2,LA
          RIFTR(L)=RIFTR(L)*DXYP(L)  ! M^3/S
        ENDDO
      ENDIF
      ! *** DSLLC END
C  
C **  DEACTIVATE DRY CELLS  
C  
 6902 FORMAT('  DRYING AT N,I,J =',I10,2I6,'  H,H1,H2 =',3(2X,E12.4))  
C  
C **  INITIALIZE ZERO DIMENSION VOLUME BALANCE  
C  
      IF(ISDRY.GE.1.AND.ISDRY.LE.98)THEN  
        OPEN(1,FILE='ZVOLBAL.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='AVSEL.OUT',STATUS='UNKNOWN')  
        LPBTMP=0  
        DO L=2,LA  
          TVAR3C(L)=0  
          IF(SPB(L).EQ.0)THEN  
            LPBTMP=LPBTMP+1  
            TVAR3C(L)=1  
          ENDIF  
          LORDER(L)=0  
        ENDDO  
        TVAR3C(1)=1  
        TVAR3C(LC)=1  
        LORMAX=LC-2-LPBTMP  
        DO LS=1,LORMAX  
          BELMIN=100000.  
          DO L=2,LA  
            IF(SPB(L).NE.0.AND.TVAR3C(L).NE.1)THEN  
              IF(BELV(L).LT.BELMIN)THEN  
                LBELMIN=L  
                BELMIN=BELV(L)  
              ENDIF  
            ENDIF  
          ENDDO  
          LORDER(LS)=LBELMIN  
          TVAR3C(LBELMIN)=1  
        ENDDO  
        WRITE(1,5300)  
        LS=1  
        L=LORDER(LS)  
        BELSURF(LS)=BELV(L)  
        ASURFEL(LS)=DXYP(L)  
        VOLSEL(LS)=0.  
        WRITE(1,5301)LS,BELSURF(LS),ASURFEL(LS),VOLSEL(LS)  
        DO LS=2,LORMAX  
          L=LORDER(LS)  
          BELSURF(LS)=BELV(L)  
          ASURFEL(LS)=ASURFEL(LS-1)+DXYP(L)  
          VOLSEL(LS)=VOLSEL(LS-1)+0.5*(BELSURF(LS)-BELSURF(LS-1))*  
     &        (ASURFEL(LS)+ASURFEL(LS-1))  
          WRITE(1,5301)LS,BELSURF(LS),ASURFEL(LS),VOLSEL(LS)  
        ENDDO  
        LS=LORMAX+1  
        BELSURF(LS)=BELV(L)+10.0  
        ASURFEL(LS)=ASURFEL(LS-1)  
        VOLSEL(LS)=VOLSEL(LS-1)+0.5*(BELSURF(LS)-BELSURF(LS-1))*  
     &      (ASURFEL(LS)+ASURFEL(LS-1))  
        WRITE(1,5301)LS,BELSURF(LS),ASURFEL(LS),VOLSEL(LS)  
        VOLZERD=0.  
        VOLLDRY=0.  
        DO L=2,LA  
          IF(SPB(L).NE.0)THEN  
            VOLZERD=VOLZERD+DXYP(L)*HP(L)  
            IF(HP(L).GT.HDRY) VOLLDRY=VOLLDRY+DXYP(L)*HP(L)  
          ENDIF  
        ENDDO  
        DO LS=1,LORMAX  
          IF(VOLZERD.GE.VOLSEL(LS).AND.VOLZERD.LT.VOLSEL(LS+1))THEN  
            WTM=VOLSEL(LS+1)-VOLZERD  
            WTMP=VOLZERD-VOLSEL(LS)  
            DELVOL=VOLSEL(LS+1)-VOLSEL(LS)  
            WTM=WTM/DELVOL  
            WTMP=WTMP/DELVOL  
            SELZERD=WTM*BELSURF(LS)+WTMP*BELSURF(LS+1)  
            ASFZERD=WTM*ASURFEL(LS)+WTMP*ASURFEL(LS+1)  
          ENDIF  
        ENDDO  
        VETZERD=VOLZERD  
        WRITE(1,5302)  
        WRITE(1,5303) SELZERD,ASFZERD,VOLZERD,VOLLDRY  
        CLOSE(1)  
      ENDIF  
 5300 FORMAT('   M    BELSURF     ASURFEL     ',  
     &    '   VOLSEL',/)  
 5301 FORMAT(1X,I3,2X,F10.5,2X,E12.4,2X,E12.4)  
 5302 FORMAT(/)  
 5303 FORMAT(2X,F10.5,3(2X,E12.4))  
C  
C **  INITIALIZE ELEVATION OF ACTIVE GROUNDWATER ZONE FOR COLD START  
C  
      IF(ISGWIE.GE.1.AND.ISRESTI.EQ.0)THEN  
        DO L=2,LA  
          IF(HP(L).GT.HDRY)THEN  
            AGWELV(L)=BELV(L)  
          ELSE  
            IF(BELAGW(L).LT.SELZERD)THEN  
              AGWELV(L)=SELZERD  
              AGWELV(L)=MIN(AGWELV(L),BELV(L))  
            ELSE  
              AGWELV(L)=BELAGW(L)  
            ENDIF  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          AGWELV1(L)=AGWELV(L)  
          AGWELV2(L)=AGWELV(L)  
        ENDDO  
        OPEN(1,FILE='GWELV.OUT',STATUS='UNKNOWN')  
        WRITE(1,5400)  
        WRITE(1,5402)  
        DO L=2,LA  
          WRITE(1,5401)IL(L),JL(L),BELV(L),BELAGW(L),AGWELV(L)  
        ENDDO  
        CLOSE(1)  
      ENDIF  
 5400 FORMAT('   I   J    BELELV      BELAGW     ',  
     &    '   AGWELV',/)  
 5401 FORMAT(1X,2I5,2X,F10.5,2X,F10.5,2X,F10.5)  
 5402 FORMAT(/)  
C  
C **  CALCULATE CONSTANT C ARRAYS FOR EXTERNAL P SOLUTION  
C **  HRU=SUB*HMU*DYU/DXU & HRV=SVB*HMV*DXV/DYV  
C **  DXYIP=1/(DXP*DYP)  
C  
      IF(IRVEC.NE.9)THEN  
        DO L=2,LA  
          CC(L)=1.  
          CCC(L)=1.  
        ENDDO  
        IF(ISRLID.EQ.1)THEN  
          DO L=2,LA  
            CC(L)=0.  
            CCC(L)=0.  
            IF(SPB(L).EQ.0.) CC(L)=1.  
            IF(SPB(L).EQ.0.) CCC(L)=1.  
          ENDDO  
        ENDIF  
        DO L=2,LA  
          LN=LNC(L)  
          C1=-G*DT*DT*SPB(L)*DXYIP(L)  
          CS(L)=C1*HRV(L)  
          CW(L)=C1*HRU(L)  
          CE(L)=C1*HRU(L+1)  
          CN(L)=C1*HRV(LN)  
          CC(L)=CC(L)-CS(L)-CW(L)-CE(L)-CN(L)  
          CCI(L)=1./CC(L)  
          CCS(L)=0.25*CS(L)  
          CCW(L)=0.25*CW(L)  
          CCE(L)=0.25*CE(L)  
          CCN(L)=0.25*CN(L)  
          CCC(L)=CCC(L)-CCS(L)-CCW(L)-CCE(L)-CCN(L)  
          CCCI(L)=1./CCC(L)  
        ENDDO  
        !DO LR=1,NRC  
          !L=LRC(LR)  
          !CCSR(LR)=CCS(L)*CCCI(L)    ! PMC - NOT USED
          !CCWR(LR)=CCW(L)*CCCI(L)    ! PMC - NOT USED
          !CCER(LR)=CCE(L)*CCCI(L)    ! PMC - NOT USED
          !CCNR(LR)=CCN(L)*CCCI(L)    ! PMC - NOT USED
          !CSR(LR)=CS(L)*CCI(L)       ! PMC - NOT USED
          !CWR(LR)=CW(L)*CCI(L)       ! PMC - NOT USED
          !CER(LR)=CE(L)*CCI(L)       ! PMC - NOT USED
          !CNR(LR)=CN(L)*CCI(L)       ! PMC - NOT USED
        !ENDDO  
        !DO LB=1,NBC  
          !L=LBC(LB)                   ! PMC - NOT USED
          !CCSB(LB)=CCS(L)*CCCI(L)     ! PMC - NOT USED
          !CCWB(LB)=CCW(L)*CCCI(L)     ! PMC - NOT USED
          !CCEB(LB)=CCE(L)*CCCI(L)     ! PMC - NOT USED
          !CCNB(LB)=CCN(L)*CCCI(L)     ! PMC - NOT USED
          !CSB(LB)=CS(L)*CCI(L)        ! PMC - NOT USED
          !CWB(LB)=CW(L)*CCI(L)        ! PMC - NOT USED
          !CEB(LB)=CE(L)*CCI(L)        ! PMC - NOT USED
          !CNB(LB)=CN(L)*CCI(L)        ! PMC - NOT USED
        !ENDDO  
      ENDIF  
C  
C **  SMOOTH INITIAL SALINITY  
C  
      IF(NSBMAX.GE.1)THEN  
        CALL SALTSMTH  
      ENDIF  
C  
C **  OUTPUT INITIAL DEPTH AND SALINITY FIELDS  
C **  PLOT SMOOTHED CELL CENTER STATIC DEPTHS  
C  
      DO L=2,LA  
        PAM(L)=HMP(L)  
      ENDDO  
      WRITE (7,16)  
      CALL PPLOT (2)  
      IF(DEBUG)CALL DEPPLT  
C  
C **  PLOT INITIAL SALINITY IN SURFACE AND BOTTOM LAYERS  
C  
      DO KK=1,KC,KS  
        DO L=2,LA  
          PAM(L)=SAL(L,KK)  
        ENDDO  
        WRITE (7,316) KK  
        CALL PPLOT (1)  
      ENDDO  
   16 FORMAT (1H1,' CELL CENTER STATIC DEPTHS',//)  
  316 FORMAT (1H1,'INITIAL SALINITY IN LAYER',I5,//)  
C  
C **  INITIALIZE SALINITY AND TEMPATURE DATA ASSIMILATION  
C  
      DO J=1,NLDAM  
        DO I=1,NDDAM  
          FSALASM(I,J)=0.0  
          FVOLASM(I,J)=0.0  
          FTEMASM(I,J)=0.0  
        ENDDO  
      ENDDO  
C  
C **  INITIALIZE WATER QUALITY MODEL AND READ INPUT  
C  
      IF(ISTRAN(8).GE.1) CALL WQ3DINP  
C  
C **  INITIALIZE EFDC EXPLORER OUTPUT  
C  
      IF(ISSPH(8).EQ.1.OR.ISBEXP.EQ.1) CALL EEXPOUT(1)  
! { GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2010.11.10
C **  INITIALIZE EFDC HYDRO DISTRIBUTION OUTPUT  
      IF(ISRESTO.LT.-20)THEN
        CALL RESTOUT(-20)
      ENDIF
! } GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2010.11.10
C  
C **  SELECT FULL HYDRODYNAMIC AND MASS TRANSPORT CALCULATION OR  
C **  LONG-TERM MASS TRANSPORT CALCULATION  
C  
      NITERAT=0  
C      IF(ISLTMT.EQ.0)THEN  
C        IF(IS1DCHAN.EQ.0)THEN  
          IF(IS2TIM.EQ.0) CALL HDMT  
          IF(IS2TIM.GE.1) CALL HDMT2T  
C        ENDIF  
C        IF(IS1DCHAN.GE.1) CALL HDMT1D  
C      ENDIF  
C      IF(ISLTMT.GE.1) CALL LTMT  
C
!{GEOSR, OIL, CWCHO, 101122
      IF(ISPD.GE.2.AND.IDTOX.LT.4440) CLOSE(ULGR)  !DHC
!}
      
C *** EE BEGIN BLOCK  
C **  OUTPUT TIMING (OUTPUT TO TIME.LOG (UNIT 9) USED BY EFDC_EXPLORER)  
C  
      TIME_END=SECNDS(TIME_END)  
      TIME_END=TIME_END/3600.  
C  
C **  DTIME AND FLUSH ARE SUPPORTED ON SUN SYSTEMS,BUT MAY NOT BE  
C **  SUPPORTED ON OTHER SYSTEMS.  
C  
      TCPU=DTIME(CPUTIME)
      TCPU=TCPU/3600          
      CPUTIME(1)=CPUTIME(1)/3600.  !TCPU
      CPUTIME(2)=CPUTIME(2)/3600.

      TCONG=TCONG/3600.
      THDMT=THDMT/3600.
      TPUV =TPUV/3600.
      TSSED=TSSED/3600.
      TSSEDZLJ=TSSEDZLJ/3600.
      TCEXP=TCEXP/3600.
      TAVB =TAVB/3600.
      TUVW =TUVW/3600.
      TQQQ =TQQQ/3600.
      TVDIF=TVDIF/3600.
      TSADV=TSADV/3600.
      TLRPD=TLRPD/3600.
      TMISC=TMISC/3600.
      TWQADV=TWQADV/3600.
      TWQDIF=TWQDIF/3600.
      TWQKIN=TWQKIN/3600.
      TWQSED=TWQSED/3600.
      WRITE(6,1995)THDMT,TCONG  
      IF( NSEDFLUME==0 )THEN
        WRITE(6,1996)TPUV,TSSED  
      ELSE
        WRITE(6,2005)TPUV,TSSEDZLJ  
      ENDIF
      WRITE(6,1997)TCEXP,TAVB  
      WRITE(6,1998)TUVW,TQQQ  
      WRITE(6,1999)TVDIF,TSADV  
      WRITE(6,2000)TLRPD,TMISC  
      IF(ISTRAN(8).GT.0)THEN
        WRITE(6,2001)TWQADV,TWQDIF  
        WRITE(6,2002)TWQKIN,TWQSED  
      ENDIF
      WRITE(6,2003)CPUTIME(1),CPUTIME(2)  
      WRITE(6,2004)TIME_END,  TCPU  
 1995 FORMAT('***TIMING (HOURS)',/,  
     &       'T HDMT      =',F7.3,'  T CONG GRAD =',F7.3)  
 1996 FORMAT('T P&UV VELS =',F7.3,'  T SSEDTOX   =',F7.3)  
 1997 FORMAT('T EXPLICIT  =',F7.3,'  T C VERT V&D=',F7.3)  
 1998 FORMAT('T CALC UVW  =',F7.3,'  T TURB QQQ  =',F7.3)  
 1999 FORMAT('T VERT DFUSN=',F7.3,'  T ADV TRANSP=',F7.3)  
 2000 FORMAT('T PART TRK  =',F7.3,'  T MISC TIME =',F7.3)  
 2001 FORMAT('WQ ADV TRANS=',F7.3,'  WQ VERT DIFF=',F7.3)  
 2002 FORMAT('WQ KINETICS =',F7.3,'  WQ DIAGEN   =',F7.3)  
 2003 FORMAT('CPU USER    =',F7.3,'  CPU SYSTEM  =',F7.3)  
 2004 FORMAT('ELAPSED TIME=',F7.3,'  CPU TIME    =',F7.3)  
 2005 FORMAT('T P&UV VELS =',F7.3,'  T SEDZLJ    =',F7.3)  

      ! *** EFDC LOG
      WRITE(8,1995)THDMT,TCONG
      IF( NSEDFLUME==0 )THEN
        WRITE(8,1996)TPUV,TSSED  
      ELSE
        WRITE(8,2005)TPUV,TSSEDZLJ  
      ENDIF
      WRITE(8,1997)TCEXP,TAVB  
      WRITE(8,1998)TUVW,TQQQ  
      WRITE(8,1999)TVDIF,TSADV  
      WRITE(8,2000)TLRPD,TMISC  
      WRITE(8,2001)TWQADV,TWQDIF  
      WRITE(8,2002)TWQKIN,TWQSED  
      WRITE(8,2003)CPUTIME(1),CPUTIME(2)
      WRITE(8,2004)TIME_END,  TCPU  
  
      ! **              EXACT FORMAT REQUIRED BY EFDC_EXPLORER  
      ! *** TIME LOG
      CALL TIMELOG(N,TIMEDAY)  
      WRITE(9,6995)THDMT,TCONG  
      IF( NSEDFLUME==0 )THEN
        WRITE(9,6996)TPUV,TSSED  
      ELSE
        WRITE(9,7005)TPUV,TSSEDZLJ  
      ENDIF
      WRITE(9,6997)TCEXP,TAVB  
      WRITE(9,6998)TUVW, TQQQ  
      WRITE(9,6999)TVDIF,TSADV
      WRITE(9,7000)TLRPD,TMISC
      WRITE(9,7001)TWQADV,TWQDIF  
      WRITE(9,7002)TWQKIN,TWQSED  
      WRITE(9,7003)CPUTIME(1),CPUTIME(2)
      WRITE(9,7004)TIME_END,  TCPU  
 6995 FORMAT('***TIMING (HOURS)',/,  
     &       'T HDMT       =',F14.4,'  T CONG GRAD  =',F14.4)  
 6996 FORMAT('T P&UV VELS  =',F14.4,'  T SSEDTOX    =',F14.4)  
 6997 FORMAT('T EXPLICIT   =',F14.4,'  T C VERT V&D =',F14.4)  
 6998 FORMAT('T CALC UVW   =',F14.4,'  T TURB QQQ   =',F14.4)  
 6999 FORMAT('T VERT DFUSN =',F14.4,'  T ADV TRANSP =',F14.4)  
 7000 FORMAT('T PART TRK   =',F14.4,'  T MISC TIME  =',F14.4)  
 7001 FORMAT('WQ ADV TRANS =',F14.4,'  WQ VERT DIFF =',F14.4)  
 7002 FORMAT('WQ KINETICS  =',F14.4,'  WQ DIAGEN    =',F14.4)  
 7003 FORMAT('CPU USER     =',F14.4,'  CPU SYSTEM   =',F14.4)  
 7004 FORMAT('ELAPSED TIME =',F14.4,'  CPU TIME     =',F14.4)  
 7005 FORMAT('T P&UV VELS  =',F14.4,'  T SEDZLJ     =',F14.4)  
      WRITE(9,'(i1)')1 ! GEOSR CHECK EFDC SUCCESS : JGCHO 2010.11.12
C  
C *** EE END BLOCK  
C **  CLOSE OUTPUT  FILES  
C  
      CLOSE(7)  
      CLOSE(8)  
      CLOSE(9)  

C***** Added By Meng Xia, Nov.19,2007

CPMC      IF (ISWASP.EQ.17) THEN
CPMC        WRITE(6,*)'  '
CPMC        CALL hlclose(Ihl_handle,ierror)
CPMC        IF(ierror .gt. 0)then
CPMC          CALL hlgetlasterror(errstring)
CPMC          WRITE(6,6000) ierror, errstring,'CLOSING'
CPMC6000      FORMAT('Error ',I10, ' : ', A30,A10)
CPMC        END IF
CPMC      END IF

C***** Added By Meng Xia, Nov.19,2007

      STOP  
      END  

