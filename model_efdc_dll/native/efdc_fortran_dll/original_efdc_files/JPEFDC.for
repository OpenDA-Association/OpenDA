      SUBROUTINE JPEFDC  
C  
C **  PROGRAM JPEFDC IS STAND ALONE VERSION OF EFDC JET-PLUME MODEL  
C **  BASED ON LEE AND CHEUNG'S LAGRANGIAN BUOYANT JET MODEL  
C **  AND EXTENDED FOR THREE-DIMENSIONAL AMBIENT CURRENTS  
C **    REF: LEE, J.H.W., AND V. CHEUNG, J. ENVIRON. ENGR., 116, 1085-  
C **    1106, 1990.  
C **  FOR MORE INFO EMAIL HAM@VISI.NET  
C CHANGE RECORD  
C  
      USE GLOBAL  
      PARAMETER (NJELM=2,NATDM=1)  
      CHARACTER*11 FNJPGEO,FNJPVEL,FNJPCON,FNJPTOX,FNJPTPF,FNJPLOG,  
     &    FNNRFLD,FNNRFLB  
      CHARACTER*2  FNNUM(25)  

      ! *** DSLLC BEGIN
      LOGICAL*4,SAVE::LOUTJET
      INTEGER,SAVE  ::NPRT
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DRHONS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DRMAJF  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DRMAJNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DRMAJS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DYEJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DYEJNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::PHJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::QJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::QJH  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::QJNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RADJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RADJNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RDQA  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RDQANS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RHOJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RLEJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RLEJNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RMAJP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RMAJPNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SALJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SALJNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SEDJET  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SEDJETI  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SEDJNE  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SFLJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SIG  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SIGNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SNDJET  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SNDJETI  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SNDJNE  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TEMJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TEMJNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::THJG  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::THJL  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TIMJP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TOXJET  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TOXJETI  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TOXJNE  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::UJG  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::UJGNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::VJG  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::VJGNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WJG  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WJGNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::XJG  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::XJGNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::YJG  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::YJGNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::ZJG  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::ZJGNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::SEDJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::SEDJNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::SNDJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::SNDJNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::TOXJ  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::TOXJNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:,:)::TOXPFJP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::TOXPFTJP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::TOXPFTNS  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::UJPAVG  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::VJPAVG  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::WJPAVG  

      IF(.NOT.ALLOCATED(DRHONS))THEN
        PRINT *,'JET/PLUME COMPUTATIONS STARTED.  NQJPIJ=',NQJPIJ
        ALLOCATE(DRHONS(NJPSM))  
        ALLOCATE(DRMAJF(NJELM))  
        ALLOCATE(DRMAJNS(NJPSM))  
        ALLOCATE(DRMAJS(NJELM))  
        ALLOCATE(DYEJ(NJELM))  
        ALLOCATE(DYEJNS(NJPSM))  
        ALLOCATE(PHJ(NJELM))  
        ALLOCATE(QJ(NJELM))  
        ALLOCATE(QJH(NJELM))  
        ALLOCATE(QJNS(NJPSM))  
        ALLOCATE(RADJ(NJELM))  
        ALLOCATE(RADJNS(NJPSM))  
        ALLOCATE(RDQA(NJELM))  
        ALLOCATE(RDQANS(NJPSM))  
        ALLOCATE(RHOJ(NJELM))  
        ALLOCATE(RLEJ(NJELM))  
        ALLOCATE(RLEJNS(NJPSM))  
        ALLOCATE(RMAJP(NJELM))  
        ALLOCATE(RMAJPNS(NJPSM))  
        ALLOCATE(SALJ(NJELM))  
        ALLOCATE(SALJNS(NJPSM))  
        ALLOCATE(SEDJ(NJELM,NSCM))  
        ALLOCATE(SEDJET(NSCM))  
        ALLOCATE(SEDJETI(NSCM))  
        ALLOCATE(SEDJNE(NSCM))  
        ALLOCATE(SEDJNS(NSCM,NJPSM))  
        ALLOCATE(SFLJ(NJELM))  
        ALLOCATE(SIG(NJELM))  
        ALLOCATE(SIGNS(NJPSM))  
        ALLOCATE(SNDJ(NJELM,NSNM))  
        ALLOCATE(SNDJET(NSNM))  
        ALLOCATE(SNDJETI(NSNM))  
        ALLOCATE(SNDJNE(NSNM))  
        ALLOCATE(SNDJNS(NSNM,NJPSM))  
        ALLOCATE(TEMJ(NJELM))  
        ALLOCATE(TEMJNS(NJPSM))  
        ALLOCATE(THJG(NJELM))  
        ALLOCATE(THJL(NJELM))  
        !ALLOCATE(TIMJP(NJPSM))  
        ALLOCATE(TOXJ(NJELM,NTXM))  
        ALLOCATE(TOXJET(NTXM))  
        ALLOCATE(TOXJETI(NTXM))  
        ALLOCATE(TOXJNE(NTXM))  
        ALLOCATE(TOXJNS(NTXM,NJPSM))  
        ALLOCATE(TOXPFJP(NJELM,NSTM,NTXM))  
        ALLOCATE(TOXPFTJP(NJELM,NTXM))  
        ALLOCATE(TOXPFTNS(NTXM,NJPSM))  
        ALLOCATE(UJG(NJELM))  
        ALLOCATE(UJGNS(NJPSM))  
        ALLOCATE(UJPAVG(KCM,NJPSM))  
        ALLOCATE(VJG(NJELM))  
        ALLOCATE(VJGNS(NJPSM))  
        ALLOCATE(VJPAVG(KCM,NJPSM))  
        ALLOCATE(WJG(NJELM))  
        ALLOCATE(WJGNS(NJPSM))  
        ALLOCATE(WJPAVG(KCM,NJPSM))  
        ALLOCATE(XJG(NJELM))  
        ALLOCATE(XJGNS(NJPSM))  
        ALLOCATE(YJG(NJELM))  
        ALLOCATE(YJGNS(NJPSM))  
        ALLOCATE(ZJG(NJELM))  
        ALLOCATE(ZJGNS(NJPSM))

        ! *** INITIALIZE LOCAL ARRAYS
        NPRT = 0
        DRHONS=0.
        DRMAJF=0.
        DRMAJNS=0.
        DRMAJS=0.
        DYEJ=0.
        DYEJNS=0.
        PHJ=0.
        QJ=0.
        QJH=0.
        QJNS=0.
        RADJ=0.
        RADJNS=0.
        RDQA=0.
        RDQANS=0.
        RHOJ=0.
        RLEJ=0.
        RLEJNS=0.
        RMAJP=0.
        RMAJPNS=0.
        SALJ=0.
        SALJNS=0.
        SEDJ=0.
        SEDJET=0.
        SEDJETI=0.
        SEDJNE=0.
        SEDJNS=0.
        SFLJ=0.
        SIG=0.
        SIGNS=0.
        SNDJ=0.
        SNDJET=0.
        SNDJETI=0.
        SNDJNE=0.
        SNDJNS=0.
        TEMJ=0.
        TEMJNS=0.
        THJG=0.
        THJL=0.
        !TIMJP=0.
        TOXJ=0.
        TOXJET=0.
        TOXJETI=0.
        TOXJNE=0.
        TOXJNS=0.
        TOXPFJP=0.
        TOXPFTJP=0.
        TOXPFTNS=0.
        UJG=0.
        UJGNS=0.
        UJPAVG=0.
        VJG=0.
        VJGNS=0.
        VJPAVG=0.
        WJG=0.
        WJGNS=0.
        WJPAVG=0.
        XJG=0.
        XJGNS=0.
        YJG=0.
        YJGNS=0.
        ZJG=0.
        ZJGNS=0.
        
        LOUTJET=.FALSE.
        DO NJP=1,NQJPIJ  
          IF(IOUTJP(NJP).GT.0)LOUTJET=.TRUE.
        ENDDO
      ENDIF
      ! *** DSLLC END  
C  
C **  SET CONSTANTS  
C  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=DT*FLOAT(N)+TCON*TBEGIN  
        TIME=TIME/TCON  
      ELSE  
        TIME=TIMESEC/TCON  
      ENDIF
      NPRT = NPRT+1
   
      RPI=3.141592654  
      G=9.807  
      RADDEG=RPI/180.
      IF(LOUTJET)THEN  
        FNNUM( 1)= '01'  
        FNNUM( 2)= '02'  
        FNNUM( 3)= '03'  
        FNNUM( 4)= '04'  
        FNNUM( 5)= '05'  
        FNNUM( 6)= '06'  
        FNNUM( 7)= '07'  
        FNNUM( 8)= '08'  
        FNNUM( 9)= '09'  
        FNNUM(10)= '10'  
        FNNUM(11)= '11'  
        FNNUM(12)= '12'  
        FNNUM(13)= '13'  
        FNNUM(14)= '14'  
        FNNUM(15)= '15'  
        FNNUM(16)= '16'  
        FNNUM(17)= '17'  
        FNNUM(18)= '18'  
        FNNUM(19)= '19'  
        FNNUM(20)= '20'  
        FNNUM(21)= '21'  
        FNNUM(22)= '22'  
        FNNUM(23)= '23'  
        FNNUM(24)= '24'  
        FNNUM(25)= '25'  
        OPEN(88,FILE='JPBUG.DIA',POSITION='APPEND')  
        CLOSE(88,STATUS='DELETE')  
      ENDIF
C  
C **  LOOP OVER ALL JET/PLUME LOCATIONS  
C  
      DO NJP=1,NQJPIJ  
        DO K=1,KC  
          QJPENT(K,NJP)=0.0  
          UJPAVG(K,NJP)=0.0  
          VJPAVG(K,NJP)=0.0  
          WJPAVG(K,NJP)=0.0  
        ENDDO
        IF(DEBUG)THEN  
          FNJPLOG='JPLOG' // FNNUM(NJP) // '.OUT'  
          IF(N.EQ.1) OPEN(10,FILE=FNJPLOG,STATUS='UNKNOWN')  
          IF(N.EQ.1) CLOSE(10,STATUS='DELETE')  
          OPEN(10,FILE=FNJPLOG,STATUS='UNKNOWN',POSITION='APPEND')  
          WRITE(10,134)NJP,TIME  
          WRITE(8,134)NJP,TIME  
        ENDIF
        !IF(IMOD(NPRT,10).EQ.0)WRITE(6,134)NJP,TIME  ! PMC
        IF(N.EQ.1) KEFFJP(NJP)=KQJP(NJP)  
        LJP=LIJ(IQJP(NJP),JQJP(NJP))  
        KJP=KQJP(NJP)  
        KFLAG=0  
        ZTMP=(ZJET(NJP)-BELV(LJP))/HP(LJP)  
        KJP=NINT(ZTMP)  
        IF(KJP.LT.1) KJP=1  
        IF(KJP.GT.KC) KJP=KC  
        IF(ICALJP(NJP).GT.0)THEN  
C  
C **  READ INPUT DATA  
C    NJEL     MAXIMUM NUMBER OF ELEMENTS ALONG JET/PLUME LENGTH  
C    ISAMB  0 FOR SPATIALLY AND TEMPORALLY CONSTANT AMBIENT CONDITIONS  
C           1 FOR SPATIALLY VARYING AND TEMPORALLY CONSTANT CONDITIONS  
C           2 FOR SPATIALLY AND TEMPORALLY VARYING AMBIENT CONDITIONS  
C    ISJPD  0 FOR TEMPORALLY CONSTANT JET/PLUME DISCHARGE  
C           1 FOR TEMPORALLY VARYING JET/PLUME DISCHARGE  
C    ISENT  0 USE MAXIMUM OF SHEAR AND FORCED ENTRAINMENT  
C           1 USE SUM OF SHEAR AND FORCED ENTRAINMENT  
C    ISTOP  0 STOP AT SPECIFIED NUMBER OF ELEMENTS  
C           1 STOP WHEN CENTERLINE PENETRATES BOTTOM OR SURFACE  
C           2 STOP WITH BOUNDARY PENETRATES BOTTOM OR SURFACE  
C    ISOUT  0 DIMENSIONAL OUTPUT, 1 NONDIM OUTPUT LENGTH SCALED BY DJET  
C           2 NONDIM OUTPUT LENGTH SCALED BY SQRT(PI)*RJET  
C    NPRTE    ELEMENT OUTPUT PRINT FREQUENCY  
C    CFRD     ADJUSTMENT FACTOR FOR FROUDE NUMBER  
C    TUJP     TEMPORAL FREQUENCY FOR UPDATING JET/PLUME (SEC)  
C             IF ISJPD = 1  
C             CONDITIONS  
C  
          ISAMB=1  
C  
C             CONDITIONS  
C  
          ISJPD=0  
          ISTOP=ISTJP(NJP)  
          ISOUT=0  
          NPRTE=0  
          ALPHA=CFRD(NJP)  
          ALPH2=ALPHA*ALPHA  
          TUJP=0  
C  
C    NIMAX    MAXIMUM NUMEBER OF ITERATION  
C    XYERR    HORIZONTAL TRAJECTORY ERROR CRITERA (M)  
C    ZERR     VERTICAL TRAJECTORY ERROR CRITERA (M)  
C    RRER     HORIZONTAL TRAJECTORY ERROR CRITERA (M)  
C    RLER     VERTICAL TRAJECTORY ERROR CRITERA (M)  
C    DMRERR   ENTRAINMENT ERROR CRITERIA  
C             CRITERIA  
C  
          NIMAX=NJPMX(NJP)  
          XERRM=1000.  
          YERRM=1000.  
          ZERRM=1000.  
          RRERM=1000.  
          RLERM=1000.  
          DMRERM=DJPER(NJP)  
C  
C    WSET   SEDIMENT SETTLING VELOCITY (M/S)  
C    TPAR   PARTITION COEFFICIENT (L/MG OR M**3/GM)  
C    TDCY   CONTAMINANT DECAY RATE (1./SEC)  
C    WSET,TPAR,TDCY NOT USED IN EMBEDDED VERSION.  APPROPRIATE  
C    EFDC VARIABLES ARE USED INSTEAD  
C    XJET   EAST COORDINATE OF DISCHARGE (M)  
C    YJET   NORTH COORDINATE OF DISCHARGE (M)  
C    ZJET   ELEVATION OF DISCHARGE (M)  
C    PHJET  VERTICAL JET ANGLE POSITIVE FROM HORIZONTAL (DEGREES)  
C    THJET  HORIZONTAL JET ANGLE POS COUNTER CLOCKWISE FROM EAST (DEGREE  
C    DJET   DIAMETER OF DISCHARGE PORT (M)  
C    QJET   CONSTANT DISCHARGE VELOCITY (M)  
C           (NEGATIVE VALUE INDICATES CALCULATE FROM QVJET)  
C    QVJET  CONSTANT DISCHARGE RATE (CMS)  
C           (NEGATIVE VALUE INDICATES CALCULATE FROM QJET)  
C  
          XJET=0.  
          YJET=0.  
          RJET=0.5*DJET(NJP)  
          AJET=RPI*RJET*RJET  
          IF(ICALJP(NJP).EQ.1)THEN  
            QVJET=QQCJP(NJP)  
            QSERTAVG=0.  
            DO K=1,KC  
              QVJET=QVJET+QSERT(K,NQSERJP(NJP))
              QSERTAVG=QSERTAVG+QSERT(K,NQSERJP(NJP))  
            ENDDO  
            IF(QVJET.LE.1.E-16) GOTO 9000  
          ENDIF  
          IF(ICALJP(NJP).EQ.2)THEN  
            QVJET=QWRCJP(NJP)+QWRSERT(NQWRSERJP(NJP))  
            IF(QVJET.LE.1.E-16) GOTO 9000  
          ENDIF  
          QVJET0=QVJET  
          QJET=QVJET/AJET  
C  
C    SALJET   SALINITY (PPT)  
C    SEDJET   SEDIMENT (MG/L OR GM/M**3)  
C    TEMJET   TEMPERATURE  
C    TOXJET   CONTAMINATE CONCENTRATION (UG/L OR MG/M**3)  
C  
          IF(ICALJP(NJP).EQ.1)THEN  
            WTC=QQCJP(NJP)  
            WTV=QSERTAVG  
            TMPVAL=WTC+WTV  
            WTC=WTC/TMPVAL  
            WTV=WTV/TMPVAL  
            SALJET=WTC*CQCJP(1,NJP,1)+WTV*CSERT(1,NCSERJP(NJP,1),1)  
            TEMJET=WTC*CQCJP(1,NJP,2)+WTV*CSERT(1,NCSERJP(NJP,2),2)  
            DYEJET=WTC*CQCJP(1,NJP,3)+WTV*CSERT(1,NCSERJP(NJP,3),3)  
            SFLJET=WTC*CQCJP(1,NJP,4)+WTV*CSERT(1,NCSERJP(NJP,4),4)  
            DO NT=1,NTOX  
              NV=4+NT  
              TOXJET(NT)=WTC*CQCJP(1,NJP,NV)+WTV*CSERT(1,NCSERJP(
     &            NJP,NV),NV)  
            ENDDO  
            DO NX=1,NSED  
              NV=4+NTOX+NX  
              SEDJET(NX)=WTC*CQCJP(1,NJP,NV)+WTV*CSERT(1,NCSERJP(    ! PMC CHANGED NS TO NX
     &            NJP,NV),NV)  
            ENDDO  
            DO NX=1,NSND  
              NV=4+NTOX+NSED+NX  
              SNDJET(NX)=WTC*CQCJP(1,NJP,NV)+WTV*CSERT(1,NCSERJP(
     &            NJP,NV),NV)  
            ENDDO  
          ENDIF  
          IF(ICALJP(NJP).EQ.2)THEN  
            WTC=QWRCJP(NJP)  
            WTV=QWRSERT(NQWRSERJP(NJP))  
            TMPVAL=WTC+WTV  
            WTC=WTC/TMPVAL  
            WTV=WTV/TMPVAL  
            NS=NQWRSERJP(NJP)  
            LU=LIJ(IUPCJP(NJP),JUPCJP(NJP))  
            KU=KUPCJP(NJP)  
            SALJET=WTC*CWRCJP(NJP,1)+WTV*CQWRSERT(NS,1)+SAL1(LU,KU)  
            TEMJET=WTC*CWRCJP(NJP,2)+WTV*CQWRSERT(NS,2)+TEM1(LU,KU)  
            DYEJET=WTC*CWRCJP(NJP,3)+WTV*CQWRSERT(NS,3)+DYE1(LU,KU)  
            SFLJET=WTC*CWRCJP(NJP,4)+WTV*CQWRSERT(NS,4)+SFL2(LU,KU)  
            DO NT=1,NTOX  
              NV=4+NT  
              TOXJET(NT)=WTC*CWRCJP(NJP,NV)+WTV*CQWRSERT(NS,NV)+
     &            TOX1(LU,KU,NT)  
            ENDDO  
            DO NX=1,NSED  
              NV=4+NTOX+NX  
              SEDJET(NX)=WTC*CWRCJP(NJP,NV)+WTV*CQWRSERT(NS,NV)+  ! PMC FIXED NS TO NX
     &            SED1(LU,KU,NX)  
            ENDDO  
            DO NX=1,NSND  
              NV=4+NTOX+NSED+NX  
              SNDJET(NX)=WTC*CWRCJP(NJP,NV)+WTV*CQWRSERT(NS,NV)+
     &            SND1(LU,KU,NX)  
            ENDDO  
          ENDIF  
C  
C    ZBOT   BOTTOM ELEVATION (M)  
C    ZSUR   SURFACE ELEVATION (M)  
C    NAZD   NUMBER OF AMBIENT DATA IN VERTICAL  
C    NATD   NUMBER OF AMBIENT DATA IN TIME  
C  
          ZBOT=BELV(LJP)  
          ZSUR=BELV(LJP)+HP(LJP)  
          NAZD=KC  
          NATD=1  
C  
C    SPATIAL PRINT/SAVE INTERVAL IN VERTICAL  
C  
          DZPRT=HP(LJP)/FLOAT(NZPRJP(NJP)-2)  
          ZJPRT=ZJET(NJP)+DZPRT  
C  
C    ZA     ELEVATION OF AMBIENT DATA (M)  
C    TA     TIME OF AMBIENT DATA (SEC, HR, OR DAY FOR OUTPUT REFERENCE)  
C    UAG    EAST VELOCITY (M/S)  
C    VAG    NORTH VELOCITY (M/S)  
C    WAG    VERTICAL VELOCITY (M/S)  
C    SALA   AMBIENT SALINITY (PPT)  
C    SEDA   AMBIENT SEDIMENT CONCENTRATION (MG/L OR GM/M**3)  
C    TEMA   AMBIENT TEMPERATURE (DEG C)  
C    TOXA   AMBIENT TOXIC CONTAMINANT CONCENTRATION (UG/L OR MG/M**3)  
C  
          ZAD(1,1)=BELV(LJP)+0.5*HP(LJP)*DZC(1)  
          DO NZ=2,NAZD  
            ZAD(NZ,1)=ZAD(NZ-1,1)+0.5*HP(LJP)*( DZC(NZ-1)+DZC(NZ) )  
          ENDDO  
          DO NZ=1,NAZD  
            K=NZ  
            L=LJP  
            LN=LNC(L)  
            LS=LSC(L)  
            UTMP=0.5*(U(L+1,NZ)+U(L,NZ))  
            VTMP=0.5*(V(LN ,NZ)+V(L,NZ))  
            UAGD(NZ,1)=CUE(L)*UTMP+CVE(L)*VTMP  
            VAGD(NZ,1)=CUN(L)*UTMP+CVN(L)*VTMP  
            WAGD(NZ,1)=0.5*(W(L,K)+W(L,K-1))+GI*ZZ(K)*(DTI*(P(L)-P1(L))  
     &          +0.5*(U(L+1,K)*(P(L+1)-P(L))*DXIU(L+1)  
     &          +U(L,K)*(P(L)-P(L-1))*DXIU(L)  
     &          +V(LN,K)*(P(LN)-P(L))*DYIV(LN)  
     &          +V(L,K)*(P(L)-P(LS))*DYIV(L)))  
     &          +0.5*(1.-ZZ(K))*(U(L+1,K)*(BELV(L+1)-BELV(L))*DXIU(L+1)  
     &          +U(L,K)*(BELV(L)-BELV(L-1))*DXIU(L)  
     &          +V(LN,K)*(BELV(LN)-BELV(L))*DYIV(LN)  
     &          +V(L,K)*(BELV(L)-BELV(LS))*DYIV(L))  
            IF(DEBUG)THEN
              OPEN(88,FILE='JPBUG.DIA',POSITION='APPEND')  
              WRITE(88,889)NZ,K,L,LN,LS,SALAD(NZ,1),TEMAD(NZ,1),
     &          TOXAD(NZ,1,1)  
              CLOSE(88)  
            ENDIF
          ENDDO  
  889 FORMAT(5I6,3E14.5)  
          DO NZ=1,KC  
            SALAD(NZ,1)=SAL(LJP,NZ)  
            TEMAD(NZ,1)=TEM(LJP,NZ)  
            DYEAD(NZ,1)=DYE(LJP,NZ)  
            SFLAD(NZ,1)=SFL(LJP,NZ)  
          ENDDO  
          DO NT=1,NTOX  
            DO NZ=1,NAZD  
              TOXAD(NZ,NT,1)=TOX(LJP,NZ,NT)  
            ENDDO  
          ENDDO  
          DO NS=1,NSED  
            DO NZ=1,NAZD  
              SEDAD(NZ,NS,1)=SED(LJP,NZ,NS)  
            ENDDO  
          ENDDO  
          DO NX=1,NSND  
            DO NZ=1,NAZD  
              SNDAD(NZ,NX,1)=SND(LJP,NZ,NX)  
            ENDDO  
          ENDDO  
C  
C **  OPEN OUTPUT FILES  
C  
          IF(LOUTJET)THEN
            FNJPGEO='JPGEO' // FNNUM(NJP) // '.OUT'  
            FNJPVEL='JPVEL' // FNNUM(NJP) // '.OUT'  
            FNJPCON='JPCON' // FNNUM(NJP) // '.OUT'  
            FNJPTOX='JPTOX' // FNNUM(NJP) // '.OUT'  
            FNJPTPF='JPTPF' // FNNUM(NJP) // '.OUT'  
            FNNRFLD='NRFLD' // FNNUM(NJP) // '.OUT'  
            FNNRFLB='NRFLD' // FNNUM(NJP) // '.BIN'  
            IF(N.EQ.1) OPEN(1,FILE=FNJPGEO,STATUS='UNKNOWN')  
            IF(N.EQ.1) CLOSE(1,STATUS='DELETE')  
            IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  
     &        OPEN(1,FILE=FNJPGEO,STATUS='UNKNOWN',POSITION='APPEND')  
            IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  
     &          WRITE(1,111)  
            IF(N.EQ.1) OPEN(2,FILE=FNJPVEL,STATUS='UNKNOWN')  
            IF(N.EQ.1) CLOSE(2,STATUS='DELETE')  
            IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  
     &        OPEN(2,FILE=FNJPVEL,STATUS='UNKNOWN',POSITION='APPEND')  
            IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  
     &        WRITE(2,112)  
            IF(N.EQ.1) OPEN(3,FILE=FNJPCON,STATUS='UNKNOWN')  
            IF(N.EQ.1) CLOSE(3,STATUS='DELETE')  
            IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  
     &        OPEN(3,FILE=FNJPCON,STATUS='UNKNOWN',POSITION='APPEND')  
            IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  
     &        WRITE(3,113)  
            IF(ISTRAN(5).GE.1)THEN  
              IF(N.EQ.1) OPEN(4,FILE=FNJPTOX,STATUS='UNKNOWN')  
              IF(N.EQ.1) CLOSE(4,STATUS='DELETE')  
              IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  
     &          OPEN(4,FILE=FNJPTOX,STATUS='UNKNOWN',POSITION='APPEND')  
              IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  
     &          WRITE(4,114)  
            ENDIF  
            IF(ISTRAN(5).GE.1)THEN  
              IF(N.EQ.1) OPEN(14,FILE=FNJPTPF,STATUS='UNKNOWN')  
              IF(N.EQ.1) CLOSE(14,STATUS='DELETE')  
              IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  
     &          OPEN(14,FILE=FNJPTPF,STATUS='UNKNOWN',POSITION='APPEND')  
              IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  
     &          WRITE(14,124)  
            ENDIF 
          ENDIF
          
          IF(DEBUG)THEN 
            IF(N.EQ.1) THEN  
              OPEN(11,FILE='JPMOMENT.OUT')  
              CLOSE(11,STATUS='DELETE')  
              OPEN(11,FILE='JPMOMENT.OUT')  
            ELSE  
              OPEN(11,FILE='JPMOMENT.OUT',POSITION='APPEND')  
            ENDIF  
          ENDIF
C  
C **  SET INITIAL CONDITIONS  
C  
          DTJP=0.1*RJET/QJET  
C  
C ++  JET/PLUME DISCHARGE, GLOBAL COORDINATES  
C  
          XJG(1)=XJET  
          YJG(1)=YJET  
          ZJG(1)=ZJET(NJP)  
          XJG(2)=XJET  
          YJG(2)=YJET  
          ZJG(2)=ZJET(NJP)  
          SIG(1)=0.  
C  
C ++  INITIALIZE JET DISCHARGE GEOMENTRY  
C  
          RADJ(1)=RJET  
          RLEJ(1)=0.1*RJET  
          RADJ(2)=RJET  
          RLEJ(2)=0.1*RJET  
C  
C ++  JET DISCHARGE VELOCITY MAGNITUDE AND COMPONENTS  
C  
          AJET=RPI*RJET*RJET  
          QJ(1)=QVJET/AJET  
          QJH(1)=QJ(1)*COS(0.0175*PHJET(NJP))  
          UJG(1)=QJ(1)*COS(0.0175*PHJET(NJP))*COS(0.0175*THJET(NJP))  
          VJG(1)=QJ(1)*COS(0.0175*PHJET(NJP))*SIN(0.0175*THJET(NJP))  
          WJG(1)=QJ(1)*SIN(0.0175*PHJET(NJP))  
          UJ0=UJG(1)  
          VJ0=VJG(1)  
          WJ0=WJG(1)  
          QJ(2)=QVJET/AJET  
          QJH(2)=QJ(2)*COS(0.0175*PHJET(NJP))  
          UJG(2)=QJ(2)*COS(0.0175*PHJET(NJP))*COS(0.0175*THJET(NJP))  
          VJG(2)=QJ(2)*COS(0.0175*PHJET(NJP))*SIN(0.0175*THJET(NJP))  
          WJG(2)=QJ(2)*SIN(0.0175*PHJET(NJP))  
C  
C ++ INITIALIZE TIME STEP  
C      DT=0.1*RADJ(NM)/QJ(NM)  
C  
          DTJP=RLEJ(1)/QJ(1)  
C  
C ++  INITIAL JET DENSITY AND MASS  
C  
          SEDJETT=0.  
          DO NS=1,NSED  
            SEDJETT=SEDJETT+SEDJET(NS)  
          ENDDO  
          DO NX=1,NSND  
            SEDJETT=SEDJETT+SNDJET(NX)  
          ENDDO  
          RHOJ(1)=FUNDEN(SALJET,SEDJETT,TEMJET)  
          RMAJP(1)=RPI*RHOJ(1)*RADJ(1)*RADJ(1)*RLEJ(1)  
C  
C ++  INITIAL JET CONCENTRATIONS  
C  
          SALJ(1)=SALJET  
          TEMJ(1)=TEMJET  
          DYEJ(1)=DYEJET  
          SFLJ(1)=SFLJET  
          DO NT=1,NTOX  
            TOXJ(1,NT)=TOXJET(NT)  
          ENDDO  
          DO NS=1,NSED  
            SEDJ(1,NS)=SEDJET(NS)  
          ENDDO  
          DO NX=1,NSND  
            SNDJ(1,NX)=SNDJET(NX)  
          ENDDO  
C  
C ++  INITIAL JET TOXIC CONTAMINANT PARTICULATE FRACTIONS  
C  
          DO NT=1,NTOX  
            IF(ISTRAN(6).GE.1)THEN  
              DO NS=1,NSED  
                TMPEXP=CONPARW(NS,NT)  
                IF(ITXPARW(NS,NT).EQ.0) TMPVAL=1.  
                IF(ITXPARW(NS,NT).EQ.1)THEN  
                  IF(SEDJET(NS).GT.0.) TMPVAL=SEDJET(NX)**TMPEXP  
                ENDIF  
                TOXPFJP(1,NS,NT)=TMPVAL*SEDJET(NS)*TOXPARW(NS,NT)  
              ENDDO  
            ENDIF  
            IF(ISTRAN(7).GE.1)THEN  
              DO NX=1,NSND  
                NS=NX+NSED  
                TMPEXP=CONPARW(NS,NT)  
                IF(ITXPARW(NS,NT).EQ.0) TMPVAL=1.  
                IF(ITXPARW(NS,NT).EQ.1)THEN  
                  IF(SNDJET(NX).GT.0.) TMPVAL=SNDJET(NX)**TMPEXP  
                ENDIF  
                TOXPFJP(1,NS,NT)=TMPVAL*SNDJET(NX)*TOXPARW(NS,NT)  
              ENDDO  
            ENDIF  
          ENDDO  
          DO NT=1,NTOX  
            TOXPFTJP(1,NT)=0.  
          ENDDO  
          DO NT=1,NTOX  
            IF(ISTRAN(6).GE.1)THEN  
              DO NS=1,NSED  
                TOXPFTJP(1,NT)=TOXPFTJP(1,NT)+TOXPFJP(1,NS,NT)  
              ENDDO  
            ENDIF  
            IF(ISTRAN(7).GE.1)THEN  
              DO NX=1,NSND  
                NS=NX+NSED  
                TOXPFTJP(1,NT)=TOXPFTJP(1,NT)+TOXPFJP(1,NS,NT)  
              ENDDO  
            ENDIF  
          ENDDO  
          DO NT=1,NTOX  
            DO NS=1,NSED+NSND  
              TOXPFJP(1,NS,NT)=TOXPFJP(1,NS,NT)/(1.+TOXPFTJP(1,NT))  
            ENDDO  
          ENDDO  
          DO NT=1,NTOX  
            TOXPFTJP(1,NT)=TOXPFTJP(1,NT)/(1.+TOXPFTJP(1,NT))  
          ENDDO  
C  
C ++  INITIALIZE JET ELEMENT 2 VALUE TO ELEMENT 1 VALUES  
C  
          RHOJ(2)=FUNDEN(SALJET,SEDJETT,TEMJET)  
          RMAJP(2)=RPI*RHOJ(2)*RADJ(2)*RADJ(2)*RLEJ(2)  
          SALJ(2)=SALJ(1)  
          TEMJ(2)=TEMJ(1)  
          DYEJ(2)=DYEJ(1)  
          SFLJ(2)=SFLJ(1)  
          DO NT=1,NTOX  
            TOXJ(2,NT)=TOXJ(1,NT)  
          ENDDO  
          DO NS=1,NSED  
            SEDJ(2,NS)=SEDJ(1,NS)  
          ENDDO  
          DO NX=1,NSND  
            SNDJ(2,NX)=SNDJ(1,NX)  
          ENDDO  
          DO NT=1,NTOX  
            DO NS=1,NSED+NSND  
              TOXPFJP(2,NS,NT)=TOXPFJP(1,NS,NT)  
            ENDDO  
          ENDDO  
          DO NT=1,NTOX  
            TOXPFTJP(2,NT)=TOXPFTJP(1,NT)  
          ENDDO  
C  
C ++  INITIALIZE AMBIENT CONDITIONS  
C  
          IF(ISAMB.EQ.0)THEN  
            UAG=UAGD(1,1)  
            VAG=VAGD(1,1)  
            WAG=WAGD(1,1)  
            SALA=SALAD(1,1)  
            TEMA=TEMAD(1,1)  
            DYEA=DYEAD(1,1)  
            SFLA=SFLAD(1,1)  
            DO NT=1,NTOX  
              TOXA(NT)=TOXAD(1,NT,1)  
            ENDDO  
            DO NS=1,NSED  
              SEDA(NS)=SEDAD(1,NS,1)  
            ENDDO  
            DO NX=1,NSND  
              SNDA(NX)=SNDAD(1,NX,1)  
            ENDDO  
          ENDIF  
          IF(ISAMB.GE.1)THEN  
            ZVAL=ZJG(1)  
            ITVAL=1  
            CALL ACON(ITVAL)  
          ENDIF  
C  
C ++  AMBIENT VELOCITY MAGNITUDES  
C  
          QA=SQRT(UAG*UAG+VAG*VAG+WAG*WAG)  
          QAH=SQRT(UAG*UAG+VAG*VAG)  
C  
C ++  AMBIENT DENSITY  
C  
          SEDATT=0.  
          DO NS=1,NSED  
            SEDATT=SEDATT+SEDA(NS)  
          ENDDO  
          DO NX=1,NSND  
            SEDATT=SEDATT+SNDA(NX)  
          ENDDO  
          RHOA=FUNDEN(SALA,SEDATT,TEMA)  
C  
C ++  GLOBAL AMBIENT AND JET ORIENTATIONS  
C  
          PHJ(1)=PHJET(NJP)  
          THJG(1)=THJET(NJP)  
          PHJ(2)=PHJET(NJP)  
          THJG(2)=THJET(NJP)  
          THAG=57.2958*ATAN2(VAG,UAG)  
C  
C ++  LOCAL JET HORIZONTAL ORIENTATION  
C  
          THJL(1)=THJG(1)-THAG  
          THJL(2)=THJG(2)-THAG  
C  
C ++  PROJECTION OF AMBIENT CURRENT TO JET DIRECTION  
C  
          RDQA(1)=COS(0.0175*PHJ(1))*COS(0.0175*THJL(1))*QAH  
          RDQA(2)=COS(0.0175*PHJ(2))*COS(0.0175*THJL(2))*QAH  
C  
C ++  INITIAL SHEAR ENTRAINMENT  
C  
          SINPHJ=SIN(0.0175*PHJ(1))  
          DVEL=ABS(QJ(1)-RDQA(1))  
          DRHO=(RHOA-RHOJ(1))/RHOA  
          FTOP=G*ABS(DRHO)*RADJ(1)  
          FRD2I=0.  
          EBOT=1.  
          ETOP=0.057  
          ENTS=0.  
          IF(DVEL.GT.0.)THEN  
            FRD2I=FTOP/(ALPH2*DVEL*DVEL)  
            EBOT=1.+5.*RDQA(1)/DVEL  
            ETOP=0.057+0.554*SINPHJ*FRD2I  
            ENTS=1.414*ETOP/EBOT  
          ENDIF  
          DRMAJS(1)=2.*DTJP*RPI*RHOA*ENTS*DVEL*RADJ(1)*RLEJ(1)  
          DRMAJS(2)=DRMAJS(1)  
C  
C ++  INITIAL FORCED ENTRAINMENT  
C  
          DRMAJF(1)=0.  
          DRMAJF(2)=0.  
C  
C ++  INITIALIZE VOLUMETRIC ENTRAINEMNT  
C  
          QJTOT=QVJET  
          QJTOTO=QVJET  
C  
C **  OUTPUT INITIAL CONDITIONS  
C  
          RLSCL=1.  
          IF(ISOUT.EQ.1) RLSCL=DJET(NJP)  
          IF(ISOUT.EQ.2) RLSCL=RJET*SQRT(RPI)  
          SALJETI=1.  
          TEMJETI=1.  
          DYEJETI=1.  
          SFLJETI=1.  
          DO NT=1,NTOX  
            TOXJETI(NT)=1.  
          ENDDO  
          DO NS=1,NSED  
            SEDJETI(NS)=1.  
          ENDDO  
          DO NX=1,NSND  
            SNDJETI(NX)=1.  
          ENDDO  
          IF(ISOUT.GE.1)THEN  
            IF(SALJET.GT.0.) SALJETI=1./SALJET  
            IF(TEMJET.GT.0.) TEMJETI=1./TEMJET  
            IF(DYEJET.GT.0.) DYEJETI=1./DYEJET  
            IF(SFLJET.GT.0.) SFLJETI=1./SFLJET  
            DO NT=1,NTOX  
              IF(TOXJET(NT).GT.0.) TOXJETI(NT)=1./TOXJET(NT)  
            ENDDO  
            DO NS=1,NSED  
              IF(SEDJET(NS).GT.0.) SEDJETI(NS)=1./SEDJET(NS)  
            ENDDO  
            DO NX=1,NSND  
              IF(SNDJET(NX).GT.0.) SNDJETI(NX)=1./SNDJET(NX)  
            ENDDO  
          ENDIF  
          NE=1  
          NJE=1  
          DJETI=1./RLSCL  
          XJGNE=DJETI*XJG(NE)  
          YJGNE=DJETI*YJG(NE)  
          ZJGNE=DJETI*ZJG(NE)  
          SIGNE=DJETI*SIG(NE)  
          RADJNE=DJETI*RADJ(NE)  
          RLEJNE=DJETI*RLEJ(NE)  
          SALJNE=SALJETI*SALJ(NE)  
          TEMJNE=TEMJETI*TEMJ(NE)  
          DYEJNE=DYEJETI*DYEJ(NE)  
          SFLJNE=TEMJETI*SFLJ(NE)  
          DO NT=1,NTOX  
            TOXJNE(NT)=TOXJETI(NT)*TOXJ(NE,NT)  
          ENDDO  
          DO NS=1,NSED  
            SEDJNE(NS)=SEDJETI(NS)*SEDJ(NE,NS)  
          ENDDO  
          DO NX=1,NSND  
            SNDJNE(NX)=SNDJETI(NX)*SNDJ(NE,NX)  
          ENDDO  
          ! *** PMC START
          IF(NE.GT.1)THEN
            DRMAJ=RMAJP(NE)-RMAJP(NE-1)  
          ELSE
            DRMAJ=0.
          ENDIF
          DRHO=(RHOA-RHOJ(NE))/RHOA  
          ! *** PMC END
          IF(LOUTJET.AND.(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3))THEN  
            WRITE(1,101)NJP,NJE,TIME,XJGNE,YJGNE,ZJGNE,  
     &          SIGNE,RADJNE,RLEJNE,QJTOT  
            WRITE(2,101)NJP,NJE,TIME,QJ(NE),UJG(NE),VJG(NE),
     &          WJG(NE),RDQA(NE),RMAJP(NE),DRMAJ,DRHO  
            IF(ISTRAN(6).EQ.0.AND.ISTRAN(7).EQ.0)THEN  
              WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE  
            ENDIF  
            IF(ISTRAN(6).EQ.1.AND.ISTRAN(7).EQ.0)THEN  
              WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE,  
     &            (SEDJNE(NS),NS=1,NSED)  
            ENDIF  
            IF(ISTRAN(6).EQ.0.AND.ISTRAN(7).EQ.1)THEN  
              WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE,  
     &            (SNDJNE(NX),NX=1,NSND)  
            ENDIF  
            IF(ISTRAN(6).EQ.1.AND.ISTRAN(7).EQ.1)THEN  
              WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE,  
     &            (SEDJNE(NS),NS=1,NSED),(SNDJNE(NX),NX=1,NSND)  
            ENDIF  
            IF(ISTRAN(5).GE.1)THEN  
              IF(NTOX.LE.8)THEN  
                WRITE(4,101)NJP,NJE,TIME,(TOXJNE(NT),NT=1,NTOX)  
                WRITE(14,101)NJP,NJE,TIME,(TOXPFTJP(NE,NT),NT=1,NTOX)  
              ELSE  
                WRITE(4,101)NJP,NJE,TIME,(TOXJNE(NT),NT=1,8)  
                WRITE(14,101)NJP,NJE,TIME,(TOXPFTJP(NE,NT),NT=1,8)  
                WRITE(4,101)NJP,NJE,TIME,(TOXJNE(NT),NT=9,NTOX)  
                WRITE(14,101)NJP,NJE,TIME,(TOXPFTJP(NE,NT),NT=9,NTOX)  
              ENDIF  
            ENDIF  
          ENDIF
          
          IF(IOUTJP(NJP).GE.2)THEN  
            NJPS=1  
            TIMJP(NJPS)=TIME  
            XJGNS(NJPS)=XJGNE  
            YJGNS(NJPS)=YJGNE  
            ZJGNS(NJPS)=ZJGNE  
            SIGNS(NJPS)=SIGNE  
            RADJNS(NJPS)=RADJNE  
            RLEJNS(NJPS)=RLEJNE  
            QJNS(NJPS)=QJ(NE)  
            UJGNS(NJPS)=UJG(NE)  
            VJGNS(NJPS)=VJG(NE)  
            WJGNS(NJPS)=WJG(NE)  
            RDQANS(NJPS)=RDQA(NE)  
            RMAJPNS(NJPS)=RMAJP(NE)  
            DRMAJNS(NJPS)=DRMAJ  
            DRHONS(NJPS)=DRHO  
            SALJNS(NJPS)=SALJNE  
            TEMJNS(NJPS)=TEMJNE  
            DYEJNS(NJPS)=DYEJNE  
            DO NS=1,NSED  
              SEDJNS(NS,NJPS)=SEDJNE(NS)  
            ENDDO  
            DO NX=1,NSND  
              SNDJNS(NX,NJPS)=SNDJNE(NX)  
            ENDDO  
            DO NT=1,NTOX  
              TOXJNS(NT,NJPS)=TOXJNE(NT)  
              TOXPFTNS(NT,NJPS)=TOXPFTJP(NE,NT)  
            ENDDO  
          ENDIF  
C  
C **  START INTEGRATION  
C  
          DO NJE=2,NJEL(NJP)  
C  
C ++  ESTIMATE NEW CONDITIONS FOR ENTRAINMENT  
C  
            NE=2  
            NM=1  
            RADJ(NE)=RADJ(NM)  
            PHJ(NE)=PHJ(NM)  
            THJG(NE)=THJG(NM)  
            THJL(NE)=THJL(NM)  
            SINPHJ=SIN(0.0175*PHJ(NM))  
            COSPHJ=COS(0.0175*PHJ(NM))  
            COSPHJM=COS(0.0175*PHJ(NM))  
            SINTHJL=SIN(0.0175*THJL(NM))  
            COSTHJL=COS(0.0175*THJL(NM))  
            COSTHJLM=COS(0.0175*THJL(NM))  
            XJOLD=XJG(NM)  
            YJOLD=YJG(NM)  
            ZJOLD=ZJG(NM)  
            RLOLD=RLEJ(NM)  
            RROLD=RADJ(NM)  
C  
C      DT=0.1*RADJ(NM)/QJ(NM)  
C  
            DTJP=RLEJ(NM)/QJ(NM)  
            NI=1  
 1000       CONTINUE  
C  
C ++  CALCULATE SHEAR ENTRAINMENT  
C  
            DRMAJSO=0.5*(DRMAJS(NE)+DRMAJS(NM))  
            DVEL=ABS(QJ(NE)-RDQA(NE))  
            DRHO=(RHOA-RHOJ(NE))/RHOA  
            FTOP=G*ABS(DRHO)*RADJ(NE)  
            FRD2I=0.  
            EBOT=1.  
            ETOP=0.057  
            ENTS=0.  
            IF(DVEL.GT.0.)THEN  
              FRD2I=FTOP/(ALPH2*DVEL*DVEL)  
              EBOT=1.+5.*RDQA(1)/DVEL  
              ETOP=0.057+0.554*SINPHJ*FRD2I  
              ENTS=1.414*ETOP/EBOT  
            ENDIF  
            DRMAJS(NE)=2.*DTJP*RPI*RHOA*ENTS*DVEL*RADJ(NE)*RLEJ(NE)  
            DRMAJSA=0.5*(DRMAJS(NE)+DRMAJS(NM))  
C  
C ++  CALCULATE FORCED ENTRAINMENT  
C  
            DRMAJFO=0.5*(DRMAJF(NE)+DRMAJF(NM))  
C  
C      ENTF1=2.*SQRT( SINPHJ*SINPHJ+SINTHJL*SINTHJL  
C  
            ENTF1=2.*SQRT( 1.-COSPHJ*COSPHJ*COSTHJL*COSTHJL )  
            DELSIG=SIG(NE)-SIG(NM)  
            ENTF2=0.  
            ENTF3=0.  
            IF(DELSIG.GT.0.)THEN  
              ENTF2=RPI*COSPHJ*COSTHJL*(RADJ(NE)-RADJ(NM))/  
     &            (DELSIG)  
              ENTF3=0.5*RPI*RADJ(NE)*(COSPHJ*COSTHJL-COSPHJM*COSTHJLM)/  
     &            (DELSIG)  
            ENDIF  
            ENTF=ENTF1+ENTF2+ENTF3  
            ENTF=MAX(ENTF,0.)  
            DRMAJF(NE)=DTJP*RHOA*RADJ(NE)*RLEJ(NE)*QAH*ENTF  
            IF(NJE.EQ.2.AND.NI.EQ.1)DRMAJF(NE)=0.  
            DRMAJFA=0.5*(DRMAJF(NE)+DRMAJF(NM))  
C  
C ++  TAKE MAX OF SHEAR AND FORCED  
C  
            ISHEAR=0  
            IFORCE=0  
            DRMAJ=DRMAJSA+DRMAJFA  
            IF(ISENT(NJP).EQ.0) DRMAJ=MAX(DRMAJSA,DRMAJFA)  
            IF(DRMAJSA.GT.DRMAJFA) ISHEAR=1  
            IF(DRMAJFA.GT.DRMAJSA) IFORCE=1  
  110 FORMAT(2I6,5E14.5)  
C  
C ++  ADVANCE MASS  
C  
            RMAJP(NE)=RMAJP(NM)+DRMAJ  
            RMAJI=1./RMAJP(NE)  
C  
C ++  ADVANCE CONCENTRATION AND DENSITY  
C  
            SALJ(NE)=RMAJI*( RMAJP(NM)*SALJ(NM)+DRMAJ*SALA )  
            TEMJ(NE)=RMAJI*( RMAJP(NM)*TEMJ(NM)+DRMAJ*TEMA )  
            DYEJ(NE)=RMAJI*( RMAJP(NM)*DYEJ(NM)+DRMAJ*DYEA )  
            SFLJ(NE)=RMAJI*( RMAJP(NM)*SFLJ(NM)+DRMAJ*TEMA )  
            IF(ISTRAN(5).GE.1)THEN  
              DO NT=1,NTOX  
                TOXJ(NE,NT)=RMAJI*(RMAJP(NM)*TOXJ(NM,NT)+DRMAJ*TOXA(NT))
              ENDDO  
            ENDIF  
            IF(ISTRAN(6).GE.1)THEN  
              DO NS=1,NSED  
                SEDJ(NE,NS)=RMAJI*(RMAJP(NM)*SEDJ(NM,NS)+DRMAJ*SEDA(NS))
              ENDDO  
            ENDIF  
            IF(ISTRAN(7).GE.1)THEN  
              DO NX=1,NSND  
                SNDJ(NE,NX)=RMAJI*(RMAJP(NM)*SNDJ(NM,NX)+DRMAJ*SNDA(NX))
              ENDDO  
            ENDIF  
C  
C ++ ADVANCE TOXIC PARTICULATE FRACTION  
C  
            IF(ISTRAN(5).GE.1)THEN  
              DO NT=1,NTOX  
                IF(ISTRAN(6).GE.1)THEN  
                  DO NS=1,NSED  
                    TMPEXP=CONPARW(NS,NT)  
                    IF(ITXPARW(NS,NT).EQ.0) TMPVAL=1.  
                    IF(ITXPARW(NS,NT).EQ.1)THEN  
                      IF(SEDJ(NE,NS).GT.0.) TMPVAL=SEDJ(NE,NS)**TMPEXP  
                    ENDIF  
                    TOXPFJP(NE,NS,NT)=TMPVAL*SEDJ(NE,NS)*TOXPARW(NS,NT)  
                  ENDDO  
                ENDIF  
                IF(ISTRAN(7).GE.1)THEN  
                  DO NX=1,NSND  
                    NS=NX+NSED  
                    TMPEXP=CONPARW(NS,NT)  
                    IF(ITXPARW(NS,NT).EQ.0) TMPVAL=1.  
                    IF(ITXPARW(NS,NT).EQ.1)THEN  
                      IF(SNDJ(NE,NX).GT.0.) TMPVAL=SNDJ(NE,NX)**TMPEXP  
                    ENDIF  
                    TOXPFJP(NE,NS,NT)=TMPVAL*SNDJ(NE,NX)*TOXPARW(NS,NT)  
                  ENDDO  
                ENDIF  
              ENDDO  
              DO NT=1,NTOX  
                TOXPFTJP(NE,NT)=0.  
              ENDDO  
              DO NT=1,NTOX  
                IF(ISTRAN(6).GE.1)THEN  
                  DO NS=1,NSED  
                    TOXPFTJP(NE,NT)=TOXPFTJP(NE,NT)+TOXPFJP(NE,NS,NT)  
                  ENDDO  
                ENDIF  
                IF(ISTRAN(7).GE.1)THEN  
                  DO NX=1,NSND  
                    NS=NX+NSED  
                    TOXPFTJP(NE,NT)=TOXPFTJP(NE,NT)+TOXPFJP(NE,NS,NT)  
                  ENDDO  
                ENDIF  
              ENDDO  
              DO NT=1,NTOX  
                DO NS=1,NSED+NSND  
                TOXPFJP(NE,NS,NT)=TOXPFJP(NE,NS,NT)/(1.+TOXPFTJP(NE,NT))  
                ENDDO  
              ENDDO  
              DO NT=1,NTOX  
                TOXPFTJP(NE,NT)=TOXPFTJP(1,NT)/(1.+TOXPFTJP(NE,NT))  
              ENDDO  
            ENDIF  
C  
C ++ ADVANCE DENSITY  
C  
            SEDJETT=0.  
            DO NS=1,NSED  
              SEDJETT=SEDJETT+SEDJ(NE,NS)  
            ENDDO  
            DO NX=1,NSND  
              SEDJETT=SEDJETT+SNDJ(NE,NX)  
            ENDDO  
            RHOJ(NE)=FUNDEN(SALJ(NE),SEDJETT,TEMJ(NE))  
            DRHO=(RHOA-RHOJ(NE))/RHOA  
C  
C ++  ADVANCE VELOCITY COMPONENTS  
C  
            UJG(NE)=RMAJI*( RMAJP(NM)*UJG(NM)+DRMAJ*UAG )  
            VJG(NE)=RMAJI*( RMAJP(NM)*VJG(NM)+DRMAJ*VAG )  
            WJG(NE)=RMAJI*( RMAJP(NM)*WJG(NM)+DRMAJ*WAG )  
     &          +G*DRHO*DTJP  
C  
C ++  NEW JET COORDINATES  
C  
            DXTMP=DTJP*UJG(NE)  
            DYTMP=DTJP*VJG(NE)  
            DZTMP=DTJP*WJG(NE)  
            XJG(NE)=XJG(NM)+DXTMP  
            YJG(NE)=YJG(NM)+DYTMP  
            ZJG(NE)=ZJG(NM)+DZTMP  
            DS=SQRT( DXTMP*DXTMP+DYTMP*DYTMP+DZTMP*DZTMP )  
            SIG(NE)=SIG(NM)+DS  
C  
C ++  RESET AMBIENT CONDITIONS  
C
            SEDAA=0.0
            IF(ISAMB.EQ.0)THEN  
              UAG=UAGD(1,1)  
              VAG=VAGD(1,1)  
              WAG=WAGD(1,1)  
              SALA=SALAD(1,1)  
              TEMA=TEMAD(1,1)  
              DYEA=DYEAD(1,1)  
              SFLA=SFLAD(1,1)  
              DO NT=1,NTOX  
                TOXA(NT)=TOXAD(1,NT,1)  
              ENDDO  
              DO NS=1,NSED  
                SEDA(NS)=SEDAD(1,NS,1)
                SEDAA=SEDAA+SEDA(NS)  
              ENDDO  
              DO NX=1,NSND  
                SNDA(NX)=SNDAD(1,NX,1)
              	SEDAA=SEDAA+SEDA(NX)  
              ENDDO  
            ENDIF  
            IF(ISAMB.GE.1)THEN  
              ZVAL=ZJG(NE)  
              ITVAL=1  
              CALL ACON(ITVAL)  
            ENDIF  
C  
C ++  AMBIENT VELOCITY MAGNITUDES  
C  
            QA=SQRT(UAG*UAG+VAG*VAG+WAG*WAG)  
            QAH=SQRT(UAG*UAG+VAG*VAG)  
C  
C ++  AMBIENT DENSITY  
C  
            RHOA=FUNDEN(SALA,SEDAA,TEMA)  
C  
C ++  CALCULATE NEW GLOBAL JET VELOCITY PARAMETERS  
C  
            QJ(NE)=SQRT(UJG(NE)*UJG(NE)+VJG(NE)*VJG(NE)+WJG(NE)*WJG(NE))
            QJH(NE)=SQRT(UJG(NE)*UJG(NE)+VJG(NE)*VJG(NE))  
C  
C ++  CALCULATE GLOBAL JET DISCHARGE ORIENTATIONS  
C  
            PHJ(NE)=57.2958*ATAN2(WJG(NE),QJH(NE))  
            THJG(NE)=57.2958*ATAN2(VJG(NE),UJG(NE))  
            THAG=57.2958*ATAN2(VAG,UAG)  
            THJL(NE)=THJG(NE)-THAG  
            SINPHJ=SIN(0.0175*PHJ(NE))  
            COSPHJ=COS(0.0175*PHJ(NE))  
            SINTHJL=SIN(0.0175*THJL(NE))  
            COSTHJL=COS(0.0175*THJL(NE))  
C  
C ++  PROJECTION OF AMBIENT ON JET  
C  
            RDQA(NE)=COS(0.0175*PHJ(NE))*COS(0.0175*THJL(NE))*QAH  
C  
C ++  CALCULATE NEW RADIUS AND ELEMENT LENGTH  
C  
            RLEJ(NE)=QJ(NE)*RLEJ(NM)/QJ(NM)  
            RADJ(NE)=SQRT( RMAJP(NE)/(RPI*RHOJ(NE)*RLEJ(NE)) )  
C  
C ++  CHECK FOR CONVERGENCE CHECK  
C      DRMAJSE=100.  
C      DRMAJFE=100.  
C  
            DRMAJSE=ABS(DRMAJSA-DRMAJSO)  
            DRMAJFE=ABS(DRMAJFA-DRMAJFO)  
            IF(DRMAJSO.GT.0.) DRMAJSE=DRMAJSE/DRMAJSO  
            IF(DRMAJFO.GT.0.) DRMAJFE=DRMAJFE/DRMAJFO  
            ITMP=0  
            IF(DRMAJFE.GT.DMRERM) ITMP=1  
            IF(DRMAJSE.GT.DMRERM) ITMP=1  
            IF(DEBUG.AND.NJE.EQ.2)THEN  
            WRITE(10,620)NJP,NJE,NI,ITMP,DRMAJSA,DRMAJSO,DRMAJFA,DRMAJFO  
            ENDIF  
            IF(DEBUG.AND.ISDJP(NJP).EQ.1)THEN  
              JTMPVAL=MOD(NJE,100)  
              IF(JTMPVAL.EQ.0)THEN  
                WRITE(10,620)NJP,NJE,NI,ITMP,DRMAJSA,DRMAJSO,
     &              DRMAJFA,DRMAJFO  
              ENDIF  
            ENDIF  
C  
C ++  STOP IF MAXIMUM ITERATIONS EXCEEDED  
C  
            IF(NI.GT.NIMAX)THEN  
              KFLAG=1  
              
              IF(DEBUG)WRITE(10,620)NJP,NJE,NI,ITMP,DRMAJSA,DRMAJSO,
     &                              DRMAJFA,DRMAJFO  
              WRITE(6,601)NJE,NI  
              IF(DEBUG)WRITE(10,601)NJE,NI  
              WRITE(8,601)NJE,NI  
              GOTO 2000  
            ENDIF  
C  
C ++  STOP IF JET CENTERLINE PENETRATES SURFACE  
C  
            IF(ISTOP.EQ.1)THEN  
              ZJGTOP=ZJG(NE)  
              IF(ZJGTOP.GT.ZSUR)THEN  
                WRITE(6,6050)NJP,NJE,NI,ZJGTOP,ZSUR  
                IF(DEBUG)WRITE(10,605)NJP,NJE,NI,ZJGTOP,ZSUR  
                IF(DEBUG)WRITE(10,899)NJP,TIME,(QJPENT(K,NJP),K=1,KC)  
                WRITE(8,605)NJP,NJE,NI,ZJGTOP,ZSUR  
                GOTO 2000  
              ENDIF  
            ENDIF  
C  
C ++  STOP IF JET CENTERLINE PENETRATES BOTTOM  
C  
            IF(ISTOP.EQ.1)THEN  
              ZJGBOT=ZJG(NE)  
              IF(ZJGBOT.LT.ZBOT)THEN  
                WRITE(6,6060)NJP,NJE,NI,ZJGBOT,ZBOT  
                IF(DEBUG)WRITE(10,606)NJP,NJE,NI,ZJGBOT,ZBOT  
                IF(DEBUG)WRITE(10,899)NJP,TIME,(QJPENT(K,NJP),K=1,KC)  
                WRITE(8,606)NJP,NJE,NI,ZJGBOT,ZBOT  
                GOTO 2000  
              ENDIF  
            ENDIF  
C  
C ++  STOP IF JET BOUNDARY PENETRATES SURFACE  
C  
            IF(ISTOP.EQ.2)THEN  
              ZJGTOP=ZJG(NE)+RADJ(NE)*COS(0.0175*PHJ(NE))  
              IF(ZJGTOP.GT.ZSUR)THEN  
                WRITE(6,6020)NJP,NJE,NI,ZJGTOP,ZSUR  
                IF(DEBUG)WRITE(10,602)NJP,NJE,NI,ZJGTOP,ZSUR  
                IF(DEBUG)WRITE(10,899)NJP,TIME,(QJPENT(K,NJP),K=1,KC)  
                WRITE(8,602)NJP,NJE,NI,ZJGTOP,ZSUR  
                GOTO 2000  
              ENDIF  
            ENDIF  
C  
C ++  STOP IF JET BOUNDARY PENETRATES BOTTOM  
C  
            IF(ISTOP.EQ.2)THEN  
              ZJGBOT=ZJG(NE)-RADJ(NE)*COS(0.0175*PHJ(NE))  
              IF(ZJGBOT.LT.ZBOT)THEN  
                WRITE(6,6030)NJP,NJE,NI,ZJGBOT,ZBOT  
                IF(DEBUG)WRITE(10,603)NJP,NJE,NI,ZJGBOT,ZBOT  
                IF(DEBUG)WRITE(10,899)NJP,TIME,(QJPENT(K,NJP),K=1,KC)  
                WRITE(8,603)NJP,NJE,NI,ZJGBOT,ZBOT  
                GOTO 2000  
              ENDIF  
            ENDIF  
C
C ++  STOP IF NEUTRAL LEVEL IS REACHED
C
C ++  RISING PLUME
C
            IF(RHOJ(NE).GE.RHOJ(NM))THEN
              DRHOT=(RHOA-RHOJ(NE))/RHOA
              IF(DRHOT.LT.0.)THEN
                WRITE(6,6040)NJP,NJE,NI,ZJG(NE)
                IF(DEBUG)WRITE(10,604)NJP,NJE,NI,ZJG(NE)
                IF(DEBUG)WRITE(10,899)NJP,TIME,(QJPENT(K,NJP),K=1,KC)
                WRITE(8,604)NJP,NJE,NI,ZJG(NE)
                GOTO 2000
              ENDIF
            ENDIF
C
C ++  FALLING PLUME
C
            IF(RHOJ(NE).LT.RHOJ(NM))THEN
              DRHOT=(RHOA-RHOJ(NE))/RHOA
              IF(DRHOT.GT.0.)THEN
                WRITE(6,6040)NJP,NJE,NI,ZJG(NE)
                IF(DEBUG)WRITE(10,604)NJP,NJE,NI,ZJG(NE)
                IF(DEBUG)WRITE(10,899)NJP,TIME,(QJPENT(K,NJP),K=1,KC)
                WRITE(8,604)NJP,NJE,NI,ZJG(NE)
                GOTO 2000
              ENDIF
            ENDIF
C
C ++  RETURN FOR ANOTHER ITERATION  
C  
            IF(ITMP.EQ.1.OR.NI.EQ.1)THEN  
              XJOLD=XJG(NE)  
              YJOLD=YJG(NE)  
              ZJOLD=ZJG(NE)  
              RLOLD=RLEJ(NE)  
              RROLD=RADJ(NE)  
              NI=NI+1  
              GOTO 1000  
            ENDIF  
C  
C ++  WRITE OUTPUT AND PROCEED TO NEXT JET ELEMENT  
C  
            IF(ZJG(NE).GE.ZJPRT)THEN  
              NPRTE=1  
              ZJPRT=ZJPRT+DZPRT  
            ELSE  
              NPRTE=0  
            ENDIF  
            DJETI=1./RLSCL  
            XJGNE=DJETI*XJG(NE)  
            YJGNE=DJETI*YJG(NE)  
            ZJGNE=DJETI*ZJG(NE)  
            SIGNE=DJETI*SIG(NE)  
            RADJNE=DJETI*RADJ(NE)  
            RLEJNE=DJETI*RLEJ(NE)  
            SALJNE=SALJETI*SALJ(NE)  
            TEMJNE=TEMJETI*TEMJ(NE)  
            DYEJNE=DYEJETI*DYEJ(NE)  
            SFLJNE=SFLJETI*SFLJ(NE)  
            DO NT=1,NTOX  
              TOXJNE(NT)=TOXJETI(NT)*TOXJ(NE,NT)  
            ENDDO  
            DO NS=1,NSED  
              SEDJNE(NS)=SEDJETI(NS)*SEDJ(NE,NS)  
            ENDDO  
            DO NX=1,NSND  
              SNDJNE(NX)=SNDJETI(NX)*SNDJ(NE,NX)  
            ENDDO  
C  
C **  CALCULATE ENTRAINMENT  
C  
            QJTOTO=QJTOT  
            QJTOT=QJTOT*RMAJP(NE)/RMAJP(NM)  
            DO K=1,KC  
              ZLOWER=Z(K-1)*HP(LJP)+BELV(LJP)  
              ZUPPER=ZLOWER+DZC(K)*HP(LJP)  
              IF(ZJG(NE).GE.ZLOWER.AND.ZJG(NE).LT.ZUPPER)THEN  
                QJPENT(K,NJP)=QJPENT(K,NJP)+(QJTOT-QJTOTO)  
                IF(RMAJI.GT.0.0)THEN  
                  UJPAVG(K,NJP)=UJG(NE)/RMAJI  
                  VJPAVG(K,NJP)=VJG(NE)/RMAJI  
                  WJPAVG(K,NJP)=WJG(NE)/RMAJI  
                ENDIF  
              ENDIF  
            ENDDO  
            QJPENTT(NJP)=0.0  
            DO K=1,KC  
              QJPENTT(NJP)=QJPENTT(NJP)+QJPENT(K,NJP)  
            ENDDO  
            IF(LOUTJET.AND.NPRTE.EQ.1)THEN  
              IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)THEN  
                WRITE(1,101)NJP,NJE,TIME,XJGNE,YJGNE,ZJGNE,  
     &              SIGNE,RADJNE,RLEJNE,QJTOT  
                DRMAJ=RMAJP(NE)-RMAJP(NE-1)  
                DRHO=(RHOA-RHOJ(NE))/RHOA  
                WRITE(2,101)NJP,NJE,TIME,QJ(NE),UJG(NE),VJG(NE),WJG(NE),  
     &              RDQA(NE),RMAJP(NE),DRMAJ,DRHO  
                IF(ISTRAN(6).EQ.0.AND.ISTRAN(7).EQ.0)THEN  
                  WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE  
                ENDIF  
                IF(ISTRAN(6).EQ.1.AND.ISTRAN(7).EQ.0)THEN  
                  WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE,  
     &                (SEDJNE(NS),NS=1,NSED)  
                ENDIF  
                IF(ISTRAN(6).EQ.0.AND.ISTRAN(7).EQ.1)THEN  
                  WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE,  
     &                (SNDJNE(NX),NX=1,NSND)  
                ENDIF  
                IF(ISTRAN(6).EQ.1.AND.ISTRAN(7).EQ.1)THEN  
                  WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE,  
     &                (SEDJNE(NS),NS=1,NSED),(SNDJNE(NX),NX=1,NSND)  
                ENDIF  
                IF(ISTRAN(5).GE.1)THEN  
                  IF(NTOX.LE.8)THEN  
                    WRITE(4,101)NJP,NJE,TIME,(TOXJNE(NT),NT=1,NTOX)  
                   WRITE(14,101)NJP,NJE,TIME,(TOXPFTJP(NE,NT),NT=1,NTOX)  
                  ELSE  
                    WRITE(4,101)NJP,NJE,TIME,(TOXJNE(NT),NT=1,8)  
                    WRITE(14,101)NJP,NJE,TIME,(TOXPFTJP(NE,NT),NT=1,8)  
                    WRITE(4,101)NJP,NJE,TIME,(TOXJNE(NT),NT=9,NTOX)  
                   WRITE(14,101)NJP,NJE,TIME,(TOXPFTJP(NE,NT),NT=9,NTOX)  
                  ENDIF  
                ENDIF  
              ENDIF  
            ENDIF  
            IF(NPRTE.EQ.1)THEN  
              IF(IOUTJP(NJP).GE.2)THEN  
                NJPS=NJPS+1  
                TIMJP(NJPS)=TIME  
                XJGNS(NJPS)=XJGNE  
                YJGNS(NJPS)=YJGNE  
                ZJGNS(NJPS)=ZJGNE  
                SIGNS(NJPS)=SIGNE  
                RADJNS(NJPS)=RADJNE  
                RLEJNS(NJPS)=RLEJNE  
                QJNS(NJPS)=QJ(NE)  
                UJGNS(NJPS)=UJG(NE)  
                VJGNS(NJPS)=VJG(NE)  
                WJGNS(NJPS)=WJG(NE)  
                RDQANS(NJPS)=RDQA(NE)  
                RMAJPNS(NJPS)=RMAJP(NE)  
                DRMAJNS(NJPS)=DRMAJ  
                DRHONS(NJPS)=DRHO  
                SALJNS(NJPS)=SALJNE  
                TEMJNS(NJPS)=TEMJNE  
                DYEJNS(NJPS)=DYEJNE  
                DO NS=1,NSED  
                  SEDJNS(NS,NJPS)=SEDJNE(NS)  
                ENDDO  
                DO NX=1,NSND  
                  SNDJNS(NX,NJPS)=SNDJNE(NX)  
                ENDDO  
                DO NT=1,NTOX  
                  TOXJNS(NT,NJPS)=TOXJNE(NT)  
                  TOXPFTNS(NT,NJPS)=TOXPFTJP(NE,NT)  
                ENDDO  
              ENDIF  
            ENDIF  
            XJG(NM)=XJG(NE)  
            YJG(NM)=YJG(NE)  
            ZJG(NM)=ZJG(NE)  
            SIG(NM)=SIG(NE)  
            RADJ(NM)=RADJ(NE)  
            RLEJ(NM)=RLEJ(NE)  
            UJG(NM)=UJG(NE)  
            VJG(NM)=VJG(NE)  
            WJG(NM)=WJG(NE)  
            QJ(NM)=QJ(NE)  
            QJH(NM)=QJH(NE)  
            RMAJP(NM)=RMAJP(NE)  
            DRMAJS(NM)=DRMAJS(NE)  
            DRMAJF(NM)=DRMAJF(NE)  
            PHJ(NM)=PHJ(NE)  
            THJG(NM)=THJG(NE)  
            THJL(NM)=THJL(NE)  
            RDQA(NM)=RDQA(NE)  
            SALJ(NM)=SALJ(NE)  
            TEMJ(NM)=TEMJ(NE)  
            DYEJ(NM)=DYEJ(NE)  
            SFLJ(NM)=SFLJ(NE)  
            DO NT=1,NTOX  
              TOXJ(NM,NT)=TOXJ(NE,NT)  
            ENDDO  
            DO NS=1,NSED  
              SEDJ(NM,NS)=SEDJ(NE,NS)  
            ENDDO  
            DO NX=1,NSND  
              SNDJ(NM,NX)=SNDJ(NE,NX)  
            ENDDO  
          ENDDO  ! *** END NJEL ELEMENT INTEGRATION
C  
C **  FINAL OUTPUT OF RESULTS  
C  
 2000     CONTINUE  
          QJTOTO=QJTOT  
          QJTOT=QJTOT*RMAJP(NE)/RMAJP(NM)  
          DO K=1,KC  
            ZLOWER=Z(K-1)*HP(LJP)+BELV(LJP)  
            ZUPPER=ZLOWER+DZC(K)*HP(LJP)  
            IF(ZJG(NE).GE.ZLOWER.AND.ZJG(NE).LT.ZUPPER)THEN  
              QJPENT(K,NJP)=QJPENT(K,NJP)+(QJTOT-QJTOTO)  
            ENDIF  
          ENDDO  
          QJPENTT(NJP)=0.0  
          DO K=1,KC  
            QJPENTT(NJP)=QJPENTT(NJP)+QJPENT(K,NJP)  
          ENDDO
          IF(DEBUG)THEN  
            WRITE(8,898)NJP,TIME,(QJPENT(K,NJP),K=1,KC),QJPENTT(NJP)  
            WRITE(10,898)NJP,TIME,(QJPENT(K,NJP),K=1,KC),QJPENTT(NJP)  
          ENDIF
          !IF(IMOD(NPRT,10).EQ.0)THEN
          !  WRITE(6,898)NJP,TIME,(QJPENT(K,NJP),K=1,KC),QJPENTT(NJP)  
          !ENDIF
          DJETI=1./RLSCL  
          XJGNE=DJETI*XJG(NE)  
          YJGNE=DJETI*YJG(NE)  
          ZJGNE=DJETI*ZJG(NE)  
          SIGNE=DJETI*SIG(NE)  
          RADJNE=DJETI*RADJ(NE)  
          RLEJNE=DJETI*RLEJ(NE)  
          SALJNE=SALJETI*SALJ(NE)  
          TEMJNE=TEMJETI*TEMJ(NE)  
          DYEJNE=DYEJETI*DYEJ(NE)  
          SFLJNE=SFLJETI*SFLJ(NE)  
          DO NT=1,NTOX  
            TOXJNE(NT)=TOXJETI(NT)*TOXJ(NE,NT)  
          ENDDO  
          DO NS=1,NSED  
            SEDJNE(NS)=SEDJETI(NS)*SEDJ(NE,NS)  
          ENDDO  
          DO NX=1,NSND  
            SNDJNE(NX)=SNDJETI(NX)*SNDJ(NE,NX)  
          ENDDO  
          DRMAJ=RMAJP(NE)-RMAJP(NE-1)  
          DRHO=(RHOA-RHOJ(NE))/RHOA  
          IF(LOUTJET.AND.(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3))THEN  
            WRITE(1,101)NJP,NJE,TIME,XJGNE,YJGNE,ZJGNE,  
     &          SIGNE,RADJNE,RLEJNE,QJTOT  
            WRITE(2,101)NJP,NJE,TIME,QJ(NE),UJG(NE),VJG(NE),WJG(NE),  
     &          RDQA(NE),RMAJP(NE),DRMAJ,DRHO  
            IF(ISTRAN(6).EQ.0.AND.ISTRAN(7).EQ.0)THEN  
              WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE  
            ENDIF  
            IF(ISTRAN(6).EQ.1.AND.ISTRAN(7).EQ.0)THEN  
              WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE,  
     &            (SEDJNE(NS),NS=1,NSED)  
            ENDIF  
            IF(ISTRAN(6).EQ.0.AND.ISTRAN(7).EQ.1)THEN  
              WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE,  
     &            (SNDJNE(NX),NX=1,NSND)  
            ENDIF  
            IF(ISTRAN(6).EQ.1.AND.ISTRAN(7).EQ.1)THEN  
              WRITE(3,101)NJP,NJE,TIME,SALJNE,TEMJNE,DYEJNE,  
     &            (SEDJNE(NS),NS=1,NSED),(SNDJNE(NX),NX=1,NSND)  
            ENDIF  
            IF(ISTRAN(5).GE.1)THEN  
              IF(NTOX.LE.8)THEN  
                WRITE(4,101)NJP,NJE,TIME,(TOXJNE(NT),NT=1,NTOX)  
                WRITE(14,101)NJP,NJE,TIME,(TOXPFTJP(NE,NT),NT=1,NTOX)  
              ELSE  
                WRITE(4,101)NJP,NJE,TIME,(TOXJNE(NT),NT=1,8)  
                WRITE(14,101)NJP,NJE,TIME,(TOXPFTJP(NE,NT),NT=1,8)  
                WRITE(4,101)NJP,NJE,TIME,(TOXJNE(NT),NT=9,NTOX)  
                WRITE(14,101)NJP,NJE,TIME,(TOXPFTJP(NE,NT),NT=9,NTOX)  
              ENDIF  
            ENDIF  
          ENDIF  
          IF(IOUTJP(NJP).GE.2)THEN  
            NJPS=NJPS+1  
            TIMJP(NJPS)=TIME  
            XJGNS(NJPS)=XJGNE  
            YJGNS(NJPS)=YJGNE  
            ZJGNS(NJPS)=ZJGNE  
            SIGNS(NJPS)=SIGNE  
            RADJNS(NJPS)=RADJNE  
            RLEJNS(NJPS)=RLEJNE  
            QJNS(NJPS)=QJ(NE)  
            UJGNS(NJPS)=UJG(NE)  
            VJGNS(NJPS)=VJG(NE)  
            WJGNS(NJPS)=WJG(NE)  
            RDQANS(NJPS)=RDQA(NE)  
            RMAJPNS(NJPS)=RMAJP(NE)  
            DRMAJNS(NJPS)=DRMAJ  
            DRHONS(NJPS)=DRHO  
            SALJNS(NJPS)=SALJNE  
            TEMJNS(NJPS)=TEMJNE  
            DYEJNS(NJPS)=DYEJNE  
            DO NS=1,NSED  
              SEDJNS(NS,NJPS)=SEDJNE(NS)  
            ENDDO  
            DO NX=1,NSND  
              SNDJNS(NX,NJPS)=SNDJNE(NX)  
            ENDDO  
            DO NT=1,NTOX  
              TOXJNS(NT,NJPS)=TOXJNE(NT)  
              TOXPFTNS(NT,NJPS)=TOXPFTJP(NE,NT)  
            ENDDO  
          ENDIF  
          IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  CLOSE(1)  
          IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  CLOSE(2)  
          IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  CLOSE(3)  
          IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  CLOSE(4)  
          IF(IOUTJP(NJP).EQ.1.OR.IOUTJP(NJP).EQ.3)  CLOSE(14)  
C  
C **  WRITE OUT SAVED RESULTS IN COMPACT ASCII FORMAT  
C  
          IF(LOUTJET.AND.(IOUTJP(NJP).EQ.2.OR.IOUTJP(NJP).EQ.3))THEN  
            IF(N.EQ.1) OPEN(1,FILE=FNNRFLD,STATUS='UNKNOWN')  
            IF(N.EQ.1) CLOSE(1,STATUS='DELETE')  
            OPEN(1,FILE=FNNRFLD,STATUS='UNKNOWN',POSITION='APPEND')  
            WRITE(1,101)NJP,NJPS,TIME  
            DO NSV=1,NJPS  
              WRITE(1,104)XJGNS(NSV),YJGNS(NSV),ZJGNS(NSV),SIGNS(NSV),  
     &            RADJNS(NSV),UJGNS(NSV),VJGNS(NSV),WJGNS(NSV),QJTOT  
              IF(ISTRAN(6).EQ.0.AND.ISTRAN(7).EQ.0)THEN  
                WRITE(1,104)SALJNS(NSV),TEMJNS(NSV),DYEJNS(NSV)  
              ENDIF  
              IF(ISTRAN(6).GE.1.AND.ISTRAN(7).EQ.0)THEN  
                WRITE(1,104)SALJNS(NSV),TEMJNS(NSV),DYEJNS(NSV),  
     &              (SEDJNS(NS,NSV),NS=1,NSED)  
              ENDIF  
              IF(ISTRAN(6).EQ.0.AND.ISTRAN(7).GE.1)THEN  
                WRITE(1,104)SALJNS(NSV),TEMJNS(NSV),DYEJNS(NSV),  
     &              (SNDJNS(NX,NSV),NX=1,NSND)  
              ENDIF  
              IF(ISTRAN(6).GE.1.AND.ISTRAN(7).GE.1)THEN  
                WRITE(1,104)SALJNS(NSV),TEMJNS(NSV),DYEJNS(NSV),  
     &             (SEDJNS(NS,NSV),NS=1,NSED),(SNDJNS(NX,NSV),NX=1,NSND)  
              ENDIF  
              IF(ISTRAN(5).GE.1)THEN  
                IF(NTOX.LE.8)THEN  
                  WRITE(1,104)(TOXJNS(NT,NSV),NT=1,NTOX)  
                ELSE  
                  WRITE(1,104)(TOXJNS(NT,NSV),NT=1,8)  
                  WRITE(1,104)(TOXJNS(NT,NSV),NT=9,NTOX)  
                ENDIF  
                IF(NTOX.LE.8)THEN  
                  WRITE(1,104)(TOXPFTNS(NT,NSV),NT=1,NTOX)  
                ELSE  
                  WRITE(1,104)(TOXPFTNS(NT,NSV),NT=1,8)  
                  WRITE(1,104)(TOXPFTNS(NT,NSV),NT=8,NTOX)  
                ENDIF  
              ENDIF  
            ENDDO  
            CLOSE(1)  
          ENDIF  
C  
C **  WRITE OUT SAVED RESULTS IN BINARY FORMAT  
C  
          IF(IOUTJP(NJP).EQ.4)THEN  
            IF(N.EQ.1) OPEN(1,FILE=FNNRFLB,FORM='UNFORMATTED')  
            IF(N.EQ.1) CLOSE(1,STATUS='DELETE')  
            OPEN(1,FILE=FNNRFLB,POSITION='APPEND',FORM='UNFORMATTED')  
            WRITE(1)NJP,NJPS,TIME  
            WRITE(1)XJGNS  
            WRITE(1)YJGNS  
            WRITE(1)ZJGNS  
            WRITE(1)SIGNS  
            WRITE(1)RADJNS  
            WRITE(1)UJGNS  
            WRITE(1)VJGNS  
            WRITE(1)WJGNS  
            IF(ISTRAN(1).GE.1)THEN  
              WRITE(1)SALJNS  
            ENDIF  
            IF(ISTRAN(2).GE.1)THEN  
              WRITE(1)TEMJNS  
            ENDIF  
            IF(ISTRAN(3).GE.1)THEN  
              WRITE(1)DYEJNS  
            ENDIF  
            IF(ISTRAN(6).GE.1)THEN  
              WRITE(1)SEDJNS  
            ENDIF  
            IF(ISTRAN(7).GE.1)THEN  
              WRITE(1)SNDJNS  
            ENDIF  
            IF(ISTRAN(5).GE.1)THEN  
              WRITE(1)TOXJNS  
            ENDIF  
            IF(ISTRAN(5).GE.1)THEN  
              WRITE(1)TOXPFTNS  
            ENDIF  
            CLOSE(1)  
          ENDIF  
C  
C **  RELOCATE VOLUME SOURCE  
C  
          ZTMP=(ZJGNE-BELV(LJP))/HP(LJP)  
          ZTMP=FLOAT(KC)*ZTMP  
          KTMP=NINT(ZTMP)  
          KTMP=MAX(1,KTMP)  
          KTMP=MIN(KC,KTMP)  
          IF(KFLAG.EQ.0) KEFFJP(NJP)=KTMP  
          GOTO 9001  
 9000     CONTINUE  
          KEFFJP(NJP)=KQJP(NJP)  
 9001     CONTINUE  
          WRITE(8 ,899)NJP,TIME,(QJPENT(K,NJP),K=1,KC)  
          IF(DEBUG)WRITE(10,899)NJP,TIME,(QJPENT(K,NJP),K=1,KC)  
          IF(DEBUG)THEN
            WRITE(10,135)NJP,TIME,KFLAG,KEFFJP(NJP),KQJP(NJP),
     &             QVJET,QJTOT  
            WRITE(8 ,135)NJP,TIME,KFLAG,KEFFJP(NJP),KQJP(NJP),QVJET,
     &             QJTOT  
     
C          IF(IMOD(NPRT,10).EQ.0)THEN
C            WRITE(6 ,899)NJP,TIME,(QJPENT(K,NJP),K=1,KC)  
C            WRITE(6 ,135)NJP,TIME,KFLAG,KEFFJP(NJP),KQJP(NJP),QVJET,
C     &                   QJTOT  
C          ENDIF
            CLOSE(10)  
          ENDIF
C  
C **  CALCULATION MOMENT INTERFACE QUANTITIES  
C  
          RDUM=0.  
          IF(DEBUG)THEN
            WRITE(11,1110)NJP,N  
            KZERO=0  
            QUJ0=UJ0*QVJET0  
            QVJ0=VJ0*QVJET0  
            QWJ0=WJ0*QVJET0  
           WRITE(11,1111)KZERO,QUJ0,QVJ0,QWJ0,QVJET0,RDUM,RDUM,RDUM,RDUM  
            QENTTMP=QVJET0  
            DO K=1,KEFFJP(NJP)  
              QENTTMP=QENTTMP+QJPENT(K,NJP)  
              QUAG=UAG*QJPENT(K,NJP)  
              QVAG=VAG*QJPENT(K,NJP)  
              QWAG=WAG*QJPENT(K,NJP)  
              WRITE(11,1111)K,UJPAVG(K,NJP),VJPAVG(K,NJP),WJPAVG(K,NJP),  
     &          QENTTMP,QUAG,QVAG,QWAG,QJPENT(K,NJP)  
            ENDDO  
          ENDIF
C  
C **  END LOOP OVER ALL JET/PLUME LOCATIONS  
C  
        ENDIF  
      ENDDO  ! *** END NQJPIJ LOOP
      
      CLOSE(11)  
 1110 FORMAT(/,'NJP,N = ',I5,I10,/)  
 1111 FORMAT(I5,10E14.5)  
  899 FORMAT(' JPENT ',I5,F12.6,12E12.4)  
  898 FORMAT(' FINAL JPENT ',I5,F12.6,12E12.4)  
  100 FORMAT(120X)  
  101 FORMAT(2I6,15E12.4)  
  104 FORMAT(15E12.4)  
  111 FORMAT('    NJ    NE     TIME         XJ          YJ     ',  
     &    '     ZJ         SIG         RAD     ',  
     &    '    LEN')  
  112 FORMAT('    NJ    NE     TIME         QJ          UJ     ',  
     &    '     VJ          WJ         RDQ     ',  
     &    '    RMA         DRM         DRHO')  
  113 FORMAT('    NJ    NE     TIME        SAL         TEM     ',  
     &    ' SED(1,NSD)  SND(1,NSN)')  
  114 FORMAT('    NJ    NE     TIME     TOX(1,NTX)')  
  124 FORMAT('    NJ    NE     TIME    TOXPF(1,NTX)')  
  134 FORMAT(' BEGIN JET/PLUME NJP,TIME = ',I6,F12.5)  
  135 FORMAT(' END JET/PLUME NJP,TIME,KFLAG,KEFFJP,KQJP,QVJET,QVJTOT',  
     &    ' = ',I6,F13.5,3I4,2E12.4)  
  600 FORMAT(' ELEMENT, # INTERATIONS = ',2I6)  
  601 FORMAT(' MAXIMUM ITERATIONS EXCEEDED NE,NI = ',2I6,' !!!!!!!!')  
  602 FORMAT(' JET/PLUME BNDRY PEN SURF NJ,NE,NI,Z,ZS = ',3I6,2F10.2)  
 6020 FORMAT(' JP BDRY PEN SURF NJ,NE,NI,Z,ZS= ',3I5,2F8.2)  
  603 FORMAT(' JET/PLUME BNDRY PEN BOTT NJ,NE,NI,Z,ZB = ',3I6,2F10.2)  
 6030 FORMAT(' JP BDRY PEN BOTT NJ,NE,NI,Z,ZB= ',3I5,2F8.2)  
  604 FORMAT(' JET/PLUME AT NEUTRAL LEVEL NJ,NE,NI,Z  = ',3I6,2F10.2)  
 6040 FORMAT(' JP @ NEUTRAL LEVEL NJ,NE,NI,Z = ',3I5,F10.2)  
  605 FORMAT(' JET/PLUME CTRLN PEN SURF NJ,NE,NI,Z,ZS = ',3I6,2F10.2)  
 6050 FORMAT(' JP CTLN PEN SURF NJ,NE,NI,Z,ZS= ',3I5,2F8.2)  
  606 FORMAT(' JET/PLUME CTRLN PEN BOTT NJ,NE,NI,Z,ZB = ',3I6,2F10.2)  
 6060 FORMAT(' JP CTLN PEN BOTT NJ,NE,NI,Z,ZS= ',3I5,2F8.2)  
  888 FORMAT(A80,/)  
  620 FORMAT('NJ,NE,NI,IT,DS,DSO,DF,DFO = ',4I6,6E13.4)  
      RETURN  
      END  

