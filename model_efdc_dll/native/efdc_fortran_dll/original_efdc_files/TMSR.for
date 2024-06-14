      SUBROUTINE TMSR  
C  
C CHANGE RECORD  
C  CHANGED TOX BED OUTPUT  
C **  SUBROUTINE TMSR WRITES TIME SERIES FILES FOR SURFACE ELEVATON,  
C **  VELOCITY, CONCENTRATION, AND VOLUME SOURCES AT SPECIFIED  
C **  (I,J) POINTS  
C  
      USE GLOBAL  
      USE MPI

      CHARACTER*80 TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6,TITLE7,  
     &    TITLE11,TITLE12,TITLE13,TITLE14,TITLE15,TITLE16,TITLE17,  
     &    TITLE18,TITLE19,TFNTXWT,TFNTXWF,TFNTXWC,TFNTXWP,  
     &    TFNTXBT,TFNTXBF,TFNTXBC,TFNTXBP  
      CHARACTER*10 CTUNIT  
      CHARACTER*1 CZTT(0:9)  
      CHARACTER*1 CCHTMF,CCHTMS  
      CHARACTER*2 CNTOX(25),CNSND(10),CNSBL(10)  
      REAL,ALLOCATABLE,DIMENSION(:)::PORH  
      REAL,ALLOCATABLE,DIMENSION(:)::QCHANUIJ  
      REAL,ALLOCATABLE,DIMENSION(:)::QCHANVIJ  
      REAL,ALLOCATABLE,DIMENSION(:)::SEDSND  
      REAL,ALLOCATABLE,DIMENSION(:)::SEDSNDB  
      REAL,ALLOCATABLE,DIMENSION(:)::TXBC  
      REAL,ALLOCATABLE,DIMENSION(:)::TXBF  
      REAL,ALLOCATABLE,DIMENSION(:)::TXBP  
      REAL,ALLOCATABLE,DIMENSION(:)::TXBT  
      REAL,ALLOCATABLE,DIMENSION(:)::TXWC  
      REAL,ALLOCATABLE,DIMENSION(:)::TXWF  
      REAL,ALLOCATABLE,DIMENSION(:)::TXWP  

      
      if(.not.ALLOCATED(PORH)) ALLOCATE(PORH(KBM))  
      if(.not.ALLOCATED(QCHANUIJ)) ALLOCATE(QCHANUIJ(LCM))  
      if(.not.ALLOCATED(QCHANVIJ)) ALLOCATE(QCHANVIJ(LCM))  
      if(.not.ALLOCATED(SEDSND)) ALLOCATE(SEDSND(KCM))  
      if(.not.ALLOCATED(SEDSNDB)) ALLOCATE(SEDSNDB(KBM))  
      if(.not.ALLOCATED(TXBC)) ALLOCATE(TXBC(KBM))  
      if(.not.ALLOCATED(TXBF)) ALLOCATE(TXBF(KBM))  
      if(.not.ALLOCATED(TXBP)) ALLOCATE(TXBP(KBM))  
      if(.not.ALLOCATED(TXBT)) ALLOCATE(TXBT(KBM))  
      if(.not.ALLOCATED(TXWC)) ALLOCATE(TXWC(KCM))  
      if(.not.ALLOCATED(TXWF)) ALLOCATE(TXWF(KCM))  
      if(.not.ALLOCATED(TXWP)) ALLOCATE(TXWP(KCM)) 

      ! *** INITIALIZE LOCAL ARRAYS
      PORH=0. 
      QCHANUIJ=0.
      QCHANVIJ=0.
      SEDSND=0.
      SEDSNDB=0.
      TXBC=0.
      TXBF=0.
      TXBP=0.
      TXBT=0.
      TXWC=0.
      TXWF=0.
      TXWP=0.
C
      IF(JSTMSR.NE.1) GOTO 300
C
      IF(MLTMSR.GT.MLTMSRM)THEN
        WRITE (6,600)
        STOP
      ENDIF
      IF(MLTMSR.GT.99)THEN
        WRITE (6,601)
        STOP
      ENDIF
C
  600 FORMAT(' NUMBER OF TIME SER LOC, MLTMSR, EXCEEDS DIM, MLTMSRM')
  601 FORMAT(' NUMBER OF TIME SERIES LOCATIONS EXCEED 99')
C
            TAUW1=0.
            TAUW2=0.
c
      CZTT(0)='0'
      CZTT(1)='1'
      CZTT(2)='2'
      CZTT(3)='3'
      CZTT(4)='4'
      CZTT(5)='5'
      CZTT(6)='6'
      CZTT(7)='7'
      CZTT(8)='8'
      CZTT(9)='9'
C
      DO MLTM=1,MLTMSR
      MSDIG=MOD(MLTM,10)
      MTMP=MLTM-MSDIG
      MFDIG=MTMP/10
      CCHTMF=CZTT(MFDIG)
      CCHTMS=CZTT(MSDIG)
      CNTMSR(MLTM)= CCHTMF // CCHTMS
      ENDDO
C
      IF(TCTMSR.EQ.1.) CTUNIT='SECONDS'
      IF(TCTMSR.EQ.60.) CTUNIT='MINUTES'
      IF(TCTMSR.EQ.3600.) CTUNIT='HOURS'
      IF(TCTMSR.EQ.86400.) CTUNIT='DAYS'
C
       CNTOX( 1)= '01'
       CNTOX( 2)= '02'
       CNTOX( 3)= '03'
       CNTOX( 4)= '04'
       CNTOX( 5)= '05'
       CNTOX( 6)= '06'
       CNTOX( 7)= '07'
       CNTOX( 8)= '08'
       CNTOX( 9)= '09'
       CNTOX(10)= '10'
       CNTOX(11)= '11'
       CNTOX(12)= '12'
       CNTOX(13)= '13'
       CNTOX(14)= '14'
       CNTOX(15)= '15'
       CNTOX(16)= '16'
       CNTOX(17)= '17'
       CNTOX(18)= '18'
       CNTOX(19)= '19'
       CNTOX(20)= '20'
       CNTOX(21)= '21'
       CNTOX(22)= '22'
       CNTOX(23)= '23'
       CNTOX(24)= '24'
       CNTOX(25)= '25'
C
       CNSND( 1)= '01'
       CNSND( 2)= '02'
       CNSND( 3)= '03'
       CNSND( 4)= '04'
       CNSND( 5)= '05'
       CNSND( 6)= '06'
       CNSND( 7)= '07'
       CNSND( 8)= '08'
       CNSND( 9)= '09'
       CNSND(10)= '10'
C
       CNSBL( 1)= '01'
       CNSBL( 2)= '02'
       CNSBL( 3)= '03'
       CNSBL( 4)= '04'
       CNSBL( 5)= '05'
       CNSBL( 6)= '06'
C

C **  WRITE HEADINGS
C
      TITLE1=' SALINITY (PSU) TIME SERIES, K=1,KC'
      TITLE2=' TEMPERATURE (DEG C) TIME SERIES, K=1,KC'
      TITLE3=' DYE CONC (KG/M**3) TIME SERIES, K=1,KC'
      TITLE4=' SED CONC (MG/LITER) TIME SERIES, K=1,KC'
      TITLE5=' TOXIC CONC (M/TOT VOL - UG/LITER) 1-4 BED,5-8 WC'
      TITLE6=' VISCOSITY (CM**2/S) TIME SERIES, K=1,KS'
      TITLE7=' DIFFUSIVITY (CM**2/S) TIME SERIES, K=1,KS'
      TITLE11=' SURFACE ELEVATION & DEPTH (METERS) TIME SERIES'
      TITLE12=' EXT MODE E,N VEL (CM/S) TBX TBY TB TBCG TBNG (CM/S)**2'
      TITLE13=' EXT MODE U,V TRANSPORT (M**3/S) TIME SERIES'
      TITLE14=' INT MODE EAST VEL (CM/S) TIME SERIES, K=1,KC'
      TITLE15=' INT MODE NORTH VEL (CM/S) TIME SERIES, K=1,KC'
      TITLE16=' EXT MODE VOLUME S/S (M**3/S) TIME SERIES'
      TITLE17=' INT MODE VOL S/S (M**3/S) TIME SERIES, K=1,KC'
      TITLE18=' SED BED LOAD QSX QSY (GM/S) CQSX CQSY (MG/L) '
      TITLE19=' BED TOP  KBT HBED(KBT) HBED(KBT-1) VOIDR FRAC SED/SND'
      TFNTXWT=' TOTAL TOXIC CONC WATER COL (M/TOT VOL), UG/LITER'
      TFNTXWF=' FREE DIS TOXIC CONC WATER COL (M/TOT VOL), UG/LITER'  
      TFNTXWC=' DOC COMP TOXIC CONC WATER COL (M/TOT VOL), UG/LITER' 
      TFNTXWP=' TOT PART TOXIC CONC WATER COL (M/M), UG/GM' 
      TFNTXBT=' TOTAL TOXIC CONC SED BED (M/TOT VOL), UG/LITER' 
      TFNTXBF=' FREE DIS TOXIC CONC SED BED (M/PORE VOL), UG/LITER' 
      TFNTXBC=' DOC COMP TOXIC CONC SED BED (M/PORE VOL), UG/LITER' 
      TFNTXBP=' TOT PART TOXIC CONC SED BED (M/M), UG/GM' 
C
      IF(ISTMSR.EQ.2)THEN
      DO MLTM=1,MLTMSR
        IF(MTMSRC(MLTM).EQ.1)THEN
          IF(ISTRAN(1).GE.1)THEN
            FNSAL(MLTM)='SALTS' // CNTMSR(MLTM) // '.OUT'
          ENDIF
          IF(ISTRAN(2).GE.1)THEN
            FNTEM(MLTM)='TEMTS' // CNTMSR(MLTM) // '.OUT'
          ENDIF
          IF(ISTRAN(3).GE.1)THEN
            FNDYE(MLTM)='DYETS' // CNTMSR(MLTM) // '.OUT'
          ENDIF
          IF(ISTRAN(4).GE.1)THEN
            FNSFL(MLTM)='SFLTS' // CNTMSR(MLTM) // '.OUT'
          ENDIF
          IF(ISTRAN(6).GE.1)THEN
            FNSED(MLTM)='SEDTS' // CNTMSR(MLTM) // '.OUT'
          ENDIF
C          IF(ISTRAN(7).GE.1)THEN
C            FNSND(MLTM)='SNDTS' // CNTMSR(MLTM) // '.OUT'
C          ENDIF
          IF(ISTRAN(7).GE.1)THEN
            DO NX=1,NSND
            FNSND(MLTM,NX)='SND' // CNSND(NX) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
            FNSBL(MLTM,NX)='SBL' // CNSBL(NX) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
            ENDDO
          ENDIF
          IF(ISTRAN(8).GE.1)THEN
            FNDOX(MLTM)='DOXTS' // CNTMSR(MLTM) // '.OUT'
            FNTOC(MLTM)='TOCTS' // CNTMSR(MLTM) // '.OUT'
            FNNHX(MLTM)='NHXTS' // CNTMSR(MLTM) // '.OUT'
          ENDIF
          IF(ISTRAN(5).GE.1)THEN
            DO NT=1,NTOX
            FNTXWT(MLTM,NT)='TXWT' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
            FNTXWF(MLTM,NT)='TXWF' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
            FNTXWC(MLTM,NT)='TXWC' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
            FNTXWP(MLTM,NT)='TXWP' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
            FNTXBT(MLTM,NT)='TXBT' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
            FNTXBF(MLTM,NT)='TXBF' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
            FNTXBC(MLTM,NT)='TXBC' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
            FNTXBP(MLTM,NT)='TXBP' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
            ENDDO
          ENDIF
        ENDIF
        IF(MTMSRA(MLTM).EQ.1)THEN
          FNAVV(MLTM)='AVVTS' // CNTMSR(MLTM) // '.OUT'
          FNAVB(MLTM)='AVBTS' // CNTMSR(MLTM) // '.OUT'
        ENDIF
        IF(MTMSRP(MLTM).EQ.1)THEN
          FNSEL(MLTM)='SELTS' // CNTMSR(MLTM) // '.OUT'
        ENDIF 
        IF(MTMSRUE(MLTM).EQ.1)THEN
          FNUVE(MLTM)='UVETS' // CNTMSR(MLTM) // '.OUT'
        ENDIF
        IF(MTMSRUT(MLTM).EQ.1)THEN
          FNUVT(MLTM)='UVTTS' // CNTMSR(MLTM) // '.OUT'
        ENDIF
        IF(MTMSRU(MLTM).EQ.1)THEN
          FNU3D(MLTM)='U3DTS' // CNTMSR(MLTM) // '.OUT'
          FNV3D(MLTM)='V3DTS' // CNTMSR(MLTM) // '.OUT'
        ENDIF
        IF(MTMSRQE(MLTM).EQ.1)THEN
          FNQQE(MLTM)='QQETS' // CNTMSR(MLTM) // '.OUT'
        ENDIF
        IF(MTMSRQ(MLTM).EQ.1)THEN
          FNQ3D(MLTM)='Q3DTS' // CNTMSR(MLTM) // '.OUT'
        ENDIF
      ENDDO
      JSTMSR=0
      ENDIF
C
      IF(JSTMSR.EQ.0) GOTO 300
C
      DO MLTM=1,MLTMSR
        IF(MTMSRC(MLTM).EQ.1)THEN
          IF(ISTRAN(1).GE.1)THEN
            FNSAL(MLTM)='SALTS' // CNTMSR(MLTM) // '.OUT'
            IF(MYRANK.EQ.0)THEN
            OPEN(11,FILE=FNSAL(MLTM),STATUS='UNKNOWN')
            CLOSE(11,STATUS='DELETE')
            OPEN(11,FILE=FNSAL(MLTM),STATUS='UNKNOWN')
            WRITE (11,100) TITLE1
            WRITE (11,101) CLTMSR(MLTM)
            WRITE (11,103)ILTMSR(MLTM),JLTMSR(MLTM)
            WRITE (11,102) CTUNIT
            CLOSE(11)
          ENDIF
          ENDIF
          IF(ISTRAN(2).GE.1)THEN
            FNTEM(MLTM)='TEMTS' // CNTMSR(MLTM) // '.OUT'
            IF(MYRANK.EQ.0)THEN
            OPEN(21,FILE=FNTEM(MLTM),STATUS='UNKNOWN')
            CLOSE(21,STATUS='DELETE')
            OPEN(21,FILE=FNTEM(MLTM),STATUS='UNKNOWN')
            WRITE (21,100) TITLE2
            WRITE (21,101) CLTMSR(MLTM)
            WRITE (21,103)ILTMSR(MLTM),JLTMSR(MLTM)
            WRITE (21,102) CTUNIT
            CLOSE(21)
          ENDIF
          ENDIF
          IF(ISTRAN(3).GE.1)THEN
            FNDYE(MLTM)='DYETS' // CNTMSR(MLTM) // '.OUT'
            IF(MYRANK.EQ.0)THEN
            OPEN(31,FILE=FNDYE(MLTM),STATUS='UNKNOWN')
            CLOSE(31,STATUS='DELETE')
            OPEN(31,FILE=FNDYE(MLTM),STATUS='UNKNOWN')
            WRITE (31,100) TITLE3
            WRITE (31,101) CLTMSR(MLTM)
            WRITE (31,103)ILTMSR(MLTM),JLTMSR(MLTM)
            WRITE (31,102) CTUNIT
            CLOSE(31)
          ENDIF
          ENDIF
          IF(ISTRAN(4).GE.1)THEN
            FNDYE(MLTM)='SFLTS' // CNTMSR(MLTM) // '.OUT'
            IF(MYRANK.EQ.0)THEN
            OPEN(31,FILE=FNSFL(MLTM),STATUS='UNKNOWN')
            CLOSE(31,STATUS='DELETE')
            OPEN(31,FILE=FNSFL(MLTM),STATUS='UNKNOWN')
            WRITE (31,100) TITLE3
            WRITE (31,101) CLTMSR(MLTM)
            WRITE (31,103)ILTMSR(MLTM),JLTMSR(MLTM)
            WRITE (31,102) CTUNIT
            CLOSE(31)
          ENDIF
          ENDIF
          IF(ISTRAN(6).GE.1)THEN
            FNSED(MLTM)='SEDTS' // CNTMSR(MLTM) // '.OUT'
            IF(MYRANK.EQ.0)THEN
            OPEN(41,FILE=FNSED(MLTM),STATUS='UNKNOWN')
            CLOSE(41,STATUS='DELETE')
            OPEN(41,FILE=FNSED(MLTM),STATUS='UNKNOWN')
            WRITE (41,100) TITLE4
            WRITE (41,101) CLTMSR(MLTM)
            WRITE (41,103)ILTMSR(MLTM),JLTMSR(MLTM)
            WRITE (41,102) CTUNIT
            CLOSE(41)
          ENDIF
          ENDIF
          IF(ISTRAN(7).GE.1)THEN
            DO NX=1,NSND
            FNSND(MLTM,NX)='SND'// CNSND(NX) // 'TS' // 
     &                        CNTMSR(MLTM) // '.OUT'
            IF(MYRANK.EQ.0)THEN
            OPEN(41,FILE=FNSND(MLTM,NX),STATUS='UNKNOWN')
            CLOSE(41,STATUS='DELETE')
            OPEN(41,FILE=FNSND(MLTM,NX),STATUS='UNKNOWN')
            WRITE (41,100) TITLE4
            WRITE (41,101) CLTMSR(MLTM)
            WRITE (41,103)ILTMSR(MLTM),JLTMSR(MLTM)
            WRITE (41,102) CTUNIT
            CLOSE(41)
            ENDIF
            ENDDO
            DO NX=1,NSND
            FNSBL(MLTM,NX)='SBL'// CNSBL(NX) // 'TS' // 
     &                        CNTMSR(MLTM) // '.OUT'
            IF(MYRANK.EQ.0)THEN
            OPEN(41,FILE=FNSBL(MLTM,NX),STATUS='UNKNOWN')
            CLOSE(41,STATUS='DELETE')
            OPEN(41,FILE=FNSBL(MLTM,NX),STATUS='UNKNOWN')
            WRITE (41,100) TITLE18
            WRITE (41,101) CLTMSR(MLTM)
            WRITE (41,103)ILTMSR(MLTM),JLTMSR(MLTM)
            WRITE (41,102) CTUNIT
            CLOSE(41)
            ENDIF
            ENDDO
          ENDIF
          IF(ISTRAN(6).GE.1.OR.ISTRAN(7).GE.1)THEN
            FNBED(MLTM)='BEDTS' // CNTMSR(MLTM) // '.OUT'
            IF(MYRANK.EQ.0)THEN
            OPEN(41,FILE=FNBED(MLTM),STATUS='UNKNOWN')
            CLOSE(41,STATUS='DELETE')
            OPEN(41,FILE=FNBED(MLTM),STATUS='UNKNOWN')
            WRITE (41,100) TITLE19
            WRITE (41,101) CLTMSR(MLTM)
            WRITE (41,103)ILTMSR(MLTM),JLTMSR(MLTM)
            WRITE (41,102) CTUNIT
            CLOSE(41)
          ENDIF
          ENDIF
          IF(ISTRAN(8).GE.1)THEN
            FNDOX(MLTM)='DOXTS' // CNTMSR(MLTM) // '.OUT'
            IF(MYRANK.EQ.0)THEN
            OPEN(41,FILE=FNDOX(MLTM),STATUS='UNKNOWN')
            CLOSE(41,STATUS='DELETE')
            OPEN(41,FILE=FNDOX(MLTM),STATUS='UNKNOWN')
            WRITE (41,100) TITLE4
            WRITE (41,101) CLTMSR(MLTM)
            WRITE (41,103)ILTMSR(MLTM),JLTMSR(MLTM)
            WRITE (41,102) CTUNIT
            CLOSE(41)
            ENDIF
            FNTOC(MLTM)='TOCTS' // CNTMSR(MLTM) // '.OUT'
            IF(MYRANK.EQ.0)THEN
            OPEN(42,FILE=FNTOC(MLTM),STATUS='UNKNOWN')
            CLOSE(42,STATUS='DELETE')
            OPEN(42,FILE=FNTOC(MLTM),STATUS='UNKNOWN')
            WRITE (42,100) TITLE4
            WRITE (42,101) CLTMSR(MLTM)
            WRITE (42,103)ILTMSR(MLTM),JLTMSR(MLTM)
            WRITE (42,102) CTUNIT
            CLOSE(42)
            ENDIF
            FNNHX(MLTM)='NHXTS' // CNTMSR(MLTM) // '.OUT'
            IF(MYRANK.EQ.0)THEN
            OPEN(43,FILE=FNNHX(MLTM),STATUS='UNKNOWN')
            CLOSE(43,STATUS='DELETE')
            OPEN(43,FILE=FNNHX(MLTM),STATUS='UNKNOWN')
            WRITE (43,100) TITLE4
            WRITE (43,101) CLTMSR(MLTM)
            WRITE (43,103)ILTMSR(MLTM),JLTMSR(MLTM)
            WRITE (43,102) CTUNIT
            CLOSE(43)
          ENDIF
          ENDIF
          IF(ISTRAN(5).GE.1)THEN
            DO NT=1,NTOX
              FNTOX(MLTM,NT)='TOX' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
              IF(MYRANK.EQ.0)THEN
              OPEN(51,FILE=FNTOX(MLTM,NT),STATUS='UNKNOWN')
              CLOSE(51,STATUS='DELETE')
              OPEN(51,FILE=FNTOX(MLTM,NT),STATUS='UNKNOWN')
              WRITE (51,100) TITLE5
              WRITE (51,101) CLTMSR(MLTM)
              WRITE (51,103)ILTMSR(MLTM),JLTMSR(MLTM)
              WRITE (51,102) CTUNIT
              CLOSE(51)
              ENDIF
              FNTXWT(MLTM,NT)='TXWT' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
              IF(MYRANK.EQ.0)THEN
              OPEN(51,FILE=FNTXWT(MLTM,NT),STATUS='UNKNOWN')
              CLOSE(51,STATUS='DELETE')
              OPEN(51,FILE=FNTXWT(MLTM,NT),STATUS='UNKNOWN')
              WRITE (51,100) TFNTXWT
              WRITE (51,101) CLTMSR(MLTM)
              WRITE (51,103)ILTMSR(MLTM),JLTMSR(MLTM)
              WRITE (51,102) CTUNIT
              CLOSE(51)
              ENDIF
              FNTXWF(MLTM,NT)='TXWF' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
              IF(MYRANK.EQ.0)THEN
              OPEN(51,FILE=FNTXWF(MLTM,NT),STATUS='UNKNOWN')
              CLOSE(51,STATUS='DELETE')
              OPEN(51,FILE=FNTXWF(MLTM,NT),STATUS='UNKNOWN')
              WRITE (51,100) TFNTXWF
              WRITE (51,101) CLTMSR(MLTM)
              WRITE (51,103)ILTMSR(MLTM),JLTMSR(MLTM)
              WRITE (51,102) CTUNIT
              CLOSE(51)
              ENDIF
              FNTXWC(MLTM,NT)='TXWC' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
              IF(MYRANK.EQ.0)THEN
              OPEN(51,FILE=FNTXWC(MLTM,NT),STATUS='UNKNOWN')
              CLOSE(51,STATUS='DELETE')
              OPEN(51,FILE=FNTXWC(MLTM,NT),STATUS='UNKNOWN')
              WRITE (51,100) TFNTXWC
              WRITE (51,101) CLTMSR(MLTM)
              WRITE (51,103)ILTMSR(MLTM),JLTMSR(MLTM)
              WRITE (51,102) CTUNIT
              CLOSE(51)
              ENDIF
              FNTXWP(MLTM,NT)='TXWP' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
              IF(MYRANK.EQ.0)THEN
              OPEN(51,FILE=FNTXWP(MLTM,NT),STATUS='UNKNOWN')
              CLOSE(51,STATUS='DELETE')
              OPEN(51,FILE=FNTXWP(MLTM,NT),STATUS='UNKNOWN')
              WRITE (51,100) TFNTXWP
              WRITE (51,101) CLTMSR(MLTM)
              WRITE (51,103)ILTMSR(MLTM),JLTMSR(MLTM)
              WRITE (51,102) CTUNIT
              CLOSE(51)
              ENDIF
              FNTXBT(MLTM,NT)='TXBT' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
              IF(MYRANK.EQ.0)THEN
              OPEN(51,FILE=FNTXBT(MLTM,NT),STATUS='UNKNOWN')
              CLOSE(51,STATUS='DELETE')
              OPEN(51,FILE=FNTXBT(MLTM,NT),STATUS='UNKNOWN')
              WRITE (51,100) TFNTXBT
              WRITE (51,101) CLTMSR(MLTM)
              WRITE (51,103)ILTMSR(MLTM),JLTMSR(MLTM)
              WRITE (51,102) CTUNIT
              CLOSE(51)
              ENDIF
              FNTXBF(MLTM,NT)='TXBF' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
              IF(MYRANK.EQ.0)THEN
              OPEN(51,FILE=FNTXBF(MLTM,NT),STATUS='UNKNOWN')
              CLOSE(51,STATUS='DELETE')
              OPEN(51,FILE=FNTXBF(MLTM,NT),STATUS='UNKNOWN')
              WRITE (51,100) TFNTXBF
              WRITE (51,101) CLTMSR(MLTM)
              WRITE (51,103)ILTMSR(MLTM),JLTMSR(MLTM)
              WRITE (51,102) CTUNIT
              CLOSE(51)
              ENDIF
              FNTXBC(MLTM,NT)='TXBC' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
              IF(MYRANK.EQ.0)THEN
              OPEN(51,FILE=FNTXBC(MLTM,NT),STATUS='UNKNOWN')
              CLOSE(51,STATUS='DELETE')
              OPEN(51,FILE=FNTXBC(MLTM,NT),STATUS='UNKNOWN')
              WRITE (51,100) TFNTXBC
              WRITE (51,101) CLTMSR(MLTM)
              WRITE (51,103)ILTMSR(MLTM),JLTMSR(MLTM)
              WRITE (51,102) CTUNIT
              CLOSE(51)
              ENDIF
              FNTXBP(MLTM,NT)='TXBP' // CNTOX(NT) // 'TS' // 
     &                           CNTMSR(MLTM) // '.OUT'
              IF(MYRANK.EQ.0)THEN
              OPEN(51,FILE=FNTXBP(MLTM,NT),STATUS='UNKNOWN')
              CLOSE(51,STATUS='DELETE')
              OPEN(51,FILE=FNTXBP(MLTM,NT),STATUS='UNKNOWN')
              WRITE (51,100) TFNTXBP
              WRITE (51,101) CLTMSR(MLTM)
              WRITE (51,103)ILTMSR(MLTM),JLTMSR(MLTM)
              WRITE (51,102) CTUNIT
              CLOSE(51)
              ENDIF
            ENDDO
          ENDIF
        ENDIF
        IF(MTMSRA(MLTM).EQ.1)THEN
          FNAVV(MLTM)='AVVTS' // CNTMSR(MLTM) // '.OUT'
          IF(MYRANK.EQ.0)THEN
          OPEN(61,FILE=FNAVV(MLTM),STATUS='UNKNOWN')
          CLOSE(61,STATUS='DELETE')
          OPEN(61,FILE=FNAVV(MLTM),STATUS='UNKNOWN')
          WRITE (61,100) TITLE6
          WRITE (61,101) CLTMSR(MLTM)
          WRITE (61,103)ILTMSR(MLTM),JLTMSR(MLTM)
          WRITE (61,102) CTUNIT
          CLOSE(61)
          ENDIF
          FNAVB(MLTM)='AVBTS' // CNTMSR(MLTM) // '.OUT'
          IF(MYRANK.EQ.0)THEN
          OPEN(71,FILE=FNAVB(MLTM),STATUS='UNKNOWN')
          CLOSE(71,STATUS='DELETE')
          OPEN(71,FILE=FNAVB(MLTM),STATUS='UNKNOWN')
          WRITE (71,100) TITLE7
          WRITE (71,101) CLTMSR(MLTM)
          WRITE (71,103)ILTMSR(MLTM),JLTMSR(MLTM)
          WRITE (71,102) CTUNIT
          CLOSE(71)
        ENDIF
        ENDIF
        IF(MTMSRP(MLTM).EQ.1)THEN
          FNSEL(MLTM)='SELTS' // CNTMSR(MLTM) // '.OUT'
          IF(MYRANK.EQ.0)THEN
          OPEN(11,FILE=FNSEL(MLTM),STATUS='UNKNOWN')
          CLOSE(11,STATUS='DELETE')
          OPEN(11,FILE=FNSEL(MLTM),STATUS='UNKNOWN')
          WRITE (11,100) TITLE11
          WRITE (11,101) CLTMSR(MLTM)
          WRITE (11,103)ILTMSR(MLTM),JLTMSR(MLTM)
          WRITE (11,102) CTUNIT
          CLOSE(11)
        ENDIF 
        ENDIF 
        IF(MTMSRUE(MLTM).EQ.1)THEN
          FNUVE(MLTM)='UVETS' // CNTMSR(MLTM) // '.OUT'
          IF(MYRANK.EQ.0)THEN
          OPEN(21,FILE=FNUVE(MLTM),STATUS='UNKNOWN')
          CLOSE(21,STATUS='DELETE')
          OPEN(21,FILE=FNUVE(MLTM),STATUS='UNKNOWN')
          WRITE (21,100) TITLE12
          WRITE (21,101) CLTMSR(MLTM)
          WRITE (21,103)ILTMSR(MLTM),JLTMSR(MLTM)
          WRITE (21,102) CTUNIT
          CLOSE(21)
        ENDIF
        ENDIF
        IF(MTMSRUT(MLTM).EQ.1)THEN
          FNUVT(MLTM)='UVTTS' // CNTMSR(MLTM) // '.OUT'
          IF(MYRANK.EQ.0)THEN
          OPEN(31,FILE=FNUVT(MLTM),STATUS='UNKNOWN')
          CLOSE(31,STATUS='DELETE')
          OPEN(31,FILE=FNUVT(MLTM),STATUS='UNKNOWN')
          WRITE (31,100) TITLE13
          WRITE (31,101) CLTMSR(MLTM)
          WRITE (31,103)ILTMSR(MLTM),JLTMSR(MLTM)
          WRITE (31,102) CTUNIT
          CLOSE(31)
        ENDIF
        ENDIF
        IF(MTMSRU(MLTM).EQ.1)THEN
          FNU3D(MLTM)='U3DTS' // CNTMSR(MLTM) // '.OUT'
          IF(MYRANK.EQ.0)THEN
          OPEN(41,FILE=FNU3D(MLTM),STATUS='UNKNOWN')
          CLOSE(41,STATUS='DELETE')
          OPEN(41,FILE=FNU3D(MLTM),STATUS='UNKNOWN')
          WRITE (41,100) TITLE14
          WRITE (41,101) CLTMSR(MLTM)
          WRITE (41,103)ILTMSR(MLTM),JLTMSR(MLTM)
          WRITE (41,102) CTUNIT
          CLOSE(41)
          ENDIF
          FNV3D(MLTM)='V3DTS' // CNTMSR(MLTM) // '.OUT'
          IF(MYRANK.EQ.0)THEN
          OPEN(51,FILE=FNV3D(MLTM),STATUS='UNKNOWN')
          CLOSE(51,STATUS='DELETE')
          OPEN(51,FILE=FNV3D(MLTM),STATUS='UNKNOWN')
          WRITE (51,100) TITLE15
          WRITE (51,101) CLTMSR(MLTM)
          WRITE (51,103)ILTMSR(MLTM),JLTMSR(MLTM)
          WRITE (51,102) CTUNIT
          CLOSE(51)
        ENDIF
        ENDIF
        IF(MTMSRQE(MLTM).EQ.1)THEN
          FNQQE(MLTM)='QQETS' // CNTMSR(MLTM) // '.OUT'
          IF(MYRANK.EQ.0)THEN
          OPEN(61,FILE=FNQQE(MLTM),STATUS='UNKNOWN')
          CLOSE(61,STATUS='DELETE')
          OPEN(61,FILE=FNQQE(MLTM),STATUS='UNKNOWN')
          WRITE (61,100) TITLE16
          WRITE (61,101) CLTMSR(MLTM)
          WRITE (61,103)ILTMSR(MLTM),JLTMSR(MLTM)
          WRITE (61,102) CTUNIT
          CLOSE(61)
        ENDIF
        ENDIF
        IF(MTMSRQ(MLTM).EQ.1)THEN
          FNQ3D(MLTM)='Q3DTS' // CNTMSR(MLTM) // '.OUT'
          IF(MYRANK.EQ.0)THEN
          OPEN(71,FILE=FNQ3D(MLTM),STATUS='UNKNOWN')
          CLOSE(71,STATUS='DELETE')
          OPEN(71,FILE=FNQ3D(MLTM),STATUS='UNKNOWN')
          WRITE (71,100) TITLE17
          WRITE (71,101) CLTMSR(MLTM)
          WRITE (71,103)ILTMSR(MLTM),JLTMSR(MLTM)
          WRITE (71,102) CTUNIT
          CLOSE(71)
        ENDIF
        ENDIF
      ENDDO
C
      JSTMSR=0
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
C
C----------------------------------------------------------------------C
C
  300 CONTINUE
C
C----------------------------------------------------------------------C
C
      IF(ISDYNSTP.EQ.0)THEN
        TIME=(DT*FLOAT(N)+TCON*TBEGIN)/TCTMSR
      ELSE
        TIME=TIMESEC/TCTMSR
      ENDIF
C
      FOURDPI=4./PI
C
C **  STEP CURRENT TIME INTERVALS FOR WRITE SCENARIOS
C
      DO NTSSS=1,NTSSTSP
       DO MTSSS=1,MTSSTSP(NTSSS)
        IF(TIME.GE.TSSTRT(MTSSS,NTSSS))THEN
        IF(TIME.LE.TSSTOP(MTSSS,NTSSS))THEN
          MTSCUR(NTSSS)=MTSSS
        ENDIF
        ENDIF
       ENDDO
      ENDDO
C
      IF(MDCHH.GE.1)THEN
	  DO L=2,LA
	    QCHANUIJ(L)=0.0
	    QCHANVIJ(L)=0.0
	  ENDDO
        DO NMD=1,MDCHH
          LMDCHHT=LMDCHH(NMD)
          LMDCHUT=LMDCHU(NMD)
          LMDCHVT=LMDCHV(NMD)
          IF(MDCHTYP(NMD).EQ.1)THEN
            QCHANUIJ(LMDCHHT)=QCHANUIJ(LMDCHHT)+QCHANU(NMD)
            QCHANUIJ(LMDCHUT)=QCHANUIJ(LMDCHUT)-QCHANU(NMD)
          ENDIF
          IF(MDCHTYP(NMD).EQ.2)THEN
            QCHANVIJ(LMDCHHT)=QCHANVIJ(LMDCHHT)+QCHANV(NMD)
            QCHANVIJ(LMDCHVT)=QCHANVIJ(LMDCHVT)-QCHANV(NMD)
          ENDIF
          IF(MDCHTYP(NMD).EQ.3)THEN
            QCHANUIJ(LMDCHHT)=QCHANUIJ(LMDCHHT)+QCHANU(NMD)
            QCHANUIJ(LMDCHUT)=QCHANUIJ(LMDCHUT)-QCHANU(NMD)
            QCHANVIJ(LMDCHHT)=QCHANVIJ(LMDCHHT)+QCHANV(NMD)
            QCHANVIJ(LMDCHVT)=QCHANVIJ(LMDCHVT)-QCHANV(NMD)
          ENDIF
	  ENDDO
	ENDIF
C
      DO MLTM=1,MLTMSR
       NTSSS=NTSSSS(MLTM)
       MTSCC=MTSCUR(NTSSS)
       IF(TIME.GE.TSSTRT(MTSCC,NTSSS))THEN
       IF(TIME.LE.TSSTOP(MTSCC,NTSSS))THEN
        I=ILTMSR(MLTM)
        J=JLTMSR(MLTM)
        L=LIJ(I,J)
        LN=LNC(L)
        IF(ISDOMAIN(L))THEN
        IF(MTMSRC(MLTM).EQ.1)THEN
          IF(ISTRAN(1).GE.1)THEN
            OPEN(11,FILE=FNSAL(MLTM),POSITION='APPEND')
            WRITE(11,201)TIME,(SAL(L,K),K=1,KC)
            CLOSE(11)
          ENDIF
          IF(ISTRAN(2).GE.1)THEN
            OPEN(21,FILE=FNTEM(MLTM),POSITION='APPEND')
            WRITE(21,201)TIME,(TEM(L,K),K=1,KC)
            CLOSE(21)
          ENDIF
          IF(ISTRAN(3).GE.1)THEN
            OPEN(31,FILE=FNDYE(MLTM),POSITION='APPEND')
            WRITE(31,201)TIME,(DYE(L,K),K=1,KC)
            CLOSE(31)
          ENDIF
          IF(ISTRAN(4).GE.1)THEN
            OPEN(31,FILE=FNSFL(MLTM),POSITION='APPEND')
            WRITE(31,201)TIME,(SFL(L,K),K=1,KC)
            CLOSE(31)
          ENDIF
          IF(ISTRAN(6).GE.1)THEN
            OPEN(41,FILE=FNSED(MLTM),POSITION='APPEND')
            IF(ISNDAL.EQ.2)THEN
		    SEDBTMP=SEDBT(L,KBT(L))+SEDBT(L,KBT(L)-1)
	      ELSE
		    SEDBTMP=SEDBT(L,KBT(L))
	      ENDIF
            WRITE(41,201)TIME,SEDBTMP,(SEDT(L,K),K=1,KC)
            CLOSE(41)
          ENDIF
          IF(ISTRAN(7).GE.1)THEN
	      DO NX=1,NSND
            OPEN(41,FILE=FNSND(MLTM,NX),POSITION='APPEND')
            IF(ISNDAL.EQ.2)THEN
		    SNDBTMP=SNDB(L,KBT(L),NX)+SNDB(L,KBT(L)-1,NX)
	      ELSE
		    SNDBTMP=SNDB(L,KBT(L),NX)
	      ENDIF
            WRITE(41,201)TIME,SNDBTMP,(SND(L,K,NX),K=1,KC),
     &                                 SNDEQSAV(L,NX)
            CLOSE(41)
	      ENDDO
	      DO NX=1,NSND
            OPEN(41,FILE=FNSBL(MLTM,NX),POSITION='APPEND')
c            CQBEDLOADX=0.
c            CQBEDLOADY=0.
            IF(UHDYE(L).NE.0.0)CQBEDLOADX(L,NX)=QSBDLDX(L,NX)/UHDYE(L)
            IF(VHDXE(L).NE.0.0)CQBEDLOADY(L,NX)=QSBDLDY(L,NX)/VHDXE(L)
            WRITE(41,201)TIME,QSBDLDX(L,NX),QSBDLDY(L,NX),
     &               CQBEDLOADX(L,NX),CQBEDLOADY(L,NX),SNDFBL(L,NX)
            CLOSE(41)
	      ENDDO
C            OPEN(41,FILE=FNSND(MLTM,NX),POSITION='APPEND')
C            WRITE(41,201)TIME,SNDBT(L,KBT(L)),(SNDT(L,K),K=1,KC)
C            CLOSE(41)
          ENDIF
          IF(ISTRAN(6).GE.1.OR.ISTRAN(7).GE.1)THEN
            OPEN(41,FILE=FNBED(MLTM),POSITION='APPEND')
	      KTMP=KBT(L)
	      KTMP1=KBT(L)-1
	      KTMP1=MAX(KTMP1,1)
	      NSXD=NSED+NSND
c            WRITE(41,221)TIME,KTMP,HBED(L,KTMP),HBED(L,KTMP1),
            WRITE(41,221)TIME,KTMP,HBED(L,KTMP),HBED(L,KTMP1),HBEDA(L),
     &              VDRBED(L,KTMP),(VFRBED(L,KTMP,NX),NX=1,NSXD)
            CLOSE(41)
          ENDIF
          IF(ISTRAN(8).GE.1)THEN
            OPEN(41,FILE=FNDOX(MLTM),POSITION='APPEND')
            WRITE(41,201)TIME,(WQV(L,K,19),K=1,KC)
            CLOSE(41)
            OPEN(42,FILE=FNTOC(MLTM),POSITION='APPEND')
            WRITE(42,201)TIME,(WQV(L,K,6),K=1,KC)
            CLOSE(42)
            OPEN(43,FILE=FNNHX(MLTM),POSITION='APPEND')
            WRITE(43,201)TIME,(WQV(L,K,14),K=1,KC)
            CLOSE(43)
          ENDIF
          IF(ISTRAN(5).GE.1)THEN
            DO NT=1,NTOX
C
              OPEN(51,FILE=FNTOX(MLTM,NT),POSITION='APPEND')
              OPEN(52,FILE=FNTXWT(MLTM,NT),POSITION='APPEND')
              OPEN(53,FILE=FNTXWF(MLTM,NT),POSITION='APPEND')
              OPEN(54,FILE=FNTXWC(MLTM,NT),POSITION='APPEND')
              OPEN(55,FILE=FNTXWP(MLTM,NT),POSITION='APPEND')
              OPEN(56,FILE=FNTXBT(MLTM,NT),POSITION='APPEND')
              OPEN(57,FILE=FNTXBF(MLTM,NT),POSITION='APPEND')
              OPEN(58,FILE=FNTXBC(MLTM,NT),POSITION='APPEND')
              OPEN(59,FILE=FNTXBP(MLTM,NT),POSITION='APPEND')
C
	        NDOC=NSED+NSND+1
              DO K=1,KC
	          SEDSND(K)=SEDT(L,K)+SNDT(L,K)
	          TXWF(K)=TOXFDFW(L,K,NT)*TOX(L,K,NT)
	          TXWC(K)=TOXCDFW(L,K,NT)*TOX(L,K,NT)
	          TXWP(K)=TOXPFTW(L,K,NT)*TOX(L,K,NT)
	        ENDDO
              DO K=1,KB
	          SEDSNDB(K)=SEDBT(L,K)+SNDBT(L,K)
	          TXBF(K)=0.
	          TXBC(K)=0.
	          TXBP(K)=0.
                TXBT(K)=0.
              ENDDO


!{ GeoSR, YSSONG, TOXIC, 101122, TOXIC IN BED
              IF(ISTOXB.EQ.1)THEN
!} GeoSR
              DO K=1,KBT(L)
	          PORH(K)=1.0/PORBED(L,K)
	          TXBF(K)=TOXFDFB(L,K,NT)*TOXB(L,K,NT)/HBED(L,K)
	          TXBC(K)=TOXCDFB(L,K,NT)*TOXB(L,K,NT)/HBED(L,K)
	          TXBP(K)=TOXPFTB(L,K,NT)*TOXB(L,K,NT)/HBED(L,K)
	          TXBT(K)=TOXB(L,K,NT)/HBED(L,K)
	        ENDDO
!{ GeoSR, YSSONG, TOXIC, 101122, TOXIC IN BED
              ENDIF
!} GeoSR

C
	        K=KBT(L)
              WRITE(51,201)TIME,TXBT(K),TXBF(K),TXBC(K),TXBP(K),
     &                           TOX(L,1,NT),TXWF(1),TXWC(1),TXWP(1)
C                WRITE(51,201)TIME,TOXFDFB(L,K,NT),TOXCDFB(L,K,NT),
C     &          TOXPFTB(L,K,NT),TOXFDFW(L,1,NT),TOXCDFW(L,1,NT),
C     &          TOXPFTW(L,1,NT)
C
              DO K=1,KC
	          IF(SEDSND(K).GT.0.0)TXWP(K)=1000.*TXWP(K)/SEDSND(K)
	        ENDDO
C
              DO K=1,KBT(L)
	          TXBP(K)=TXBP(K)*HBED(L,K)
	          TXBF(K)=TXBF(K)*PORH(K)
	          TXBC(K)=TXBC(K)*PORH(K)
	          IF(SEDSNDB(K).GT.0.0)TXBP(K)=1000.*TXBP(K)/SEDSNDB(K)
	        ENDDO
              WRITE(52,201)TIME,(TOX(L,K,NT),K=1,KC)
              WRITE(53,201)TIME,(TXWF(K),K=1,KC)
              WRITE(54,201)TIME,(TXWC(K),K=1,KC)
              WRITE(55,201)TIME,(TXWP(K),K=1,KC)
              WRITE(56,201)TIME,(TXBT(K),K=1,KB)
              WRITE(57,201)TIME,(TXBF(K),K=1,KB)
              WRITE(58,201)TIME,(TXBC(K),K=1,KB)
              WRITE(59,201)TIME,(TXBP(K),K=1,KB)
C
              CLOSE(51)
              CLOSE(52)
              CLOSE(53)
              CLOSE(54)
              CLOSE(55)
              CLOSE(56)
              CLOSE(57)
              CLOSE(58)
              CLOSE(59)
C
            ENDDO
c            DO NT=1,NTOX
c            OPEN(51,FILE=FNTOXPF(MLTM,NT),POSITION='APPEND')
c            WRITE(51,201)TIME,TOXPFTB(L,KBT(L),NT),
c     &                  (TOXPFTW(L,K,NT),K=1,KC)
c            CLOSE(51)
c            ENDDO
c            DO NT=1,NTOX
c            OPEN(51,FILE=FNTOXFD(MLTM,NT),POSITION='APPEND')
c            WRITE(51,201)TIME,TOXPFTB(L,KBT(L),NT),
c     &                  (TOXPFTW(L,K,NT),K=1,KC)
c            CLOSE(51)
c            ENDDO
          ENDIF
        ENDIF
        IF(MTMSRA(MLTM).EQ.1)THEN
          OPEN(61,FILE=FNAVV(MLTM),POSITION='APPEND')
          OPEN(71,FILE=FNAVB(MLTM),POSITION='APPEND')
           DO K=1,KS
           ATMP(K)=10000.*AV(L,K)*HP(L)
           ENDDO
          WRITE(61,201)TIME,(ATMP(K),K=1,KS)
           DO K=1,KS
           ATMP(K)=10000.*AB(L,K)*HP(L)
           ENDDO
          WRITE(71,201)TIME,(ATMP(K),K=1,KS)
          CLOSE(61)
          CLOSE(71)
        ENDIF
        IF(MTMSRP(MLTM).EQ.1)THEN
          OPEN(11,FILE=FNSEL(MLTM),POSITION='APPEND')
          PPTMP=HP(L)+BELV(L)
	    TMPVAL=VDWASTE(L)/DXYP(L)
C          HHTMP=PPTMP-BELV(L)
C          GWELTMP=AGWELV(L)-BELAGW(L)
          WRITE(11,201)TIME,PPTMP,HP(L),BELV(L),HBEDA(L),ZELBEDA(L),
     &          TMPVAL,VDWASTE(L)
          CLOSE(11)
        ENDIF 
        IF(MTMSRUE(MLTM).EQ.1)THEN
          OPEN(21,FILE=FNUVE(MLTM),POSITION='APPEND')
          UTMP1=50.*(UHDYE(L+1)+UHDYE(L))/(DYP(L)*HP(L))
          VTMP1=50.*(VHDXE(LN)+VHDXE(L))/(DXP(L)*HP(L))
          IF(SPB(L).EQ.0)THEN
            UTMP1=2.*UTMP1
            VTMP1=2.*VTMP1
          ENDIF
          UTMP=CUE(L)*UTMP1+CVE(L)*VTMP1
          VTMP=CUN(L)*UTMP1+CVN(L)*VTMP1
          UTMP1=5000.*(TBX(L+1)+TBX(L))
          VTMP1=5000.*(TBY(LN)+TBY(L))
          TBEAST=CUE(L)*UTMP1+CVE(L)*VTMP1
          TBNORT=CUN(L)*UTMP1+CVN(L)*VTMP1
C          TAUBDYN=10000.*QQ(L,0)/CTURB2
          TAUBDYN=10000.*TAUB(L)
c          UTMP=0.5*STCUV(L)*(U(L+1,1)+U(L,1))+1.E-12
c          VTMP=0.5*STCUV(L)*(V(LN,1)+V(L,1))
          TAUB2=TAUBDYN*TAUBDYN
          IF(ISWAVE.GT.0)THEN
            CURANG=ATAN2(VTMP,UTMP)
            TAUW1=10000.*QQWV1(L)
            TAUW2=10000.*QQWV2(L)
            TAUB2=TAUB2+0.5*(TAUW2*TAUW2)
     &           +FOURDPI*TAUBDYN*TAUW2*COS(CURANG-WACCWE(L))
          END IF
          TAUB2=MAX(TAUB2,0.)
          TAUTOT=SQRT(TAUB2)
	    VELMAG=UTMP*UTMP+VTMP*VTMP
	    TMPDRAG=0.0
          TAUBSEDDYN=10000.*TAUBSED(L)
          TAUBSNDDYN=10000.*TAUBSND(L)
	    IF(VELMAG.GT.0.0)TMPDRAG=TAUTOT/VELMAG
C          WRITE(21,201)TIME,UTMP,VTMP,TBEAST,TBNORT,TAUB,TAUW1,
C     &                 TAUW2,TAUTOT,TMPDRAG
          WRITE(21,201)TIME,UTMP,VTMP,TBEAST,TBNORT,TAUBDYN,
     &                 TAUBSEDDYN,TAUBSNDDYN
          CLOSE(21)
        ENDIF
        IF(MTMSRUT(MLTM).EQ.1)THEN
          OPEN(31,FILE=FNUVT(MLTM),POSITION='APPEND')
	    QBEDLOADX=0.
	    QBEDLOADY=0.
          CQBEDLOADXT=0.
          CQBEDLOADYT=0.
          DO NX=1,NSND
            QBEDLOADX=QBEDLOADX+QSBDLDX(L,NX)
            QBEDLOADY=QBEDLOADY+QSBDLDY(L,NX)
            CQBEDLOADXT=CQBEDLOADXT+CQBEDLOADX(L,NX)
            CQBEDLOADYT=CQBEDLOADYT+CQBEDLOADY(L,NX)
	    ENDDO
c          IF(UHDYE(L).NE.0.0)CQBEDLOADX=QBEDLOADX/UHDYE(L)
c          IF(VHDXE(L).NE.0.0)CQBEDLOADY=QBEDLOADY/VHDXE(L)
          WRITE(31,201)TIME,UHDYE(L),VHDXE(L),QBEDLOADX,QBEDLOADY,
     &                 CQBEDLOADXT,CQBEDLOADYT
          CLOSE(31)
        ENDIF
        IF(MTMSRU(MLTM).EQ.1)THEN
          OPEN(41,FILE=FNU3D(MLTM),POSITION='APPEND')
          OPEN(51,FILE=FNV3D(MLTM),POSITION='APPEND')
          RUVTMP=50.
          IF(SPB(L).EQ.0) RUVTMP=100.
          DO K=1,KC
           UTMP1=RUVTMP*(U(L+1,K)+U(L,K))
           VTMP1=RUVTMP*(V(LN,K)+V(L,K))
           ATMP(K)=CUE(L)*UTMP1+CVE(L)*VTMP1
           BTMP(K)=CUN(L)*UTMP1+CVN(L)*VTMP1
          ENDDO
          WRITE(41,201)TIME,(ATMP(K),K=1,KC)
          WRITE(51,201)TIME,(BTMP(K),K=1,KC)
          CLOSE(41)
          CLOSE(51)
        ENDIF
        IF(MTMSRQE(MLTM).EQ.1)THEN
          OPEN(61,FILE=FNQQE(MLTM),POSITION='APPEND')
          QRNT=DXYP(L)*RAINT(L)
          WRITE(61,201)TIME,QSUME(L),QRNT,EVAPSW(L),EVAPGW(L),RIFTR(L),
     &                 QCHANUIJ(L),QCHANVIJ(L),QDWASTE(L),VDWASTE(L)
          CLOSE(61)
        ENDIF
        IF(MTMSRQ(MLTM).EQ.1)THEN
          OPEN(71,FILE=FNQ3D(MLTM),POSITION='APPEND')
          WRITE (71,201)TIME,(QSUM(L,K),K=1,KC)
          CLOSE(71)
        ENDIF
       ENDIF ! ISDOMAIN(L) 
       ENDIF
       ENDIF
      ENDDO  
C
C**********************************************************************C
C
  100 FORMAT(A80)
  101 FORMAT('  AT LOCATION  ',A20)
  102 FORMAT('  TIME IN FIRST COLUMN HAS UNITS OF ',A10)
  103 FORMAT('  CELL I,J = ',2I5)
![GeoSR : 100831
C  201 FORMAT(F12.5,12E12.4)
  201 FORMAT(F12.5,12E13.5)
! GeoSR : 100831]
  221 FORMAT(F12.5,I5,14E12.4)
C
C**********************************************************************C
C
      RETURN
      END
