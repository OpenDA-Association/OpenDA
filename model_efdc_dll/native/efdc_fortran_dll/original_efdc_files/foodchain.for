C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C
      SUBROUTINE FOODCHAIN(IFINISH)
C
C **  THIS SUBROUTINE IS PART OF  EFDC-FULL VERSION 1.0a 
C
C **  LAST MODIFIED BY JOHN HAMRICK ON 1 NOVEMBER 2001
C
C----------------------------------------------------------------------C
C
C CHANGE RECORD
C DATE MODIFIED     BY                 DATE APPROVED    BY
C
C----------------------------------------------------------------------C
C
C **  SUBROUTINES OUTPUT SPACE AND TIME AVERAGE TOXICS CONCENTRATIONS
C **  FOR FOOD CHAIN MODEL
C
C**********************************************************************C
C
      USE GLOBAL
C
      ! *** DSLLC BEGIN
      INTEGER,ALLOCATABLE,DIMENSION(:)::KBFC  

      REAL,ALLOCATABLE,DIMENSION(:,:)::VALPOCW
      REAL,ALLOCATABLE,DIMENSION(:,:)::TMPVOLW

      REAL,ALLOCATABLE,DIMENSION(:,:)::WTBED
      REAL,ALLOCATABLE,DIMENSION(:,:)::VALPOCB
      REAL,ALLOCATABLE,DIMENSION(:,:)::TMPVOLB

      REAL,ALLOCATABLE,DIMENSION(:,:)::TMPTXWF
      REAL,ALLOCATABLE,DIMENSION(:,:)::TMPTXWC
      REAL,ALLOCATABLE,DIMENSION(:,:)::TMPTXWP
      REAL,ALLOCATABLE,DIMENSION(:,:)::TMPTXBF
      REAL,ALLOCATABLE,DIMENSION(:,:)::TMPTXBC
      REAL,ALLOCATABLE,DIMENSION(:,:)::TMPTXBP
      REAL,ALLOCATABLE,DIMENSION(:,:)::TMPTXBPD

      REAL,ALLOCATABLE,DIMENSION(:,:)::VALBCONC

      REAL,ALLOCATABLE,DIMENSION(:)::TMPDOCW
      REAL,ALLOCATABLE,DIMENSION(:)::TMPPOCW
      REAL,ALLOCATABLE,DIMENSION(:)::TMPDOCB
      REAL,ALLOCATABLE,DIMENSION(:)::TMPPOCB
      REAL,ALLOCATABLE,DIMENSION(:)::VOLFCW
      REAL,ALLOCATABLE,DIMENSION(:)::VOLFCB

      LOGICAL,ALLOCATABLE,DIMENSION(:)::LMASKFC

      REAL,ALLOCATABLE,DIMENSION(:)::FDCHDOCW
      REAL,ALLOCATABLE,DIMENSION(:)::FDCHPOCW
      REAL,ALLOCATABLE,DIMENSION(:)::FDCHDOCB
      REAL,ALLOCATABLE,DIMENSION(:)::FDCHPOCB
C
      REAL,ALLOCATABLE,DIMENSION(:,:)::FDCHTXWF
      REAL,ALLOCATABLE,DIMENSION(:,:)::FDCHTXWC
      REAL,ALLOCATABLE,DIMENSION(:,:)::FDCHTXWP
      REAL,ALLOCATABLE,DIMENSION(:,:)::FDCHTXBF
      REAL,ALLOCATABLE,DIMENSION(:,:)::FDCHTXBC
      REAL,ALLOCATABLE,DIMENSION(:,:)::FDCHTXBP
      REAL,ALLOCATABLE,DIMENSION(:,:)::FDCHTXBD
C
      IF(.NOT.ALLOCATED(KBFC))THEN
        ALLOCATE(KBFC(LCM))  
        ALLOCATE(VALPOCW(LCM,KCM))  
        ALLOCATE(TMPVOLW(LCM,KCM))
  
        ALLOCATE(WTBED(LCM,KBM))  
        ALLOCATE(VALPOCB(LCM,KBM))  
        ALLOCATE(VALBCONC(LCM,KBM))  

        ALLOCATE(TMPVOLB(LCM,KBM))  
        ALLOCATE(TMPTXWF(NFDCHZ,NTXM))  
        ALLOCATE(TMPTXWC(NFDCHZ,NTXM))  
        ALLOCATE(TMPTXWP(NFDCHZ,NTXM))  
        ALLOCATE(TMPTXBF(NFDCHZ,NTXM))  
        ALLOCATE(TMPTXBC(NFDCHZ,NTXM))  
        ALLOCATE(TMPTXBP(NFDCHZ,NTXM))  
        ALLOCATE(TMPTXBPD(NFDCHZ,NTXM))  

        ALLOCATE(TMPDOCW(NFDCHZ))  
        ALLOCATE(TMPPOCW(NFDCHZ))  
        ALLOCATE(TMPDOCB(NFDCHZ))  
        ALLOCATE(TMPPOCB(NFDCHZ))  
        ALLOCATE(VOLFCW(NFDCHZ))  
        ALLOCATE(VOLFCB(NFDCHZ))  

        ALLOCATE(FDCHDOCW(NFDCHZ))  
        ALLOCATE(FDCHPOCW(NFDCHZ))  
        ALLOCATE(FDCHDOCB(NFDCHZ))  
        ALLOCATE(FDCHPOCB(NFDCHZ))  

        ALLOCATE(FDCHTXWF(NFDCHZ,NTXM))  
        ALLOCATE(FDCHTXWC(NFDCHZ,NTXM))  
        ALLOCATE(FDCHTXWP(NFDCHZ,NTXM))  
        ALLOCATE(FDCHTXBF(NFDCHZ,NTXM))  
        ALLOCATE(FDCHTXBC(NFDCHZ,NTXM))  
        ALLOCATE(FDCHTXBP(NFDCHZ,NTXM))  
        ALLOCATE(FDCHTXBD(NFDCHZ,NTXM))  

        ! *** ALLOCATE LOCAL ARRAYS
        KBFC=0
        VALPOCW=0.
        TMPVOLW=0.
  
        WTBED=0.
        VALPOCB=0.
        VALBCONC=0.

        TMPVOLB=0.
        TMPTXWF=0.
        TMPTXWC=0.
        TMPTXWP=0.
        TMPTXBF=0.
        TMPTXBC=0.
        TMPTXBP=0.
        TMPTXBPD=0.

        TMPDOCW=0.
        TMPPOCW=0.
        TMPDOCB=0.
        TMPPOCB=0.
        VOLFCW=0.
        VOLFCB=0.

        FDCHDOCW=0.
        FDCHPOCW=0.
        FDCHDOCB=0.
        FDCHPOCB=0.

        FDCHTXWF=0.
        FDCHTXWC=0.
        FDCHTXWP=0.
        FDCHTXBF=0.
        FDCHTXBC=0.
        FDCHTXBP=0.
        FDCHTXBD=0.
      ENDIF
      ! *** DSLLC END
C
C**********************************************************************C
C
      IF(ISDYNSTP.EQ.0)THEN
        TIME=DT*FLOAT(N)+TCON*TBEGIN
        TIME=TIME/TCON    
      ELSE
        TIME=TIMESEC/TCON 
      ENDIF
C
      IF(IFINISH.EQ.1) GO TO 2000
      IF(JSFDCH.EQ.0)GO TO 1000
C
C      WRITE(8,*)' FIRST ENTRY TO FOODCHAIN.FOR '
C
      IF(DEBUG)THEN
        OPEN(1,FILE='FOODCHAIN.OUT')
        CLOSE(1,STATUS='DELETE')
        OPEN(1,FILE='FOODCHAIN.OUT')
        WRITE(1,121)
        WRITE(1,122)
        WRITE(1,123)
        CLOSE(1)
      ENDIF
C
C     JSFDCH=0
C
      DO M=1,NFDCHZ
        FDCHDOCW(M)=0. 
        FDCHPOCW(M)=0. 
        FDCHDOCB(M)=0.
        FDCHPOCB(M)=0.
      ENDDO
C
      DO NT=1,NTOX
      DO M=1,NFDCHZ
        FDCHTXWF(M,NT)=0.
        FDCHTXWC(M,NT)=0.
        FDCHTXWP(M,NT)=0.  
        FDCHTXBF(M,NT)=0.
        FDCHTXBC(M,NT)=0. 
        FDCHTXBP(M,NT)=0.
C####################################################################################
C RM 05/14/04
C Change to average dry weight PCBs
        FDCHTXBD(M,NT)=0.        
C####################################################################################
      ENDDO
      ENDDO
C
      TIMFDCH=0.0
C
C**********************************************************************C
C
 1000 CONTINUE
C
      TIMFDCH=TIMFDCH+DTSED
C
C      TIMETMP=TIMESEC-TCON*TBEGIN
C      NEQUIVAL=TIMETMP/DT
C      WRITE(8,*)'FOODCHAIN ETSEC,TIMFDCH,DT,N,NE ',TIMETMP,TIMFDCH,
C     &           DTSED,N,NEQUIVAL
C
C **  INITIALIZE VOLUMES AND VOLUME AVERAGES
C
      DO M=1,NFDCHZ
        VOLFCW(M)=0.
        VOLFCB(M)=0.
      ENDDO
C
      DO M=1,NFDCHZ
        TMPDOCW(M)=0.  
        TMPPOCW(M)=0.  
        TMPDOCB(M)=0. 
        TMPPOCB(M)=0.  
      ENDDO
C
      DO NT=1,NTOX
      DO M=1,NFDCHZ
        TMPTXWF(M,NT)=0.
        TMPTXWC(M,NT)=0.  
        TMPTXWP(M,NT)=0.  
        TMPTXBF(M,NT)=0.  
        TMPTXBC(M,NT)=0.  
        TMPTXBP(M,NT)=0. 
C####################################################################################
C RM 05/14/04
C Change to average dry weight PCBs
        TMPTXBPD(M,NT)=0.  
C#################################################################################### 
      ENDDO
      ENDDO
C
C **  INITIALIZE MASK
C
      DO L=2,LA
        LMASKFC(L)=.FALSE.
      ENDDO
C
      DO L=2,LA
        IF(LMASKDRY(L))THEN
          IF(MFDCHZ(L).GT.0)LMASKFC(L)=.TRUE.
        ENDIF
      ENDDO
C
C----------------------------------------------------------------------C
C
C **  VOLUME WEIGHTED AVERAGE OVER WATER COLUMN ZONES
C
C     STDOCW(L,K) HAS UNITS: MG/L OR GM/M**3
C     STPOCW(L,K) AND VALPOCW(L,K) HAVE UNITS: MG/L OR GM/M**3
C
      IF(ISTPOCW.LE.1)THEN
        DO K=1,KC
          DO L=2,LA
            IF(LMASKFC(L)) VALPOCW(L,K)=STPOCW(L,K)
          ENDDO
        ENDDO
      ENDIF
C
      IF(ISTPOCW.GE.2)THEN
        DO K=1,KC
          DO L=2,LA
            IF(LMASKFC(L)) VALPOCW(L,K)=0.
          ENDDO
        ENDDO
        DO NS=1,NSED
          DO K=1,KC
            DO L=2,LA
              IF(LMASKFC(L)) VALPOCW(L,K)=
     &          VALPOCW(L,K)+SED(L,K,NS)*STFPOCW(L,K,NS)
            ENDDO
          ENDDO
        ENDDO
        DO NX=1,NSND
          NS=NX+NSED
          DO K=1,KC
            DO L=2,LA
              IF(LMASKFC(L)) VALPOCW(L,K)=
     &          VALPOCW(L,K)+SND(L,K,NX)*STFPOCW(L,K,NS)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
c     changed to areal weighting from voloume weighting
      DO K=1,KC
      DO L=2,LA
c        IF(LMASKFC(L)) TMPVOLW(L,K)=DXYP(L)*HP(L)*DZC(K)
        IF(LMASKFC(L)) TMPVOLW(L,K)=DXYP(L)*DZC(K)
      ENDDO
      ENDDO
C
      DO K=1,KC
      DO L=2,LA
        IF(LMASKFC(L))THEN
          M=MFDCHZ(L)
          VOLFCW(M)=VOLFCW(M)+TMPVOLW(L,K)
        ENDIF  
      ENDDO
      ENDDO
C
C     TMPTXWF(M,NT)/TMPVOLW HAS UNITS: UG/L OR MG/M**3
C     TMPTXWC(M,NT)/TMPVOLW HAS UNITS: UG/L OR MG/M**3
C     TMPTXWP(M,NT)/TMPVOLW HAS UNITS: UG/MG
C     TMPDOCW(M,NT)/TMPVOLW AND STDOCW(L,K) HAVE UNITS: MG/L OR GM/M**3
C     TMPPOCW(M,NT)/TMPVOLW AND VALPOCW(L,K) HAVE UNITS: MG/L OR GM/M**3
C
      DO K=1,KC
      DO L=2,LA
        IF(LMASKFC(L))THEN
        M=MFDCHZ(L)
        TMPDOCW(M)=TMPDOCW(M)+TMPVOLW(L,K)*STDOCW(L,K)
        TMPPOCW(M)=TMPPOCW(M)+TMPVOLW(L,K)*VALPOCW(L,K)
        ENDIF  
      ENDDO
      ENDDO
C
      DO NT=1,NTOX
      DO K=1,KC
      DO L=2,LA
        IF(LMASKFC(L))THEN
        M=MFDCHZ(L)
        TMPTXWF(M,NT)=TMPTXWF(M,NT)
     &               +TMPVOLW(L,K)*TOXFDFW(L,K,NT)*TOX(L,K,NT)
        TMPTXWC(M,NT)=TMPTXWC(M,NT)
     &               +TMPVOLW(L,K)*TOXCDFW(L,K,NT)*TOX(L,K,NT) 
        IF(VALPOCW(L,K).GT.0.) TMPTXWP(M,NT)=TMPTXWP(M,NT)
     &               +TMPVOLW(L,K)*TOXPFTW(L,K,NT)*TOX(L,K,NT)
     &               /VALPOCW(L,K) 
        ENDIF  
      ENDDO
      ENDDO
      ENDDO
C
      DO M=1,NFDCHZ
        IF(VOLFCW(M).GT.0.0)THEN
          TMPDOCW(M)=TMPDOCW(M)/VOLFCW(M)
          TMPPOCW(M)=TMPPOCW(M)/VOLFCW(M)
        ENDIF
      ENDDO
C

      DO NT=1,NTOX
      DO M=1,NFDCHZ
        IF(VOLFCW(M).GT.0.0)THEN
          TMPTXWF(M,NT)=TMPTXWF(M,NT)/VOLFCW(M)
          TMPTXWC(M,NT)=TMPTXWC(M,NT)/VOLFCW(M)
          TMPTXWP(M,NT)=TMPTXWP(M,NT)/VOLFCW(M)
        ENDIF
      ENDDO
      ENDDO
C
C     CONVERT PARTICULATE FROM UG/MG TO UG/GM
C
      DO NT=1,NTOX
      DO M=1,NFDCHZ
        TMPTXWP(M,NT)=1000.*TMPTXWP(M,NT)
      ENDDO
      ENDDO
C
C----------------------------------------------------------------------C
C
C **  VOLUME WEIGHTED AVERAGE OVER BED ZONES
C
C     STDOCB(L,K) HAS UNITS: MG/L OR GM/M**3 (MASS PER VOLUME OF PORE WATER)
C     STPOCB(L,K) AND VALPOCB(L,K) HAVE UNITS: MG/L OR GM/M**3 (MASS PER TOTAL VOLUME)
C
      IF(ISTPOCB.LE.1)THEN
        DO K=1,KB
          DO L=2,LA
            IF(LMASKFC(L)) VALPOCB(L,K)=STPOCB(L,K)
          ENDDO
        ENDDO
      ENDIF
C
C      IF(ISTPOCB.GE.2)THEN   ! PMC  FPOCB IS NOT INIITALIZED UNTIL ISTPOCB>3
      IF(ISTPOCB.GE.4)THEN
        DO K=1,KB
          DO L=2,LA
            VALPOCB(L,K)=0.
          ENDDO
        ENDDO
        DO NS=1,NSED
          DO K=1,KB
            DO L=2,LA
              IF(LMASKFC(L))THEN
                  IF(K.LE.KBT(L)) VALPOCB(L,K)=VALPOCB(L,K)
C####################################################################################
C RM 05/14/04
C Change to average using data-based foc rather than partitioning foc
c     &                    +SEDB(L,K,NS)*STFPOCB(L,K,NS)/HBED(L,K)  
     &                    +SEDB(L,K,NS)*FPOCB(L,K)/HBED(L,K)  
C####################################################################################
              ENDIF
            ENDDO
          ENDDO
        ENDDO
        DO NX=1,NSND
          NS=NX+NSED
          DO K=1,KB
            DO L=2,LA
              IF(LMASKFC(L))THEN
                IF(K.LE.KBT(L)) VALPOCB(L,K)=VALPOCB(L,K)
C####################################################################################
C RM 05/14/04
C Change to average using data-based foc rather than partitioning foc
C     &                    +SNDB(L,K,NX)*STFPOCB(L,K,NS)/HBED(L,K)
     &                    +SNDB(L,K,NX)*FPOCB(L,K)/HBED(L,K)
C####################################################################################
        ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF

C####################################################################################
C RM 05/14/04
C Change to average dry weight PCB
      DO K=1,KB
        DO L=2,LA
           VALBCONC(L,K)=0.
        ENDDO
      ENDDO
      DO NS=1,NSED
        DO K=1,KB
          DO L=2,LA
            IF(LMASKFC(L))THEN
              IF(K.LE.KBT(L)) VALBCONC(L,K)=VALBCONC(L,K) + SEDB(L,K,NS)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      DO NX=1,NSND
        NS=NX+NSED
        DO K=1,KB
          DO L=2,LA
            IF(LMASKFC(L))THEN
              IF(K.LE.KBT(L)) VALBCONC(L,K)=VALBCONC(L,K) + SNDB(L,K,NX)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C####################################################################################
        
C
      DO K=1,KB
      DO L=2,LA
        WTBED(L,K)=0.0
      ENDDO
      ENDDO
C
      DO L=2,LA
        IF(LMASKFC(L))THEN
          KBFC(L)=KBT(L)
          HBEDTMP=0.0
          KSTOP=0
          DO K=KBT(L),1,-1
            HBEDTMP=HBEDTMP+HBED(L,K)
            IF(HBEDTMP.GT.HBFDCH.AND.KSTOP.EQ.0)THEN
              KBFC(L)=K
              KSTOP=1
              HBSTOP=HBED(L,K)-HBEDTMP+HBFDCH
              WTBED(L,K)=HBSTOP/HBFDCH
            ENDIF
          ENDDO
          KTMP=KBFC(L)+1
          DO K=KTMP,KBT(L)
C####################################################################################
C RM 05/14/04
C Weightages greater than 1 could occur with this method of depth-weighting.
C When the thickness of the top layer is greater than HBFDCH (0.1524
C meters), the weightage assigned to this layer could become greater
C than 1. Need to confirm this with JH.
C####################################################################################
            WTBED(L,K)=HBED(L,K)/HBFDCH
          ENDDO
        ELSE
          KBFC(L)=0 ! *** DSLLC SINGLE LINE
        ENDIF
      ENDDO
C
      IF(JSFDCH.EQ.1.AND.DEBUG)THEN
        OPEN(1,FILE='FOODCHAIN.DIA')
        CLOSE(1,STATUS='DELETE')
        OPEN(1,FILE='FOODCHAIN.DIA')
        DO L=2,LA
          IF(LMASKFC(L))THEN
            WRITE(1,111)IL(L),JL(L),KBFC(L),KBT(L),
     &               (WTBED(L,K),K=KBFC(L),KBT(L))
            WRITE(1,112)(HBED(L,K),K=KBFC(L),KBT(L))
          ENDIF
        ENDDO
        CLOSE(1)
      ENDIF
C
      DO K=1,KB
      DO L=2,LA
        IF(LMASKFC(L))THEN
          IF(K.GE.KBFC(L).AND.K.LE.KBT(L))THEN
            TMPVOLB(L,K)=DXYP(L)*WTBED(L,K)*HBED(L,K)
          ENDIF  
        ENDIF  
      ENDDO
      ENDDO
C
      DO K=1,KB
      DO L=2,LA
        IF(LMASKFC(L))THEN
          M=MFDCHZ(L)
          IF(K.GE.KBFC(L).AND.K.LE.KBT(L))THEN
            VOLFCB(M)=VOLFCB(M)+TMPVOLB(L,K)
          ENDIF  
        ENDIF  
      ENDDO
      ENDDO
C
C     TMPTXBF(M,NT)/TMPVOLB HAS UNITS: UG/L OR MG/M**3  (MASS PER VOLUME PORE WATER)
C     TMPTXBC(M,NT)/TMPVOLB HAS UNITS: UG/L OR MG/M**3  (MASS PER VOLUME PORE WATER)
C     TMPTXWP(M,NT)/TMPVOLB HAS UNITS: UG/MG
C     TMPDOCW(M,NT)/TMPVOLB AND STDOCB(L,K) HAVE UNITS: MG/L OR GM/M**3 (MASS PER VOLUME PORE WATER)
C     TMPPOCW(M,NT)/TMPVOLB AND VALPOCB(L,K) HAVE UNITS: MG/L OR GM/M**3 (MASS PER TOTAL VOLUME)
C
      DO K=KB,1,-1
      DO L=2,LA
        IF(LMASKFC(L))THEN
          M=MFDCHZ(L)
          IF(K.GE.KBFC(L).AND.K.LE.KBT(L))THEN
            TMPDOCB(M)=TMPDOCB(M)+TMPVOLB(L,K)*STDOCB(L,K)
            TMPPOCB(M)=TMPPOCB(M)+TMPVOLB(L,K)*VALPOCB(L,K)
          ENDIF  
        ENDIF  
      ENDDO
      ENDDO
C
      DO NT=1,NTOX
      DO K=KB,1,-1
      DO L=2,LA
        IF(LMASKFC(L))THEN
          M=MFDCHZ(L)
          IF(K.GE.KBFC(L).AND.K.LE.KBT(L))THEN
            TMPVAL=HBED(L,K)*VALPOCB(L,K)
            PORHINV=1.0/(HBED(L,K)*PORBED(L,K))
            TMPTXBF(M,NT)=TMPTXBF(M,NT)
     &                +TMPVOLB(L,K)*PORHINV*TOXFDFB(L,K,NT)*TOXB(L,K,NT)
            TMPTXBC(M,NT)=TMPTXBC(M,NT)
     &                +TMPVOLB(L,K)*PORHINV*TOXCDFB(L,K,NT)*TOXB(L,K,NT)
            IF(TMPVAL.GT.0.) TMPTXBP(M,NT)=TMPTXBP(M,NT)
     &                +TMPVOLB(L,K)*TOXPFTB(L,K,NT)*TOXB(L,K,NT)
     &                /TMPVAL
C####################################################################################
C RM 05/14/04
C Change to average dry weight PCBs
            TMPTXBPD(M,NT)=TMPTXBPD(M,NT) + 
     &                TMPVOLB(L,K)*TOXPFTB(L,K,NT)*TOXB(L,K,NT)
     &                /VALBCONC(L,K)
C####################################################################################
          ENDIF  
        ENDIF  
      ENDDO
      ENDDO
      ENDDO
C
      DO M=1,NFDCHZ
             IF(VOLFCB(M).GT.0.0)THEN
          TMPDOCB(M)=TMPDOCB(M)/VOLFCB(M)
          TMPPOCB(M)=TMPPOCB(M)/VOLFCB(M)
        ENDIF
      ENDDO
C
      DO NT=1,NTOX
      DO M=1,NFDCHZ
        IF(VOLFCB(M).GT.0.0)THEN
          TMPTXBF(M,NT)=TMPTXBF(M,NT)/VOLFCB(M)
          TMPTXBC(M,NT)=TMPTXBC(M,NT)/VOLFCB(M)
          TMPTXBP(M,NT)=TMPTXBP(M,NT)/VOLFCB(M)
C####################################################################################
C RM 05/14/04
C Change to average dry weight PCBs
          TMPTXBPD(M,NT)=TMPTXBPD(M,NT)/VOLFCB(M)
C####################################################################################
        ENDIF
      ENDDO
      ENDDO
C
C     CONVERT PARTICULATE FROM UG/MG TO UG/GM
C
      DO NT=1,NTOX
      DO M=1,NFDCHZ
        TMPTXBP(M,NT)=1000.*TMPTXBP(M,NT)
      ENDDO
      ENDDO
C
C----------------------------------------------------------------------C
C
C **  ACCUMULATE THE TIME AVERAGE
C
      DO M=1,NFDCHZ
        FDCHDOCW(M)=FDCHDOCW(M)+DTSED*TMPDOCW(M)  
        FDCHPOCW(M)=FDCHPOCW(M)+DTSED*TMPPOCW(M)  
        FDCHDOCB(M)=FDCHDOCB(M)+DTSED*TMPDOCB(M) 
        FDCHPOCB(M)=FDCHPOCB(M)+DTSED*TMPPOCB(M)   
      ENDDO
C
      DO NT=1,NTOX
      DO M=1,NFDCHZ
        FDCHTXWF(M,NT)=FDCHTXWF(M,NT)+DTSED*TMPTXWF(M,NT)
        FDCHTXWC(M,NT)=FDCHTXWC(M,NT)+DTSED*TMPTXWC(M,NT)  
        FDCHTXWP(M,NT)=FDCHTXWP(M,NT)+DTSED*TMPTXWP(M,NT)  
        FDCHTXBF(M,NT)=FDCHTXBF(M,NT)+DTSED*TMPTXBF(M,NT)
        FDCHTXBC(M,NT)=FDCHTXBC(M,NT)+DTSED*TMPTXBC(M,NT)  
        FDCHTXBP(M,NT)=FDCHTXBP(M,NT)+DTSED*TMPTXBP(M,NT)  
C####################################################################################
C RM 05/14/04
C Change to average dry weight PCBs
        FDCHTXBD(M,NT)=FDCHTXBD(M,NT)+DTSED*TMPTXBPD(M,NT)  
C####################################################################################
        ENDDO
      ENDDO
C
      JSFDCH=0
C
      IF(TIMFDCH.LT.TFCAVG) RETURN
C
C**********************************************************************C
C
C **  COMPLETE AVERAGING AND OUTPUT RESULTS
C
 2000 CONTINUE
C
C      WRITE(8,*)'ENTRY TO FOODCHAIN OUTPUT'
C      TIMETMP=TIMESEC-TCON*TBEGIN
C      NEQUIVAL=TIMETMP/DT
C      WRITE(8,*)'FOODCHAIN ETSEC,TIMFDCH,DT,N,NE ',TIMETMP,TIMFDCH,
C     &           DTSED,N,NEQUIVAL
C
      FDCHVAL=1./TIMFDCH
      DO M=1,NFDCHZ
        FDCHDOCW(M)=FDCHVAL*FDCHDOCW(M)
        FDCHPOCW(M)=FDCHVAL*FDCHPOCW(M)
        FDCHDOCB(M)=FDCHVAL*FDCHDOCB(M)
        FDCHPOCB(M)=FDCHVAL*FDCHPOCB(M)
      ENDDO
C
      DO NT=1,NTOX
      DO M=1,NFDCHZ
        FDCHTXWF(M,NT)=FDCHVAL*FDCHTXWF(M,NT)
        FDCHTXWC(M,NT)=FDCHVAL*FDCHTXWC(M,NT)
        FDCHTXWP(M,NT)=FDCHVAL*FDCHTXWP(M,NT)
        FDCHTXBF(M,NT)=FDCHVAL*FDCHTXBF(M,NT)
        FDCHTXBC(M,NT)=FDCHVAL*FDCHTXBC(M,NT)
        FDCHTXBP(M,NT)=FDCHVAL*FDCHTXBP(M,NT)
C####################################################################################
C RM 05/14/04
C Change to average dry weight PCBs
        FDCHTXBD(M,NT)=FDCHVAL*FDCHTXBD(M,NT)
C####################################################################################
      ENDDO
      ENDDO
C
      IF(DEBUG)THEN
        OPEN(1,FILE='FOODCHAIN.OUT',POSITION='APPEND')
C
        WRITE(1,101)TIME,NTOX,NFDCHZ,TIMFDCH
C
        DO NT=1,NTOX
          DO M=1,NFDCHZ
            WRITE(1,102)NT,M,
     &                   FDCHTXWF(M,NT),FDCHTXWC(M,NT),FDCHTXWP(M,NT),
     &                   FDCHDOCW(M),FDCHPOCW(M),FDCHTXBF(M,NT),
     &                   FDCHTXBC(M,NT),FDCHTXBP(M,NT),FDCHDOCB(M),
     &                   FDCHPOCB(M),FDCHTXBD(M,NT)
          ENDDO
        ENDDO
C
        CLOSE(1)
      ENDIF
C
C**********************************************************************C
C
C **  INITIALIZE FOR NEXT AVERAGING PERIOD
C
      DO M=1,NFDCHZ
        FDCHDOCW(M)=0. 
        FDCHPOCW(M)=0. 
        FDCHDOCB(M)=0.
        FDCHPOCB(M)=0.
      ENDDO
C
      DO NT=1,NTOX
      DO M=1,NFDCHZ
        FDCHTXWF(M,NT)=0.
        FDCHTXWC(M,NT)=0.
        FDCHTXWP(M,NT)=0.  
        FDCHTXBF(M,NT)=0.
        FDCHTXBC(M,NT)=0. 
        FDCHTXBP(M,NT)=0. 
C####################################################################################
C RM 05/14/04
C Change to average dry weight PCBs
        FDCHTXBD(M,NT)=0.        
C####################################################################################
      ENDDO
      ENDDO

      TIMFDCH=0.0
C
C**********************************************************************C
C
  111 FORMAT(4I5,10F10.4)
  112 FORMAT(20X,10F10.4)
  101 FORMAT(F12.4,2I7,F12.3)
  102 FORMAT(1X,2I6,10E13.5)
  103 FORMAT('              TXWF         TXWC         TXWP',
     &       '         DOCW         POCW         TXBF         TXBC',
     &       '         TXBP (roc)   DOCB         POCB        TXBPD (r)')
  121 FORMAT('DATA: OUTPUT TIME (DAYS), NTOX, NZONES, ',
     &       'AERAGING PERIOD (SECS)')
  122 FORMAT('DATA: NT    NZ   TXWF         TXWC         TXWP',
     &       '         DOCW         POCW         TXBF         TXBC',
     &       '         TXBP (roc)   DOCB         POCB        TXBPD (r)')
  123 FORMAT('DATA:            UG/L         UG/L         UG/GM',
     &       '        MG/L         MG/L         UG/L         UG/L',
     &       '         UG/GM OC     MG/L         MG/L        UG/GM Dry')
C
C**********************************************************************C
C
      RETURN
      END
