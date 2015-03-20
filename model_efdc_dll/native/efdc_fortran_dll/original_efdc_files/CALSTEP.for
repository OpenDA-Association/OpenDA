      SUBROUTINE CALSTEP  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALSTEP ESTIMATE THE CURRENT MAXIMUM TIME STEP SIZE  
C **  FORM LINEAR STABILITY CRITERIA AND A FACTOR OF SAFETY  
C  
      USE GLOBAL  

      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DTL1  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DTL2  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DTL3  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::QSUBINN  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::QSUBOUT  
      IF(.NOT.ALLOCATED(DTL1))THEN
        ALLOCATE(DTL1(LCM))
        ALLOCATE(DTL2(LCM))  
        ALLOCATE(DTL3(LCM))  
        ALLOCATE(QSUBINN(LCM,KCM))  
        ALLOCATE(QSUBOUT(LCM,KCM))  
        DTL1=0.0 
        DTL2=0.0 
        DTL3=0.0 
        QSUBINN=0.0 
        QSUBOUT=0.0 
      ENDIF        
C  
      ITRNTMP=0  
      DO NX=1,7  
        ITRNTMP=ITRNTMP+ISTRAN(NX)  
      ENDDO  
      IF(N.LE.0)DTDYN=0.0  
C
      DTMIN=DT  
      DTMAX=TIDALP  
C
      DO L=2,LA  
        DTL1(L)=DTMAX  
        DTL2(L)=DTMAX  
        DTL3(L)=DTMAX  
      ENDDO  
C  
C **  DETERMINE SOURCE/SINKS FOR SUBGRID SCALE CHANNEL EXCHANGES  
C  
      DO K=1,KC  
        DO L=2,LA  
          QSUBOUT(L,K)=0.0  
          QSUBINN(L,K)=0.0  
        ENDDO  
      ENDDO  
      IF(MDCHH.GE.1)THEN  
        DO K=1,KC  
          DO NMD=1,MDCHH  
            LMDCHHT=LMDCHH(NMD)  
            LMDCHUT=LMDCHU(NMD)  
            LMDCHVT=LMDCHV(NMD)  
            IF(MDCHTYP(NMD).EQ.1)THEN  
              QUKTMP=QCHANU(NMD)*DZC(K)  
              QVKTMP=0.  
            ENDIF  
            IF(MDCHTYP(NMD).EQ.2)THEN  
              QVKTMP=QCHANV(NMD)*DZC(K)  
              QUKTMP=0.  
            ENDIF  
            IF(MDCHTYP(NMD).EQ.3)THEN  
              QUKTMP=QCHANU(NMD)*DZC(K)  
              QVKTMP=QCHANV(NMD)*DZC(K)  
            ENDIF  
            QSUBOUT(LMDCHHT,K)=QSUBOUT(LMDCHHT,K)  
     &          +MIN(QUKTMP,0.)  
     &          +MIN(QVKTMP,0.)  
            QSUBINN(LMDCHHT,K)=QSUBINN(LMDCHHT,K)  
     &          +MAX(QUKTMP,0.)  
     &          +MAX(QVKTMP,0.)  
            QSUBOUT(LMDCHUT,K)=QSUBOUT(LMDCHUT,K)  
     &          -MAX(QUKTMP,0.)  
            QSUBINN(LMDCHUT,K)=QSUBINN(LMDCHUT,K)  
     &          -MIN(QUKTMP,0.)  
            QSUBOUT(LMDCHVT,K)=QSUBOUT(LMDCHVT,K)  
     &          -MAX(QVKTMP,0.)  
            QSUBINN(LMDCHVT,K)=QSUBINN(LMDCHVT,K)  
     &          -MIN(QVKTMP,0.)  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  METHOD 1: UPWIND DIFF IN MOMENTUM EQUATIONS  
C  
      DO K=1,KC  
        DO L=2,LA  
          LE=L+1  
          LN=LNC(L)  
          LS=LSC(L)  
          KM=K-1  
          VATUUU=0.25*(V(L,K)+V(L-1,K)+V(LN,K)+V(LN-1,K))  
          TMPUUU=ABS(U(L,K)/DXU(L))+ABS(VATUUU/DYU(L))  
          DTTMP=1./TMPUUU  
          DTL1(L)=MIN(DTL1(L),DTTMP)  
          UATVVV=0.25*(U(L,K)+U(LS,K)+V(L+1,K)+V(LS+1,K))  
          TMPVVV=ABS(V(L,K)/DYV(L))+ABS(UATVVV/DXV(L))  
          DTTMP=1./TMPVVV  
          DTL1(L)=MIN(DTL1(L),DTTMP)  
          UEAST=ABS(U(L,K))  
          UWEST=ABS(U(L+1,K))  
          VSOUTH=ABS(V(L,K))  
          VNORTH=ABS(V(LN,K))  
          TMPVVV=MAX(VSOUTH,VNORTH)  
          TMPUUU=MAX(UEAST,UWEST)  
          TMPVAL=TMPUUU/DXP(L)+TMPVVV/DYP(L)  
          DTTMP=1./TMPVAL  
          DTL1(L)=MIN(DTL1(L),DTTMP)  
        ENDDO  
      ENDDO  
C  
C **  METHOD 2: POSITIVITY OF ADVECTED MATERIAL, DTL2  
C  
      IF(ITRNTMP.GE.1)THEN  
        DO K=1,KC  
          DO L=2,LA  
            LE=L+1  
            LN=LNC(L)  
            LS=LSC(L)  
            KM=K-1  
            TOP=DZC(K)*H1P(L)*DXYP(L)  
            QXPLUS=UHDY2(LE,K)*DZC(K)  
            QXPLUS=MAX(QXPLUS,0.0)  
            QYPLUS=VHDX2(LN,K)*DZC(K)  
            QYPLUS=MAX(QYPLUS,0.0)  
            QZPLUS=W2(L,K)*DXYP(L)  
            QZPLUS=MAX(QZPLUS,0.0)  
            QXMINS=UHDY2(L,K)*DZC(K)  
            QXMINS=-MIN(QXMINS,0.0)  
            QYMINS=VHDX2(L,K)*DZC(K)  
            QYMINS=-MIN(QYMINS,0.0)  
            QZMINS=W2(L,KM)*DXYP(L)  
            QZMINS=-MIN(QZMINS,0.0)  
            QTOTAL=QSUM(L,K)+QSUBOUT(L,K)+QSUBINN(L,K)  
            QSRC=-MIN(QTOTAL,0.0)  
            BOT=QXPLUS+QYPLUS+QZPLUS+QXMINS+QYMINS+QZMINS+QSRC  
            IF(BOT.GT.0.0)THEN  
              DTTMP=TOP/BOT  
              DTL2(L)=MIN(DTL2(L),DTTMP)  
              IF(DTTMP.LT.0.0)THEN  
                WRITE(6,880)IL(L),JL(L),K,TOP,QXPLUS,QYPLUS,QZPLUS,  
     &              QXMINS,QYMINS,QZMINS,QSRC  
                WRITE(8,880)IL(L),JL(L),K,TOP,QXPLUS,QYPLUS,QZPLUS,  
     &              QXMINS,QYMINS,QZMINS,QSRC  
              ENDIF  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  METHOD 3: IMPLICIT BOTTOM FRICTION AND ROTATIONAL ACCELERATION DAM  
C  
      DO L=2,LA  
        TMPVAL=SUB(L)+SUB(L+1)+SVB(L)+SVB(LNC(L))  
        IF(TMPVAL.LT.0.5)THEN  
          LN=LNC(L)  
          TAUBC=QQ(L,0)/CTURB2  
          UCTR=0.5*(U(L,1)+U(L+1,1))  
          VCTR=0.5*(V(L,1)+V(LN,1))  
          UHMAG=HP(L)*SQRT(UCTR*UCTR+VCTR*VCTR)  
          IF(UHMAG.GT.0.0)THEN  
            FRIFRE=TAUBC/UHMAG  
            FRIFRE2=FRIFRE*FRIFRE  
            ACACTMP=(CAC(L,KC)*HPI(L)*DXYIP(L))**2  
            IF(ACACTMP.GT.FRIFRE2)THEN  
              DTTMP=2.*FRIFRE/(ACACTMP-FRIFRE2)  
              DTL3(L)=MIN(DTL3(L),DTTMP)  
            ENDIF  
          ENDIF  
        ENDIF  
      ENDDO  
C  
C **  CHOOSE THE MINIMUM OF THE THREE METHODS  
C  
      DTL1MN=2.*DTMAX  
      DTL2MN=2.*DTMAX  
      DTL3MN=2.*DTMAX  
      DTTMP=2.*DTMAX  
      DO L=2,LA  
        IF(DTL1MN.GT.DTL1(L))THEN  
          DTL1MN=DTL1(L)  
          L1LOC=L  
        ENDIF  
        IF(DTL2MN.GT.DTL2(L))THEN  
          DTL2MN=DTL2(L)  
          L2LOC=L  
        ENDIF  
        IF(DTL3MN.GT.DTL3(L))THEN  
          DTL3MN=DTL3(L)  
          L3LOC=L  
        ENDIF  
      ENDDO  
C
C *** DSLLC BEGIN BLOCK
C
C **  FIND MINIMUM & APPLY A SAFETY FACTOR
C
      DTL1MN=DTL1MN*DTSSFAC
      IF(DTTMP.GT.DTL1MN)THEN
        DTTMP=DTL1MN
        DTCOMP=DTTMP/DTSSFAC
        LLOC=L1LOC
      ENDIF
      DTL2MN=DTL2MN*0.5
      IF(DTTMP.GT.DTL2MN)THEN
        DTTMP=DTL2MN
        DTCOMP=DTTMP/0.5
        LLOC=L2LOC
      ENDIF
      DTL3MN=DTL3MN*DTSSFAC
      IF(DTTMP.GT.DTL3MN)THEN
        DTTMP=DTL3MN
        DTCOMP=DTTMP/DTSSFAC
        LLOC=L3LOC
      ENDIF
      LMINSTEP=LLOC
C *** DSLLC END BLOCK
C  
C **  CHECK IF CURVATURE INSTABILITY IS CONTROLLED  
C      CACDTMX=-1000.  
C **  APPLY A SAFTY FACTOR  
C  
C      DTTMP=DTSSFAC*DTTMP     pmc delete these lines after checking!!
C  
C **  MAKE A MULTIPLE OF OF DTMIN  
C  
      TIMEDAY=TIMESEC/86400.  
      IF(DTCOMP.LT.DTMIN)THEN   ! *** DSLLC SINGLE LINE
        WRITE(8,800)TIMEDAY,DTTMP,DTMIN,IL(LLOC),JL(LLOC)  
        WRITE(6,800)TIMEDAY,DTTMP,DTMIN,IL(LLOC),JL(LLOC)  
        WRITE(8,801)IL(L1LOC),JL(L1LOC),DTL1MN  
        WRITE(6,801)IL(L1LOC),JL(L1LOC),DTL1MN  
        WRITE(8,802)IL(L2LOC),JL(L2LOC),DTL2MN  
        WRITE(6,802)IL(L2LOC),JL(L2LOC),DTL2MN  
        WRITE(8,803)IL(L3LOC),JL(L3LOC),DTL3MN  
        WRITE(6,803)IL(L3LOC),JL(L3LOC),DTL3MN  
        DTTMP=DTMIN  
C *** DSLLC BEGIN BLOCK
      ELSEIF(DTTMP.LT.DTMIN)THEN
        DTWARN=DTTMP
        DTTMP=DTMIN
C *** DSLLC END BLOCK
      ELSE  
        TMPVAL=DTTMP/DTMIN  
        ITMPR=NINT(TMPVAL)  
        RTMPR=FLOAT(ITMPR)  
        IF(RTMPR.LT.TMPVAL)THEN  
          DTTMP=RTMPR*DTMIN  
        ELSE  
          DTTMP=(RTMPR-1.)*DTMIN  
        ENDIF  
      ENDIF  
C  
C **  SET TO MINIMUM TIME STEP ON STARTUP  
C  
      IF(N.EQ.0)DTTMP=DTMIN  
C  
C **  RESTRICT INCREASE IN TIME STEP TO DTMIN  
C      DTDYN2=2.*DTDYN  
C  
      DTDYNP=DTDYN+DTMIN  
      IF(DTTMP.GT.DTDYNP)THEN  
        DTTMP=DTDYNP  
      ENDIF  
      DTDYN=DTTMP  
C  
C **  SET INCREMENTAL INCREASE IN OUTPUT COUNTER  
C  
      NINCRMT=NINT(DTDYN/DTMIN)  
C  
C **  ADJUST INCREMENT FOR N TO LAND EVENLY ON NTSPTC  
C  
      RTCTMP=FLOAT(N)/FLOAT(NTSPTC)  
      NTCTMP=RTCTMP  
      NTMP=(1+NTCTMP)*NTSPTC-N  
      IF(NINCRMT.GT.NTMP)THEN  
        NINCRMT=NTMP  
        DTDYN=FLOAT(NTMP)*DTMIN  
      ENDIF  
C  
C **  WRITE TO TIME STEP LOG FILE  
C  
  100 FORMAT(5I5,5F12.5,E13.5)  
  101 FORMAT(3I5,E13.5)  
  800 FORMAT('  TIME,DTDYN,DTMIN,I,J = ',F12.5,2E12.4,2I7)  
  801 FORMAT('  MOM  ADV,I,J,DTM = ',2I5,E13.4)  
  802 FORMAT('  MASS ADV,I,J,DTM = ',2I5,E13.4)  
  803 FORMAT('  CURV ACC,I,J,DTM = ',2I5,E13.4)  
  880 FORMAT(3I5,8E13.4)  
 8899 FORMAT(' DT3 ERROR ',2I5,6E13.5)  
      RETURN  
      END  

