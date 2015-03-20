      SUBROUTINE CALDISP3  
C  
C CHANGE RECORD  
C  
      USE GLOBAL  

	IMPLICIT NONE
	INTEGER::K,KK,L,LS,KT,LN
	REAL::CLTMP,DDD,CMTMP,DXXTMP,WTX,DXXWEST,DXXEAST
	REAL::DXXSOUT,DXXNORT,WTY,DYXWEST,DYXEAST,DYXSOUT,DYXNORT
	REAL::DYYTMP,DYYWEST,DYYSOUT,DYYNORT,DYYEAST,AMCPT,AMSPT
	REAL::DXYWEST,DXYEAST,DXYSOUT,DXYNORT,DYXTMP,DMAX,DXYTMP
	REAL::CCUU,CCVV,CCUV,CCVU,TPNN,UAVG,VAVG,CUTMP,CTMP

      ! *** DSLLC BEGIN BLOCK
      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:)::INDX  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::CCUTMP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::CCVTMP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::CSOL  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::UP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::VP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::CCTMP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::CDISP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::CDISPI  

      IF(.NOT.ALLOCATED(INDX))THEN
        ALLOCATE(INDX(KC))
        ALLOCATE(CCUTMP(KCM))
        ALLOCATE(CCVTMP(KCM))
        ALLOCATE(CSOL(MGM))
        ALLOCATE(UP(KCM))
        ALLOCATE(VP(KCM))
        ALLOCATE(CCTMP(KCM,KCM))
        ALLOCATE(CDISP(MGM,MGM))
        ALLOCATE(CDISPI(KCM,KCM))

        ! *** ZERO LOCAL ARRAYS
        INDX=0 
        CCUTMP=0.0 
        CCVTMP=0.0 
        CSOL=0.0 
        UP=0.0 
        VP=0.0 
        CCTMP=0.0 
        CDISP=0.0 
        CDISPI=0.0 
      ENDIF
      ! *** DSLLC END BLOCK
C  
C **  INITIALIZE ON FIRST CALL  
C  
      IF(N.EQ.NDISP)THEN  
        DO L=1,LC  
          DO KK=1,KC  
            DO K=1,KC  
              BDISP(K,KK,L)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        DO L=1,LC  
          DO K=1,KC  
            BDISP(K,K,L)=1.  
            FUDISP(K,L)=0.  
            FVDISP(K,L)=0.  
          ENDDO  
        ENDDO  
        DO L=1,LC  
          DXXTCA(L)=0.  
          DXYTCA(L)=0.  
          DYXTCA(L)=0.  
          DYYTCA(L)=0.  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE VERTICAL DIFFUSION MATRIX AND INVERSE AND ACCUMULATE  
C **  DATA FOR DISPERSION CALCULATION  
C  
      DELT=FLOAT(NTSTBC)*DT  
      DO L=2,LA  
        IF(LCT(L).EQ.5.AND.SPB(L).NE.0.)THEN  
          LN=LNC(L)  
          DO K=1,KC  
            UP(K)=0.5*(U(L,K)+U(L+1,K))  
            VP(K)=0.5*(V(L,K)+V(LN,K))  
          ENDDO  
          UAVG=0.  
          VAVG=0.  
          DO K=1,KC  
            UAVG=UAVG+DZC(K)*UP(K)  
            VAVG=VAVG+DZC(K)*VP(K)  
          ENDDO  
          DO K=1,KC  
            UP(K)=UP(K)-UAVG  
            VP(K)=VP(K)-VAVG  
          ENDDO  
          DO KK=1,KC  
            DO K=1,KC  
              CDISP(K,KK)=0.  
            ENDDO  
          ENDDO  
          CUTMP=-DELT*CDZKK(1)*AB(L,1)*HPI(L)  
          CMTMP=1.-CUTMP  
          CDISP(1,1)=CMTMP*DZC(1)  
          CDISP(1,2)=CUTMP*DZC(1)  
          DO K=2,KS  
            CLTMP=-DELT*CDZKMK(K)*AB(L,K-1)*HPI(L)  
            CUTMP=-DELT*CDZKK(K)*AB(L,K)*HPI(L)  
            CMTMP=1.-CLTMP-CUTMP  
            CDISP(K,K-1)=CLTMP*DZC(K)  
            CDISP(K,K)=CMTMP*DZC(K)  
            CDISP(K,K+1)=CUTMP*DZC(K)  
          ENDDO  
          CLTMP=-DELT*CDZKMK(KC)*AB(L,KS)*HPI(L)  
          CMTMP=1.-CLTMP  
          CDISP(KC,KS)=CLTMP*DZC(KC)  
          CDISP(KC,KC)=CMTMP*DZC(KC)  
          CALL LUDCMP(CDISP,KC,MGM,INDX,DDD)  
          DO KK=1,KC  
            DO K=1,KC  
              CDISPI(K,KK)=0.  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            CDISPI(K,K)=1.  
          ENDDO  
          DO K=1,KC  
            CALL LUBKSB(CDISP,KC,MGM,INDX,CDISPI(1,K))  
          ENDDO  
          DO KK=1,KC  
            DO K=1,KC  
              CDISPI(K,KK)=CDISPI(K,KK)*DZC(K)  
            ENDDO  
          ENDDO  
          DO KK=1,KC  
            DO K=1,KC  
              CTMP=0.  
              DO KT=1,KC  
                CTMP=CTMP+CDISPI(K,KT)*BDISP(KT,KK,L)  
              ENDDO  
              CCTMP(K,KK)=CTMP  
            ENDDO  
          ENDDO  
          DO KK=1,KC  
            DO K=1,KC  
              BDISP(K,KK,L)=CCTMP(K,KK)  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            CCUTMP(K)=FUDISP(K,L)-DT*UP(K)/HMIN  
            CCVTMP(K)=FVDISP(K,L)-DT*VP(K)/HMIN  
          ENDDO  
          DO K=1,KC  
            CCUU=0.  
            CCVV=0.  
            DO KK=1,KC  
              CCUU=CCUU+CDISPI(K,KK)*CCUTMP(KK)  
              CCVV=CCVV+CDISPI(K,KK)*CCVTMP(KK)  
            ENDDO  
            FUDISP(K,L)=CCUU  
            FVDISP(K,L)=CCVV  
          ENDDO  
          CCUU=0.  
          CCVV=0.  
          CCUV=0.  
          CCVU=0.  
          DO K=1,KC  
            CCUU=CCUU+DZC(K)*UP(K)*FUDISP(K,L)  
            CCUV=CCUV+DZC(K)*UP(K)*FVDISP(K,L)  
            CCVU=CCVU+DZC(K)*VP(K)*FUDISP(K,L)  
            CCVV=CCVV+DZC(K)*VP(K)*FVDISP(K,L)  
          ENDDO  
          DXXTCA(L)=DXXTCA(L)+CCUU*HP(L)  
          DXYTCA(L)=DXYTCA(L)+CCUV*HP(L)  
          DYXTCA(L)=DYXTCA(L)+CCVU*HP(L)  
          DYYTCA(L)=DYYTCA(L)+CCVV*HP(L)  
          DO K=1,KC  
            CCUU=0.  
            CCVV=0.  
            DO KK=1,KC  
              CCUU=CCUU+DZC(KK)*UP(KK)*BDISP(KK,K,L)  
              CCVV=CCVV+DZC(KK)*VP(KK)*BDISP(KK,K,L)  
            ENDDO  
            CUDISPT(K,L)=CCUU*HP(L)  
            CVDISPT(K,L)=CCVV*HP(L)  
          ENDDO  
        ENDIF  
      ENDDO  
      IF(N.LT.NTS) RETURN  
C  
C **  COMPLETE CALCULATION OF DISPERSION COEFFICIENTS  
C  
      TPNN=TPN/FLOAT(NTSTBC)  
      DO L=2,LA  
        IF(LCT(L).EQ.5.AND.SPB(L).NE.0.)THEN  
          DO KK=1,KC  
            DO K=1,KC  
              CDISP(K,KK)=-BDISP(K,KK,L)  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            CDISP(K,K)=1.+CDISP(K,K)  
          ENDDO  
          CALL LUDCMP(CDISP,KC,MGM,INDX,DDD)  
          DO K=1,KC  
            CSOL(K)=FUDISP(K,L)  
          ENDDO  
          CALL LUBKSB(CDISP,KC,MGM,INDX,CSOL)  
          CCUU=0.  
          CCVU=0.  
          DO K=1,KC  
            CCUU=CCUU+CUDISPT(K,L)*CSOL(K)  
            CCVU=CCVU+CVDISPT(K,L)*CSOL(K)  
          ENDDO  
          DXXTCA(L)=-(DXXTCA(L)+CCUU)*HMIN/TPNN  
          DYXTCA(L)=-(DYXTCA(L)+CCVU)*HMIN/TPNN  
          DO K=1,KC  
            CSOL(K)=FVDISP(K,L)  
          ENDDO  
          CALL LUBKSB(CDISP,KC,MGM,INDX,CSOL)  
          CCVV=0.  
          CCUV=0.  
          DO K=1,KC  
            CCVV=CCVV+CVDISPT(K,L)*CSOL(K)  
            CCUV=CCUV+CUDISPT(K,L)*CSOL(K)  
          ENDDO  
          DYYTCA(L)=-(DYYTCA(L)+CCVV)*HMIN/TPNN  
          DXYTCA(L)=-(DXYTCA(L)+CCUV)*HMIN/TPNN  
        ENDIF  
      ENDDO  
      DO L=2,LA  
        DXXTCA(L)=DXXTCA(L)/HLPF(L)  
        DXYTCA(L)=DXYTCA(L)/HLPF(L)  
        DYXTCA(L)=DYXTCA(L)/HLPF(L)  
        DYYTCA(L)=DYYTCA(L)/HLPF(L)  
      ENDDO  
C  
C **  ADJUST DISPERSON TENSOR COMPONENTS  
C  
      OPEN(88,FILE='DISDIA.OUT',STATUS='UNKNOWN')  
      CLOSE(88,STATUS='DELETE')  
      OPEN(88,FILE='DISDIA.OUT',STATUS='UNKNOWN')  
      DMAX=10000.  
      DO L=2,LA  
        DXXTMP=DXXTCA(L)  
        IF(DXXTMP.LT.0.OR.DXXTMP.GT.DMAX)THEN  
          WRITE(88,8881)IL(L),JL(L),DXXTMP  
          DXXTCA(L)=0.  
          LN=LNC(L)  
          LS=LSC(L)  
          WTX=0.  
          DXXWEST=0.  
          DXXEAST=0.  
          DXXSOUT=0.  
          DXXNORT=0.  
          IF(SUB(L).NE.0)THEN  
            DXXWEST=DXXTCA(L-1)  
            IF(DXXWEST.LT.0.OR.DXXWEST.GT.DMAX)THEN  
              DXXWEST=0.  
            ELSE  
              WTX=WTX+1  
            ENDIF  
          ENDIF  
          IF(SUB(L+1).NE.0)THEN  
            DXXEAST=DXXTCA(L+1)  
            IF(DXXEAST.LT.0.OR.DXXEAST.GT.DMAX)THEN  
              DXXEAST=0.  
            ELSE  
              WTX=WTX+1  
            ENDIF  
          ENDIF  
          IF(SVB(L).NE.0)THEN  
            DXXSOUT=DXXTCA(LS)  
            IF(DXXSOUT.LT.0.OR.DXXSOUT.GT.DMAX)THEN  
              DXXSOUT=0.  
            ELSE  
              WTX=WTX+1  
            ENDIF  
          ENDIF  
          IF(SVB(LN).NE.0)THEN  
            DXXNORT=DXXTCA(LN)  
            IF(DXXNORT.LT.0.OR.DXXNORT.GT.DMAX)THEN  
              DXXNORT=0.  
            ELSE  
              WTX=WTX+1  
            ENDIF  
          ENDIF  
          IF(WTX.NE.0) DXXTCA(L)=(DXXWEST+DXXEAST+DXXSOUT+DXXNORT)/WTX  
        ENDIF  
      ENDDO  
      DO L=2,LA  
        DXYTMP=DXYTCA(L)  
        IF(ABS(DXYTMP).GT.DMAX)THEN  
          WRITE(88,8882)IL(L),JL(L),DXYTMP  
          DXYTCA(L)=0.  
          LN=LNC(L)  
          LS=LSC(L)  
          WTX=0.  
          DXYWEST=0.  
          DXYEAST=0.  
          DXYSOUT=0.  
          DXYNORT=0.  
          IF(SUB(L).NE.0)THEN  
            DXYWEST=DXYTCA(L-1)  
            IF(ABS(DXYWEST).GT.DMAX)THEN  
              DXYWEST=0.  
            ELSE  
              WTX=WTX+1  
            ENDIF  
          ENDIF  
          IF(SUB(L+1).NE.0)THEN  
            DXYEAST=DXYTCA(L+1)  
            IF(ABS(DXYEAST).GT.DMAX)THEN  
              DXYEAST=0.  
            ELSE  
              WTX=WTX+1  
            ENDIF  
          ENDIF  
          IF(SVB(L).NE.0)THEN  
            DXYSOUT=DXYTCA(LS)  
            IF(ABS(DXYSOUT).GT.DMAX)THEN  
              DXYSOUT=0.  
            ELSE  
              WTX=WTX+1  
            ENDIF  
          ENDIF  
          IF(SVB(LN).NE.0)THEN  
            DXYNORT=DXYTCA(LN)  
            IF(ABS(DXYNORT).GT.DMAX)THEN  
              DXYNORT=0.  
            ELSE  
              WTX=WTX+1  
            ENDIF  
          ENDIF  
          DXYTCA(L)=(DXYWEST+DXYEAST+DXYSOUT+DXYNORT)/WTX  
        ENDIF  
      ENDDO  
      DO L=2,LA  
        DYXTMP=DYXTCA(L)  
        IF(DYXTMP.GT.DMAX)THEN  
          WRITE(88,8883)IL(L),JL(L),DYXTMP  
          DYXTCA(L)=0.  
          LN=LNC(L)  
          LS=LSC(L)  
          WTY=0.  
          DYXWEST=0.  
          DYXEAST=0.  
          DYXSOUT=0.  
          DYXNORT=0.  
          IF(SUB(L).NE.0)THEN  
            DYXWEST=DYXTCA(L-1)  
            IF(ABS(DYXWEST).GT.DMAX)THEN  
              DYXWEST=0.  
            ELSE  
              WTY=WTY+1  
            ENDIF  
          ENDIF  
          IF(SUB(L+1).NE.0)THEN  
            DYXEAST=DYXTCA(L+1)  
            IF(ABS(DYXEAST).GT.DMAX)THEN  
              DYXEAST=0.  
            ELSE  
              WTY=WTY+1  
            ENDIF  
          ENDIF  
          IF(SVB(L).NE.0)THEN  
            DYYSOUT=DYYTCA(LS)  
            IF(ABS(DYXSOUT).GT.DMAX)THEN  
              DYXSOUT=0.  
            ELSE  
              WTY=WTY+1  
            ENDIF  
          ENDIF  
          IF(SVB(LN).NE.0)THEN  
            DYYNORT=DYYTCA(LN)  
            IF(ABS(DYXNORT).GT.DMAX)THEN  
              DYXNORT=0.  
            ELSE  
              WTY=WTY+1  
            ENDIF  
          ENDIF  
          DYXTCA(L)=(DYXWEST+DYXEAST+DYXSOUT+DYXNORT)/WTY  
        ENDIF  
      ENDDO  
      DO L=2,LA  
        DYYTMP=DYYTCA(L)  
        IF(DYYTMP.LT.0.OR.DYYTMP.GT.DMAX)THEN  
          WRITE(88,8884)IL(L),JL(L),DYYTMP  
          DYYTCA(L)=0.  
          LN=LNC(L)  
          LS=LSC(L)  
          WTY=0.  
          DYYWEST=0.  
          DYYEAST=0.  
          DYYSOUT=0.  
          DYYNORT=0.  
          IF(SUB(L).NE.0)THEN  
            DYYWEST=DYYTCA(L-1)  
            IF(DYYWEST.LT.0.OR.DYYWEST.GT.DMAX)THEN  
              DYYWEST=0.  
            ELSE  
              WTY=WTY+1  
            ENDIF  
          ENDIF  
          IF(SUB(L+1).NE.0)THEN  
            DYYEAST=DYYTCA(L+1)  
            IF(DYYEAST.LT.0.OR.DYYEAST.GT.DMAX)THEN  
              DYYEAST=0.  
            ELSE  
              WTY=WTY+1  
            ENDIF  
          ENDIF  
          IF(SVB(L).NE.0)THEN  
            DYYSOUT=DYYTCA(LS)  
            IF(DYYSOUT.LT.0.OR.DYYSOUT.GT.DMAX)THEN  
              DYYSOUT=0.  
            ELSE  
              WTY=WTY+1  
            ENDIF  
          ENDIF  
          IF(SVB(LN).NE.0)THEN  
            DYYNORT=DYYTCA(LN)  
            IF(DYYNORT.LT.0.OR.DYYNORT.GT.DMAX)THEN  
              DYYNORT=0.  
            ELSE  
              WTY=WTY+1  
            ENDIF  
          ENDIF  
          DYYTCA(L)=(DYYWEST+DYYEAST+DYYSOUT+DYYNORT)/WTY  
        ENDIF  
      ENDDO  
      CLOSE(88)  
 8881 FORMAT('  I=',I5,2X,'J=',I5,2X,'DXX= ',E12.4)  
 8882 FORMAT('  I=',I5,2X,'J=',I5,2X,'DXY= ',E12.4)  
 8883 FORMAT('  I=',I5,2X,'J=',I5,2X,'DYX= ',E12.4)  
 8884 FORMAT('  I=',I5,2X,'J=',I5,2X,'DYY= ',E12.4)  
C  
C **  WRITE OUTPUT FILES  
C  
      OPEN(88,FILE='DISTEN.OUT',STATUS='UNKNOWN')  
      CLOSE(88,STATUS='DELETE')  
      OPEN(88,FILE='DISTEN.OUT',STATUS='UNKNOWN')  
      WRITE(88,881)  
      DO L=2,LA  
        WRITE(88,2011)IL(L),JL(L),DLON(L),DLAT(L),DXXTCA(L),  
     &      DXYTCA(L),DYXTCA(L),DYYTCA(L)  
      ENDDO  
      CLOSE(88)  
      OPEN(88,FILE='UVTSC.OUT',STATUS='UNKNOWN')  
      CLOSE(88,STATUS='DELETE')  
      OPEN(88,FILE='UVTSC.OUT',STATUS='UNKNOWN')  
      WRITE(88,882)  
      DO L=2,LA  
        AMCPT=AMCP(L)*GI  
        AMSPT=AMSP(L)*GI  
        WRITE(88,2012)IL(L),JL(L),DLON(L),DLAT(L),AMCPT,AMSPT,  
     &      AMCUE(L),AMSUE(L),AMCVE(L),AMSVE(L)  
      ENDDO  
      CLOSE(88)  
      OPEN(88,FILE='UVERV.OUT',STATUS='UNKNOWN')  
      CLOSE(88,STATUS='DELETE')  
      OPEN(88,FILE='UVERV.OUT',STATUS='UNKNOWN')  
      WRITE(88,883)  
      DO L=2,LA  
        WRITE(88,2012)IL(L),JL(L),DLON(L),DLAT(L),HLPF(L),UELPF(L),  
     &      VELPF(L),SALLPF(L,1),SALLPF(L,KC)  
      ENDDO  
      CLOSE(88)  
  881 FORMAT(3X,'I',3X,'J',3X,'LON',9X,'LAT',9X,'DXX',10X,'DXY',10X,  
     &    'DYX',10X,'DYY')  
  882 FORMAT(3X,'I',3X,'J',3X,'LON',9X,'LAT',9X,'AMCPT',8X,'AMSPT',8X,  
     &    'AMCUE',8X,'AMSUE',8X,'AMCVE',8X,'AMSVE')  
  883 FORMAT(3X,'I',3X,'J',3X,'LON',9X,'LAT',9X,'HLPF',9X,'UELPF',8X,  
     &    'VELPF',8X,'SALLPFBOT',4X,'SALLPFSURF')  
 2011 FORMAT(2I4,2X,F10.6,2X,F10.6,4(2X,E12.4))  
 2012 FORMAT(2I4,2X,F10.6,2X,F10.6,6(2X,E12.4))  
      RETURN  
      END  

