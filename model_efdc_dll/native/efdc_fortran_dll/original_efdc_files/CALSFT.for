      SUBROUTINE CALSFT(ISTL_,IS2TL_)  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALSFT CALCULATES THE TRANSPORT OF SHELL FISH LARVAE  
C **  AT TIME LEVEL (N+1).  
C **  CALLED ONLY ON ODD THREE TIME LEVEL STEPS  (PMC - NO, CALLED IN BOTH HDMT & HDMT2T)
C  
      USE GLOBAL

      ! *** DSLLC BEGIN BLOCK
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WTFKB  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WTFKC  
      IF(.NOT.ALLOCATED(WTFKB))THEN
        ALLOCATE(WTFKB(KCM))  
        ALLOCATE(WTFKC(KCM))
        ! *** ZERO LOCAL ARRAYS
        WTFKB=0.0 
        WTFKC=0.0 
      ENDIF
      ! *** DSLLC END BLOCK
C
CPMC      DELT=DT2  
      ! *** PMC
      IF(ISTL_.EQ.2)THEN  
        IF(ISDYNSTP.EQ.0)THEN  
          DELT=DT  
        ELSE  
          DELT=DTDYN  
        END IF  
      ELSE
        DELT=DT2 
      ENDIF  
      ! *** PMC
C  
C **  UPDATED TIME SERIES CONCENTRATION BOUNDARY CONDITIONS  
C **  DETERMINE IF CURRENT TIME STEP IS DURING DAYLIGHT OR DARKNESS  
C  
      IF(ISSFLDN.GE.1)THEN  
        ISDARK=1  
        IF(ISDYNSTP.EQ.0)THEN  
          TIME=(DT*FLOAT(N)+TCON*TBEGIN)/86400.  
        ELSE  
          TIME=TIMESEC/86400.  
        ENDIF  
        ITIME=INT(TIME)  
        RTIME=FLOAT(ITIME)  
        TIMTMP=TIME-RTIME  
        IF(TIMTMP.GE.TSRSF.AND.TIMTMP.LE.TSSSF) ISDARK=0  
      ENDIF  
C  
C **  DETERMINE IF LOCAL CONDITIONS ARE EBB OR FLOOD  
C  
      IF(ISSFLFE.GE.1)THEN  
        IF(KC.EQ.1)THEN  
          WTFKB(1)=1.  
          WTFKC(1)=0.  
        ENDIF  
        IF(KC.EQ.2)THEN  
          WTFKB(1)=1.0  
          WTFKC(1)=0.0  
          WTFKB(2)=0.0  
          WTFKC(2)=1.0  
        ENDIF  
        IF(KC.EQ.3)THEN  
          DO K=1,KC  
            WTFKB(K)=FLOAT(KC-K)/FLOAT(KS)  
            WTFKC(K)=1.0-WTFKB(K)  
          ENDDO  
        ENDIF  
C  
C **  SET SWITCHES TO EBB  
C  
        DO K=1,KC  
          DO L=2,LA  
            UUU(L,K)=0.  
            VVV(L,K)=1.  
          ENDDO  
        ENDDO  
C  
C **  RESET SWITCHES FOR FLOOD  
C  
        DO K=1,KC  
          DO L=2,LA  
            LN=LNC(L)  
            FANGTMP=ACCWFLD(L,1)*WTFKB(K)+ACCWFLD(L,2)*WTFKC(K)  
            UTMP=0.5*STCUV(L)*(UWQ(L+1,K)+UWQ(L,K))  
            VTMP=0.5*STCUV(L)*(VWQ(LN ,K)+VWQ(L,K))  
            VELEKB=CUE(L)*UTMP+CVE(L)*VTMP+1.E-12  
            VELNKB=CUN(L)*UTMP+CVN(L)*VTMP  
            CURANG=ATAN2(VELNKB,VELEKB)  
            ANGDIF=ABS(FANGTMP-CURANG)  
            IF(ANGDIF.LT.1.5708)THEN  
              UUU(L,K)=1.  
              VVV(L,K)=0.  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  SET UP ADVECTION FIELD  
C **  SET ATTACHED TO BOTTOM AND NO ADVECTIVE TRANSPORT IN BOTTOM  
C **  LAYER DURING EBB IF APPROPRIATE  
C  
      IF(ISSFLFE.GE.1)THEN  
        IF(SFNTBET.LT.1.)THEN  
          K=1  
          DO L=2,LA  
            LN=LNC(L)  
           UHDYWQ(L,K)=UUU(L,1)*UHDYWQ(L,1)+SFNTBET*VVV(L,1)*UHDYWQ(L,1)  
           VHDXWQ(L,K)=UUU(L,1)*UHDYWQ(L,1)+SFNTBET*VVV(L,1)*VHDXWQ(L,1)  
            UWQ(L,K)=UUU(L,1)*UWQ(L,1)+SFNTBET*VVV(L,1)*UWQ(L,1)  
            VWQ(L,K)=UUU(L,1)*VWQ(L,1)+SFNTBET*VVV(L,1)*VWQ(L,1)  
          ENDDO  
        ENDIF  
      ENDIF  
      ! *** COMPUTE SHELLFISH LARVAE ADVECTION
      CALL CALTRAN (ISTL_,IS2TL_,4,4,SFL,SFL2)  
      !CALL CALTRWQ (4,0,SFL,SFL2)   ! PMC 
C  
C **  SET UP VERTICAL MIGRATION AND SETTLING BEHAVIOR  
C **  INITIALIZE VERTICAL VELOCTIY TO TIME DEPENDENT SETTLING VELOCITY  
C  
      DO K=1,KS  
        DO L=2,LA  
          WWQ(L,K)=-WSFLSTT  
        ENDDO  
      ENDDO  
      DO L=2,LA  
        WWQ(L,KC)=0.  
        WWQ(L,0)=0.  
      ENDDO  
      IF(ISSFLFE.GE.1.AND.ISSFLDN.GE.1)THEN  
C  
C **  DAYLIGHT CONDITIONS  
C  
        IF(ISDARK.EQ.0)THEN  
          DO K=1,KS  
            RABOVE=FLOAT(KC-K)/FLOAT(KC)  
            DO L=2,LA  
C  
C **   DETERMINE DISTANCE TO SURFACE  
C  
              HABOVE=RABOVE*HWQ(L)  
              IF(UUU(L,K).GT.0.)THEN  
C  
C **    FLOOD CONDITION : SWIM UP TO MIN DIST BELOW SURFACE  
C  
                IF(HABOVE.GT.DSFLMNT) WWQ(L,K)=WSFLSMT  
              ELSE  
C  
C **    EBB CONDITION : CONTINUE TO SINK OR SWIM UP TO MAX DIST BL SURF  
C  
                IF(HABOVE.GT.DSFLMXT) WWQ(L,K)=WSFLSMT  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDIF  
C  
C **  DARK CONDITIONS  
C  
        IF(ISDARK.EQ.1)THEN  
          DO K=1,KS  
            DO L=2,LA  
C  
C **   FLOOD CONDITION : SWIM UP TO  SURFACE  
C  
              WWQ(L,K)=VVV(L,K)*WWQ(L,K)+UUU(L,K)*WSFLSMT  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(SFATBTT.GT.0.)THEN  
        DO L=2,LA  
          WWQ(L,0)=-WSFLSTT  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE NET VERTICAL SWIMING OR SETTLING  
C  
      IF(WSFLSMT.EQ.0.) GOTO 100  
C  
C **  LIMIT VERTICAL SETTLING AND/OR SWIMMING FOR STABILITY  
C  
      DO K=0,KS  
        DO L=2,LA  
          WWW(L,K)=MIN(WWQ(L,K),0.)  
          WWW(L,K)=ABS(WWW(L,K))  
          WWQ(L,K)=MAX(WWQ(L,K),0.)  
        ENDDO  
      ENDDO  
      TMPVAL=0.25/(DELT*FLOAT(KC))  
      DO K=1,KS  
        DO L=2,LA  
          WMAXX=TMPVAL*HWQ(L)  
          WWW(L,K)=MIN(WWW(L,K),WMAXX)  
          WWQ(L,K)=MIN(WWQ(L,K),WMAXX)  
          WWQ(L,K)=WWQ(L,K)-WWW(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KS  
        DO L=2,LA  
          FWU(L,K)=MAX(WWQ(L,K),0.)*SFL(L,K)  
     &        +MIN(WWQ(L,K),0.)*SFL(L,K+1)  
        ENDDO  
      ENDDO  
      IF(SFATBTT.GT.0.)THEN  
        DO L=2,LA  
          SFLSBOT(L)=SFLSBOT(L)-DELT*FWU(L,0)  
        ENDDO  
      ENDIF  
      DO K=1,KC  
        DO L=2,LA  
          SFL(L,K)=SFL(L,K)  
     &        +DELT*(FWU(L,K-1)-FWU(L,K))*DZIC(K)/HWQ(L)  
        ENDDO  
      ENDDO  
      GOTO 200  
  100 CONTINUE  
C  
C **  FULLY IMPLICIT SETTLING IF SWIMMING IS ZERO EVERYWHERE  
C **  FULLY IMPLICIT SETTLING IN SURFACE LAYER  
C  
      TMPVAL=DELT*WSFLSTT  
      DZCIT=TMPVAL/DZC(KC)  
      DO L=2,LA  
        TMPVAL1=DZCIT/HWQ(L)  
        SFL(L,KC)=SFL(L,KC)/(1.+TMPVAL1)  
      ENDDO  
C  
C **  FULLY IMPLICIT SETTLING IN REMAINING LAYERS  
C  
      IF(KC.GT.1)THEN  
        DO K=KS,1,-1  
          DZCIT=TMPVAL/DZC(K)  
          DO L=2,LA  
            TMPVAL1=DZCIT/HWQ(L)  
            SFL(L,K)=(SFL(L,K)+TMPVAL1*SFL(L,K+1))/(1.+TMPVAL1)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(SFATBTT.GT.0.)THEN  
        DO L=2,LA  
          SFLSBOT(L)=SFLSBOT(L)+TMPVAL*SFL(L,1)  
        ENDDO  
      ENDIF  
  200 CONTINUE  
      DO L=2,LA  
        FWU(L,0)=0.  
        WWQ(L,0)=0.  
        WWW(L,0)=0.  
      ENDDO  
C  
C **  CALCULATE LINEAR DECAY  
C  
      IF(RKDSFLT.GE.0.)THEN  
        CDYETMP=1./(1.+DELT*RKDSFLT)  
        DO K=1,KC  
          DO L=2,LA  
            SFL(L,K)=CDYETMP*SFL(L,K)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(KC.EQ.1) GOTO 2000  
C  
C **  VERTICAL DIFFUSION CALCULATION  
C  
      DO L=2,LA  
        HWQI(L)=1./HWQ(L)  
      ENDDO  
      RCDZKK=-DELT*CDZKK(1)  
      DO ND=1,NDM  
        LF=2+(ND-1)*LDM  
        LL=LF+LDM-1  
        DO L=LF,LL  
          CCUBTMP=RCDZKK*HWQI(L)*AB(L,1)  
          CCMBTMP=1.-CCUBTMP  
          EEB=1./CCMBTMP  
          CU1(L,1)=CCUBTMP*EEB  
          SFL(L,1)=SFL(L,1)*EEB  
        ENDDO  
      ENDDO  
      DO ND=1,NDM  
        LF=2+(ND-1)*LDM  
        LL=LF+LDM-1  
        DO K=2,KS  
          RCDZKMK=-DELT*CDZKMK(K)  
          RCDZKK=-DELT*CDZKK(K)  
          DO L=LF,LL  
            CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)  
            CCUBTMP=RCDZKK*HWQI(L)*AB(L,K)  
            CCMBTMP=1.-CCLBTMP-CCUBTMP  
            EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))  
            CU1(L,K)=CCUBTMP*EEB  
            SFL(L,K)=(SFL(L,K)-CCLBTMP*SFL(L,K-1))*EEB  
          ENDDO  
        ENDDO  
      ENDDO  
      K=KC  
      RCDZKMK=-DELT*CDZKMK(K)  
      DO ND=1,NDM  
        LF=2+(ND-1)*LDM  
        LL=LF+LDM-1  
        DO L=LF,LL  
          CCLBTMP=RCDZKMK*HWQI(L)*AB(L,K-1)  
          CCMBTMP=1.-CCLBTMP  
          EEB=1./(CCMBTMP-CCLBTMP*CU1(L,K-1))  
          SFL(L,K)=(SFL(L,K)-CCLBTMP*SFL(L,K-1))*EEB  
        ENDDO  
      ENDDO  
      DO ND=1,NDM  
        LF=2+(ND-1)*LDM  
        LL=LF+LDM-1  
        DO K=KC-1,1,-1  
          DO L=LF,LL  
            SFL(L,K)=SFL(L,K)-CU1(L,K)*SFL(L,K+1)  
          ENDDO  
        ENDDO  
      ENDDO  
C  
C **  UPDATE SHELL FISH LARVAE CONCENTRATIONS  
C  
 2000 CONTINUE  
      DO K=1,KC  
        DO L=2,LA  
          SFL2(L,K)=SFL(L,K)  
        ENDDO  
      ENDDO  
      RETURN  
      END  

