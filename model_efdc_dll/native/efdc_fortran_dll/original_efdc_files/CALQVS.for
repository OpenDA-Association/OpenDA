      SUBROUTINE CALQVS (ISTL_)  
C  
C CHANGE RECORD  
C ** SUBROUTINE CALQVS UPDATES TIME VARIABLE VOLUME SOURCES  
C  
      USE GLOBAL  

      REAL T1TMP,T2TMP
      INTEGER*4  NS
      
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
C **  INITIALIZE NULL (0) FLOW SERIES  
C  
      GWSERT(0)=0.  
      QWRSERT(0)=0.  
      QSERTCELL=0.0
      DO K=1,KC
        QSERT(K,0)=0.  
        QCTLT(K,0)=0.  
        QCTLTO(K,0)=0.  
      ENDDO  

      IF(NGWSER.GE.1)THEN  
        NCTMP=4+NSED+NSND+NTOX  
        DO NC=1,NCTMP  
          GWCSERT(0,NC)=0.  
        ENDDO  
      
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO L=LF,LL
          QGW(L)=0.0  
        END DO
        IF(ISTRAN(5).GT.0)THEN  
          DO NC=1,NCTMP  
            DO L=LF,LL
              CONGW(L,NC)=0.0  
            END DO  
          END DO  
        ENDIF
c
      enddo
      ENDIF
C  
C **  INITIALIZE TOTAL FLOW SERIES  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_LC(1,ithds)
         LL=jse_LC(2,ithds)
c
      DO L=LF,LL
        QSUM1E(L)=QSUME(L)  ! *** DSLLC SINGLE LINE
        QSUME(L)=0.  
      ENDDO  
c
      enddo
      
      ! *** SELECTIVE ZEROING
      IF(KC.GT.1)THEN
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_LC(1,ithds)
         LL=jse_LC(2,ithds)
c
        IF(NGWSER.GT.0.OR.ISGWIT.NE.0)THEN
          DO L=LF,LL
            QSUM(L,1)=0.  
          ENDDO  
        ENDIF
        
        ! *** ZERO EVAP/RAINFALL        
        DO L=LF,LL
          QSUM(L,KC)=0.  
        ENDDO  
c
      enddo
        
        ! *** ZERO ALL DEFINED BC'S
          DO K=1,KC
        DO NS=1,NBCS
          L=LBCS(NS)
            QSUM(L,K)=0.
          ENDDO
        ENDDO

      ELSE
        ! *** SINGLE LAYER
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_LC(1,ithds)
         LL=jse_LC(2,ithds)
c
        DO L=LF,LL
          QSUM(L,1)=0.  
        ENDDO  
c
      enddo
      ENDIF
C  
C **  VOLUME SOURCE/SINK INTERPOLATION  
C  
      DO NS=1,NQSER  
        IF(ISTL_.EQ.2)THEN  
          IF(ISDYNSTP.EQ.0)THEN  
            CTIM=DT*(FLOAT(N)-0.5)/TCQSER(NS)  
     &          +TBEGIN*(TCON/TCQSER(NS))  
          ELSE  
            CTIM=TIMESEC/TCQSER(NS)  
          ENDIF  
        ELSE  
          IF(ISDYNSTP.EQ.0)THEN  
            CTIM=DT*FLOAT(N-1)/TCQSER(NS)+TBEGIN*(TCON/TCQSER(NS))  
          ELSE  
            CTIM=TIMESEC/TCQSER(NS)  
          ENDIF  
        ENDIF  
        M1=MQTLAST(NS)  
  100   CONTINUE  
        M2=M1+1  
        IF(CTIM.GT.TQSER(M2,NS))THEN  
          M1=M2  
          GOTO 100  
        ELSE  
          MQTLAST(NS)=M1  
        ENDIF  
        TDIFF=TQSER(M2,NS)-TQSER(M1,NS)  
        WTM1=(TQSER(M2,NS)-CTIM)/TDIFF  
        WTM2=(CTIM-TQSER(M1,NS))/TDIFF  
        DO K=1,KC  
          QSERT(K,NS)=WTM1*QSER(M1,K,NS)+WTM2*QSER(M2,K,NS)  
        ENDDO  
      ENDDO  
      IF(N.EQ.1)THEN  
        DO LL=1,NQSIJ  
          L=LQS(LL)  
          ITYP=LCT(L)  
          IF(ITYP.LE.0.OR.ITYP.GE.8)THEN  
            WRITE(6,6111)LL,IQS(LL),JQS(LL)  
            WRITE(8,6111)LL,IQS(LL),JQS(LL)  
          ENDIF  
        ENDDO  
      ENDIF  
      DO LL=1,NQSIJ  
        NS=NQSERQ(LL)  
        L=LQS(LL)  
        DO K=1,KC
          ! *** PMC START
          ! *** APPLY MULTIPLIERS HERE TO CORRECT MASS BALANCE PROBLEMS
          QSS(K,LL)     =QSS(K,LL)  *RQSMUL(LL)
          QSERCELL(K,LL)=QSERT(K,NS)*RQSMUL(LL)*QFACTOR(LL)
          QSUM(L,K)=QSUM(L,K)+QSS(K,LL)+QSERCELL(K,LL)
          ! *** PMC END
        ENDDO  
      ENDDO  
C  
C **  GROUNDWATER SOURCE/SINK INTERPOLATION  
C  
      IF(NGWSER.GE.1)THEN  
        NCTMP=4+NSED+NSND+NTOX  
        DO NS=1,NGWSER  
          IF(ISTL_.EQ.2)THEN  
            IF(ISDYNSTP.EQ.0)THEN  
              CTIM=DT*(FLOAT(N)-0.5)/TCGWSER(NS)  
     &            +TBEGIN*(TCON/TCGWSER(NS))  
            ELSE  
              CTIM=TIMESEC/TCGWSER(NS)  
            ENDIF  
          ELSE  
            IF(ISDYNSTP.EQ.0)THEN  
              CTIM=DT*FLOAT(N-1)/TCQSER(NS)+TBEGIN*(TCON/TCQSER(NS))  
            ELSE  
              CTIM=TIMESEC/TCGWSER(NS)  
            ENDIF  
          ENDIF  
          M1=MGWTLAST(NS)  
  700     CONTINUE  
          M2=M1+1  
          IF(CTIM.GT.TGWSER(M2,NS))THEN  
            M1=M2  
            GOTO 700  
          ELSE  
            MGWTLAST(NS)=M1  
          ENDIF  
          TDIFF=TGWSER(M2,NS)-TGWSER(M1,NS)  
          WTM1=(TGWSER(M2,NS)-CTIM)/TDIFF  
          WTM2=(CTIM-TGWSER(M1,NS))/TDIFF  
          GWSERT(NS)=WTM1*GWSER(M1,NS)+WTM2*GWSER(M2,NS)  
          DO NC=1,NCTMP  
            GWCSERT(NC,NS)=WTM1*GWCSER(M1,NC,NS)+WTM2*GWCSER(M2,NC,NS)  
          END DO  
        ENDDO  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO L=LF,LL
          QGW(L)=GWFAC(L)*GWSERT(NGWSL(L))  
        END DO
        IF(ISTRAN(5).GT.0)THEN  
          DO NC=1,NCTMP  
            DO L=LF,LL
              CONGW(L,NC)=GWCSERT(NC,NGWSL(L))  
            END DO  
          END DO  
        ENDIF
c
      enddo
      ENDIF  

      ! *** CONSTANT GW LOSSES
      IF(ISGWIT.EQ.3)THEN
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO L=LF,LL
          IF(H1P(L).GE.HDRY)THEN
            !VOLOUTO=VOLOUTO+RIFTR(L)*DTIM
            QSUM(L,1)=QSUM(L,1)-RIFTR(L)
          ENDIF
        ENDDO
c
      enddo
        !IF((H1P(343).GE.HDRY.or.HP(343).GE.HDRY).and.TIMEDAY.GT.6.5)THEN
        !  VOLOUTE=VOLOUTE+RIFTR(L)*DTIM
        !  WRITE(99,*)N,TIMEDAY,RIFTR(L),H1P(L),HP(L),VOLOUTE
        !ENDIF
      ENDIF
C  
C **  CONTROL STRUCTURES AND TIDAL INLETS  
C  
      CALL CPU_TIME(T1TMP)  
      DO NCTL=1,NQCTL  
        IF(NQCTYP(NCTL).LE.1)THEN  
          NCTLT=NQCTLQ(NCTL)  
          RQDW=1.  
          IU=IQCTLU(NCTL)  
          JU=JQCTLU(NCTL)  
          LU=LIJ(IU,JU)  
          HUP=HP(LU)+BELV(LU)+HCTLUA(NCTLT)  
          ID=IQCTLD(NCTL)  
          JD=JQCTLD(NCTL)  
          IF(ID.EQ.0.AND.JD.EQ.0)THEN  
            LD=LC  
            HDW=0.  
            RQDW=0.  
          ELSE  
            LD=LIJ(ID,JD)  
            HDW=HP(LD)+BELV(LD)+HCTLDA(NCTLT)  
          ENDIF  
          DELH=HCTLUM(NCTLT)*HUP-HCTLDM(NCTLT)*HDW  
          IF(NQCTYP(NCTL).EQ.0.AND.AQCTL(NCTLT).GT.0.0)THEN  
            IF(HUP.LT.AQCTL(NCTLT)) DELH=-100.  
          ENDIF  
          IF(DELH.LE.0.OR.HP(LU).LT.HWET)THEN  
            DO K=1,KC  
              QCTLT(K,NCTL)=0.  
            ENDDO  
          ELSE  
            IF(NQCTYP(NCTL).EQ.1)DELH=SQRT(DELH)  
            M1=0  
            M2=1  
  500       M1=M1+1  
            M2=M2+1  
            IF(M2.GT.MQCTL(NCTLT))THEN  
              WRITE(6,6666)  
              WRITE(6,6667)NCTL,NCTLT,IU,JU,ID,JD  
              WRITE(6,6668)HUP,HP(LU),HDW,HP(LD)  
              WRITE(8,6666)  
              WRITE(8,6667)NCTL,NCTLT,IU,JU,ID,JD  
              WRITE(8,6668)HUP,HP(LU),HDW,HP(LD)  
              STOP  
            ENDIF  
            IF(DELH.GE.HDIFCTL(M1,NCTLT).AND.DELH.LE.HDIFCTL(M2,NCTLT)
     &            )THEN  
              TDIFF=HDIFCTL(M2,NCTLT)-HDIFCTL(M1,NCTLT)  
              WTM1=(HDIFCTL(M2,NCTLT)-DELH)/TDIFF  
              WTM2=(DELH-HDIFCTL(M1,NCTLT))/TDIFF  
              DO K=1,KC  
                QCTLT(K,NCTL)=WTM1*QCTL(M1,1,K,NCTLT)  
     &              +WTM2*QCTL(M2,1,K,NCTLT)  
              ENDDO  
            ELSE  
              GOTO 500  
            ENDIF  
          ENDIF  
          IF(NQCTYP(NCTL).EQ.1)THEN  
            IF(ISTL_.EQ.3)THEN  
              DO K=1,KC  
                QCTLST(K,NCTL)=QCTLT(K,NCTL)  
                TMPVAL=QCTLTO(K,NCTL)  
     &              +DT*AQCTL(NCTLT)*QCTLST(K,NCTL)*QCTLST(K,NCTL)  
                QCTLT(K,NCTL)=TMPVAL/(1.+DT*AQCTL(NCTLT)*QCTLTO(K,NCTL))  
                QCTLTO(K,NCTL)=QCTLT(K,NCTL)  
                QCTLSTO(K,NCTL)=QCTLST(K,NCTL)  
              ENDDO  
            ELSE  
              DO K=1,KC  
                QCTLST(K,NCTL)=QCTLT(K,NCTL)  
                TMPVAL=QCTLTO(K,NCTL)  
     &              +DT*AQCTL(NCTLT)*QCTLST(K,NCTL)*QCTLST(K,NCTL)  
                QCTLT(K,NCTL)=TMPVAL/(1.+DT*AQCTL(NCTLT)*QCTLTO(K,NCTL))  
                QCTLT(K,NCTL)=0.5*(QCTLT(K,NCTL)+QCTLTO(K,NCTL))  
              ENDDO  
            ENDIF  
          ENDIF  
          QCTLMAX=(HP(LU)-HDRY)*DXYP(LU)/(DELT*FLOAT(KC))  
          DO K=1,KC  
            QCTLT(K,NCTL)=MIN(QCTLT(K,NCTL),QCTLMAX)  
          ENDDO  
          DO K=1,KC  
            ! *** PMC START - CORRECTED VOLUME MULTIPLIER TO FIX MASS BALANCE PROBLEM
            QCTLT(K,NCTL)=QCTLT(K,NCTL)*RQCMUL(NCTL)
            QSUM(LU,K)=QSUM(LU,K)-QCTLT(K,NCTL)  
            QSUM(LD,K)=QSUM(LD,K)+QCTLT(K,NCTL)*RQDW
            ! *** PMC END
          ENDDO
          IPMC=0  
        ENDIF  
      ENDDO  
      DO NCTL=1,NQCTL  
        IF(NQCTYP(NCTL).EQ.2)THEN  
          NCTLT=NQCTLQ(NCTL)  
          RQDW=1.  
          IU=IQCTLU(NCTL)  
          JU=JQCTLU(NCTL)  
          LU=LIJ(IU,JU)  
          HUP=HP(LU)+BELV(LU)+HCTLUA(NCTLT)  
          IF(HUP.LT.HDIFCTL(1,NCTLT).OR.HP(LU).LT.HWET)THEN  
            DO K=1,KC  
              QCTLT(K,NCTL)=0.  
            ENDDO  
            GOTO 560  
          ENDIF  
          ID=IQCTLD(NCTL)  
          JD=JQCTLD(NCTL)  
          LD=LIJ(ID,JD)  
          HDW=HP(LD)+BELV(LD)+HCTLDA(NCTLT)  
          HTMPD=HDIFCTD(1,NCTLT)+0.001  
          HDW=MAX(HDW,HTMPD)  
          MU1=0  
          MU2=1  
          MD1=0  
          MD2=1  
  555     MU1=MU1+1  
          MU2=MU1+1  
          IF(MU2.GT.MQCTL(NCTLT))THEN  
            WRITE(6,6676)  
            WRITE(6,6677)NCTL,NCTLT,IU,JU,ID,JD  
            WRITE(6,6678)HUP,HP(LU),HDW,HP(LD)  
            WRITE(6,6679)HDIFCTL(1,NCTLT),HDIFCTL(MQCTL(NCTLT),NCTLT),  
     &          HDIFCTD(1,NCTLT),HDIFCTD(MQCTL(NCTLT),NCTLT)  
            WRITE(8,6676)  
            WRITE(8,6677)NCTL,NCTLT,IU,JU,ID,JD  
            WRITE(8,6678)HUP,HP(LU),HDW,HP(LD)  
            WRITE(8,6679)HDIFCTL(1,NCTLT),HDIFCTL(MQCTL(NCTLT),NCTLT),  
     &          HDIFCTD(1,NCTLT),HDIFCTD(MQCTL(NCTLT),NCTLT)  
            STOP  
          ENDIF  
         IF(HUP.GE.HDIFCTL(MU1,NCTLT).AND.HUP.LE.HDIFCTL(MU2,NCTLT))THEN  
            TDIFFU=HDIFCTL(MU2,NCTLT)-HDIFCTL(MU1,NCTLT)  
            WTM1U=(HDIFCTL(MU2,NCTLT)-HUP)/TDIFFU  
            WTM2U=(HUP-HDIFCTL(MU1,NCTLT))/TDIFFU  
          ELSE  
            GOTO 555  
          ENDIF  
  556     MD1=MD1+1  
          MD2=MD1+1  
          IF(MD2.GT.MQCTL(NCTLT))THEN  
            WRITE(6,6686)  
            WRITE(6,6687)NCTL,NCTLT,IU,JU,ID,JD  
            WRITE(6,6688)HUP,HP(LU),HDW,HP(LD)  
            WRITE(6,6679)HDIFCTL(1,NCTLT),HDIFCTL(MQCTL(NCTLT),NCTLT),  
     &          HDIFCTD(1,NCTLT),HDIFCTD(MQCTL(NCTLT),NCTLT)  
            WRITE(8,6686)  
            WRITE(8,6687)NCTL,NCTLT,IU,JU,ID,JD  
            WRITE(8,6688)HUP,HP(LU),HDW,HP(LD)  
            WRITE(8,6679)HDIFCTL(1,NCTLT),HDIFCTL(MQCTL(NCTLT),NCTLT),  
     &          HDIFCTD(1,NCTLT),HDIFCTD(MQCTL(NCTLT),NCTLT)  
            STOP  
          ENDIF  
         IF(HDW.GE.HDIFCTD(MD1,NCTLT).AND.HDW.LE.HDIFCTD(MD2,NCTLT))THEN  
            TDIFFD=HDIFCTD(MD2,NCTLT)-HDIFCTD(MD1,NCTLT)  
            WTM1D=(HDIFCTD(MD2,NCTLT)-HDW)/TDIFFD  
            WTM2D=(HDW-HDIFCTD(MD1,NCTLT))/TDIFFD  
          ELSE  
            GOTO 556  
          ENDIF  
          DO K=1,KC  
            QCTLT(K,NCTL)=WTM1U*( WTM1D*QCTL(MU1,MD1,K,NCTLT)  
     &          +WTM2D*QCTL(MU1,MD2,K,NCTLT) )  
     &          +WTM2U*( WTM1D*QCTL(MU2,MD1,K,NCTLT)  
     &          +WTM2D*QCTL(MU2,MD2,K,NCTLT) )  
          ENDDO  
  560     CONTINUE  
          QCTLMAX=(HP(LU)-HDRY)*DXYP(LU)/(DELT*FLOAT(KC))  
          DO K=1,KC  
            QCTLT(K,NCTL)=MIN(QCTLT(K,NCTL),QCTLMAX)  
          ENDDO  
          DO K=1,KC  
            ! *** PMC START - CORRECTED VOLUME MULTIPLIER TO FIX MASS BALANCE PROBLEM
            QCTLT(K,NCTL)=QCTLT(K,NCTL)*RQCMUL(NCTL)
            QSUM(LU,K)=QSUM(LU,K)-QCTLT(K,NCTL)  
            QSUM(LD,K)=QSUM(LD,K)+QCTLT(K,NCTL)*RQDW
            ! *** PMC END
          ENDDO  
        ENDIF  
      ENDDO  
C {   GEOSR 2010.5.6 GATE NORMAL FORMULA
      IF (NQCTL.GE.1) THEN
        IF (NQCTYP(1).GE.3) THEN
          CALL CGATEFLX
        ENDIF
      ENDIF
C }   GEOSR 2010.5.6 GATE NORMAL FORMULA
      CALL CPU_TIME(T2TMP)
      TQCTL=TQCTL+T2TMP-T1TMP
C  
C **  FLOW WITHDRAWAL AND RETURN  
C  
      NTMP=4+NSED+NSND+NTOX
      IF(ISTRAN(8).GT.0)NTMP=NTMP+NWQV
        
      DO NC=1,NTMP  
        CQWRSERT(0,NC)=0.  
      ENDDO  
      DO NS=1,NQWRSR  
        IF(ISTL_.EQ.2)THEN  
          IF(ISDYNSTP.EQ.0)THEN  
            CTIM=DT*(FLOAT(N)-0.5)/TCQWRSR(NS)  
     &          +TBEGIN*(TCON/TCQWRSR(NS))  
          ELSE  
            CTIM=TIMESEC/TCQWRSR(NS)  
          ENDIF  
        ELSE  
          IF(ISDYNSTP.EQ.0)THEN  
            CTIM=DT*FLOAT(N-1)/TCQWRSR(NS)+TBEGIN*(TCON/TCQWRSR(NS))  
          ELSE  
            CTIM=TIMESEC/TCQWRSR(NS)  
          ENDIF  
        ENDIF  
        M1=MQWRTLST(NS)  
  200   CONTINUE  
        M2=M1+1  
        IF(CTIM.GT.TQWRSER(M2,NS))THEN  
          M1=M2  
          GOTO 200  
        ELSE  
          MQWRTLST(NS)=M1  
        ENDIF  
        TDIFF=TQWRSER(M2,NS)-TQWRSER(M1,NS)  
        WTM1=(TQWRSER(M2,NS)-CTIM)/TDIFF  
        WTM2=(CTIM-TQWRSER(M1,NS))/TDIFF  
        QWRSERT(NS)=WTM1*QWRSER(M1,NS)+WTM2*QWRSER(M2,NS)  
        DO NC=1,NTMP  
          CQWRSERT(NS,NC)=WTM1*CQWRSER(M1,NS,NC)+WTM2*CQWRSER(M2,NS,NC)  
        ENDDO  
      ENDDO  
      IF(NQWR.GT.0)THEN  
        DO NWR=1,NQWR  
          IU=IQWRU(NWR)  
          JU=JQWRU(NWR)  
          KU=KQWRU(NWR)  
          ID=IQWRD(NWR)  
          JD=JQWRD(NWR)  
          KD=KQWRD(NWR)  
          LU=LIJ(IU,JU)  
          LD=LIJ(ID,JD)  
          NS=NQWRSERQ(NWR)  
          QSUM(LU,KU)=QSUM(LU,KU)-QWR(NWR)-QWRSERT(NS)  
          QSUM(LD,KD)=QSUM(LD,KD)+QWR(NWR)+QWRSERT(NS)  
        ENDDO  
      ENDIF  
C  
C **  CALL JPEFDC AND PLACE JET-PLUME VOLUMES SOURCES  
C  
      IF(NQJPIJ.GT.0.AND.N.EQ.1) CALL JPEFDC  
      IF(NQJPIJ.GT.0.AND.ISTL_.EQ.3)THEN  
        IF(NUDJPC(1).EQ.NUDJP(1))THEN  
          CALL JPEFDC  
          NUDJPC(1)=1  
        ELSE  
          NUDJPC(1)=NUDJPC(1)+1  
        ENDIF  
      ENDIF  
      IF(NQJPIJ.GT.0.AND.IS2TIM.GE.1)THEN  
        IF(NUDJPC(1).EQ.NUDJP(1))THEN  
          CALL JPEFDC  
          NUDJPC(1)=1  
        ELSE  
          NUDJPC(1)=NUDJPC(1)+1  
        ENDIF  
      ENDIF  
C  
C **  PLACE JET-PLUME VOLUMES SOURCES  
C  
      IF(NQJPIJ.GT.0)THEN  
        DO NJP=1,NQJPIJ  
          IF(ICALJP(NJP).EQ.1)THEN  
            RPORTS=FLOAT(NPORTJP(NJP))  
            LJP=LIJ(IQJP(NJP),JQJP(NJP))  
            KTMP=KEFFJP(NJP)  
C  
C QVJPTMP = JETPLUME DISCHARGE PER PORT  
C  
            QVJPTMP=QQCJP(NJP)  
            DO K=1,KC  
              QVJPTMP=QVJPTMP+QSERT(K,NQSERJP(NJP))
            ENDDO  
C  
C SUBTRACT THE ENTRAINMENT FROM EACH LAYER  
C  
            DO K=1,KC  
              QSUM(LJP,K)=QSUM(LJP,K)-RPORTS*QJPENT(K,NJP)  
            ENDDO  
C  
C PLACE DISCHARGE AND TOTAL ENTRAINMENT AT EFFECTIVE LOCATION  
C  
            QSUM(LJP,KTMP)=QSUM(LJP,KTMP)+RPORTS*(QVJPTMP+QJPENTT(NJP))  
          ENDIF  
          IF(ICALJP(NJP).EQ.2)THEN  
            RPORTS=FLOAT(NPORTJP(NJP))  
            LJP=LIJ(IQJP(NJP),JQJP(NJP))  
            KTMP=KEFFJP(NJP)  
C  
C QVJPTMP = JETPLUME DISCHARGE PER PORT  
C  
            QVJPTMP=QWRCJP(NJP)+QWRSERT(NQWRSERJP(NJP))  
C  
C SUBTRACT ENTRAIMENT FROM EACH LAYER  
C  
            DO K=1,KC  
              QSUM(LJP,K)=QSUM(LJP,K)-RPORTS*QJPENT(K,NJP)  
            ENDDO  
C  
C PLACE DISCHARGE AND TOTAL ENTRAINMENT AT EFFECTIVE LOCATION  
C  
            QSUM(LJP,KTMP)=QSUM(LJP,KTMP)+RPORTS*(QVJPTMP+QJPENTT(NJP))  
C  
C REMOVE DISCHARGE FROM UPSTREAM INTAKE CELL  
C  
            LU=LIJ(IUPCJP(NJP),JUPCJP(NJP))  
            KU=KUPCJP(NJP)  
            QSUM(LU,KU)=QSUM(LU,KU)-RPORTS*QVJPTMP  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  GROUND WATER INTERACTION, EVAPORATION AND RAINFALL  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL,SVPW)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      IF(ISGWIE.EQ.0)THEN  
        IF(EVAPCVT.LT.0.)THEN  
          DO L=LF,LL
            SVPW=(10.**((0.7859+0.03477*TEM(L,KC))/  
     &          (1.+0.00412*TEM(L,KC))))  
           EVAPT(L)=CLEVAP(L)*0.7464E-3*WINDST(L)*(SVPW-VPA(L))/PATMT(L)  
            IF(HP(L).LT.HWET) EVAPT(L)=0.  
            QSUM(L,KC)=QSUM(L,KC)+DXYP(L)*(RAINT(L)-EVAPT(L))  
          ENDDO  
        ELSE  
          DO L=LF,LL
            IF(HP(L).LT.HWET) EVAPT(L)=0.  
            QSUM(L,KC)=QSUM(L,KC)+DXYP(L)*(RAINT(L)-EVAPT(L))  
          ENDDO  
        ENDIF  
      ELSE  
        DO L=LF,LL
          QSUM(L,KC)=QSUM(L,KC)+DXYP(L)*RAINT(L)  
        ENDDO  
      ENDIF  
c
      enddo
C  
C **  DETERMINE NET EXTERNAL VOLUME SOURCE/SINK  
C  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse_LC(1,ithds)
         LL=jse_LC(2,ithds)
c
      DO K=1,KC  
        DO L=LF,LL
          QSUME(L)=QSUME(L)+QSUM(L,K)  
        ENDDO  
      ENDDO  
c
      enddo
C  
C **  UPDATE ZERO DIMENSION VOLUME BALANCE  
C       VOLADD=0.  
C **  WRITE DIAGNOSTIC FILE FOR VOLUME SOURCES,SINKS, ETC  
C  
      ITMPD=0  
      IF(ISDIQ.EQ.2.AND.ISTL_.EQ.2) ITMPD=1  
      IF(ISDIQ.EQ.1) ITMPD=1  
      NTT=4+NTOX+NSED+NSND  
      IF(ITMPD.EQ.1.AND.DEBUG)THEN  
        IF(N.EQ.NTSPTC.OR.N.EQ.1)THEN  
          OPEN(1,FILE='QDIAG.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='QDIAG1.OUT',STATUS='UNKNOWN')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='QDIAG1.OUT',STATUS='UNKNOWN')  
        ELSE  
          OPEN(1,FILE='QDIAG.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        ENDIF  
        WRITE(1,101)N  
        DO LL=1,NQSIJ  
          NQSTMP=NQSERQ(LL)  
          NCSTMP=NCSERQ(LL,1)  
          L=LQS(LL)  
          I=IL(L)  
          J=JL(L)  
          WRITE(1,102)I,J  
          WRITE(1,216)LL,L,(QSS(K,LL),K=1,KC)  
          DO NT=1,NTT  
            WRITE(1,217)LL,NT,(CQS(K,LL,NT),K=1,KC)  
          ENDDO  
          WRITE(1,104)  
          WRITE(1,105)I,J  
          WRITE(1,206)LL,L,(QSERCELL(K,LL),K=1,KC)  
          DO NT=1,NTT  
            NCSTMP=NCSERQ(LL,NT)  
            WRITE(1,207)LL,NT,NCSTMP,(CSERT(K,NCSTMP,NT),K=1,KC)  
          ENDDO  
          WRITE(1,104)  
        ENDDO  
        DO NCTL=1,NQCTL  
          IU=IQCTLU(NCTL)  
          JU=JQCTLU(NCTL)  
          ID=IQCTLD(NCTL)  
          JD=JQCTLD(NCTL)  
		NCTLT=NQCTLQ(NCTL) 
          IF(IU.EQ.0.AND.JU.EQ.0)THEN  
            LU=0  
            HUP=0.  
          ELSE  
            LU=LIJ(IU,JU)  
            HUP=HP(LU)+BELV(LU)+HCTLUA(NCTLT)  
          ENDIF  
          IF(ID.EQ.0.AND.JD.EQ.0)THEN  
            LD=0  
            HDW=0.  
          ELSE  
            LD=LIJ(ID,JD)  
            HDW=HP(LD)+BELV(LD)+HCTLDA(NCTLT)  
          ENDIF  
          WRITE(1,107)IU,JU,LU,NCTLT,HUP  
          DO K=1,KC  
            WRITE(1,108)K,QCTLT(K,NCTL)  
          ENDDO  
          WRITE(1,104)  
          WRITE(1,109)ID,JD,LD,NCTLT,HDW  
          DO K=1,KC  
            WRITE(1,108)K,QCTLT(K,NCTL)  
          ENDDO  
          WRITE(1,104)  
        ENDDO  
        DO NWR=1,NQWR  
          IU=IQWRU(NWR)  
          JU=JQWRU(NWR)  
          KU=KQWRU(NWR)  
          ID=IQWRD(NWR)  
          JD=JQWRD(NWR)  
          KD=KQWRD(NWR)  
          LU=LIJ(IU,JU)  
          LD=LIJ(ID,JD)  
          NQSTMP=NQWRSERQ(NWR)  
          WRITE(1,110)IU,JU  
          WRITE(1,111)KU,QWR(NWR),CQWR(NWR,1),CQWR(NWR,2)  
          WRITE(1,104)  
          WRITE(1,112)ID,JD  
          WRITE(1,111)KD,QWR(NWR),CQWR(NWR,1),CQWR(NWR,2)  
          WRITE(1,104)  
          WRITE(1,113)IU,JU  
          WRITE(1,114)KU,QWRSERT(NQSTMP),CQWRSERT(NQSTMP,1),  
     &        CQWRSERT(NQSTMP,2)  
          WRITE(1,104)  
          WRITE(1,115)ID,JD  
          WRITE(1,114)KD,QWRSERT(NQSTMP),CQWRSERT(NQSTMP,1),  
     &        CQWRSERT(NQSTMP,2)  
          WRITE(1,104)  
        ENDDO  
        CLOSE(1)  
      ENDIF  
  101 FORMAT('  SOURCE/SINK DIAGNOSTICS AT TIME STEP =',I8,//)  
  102 FORMAT(3X,'CONST NQSIJ SOURCE/SINK FLOW AT I =',I5,' J =',I5,/)  
C 103 FORMAT(5X,'K =',I5,5X,'QSS(K) = ',E12.4,5X,'CQS(K,1) = ',E12.4,  
C    &    5X,'CQS(K,5) = ',E12.4)  
C 203 FORMAT(5X,'K =',I5,5X,'QSS(K) = ',E12.4,5X,'CQS(K, ) = ',  
C    &    5X, 12E12.4)  
  104 FORMAT(/)  
  105 FORMAT(3X,'TIME VAR NQSIJ SOURCE/SINK FLOW AT I =',I5,' J=',I5,/)  
C 106 FORMAT(5X,'K =',I5,5X,'QSERT(K) = ',E12.4,  
C    &    5X,'CSERT(K,1) = ',E12.4,5X,'CSERT(K,5) = ',E12.4)  
  206 FORMAT(5X,'NQ,LQ     =',2I4,7X,'QSERT() = ',12E12.4)  
  207 FORMAT(5X,'NQ,NT,NCQ =',3I4,3X,'CSERT() = ',12E12.4)  
  216 FORMAT(5X,'NQ,LQ =',2I4,3X,'QSS() = ',12E12.4)  
  217 FORMAT(5X,'NQ,NT =',2I4,3X,'CQS() = ',12E12.4)  
  107 FORMAT(3X,'UPSTRM CONTROLED SINK FLOW AT I =',I5,' J =',I5,  
     &    ' L =',I5,'  NQCTLT =',I5,'  HUP = ',E12.4/)  
  108 FORMAT(5X,'K =',I5,5X,'QCTL(K) = ',2E12.4)  
  109 FORMAT(3X,'DWNSTRM CONTROLED SOURCE FLOW AT I =',I5,' J =',I5,  
     &    ' L =',I5,'  NQCTLT =',I5,'  HDW = ',E12.4/)  
  110 FORMAT(3X,'UPSTRM CONST WITHDRW SINK FLOW AT I =',I5,' J =',I5,/)  
  111 FORMAT(5X,'K =',I5,5X,'QWR(K) = ',E12.4,  
     &    5X,'CQWR(1) = ',E12.4,5X,'CQWR(2) = ',E12.4)  
  112 FORMAT(3X,'DWNSTRM CONST RETN SOURCE FLOW AT I =',I5,' J =',I5,/)  
  113 FORMAT(3X,'UPSTRM VAR WITHDRW SINK FLOW AT I =',I5,' J =',I5,/)  
  114 FORMAT(5X,'K =',I5,5X,'QSERT(K) = ',E12.4,  
     &    5X,'CSERT(K,1) = ',E12.4,5X,'CSERT(K,2) = ',E12.4)  
  115 FORMAT(3X,'DWNSTRM VAR RETN SOURCE FLOW AT I =',I5,' J =',I5,/)  
 6666 FORMAT(' SINGLE VAL CONTROL STRUCTURE TABLE OUT OF BOUNDS ')  
 6667 FORMAT(' NCTL,NCTLT,IU,JU,ID,JD = ',6I5)  
 6668 FORMAT(' SELU,HU,SELD,HD = ',4(2X,E12.4))  
 6676 FORMAT(' DOUBLE VAL CONTROL STRUCTURE TABLE OUT OF BOUNDS, UP ')  
 6677 FORMAT(' NCTL,NCTLT,IU,JU,ID,JD = ',6I5)  
 6678 FORMAT(' SELU,HU,SELD,HD = ',4(2X,E12.4))  
 6679 FORMAT(' HUF,HUL,HDF,HDL = ',4(2X,E12.4))  
 6686 FORMAT(' DOUBLE VAL CONTROL STRUCTURE TABLE OUT OF BOUNDS, DW ')  
 6687 FORMAT(' NCTL,NCTLT,IU,JU,ID,JD = ',6I5)  
 6688 FORMAT(' SELU,HU,SELD,HD = ',4(2X,E12.4))  
 6111 FORMAT(' INVALID NQSIJ LOCATION, NQSIJ,I,J = ',3I5)  
      RETURN  
      END  

