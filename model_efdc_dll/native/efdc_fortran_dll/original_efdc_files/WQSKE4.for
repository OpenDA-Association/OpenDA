      SUBROUTINE WQSKE4  
C  
C**********************************************************************C  
C  
C SOLVE KINETIC EQ FROM K=KC (SURFACE LAYER) TO K=1 (BOTTOM).  
C: AFTER COMPUTING NEW VALUES, STORE WQVO+WQV INTO WQVO(L,K,NWQV) EXCEPT  
C: NWQV=15,19,21.  
C  
C**********************************************************************C  
C  
C  ORGINALLY CODED BY K.-Y. PARK  
C  OPTIMIZED AND MODIFIED BY J.M. HAMRICK  
C  
C  MODIFIED BY J.S. ON 5/5/98 TO ADD A MACROALGAE COMPONENT  
C  
C **  LAST MODIFIED BY JOHN HAMRICK AND MIKE MORTON ON 8 AUGUST 2001  
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
C  
C**********************************************************************C  
C  
      USE GLOBAL  
C  
C**********************************************************************C  
C  
      CNS1=2.718  
C  
      NS=1  
C  
C INITIAL SOLAR RADIATION AT TOP OF SURFACE LAYER  (J.S. 5/4/99)  
C  
      IF(USESHADE.AND.SOLSWRT(L).GE.0.001)THEN
        DO L=2,LA  
          WQI0BOT(L)=PARADJ*2.065*SOLSWRT(L)/PSHADE(L)
        ENDDO
      ELSE
        DO L=2,LA  
          WQI0BOT(L)=WQI0  
          !WQI0BOT(L)=WQI0 * PSHADE(L)  
        ENDDO
      ENDIF
C  
      DO ND=1,NDMWQ  
        LF=2+(ND-1)*LDMWQ  
        LL=LF+LDM-1  
        DO K=KC,1,-1  
C  
C DZWQ=1/H, VOLWQ=1/VOL  
C  
          DO L=LF,LL  
            TWQ(L)=TEM(L,K)  
            SWQ(L)=MAX(SAL(L,K), 0.0)  
            DZWQ(L) = 1.0 / (DZC(K)*HP(L))  
            VOLWQ(L) = DZWQ(L) / DXYP(L)  
            IMWQZT(L)=IWQZMAP(L,K)  
          ENDDO  
C  
          DO L=LF,LL  
            WQBCSET(L,1) = WQWSC(IMWQZT(L))*DZWQ(L)  
            WQBDSET(L,1) = WQWSD(IMWQZT(L))*DZWQ(L)  
            WQBGSET(L,1) = WQWSG(IMWQZT(L))*DZWQ(L)  
            WQRPSET(L,1) = WQWSRP(IMWQZT(L))*DZWQ(L)  
            WQLPSET(L,1) = WQWSLP(IMWQZT(L))*DZWQ(L)  
          ENDDO  
C  
          IF(IWQSRP.EQ.1)THEN  
            DO L=LF,LL  
              WQWSSET(L,1) = WQWSS(IMWQZT(L))*DZWQ(L)  
            ENDDO  
          ENDIF  
C  
          IF(K.NE.KC)THEN  
C  
            DO L=LF,LL  
              IMWQZT1(L)=IWQZMAP(L,K+1)  
            ENDDO  
C  
            DO L=LF,LL  
              WQBCSET(L,2) = WQWSC(IMWQZT1(L))*DZWQ(L)  
              WQBDSET(L,2) = WQWSD(IMWQZT1(L))*DZWQ(L)  
              WQBGSET(L,2) = WQWSG(IMWQZT1(L))*DZWQ(L)  
              WQRPSET(L,2) = WQWSRP(IMWQZT1(L))*DZWQ(L)  
              WQLPSET(L,2) = WQWSLP(IMWQZT1(L))*DZWQ(L)  
            ENDDO  
C  
            IF(IWQSRP.EQ.1)THEN  
              DO L=LF,LL  
                WQWSSET(L,2) = WQWSS(IMWQZT1(L))*DZWQ(L)  
              ENDDO  
            ENDIF  
C  
          ENDIF  
C  
C FIND AN INDEX FOR LOOK-UP TABLE FOR TEMPERATURE DEPENDENCY  
C  
          DO L=LF,LL  
C            IWQT(L) = NINT( 4.*TWQ(L)+121.)  
            IWQT(L)=NINT((TWQ(L)-WQTDMIN)/WQTDINC)  ! *** DSLLC SINGLE LINE
            IF(IWQT(L).LT.1 .OR. IWQT(L).GT.NWQTD)THEN  
C MRM +++++++++ ADDED BY M. MORTON 08/05/98  
              TIMTMP = (DT*FLOAT(N) + TCON*TBEGIN)/86400.0  
              IF(ISDYNSTP.EQ.0)THEN  
                TIMTMP=(DT*FLOAT(N)+TCON*TBEGIN)/86400.  
              ELSE  
                TIMTMP=TIMESEC/86400.  
              ENDIF  
              WRITE(8,911) TIMTMP, L, IL(L), JL(L), K, TWQ(L)  
C MRM +++++++++ ADDED BY M. MORTON 07/24/98  
              WRITE(6,600)IL(L),JL(L),K,TWQ(L)  
              IWQT(L)=MAX(IWQT(L),1)  
              IWQT(L)=MIN(IWQT(L),NWQTD)  
C          STOP 'ERROR!! INVALID WATER TEMPERATURE'  
            ENDIF  
          ENDDO  
C  
  600 FORMAT(' I,J,K,TEM = ',3I5,E13.4)
  911 FORMAT(/,'ERROR  ',
     &    'TIME, L, I, J, K, TWQ(L) = ', F10.5, 4I4, F10.4)  
C  
C NOTE: MRM 04/29/99  ADDED ARRAYS TO KEEP TRACK OF  
C       NITROGEN, PHOSPHORUS, LIGHT, AND TEMPERATURE LIMITS  
C       FOR ALGAE GROWTH FOR CYANOBACTERIA, DIATOMS, GREENS,  
C       AND MACROALGAE.  THESE ARE THE ARRAYS:  
C        XLIMNX(L,K) = NITROGEN    LIMITATION FOR ALGAE GROUP X  
C        XLIMPX(L,K) = PHOSPHORUS  LIMITATION FOR ALGAE GROUP X  
C        XLIMIX(L,K) = LIGHT       LIMITATION FOR ALGAE GROUP X  
C        XLIMTX(L,K) = TEMPERATURE LIMITATION FOR ALGAE GROUP X  
C  
C1-3 ALGAL GROWTH: NUTRIENT  
C  
          DO L=LF,LL  
            IZ=IWQZMAP(L,K)  
  
            RNH4WQ_ = MAX (WQVO(L,K,14), 0.0)  
            RNO3WQ_ = MAX (WQVO(L,K,15), 0.0)  
            PO4DWQ_ = MAX (WQPO4D(L,K), 0.0)  
            RNH4NO3_ = RNH4WQ_ + RNO3WQ_  
            WQGNC = RNH4NO3_ / (WQKHNC+RNH4NO3_+ 1.E-18)  
            WQGND = RNH4NO3_ / (WQKHND+RNH4NO3_+ 1.E-18)  
            WQGNG = RNH4NO3_ / (WQKHNG+RNH4NO3_+ 1.E-18)  
            WQGPC = PO4DWQ_ / (WQKHPC+PO4DWQ_+ 1.E-18)  
            WQGPD = PO4DWQ_ / (WQKHPD+PO4DWQ_+ 1.E-18)  
            WQGPG = PO4DWQ_ / (WQKHPG+PO4DWQ_+ 1.E-18)  
            XLIMNC(L,K) = XLIMNC(L,K) + WQGNC  
            XLIMND(L,K) = XLIMND(L,K) + WQGND  
            XLIMNG(L,K) = XLIMNG(L,K) + WQGNG  
            XLIMPC(L,K) = XLIMPC(L,K) + WQGPC  
            XLIMPD(L,K) = XLIMPD(L,K) + WQGPD  
            XLIMPG(L,K) = XLIMPG(L,K) + WQGPG  
C  
C MODIFIED BY J.S.(5/5/98) FOR MACALGAE  
C  
            IF(IDNOTRVA.GT.0 .AND. K.EQ.1)THEN  
              WQGNM = RNH4NO3_ / (WQKHNM+RNH4NO3_ + 1.E-18)  
              WQGPM = PO4DWQ_ / (WQKHPM+PO4DWQ_ + 1.E-18)  
              WQF1NM = MIN(WQGNM, WQGPM)  
              XLIMNM(L,K) = XLIMNM(L,K) + WQGNM  
              XLIMPM(L,K) = XLIMPM(L,K) + WQGPM  
            ENDIF  
C J.S.  
            WQF1NC = MIN(WQGNC, WQGPC)  
            IF(IWQSI.EQ.1)THEN  
              SADWQ = MAX (WQSAD(L,K), 0.0)  
              WQGSD = SADWQ / (WQKHS+SADWQ+ 1.E-18)  
              WQF1ND = MIN(WQGND, WQGPD, WQGSD)  
            ELSE  
              WQF1ND = MIN(WQGND, WQGPD)  
            ENDIF  
            WQF1NG = MIN(WQGNG, WQGPG)  
  
            IF(IDNOTRVA.GT.0)THEN  
              PO4DWQ_ = MAX (WQPO4D(L,K), 0.0)  
            ENDIF  
C  
C ALGAL GROWTH: LIGHT, WQHT(K)=REAL(KC-K)/REAL(KC)  
C IN C&C, F2IC=F2IC/FCYAN, FACTOR TO ALLOW CYANOBACTERIA MAT FORMATION  
C  
C MRM 05/12/1999 USE RILEY (1956) EQUATION TO COMPUTE LIGHT EXTINCTION  
C     AS A FUNCTION OF CHL CONC. IF WQKECHL IS LESS THAN ZERO:  
C  
            IF(SOLSWRT(L).GE.0.001)THEN
              IF(USESHADE)THEN
                WQI0 = PARADJ*2.065*SOLSWRT(L)
              ENDIF
              XMRM = WQKECHL*WQCHL(L,K)  
              IF(WQKECHL .LT. 0.0)THEN  
                XMRM = 0.054*WQCHL(L,K)**0.6667 + 0.0088*WQCHL(L,K)  
              ENDIF  
              WQKESS = WQKEB(IMWQZT(L))+WQKETSS*SEDT(L,K) + XMRM  
              WQKESS1 = WQKESS  
              IF(K.NE.KC)THEN  
                XMRM = WQKECHL*WQCHL(L,KC)  
                IF(WQKECHL .LT. 0.0)THEN  
                  XMRM = 0.054*WQCHL(L,KC)**0.6667 + 0.0088*WQCHL(L,KC)  
                ENDIF  
                WQKESS1=WQKEB(IMWQZT(L))+WQKETSS*SEDT(L,KC) + XMRM  
              ENDIF  
C  
C COMPUTE SECCHI DEPTH FOR USE AS OUTPUT VARIABLE:  
C  
              WQKETOT(L,K) = WQKESS  
              WQAVGIO = WQCIA*WQI0 + WQCIB*WQI1 + WQCIC*WQI2  
C  
              IF(IWQSUN .EQ. 2)THEN  
                WQAVGIO = WQCIA*WQI1 + WQCIB*WQI2 + WQCIC*WQI3  
              ENDIF  
             !WQAVGIO = WQAVGIO * PSHADE(L)  
C  
              WQISC = MAX( WQAVGIO*EXP(-WQKESS1*WQDOPC), WQISMIN )  
              WQISD = MAX( WQAVGIO*EXP(-WQKESS1*WQDOPD), WQISMIN )  
              WQISG = MAX( WQAVGIO*EXP(-WQKESS1*WQDOPG), WQISMIN )  
              WQTT1 = (CNS1 * WQFD * DZWQ(L)) / WQKESS  
              WQFDI0 = - WQI0BOT(L) / (WQFD + 1.E-18)  
C  
              WQFDC = WQFDI0 / (WQISC + 1.E-18)  
              WQFDD = WQFDI0 / (WQISD + 1.E-18)  
              WQFDG = WQFDI0 / (WQISG + 1.E-18)  
              WQHTT = WQHT(K) * HP(L)  
C  
              WQTTB = EXP( -WQKESS * (WQHTT+1.0/DZWQ(L)) )  
              WQTTT = EXP( -WQKESS * WQHTT )  
              WQF2IC = WQTT1 * (EXP(WQFDC*WQTTB) - EXP(WQFDC*WQTTT))  
              WQF2ID = WQTT1 * (EXP(WQFDD*WQTTB) - EXP(WQFDD*WQTTT))  
              WQF2IG = WQTT1 * (EXP(WQFDG*WQTTB) - EXP(WQFDG*WQTTT))  
C  
C MRM APPLY SHADE FACTOR HERE:  
C  
              WQF2IC = WQF2IC * PSHADE(L)  
              WQF2ID = WQF2ID * PSHADE(L)  
              WQF2IG = WQF2IG * PSHADE(L)  
              XLIMIC(L,K) = XLIMIC(L,K) + WQF2IC  
              XLIMID(L,K) = XLIMID(L,K) + WQF2ID  
              XLIMIG(L,K) = XLIMIG(L,K) + WQF2IG  
C  
C UPDATE SOLAR RADIATION AT BOTTOM OF THIS LAYER  
C  
              WQI0BOT(L)=WQI0BOT(L)*EXP(-WQKESS*(1.0/DZWQ(L)))  
            ELSE
              WQF2IC=0.0
              WQF2ID=0.0
              WQF2IG=0.0
            ENDIF
C  
C MODIFIED BY J.S.(5/5/98) FOR MACALGAE  
C  
            IF(IDNOTRVA.GT.0 .AND. K.EQ.1)THEN  
              WQFDI0 = - WQI0BOT(L) / (WQFD + 1.E-18)  
              WQISM = MAX( WQAVGIO*EXP(-WQKESS1*WQDOPM(IZ)), WQISMIN )  
              WQFDM = WQFDI0 / (WQISM + 1.E-18)  
              WQF2IM = WQTT1 * (EXP(WQFDM*WQTTB) - EXP(WQFDM*WQTTT))  
C  
C MRM APPLY SHADE FACTOR HERE:  
C  
              WQF2IM = WQF2IM * PSHADE(L)  
C  
C MRM 09/02/99: APPLY MACROALGAE VELOCITY LIMITATION HERE:  
C  
C          WQVEL=SQRT(U(L,K)*U(L,K) + V(L,K)*V(L,K))  
              UMRM = MAX(U(L,K), U(L+1,K))  
              VMRM = MAX(V(L,K), V(LNC(L),K))  
              WQVEL=SQRT(UMRM*UMRM + VMRM*VMRM)  
              WQLVF=1.0  
C  
C OPTION 1 FOR VELOCITY LIMITATION ASSUMES MACROALGAE GROWTH  
C IS LIMITED AT LOW VELOCITIES DUE TO REDUCED AVAILABILITY OF  
C NUTRIENTS REACHING THE ALGAE BIOMASS.  USES A MICHAELIS-MENTON  
C TYPE OF EQUATION.  
C  
              IF(IWQVLIM .EQ. 1)THEN  
                IF(WQVEL .GT. WQKMVMIN(L))THEN  
                  WQLVF = WQVEL / (WQKMV(L) + WQVEL)  
                ELSE  
                  WQLVF = WQKMVMIN(L) / (WQKMV(L) + WQKMVMIN(L))  
                ENDIF  
              ENDIF  
C  
C OPTION 2 FOR VELOCITY LIMITATION APPLIES A FIVE-PARAMETER LOGISTIC  
C FUNCTION THAT CAN BE ADJUSTED TO LIMIT MACROALGAE GROWTH FOR  
C EITHER LOW OR HIGH (SCOUR) VELOCITIES.  IN STREAMS WITH LOW NUTRIENTS,  
C THE LOW VELOCITY WILL LIKELY BE LIMITING SINCE AMPLE NUTRIENTS MAY  
C NOT REACH THE ALGAE BIOMASS DUE TO REDUCED FLOW.  IN STREAMS WITH  
C ABUNDANT NUTRIENTS, LOW VELOCITIES WILL NOT LIMIT MACROALGAE GROWTH,  
C INSTEAD, HIGH VELOCITIES WILL LIKELY SCOUR THE MACROALGAE AND DETACH  
C IT FROM THE SUBSTRATE.  
C  
              IF(IWQVLIM .EQ.2)THEN  
                XNUMER = WQKMVA(L) - WQKMVD(L)  
                XDENOM = 1.0 + (WQVEL/WQKMVC(L))**WQKMVB(L)  
                WQLVF = WQKMVD(L) + ( XNUMER / (XDENOM**WQKMVE(L)) )  
              ENDIF  
C  
C USE THE MORE SEVERELY LIMITING OF VELOCITY OR NUTRIENT FACTORS:  
C  
              XMRM = MIN(WQLVF, WQF1NM)  
              WQF1NM = XMRM  
C  
C MRM 09/02/99: APPLY MACROALGAE DENSITY LIMITATION HERE:  
C FIRST CONVERT FROM MACROALGAE FROM A CONCENTRATION (MG C/M3)  
C TO A DENSITY (MG C/M2).  
C  
              XMRM = WQVO(L,K,IDNOTRVA)*DZC(K)*HP(L)  
              WQLDF = WQKBP(L) / (WQKBP(L) + XMRM)  
  
              WQPM(L)= WQPMM(IMWQZT(L))*WQF1NM*WQF2IM*WQTDGM(IWQT(L))*  
     &            WQLDF  
              XLIMVM(L,K) = XLIMVM(L,K) + WQLVF  
              XLIMDM(L,K) = XLIMDM(L,K) + WQLDF  
              XLIMIM(L,K) = XLIMIM(L,K) + WQF2IM  
              XLIMTM(L,K) = XLIMTM(L,K) + WQTDGM(IWQT(L))  
  
            ENDIF  
            XLIMTC(L,K) = XLIMTC(L,K) + WQTDGC(IWQT(L))  
            XLIMTD(L,K) = XLIMTD(L,K) + WQTDGD(IWQT(L))  
            XLIMTG(L,K) = XLIMTG(L,K) + WQTDGG(IWQT(L))  
C J.S.  
  
C  
C: WQSTOX=WQSTOX**2  
C  
            IF(IWQSTOX.EQ.1)THEN  
              WQF4SC = WQSTOX / (WQSTOX + SWQ(L)*SWQ(L)+1.E-12)  
              WQPC(L)=WQPMC(IMWQZT(L))*WQF1NC*WQF2IC*WQTDGC(IWQT(L))
     &            *WQF4SC  
            ELSE  
              WQPC(L) = WQPMC(IMWQZT(L))*WQF1NC*WQF2IC*WQTDGC(IWQT(L))  
            ENDIF  
            WQPD(L) = WQPMD(IMWQZT(L))*WQF1ND*WQF2ID*WQTDGD(IWQT(L))  
            WQPG(L) = WQPMG(IMWQZT(L))*WQF1NG*WQF2IG*WQTDGG(IWQT(L))  
  
C MRM: WHEN USING HOURLY SOLAR RADIATION, SHUT DOWN PHOTOSYNTHESIS  
C      AT NIGHT, I.E., WHEN SOLAR RADIATION IS LESS THAN 0.001 (05/11/99  
            IF(IWQSUN .EQ. 2)THEN  
              IF(WQI0 .LE. 0.001)THEN  
                WQPC(L) = 0.0  
                WQPD(L) = 0.0  
                WQPG(L) = 0.0  
                WQPM(L) = 0.0  
              ENDIF  
            ENDIF  
C  
C ALGAL BASAL METABOLISM & PREDATION  
C  
            WQBMC(L) = WQBMRC(IMWQZT(L)) * WQTDRC(IWQT(L))  
            WQPRC(L) = WQPRRC(IMWQZT(L)) * WQTDRC(IWQT(L))  
C MRM: 06/20/98  
C THE VARIABLE WQTDGP ADJUSTS PREDATION AND BASAL METABOLISM BASED ON A  
C LOWER/UPPER OPTIMUM TEMPERATURE FUNCTION.  THIS WILL ALLOW DIATOMS TO  
C BLOOM IN WINTER IF WQTDGP IS CLOSE TO ZERO.  
C        WQBMD(L) = WQBMRD(IMWQZT(L)) * WQTDRD(IWQT(L))  
C        WQPRD(L) = WQPRRD(IMWQZT(L)) * WQTDRD(IWQT(L))  
            WQBMD(L)=WQBMRD(IMWQZT(L))*WQTDRD(IWQT(L))*WQTDGP(IWQT(L))  
            WQPRD(L)=WQPRRD(IMWQZT(L))*WQTDRD(IWQT(L))*WQTDGP(IWQT(L))  
            WQBMG(L) = WQBMRG(IMWQZT(L)) * WQTDRG(IWQT(L))  
            WQPRG(L) = WQPRRG(IMWQZT(L)) * WQTDRG(IWQT(L))  
C  
C MODIFIED BY J.S.(5/5/98) FOR MACALGAE  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQBMM(L) = WQBMRM(IMWQZT(L)) * WQTDRM(IWQT(L))  
              WQPRM(L) = WQPRRM(IMWQZT(L)) * WQTDRM(IWQT(L))  
            ENDIF  
  
C J.S.  
C  
C4-6 ORGANIC CARBON: WQAANOX=WQAANOX*WQKHORDO  
C  
            WQOBTOT_ = WQVO(L,K,1)+WQVO(L,K,2)+WQVO(L,K,3)  
            WQKRPC(L) = (WQKRC + WQKRCALG*WQOBTOT_) * WQTDHDR(IWQT(L))  
            WQKLPC(L) = (WQKLC + WQKLCALG*WQOBTOT_) * WQTDHDR(IWQT(L))  
            XMRM = 0.0  
            IF(IDNOTRVA.GT.0 .AND. K.EQ.1)THEN  
              XMRM = WQKDCALM(IZ) * WQVO(L,K,IDNOTRVA)  
            ENDIF  
C  
C M. MORTON 08/28/99: ADDED SPATIALLY VARIABLE DOC HYDROLYSIS RATE WQKDC  
C    TO ACHIEVE BETTER CONTROL IN SYSTEMS WITH A COMBINATION OF FRESHWAT  
C    STREAMS AND TIDAL RIVERS WITH DIFFERENT CHARACTERISTICS.  
C  
            WQKDOC=(WQKDC(IZ)+WQKDCALG*WQOBTOT_+XMRM)*WQTDMNL(IWQT(L))  
            O2WQ_ = MAX(WQVO(L,K,19), 0.0)  
            WQTT1 = WQKDOC / (WQKHORDO + O2WQ_+ 1.E-18)  
            WQKHR(L) = WQTT1 * O2WQ_  
            WQDENIT(L)=WQTT1*WQAANOX*RNO3WQ_/(WQKHDNN+RNO3WQ_+1.E-18)  
C  
C 7-10 PHOSPHORUS  
C  
            WQAPC(L)=1.0/(WQCP1PRM+WQCP2PRM*EXP(-WQCP3PRM*PO4DWQ_))  
            WQKHP = (WQKHPC+WQKHPD+WQKHPG) / 3.0  
            WQTT1 = WQKHP / (WQKHP+PO4DWQ_+ 1.E-18) * WQOBTOT_  
            WQKRPP(L) = (WQKRP + WQKRPALG*WQTT1) * WQTDHDR(IWQT(L))  
            WQKLPP(L) = (WQKLP + WQKLPALG*WQTT1) * WQTDHDR(IWQT(L))  
            WQKDOP(L) = (WQKDP + WQKDPALG*WQTT1) * WQTDMNL(IWQT(L))  
C  
C10 PO4T  
C  
            IF(IWQSRP.EQ.1)THEN  
              WQTTM = WQKPO4P*WQTAMP(L,K)  
              WQH10(L) = - WQWSSET(L,1) * WQTTM / (1.0+WQTTM)  
              IF(K.NE.KC)THEN  
                WQTTM = WQKPO4P*WQTAMP(L,K+1)  
                WQT10(L) = WQWSSET(L,2) * WQTTM / (1.0+WQTTM)  
              ENDIF  
            ELSE IF(IWQSRP.EQ.2)THEN  
              WQTTS = WQKPO4P*SEDT(L,K)  
              WQH10(L) = - WSEDO(NS) * WQTTS * DZWQ(L) / (1.0+WQTTS)  
              IF(K.NE.KC)THEN  
                WQTTS = WQKPO4P*SEDT(L,K)  
                WQT10(L) = WSEDO(NS) * WQTTS * DZWQ(L) / (1.0+WQTTS)  
              ENDIF  
            ELSE  
              WQH10(L) = 0.0  
              WQT10(L) = 0.0  
            ENDIF  
C  
C 11-15 NITROGEN  
C  
            WQKHN = (WQKHNC+WQKHND+WQKHNG) / 3.0  
            WQTT1 = WQKHN / (WQKHN+RNH4NO3_+ 1.E-18) * WQOBTOT_  
            WQKRPN(L) = (WQKRN + WQKRNALG*WQTT1) * WQTDHDR(IWQT(L))  
            WQKLPN(L) = (WQKLN + WQKLNALG*WQTT1) * WQTDHDR(IWQT(L))  
            WQKDON(L) = (WQKDN + WQKDNALG*WQTT1) * WQTDMNL(IWQT(L))  
C  
C14 NH4: WQFTNIT=WQNITM*WQFTNIT  
C  
            IF(RNH4NO3_.EQ.0.0)THEN  
              WQPNC(L)=0.0  
              WQPND(L)=0.0  
              WQPNG(L)=0.0  
              WQPNM(L)=0.0  
            ELSE  
              WQTTC = RNH4WQ_/(WQKHNC+RNO3WQ_+ 1.E-18)  
              WQTTD = RNH4WQ_/(WQKHND+RNO3WQ_+ 1.E-18)  
              WQTTG = RNH4WQ_/(WQKHNG+RNO3WQ_+ 1.E-18)  
              WQTTM = RNH4WQ_/(WQKHNM+RNO3WQ_+ 1.E-18)  
              WQPNC(L) = (RNO3WQ_/(WQKHNC+RNH4WQ_+ 1.E-18)  
     &            + WQKHNC/(RNH4NO3_+ 1.E-18)) * WQTTC  
              WQPND(L) = (RNO3WQ_/(WQKHND+RNH4WQ_+ 1.E-18)  
     &            + WQKHND/(RNH4NO3_+ 1.E-18)) * WQTTD  
              WQPNG(L) = (RNO3WQ_/(WQKHNG+RNH4WQ_+ 1.E-18)  
     &            + WQKHNG/(RNH4NO3_+ 1.E-18)) * WQTTG  
              WQPNM(L) = (RNO3WQ_/(WQKHNM+RNH4WQ_+ 1.E-18)  
     &            + WQKHNM/(RNH4NO3_+ 1.E-18)) * WQTTM  
  
            ENDIF  
            WQNIT(L) = O2WQ_ * WQTDNIT(IWQT(L)) /  
     &          ( (WQKHNDO+O2WQ_) * (WQKHNN+RNH4WQ_) + 1.E-18)  
C  
C16-17 SILICA  
C  
            IF(IWQSI.EQ.1)THEN  
              IF(IWQSRP.EQ.1)THEN  
                WQTTM = WQKSAP*WQTAMP(L,K)  
                WQN17(L) = - WQWSSET(L,1) * WQTTM / (1.0+WQTTM)  
                IF(K.NE.KC)THEN  
                  WQTTM = WQKSAP*WQTAMP(L,K+1)  
                  WQT17(L) = WQWSSET(L,2) * WQTTM / (1.0+WQTTM)  
                ENDIF  
              ELSE IF(IWQSRP.EQ.2)THEN  
                WQTTS = WQKSAP*SEDT(L,K)  
                WQN17(L) = - WSEDO(NS) * WQTTS * DZWQ(L) / (1.0+WQTTS)  
                IF(K.NE.KC)THEN  
                  WQTTS = WQKSAP*SEDT(L,K+1)  
                  WQT17(L) = WSEDO(NS) * WQTTS * DZWQ(L) / (1.0+WQTTS)  
                ENDIF  
              ELSE  
                WQN17(L) = 0.0  
                WQT17(L) = 0.0  
              ENDIF  
            ENDIF  
C  
C 04/29/99 MRM:  
C THE FOLLOWING ARRAYS WERE ADDED TO KEEP TRACK OF THE VARIOUS COMPONENT  
C OF DISSOLVED OXYGEN.  THE INSTANTANEOUS VALUES FOR EACH COMPONENT ARE  
C SUMMED IN THE ARRAYS AND THEN DUMPED TO THE WQDOCOMP.BIN FILE AT THE  
C SAME TIME INTERVAL AS FOR THE WQWCAVG.BIN FILES (I.E., IWQTSDT INTERVA  
C USUALLY DAILY AVERAGES).  THE ARRAY DESCRIPTIONS ARE:  
C  
C  XDOSAT(L,K) = D.O. SATURATION FOR CELL L, LAYER K (MG/L)  
C  XDODEF(L,K) = D.O. DEFICIT FOR CELL L, LAYER K (MG/L)  
C  XDOPSL(L,K) = D.O. COMPONENT FOR EXTERNAL LOADS (MG/L/DAY)  
C  XDOSOD(L,K) = D.O. COMPONENT FOR SEDIMENT OXYGEN DEMAND  
C  XDOKAR(L,K) = D.O. COMPONENT FOR REAERATION  
C  XDODOC(L,K) = D.O. COMPONENT FOR DISS. ORG. CARBON DECAY  
C  XDONIT(L,K) = D.O. COMPONENT FOR AMMONIA NITRIFICATION  
C  XDOCOD(L,K) = D.O. COMPONENT FOR CHEM. OXY. DEMAND OXIDATION  
C  XDOPPB(L,K) = D.O. COMPONENT FOR PHOTOSYNTHESIS OF TOTAL CHLOROPHYLL  
C  XDORRB(L,K) = D.O. COMPONENT FOR RESPIRATION OF TOTAL CHLOROPHYLL  
C  XDOPPM(L,K) = D.O. COMPONENT FOR PHOTOSYNTHESIS OF MACROALGAE  
C  XDORRM(L,K) = D.O. COMPONENT FOR RESPIRATION OF MACROALGAE  
C  XDOALL(L,K) = SUM OF THE ABOVE 10 D.O. COMPONENTS  
C  XDODZ (L,K) = LAYER THICKNESS (METERS)  
C  NLIM = COUNTER FOR NUMBER OF ITEMS SUMMED IN EACH ARRAY SLOT  
C  
C18-19 COD, O2: WQO18(L)=DTWQO2*WQO18, WQKRDOS(L)=-WQP19(L)*WQDOS  
C  
            WQO18(L)= -DTWQO2*WQKCOD(IWQT(L),IZ)*O2WQ_ /  
     &          (WQKHCOD(IZ) + O2WQ_ + 1.E-18)  
C  
C TT THE FOLLOWING MODIFICATION TO THE D.O. SATURATION CALCULATION MADE  
C TT BY J.M. HAMRICK / M.R. MORTON ON 03/08/97.  SEE CHAPRA (1997) PG. 3  
C  
            TVAL1=1./(TWQ(L)+273.15)  
            TVAL2=TVAL1*TVAL1  
            TVAL3=TVAL1*TVAL2  
            TVAL4=TVAL2*TVAL2  
            RLNSAT1=-139.3441+(1.575701E+5*TVAL1)-(6.642308E+7*TVAL2)  
     &          +(1.2438E+10*TVAL3)-(8.621949E+11*TVAL4)  
            RLNSAT2=RLNSAT1-SWQ(L)*( 1.7674E-2-(1.0754E+1*TVAL1)  
     &          +(2.1407E+3*TVAL2) )  
            WQDOS(L) = EXP(RLNSAT2)  
            XDOSAT(L,K) = XDOSAT(L,K) + WQDOS(L)*DTWQ*DZC(K)*HP(L)  
            IF(K.EQ.KC)THEN  
C MRM: 06/20/98  
C IN THE FOLLOWING EQUATION, SALINITY MUST BE IN MG/L, HENCE, SWQ(L)  
C IS MULTIPLIED BY 1000.  
C        TVAL1 = TWQ(L)  
C        TVAL2 = TVAL1*TVAL1  
C        WQDOS = 14.5532 -  0.38217*TVAL1 + 5.4258E-3*TVAL2 -  
C     *     (SWQ(L)*1000.0/1.80655) * (1.665E-4 - 5.866E-6*TVAL1 +  
C     *     9.796E-8*TVAL2)  
CTT          WQDOS = 1.45532E1 + (5.4258E-3*TWQ(L)- 3.8217E-1)*TWQ(L) -  
CTT  *  (SWQ(L)/1.80655)  
CTT  &           * ( 1.665E-4 + (9.796E-8*TWQ(L) - 5.866E-6)*TWQ(L) )  
CTT  UNCOMMENT AND CHANGE LINE BELOW TO ACTIVATE WIND EFFECT ON REAREATI  
C MRM: 06/20/98  
C DO NOT ALLOW WIND SPEEDS ABOVE 11 M/SEC IN THE FOLLOWING EQUATION:  
C        WINDREA = WINDSTKA(L)  
C        IF(WINDSTKA(L) .GT. 11.0) WINDREA = 11.0  
              WINDREA = WINDST(L)  
              WQWREA=0.728*SQRT(WINDREA)+(0.0372*WINDREA-0.317)*WINDREA  
C        WQWREA = 0.728*SQRT(WINDST(L))  
C     &           +(0.0372*WINDST(L) - 0.317)*WINDST(L)  
CTT       WQWREA = 0.0  
  
C J.S. 2/21/99 CORRECT SURFACE VELOCITY  
C          WQP19(L) = - (WQKRO*SQRT(U(L,KC)**2+V(L,KC)**2) + WQWREA)  
C          WQP19(L) = - (WQKRO*SQRT(0.5) + WQWREA) * DZWQ(L)  
C     *      * WQTDKR(IWQT(L))  
C  
C MRM 04/29/1999  USER SPECIFIES CONSTANT REAERATION WQKRO:  
C  
              IF(IWQKA(IZ) .EQ. 0)THEN  
                WQVREA = WQKRO(IZ)  
                WQWREA = 0.0  
              ENDIF  
C  
C MRM 04/12/1999  CONSTANT REAERATION DUE TO WATER VELOCITY,  
C                 WIND VELOCITY COMPUTED ABOVE:  
C  
              IF(IWQKA(IZ) .EQ. 1)THEN  
                WQVREA = WQKRO(IZ)  
              ENDIF  
C  
C MRM 03/06/1999  O'CONNOR-DOBBINS (1958) EQUATION FOR REAERATION IS:  
C    WQKRO = 3.933 TYPICALLY  
C  
C          IF(IWQKA(IZ) .EQ. 2)THEN  
C            XMRM = SQRT(U(L,K)*U(L,K) + V(L,K)*V(L,K))  
C            UMRM = MAX(U(L,K), U(L+1,K))  
C            VMRM = MAX(V(L,K), V(LNC(L),K))  
C            XMRM = SQRT(UMRM*UMRM + VMRM*VMRM)  
C            WQVREA = WQKRO(IZ) * XMRM**0.5 / HP(L)**1.5  
C          ENDIF  
C  
              IF(IWQKA(IZ) .EQ. 2)THEN  
C            XMRM = SQRT(U(L,K)*U(L,K) + V(L,K)*V(L,K))  
                UMRM = 0.5*(U(L,K)+U(L+1,K))  
                VMRM = 0.5*(V(L,K)+V(LNC(L),K))  
                XMRM = SQRT(UMRM*UMRM + VMRM*VMRM)  
                WQVREA = WQKRO(IZ) * XMRM**0.5 / HP(L)**0.5  
              ENDIF  
C  
C MRM 04/12/1999  OWENS AND GIBBS (1964) REAERATION EQUATION:  
C    WQKRO = 5.32 TYPICALLY  
C  
              IF(IWQKA(IZ) .EQ. 3)THEN  
C            XMRM = SQRT(U(L,K)*U(L,K) + V(L,K)*V(L,K))  
                UMRM = MAX(U(L,K), U(L+1,K))  
                VMRM = MAX(V(L,K), V(LNC(L),K))  
                XMRM = SQRT(UMRM*UMRM + VMRM*VMRM)  
                WQVREA = WQKRO(IZ) * XMRM**0.67 / HP(L)**1.85  
              ENDIF  
C  
C MODIFIED OWENS AND GIBBS REAERATION EQUATION:  
C NOTE: NORMALIZED TO A DEPTH OF 1.0 FT, I.E., THIS EQUATION GIVES THE  
C       SAME REAERATION AS OWENS & GIBBS AT 1.0 FT DEPTH; AT HIGHER  
C       DEPTHS IT GIVES LARGER REAERATION THAN OWENS & GIBBS.  
C WQKRO = 5.32 TYPICALLY  
C  
              IF(IWQKA(IZ) .EQ. 4)THEN  
C                XMRM = XMRM * SQRT(HP(L))/0.3048  
C                XMRM = SQRT(U(L,K)*U(L,K) + V(L,K)*V(L,K))  
                UMRM = MAX(U(L,K), U(L+1,K))  
                VMRM = MAX(V(L,K), V(LNC(L),K))  
                XMRM = SQRT(UMRM*UMRM + VMRM*VMRM)  
                YMRM = HP(L)*3.0*(1.0 - HP(L)/(HP(L)+0.1524))  
                WQVREA = WQKRO(IZ) * XMRM**0.67 / YMRM**1.85  
              ENDIF  
C  
C NOW COMBINE REAERATION DUE TO WATER VELOCITY AND WIND STRESS:  
C  
              WQVREA = WQVREA * REAC(IZ)  
              WQWREA = WQWREA * REAC(IZ)  
              WQP19(L)=-(WQVREA+WQWREA)*DZWQ(L)*WQTDKR(IWQT(L),IZ)  
              WQKRDOS(L) = - WQP19(L)*WQDOS(L)  
            ELSE  
              WQP19(L) = 0.0  
            ENDIF  
C  
C20 TAM: WQTDTAM=WQKHBMF*BFTAM*EXP()  
C  
            IF(IWQSRP.EQ.1)THEN  
              WQR20(L) = (WQWDSL(L,K,20)+WQWPSL(L,K,20))*VOLWQ(L)  
     &            + (WQVO(L,K,20) - WQTAMP(L,K)) * WQWSSET(L,1)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
              IF(K.EQ.KC)THEN  
                WQR20(L) = WQR20(L) + WQATML(L,KC,20) * VOLWQ(L)  
              ENDIF  
              IF(K.EQ.1) WQR20(L) = WQR20(L)  
     &            + WQTDTAM(IWQT(L))*DZWQ(L)/(WQKHBMF+O2WQ_+ 1.E-18)  
              IF(K.NE.KC) WQR20(L) = WQR20(L)  
     &            + (WQVO(L,K+1,20) - WQTAMP(L,K+1)) * WQWSSET(L,2)  
            ENDIF  
C  
          ENDDO  
C  
C TRAPEZOIDAL SOLUTION OF KINETIC EQS: AFTER COMPUTING NEW VALUES, STORE  
C WQVO+WQV INTO WQVO(L,K,NWQV)  
C  
C MODIFIED BY J.S.(5/5/98) FOR MACALGAE  
C  
          IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
            DO L=LF,LL  
              WQA1C=(WQPM(L)-WQBMM(L)-WQPRM(L)-WQWSM*DZWQ(L))*DTWQO2  
              WQVA1C = 1.0 / (1.0 - WQA1C)  
              WQV(L,K,IDNOTRVA)=(WQVO(L,K,IDNOTRVA)+WQA1C*
     &                           WQVO(L,K,IDNOTRVA))*WQVA1C*SMAC(L)  
              WQV(L,K,IDNOTRVA) = MAX(WQV(L,K,IDNOTRVA),WQMCMIN)*SMAC(L)  
              WQVO(L,K,IDNOTRVA) = WQVO(L,K,IDNOTRVA)+WQV(L,K,IDNOTRVA)  
            ENDDO  
          ENDIF  
C J.S  
C  
C1 BC: WQT1C=WQBCSET(L,2)  
C  
          DO L=LF,LL  
            WQA1C=(WQPC(L)-WQBMC(L)-WQPRC(L)-WQBCSET(L,1))*DTWQO2  
            WQKK(L) = 1.0 / (1.0 - WQA1C)  
            WQR1C = (WQWDSL(L,K,1) + WQWPSL(L,K,1)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR1C = WQR1C + WQATML(L,KC,1) * VOLWQ(L)  
            ENDIF  
            WQRR(L) = WQVO(L,K,1) + DTWQ*WQR1C + WQA1C*WQVO(L,K,1)  
          ENDDO  
C  
          IF(K.NE.KC)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + DTWQO2*WQBCSET(L,2)*WQVO(L,K+1,1)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
            WQV(L,K,1)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,1)  
CTT        WQV(L,K,1) = WQRR(L)*WQKK(L)  
            WQVO(L,K,1) = WQVO(L,K,1)+WQV(L,K,1)  
C  
C2 BD: WQT2D=WQBDSET(L,2)  
C  
            WQA2D=(WQPD(L)-WQBMD(L)-WQPRD(L)-WQBDSET(L,1))*DTWQO2  
            WQKK(L) = 1.0 / (1.0 - WQA2D)  
            WQR2D = (WQWDSL(L,K,2) + WQWPSL(L,K,2)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR2D = WQR2D + WQATML(L,KC,2) * VOLWQ(L)  
            ENDIF  
            WQRR(L) = WQVO(L,K,2) + DTWQ*WQR2D + WQA2D*WQVO(L,K,2)  
          ENDDO  
C  
          IF(K.NE.KC)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + DTWQO2*WQBDSET(L,2)*WQVO(L,K+1,2)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
            WQV(L,K,2)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,2)  
CTT        WQV(L,K,2) = WQRR(L)*WQKK(L)  
            WQVO(L,K,2) = WQVO(L,K,2)+WQV(L,K,2)  
C  
C3 BG: WQT3D=WQBGSET(L,2)  
C  
            WQA3G=(WQPG(L)-WQBMG(L)-WQPRG(L)-WQBGSET(L,1))*DTWQO2  
            WQKK(L) = 1.0 / (1.0 - WQA3G)  
            WQR3G = (WQWDSL(L,K,3) + WQWPSL(L,K,3)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR3G = WQR3G + WQATML(L,KC,3) * VOLWQ(L)  
            ENDIF  
            WQRR(L) = WQVO(L,K,3) + DTWQ*WQR3G + WQA3G*WQVO(L,K,3)  
          ENDDO  
C  
          IF(K.NE.KC)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + DTWQO2*WQBGSET(L,2)*WQVO(L,K+1,3)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
            WQV(L,K,3)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,3)  
CTT        WQV(L,K,3) = WQRR(L)*WQKK(L)  
            WQVO(L,K,3) = WQVO(L,K,3)+WQV(L,K,3)  
C  
C4 RPOC: WQA4=WQRR(L),WQT4=WQRPSET(L,2)  
C  
            WQB4 = - (WQKRPC(L)+WQRPSET(L,1))  
            WQKK(L) = 1.0 / (1.0 - DTWQO2*WQB4)  
            WQA4 = WQFCRP * (WQPRC(L)*WQVO(L,K,1)  
     &          + WQPRD(L)*WQVO(L,K,2) + WQPRG(L)*WQVO(L,K,3))  
C  
C ADD MACALGAL SOURCE     J.S.  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQA4 = WQA4+WQFCRPM*WQPRM(L)*WQVO(L,K,IDNOTRVA)  
            ENDIF  
C J.S.  
            WQR4 = (WQWDSL(L,K,4) + WQWPSL(L,K,4)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR4 = WQR4 + WQATML(L,KC,4) * VOLWQ(L)  
            ENDIF  
            WQRR(L) = WQVO(L,K,4) + DTWQ*WQR4 + DTWQO2*( WQA4  
     &          + WQB4*WQVO(L,K,4) )  
          ENDDO  
C  
          IF(K.NE.KC)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + DTWQO2*WQRPSET(L,2)*WQVO(L,K+1,4)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
            WQV(L,K,4)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,4)  
CTT        WQV(L,K,4) = WQRR(L)*WQKK(L)  
            WQVO(L,K,4) = WQVO(L,K,4)+WQV(L,K,4)  
C  
C5 LPOC: WQT5=WQLPSET(L,2)  
C  
            WQC5 = - (WQKLPC(L)+WQLPSET(L,1))  
            WQKK(L) = 1.0 / (1.0 - DTWQO2*WQC5)  
            WQA5 = WQFCLP * (WQPRC(L)*WQVO(L,K,1)  
     &          + WQPRD(L)*WQVO(L,K,2) + WQPRG(L)*WQVO(L,K,3))  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQA5 =WQA5 + WQFCLPM * WQPRM(L)*WQVO(L,K,IDNOTRVA)  
            ENDIF  
C J.S.  
            WQR5 = (WQWDSL(L,K,5) + WQWPSL(L,K,5)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR5 = WQR5 + WQATML(L,KC,5) * VOLWQ(L)  
            ENDIF  
            WQRR(L) = WQVO(L,K,5) + DTWQ*WQR5 + DTWQO2*( WQA5  
     &          + WQC5*WQVO(L,K,5) )  
          ENDDO  
C  
          IF(K.NE.KC)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + DTWQO2*WQLPSET(L,2)*WQVO(L,K+1,5)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
            IZ=IWQZMAP(L,K)  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
            WQV(L,K,5)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,5)  
CTT        WQV(L,K,5) = WQRR(L)*WQKK(L)  
            WQVO(L,K,5) = WQVO(L,K,5)+WQV(L,K,5)  
C  
C6 DOC: CFCDXWQ=1-WQFCDX, WQB6=WQKRPC(L),WQC6=WQKLPC(L)  
C  
            WQD6 = - (WQKHR(L)+WQDENIT(L))  
            WQKK(L) = 1.0 / (1.0 - DTWQO2*WQD6)  
            O2WQ_ = MAX(WQVO(L,K,19), 0.0)  
            WQA6C=(WQFCDC+CFCDCWQ*WQKHRC/(WQKHRC+O2WQ_+1.E-18))*WQBMC(L)  
            WQA6D=(WQFCDD+CFCDDWQ*WQKHRD/(WQKHRD+O2WQ_+1.E-18))*WQBMD(L)  
            WQA6G=(WQFCDG+CFCDGWQ*WQKHRG/(WQKHRG+O2WQ_+1.E-18))*WQBMG(L)  
            WQA6 = ( WQA6C + WQFCDP*WQPRC(L) )*WQVO(L,K,1)  
     &          + ( WQA6D + WQFCDP*WQPRD(L) )*WQVO(L,K,2)  
     &          + ( WQA6G + WQFCDP*WQPRG(L) )*WQVO(L,K,3)  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQA6M=(WQFCDM+(1-WQFCDM)*WQKHRM(IZ) /  
     &            (WQKHRM(IZ) + O2WQ_ + 1.E-18))*WQBMM(L)  
              WQA6 =WQA6+ (WQA6M+ WQFCDPM*WQPRM(L))*WQVO(L,K,IDNOTRVA)  
            ENDIF  
C J.S.  
C  
            WQR6 = (WQWDSL(L,K,6) + WQWPSL(L,K,6)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR6 = WQR6 + WQATML(L,KC,6) * VOLWQ(L)  
            ENDIF  
            WQRR(L)=WQVO(L,K,6)+DTWQ*WQR6+DTWQO2*(WQA6+WQKRPC(L)*  
     &          WQVO(L,K,4) + WQKLPC(L)*WQVO(L,K,5) + WQD6*WQVO(L,K,6) )  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
            WQV(L,K,6)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,6)  
CTT        WQV(L,K,6) = WQRR(L)*WQKK(L)  
            WQVO(L,K,6) = WQVO(L,K,6)+WQV(L,K,6)  
C  
C7 RPOP: WQT7=WQT4=WQRPSET(L,2)  
C  
            WQE7 = - (WQKRPP(L)+WQRPSET(L,1))  
            WQKK(L) = 1.0 / (1.0 - DTWQO2*WQE7)  
            WQA7C = (WQFPRC*WQBMC(L) + WQFPRP*WQPRC(L)) * WQVO(L,K,1)  
            WQA7D = (WQFPRD*WQBMD(L) + WQFPRP*WQPRD(L)) * WQVO(L,K,2)  
            WQA7G = (WQFPRG*WQBMG(L) + WQFPRP*WQPRG(L)) * WQVO(L,K,3)  
            WQA7 = (WQA7C+WQA7D+WQA7G) * WQAPC(L)  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQA7 = WQA7 + (WQFPRM*WQBMM(L) + WQFPRPM*WQPRM(L))  
     &            * WQVO(L,K,IDNOTRVA)* WQAPC(L)*WQAPCM  
  
            ENDIF  
C J.S.  
            WQR7 = (WQWDSL(L,K,7) + WQWPSL(L,K,7)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR7 = WQR7 + WQATML(L,KC,7) * VOLWQ(L)  
            ENDIF  
            WQRR(L) = WQVO(L,K,7) + DTWQ*WQR7 + DTWQO2*( WQA7  
     &          + WQE7*WQVO(L,K,7) )  
          ENDDO  
C  
          IF(K.NE.KC)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + DTWQO2*WQRPSET(L,2)*WQVO(L,K+1,7)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
            WQV(L,K,7)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,7)  
CTT        WQV(L,K,7) = WQRR(L)*WQKK(L)  
            WQVO(L,K,7) = WQVO(L,K,7)+WQV(L,K,7)  
C  
C8 LPOP: WQT8=WQT5=WQLPSET(L,2)  
C  
            WQF8 = - (WQKLPP(L)+WQLPSET(L,1))  
            WQKK(L) = 1.0 / (1.0 - DTWQO2*WQF8)  
            WQA8C = (WQFPLC*WQBMC(L) + WQFPLP*WQPRC(L)) * WQVO(L,K,1)  
            WQA8D = (WQFPLD*WQBMD(L) + WQFPLP*WQPRD(L)) * WQVO(L,K,2)  
            WQA8G = (WQFPLG*WQBMG(L) + WQFPLP*WQPRG(L)) * WQVO(L,K,3)  
            WQA8 = (WQA8C+WQA8D+WQA8G) * WQAPC(L)  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQA8 = WQA8 +     (WQFPLM*WQBMM(L) + WQFPLPM*WQPRM(L))  
     &            * WQVO(L,K,IDNOTRVA)* WQAPC(L)*WQAPCM  
            ENDIF  
C J.S.  
            WQR8 = (WQWDSL(L,K,8) + WQWPSL(L,K,8)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR8 = WQR8 + WQATML(L,KC,8) * VOLWQ(L)  
            ENDIF  
            WQRR(L) = WQVO(L,K,8) + DTWQ*WQR8 + DTWQO2*( WQA8  
     &          + WQF8*WQVO(L,K,8) )  
          ENDDO  
C  
          IF(K.NE.KC)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + DTWQO2*WQLPSET(L,2)*WQVO(L,K+1,8)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
            WQV(L,K,8)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,8)  
CTT        WQV(L,K,8) = WQRR(L)*WQKK(L)  
            WQVO(L,K,8) = WQVO(L,K,8)+WQV(L,K,8)  
C  
C9 DOP: WQE9=WQKRPP(L),WQF9=WQKLPP(L),WQG9=-WQKDOP(L)  
C  
            WQKK(L) = 1.0 / (1.0 + DTWQO2*WQKDOP(L))  
            WQA9C = (WQFPDC*WQBMC(L) + WQFPDP*WQPRC(L)) * WQVO(L,K,1)  
            WQA9D = (WQFPDD*WQBMD(L) + WQFPDP*WQPRD(L)) * WQVO(L,K,2)  
            WQA9G = (WQFPDG*WQBMG(L) + WQFPDP*WQPRG(L)) * WQVO(L,K,3)  
            WQA9 = (WQA9C+WQA9D+WQA9G) * WQAPC(L)  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQA9 = WQA9 + (WQFPDM*WQBMM(L) + WQFPDPM*WQPRM(L))  
     &            * WQVO(L,K,IDNOTRVA) * WQAPC(L)*WQAPCM  
            ENDIF  
C J.S.  
            WQR9 = (WQWDSL(L,K,9) + WQWPSL(L,K,9)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR9 = WQR9 + WQATML(L,KC,9) * VOLWQ(L)  
            ENDIF  
            WQRR(L)=WQVO(L,K,9)+DTWQ*WQR9+DTWQO2*(WQA9+WQKRPP(L)*  
     &          WQVO(L,K,7)+WQKLPP(L)*WQVO(L,K,8)-WQKDOP(L)*WQVO(L,K,9))  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
            WQV(L,K,9)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,9)  
CTT        WQV(L,K,9) = WQRR(L)*WQKK(L)  
            WQVO(L,K,9) = WQVO(L,K,9)+WQV(L,K,9)  
C  
C10 PO4T: WQG10=WQKDOP(L),WQA10=WQKK(L)  
C  
            WQA10C=(WQFPIC*WQBMC(L)+WQFPIP*WQPRC(L)-WQPC(L))*WQVO(L,K,1)  
            WQA10D=(WQFPID*WQBMD(L)+WQFPIP*WQPRD(L)-WQPD(L))*WQVO(L,K,2)  
            WQA10G=(WQFPIG*WQBMG(L)+WQFPIP*WQPRG(L)-WQPG(L))*WQVO(L,K,3)  
            WQKK(L) = (WQA10C+WQA10D+WQA10G) * WQAPC(L)  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQKK(L) =WQKK(L)+(WQFPIM*WQBMM(L)+WQFPIP*WQPRM(L)-WQPM(L))  
     &            *WQVO(L,K,IDNOTRVA) * WQAPC(L)*WQAPCM  
            ENDIF  
C J.S.  
            WQRR(L) = (WQWDSL(L,K,10)+WQWPSL(L,K,10)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQRR(L) = WQRR(L) + WQATML(L,KC,10) * VOLWQ(L)  
            ENDIF  
          ENDDO  
C  
          IF(K.EQ.1)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + WQBFPO4D(L)*DZWQ(L)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
            WQRR(L) = WQVO(L,K,10) + DTWQ*WQRR(L) + DTWQO2*( WQKK(L)  
     &          + WQKDOP(L)*WQVO(L,K,9) + WQH10(L)*WQVO(L,K,10) )  
          ENDDO  
C  
          IF(K.NE.KC)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + DTWQO2*WQT10(L)*WQVO(L,K+1,10)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
            WQKK(L) = 1.0 / (1.0 - DTWQO2*WQH10(L))  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
           WQV(L,K,10)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,10)  
CTT         WQV(L,K,10) = WQRR(L)*WQKK(L)  
            WQVO(L,K,10) = WQVO(L,K,10)+WQV(L,K,10)  
C  
C11 RPON: WQT11=WQT4=WQRPSET(L,2)  
C  
            WQI11 = - (WQKRPN(L)+WQRPSET(L,1))  
            WQKK(L) = 1.0 / (1.0 - DTWQO2*WQI11)  
            WQA11C=(WQFNRC*WQBMC(L)+WQFNRP*WQPRC(L))*WQANCC*WQVO(L,K,1)  
            WQA11D=(WQFNRD*WQBMD(L)+WQFNRP*WQPRD(L))*WQANCD*WQVO(L,K,2)  
            WQA11G=(WQFNRG*WQBMG(L)+WQFNRP*WQPRG(L))*WQANCG*WQVO(L,K,3)  
            WQA11 = WQA11C+WQA11D+WQA11G  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQA11 =WQA11 +     (WQFNRM*WQBMM(L)+WQFNRPM*WQPRM(L))  
     &            *WQANCM*WQVO(L,K,IDNOTRVA)  
            ENDIF  
C J.S.  
            WQR11 = (WQWDSL(L,K,11)+WQWPSL(L,K,11)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR11 = WQR11 + WQATML(L,KC,11) * VOLWQ(L)  
            ENDIF  
            WQRR(L) = WQVO(L,K,11) + DTWQ*WQR11 + DTWQO2*( WQA11  
     &          + WQI11*WQVO(L,K,11) )  
          ENDDO  
C  
          IF(K.NE.KC)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + DTWQO2*WQRPSET(L,2)*WQVO(L,K+1,11)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
           WQV(L,K,11)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,11)  
CTT         WQV(L,K,11) = WQRR(L)*WQKK(L)  
            WQVO(L,K,11) = WQVO(L,K,11)+WQV(L,K,11)  
C  
C12 LPON: WQT12=WQT5=WQLPSET(L,2)  
C  
            WQJ12 = - (WQKLPN(L)+WQLPSET(L,1))  
            WQKK(L) = 1.0 / (1.0 - DTWQO2*WQJ12)  
            WQA12C=(WQFNLC*WQBMC(L)+WQFNLP*WQPRC(L))*WQANCC*WQVO(L,K,1)  
            WQA12D=(WQFNLD*WQBMD(L)+WQFNLP*WQPRD(L))*WQANCD*WQVO(L,K,2)  
            WQA12G=(WQFNLG*WQBMG(L)+WQFNLP*WQPRG(L))*WQANCG*WQVO(L,K,3)  
            WQA12 = WQA12C+WQA12D+WQA12G  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQA12 =WQA12 +(WQFNLM*WQBMM(L)+WQFNLPM*WQPRM(L))  
     &            *WQANCM*WQVO(L,K,IDNOTRVA)  
            ENDIF  
C J.S.  
            WQR12 = (WQWDSL(L,K,12)+WQWPSL(L,K,12)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR12 = WQR12 + WQATML(L,KC,12) * VOLWQ(L)  
            ENDIF  
            WQRR(L) = WQVO(L,K,12) + DTWQ*WQR12 + DTWQO2*( WQA12  
     &          + WQJ12*WQVO(L,K,12) )  
          ENDDO  
C  
          IF(K.NE.KC)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + DTWQO2*WQLPSET(L,2)*WQVO(L,K+1,12)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
           WQV(L,K,12)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,12)  
CTT        WQV(L,K,12) = WQRR(L)*WQKK(L)  
            WQVO(L,K,12) = WQVO(L,K,12)+WQV(L,K,12)  
C  
C13 DON: WQI13=WQKRPN(L),WQJ13=WQKLPN(L),WQK13=-WQKDON(L)  
C  
            WQKK(L) = 1.0 / (1.0 + DTWQO2*WQKDON(L))  
            WQA13C=(WQFNDC*WQBMC(L)+WQFNDP*WQPRC(L))*WQANCC*WQVO(L,K,1)  
            WQA13D=(WQFNDD*WQBMD(L)+WQFNDP*WQPRD(L))*WQANCD*WQVO(L,K,2)  
            WQA13G=(WQFNDG*WQBMG(L)+WQFNDP*WQPRG(L))*WQANCG*WQVO(L,K,3)  
            WQA13 = WQA13C+WQA13D+WQA13G  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQA13 =WQA13 + (WQFNDM*WQBMM(L)+WQFNDPM*WQPRM(L))  
     &            *WQANCM*WQVO(L,K,IDNOTRVA)  
            ENDIF  
C J.S.  
            WQR13 = (WQWDSL(L,K,13) + WQWPSL(L,K,13)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQR13 = WQR13 + WQATML(L,KC,13) * VOLWQ(L)  
            ENDIF  
            WQRR(L) = WQVO(L,K,13) + DTWQ*WQR13 + DTWQO2*( WQA13  
     &          + WQKRPN(L)*WQVO(L,K,11) + WQKLPN(L)*WQVO(L,K,12)  
     &          - WQKDON(L)*WQVO(L,K,13) )  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
           WQV(L,K,13)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,13)  
CTT        WQV(L,K,13) = WQRR(L)*WQKK(L)  
            WQVO(L,K,13) = WQVO(L,K,13)+WQV(L,K,13)  
C  
C14 NH4: WQL14=-WQNIT(L),WQK14=WQKDON(L),WQR14=WQRR(L)  
C  
  
            WQRR(L) = (WQWDSL(L,K,14)+WQWPSL(L,K,14)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQRR(L) = WQRR(L) + WQATML(L,KC,14) * VOLWQ(L)  
            ENDIF  
          ENDDO  
C  
          IF(K.EQ.1)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + WQBFNH4(L)*DZWQ(L)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
            WQKK(L) = 1.0 / (1.0 + DTWQO2*WQNIT(L))  
            WQA14C=WQFNIC*WQBMC(L)+WQFNIP*WQPRC(L)-WQPNC(L)*WQPC(L)  
            WQA14D=WQFNID*WQBMD(L)+WQFNIP*WQPRD(L)-WQPND(L)*WQPD(L)  
            WQA14G=WQFNIG*WQBMG(L)+WQFNIP*WQPRG(L)-WQPNG(L)*WQPG(L)  
            WQA14 = WQA14C*WQANCC*WQVO(L,K,1)  
     &          + WQA14D*WQANCD*WQVO(L,K,2) + WQA14G*WQANCG*WQVO(L,K,3)  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQA14 = WQA14 + (WQFNIM*WQBMM(L)+WQFNIPM*WQPRM(L)  
     &            - WQPNM(L)*WQPM(L))*WQANCM*WQVO(L,K,IDNOTRVA)  
            ENDIF  
C J.S.  
            WQRR(L) = WQVO(L,K,14) + DTWQ*WQRR(L) + DTWQO2*( WQA14  
     &          + WQKDON(L)*WQVO(L,K,13) - WQNIT(L)*WQVO(L,K,14) )  
  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
           WQV(L,K,14)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,14)  
CTT        WQV(L,K,14) = WQRR(L)*WQKK(L)  
            WQVO(L,K,14) = WQVO(L,K,14)+WQV(L,K,14)  
C  
C15 NO3: WQKK(L)=1,WQR15=WQRR(L),WQD15=-WQANDC*WQDENIT(L),WQL15=WQNIT(L)  
C  
            WQRR(L) = (WQWDSL(L,K,15)+WQWPSL(L,K,15)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQRR(L) = WQRR(L) + WQATML(L,KC,15) * VOLWQ(L)  
            ENDIF  
          ENDDO  
C  
          IF(K.EQ.1)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + WQBFNO3(L)*DZWQ(L)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
            WQA15C = (WQPNC(L)-1.0)*WQPC(L) * WQANCC * WQVO(L,K,1)  
            WQA15D = (WQPND(L)-1.0)*WQPD(L) * WQANCD * WQVO(L,K,2)  
            WQA15G = (WQPNG(L)-1.0)*WQPG(L) * WQANCG * WQVO(L,K,3)  
            WQA15 = WQA15C+WQA15D+WQA15G  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQA15 =WQA15 + (WQPNM(L)-1.0)*WQPM(L)*WQANCM  
     &            *WQVO(L,K,IDNOTRVA)  
            ENDIF  
C J.S.  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
            WQV(L,K,15)=SCB(L)*( WQVO(L,K,15) + DTWQ*WQRR(L)  
     &          + DTWQO2*( WQA15  
     &          -WQANDC*WQDENIT(L)*WQVO(L,K,6)+WQNIT(L)*WQVO(L,K,14)))  
     &          +(1.-SCB(L))*WQVO(L,K,15)  
CTT         WQV(L,K,15) = WQVO(L,K,15) + DTWQ*WQRR(L) + DTWQO2*( WQA15  
CTT     *     - WQANDC*WQDENIT(L)*WQVO(L,K,6) + WQNIT(L)*WQVO(L,K,14) )  
            WQVO(L,K,15) = WQVO(L,K,15)+WQV(L,K,15)  
          ENDDO  
C  
C16 SU: WQT16=WQBDSET(L,2)  
C  
          IF(IWQSI.EQ.1)THEN  
C  
            DO L=LF,LL  
              WQM16 = - (WQKSUA(IWQT(L)) + WQBDSET(L,1))  
              WQKK(L) = 1.0 / (1.0 - DTWQO2*WQM16)  
              WQA16D = (WQFSPD*WQBMD(L) + WQFSPP*WQPRD(L)) * WQASCD  
     &            * WQVO(L,K,2)  
              WQR16 = (WQWDSL(L,K,16)+WQWPSL(L,K,16)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
              IF(K.EQ.KC)THEN  
                WQR16 = WQR16 + WQATML(L,KC,16) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,16) + DTWQ*WQR16 + DTWQO2*( WQA16D  
     &            + WQM16*WQVO(L,K,16) )  
            ENDDO  
C  
            IF(K.NE.KC)THEN  
              DO L=LF,LL  
                WQRR(L) = WQRR(L) + DTWQO2*WQBDSET(L,2)*WQVO(L,K+1,16)  
              ENDDO  
            ENDIF  
C  
            DO L=LF,LL  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
              WQV(L,K,16)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(
     &            L,K,16)  
CTT           WQV(L,K,16) = WQRR(L)*WQKK(L)  
              WQVO(L,K,16) = WQVO(L,K,16)+WQV(L,K,16)  
C  
C17 SA: WQM17=WQKSUA(IWQT(L)),WQA17D=WQKK(L)  
C  
              WQKK(L) = (WQFSID*WQBMD(L) + WQFSIP*WQPRD(L) - WQPD(L))  
     &            * WQASCD * WQVO(L,K,2)  
              WQRR(L) = (WQWDSL(L,K,17)+WQWPSL(L,K,17)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
              IF(K.EQ.KC)THEN  
                WQRR(L) = WQRR(L) + WQATML(L,KC,17) * VOLWQ(L)  
              ENDIF  
            ENDDO  
C  
            IF(K.EQ.1)THEN  
              DO L=LF,LL  
                WQRR(L) = WQRR(L) + WQBFSAD(L)*DZWQ(L)  
              ENDDO  
            ENDIF  
C  
            DO L=LF,LL  
              WQRR(L) = WQVO(L,K,17) + DTWQ*WQRR(L) + DTWQO2*( WQKK(L)  
     &            +WQKSUA(IWQT(L))*WQVO(L,K,16)+WQN17(L)*WQVO(L,K,17))  
            ENDDO  
C  
            IF(K.NE.KC)THEN  
              DO L=LF,LL  
                WQRR(L) = WQRR(L) + DTWQO2*WQT17(L)*WQVO(L,K+1,17)  
              ENDDO  
            ENDIF  
C  
            DO L=LF,LL  
              WQKK(L) = 1.0 / (1.0 - DTWQO2*WQN17(L))  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
              WQV(L,K,17)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(
     &            L,K,17)  
CTT           WQV(L,K,17) = WQRR(L)*WQKK(L)  
              WQVO(L,K,17) = WQVO(L,K,17)+WQV(L,K,17)  
            ENDDO  
C  
          ENDIF  
C  
C18 COD: WQO18(L)=DTWQO2*WQO18  
C  
          DO L=LF,LL  
            WQKK(L) = 1.0 / (1.0 - WQO18(L))  
            WQRR(L) = (WQWDSL(L,K,18)+WQWPSL(L,K,18)) * VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQRR(L) = WQRR(L) + WQATML(L,KC,18) * VOLWQ(L)  
            ENDIF  
          ENDDO  
C  
          IF(K.EQ.1)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + WQBFCOD(L)*DZWQ(L)  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
            WQRR(L)=WQVO(L,K,18)+DTWQ*WQRR(L)+WQO18(L)*WQVO(L,K,18)  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
           WQV(L,K,18)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,18)  
CTT        WQV(L,K,18) = WQRR(L)*WQKK(L)  
            WQVO(L,K,18) = WQVO(L,K,18)+WQV(L,K,18)  
C  
C19 O2: WQD19=-WQAOCR*WQKHR(L),WQL19=-WQAONT*WQNIT(L),WQO19=WQO18  
C: WQKRDOS(L)=-WQP19(L)*WQDOS  
C  
            WQKK(L) = 1.0 / (1.0 - DTWQO2*WQP19(L))  
            WQRR(L) = (WQWDSL(L,K,19)+WQWPSL(L,K,19)) * VOLWQ(L)  
            XDOPSL(L,K) = XDOPSL(L,K) + WQRR(L)*DTWQ*DZC(K)*HP(L)  
            XDOALL(L,K) = XDOALL(L,K) + WQRR(L)*DTWQ*DZC(K)*HP(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
            IF(K.EQ.KC)THEN  
              WQRR(L) = WQRR(L) + WQATML(L,KC,19) * VOLWQ(L)  
            ENDIF  
          ENDDO  
C  
          IF(K.EQ.KC)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + WQKRDOS(L)  
            ENDDO  
          ENDIF  
C  
          IF(K.EQ.1)THEN  
            DO L=LF,LL  
              WQRR(L) = WQRR(L) + WQBFO2(L)*DZWQ(L)  
              XDOSOD(L,K) = XDOSOD(L,K) + WQBFO2(L)*DTWQ  
              XDOALL(L,K) = XDOALL(L,K) + WQBFO2(L)*DTWQ  
            ENDDO  
          ENDIF  
C  
          DO L=LF,LL  
            IZ=IWQZMAP(L,K)  
            O2WQ_ = MAX(WQVO(L,K,19), 0.0)  
            WQTTC = (1.3 - 0.3*WQPNC(L)) * WQPC(L)  
            WQTTD = (1.3 - 0.3*WQPND(L)) * WQPD(L)  
            WQTTG = (1.3 - 0.3*WQPNG(L)) * WQPG(L)  
            XDOPPB(L,K) = XDOPPB(L,K) + ( WQTTC*WQVO(L,K,1)  
     &          +WQTTD*WQVO(L,K,2)+WQTTG*WQVO(L,K,3))*WQAOCR*DTWQO2  
     &          *DZC(K)*HP(L)  
            XDOALL(L,K) = XDOALL(L,K) + ( WQTTC*WQVO(L,K,1)  
     &          +WQTTD*WQVO(L,K,2)+WQTTG*WQVO(L,K,3))*WQAOCR*DTWQO2  
     &          *DZC(K)*HP(L)  
  
            XMRM = CFCDCWQ*O2WQ_*WQBMC(L)/(WQKHRC+O2WQ_+ 1.E-18)  
            WQA19C = WQTTC - XMRM  
            XDORRB(L,K) = XDORRB(L,K) - XMRM*WQVO(L,K,1) * WQAOCR*DTWQO2  
     &          *DZC(K)*HP(L)  
            XDOALL(L,K) = XDOALL(L,K) - XMRM*WQVO(L,K,1) * WQAOCR*DTWQO2  
     &          *DZC(K)*HP(L)  
            XMRM = CFCDDWQ*O2WQ_*WQBMD(L)/(WQKHRD+O2WQ_+ 1.E-18)  
            WQA19D = WQTTD - XMRM  
            XDORRB(L,K) = XDORRB(L,K) - XMRM*WQVO(L,K,2) * WQAOCR*DTWQO2  
     &          *DZC(K)*HP(L)  
            XDOALL(L,K) = XDOALL(L,K) - XMRM*WQVO(L,K,2) * WQAOCR*DTWQO2  
     &          *DZC(K)*HP(L)  
            XMRM = CFCDGWQ*O2WQ_*WQBMG(L)/(WQKHRG+O2WQ_+ 1.E-18)  
            WQA19G = WQTTG - XMRM  
            XDORRB(L,K) = XDORRB(L,K) - XMRM*WQVO(L,K,3) * WQAOCR*DTWQO2  
     &          *DZC(K)*HP(L)  
            XDOALL(L,K) = XDOALL(L,K) - XMRM*WQVO(L,K,3) * WQAOCR*DTWQO2  
     &          *DZC(K)*HP(L)  
            WQA19 = ( WQA19C*WQVO(L,K,1) + WQA19D*WQVO(L,K,2)  
     &          + WQA19G*WQVO(L,K,3) ) * WQAOCR  
C  
C ADD MACALGAL SOURCE     J.S.  
C  
C MODIFIED BY MRM 05/23/99 TO ALLOW DIFFERENT AOCR CONSTANTS TO BE APPLI  
C   TO PHOTOSYNTHESIS AND RESPIRATION TERMS FOR MACROALGAE:  
C     WQAOCRPM = AOCR APPLIED TO MACROALGAE PHOTOSYNTHESIS TERM  
C     WQAOCRRM = AOCR APPLIED TO MACROALGAE RESPIRATION TERM  
C  
            IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
              WQTTM = (1.3 - 0.3*WQPNM(L)) * WQPM(L)  
              XMRM=(1.0-WQFCDM)*O2WQ_*WQBMM(L)/(WQKHRM(IZ)+O2WQ_+1.E-18)  
C MRM ++++ START NEW CODE  
C           WQA19A =  WQTTM - XMRM  
C           WQA19 = WQA19 + WQA19A * WQVO(L,K,IDNOTRVA) * WQAOCR  
              WQA19A = WQTTM * WQVO(L,K,IDNOTRVA) * WQAOCRPM -  
     &            XMRM *  WQVO(L,K,IDNOTRVA) * WQAOCRRM  
              WQA19 = WQA19 + WQA19A  
C MRM ++++ END NEW CODE  
              XDOPPM(L,K) = XDOPPM(L,K) +  
     &            WQTTM*WQVO(L,K,IDNOTRVA)*WQAOCRPM*DTWQO2*DZC(K)*HP(L)  
              XDOALL(L,K) = XDOALL(L,K) +  
     &            WQTTM*WQVO(L,K,IDNOTRVA)*WQAOCRPM*DTWQO2*DZC(K)*HP(L)  
              XDORRM(L,K) = XDORRM(L,K) -  
     &            XMRM*WQVO(L,K,IDNOTRVA)*WQAOCRRM*DTWQO2*DZC(K)*HP(L)  
              XDOALL(L,K) = XDOALL(L,K) -  
     &            XMRM*WQVO(L,K,IDNOTRVA)*WQAOCRRM*DTWQO2*DZC(K)*HP(L)  
            ENDIF  
C J.S.  
            WQRR(L) = WQVO(L,K,19) + DTWQ*WQRR(L) + DTWQO2*( WQA19  
     &         -WQAOCR*WQKHR(L)*WQVO(L,K,6)-WQAONT*WQNIT(L)*WQVO(L,K,14)  
     &          + WQP19(L)*WQVO(L,K,19) ) + WQO18(L)*WQVO(L,K,18)  
C MRM     *     + WQO18(L)*WQVO(L,K,18) + WQP19(L)*WQVO(L,K,19) )  
C  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
           WQV(L,K,19)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,19)  
CTT         WQV(L,K,19) = WQRR(L)*WQKK(L)  
C  
C MRM DO NOT ALLOW D.O. TO GO NEGATIVE:  
C  
            WQV(L,K,19) = MAX (WQV(L,K,19), 0.0)  
            WQVO(L,K,19) = WQVO(L,K,19)+WQV(L,K,19)  
C  
C COMPUTE AND SAVE D.O. DEFICIT:  
C  
            XMRM = WQDOS(L) - WQV(L,K,19)  
            XDODEF(L,K) = XDODEF(L,K) + XMRM*DTWQ*DZC(K)*HP(L)  
            IF(K.EQ.KC)THEN  
              XDOKAR(L,K) = XDOKAR(L,K) + WQKRDOS(L)*DTWQ*DZC(K)*HP(L) +  
     &            WQP19(L)*WQVO(L,K,19)*DTWQO2*DZC(K)*HP(L)  
              XDOALL(L,K) = XDOALL(L,K) + WQKRDOS(L)*DTWQ*DZC(K)*HP(L) +  
     &            WQP19(L)*WQVO(L,K,19)*DTWQO2*DZC(K)*HP(L)  
            ENDIF  
            XDODOC(L,K)=XDODOC(L,K) - WQAOCR*WQKHR(L)*WQVO(L,K,6)*DTWQO2  
     &          *DZC(K)*HP(L)  
            XDOALL(L,K)=XDOALL(L,K) - WQAOCR*WQKHR(L)*WQVO(L,K,6)*DTWQO2  
     &          *DZC(K)*HP(L)  
            XDONIT(L,K)=XDONIT(L,K)-WQAONT*WQNIT(L)*WQVO(L,K,14)*DTWQO2  
     &          *DZC(K)*HP(L)  
            XDOALL(L,K)=XDOALL(L,K)-WQAONT*WQNIT(L)*WQVO(L,K,14)*DTWQO2  
     &          *DZC(K)*HP(L)  
  
            XDOCOD(L,K)=XDOCOD(L,K) - WQO18(L)*WQVO(L,K,18)  
     &          *DZC(K)*HP(L)  
            XDOALL(L,K)=XDOALL(L,K) - WQO18(L)*WQVO(L,K,18)  
     &          *DZC(K)*HP(L)  
C MRM         XDOCOD(L,K)=XDOCOD(L,K) - WQO18(L)*WQVO(L,K,18)*DTWQO2  
C MRM     *     *DZC(K)*HP(L)  
C MRM         XDOALL(L,K)=XDOALL(L,K) - WQO18(L)*WQVO(L,K,18)*DTWQO2  
C MRM     *     *DZC(K)*HP(L)  
            XDODZ(L,K) = XDODZ(L,K) + DZC(K)*HP(L)  
          ENDDO  
C  
C TAM: WQQ20=-WQWSSET(L,1),WQT20=WQWSSET(L,2)  
C  
          IF(IWQSRP.EQ.1)THEN  
C  
            DO L=LF,LL  
              WQT20 = - DTWQO2*WQWSSET(L,1)  
              WQKK(L) = 1.0 / (1.0 - WQT20)  
              WQRR(L)=WQVO(L,K,20)+DTWQ*WQR20(L)+WQT20*WQVO(L,K,20)  
            ENDDO  
C  
            IF(K.NE.KC)THEN  
              DO L=LF,LL  
                WQRR(L) = WQRR(L) + DTWQO2*WQWSSET(L,2)*WQVO(L,K+1,20)  
              ENDDO  
            ENDIF  
C  
            DO L=LF,LL  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
              WQV(L,K,20)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(
     &            L,K,20)  
CTT           WQV(L,K,20) = WQRR(L)*WQKK(L)  
              WQVO(L,K,20) = WQVO(L,K,20)+WQV(L,K,20)  
            ENDDO  
C  
          ENDIF  
C  
C FCB: WQTDFCB(IWQT(L))=WQKFCB*WQTFCB**(T-20),S21=-KFCB*TFCB^(T-20)  
C WQTD1FCB=1+DTWQO2*WQS21,WQTD2FCB=1/(1-DTWQO2*S21)  
C  
          IF(IWQFCB.EQ.1)THEN  
C  
            DO L=LF,LL  
              WQKK(L) = WQTD2FCB(IWQT(L))  
              WQR21= (WQWDSL(L,K,NWQV)+WQWPSL(L,K,NWQV))*VOLWQ(L)  
C MRM  ADD WET ATMOSPHERIC DEPOSITION:  
              IF(K.EQ.KC)THEN  
                WQR21 = WQR21 + WQATML(L,KC,21) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,NWQV)*WQTD1FCB(IWQT(L)) + DTWQ*WQR21  
CTT UPDATE MODIFIED NEXT LINE ADDED LINE BELOW COMMENTED OUT  
              WQV(L,K,21)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(
     &            L,K,21)  
CTT           WQV(L,K,21) = WQRR(L)*WQKK(L)  
              WQVO(L,K,21) = WQVO(L,K,21)+WQV(L,K,21)  
            ENDDO  
C  
          ENDIF  
C  
C END OF DO K=KC,1,-1  LOOP  
C END OF DO ND=1,NDMWQ LOOP  
C  
        ENDDO  
      ENDDO  
C  
C INCREMENT COUNTER FOR LIMITATION AND XDOXXX DO COMPONENT ARRAYS:  
C  
      IF(ISDYNSTP.EQ.0)THEN  
        TIMTMP=DT*FLOAT(N)+TCON*TBEGIN  
        TIMTMP=TIMTMP/TCTMSR  
      ELSE  
        TIMTMP=TIMESEC/TCTMSR  
      ENDIF  
      TIMESUM3 = TIMESUM3 + TIMTMP  
      NLIM = NLIM + 1  
C  
C COMPUTE WQCHL,WQTAMP,WQPO4D,WQSAD AT A NEW TIME STEP: WQCHLX=1/WQCHLX  
C  
      DO K=1,KC  
        DO L=2,LA  
          WQCHL(L,K) = WQV(L,K,1)*WQCHLC + WQV(L,K,2)*WQCHLD  
     &        + WQV(L,K,3)*WQCHLG  
        ENDDO  
      ENDDO  
      IF(IWQSRP.EQ.1)THEN  
        DO K=1,KC  
          DO L=2,LA  
            O2WQ_ = MAX(WQV(L,K,19), 0.0)  
            WQTAMD = MIN( WQTAMDMX*EXP(-WQKDOTAM*O2WQ_), WQV(L,K,20) )  
            WQTAMP(L,K) = WQV(L,K,20) - WQTAMD  
            WQPO4D(L,K) = WQV(L,K,10) / (1.0 + WQKPO4P*WQTAMP(L,K))  
            WQSAD(L,K)  = WQV(L,K,17) / (1.0 + WQKSAP*WQTAMP(L,K))  
          ENDDO  
        ENDDO  
      ELSE IF(IWQSRP.EQ.2)THEN  
        DO K=1,KC  
          DO L=2,LA  
            WQPO4D(L,K) = WQV(L,K,10) / (1.0 + WQKPO4P*SEDT(L,K))  
            WQSAD(L,K)  = WQV(L,K,17) / (1.0 + WQKSAP*SEDT(L,K))  
          ENDDO  
        ENDDO  
      ELSE  
        DO K=1,KC  
          DO L=2,LA  
            WQPO4D(L,K) = WQV(L,K,10)  
            WQSAD(L,K)  = WQV(L,K,17)  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C COUPLING TO SEDIMENT MODEL  
C: EVALUATE DEP. FLUX USING NEW VALUES CAUSE IMPLICIT SCHEME IS USED IN  
C  SPM  
C  
      IF(IWQBEN.EQ.1)THEN  
C  
        DO ND=1,NDMWQ  
          LF=2+(ND-1)*LDMWQ  
          LL=LF+LDM-1  
          DO L=LF,LL  
            IMWQZ = IWQZMAP(L,1)  
            WQDFBC(L) = SCB(L)*WQWSC(IMWQZ)*WQV(L,1,1)  
            WQDFBD(L) = SCB(L)*WQWSD(IMWQZ)*WQV(L,1,2)  
            WQDFBG(L) = SCB(L)*WQWSG(IMWQZ)*WQV(L,1,3)  
     &          +WQWSM*DZWQ(L)*WQV(L,1,IDNOTRVA)  
            WQDFRC(L) = SCB(L)*WQWSRP(IMWQZ)*WQV(L,1,4)  
            WQDFLC(L) = SCB(L)*WQWSLP(IMWQZ)*WQV(L,1,5)  
            WQDFRP(L) = SCB(L)*WQWSRP(IMWQZ)*WQV(L,1,7)  
            WQDFLP(L) = SCB(L)*WQWSLP(IMWQZ)*WQV(L,1,8)  
            WQDFRN(L) = SCB(L)*WQWSRP(IMWQZ)*WQV(L,1,11)  
            WQDFLN(L) = SCB(L)*WQWSLP(IMWQZ)*WQV(L,1,12)  
            IF(IWQSI.EQ.1) WQDFSI(L) = SCB(L)*WQWSD(IMWQZ)*WQV(L,1,16)  
          ENDDO  
        ENDDO  
C  
        IF(IWQSRP.EQ.1)THEN  
          DO ND=1,NDMWQ  
            LF=2+(ND-1)*LDMWQ  
            LL=LF+LDM-1  
            DO L=LF,LL  
              IMWQZ = IWQZMAP(L,1)  
              WQDFLP(L) = SCB(L)*( WQDFLP(L)  
     &            + WQWSS(IMWQZ)*( WQV(L,1,10)-WQPO4D(L,1) ) )  
              IF(IWQSI.EQ.1) WQDFSI(L) = SCB(L)*( WQDFSI(L)  
     &            + WQWSS(IMWQZ)*( WQV(L,1,17)-WQSAD(L,1) ) )  
            ENDDO  
          ENDDO  
        ELSE IF(IWQSRP.EQ.2)THEN  
          DO ND=1,NDMWQ  
            LF=2+(ND-1)*LDMWQ  
            LL=LF+LDM-1  
            DO L=LF,LL  
              WQDFLP(L) = SCB(L)*( WQDFLP(L)+WSEDO(NS)*( WQV(L,1,10)  
     &            -WQPO4D(L,1) ) )  
              IF(IWQSI.EQ.1) WQDFSI(L) = SCB(L)*( WQDFSI(L)  
     &            + WSEDO(NS)*( WQV(L,1,17)-WQSAD(L,1) ) )  
            ENDDO  
          ENDDO  
        ENDIF  
C  
      ENDIF  
C  
C DIURNAL DO ANALYSIS  
C  
      IF(NDDOAVG.GE.1)THEN  
        OPEN(1,FILE='DIURNDO.OUT',POSITION='APPEND')  
        NDDOCNT=NDDOCNT+1  
        NSTPTMP=NDDOAVG*NTSPTC/2  
        RMULTMP=1./FLOAT(NSTPTMP)  
C  
        DO K=1,KC  
          DO L=2,LA  
            DDOMAX(L,K)=MAX(DDOMAX(L,K),WQV(L,K,19))  
            DDOMIN(L,K)=MIN(DDOMIN(L,K),WQV(L,K,19))  
          ENDDO  
        ENDDO  
C  
        IF(NDDOCNT.EQ.NSTPTMP)THEN  
          NDDOCNT=0  
          IF(ISDYNSTP.EQ.0)THEN  
            TIME=DT*FLOAT(N)+TCON*TBEGIN  
            TIME=TIME/TCON  
          ELSE  
            TIME=TIMESEC/TCON  
          ENDIF  
          WRITE(1,1111)N,TIME  
          DO L=2,LA  
            WRITE(1,1112)IL(L),JL(L),(DDOMIN(L,K),K=1,KC),  
     &          (DDOMAX(L,K),K=1,KC)  
          ENDDO  
          DO K=1,KC  
            DO L=2,LA  
              DDOMAX(L,K)=-1.E6  
              DDOMIN(L,K)=1.E6  
            ENDDO  
          ENDDO  
        ENDIF  
C  
        CLOSE(1)  
      ENDIF  
C  
C LIGHT EXTINCTION ANALYSIS  
C  
      IF(NDLTAVG.GE.1)THEN  
        OPEN(1,FILE='LIGHT.OUT',POSITION='APPEND')  
        NDLTCNT=NDLTCNT+1  
        NSTPTMP=NDLTAVG*NTSPTC/2  
        RMULTMP=1./FLOAT(NSTPTMP)  
C  
        DO K=1,KC  
          DO L=2,LA  
            RLIGHT1=WQKEB(IMWQZT(L))+WQKETSS*SEDT(L,K)  
C  
C MRM 05/12/1999 USE RILEY (1956) EQUATION TO COMPUTE LIGHT EXTINCTION  
C     AS A FUNCTION OF CHL CONC. IF WQKECHL IS LESS THAN ZERO:  
C  
C          RLIGHT2=WQKECHL*WQCHL(L,K)  
            XMRM = WQKECHL*WQCHL(L,K)  
            IF(WQKECHL .LT. 0.0)THEN  
              XMRM = 0.054*WQCHL(L,K)**0.6667 + 0.0088*WQCHL(L,K)  
            ENDIF  
            RLIGHT2 = XMRM  
            RLIGHTT(L,K)=RLIGHTT(L,K)+RLIGHT1  
            RLIGHTC(L,K)=RLIGHTC(L,K)+RLIGHT1+RLIGHT2  
          ENDDO  
        ENDDO  
C  
        IF(NDLTCNT.EQ.NSTPTMP)THEN  
          NDLTCNT=0  
          IF(ISDYNSTP.EQ.0)THEN  
            TIME=DT*FLOAT(N)+TCON*TBEGIN  
            TIME=TIME/TCON  
          ELSE  
            TIME=TIMESEC/TCON  
          ENDIF  
          DO K=1,KC  
            DO L=2,LA  
              RLIGHTT(L,K)=RMULTMP*RLIGHTT(L,K)  
              RLIGHTC(L,K)=RMULTMP*RLIGHTC(L,K)  
            ENDDO  
          ENDDO  
          WRITE(1,1111)N,TIME  
          DO L=2,LA  
            WRITE(1,1113)IL(L),JL(L),(RLIGHTT(L,K),K=1,KC),  
     &          (RLIGHTC(L,K),K=1,KC)  
          ENDDO  
          DO K=1,KC  
            DO L=2,LA  
              RLIGHTT(L,K)=0.  
              RLIGHTC(L,K)=0.  
            ENDDO  
          ENDDO  
        ENDIF  
C  
        CLOSE(1)  
      ENDIF  
C  
C  
 1111 FORMAT(I12,F10.4)  
 1112 FORMAT(2I5,12F7.2)  
 1113 FORMAT(2I5,12E12.4)  
C  
      RETURN  
      END  
