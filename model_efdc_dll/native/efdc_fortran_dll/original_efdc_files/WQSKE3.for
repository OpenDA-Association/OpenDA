      SUBROUTINE WQSKE3  
C  
C: AFTER COMPUTING NEW VALUES, STORE WQVO+WQV INTO WQVO(L,K,NWQV) EXCEPT  
C: NWQV=15,19,21.  
C  ORGINALLY CODED BY K.-Y. PARK  
C  OPTIMIZED AND MODIFIED BY J.M. HAMRICK  
C 
C  PMC - THIS IS THE SAME AS WQSKE2
C 
C LAST MODIFIED BY YSSONG ON 24 NOVEMBER 2011

      USE GLOBAL  
C
      CHARACTER*11 FLN ! character array to print growth limit and algal rate
      INTEGER   IZA ! Integer for benthic flux for anoxic env
      ! Arrays to facilitate x-species
      REAL WQGNX(NXSP),WQGPX(NXSP),WQF1NX(NXSP)
      REAL WQISX(NXSP),WQFDX(NXSP),WQF2IX(NXSP)
      REAL WQTTX(NXSP)
      REAL WQACX(NXSP),WQKKX(LCMWQ,NXSP)
      REAL WQA2X(NXSP),WQA3X(NXSP)
      CNS1=2.718  
      NS=1  
      DO L=2,LA  
        WQI0BOT(L)=WQI0  
      ENDDO  
      WQKESS=0.0
      WQKESS1=0.0
      ! Stokes
      CYANOMASS=0.0      
      ! Deal with benthic flux for anoxic env
      IF(IWQBEN .EQ. 0 .AND. IWQBENOX .EQ. 0)THEN
        DO L=2,LA  
          WQBFCOD(L)=WQBFOXCOD(1,1)
          WQBFNH4(L)=WQBFOXNH4(1,1)   
          WQBFNO3(L)=WQBFOXNO3(1,1)   
          WQBFO2(L)= WQBFOXO2(1,1)  
          WQBFPO4D(L)=WQBFOXPO4D(1,1)
          WQBFSAD(L)=WQBFOXSAD(1,1)
        ENDDO 
      ELSEIF(IWQBEN .EQ. 0 .AND. IWQBENOX .NE. 0)THEN 
        DO L=2,LA  
          IF(WQVO(L,1,19).GT.DOXCRT)THEN
            WQBFCOD(L)=WQBFOXCOD(1,1)
            WQBFNH4(L)=WQBFOXNH4(1,1)   
            WQBFNO3(L)=WQBFOXNO3(1,1)   
            WQBFO2(L)= WQBFOXO2(1,1)  
            WQBFPO4D(L)=WQBFOXPO4D(1,1)
            WQBFSAD(L)=WQBFOXSAD(1,1)
          ELSE
            WQBFCOD(L)=WQBFOXCOD(1,2)
            WQBFNH4(L)=WQBFOXNH4(1,2)   
            WQBFNO3(L)=WQBFOXNO3(1,2)   
            WQBFO2(L)= WQBFOXO2(1,2)  
            WQBFPO4D(L)=WQBFOXPO4D(1,2)
            WQBFSAD(L)=WQBFOXSAD(1,2)
          ENDIF 
        ENDDO  
      ENDIF    
      
      IF(IWQBEN .EQ. 2 .AND. IWQBENOX .EQ.0)THEN
        DO L=2,LA  
          WQBFCOD(L)=WQBFOXCOD(L,1)
          WQBFNH4(L)=WQBFOXNH4(L,1)   
          WQBFNO3(L)=WQBFOXNO3(L,1)   
          WQBFO2(L)= WQBFOXO2(L,1)  
          WQBFPO4D(L)=WQBFOXPO4D(L,1)
          WQBFSAD(L)=WQBFOXSAD(L,1)   
        ENDDO            
      ELSEIF(IWQBEN .EQ. 2 .AND. IWQBENOX .NE. 0)THEN          
        DO L=2,LA  
          IF(WQVO(L,1,19).GT.DOXCRT)THEN
            WQBFCOD(L)=WQBFOXCOD(L,1)
            WQBFNH4(L)=WQBFOXNH4(L,1)   
            WQBFNO3(L)=WQBFOXNO3(L,1)   
            WQBFO2(L)= WQBFOXO2(L,1)  
            WQBFPO4D(L)=WQBFOXPO4D(L,1)
            WQBFSAD(L)=WQBFOXSAD(L,1)   
          ELSE
            WQBFCOD(L)=WQBFOXCOD(L,2)
            WQBFNH4(L)=WQBFOXNH4(L,2)   
            WQBFNO3(L)=WQBFOXNO3(L,2)   
            WQBFO2(L)= WQBFOXO2(L,2)  
            WQBFPO4D(L)=WQBFOXPO4D(L,2)
            WQBFSAD(L)=WQBFOXSAD(L,2)  
          ENDIF 
        ENDDO  
      ENDIF
      DO K=KC,1,-1  
C  
C DZWQ=1/H, VOLWQ=1/VOL  
C  
        DO L=2,LA  
          TWQ(L)=TEM(L,K)  
          SWQ(L)=MAX(SAL(L,K), 0.0)  
          DZWQ(L) = 1.0 / (DZC(K)*HP(L))  
          VOLWQ(L) = DZWQ(L) / DXYP(L)  
          IMWQZT(L)=IWQZMAP(L,K)  
        ENDDO  
        DO L=2,LA  
          WQBCSET(L,1) = WQWSC(IMWQZT(L))*DZWQ(L)  
          WQBDSET(L,1) = WQWSD(IMWQZT(L))*DZWQ(L)  
          WQBGSET(L,1) = WQWSG(IMWQZT(L))*DZWQ(L)  
          WQRPSET(L,1) = WQWSRP(IMWQZT(L))*DZWQ(L)  
          WQLPSET(L,1) = WQWSLP(IMWQZT(L))*DZWQ(L)  
          ! Also treat x-species
          DO nsp=1,NXSP
            WQBXSET(L,1,nsp) = WQWSX(IMWQZT(L),nsp)*DZWQ(L)  
          ENDDO
        ENDDO  
        IF(IWQSRP.EQ.1)THEN  
          DO L=2,LA  
            WQWSSET(L,1) = WQWSS(IMWQZT(L))*DZWQ(L)  
          ENDDO  
        ENDIF  
        IF(K.NE.KC)THEN  
          DO L=2,LA  
            IMWQZT1(L)=IWQZMAP(L,K+1)  
          ENDDO  
          DO L=2,LA  
            WQBCSET(L,2) = WQWSC(IMWQZT1(L))*DZWQ(L)  
            WQBDSET(L,2) = WQWSD(IMWQZT1(L))*DZWQ(L)  
            WQBGSET(L,2) = WQWSG(IMWQZT1(L))*DZWQ(L)  
            WQRPSET(L,2) = WQWSRP(IMWQZT1(L))*DZWQ(L)  
            WQLPSET(L,2) = WQWSLP(IMWQZT1(L))*DZWQ(L)  
            ! Also treat x-species
            DO nsp=1,NXSP
              WQBXSET(L,2,nsp) = WQWSX(IMWQZT1(L),nsp)*DZWQ(L)  
            ENDDO
          ENDDO  
          IF(IWQSRP.EQ.1)THEN  
            DO L=2,LA  
              WQWSSET(L,2) = WQWSS(IMWQZT1(L))*DZWQ(L)  
            ENDDO  
          ENDIF  
        ENDIF  
C  
C FIND AN INDEX FOR LOOK-UP TABLE FOR TEMPERATURE DEPENDENCY  
C  
        DO L=2,LA  
C           IWQT(L) = 2.0*TWQ(L) +11  
C - charles  IWQT(L) = 10.0*TWQ(L) +151  
C          IWQT(L) = NINT( 4.*TWQ(L)+121.)  
          IWQT(L)=NINT((TWQ(L)-WQTDMIN)/WQTDINC)  ! *** DSLLC SINGLE LINE
          IF(IWQT(L).LT.1 .OR. IWQT(L).GT.NWQTD)THEN  
            IF(ISDYNSTP.EQ.0)THEN  
              TIMTMP=DT*FLOAT(N)+TCON*TBEGIN  
              TIMTMP=TIMTMP/86400.  
            ELSE  
              TIMTMP=TIMESEC/86400.  
            ENDIF  
            WRITE(8,911) TIMTMP, L, IL(L), JL(L), K, TWQ(L)  
            WRITE(6,600)IL(L),JL(L),K,TWQ(L)  
            IWQT(L)=MAX(IWQT(L),1)  
            IWQT(L)=MIN(IWQT(L),NWQTD)  
C            STOP 'ERROR!! INVALID WATER TEMPERATURE'  
          ENDIF  
        ENDDO  
  600 FORMAT(' I,J,K,TEM = ',3I5,E13.4)  
  911 FORMAT(/,'ERROR: TIME, L, I, J, K, TWQ(L) = ', F10.5, 4I4, F10.4)  
C  
C NOTE: MRM 04/29/99  ADDED ARRAYS TO KEEP TRACK OF  
C       NITROGEN, PHOSPHORUS, LIGHT, AND TEMPERATURE LIMITS  
C       FOR ALGAE GROWTH FOR CYANOBACTERIA, DIATOMS, GREENS,  
C       AND MACROALGAE.  THESE ARE THE ARRAYS:  
C        XLIMNX(L,K) = NITROGEN    LIMITATION FOR ALGAE GROUP X  
C        XLIMPX(L,K) = PHOSPHORUS  LIMITATION FOR ALGAE GROUP X  
C        XLIMIX(L,K) = LIGHT       LIMITATION FOR ALGAE GROUP X  
C        XLIMTX(L,K) = TEMPERATURE LIMITATION FOR ALGAE GROUP X  
C BEGIN HORIZONTAL LOOP FOR ALGAE PARMETERS  
C  
        DO L=2,LA  
          IZ=IWQZMAP(L,K)  
          RNH4WQ(L) = MAX (WQVO(L,K,14), 0.0)  
          RNO3WQ(L) = MAX (WQVO(L,K,15), 0.0)  
          PO4DWQ(L) = MAX (WQPO4D(L,K), 0.0)  
          RNH4NO3(L) = RNH4WQ(L) + RNO3WQ(L)  
          WQGNC = RNH4NO3(L) / (WQKHNC+RNH4NO3(L)+ 1.E-18)  
          WQGND = RNH4NO3(L) / (WQKHND+RNH4NO3(L)+ 1.E-18)  
          WQGNG = RNH4NO3(L) / (WQKHNG+RNH4NO3(L)+ 1.E-18)  
          WQGPC = PO4DWQ(L) / (WQKHPC+PO4DWQ(L)+ 1.E-18)  
          WQGPD = PO4DWQ(L) / (WQKHPD+PO4DWQ(L)+ 1.E-18)  
          WQGPG = PO4DWQ(L) / (WQKHPG+PO4DWQ(L)+ 1.E-18)  
          XLIMNC(L,K) = XLIMNC(L,K) + WQGNC  
          XLIMND(L,K) = XLIMND(L,K) + WQGND  
          XLIMNG(L,K) = XLIMNG(L,K) + WQGNG  
          XLIMPC(L,K) = XLIMPC(L,K) + WQGPC  
          XLIMPD(L,K) = XLIMPD(L,K) + WQGPD  
          XLIMPG(L,K) = XLIMPG(L,K) + WQGPG  
          IF(IDNOTRVA.GT.0 .AND. K.EQ.1)THEN  
            WQGNM = RNH4NO3(L) / (WQKHNM+RNH4NO3(L) + 1.E-18)  
            WQGPM = PO4DWQ(L) / (WQKHPM+PO4DWQ(L) + 1.E-18)  
            WQF1NM = MIN(WQGNM, WQGPM)  
            XLIMNM(L,K) = XLIMNM(L,K) + WQGNM  
            XLIMPM(L,K) = XLIMPM(L,K) + WQGPM  
          ENDIF  
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
            PO4DWQ(L) = MAX (WQPO4D(L,K), 0.0)  
          ENDIF  
          ! Also treat x-species
          do nsp=1,NXSP
            WQGNX(nsp)=RNH4NO3(L) / (WQKHNX(nsp)+RNH4NO3(L)+ 1.E-18) 
            WQGPX(nsp)=PO4DWQ(L) / (WQKHPX(nsp)+PO4DWQ(L)+ 1.E-18)
            XLIMNX(L,K,nsp) = XLIMNX(L,K,nsp) + WQGNX(nsp)
            XLIMPX(L,K,nsp) = XLIMPX(L,K,nsp) + WQGPX(nsp)
            if (IWQX(nsp).eq.1) then  ! cyano
              WQF1NX(nsp) = MIN(WQGNX(nsp), WQGPX(nsp))
            endif
            WQF1NX(nsp) = MIN(WQGNC, WQGPC)
            if (IWQSI.EQ.1 .and. IWQX(nsp).eq.2) then  ! diatom
              SADWQ = MAX (WQSAD(L,K), 0.0)  
              WQGSD = SADWQ / (WQKHSX(nsp)+SADWQ+ 1.E-18)
              WQF1NX(nsp) = MIN(WQGNX(nsp), WQGPX(nsp),WQGSD)
            else
              WQF1NX(nsp) = MIN(WQGNX(nsp), WQGPX(nsp))
            endif
          enddo
C  
C IN C&C, F2IC=F2IC/FCYAN, FACTOR TO ALLOW CYANOBACTERIA MAT FORMATION  
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
            IF(IWQSUN .EQ. 2)THEN  
              WQAVGIO = WQCIA*WQI1 + WQCIB*WQI2 + WQCIC*WQI3  
            ENDIF  
            WQISC = MAX( WQAVGIO*EXP(-WQKESS1*WQDOPC), WQISMIN )  
            WQISD = MAX( WQAVGIO*EXP(-WQKESS1*WQDOPD), WQISMIN )  
            WQISG = MAX( WQAVGIO*EXP(-WQKESS1*WQDOPG), WQISMIN )  
            WQTT1 = (CNS1 * WQFD * DZWQ(L)) / WQKESS  
C  
C        WQFDI0 = - WQI0 / (WQFD+ 1.E-18)  
C  
            WQFDI0 = - WQI0BOT(L) / (WQFD + 1.E-18)  
            WQFDC = WQFDI0 / (WQISC + 1.E-18)  
            WQFDD = WQFDI0 / (WQISD + 1.E-18)  
            WQFDG = WQFDI0 / (WQISG + 1.E-18)  
            WQHTT = WQHT(K) * HP(L)  
            WQTTB = EXP( -WQKESS * (WQHTT+1.0/DZWQ(L)) )  
            WQTTT = EXP( -WQKESS * WQHTT )  
            WQF2IC = WQTT1 * (EXP(WQFDC*WQTTB) - EXP(WQFDC*WQTTT))  
            WQF2ID = WQTT1 * (EXP(WQFDD*WQTTB) - EXP(WQFDD*WQTTT))  
            WQF2IG = WQTT1 * (EXP(WQFDG*WQTTB) - EXP(WQFDG*WQTTT))  
            !WQF2IC = WQF2IC * PSHADE(L)  
            !WQF2ID = WQF2ID * PSHADE(L)  
            !WQF2IG = WQF2IG * PSHADE(L)  
            XLIMIC(L,K) = XLIMIC(L,K) + WQF2IC  
            XLIMID(L,K) = XLIMID(L,K) + WQF2ID  
            XLIMIG(L,K) = XLIMIG(L,K) + WQF2IG  
            ! Also treat x-species
            do nsp=1,NXSP
              WQISX(nsp) = MAX( WQAVGIO*EXP(-WQKESS1*WQDOPX(nsp))
     &                        , WQISMIN )  
              WQFDX(nsp) = WQFDI0 / (WQISX(nsp) + 1.E-18)
              WQF2IX(nsp) = WQTT1 * (EXP(WQFDX(nsp)*WQTTB)
     &                             - EXP(WQFDX(nsp)*WQTTT))  
              XLIMIX(L,K,nsp) = XLIMIX(L,K,nsp) + WQF2IX(nsp)
            enddo
          ELSE
            WQF2IC=0.0
            WQF2ID=0.0
            WQF2IG=0.0
            ! Also treat x-species
            do nsp=1,NXSP
              WQF2IX(nsp) = 0.
            enddo
          ENDIF
          ! Treate stokes for x-species
          do nsp=1,NXSP
            IF(ISSTOKEX(nsp).GE.1)THEN  
              CALL WQSTOKES01(WQKESS1,L,K,nsp)
            ELSE  
              WQALSETX(L,KC,nsp) = WQBXSET(L,1,nsp)
              IF(K.NE.KC) WQALSETX(L,K,nsp) = WQBXSET(L,2,nsp) 
            ENDIF    
          enddo
C  
C UPDATE SOLAR RADIATION AT BOTTOM OF THIS LAYER  
C  
          IF (WQKESS.LT.1.0E-12) WQKESS=0.
          IF (WQKESS1.LT.1.0E-12) WQKESS1=0.
          WQI0BOT(L)=WQI0BOT(L)*EXP(-WQKESS*(1.0/DZWQ(L)))  
          IF(IDNOTRVA.GT.0 .AND. K.EQ.1)THEN  
            WQFDI0 = - WQI0BOT(L) / (WQFD + 1.E-18)  
            WQISM = MAX( WQAVGIO*EXP(-WQKESS1*WQDOPM(IZ)), WQISMIN )  
            WQFDM = WQFDI0 / (WQISM + 1.E-18)  
            WQF2IM = WQTT1 * (EXP(WQFDM*WQTTB) - EXP(WQFDM*WQTTT))  
            !WQF2IM = WQF2IM * PSHADE(L)  
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
C FIRST CONVERT FROM MACROALGAE FROM A CONCENTRATION (MG C/M3)  
C TO A DENSITY (MG C/M2).  
C  
            XMRM = WQVO(L,K,IDNOTRVA)*DZC(K)*HP(L)  
            WQLDF = WQKBP(L) / (WQKBP(L) + XMRM)  
            WQPM(L)= WQPMM(IMWQZT(L))*WQF1NM*WQF2IM*WQTDGM(IWQT(L))*  
     &          WQLDF  
            XLIMVM(L,K) = XLIMVM(L,K) + WQLVF  
            XLIMDM(L,K) = XLIMDM(L,K) + WQLDF  
            XLIMIM(L,K) = XLIMIM(L,K) + WQF2IM  
            XLIMTM(L,K) = XLIMTM(L,K) + WQTDGM(IWQT(L))  
          ENDIF  
          XLIMTC(L,K) = XLIMTC(L,K) + WQTDGC(IWQT(L))  
          XLIMTD(L,K) = XLIMTD(L,K) + WQTDGD(IWQT(L))  
          XLIMTG(L,K) = XLIMTG(L,K) + WQTDGG(IWQT(L))  
          ! X-species
          do nsp=1,NXSP
            XLIMTX(L,K,nsp) = XLIMTX(L,K,nsp) + WQTDGX(IWQT(L),nsp)  
          enddo
C  
C: WQSTOX=WQSTOX**2  
C  
          IF(IWQSTOX.EQ.1)THEN  
            WQF4SC = WQSTOX / (WQSTOX + SWQ(L)*SWQ(L)+1.E-12)  
            WQPC(L)=WQPMC(IMWQZT(L))*WQF1NC*WQF2IC*WQTDGC(IWQT(L))  
     &          *WQF4SC  
          ELSE  
            WQPC(L) = WQPMC(IMWQZT(L))*WQF1NC*WQF2IC*WQTDGC(IWQT(L))  
          ENDIF  
          WQPD(L) = WQPMD(IMWQZT(L))*WQF1ND*WQF2ID*WQTDGD(IWQT(L))  
          WQPG(L) = WQPMG(IMWQZT(L))*WQF1NG*WQF2IG*WQTDGG(IWQT(L))  
          ! X-species
          do nsp=1,NXSP
            IF(IWQSTOX.EQ.1 .and. IWQX(nsp).eq.1)THEN  
              WQF4SC = WQSTOXX(nsp) / (WQSTOXX(nsp)
     &              + SWQ(L)*SWQ(L)+1.E-12)  
              WQPX(L,nsp)=WQPMX(IMWQZT(L),nsp)*WQF1NX(nsp)*WQF2IX(nsp)
     &            *WQTDGX(IWQT(L),nsp)*WQF4SC  
            ENDIF  
            WQPX(L,nsp) = WQPMX(IMWQZT(L),nsp)*WQF1NX(nsp)*WQF2IX(nsp)
     &                   *WQTDGX(IWQT(L),nsp)  
          enddo
C  
C      AT NIGHT, I.E., WHEN SOLAR RADIATION IS LESS THAN 0.001 (05/11/99  
C  
          IF(IWQSUN .EQ. 2)THEN  
            IF(WQI0 .LE. 0.001)THEN  
              WQPC(L) = 0.0  
              WQPD(L) = 0.0  
              WQPG(L) = 0.0  
              ! X-species
              do nsp=1,NXSP
                WQPX(L,nsp) = 0.0
              enddo  
              WQPM(L) = 0.0  
            ENDIF  
          ENDIF  
C  
C ALGAL BASAL METABOLISM & PREDATION  
C  
          WQBMC(L) = WQBMRC(IMWQZT(L)) * WQTDRC(IWQT(L))  
          WQPRC(L) = WQPRRC(IMWQZT(L)) * WQTDRC(IWQT(L))  
C  
C THE VARIABLE WQTDGP ADJUSTS PREDATION AND BASAL METABOLISM BASED ON A  
C LOWER/UPPER OPTIMUM TEMPERATURE FUNCTION.  THIS WILL ALLOW DIATOMS TO  
C BLOOM IN WINTER IF WQTDGP IS CLOSE TO ZERO.  
C  
          WQBMD(L)=WQBMRD(IMWQZT(L))*WQTDRD(IWQT(L))*WQTDGP(IWQT(L))  
          WQPRD(L)=WQPRRD(IMWQZT(L))*WQTDRD(IWQT(L))*WQTDGP(IWQT(L))  
          WQBMG(L) = WQBMRG(IMWQZT(L)) * WQTDRG(IWQT(L))  
          WQPRG(L) = WQPRRG(IMWQZT(L)) * WQTDRG(IWQT(L))  
          IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
            WQBMM(L) = WQBMRM(IMWQZT(L)) * WQTDRM(IWQT(L))  
            WQPRM(L) = WQPRRM(IMWQZT(L)) * WQTDRM(IWQT(L))  
          ENDIF  
          ! X-species
          do nsp=1,NXSP
            WQBMX(L,nsp) = WQBMRX(IMWQZT(L),nsp)*WQTDRX(IWQT(L),nsp)
     &              *WQTDGPX(IWQT(L),nsp)  
            WQPRX(L,nsp) = WQPRRX(IMWQZT(L),nsp)*WQTDRX(IWQT(L),nsp)
     &              *WQTDGPX(IWQT(L),nsp)  
          enddo
        ENDDO  
C  
C END HORIZONTAL LOOP FOR ALGAE PARMETERS  
C  
        DO L=2,LA  
          IZ=IWQZMAP(L,K)  
          WQOBTOT(L) = WQVO(L,K,1)+WQVO(L,K,2)+WQVO(L,K,3)  
         ! X-species
          do nsp=1,NXSP
            WQOBTOT(L) = WQOBTOT(L) + WQVOX(L,K,nsp)
          enddo
          WQKRPC(L) = (WQKRC + WQKRCALG*WQOBTOT(L)) * WQTDHDR(IWQT(L))  
          WQKLPC(L) = (WQKLC + WQKLCALG*WQOBTOT(L)) * WQTDHDR(IWQT(L))  
          XMRM = 0.0  
          IF(IDNOTRVA.GT.0 .AND. K.EQ.1)THEN  
            XMRM = WQKDCALM(IZ) * WQVO(L,K,IDNOTRVA)  
          ENDIF  
C  
C M. MORTON 08/28/99: ADDED SPATIALLY VARIABLE DOC HYDROLYSIS RATE WQKDC  
C    TO ACHIEVE BETTER CONTROL IN SYSTEMS WITH A COMBINATION OF FRESHWAT  
C    STREAMS AND TIDAL RIVERS WITH DIFFERENT CHARACTERISTICS.  
C  
          WQKDOC=(WQKDC(IZ)+WQKDCALG*WQOBTOT(L)+XMRM)*WQTDMNL(IWQT(L))  
          O2WQ(L) = MAX(WQVO(L,K,19), 0.0)  
          WQTT1 = WQKDOC / (WQKHORDO + O2WQ(L)+ 1.E-18)  
          WQKHR(L) = WQTT1 * O2WQ(L)  
          WQDENIT(L)=WQTT1*WQAANOX*RNO3WQ(L)/(WQKHDNN+RNO3WQ(L)+1.E-18)  
        ENDDO  
C  
C 7-10 PHOSPHORUS  
C  
        WQKHP = 0.
        do nsp=1,NXSP
          WQKHP = WQKHP + WQKHPX(nsp)
        enddo
        WQKHP = (WQKHP+WQKHPC+WQKHPD+WQKHPG)/float(NXSP+3)
        DO L=2,LA  
          IZ=IWQZMAP(L,K)  
          WQAPC(L)=1.0/(WQCP1PRM+WQCP2PRM*EXP(-WQCP3PRM*PO4DWQ(L)))  
          WQTT1 = WQKHP / (WQKHP+PO4DWQ(L)+ 1.E-18) * WQOBTOT(L)  
          WQKRPP(L) = (WQKRP + WQKRPALG*WQTT1) * WQTDHDR(IWQT(L))  
          WQKLPP(L) = (WQKLP + WQKLPALG*WQTT1) * WQTDHDR(IWQT(L))  
          WQKDOP(L) = (WQKDP + WQKDPALG*WQTT1) * WQTDMNL(IWQT(L))  
        ENDDO  
        DO L=2,LA  
          IZ=IWQZMAP(L,K)  
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
        ENDDO  
C  
C 11-15 NITROGEN  
C  
        WQKHN = 0.
        do nsp=1,NXSP
          WQKHN = WQKHN + WQKHNX(nsp)
        enddo
        WQKHN = (WQKHN+WQKHNC+WQKHND+WQKHNG)/float(NXSP+3)
        DO L=2,LA  
          IZ=IWQZMAP(L,K)  
          WQTT1 = WQKHN / (WQKHN+RNH4NO3(L)+ 1.E-18) * WQOBTOT(L)  
          WQKRPN(L) = (WQKRN + WQKRNALG*WQTT1) * WQTDHDR(IWQT(L))  
          WQKLPN(L) = (WQKLN + WQKLNALG*WQTT1) * WQTDHDR(IWQT(L))  
          WQKDON(L) = (WQKDN + WQKDNALG*WQTT1) * WQTDMNL(IWQT(L))  
        ENDDO  
        DO L=2,LA  
          IZ=IWQZMAP(L,K)  
          IF(RNH4NO3(L).EQ.0.0)THEN  
            WQPNC(L)=0.0  
            WQPND(L)=0.0  
            WQPNG(L)=0.0  
            WQPNM(L)=0.0  
            ! X-species
            do nsp=1,NXSP
              WQPNX(L,nsp)=0.0
            enddo
          ELSE  
            WQTTC = RNH4WQ(L)/(WQKHNC+RNO3WQ(L)+ 1.E-18)  
            WQTTD = RNH4WQ(L)/(WQKHND+RNO3WQ(L)+ 1.E-18)  
            WQTTG = RNH4WQ(L)/(WQKHNG+RNO3WQ(L)+ 1.E-18)  
            WQTTM = RNH4WQ(L)/(WQKHNM+RNO3WQ(L)+ 1.E-18)  
            WQPNC(L) = (RNO3WQ(L)/(WQKHNC+RNH4WQ(L)+ 1.E-18)  
     &          + WQKHNC/(RNH4NO3(L)+ 1.E-18)) * WQTTC  
            WQPND(L) = (RNO3WQ(L)/(WQKHND+RNH4WQ(L)+ 1.E-18)  
     &          + WQKHND/(RNH4NO3(L)+ 1.E-18)) * WQTTD  
            WQPNG(L) = (RNO3WQ(L)/(WQKHNG+RNH4WQ(L)+ 1.E-18)  
     &          + WQKHNG/(RNH4NO3(L)+ 1.E-18)) * WQTTG  
            WQPNM(L) = (RNO3WQ(L)/(WQKHNM+RNH4WQ(L)+ 1.E-18)  
     &          + WQKHNM/(RNH4NO3(L)+ 1.E-18)) * WQTTM  
            ! X-species
            do nsp=1,NXSP
              WQTTX(nsp) = RNH4WQ(L)/(WQKHNX(nsp)+RNO3WQ(L)+ 1.E-18)
              WQPNX(L,nsp) = (RNO3WQ(L)/(WQKHNX(nsp)+RNH4WQ(L)+ 1.E-18)
     &          + WQKHNX(nsp)/(RNH4NO3(L)+ 1.E-18)) * WQTTX(nsp)
            enddo
          ENDIF  
          WQNIT(L) = O2WQ(L) * WQTDNIT(IWQT(L)) /  
     &        ( (WQKHNDO+O2WQ(L)) * (WQKHNN+RNH4WQ(L)) + 1.E-18)  
        ENDDO  
        IF(IWQSI.EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
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
          ENDDO  
        ENDIF  
C  
C 04/29/99 MRM:  
C THE FOLLOWING ARRAYS WERE ADDED TO KEEP TRACK OF THE VARIOUS COMPONENT  
C OF DISSOLVED OXYGEN.  THE INSTANTANEOUS VALUES FOR EACH COMPONENT ARE  
C SUMMED IN THE ARRAYS AND THEN DUMPED TO THE WQDOCOMP.BIN FILE AT THE  
C SAME TIME INTERVAL AS FOR THE WQWCAVG.BIN FILES (I.E., IWQTSDT INTERVA  
C USUALLY DAILY AVERAGES).  THE ARRAY DESCRIPTIONS ARE:  
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
C  NLIM = COUNTER FOR NUMBER OF ITEMS SUMMED IN EACH ARRAY SLOT  
C  
        DO L=2,LA  
          IZ=IWQZMAP(L,K)  
          WQO18(L)= -DTWQO2*WQKCOD(IWQT(L),IZ)*O2WQ(L) /  
     &        (WQKHCOD(IZ) + O2WQ(L) + 1.E-18)  
C  
C TT THE FOLLOWING MODIFICATION TO THE D.O. SATURATION CALCULATION MADE  
C TT BY J.M. HAMRICK / M.R. MORTON ON 03/08/97.  SEE CHAPRA (1997) PG. 3  
C  
          TVAL1=1./(TWQ(L)+273.15)  
          TVAL2=TVAL1*TVAL1  
          TVAL3=TVAL1*TVAL2  
          TVAL4=TVAL2*TVAL2  
          RLNSAT1=-139.3441+(1.575701E+5*TVAL1)-(6.642308E+7*TVAL2)  
     &        +(1.2438E+10*TVAL3)-(8.621949E+11*TVAL4)  
          RLNSAT2=RLNSAT1-SWQ(L)*( 1.7674E-2-(1.0754E+1*TVAL1)  
     &        +(2.1407E+3*TVAL2) )  
          WQDOS(L) = EXP(RLNSAT2)  
          XDOSAT(L,K) = XDOSAT(L,K) + WQDOS(L)*DTWQ*DZC(K)*HP(L)  
          IF(K.EQ.KC)THEN  
C  
C IN THE FOLLOWING EQUATION, SALINITY MUST BE IN MG/L, HENCE, SWQ(L)  
C IS MULTIPLIED BY 1000.  
C        WQDOS = 14.5532 -  0.38217*TVAL1 + 5.4258E-3*TVAL2 -  
C DO NOT ALLOW WIND SPEEDS ABOVE 11 M/SEC IN THE FOLLOWING EQUATION:  
C  
            WINDREA = WINDST(L)  
            WQWREA=0.728*SQRT(WINDREA)+(0.0372*WINDREA-0.317)*WINDREA  
C  
C        WQWREA = 0.728*SQRT(WINDST(L))  
C  
            IF(IWQKA(IZ) .EQ. 0)THEN  
              WQVREA = WQKRO(IZ)  
              WQWREA = 0.0  
            ENDIF  
C  
C                 WIND VELOCITY COMPUTED ABOVE:  
C  
            IF(IWQKA(IZ) .EQ. 1)THEN  
              WQVREA = WQKRO(IZ)  
            ENDIF  
C  
C    WQKRO = 3.933 TYPICALLY  
C  
            IF(IWQKA(IZ) .EQ. 2)THEN  
              UMRM = 0.5*(U(L,K)+U(L+1,K))  
              VMRM = 0.5*(V(L,K)+V(LNC(L),K))  
              XMRM = SQRT(UMRM*UMRM + VMRM*VMRM)  
              WQVREA = WQKRO(IZ) * XMRM**0.5 / HP(L)**0.5  
            ENDIF  
C  
C    WQKRO = 5.32 TYPICALLY  
C  
            IF(IWQKA(IZ) .EQ. 3)THEN  
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
        ENDDO  
        IF(IWQSRP.EQ.1)THEN  
          DO L=2,LA  
            WQR20(L) = (WQWDSL(L,K,20)+WQWPSL(L,K,20))*VOLWQ(L)  
     &          + (WQVO(L,K,20) - WQTAMP(L,K)) * WQWSSET(L,1)  
            IF(K.EQ.KC)THEN  
              WQR20(L) = WQR20(L) + WQATML(L,KC,20) * VOLWQ(L)  
            ENDIF  
            IF(K.EQ.1) WQR20(L) = WQR20(L)  
     &          + WQTDTAM(IWQT(L))*DZWQ(L)/(WQKHBMF+O2WQ(L)+ 1.E-18)  
            IF(K.NE.KC) WQR20(L) = WQR20(L)  
     &          + (WQVO(L,K+1,20) - WQTAMP(L,K+1)) * WQWSSET(L,2)  
          ENDDO  
        ENDIF  
C  
C TRAPEZOIDAL SOLUTION OF KINETIC EQS: AFTER COMPUTING NEW VALUES, STORE  
C WQVO+WQV INTO WQVO(L,K,NWQV)  
C  
        IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
          DO L=2,LA  
            WQA1C=(WQPM(L)-WQBMM(L)-WQPRM(L)-WQWSM*DZWQ(L))*DTWQO2  
            WQVA1C = 1.0 / (1.0 - WQA1C)  
            WQV(L,K,IDNOTRVA)=(WQVO(L,K,IDNOTRVA)+WQA1C*WQVO(L,  
     &          K,IDNOTRVA))*WQVA1C*SMAC(L)  
            WQV(L,K,IDNOTRVA) = MAX(WQV(L,K,IDNOTRVA),WQMCMIN)*SMAC(L)  
            WQVO(L,K,IDNOTRVA) = WQVO(L,K,IDNOTRVA)+WQV(L,K,IDNOTRVA)  
          ENDDO  
        ENDIF  
C ****  PARAM 01  
        IF(ISTRWQ(1).EQ.1)THEN  
          ! Bentic-cyano
          IF(ISCYANO .EQ. 1 .AND. K .EQ. 1) THEN
            CALL Sub_SPORE(TIMTMP)
          ENDIF 
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
C  
C DEFINITIONS    GROWTH  BASAL METAB  PREDATION  SETTLING      TIME STEP  
C  
              IF(WQBCSET(L,1).GE.0.0)THEN
                WQA1C=(WQPC(L)-WQBMC(L)-WQPRC(L)-WQBCSET(L,1))*DTWQO2  
              ELSE
                IF(K.NE.KC)THEN
                  WQA1C=(WQPC(L)-WQBMC(L)-WQPRC(L)+WQBCSET(L,1))*DTWQO2
                ENDIF
              ENDIF
              WQKK(L) = 1.0 / (1.0 - WQA1C)  
              do nsp=1,NXSP
                if (IWQX(nsp).eq.1) then ! cyano
                  WQACX(nsp)=(WQPX(L,nsp)-WQBMX(L,nsp)-WQPRX(L,nsp))
     &                       *DTWQO2                                 ! GEOSR X-species : jgcho 2015.10.08
                  IF(WQALSETX(L,K,nsp).GT.0.0)THEN                   !{ GEOSR STOKES : YSSONG 2015.08.18  !!!! SINK WQALSET(L,K,1) ! GEOSR X-species : jgcho 2015.10.08
                    WQACX(nsp)=WQACX(nsp)-WQALSETX(L,K,nsp)*DTWQO2   ! GEOSR X-species : jgcho 2015.10.08
                  ENDIF                                              !{ GEOSR STOKES : YSSONG 2015.08.18  !!!! SINK
                  IF(WQALSETX(L,K,nsp).LT.0.0)THEN                   ! GEOSR X-species : jgcho 2015.10.08
                    IF(K.NE.KC)THEN
                      WQACX(nsp)=WQACX(nsp)+WQALSETX(L,K,nsp)*DTWQO2 ! GEOSR X-species : jgcho 2015.10.08
                    ENDIF
                  ENDIF
                  WQKKX(L,nsp) = 1.0 / (1.0 - WQACX(nsp))
                endif
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR1C = (WQWDSL(L,K,1) + WQWPSL(L,K,1)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR1C = WQR1C + WQATML(L,KC,1) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,1) + DTWQ*WQR1C + WQA1C*WQVO(L,K,1)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              do nsp=1,NXSP
                if (IWQX(nsp).eq.1) then ! cyano
                 WQRRX(L,nsp)=WQVOX(L,K,nsp) + WQACX(nsp)*WQVOX(L,K,nsp)
                  IF(ISCYANO.EQ.1.AND.K.EQ.1)THEN  
                    WQRRX(L,nsp) = WQRRX(L,nsp) + 
     &                             CYA_ADD(L)*DZWQ(L)*DTWQ
                  ENDIF
                endif
              enddo
              IF(K .NE. KC)THEN  
                IF(WQBCSET(L,1) .GT. 0.0)THEN
                  WQRR(L) = WQRR(L) + DTWQO2*WQBCSET(L,1)*WQVOCB(L,K+1) ! ORG
                endif
                do nsp=1,NXSP
                  IF(WQALSETX(L,K+1,nsp) .GT. 0.0)THEN  !!! SOURCE from UPPER LAYER
                    WQRRX(L,nsp) = WQRRX(L,nsp)
     &                   + DTWQO2*WQALSETX(L,K+1,nsp)*WQVOXB(L,K+1,nsp)
                  ENDIF
                enddo
              ENDIF  
              IF(K .NE. 1)THEN  
                IF(WQBCSET(L,1) .LT. 0.0)THEN
                  WQRR(L) = WQRR(L) - DTWQO2*WQBCSET(L,1)*WQVOCB(L,K-1) ! ORG
                endif
                do nsp=1,NXSP
                  IF(WQALSETX(L,K-1,nsp) .LT. 0.0)THEN  !!! SOURCE from LOWER LAYER
                    WQRRX(L,nsp) = WQRRX(L,nsp)
     &                   - DTWQO2*WQALSETX(L,K-1,nsp)*WQVOXB(L,K-1,nsp)
                  ENDIF  
                enddo
              ENDIF
C          ENDDO  
!}
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          IF(K.NE.KC)THEN  
C            DO L=2,LA  
C              WQRR(L) = WQRR(L) + DTWQO2*WQBCSET(L,2)*WQVO(L,K+1,1)  
C            ENDDO  
C          ENDIF  

C          DO L=2,LA  
!}
            WQV(L,K,1)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))*WQVO(L,K,1)  
            WQVO(L,K,1) = WQVO(L,K,1)+WQV(L,K,1)  
            do nsp=1,NXSP
               if (IWQX(nsp).eq.1) then
                 WQVX(L,K,nsp)=SCB(L)*(WQRRX(L,nsp)*WQKKX(L,nsp))
     &                        +(1.-SCB(L))*WQVOX(L,K,nsp)  
                 WQVOX(L,K,nsp) = WQVOX(L,K,nsp)+WQVX(L,K,nsp)  
               endif
             enddo
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
             WQV(L,K,1)=WQVO(L,K,1)  
             WQVO(L,K,1) = WQVO(L,K,1)+WQV(L,K,1)  
             do nsp=1,NXSP
               if (IWQX(nsp).eq.1) then
                 WQVX(L,K,nsp)=WQVOX(L,K,nsp)  
                 WQVOX(L,K,nsp) = WQVOX(L,K,nsp)+WQVX(L,K,nsp)  
               endif
             enddo
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,1)=WQVO(L,K,1)  
            WQVO(L,K,1) = WQVO(L,K,1)+WQV(L,K,1)  
            do nsp=1,NXSP
               if (IWQX(nsp).eq.1) then
                 WQVX(L,K,nsp)=WQVOX(L,K,nsp)  
                 WQVOX(L,K,nsp) = WQVOX(L,K,nsp)+WQVX(L,K,nsp)  
               endif
            enddo
          ENDDO  
        ENDIF  
        IF(NXSP .EQ. 1)THEN  ! FOR MASS CONSERVE TEST
          DO L=2,LA  
            CYANOMASS=CYANOMASS+WQVOX(L,K,1)*DZC(K)*HP(L)*DXYP(L)
          ENDDO
        ENDIF
C ****  PARAM 02  
        IF(ISTRWQ(2).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
C  
C DEFINITIONS    GROWTH  BASAL METAB  PREDATION  SETTLING      TIME STEP  
C  
              WQA2D=(WQPD(L)-WQBMD(L)-WQPRD(L)-WQBDSET(L,1))*DTWQO2  
              WQKK(L) = 1.0 / (1.0 - WQA2D)  
              ! X-species
              do nsp=1,NXSP
                if (IWQX(nsp).eq.2) then                  
                  WQA2X(nsp)=(WQPX(L,nsp)-WQBMX(L,nsp)
     &                      -WQPRX(L,nsp)-WQBXSET(L,1,nsp))*DTWQO2  
                  WQKKX(L,nsp) = 1.0 / (1.0 - WQA2X(nsp))
                endif
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR2D = (WQWDSL(L,K,2) + WQWPSL(L,K,2)) * VOLWQ(L)  
              IF(K .EQ. KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR2D = WQR2D + WQATML(L,KC,2) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,2) + DTWQ*WQR2D + WQA2D*WQVO(L,K,2)  
              ! X-species
              do nsp=1,NXSP
                if (IWQX(nsp).eq.2) then
                  WQRRX(L,nsp)=WQVOX(L,K,nsp) + WQA2X(nsp)
     &                  *WQVOX(L,K,nsp)
                endif
              enddo
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(K.NE.KC)THEN  
                WQRR(L) = WQRR(L) + DTWQO2*WQBDSET(L,2)*WQVO(L,K+1,2)  
                ! X-species
                do nsp=1,NXSP
                  if (IWQX(nsp).eq.2) then
                    WQRRX(L,nsp)=WQRRX(L,nsp) + DTWQO2*WQBXSET(L,2,nsp)
     &                   *WQVOX(L,K+1,nsp)
                  endif
                enddo
              ENDIF  
C          ENDDO  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          IF(K.NE.KC)THEN  
C            DO L=2,LA  
C              WQRR(L) = WQRR(L) + DTWQO2*WQBDSET(L,2)*WQVO(L,K+1,2)  
C            ENDDO  
C          ENDIF  
C          DO L=2,LA  
!}
              WQV(L,K,2)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &                  *WQVO(L,K,2)  
              WQVO(L,K,2) = WQVO(L,K,2)+WQV(L,K,2)  
!{ GEOSR X-species : jgcho 2015.10.10
              do nsp=1,NXSP
                if (IWQX(nsp).eq.2) then
                  WQVX(L,K,nsp)=SCB(L)*(WQRRX(L,nsp)*WQKKX(L,nsp))
     &                         +(1.-SCB(L))*WQVOX(L,K,nsp)  
                  WQVOX(L,K,nsp) = WQVOX(L,K,nsp)+WQVX(L,K,nsp)  
                endif
              enddo
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,2)=WQVO(L,K,2)  
              WQVO(L,K,2) = WQVO(L,K,2)+WQV(L,K,2)  
!{ GEOSR X-species : jgcho 2015.10.10
              do nsp=1,NXSP
                if (IWQX(nsp).eq.2) then
                  WQVX(L,K,nsp)=WQVOX(L,K,nsp)
                  WQVOX(L,K,nsp) = WQVOX(L,K,nsp)+WQVX(L,K,nsp)
                endif
              enddo
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,2)=WQVO(L,K,2)  
            WQVO(L,K,2) = WQVO(L,K,2)+WQV(L,K,2)  
            ! X-species
            do nsp=1,NXSP
              if (IWQX(nsp).eq.2) then
                WQVX(L,K,nsp)=WQVOX(L,K,nsp)
                WQVOX(L,K,nsp) = WQVOX(L,K,nsp)+WQVX(L,K,nsp)
              endif
            enddo
          ENDDO  
        ENDIF  
C ****  PARAM 03  
        IF(ISTRWQ(3).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
C  
C DEFINITIONS    GROWTH  BASAL METAB  PREDATION  SETTLING      TIME STEP  
C  
              WQA3G=(WQPG(L)-WQBMG(L)-WQPRG(L)-WQBGSET(L,1))*DTWQO2  
              WQKK(L) = 1.0 / (1.0 - WQA3G)  
              ! X-species
              do nsp=1,NXSP
                if (IWQX(nsp).eq.3) then                  
                  WQA3X(nsp)=(WQPX(L,nsp)-WQBMX(L,nsp)
     &                      -WQPRX(L,nsp)-WQBXSET(L,1,nsp))*DTWQO2  
                  WQKKX(L,nsp) = 1.0 / (1.0 - WQA3X(nsp))
                endif
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR3G = (WQWDSL(L,K,3) + WQWPSL(L,K,3)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR3G = WQR3G + WQATML(L,KC,3) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,3) + DTWQ*WQR3G + WQA3G*WQVO(L,K,3)  
              ! X-species
              do nsp=1,NXSP
                if (IWQX(nsp).eq.3) then
                  WQRRX(L,nsp)=WQVOX(L,K,nsp) + WQA3X(nsp)
     &                  *WQVOX(L,K,nsp)
                endif
              enddo
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(K.NE.KC)THEN  
                WQRR(L) = WQRR(L) + DTWQO2*WQBGSET(L,2)*WQVO(L,K+1,3)  
                ! X-species
                do nsp=1,NXSP
                  if (IWQX(nsp).eq.3) then
                   WQRRX(L,nsp)=WQRRX(L,nsp) + DTWQO2*WQBXSET(L,2,nsp)
     &                   *WQVOX(L,K+1,nsp)
                  endif
                enddo
              ENDIF  
C          ENDDO  
!}
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          IF(K.NE.KC)THEN  
C            DO L=2,LA  
C              WQRR(L) = WQRR(L) + DTWQO2*WQBGSET(L,2)*WQVO(L,K+1,3)  
C            ENDDO  
C          ENDIF  

C          DO L=2,LA  
!}
              WQV(L,K,3)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &                  *WQVO(L,K,3)  
              WQVO(L,K,3) = WQVO(L,K,3)+WQV(L,K,3)  
              ! X-species
              do nsp=1,NXSP
                if (IWQX(nsp).eq.3) then
                  WQVX(L,K,nsp)=SCB(L)*(WQRRX(L,nsp)*WQKKX(L,nsp))
     &                         +(1.-SCB(L))*WQVOX(L,K,nsp)  
                  WQVOX(L,K,nsp) = WQVOX(L,K,nsp)+WQVX(L,K,nsp)  
                endif
              enddo
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,3)=WQVO(L,K,3)  
              WQVO(L,K,3) = WQVO(L,K,3)+WQV(L,K,3)  
              ! X-species
              do nsp=1,NXSP
                if (IWQX(nsp).eq.3) then
                  WQVX(L,K,nsp)=WQVOX(L,K,nsp)
                  WQVOX(L,K,nsp) = WQVOX(L,K,nsp)+WQVX(L,K,nsp)
                endif
              enddo
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,3)=WQVO(L,K,3)  
            WQVO(L,K,3) = WQVO(L,K,3)+WQV(L,K,3)  
            ! X-species
            do nsp=1,NXSP
              if (IWQX(nsp).eq.3) then
                WQVX(L,K,nsp)=WQVOX(L,K,nsp)
                WQVOX(L,K,nsp) = WQVOX(L,K,nsp)+WQVX(L,K,nsp)
              endif
            enddo
          ENDDO  
        ENDIF  
C ****  PARAM 04  
        IF(ISTRWQ(4).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
C  
C DEFINITIONS    HYDROLYSIS  SETTLING  
C  
              WQB4 = - (WQKRPC(L)+WQRPSET(L,1))  
              WQKK(L) = 1.0 / (1.0 - DTWQO2*WQB4)  
C  
C DEFINITIONS    ALGAE PREDATION SOURCE OF RPOC  
C  
              WQA4 = WQFCRP * (WQPRC(L)*WQVO(L,K,1)  
     &            + WQPRD(L)*WQVO(L,K,2) + WQPRG(L)*WQVO(L,K,3))  
              IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
                WQA4 = WQA4+WQFCRPM*WQPRM(L)*WQVO(L,K,IDNOTRVA)  
              ENDIF  
              ! X-species
              do nsp=1,NXSP
                WQA4 = WQA4 + WQFCRP*(WQPRX(L,nsp)*WQVOX(L,K,nsp))
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR4 = (WQWDSL(L,K,4) + WQWPSL(L,K,4)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR4 = WQR4 + WQATML(L,KC,4) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,4) + DTWQ*WQR4 + DTWQO2*( WQA4  
     &            + WQB4*WQVO(L,K,4) )  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(K.NE.KC)THEN  
                WQRR(L) = WQRR(L) + DTWQO2*WQRPSET(L,2)*WQVO(L,K+1,4)  
              ENDIF  
C          ENDDO 
!}
!{GeoSR, YSSONG, WQ WET/DRY, 110915 
C          IF(K.NE.KC)THEN  
C            DO L=2,LA  
C              WQRR(L) = WQRR(L) + DTWQO2*WQRPSET(L,2)*WQVO(L,K+1,4)  
C            ENDDO  
C          ENDIF  

C          DO L=2,LA  
!}
              WQV(L,K,4)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &              *WQVO(L,K,4)  
              WQVO(L,K,4) = WQVO(L,K,4)+WQV(L,K,4)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,4)=WQVO(L,K,4)  
              WQVO(L,K,4) = WQVO(L,K,4)+WQV(L,K,4)  
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,4)=WQVO(L,K,4)  
            WQVO(L,K,4) = WQVO(L,K,4)+WQV(L,K,4)  
          ENDDO  
        ENDIF  
C ****  PARAM 05  
        IF(ISTRWQ(5).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
              WQC5 = - (WQKLPC(L)+WQLPSET(L,1))  
              WQKK(L) = 1.0 / (1.0 - DTWQO2*WQC5)  
              WQA5 = WQFCLP * (WQPRC(L)*WQVO(L,K,1)  
     &            + WQPRD(L)*WQVO(L,K,2) + WQPRG(L)*WQVO(L,K,3))  
              IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
                WQA5 =WQA5 + WQFCLPM * WQPRM(L)*WQVO(L,K,IDNOTRVA)  
              ENDIF  
             ! X-species
              do nsp=1,NXSP
                WQA5 = WQA5 + WQFCLP*(WQPRX(L,nsp)*WQVOX(L,K,nsp))
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR5 = (WQWDSL(L,K,5) + WQWPSL(L,K,5)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR5 = WQR5 + WQATML(L,KC,5) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,5) + DTWQ*WQR5 + DTWQO2*( WQA5  
     &            + WQC5*WQVO(L,K,5) )  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(K.NE.KC)THEN  
                  WQRR(L) = WQRR(L) + DTWQO2*WQLPSET(L,2)*WQVO(L,K+1,5)  
              ENDIF  
C          ENDDO  
!}
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          IF(K.NE.KC)THEN  
C            DO L=2,LA  
C              WQRR(L) = WQRR(L) + DTWQO2*WQLPSET(L,2)*WQVO(L,K+1,5)  
C            ENDDO  
C          ENDIF  

C          DO L=2,LA  
!}
              WQV(L,K,5)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &                   *WQVO(L,K,5)  
              WQVO(L,K,5) = WQVO(L,K,5)+WQV(L,K,5)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,5)=WQVO(L,K,5)  
              WQVO(L,K,5) = WQVO(L,K,5)+WQV(L,K,5)  
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,5)=WQVO(L,K,5)  
            WQVO(L,K,5) = WQVO(L,K,5)+WQV(L,K,5)  
          ENDDO  
        ENDIF  
C ****  PARAM 06  
        IF(ISTRWQ(6).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
              WQD6 = - (WQKHR(L)+WQDENIT(L))  
              WQKK(L) = 1.0 / (1.0 - DTWQO2*WQD6)  
              O2WQ(L) = MAX(WQVO(L,K,19), 0.0)  
              WQA6C=(WQFCDC+CFCDCWQ*WQKHRC/(WQKHRC+O2WQ(L)+1.E-18))
     &              *WQBMC(L)
              WQA6D=(WQFCDD+CFCDDWQ*WQKHRD/(WQKHRD+O2WQ(L)+1.E-18))
     &              *WQBMD(L)
              WQA6G=(WQFCDG+CFCDGWQ*WQKHRG/(WQKHRG+O2WQ(L)+1.E-18))
     &              *WQBMG(L)
              WQA6 = ( WQA6C + WQFCDP*WQPRC(L) )*WQVO(L,K,1)  
     &            + ( WQA6D + WQFCDP*WQPRD(L) )*WQVO(L,K,2)  
     &            + ( WQA6G + WQFCDP*WQPRG(L) )*WQVO(L,K,3)  
              IF(IDNOTRVA .GT. 0 .AND. K .EQ. 1)THEN  
                WQA6M=(WQFCDM+(1-WQFCDM)*WQKHRM(IZ) /  
     &              (WQKHRM(IZ) + O2WQ(L) + 1.E-18))*WQBMM(L)  
                WQA6 =WQA6+ (WQA6M+ WQFCDPM*WQPRM(L))*WQVO(L,K,IDNOTRVA)  
              ENDIF  
              ! X-species
              do nsp=1,NXSP
                WQA6X=(WQFCDX(nsp)+CFCDWQX(nsp)*WQKHRX(nsp)
     &                /(WQKHRX(nsp)+O2WQ(L)+1.E-18))*WQBMX(L,nsp)
                WQA6 = WQA6
     &                + (WQA6X + WQFCDP*WQPRX(L,nsp))*WQVOX(L,K,nsp)
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR6 = (WQWDSL(L,K,6) + WQWPSL(L,K,6)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR6 = WQR6 + WQATML(L,KC,6) * VOLWQ(L)  
              ENDIF  
              WQRR(L)=WQVO(L,K,6)+DTWQ*WQR6+DTWQO2*(WQA6+WQKRPC(L)*  
     &          WQVO(L,K,4) + WQKLPC(L)*WQVO(L,K,5) + WQD6*WQVO(L,K,6) )  
              WQV(L,K,6)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &              *WQVO(L,K,6)  
              WQVO(L,K,6) = WQVO(L,K,6)+WQV(L,K,6)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,6)=WQVO(L,K,6)  
              WQVO(L,K,6) = WQVO(L,K,6)+WQV(L,K,6)  
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,6)=WQVO(L,K,6)  
            WQVO(L,K,6) = WQVO(L,K,6)+WQV(L,K,6)  
          ENDDO  
        ENDIF  
C ****  PARAM 07  
        IF(ISTRWQ(7).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
              WQE7 = - (WQKRPP(L)+WQRPSET(L,1))  
              WQKK(L) = 1.0 / (1.0 - DTWQO2*WQE7)  
              WQA7C = (WQFPRC*WQBMC(L) + WQFPRP*WQPRC(L)) * WQVO(L,K,1)  
              WQA7D = (WQFPRD*WQBMD(L) + WQFPRP*WQPRD(L)) * WQVO(L,K,2)  
              WQA7G = (WQFPRG*WQBMG(L) + WQFPRP*WQPRG(L)) * WQVO(L,K,3)  
              WQA7 = (WQA7C+WQA7D+WQA7G) * WQAPC(L)  
              IF(IDNOTRVA .GT. 0 .AND. K .EQ. 1)THEN  
                WQA7 = WQA7 + (WQFPRM*WQBMM(L) + WQFPRPM*WQPRM(L))  
     &              * WQVO(L,K,IDNOTRVA)* WQAPC(L)*WQAPCM  
              ENDIF  
              ! X-species
              do nsp=1,NXSP
                WQA7X = (WQFPRX(nsp)*WQBMX(L,nsp) + WQFPRP*WQPRX(L,nsp))
     &                  * WQVOX(L,K,nsp)  
                WQA7 = WQA7 + WQA7X*WQAPC(L)
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR7 = (WQWDSL(L,K,7) + WQWPSL(L,K,7)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR7 = WQR7 + WQATML(L,KC,7) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,7) + DTWQ*WQR7 + DTWQO2*( WQA7  
     &            + WQE7*WQVO(L,K,7) )  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(K.NE.KC)THEN  
                  WQRR(L) = WQRR(L) + DTWQO2*WQRPSET(L,2)*WQVO(L,K+1,7)  
              ENDIF  
C          ENDDO  
!}
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          IF(K.NE.KC)THEN  
C            DO L=2,LA  
C              WQRR(L) = WQRR(L) + DTWQO2*WQRPSET(L,2)*WQVO(L,K+1,7)  
C            ENDDO  
C          ENDIF  

C          DO L=2,LA  
!}
              WQV(L,K,7)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &                   *WQVO(L,K,7)  
              WQVO(L,K,7) = WQVO(L,K,7)+WQV(L,K,7)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,7)=WQVO(L,K,7)  
              WQVO(L,K,7) = WQVO(L,K,7)+WQV(L,K,7)  
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,7)=WQVO(L,K,7)  
            WQVO(L,K,7) = WQVO(L,K,7)+WQV(L,K,7)  
          ENDDO  
        ENDIF  
C ****  PARAM 08  
        IF(ISTRWQ(8).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
              WQF8 = - (WQKLPP(L)+WQLPSET(L,1))  
              WQKK(L) = 1.0 / (1.0 - DTWQO2*WQF8)  
              WQA8C = (WQFPLC*WQBMC(L) + WQFPLP*WQPRC(L)) * WQVO(L,K,1)  
              WQA8D = (WQFPLD*WQBMD(L) + WQFPLP*WQPRD(L)) * WQVO(L,K,2)  
              WQA8G = (WQFPLG*WQBMG(L) + WQFPLP*WQPRG(L)) * WQVO(L,K,3)  
              WQA8 = (WQA8C+WQA8D+WQA8G) * WQAPC(L)  
              IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
                WQA8 = WQA8 +     (WQFPLM*WQBMM(L) + WQFPLPM*WQPRM(L))  
     &              * WQVO(L,K,IDNOTRVA)* WQAPC(L)*WQAPCM  
              ENDIF  
              ! X-species
              do nsp=1,NXSP
                WQA8X = (WQFPLX(nsp)*WQBMX(L,nsp) + WQFPLP*WQPRX(L,nsp))
     &                  * WQVOX(L,K,nsp)  
                WQA8 = WQA8 + WQA8X*WQAPC(L)
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR8 = (WQWDSL(L,K,8) + WQWPSL(L,K,8)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR8 = WQR8 + WQATML(L,KC,8) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,8) + DTWQ*WQR8 + DTWQO2*( WQA8  
     &            + WQF8*WQVO(L,K,8) )  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(K.NE.KC)THEN  
                  WQRR(L) = WQRR(L) + DTWQO2*WQLPSET(L,2)*WQVO(L,K+1,8)  
              ENDIF  
C          ENDDO  
!}
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          IF(K.NE.KC)THEN  
C            DO L=2,LA  
C              WQRR(L) = WQRR(L) + DTWQO2*WQLPSET(L,2)*WQVO(L,K+1,8)  
C            ENDDO  
C          ENDIF  

C          DO L=2,LA  
!}
            WQV(L,K,8)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &              *WQVO(L,K,8)  
            WQVO(L,K,8) = WQVO(L,K,8)+WQV(L,K,8)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
             WQV(L,K,8)=WQVO(L,K,8)  
             WQVO(L,K,8) = WQVO(L,K,8)+WQV(L,K,8)  
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,8)=WQVO(L,K,8)  
            WQVO(L,K,8) = WQVO(L,K,8)+WQV(L,K,8)  
          ENDDO  
        ENDIF  
C ****  PARAM 09  
        IF(ISTRWQ(9).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
              WQKK(L) = 1.0 / (1.0 + DTWQO2*WQKDOP(L))  
              WQA9C = (WQFPDC*WQBMC(L) + WQFPDP*WQPRC(L)) * WQVO(L,K,1)  
              WQA9D = (WQFPDD*WQBMD(L) + WQFPDP*WQPRD(L)) * WQVO(L,K,2)  
              WQA9G = (WQFPDG*WQBMG(L) + WQFPDP*WQPRG(L)) * WQVO(L,K,3)  
              WQA9 = (WQA9C+WQA9D+WQA9G) * WQAPC(L)  
              IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
                WQA9 = WQA9 + (WQFPDM*WQBMM(L) + WQFPDPM*WQPRM(L))  
     &              * WQVO(L,K,IDNOTRVA) * WQAPC(L)*WQAPCM  
              ENDIF  
              ! X-species
              do nsp=1,NXSP
                WQA9X = (WQFPDX(nsp)*WQBMX(L,nsp) + WQFPDP*WQPRX(L,nsp))
     &                  * WQVOX(L,K,nsp)  
                WQA9 = WQA9 + WQA9X*WQAPC(L)
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR9 = (WQWDSL(L,K,9) + WQWPSL(L,K,9)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR9 = WQR9 + WQATML(L,KC,9) * VOLWQ(L)  
              ENDIF  
              WQRR(L)=WQVO(L,K,9)+DTWQ*WQR9+DTWQO2*(WQA9+WQKRPP(L)*  
     &          WQVO(L,K,7)+WQKLPP(L)*WQVO(L,K,8)-WQKDOP(L)*WQVO(L,K,9))  
              WQV(L,K,9)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &                     *WQVO(L,K,9)
              WQVO(L,K,9) = WQVO(L,K,9)+WQV(L,K,9)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,9)=WQVO(L,K,9)  
              WQVO(L,K,9) = WQVO(L,K,9)+WQV(L,K,9)  
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,9)=WQVO(L,K,9)  
            WQVO(L,K,9) = WQVO(L,K,9)+WQV(L,K,9)  
          ENDDO  
        ENDIF  
C ****  PARAM 10  
        IF(ISTRWQ(10).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
              WQA10C=(WQFPIC*WQBMC(L)+WQFPIP*WQPRC(L)-WQPC(L))
     &              *WQVO(L,K,1)  
              WQA10D=(WQFPID*WQBMD(L)+WQFPIP*WQPRD(L)-WQPD(L))
     &        *WQVO(L,K,2)  
              WQA10G=(WQFPIG*WQBMG(L)+WQFPIP*WQPRG(L)-WQPG(L))
     &        *WQVO(L,K,3)  
              WQKK(L) = (WQA10C+WQA10D+WQA10G) * WQAPC(L)  
              IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
                WQKK(L) =WQKK(L)
     &              +(WQFPIM*WQBMM(L)+WQFPIP*WQPRM(L)-WQPM(L))  
     &              *WQVO(L,K,IDNOTRVA) * WQAPC(L)*WQAPCM  
              ENDIF  
              ! X-species
              do nsp=1,NXSP
                WQA10X=(WQFPIX(nsp)*WQBMX(L,nsp)+WQFPIP*WQPRX(L,nsp)
     &              -WQPX(L,nsp))*WQVOX(L,K,nsp)
                WQKK(L) = WQKK(L) + WQA10X*WQAPC(L)
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQRR(L) = (WQWDSL(L,K,10)+WQWPSL(L,K,10)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQRR(L) = WQRR(L) + WQATML(L,KC,10) * VOLWQ(L)  
              ENDIF  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          ENDDO  
!}
              IF(K.EQ.1)THEN  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C            DO L=2,LA  
                WQRR(L) = WQRR(L) + WQBFPO4D(L)*DZWQ(L)  
C            ENDDO  
              ENDIF  
C          DO L=2,LA  
!}
              WQRR(L) = WQVO(L,K,10) + DTWQ*WQRR(L) + DTWQO2*( WQKK(L)  
     &          + WQKDOP(L)*WQVO(L,K,9) + WQH10(L)*WQVO(L,K,10) )  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(K.NE.KC)THEN  
                  WQRR(L) = WQRR(L) + DTWQO2*WQT10(L)*WQVO(L,K+1,10)  
              ENDIF  
C          ENDDO  
!}
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          IF(K.NE.KC)THEN  
C            DO L=2,LA  
C              WQRR(L) = WQRR(L) + DTWQO2*WQT10(L)*WQVO(L,K+1,10)  
C            ENDDO  
C          ENDIF  

C          DO L=2,LA  
!}
              WQKK(L) = 1.0 / (1.0 - DTWQO2*WQH10(L))  
              WQV(L,K,10)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &              *WQVO(L,K,10)  
              WQVO(L,K,10) = WQVO(L,K,10)+WQV(L,K,10)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,10)=WQVO(L,K,10)  
              WQVO(L,K,10) = WQVO(L,K,10)+WQV(L,K,10)  
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,10)=WQVO(L,K,10)  
            WQVO(L,K,10) = WQVO(L,K,10)+WQV(L,K,10)  
          ENDDO  
        ENDIF  
C ****  PARAM 11  
        IF(ISTRWQ(11).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
              WQI11 = - (WQKRPN(L)+WQRPSET(L,1))  
              WQKK(L) = 1.0 / (1.0 - DTWQO2*WQI11)  
              WQA11C=(WQFNRC*WQBMC(L)+WQFNRP*WQPRC(L))
     &              *WQANCC*WQVO(L,K,1)  
              WQA11D=(WQFNRD*WQBMD(L)+WQFNRP*WQPRD(L))
     &              *WQANCD*WQVO(L,K,2)  
              WQA11G=(WQFNRG*WQBMG(L)+WQFNRP*WQPRG(L))
     &              *WQANCG*WQVO(L,K,3)  
              WQA11 = WQA11C+WQA11D+WQA11G  
              IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
                WQA11 =WQA11 +     (WQFNRM*WQBMM(L)+WQFNRPM*WQPRM(L))  
     &              *WQANCM*WQVO(L,K,IDNOTRVA)  
              ENDIF  
              ! X-species
              do nsp=1,NXSP
                WQA11X=(WQFNRX(nsp)*WQBMX(L,nsp)+WQFNRP*WQPRX(L,nsp))
     &                *WQANCX(nsp)*WQVOX(L,K,nsp)  
                WQA11 = WQA11+WQA11X
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR11 = (WQWDSL(L,K,11)+WQWPSL(L,K,11)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR11 = WQR11 + WQATML(L,KC,11) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,11) + DTWQ*WQR11 + DTWQO2*( WQA11  
     &            + WQI11*WQVO(L,K,11) )  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(K.NE.KC)THEN  
                  WQRR(L) = WQRR(L) + DTWQO2*WQRPSET(L,2)*WQVO(L,K+1,11)  
              ENDIF  
C          ENDDO 
!}
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          IF(K.NE.KC)THEN  
C            DO L=2,LA  
C              WQRR(L) = WQRR(L) + DTWQO2*WQRPSET(L,2)*WQVO(L,K+1,11)  
C            ENDDO  
C          ENDIF  

C          DO L=2,LA  
!}
              WQV(L,K,11)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &             *WQVO(L,K,11)  
              WQVO(L,K,11) = WQVO(L,K,11)+WQV(L,K,11)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,11)=WQVO(L,K,11)  
              WQVO(L,K,11) = WQVO(L,K,11)+WQV(L,K,11)  
            ENDIF
          ENDDO  
!} 
        ELSE  
          DO L=2,LA  
            WQV(L,K,11)=WQVO(L,K,11)  
            WQVO(L,K,11) = WQVO(L,K,11)+WQV(L,K,11)  
          ENDDO  
        ENDIF  
C ****  PARAM 12  
        IF(ISTRWQ(12).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
              WQJ12 = - (WQKLPN(L)+WQLPSET(L,1))  
              WQKK(L) = 1.0 / (1.0 - DTWQO2*WQJ12)  
              WQA12C=(WQFNLC*WQBMC(L)+WQFNLP*WQPRC(L))*WQANCC
     &                *WQVO(L,K,1)  
              WQA12D=(WQFNLD*WQBMD(L)+WQFNLP*WQPRD(L))*WQANCD
     &                *WQVO(L,K,2)  
              WQA12G=(WQFNLG*WQBMG(L)+WQFNLP*WQPRG(L))*WQANCG
     &                *WQVO(L,K,3)  
              WQA12 = WQA12C+WQA12D+WQA12G  
              IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
                WQA12 =WQA12 +(WQFNLM*WQBMM(L)+WQFNLPM*WQPRM(L))  
     &              *WQANCM*WQVO(L,K,IDNOTRVA)  
              ENDIF  
              ! X-species
              do nsp=1,NXSP
                WQA12X=(WQFNLX(nsp)*WQBMX(L,nsp)+WQFNLP*WQPRX(L,nsp))
     &                *WQANCX(nsp)*WQVOX(L,K,nsp)  
                WQA12 = WQA12+WQA12X
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR12 = (WQWDSL(L,K,12)+WQWPSL(L,K,12)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR12 = WQR12 + WQATML(L,KC,12) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,12) + DTWQ*WQR12 + DTWQO2*( WQA12  
     &            + WQJ12*WQVO(L,K,12) )  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(K.NE.KC)THEN  
                  WQRR(L) = WQRR(L) + DTWQO2*WQLPSET(L,2)*WQVO(L,K+1,12)  
              ENDIF  
C          ENDDO  
!}
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          IF(K.NE.KC)THEN  
C            DO L=2,LA  
C              WQRR(L) = WQRR(L) + DTWQO2*WQLPSET(L,2)*WQVO(L,K+1,12)  
C            ENDDO  
C          ENDIF  

C          DO L=2,LA  
!}
              WQV(L,K,12)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &          *WQVO(L,K,12)  
              WQVO(L,K,12) = WQVO(L,K,12)+WQV(L,K,12)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,12)=WQVO(L,K,12)  
              WQVO(L,K,12) = WQVO(L,K,12)+WQV(L,K,12)  
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,12)=WQVO(L,K,12)  
            WQVO(L,K,12) = WQVO(L,K,12)+WQV(L,K,12)  
          ENDDO  
        ENDIF  
C ****  PARAM 13  
        IF(ISTRWQ(13).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
              WQKK(L) = 1.0 / (1.0 + DTWQO2*WQKDON(L))  
              WQA13C=(WQFNDC*WQBMC(L)+WQFNDP*WQPRC(L))*WQANCC
     &                *WQVO(L,K,1)  
              WQA13D=(WQFNDD*WQBMD(L)+WQFNDP*WQPRD(L))*WQANCD
     &                *WQVO(L,K,2)  
              WQA13G=(WQFNDG*WQBMG(L)+WQFNDP*WQPRG(L))*WQANCG
     &                *WQVO(L,K,3)  
              WQA13 = WQA13C+WQA13D+WQA13G  
              IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
                WQA13 =WQA13 + (WQFNDM*WQBMM(L)+WQFNDPM*WQPRM(L))  
     &              *WQANCM*WQVO(L,K,IDNOTRVA)  
              ENDIF  
              ! X-species
              do nsp=1,NXSP
                WQA13X=(WQFNDX(nsp)*WQBMX(L,nsp)+WQFNDP*WQPRX(L,nsp))
     &                *WQANCX(nsp)*WQVOX(L,K,nsp)  
                WQA13 = WQA13+WQA13X
              enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQR13 = (WQWDSL(L,K,13) + WQWPSL(L,K,13)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQR13 = WQR13 + WQATML(L,KC,13) * VOLWQ(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,13) + DTWQ*WQR13 + DTWQO2*( WQA13  
     &            + WQKRPN(L)*WQVO(L,K,11) + WQKLPN(L)*WQVO(L,K,12)  
     &            - WQKDON(L)*WQVO(L,K,13) )  
              WQV(L,K,13)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &                *WQVO(L,K,13)  
              WQVO(L,K,13) = WQVO(L,K,13)+WQV(L,K,13)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,13)=WQVO(L,K,13)  
              WQVO(L,K,13) = WQVO(L,K,13)+WQV(L,K,13)  
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,13)=WQVO(L,K,13)  
            WQVO(L,K,13) = WQVO(L,K,13)+WQV(L,K,13)  
          ENDDO  
        ENDIF  
C ****  PARAM 14  
        IF(ISTRWQ(14).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQRR(L) = (WQWDSL(L,K,14)+WQWPSL(L,K,14)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQRR(L) = WQRR(L) + WQATML(L,KC,14) * VOLWQ(L)  
              ENDIF  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          ENDDO  
!}
              IF(K.EQ.1)THEN  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C            DO L=2,LA  
                WQRR(L) = WQRR(L) + WQBFNH4(L)*DZWQ(L)  
C            ENDDO  
!}
              ENDIF  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          DO L=2,LA  
              WQKK(L) = 1.0 / (1.0 + DTWQO2*WQNIT(L))  
              WQA14C=WQFNIC*WQBMC(L)+WQFNIP*WQPRC(L)-WQPNC(L)*WQPC(L)  
              WQA14D=WQFNID*WQBMD(L)+WQFNIP*WQPRD(L)-WQPND(L)*WQPD(L)  
              WQA14G=WQFNIG*WQBMG(L)+WQFNIP*WQPRG(L)-WQPNG(L)*WQPG(L)  
              WQA14 = WQA14C*WQANCC*WQVO(L,K,1)  
     &          + WQA14D*WQANCD*WQVO(L,K,2) + WQA14G*WQANCG*WQVO(L,K,3)  
              IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
                WQA14 = WQA14 + (WQFNIM*WQBMM(L)+WQFNIPM*WQPRM(L)  
     &              - WQPNM(L)*WQPM(L))*WQANCM*WQVO(L,K,IDNOTRVA)  
              ENDIF  
              ! X-species
              do nsp=1,NXSP
                WQA14X=WQFNIX(nsp)*WQBMX(L,nsp)+WQFNIP*WQPRX(L,nsp)
     &                -WQPNX(L,nsp)*WQPX(L,nsp)  
                WQA14 = WQA14 + WQA14X*WQANCX(nsp)*WQVOX(L,K,nsp)
              enddo
              WQRR(L) = WQVO(L,K,14) + DTWQ*WQRR(L) + DTWQO2*( WQA14  
     &            + WQKDON(L)*WQVO(L,K,13) - WQNIT(L)*WQVO(L,K,14) )  
              WQV(L,K,14)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &              *WQVO(L,K,14)  
              WQVO(L,K,14) = WQVO(L,K,14)+WQV(L,K,14)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,14)=WQVO(L,K,14)  
              WQVO(L,K,14) = WQVO(L,K,14)+WQV(L,K,14)  
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,14)=WQVO(L,K,14)  
            WQVO(L,K,14) = WQVO(L,K,14)+WQV(L,K,14)  
          ENDDO  
        ENDIF  
C ****  PARAM 15  
        IF(ISTRWQ(15).EQ.1)THEN  
          DO L=2,LA  
            IZ=IWQZMAP(L,K)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQRR(L) = (WQWDSL(L,K,15)+WQWPSL(L,K,15)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQRR(L) = WQRR(L) + WQATML(L,KC,15) * VOLWQ(L)  
              ENDIF  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          ENDDO  
              IF(K.EQ.1)THEN  
C            DO L=2,LA  
                WQRR(L) = WQRR(L) + WQBFNO3(L)*DZWQ(L)  
C            ENDDO  
              ENDIF  
C          DO L=2,LA  
!}
              WQA15C = (WQPNC(L)-1.0)*WQPC(L) * WQANCC * WQVO(L,K,1)  
              WQA15D = (WQPND(L)-1.0)*WQPD(L) * WQANCD * WQVO(L,K,2)  
              WQA15G = (WQPNG(L)-1.0)*WQPG(L) * WQANCG * WQVO(L,K,3)  
              WQA15 = WQA15C+WQA15D+WQA15G  
              IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
                WQA15 =WQA15 + (WQPNM(L)-1.0)*WQPM(L)*WQANCM  
     &              *WQVO(L,K,IDNOTRVA)  
              ENDIF  
              ! X-species 
              do nsp=1,NXSP
                WQA15X=(WQPNX(L,nsp)-1.0)*WQPX(L,nsp) * WQANCX(nsp)
     &                * WQVO(L,K,nsp)  
                WQA15 = WQA15 + WQA15X
              enddo
            WQV(L,K,15)=SCB(L)*( WQVO(L,K,15) + DTWQ*WQRR(L)  
     &          + DTWQO2*( WQA15  
     &          -WQANDC*WQDENIT(L)*WQVO(L,K,6)+WQNIT(L)*WQVO(L,K,14)))  
     &          +(1.-SCB(L))*WQVO(L,K,15)  
            WQVO(L,K,15) = WQVO(L,K,15)+WQV(L,K,15)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
             WQV(L,K,15)=WQVO(L,K,15)  
             WQVO(L,K,15) = WQVO(L,K,15)+WQV(L,K,15)  
            ENDIF
          ENDDO  
!}
        ELSE  
          DO L=2,LA  
            WQV(L,K,15)=WQVO(L,K,15)  
            WQVO(L,K,15) = WQVO(L,K,15)+WQV(L,K,15)  
          ENDDO  
        ENDIF  
C ****  PARAM 16  
        IF(ISTRWQ(16).EQ.1)THEN  
          IF(IWQSI.EQ.1)THEN  
            DO L=2,LA  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
                WQM16 = - (WQKSUA(IWQT(L)) + WQBDSET(L,1))  
                WQKK(L) = 1.0 / (1.0 - DTWQO2*WQM16)  
                WQA16D = (WQFSPD*WQBMD(L) + WQFSPP*WQPRD(L)) * WQASCD  
     &              * WQVO(L,K,2)  
!{ GEOSR X-species : jgcho 2015.10.12
                do nsp=1,NXSP
                  if (IWQX(nsp).eq.2) then
                    WQA16D = WQA16D + (WQFSPDX(nsp)*WQBMX(L,nsp)
     &                    + WQFSPPX(nsp)*WQPRX(L,nsp)) * WQASCDX(nsp)
     &               * WQVOX(L,K,nsp)  
                  endif
                enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
                WQR16 = (WQWDSL(L,K,16)+WQWPSL(L,K,16)) * VOLWQ(L)  
                IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                  WQR16 = WQR16 + WQATML(L,KC,16) * VOLWQ(L)  
                ENDIF  
                WQRR(L) = WQVO(L,K,16) + DTWQ*WQR16 + DTWQO2*( WQA16D  
     &              + WQM16*WQVO(L,K,16) )  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C            ENDDO  
                IF(K.NE.KC)THEN  
C              DO L=2,LA  
                  WQRR(L) = WQRR(L) + DTWQO2*WQBDSET(L,2)*WQVO(L,K+1,16)  
C              ENDDO  
                ENDIF  
C            DO L=2,LA  
!}
                WQV(L,K,16)=SCB(L)*( WQRR(L)*WQKK(L) )  
     &              +(1.-SCB(L))*WQVO(L,K,16)  
                WQVO(L,K,16) = WQVO(L,K,16)+WQV(L,K,16)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              ELSE
                WQV(L,K,16)=WQVO(L,K,16)  
                WQVO(L,K,16) = WQVO(L,K,1)+WQV(L,K,16)  
              ENDIF
            ENDDO  
!}
          ENDIF  
        ELSE  
          DO L=2,LA  
            WQV(L,K,16)=WQVO(L,K,16)  
            WQVO(L,K,16) = WQVO(L,K,16)+WQV(L,K,16)  
          ENDDO  
        ENDIF  
C ****  PARAM 17  
        IF(ISTRWQ(17).EQ.1)THEN  
          IF(IWQSI.EQ.1)THEN  
            DO L=2,LA  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
                WQKK(L) = (WQFSID*WQBMD(L) + WQFSIP*WQPRD(L) - WQPD(L))  
     &              * WQASCD * WQVO(L,K,2)  
                ! X-species
                do nsp=1,NXSP
                  if (IWQX(nsp).eq.2) then
                    WQKK(L) = WQKK(L) + (WQFSIDX(nsp)*WQBMX(L,nsp)
     &               + WQFSIPX(nsp)*WQPRX(L,nsp) - WQPX(L,nsp))  
     &               * WQASCDX(nsp) * WQVOX(L,K,nsp)  
                  endif
                enddo
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
                WQRR(L) = (WQWDSL(L,K,17)+WQWPSL(L,K,17)) * VOLWQ(L)  
                IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                  WQRR(L) = WQRR(L) + WQATML(L,KC,17) * VOLWQ(L)  
                ENDIF  
C            ENDDO  
                IF(K.EQ.1)THEN  
C              DO L=2,LA  
                  WQRR(L) = WQRR(L) + WQBFSAD(L)*DZWQ(L)  
C              ENDDO  
                ENDIF  
C            DO L=2,LA  
                WQRR(L) = WQVO(L,K,17) + DTWQ*WQRR(L) + DTWQO2
     &            *(WQKK(L)+WQKSUA(IWQT(L))*WQVO(L,K,16)
     &            +WQN17(L)*WQVO(L,K,17))  
C            ENDDO  
                IF(K.NE.KC)THEN  
C              DO L=2,LA  
                  WQRR(L) = WQRR(L) + DTWQO2*WQT17(L)*WQVO(L,K+1,17)  
C              ENDDO  
                ENDIF  
C            DO L=2,LA  
!}
                WQKK(L) = 1.0 / (1.0 - DTWQO2*WQN17(L))  
                WQV(L,K,17)=SCB(L)*( WQRR(L)*WQKK(L) )  
     &              +(1.-SCB(L))*WQVO(L,K,17)  
                WQVO(L,K,17) = WQVO(L,K,17)+WQV(L,K,17)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              ELSE
                WQV(L,K,17)=WQVO(L,K,17)  
                WQVO(L,K,17) = WQVO(L,K,17)+WQV(L,K,17)  
              ENDIF
            ENDDO  
!}
          ENDIF  
        ELSE  
          DO L=2,LA  
            WQV(L,K,17)=WQVO(L,K,17)  
            WQVO(L,K,17) = WQVO(L,K,17)+WQV(L,K,17)  
          ENDDO  
        ENDIF  
C ****  PARAM 18  
        IF(ISTRWQ(18).EQ.1)THEN  
          DO L=2,LA  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
              WQKK(L) = 1.0 / (1.0 - WQO18(L))  
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQRR(L) = (WQWDSL(L,K,18)+WQWPSL(L,K,18)) * VOLWQ(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQRR(L) = WQRR(L) + WQATML(L,KC,18) * VOLWQ(L)  
              ENDIF  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          ENDDO  
              IF(K.EQ.1)THEN  
C            DO L=2,LA  
                WQRR(L) = WQRR(L) + WQBFCOD(L)*DZWQ(L)  
C            ENDDO  
              ENDIF  
C          DO L=2,LA  
!}
              WQRR(L)=WQVO(L,K,18)+DTWQ*WQRR(L)+WQO18(L)*WQVO(L,K,18)  
              WQV(L,K,18)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &              *WQVO(L,K,18)  
              WQVO(L,K,18) = WQVO(L,K,18)+WQV(L,K,18)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,18)=WQVO(L,K,18)  
              WQVO(L,K,18) = WQVO(L,K,18)+WQV(L,K,18)  
            ENDIF
          ENDDO  
!} 
        ELSE  
          DO L=2,LA  
            WQV(L,K,18)=WQVO(L,K,18)  
            WQVO(L,K,18) = WQVO(L,K,18)+WQV(L,K,18)  
          ENDDO  
        ENDIF  
C ****  PARAM 19  
        IF(ISTRWQ(19).EQ.1)THEN  
          DO L=2,LA  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
              WQKK(L) = 1.0 / (1.0 - DTWQO2*WQP19(L))  
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
              WQRR(L) = (WQWDSL(L,K,19)+WQWPSL(L,K,19)) * VOLWQ(L)  
              XDOPSL(L,K) = XDOPSL(L,K) + WQRR(L)*DTWQ*DZC(K)*HP(L)  
              XDOALL(L,K) = XDOALL(L,K) + WQRR(L)*DTWQ*DZC(K)*HP(L)  
              IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                WQRR(L) = WQRR(L) + WQATML(L,KC,19) * VOLWQ(L)  
              ENDIF  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
C          ENDDO  
              IF(K.EQ.KC)THEN  
C            DO L=2,LA  
                WQRR(L) = WQRR(L) + WQKRDOS(L)  
C            ENDDO  
              ENDIF  
              IF(K.EQ.1)THEN  
C            DO L=2,LA  
                WQRR(L) = WQRR(L) + WQBFO2(L)*DZWQ(L)  
                XDOSOD(L,K) = XDOSOD(L,K) + WQBFO2(L)*DTWQ  
                XDOALL(L,K) = XDOALL(L,K) + WQBFO2(L)*DTWQ  
C            ENDDO  
              ENDIF  
C          DO L=2,LA  
!}
              IZ=IWQZMAP(L,K)  
              O2WQ(L) = MAX(WQVO(L,K,19), 0.0)  
              WQTTC = (1.3 - 0.3*WQPNC(L)) * WQPC(L)  
              WQTTD = (1.3 - 0.3*WQPND(L)) * WQPD(L)  
              WQTTG = (1.3 - 0.3*WQPNG(L)) * WQPG(L)  
              XDOPPB(L,K) = XDOPPB(L,K) + ( WQTTC*WQVO(L,K,1)  
     &            +WQTTD*WQVO(L,K,2)+WQTTG*WQVO(L,K,3))*WQAOCR*DTWQO2  
     &            *DZC(K)*HP(L)  
              XDOALL(L,K) = XDOALL(L,K) + ( WQTTC*WQVO(L,K,1)  
     &            +WQTTD*WQVO(L,K,2)+WQTTG*WQVO(L,K,3))*WQAOCR*DTWQO2  
     &            *DZC(K)*HP(L)  
              ! X-species
              do nsp=1,NXSP
                WQTTX(nsp)=(1.3 - 0.3*WQPNX(L,nsp)) * WQPX(L,nsp)  
                XDOPPB(L,K) = XDOPPB(L,K) + ( WQTTX(nsp)*WQVOX(L,K,nsp))
     &           *WQAOCR*DTWQO2*DZC(K)*HP(L)
                XDOALL(L,K) = XDOALL(L,K) + ( WQTTX(nsp)*WQVOX(L,K,nsp))
     &           *WQAOCR*DTWQO2*DZC(K)*HP(L)
              enddo
              XMRM = CFCDCWQ*O2WQ(L)*WQBMC(L)/(WQKHRC+O2WQ(L)+ 1.E-18)  
              WQA19C = WQTTC - XMRM  
              XDORRB(L,K) = XDORRB(L,K) - XMRM*WQVO(L,K,1)
     &           * WQAOCR*DTWQO2*DZC(K)*HP(L)  
              XDOALL(L,K) = XDOALL(L,K) - XMRM*WQVO(L,K,1)
     &           * WQAOCR*DTWQO2*DZC(K)*HP(L)  
              XMRM = CFCDDWQ*O2WQ(L)*WQBMD(L)/(WQKHRD+O2WQ(L)+ 1.E-18)  
              WQA19D = WQTTD - XMRM  
              XDORRB(L,K) = XDORRB(L,K) - XMRM*WQVO(L,K,2)
     &           * WQAOCR*DTWQO2*DZC(K)*HP(L)  
              XDOALL(L,K) = XDOALL(L,K) - XMRM*WQVO(L,K,2)
     &           * WQAOCR*DTWQO2*DZC(K)*HP(L)  
              XMRM = CFCDGWQ*O2WQ(L)*WQBMG(L)/(WQKHRG+O2WQ(L)+ 1.E-18)  
              WQA19G = WQTTG - XMRM  
              XDORRB(L,K) = XDORRB(L,K) - XMRM*WQVO(L,K,3)
     &           * WQAOCR*DTWQO2*DZC(K)*HP(L)  
              XDOALL(L,K) = XDOALL(L,K) - XMRM*WQVO(L,K,3)
     &           * WQAOCR*DTWQO2*DZC(K)*HP(L)  
              WQA19 = ( WQA19C*WQVO(L,K,1) + WQA19D*WQVO(L,K,2)  
     &          + WQA19G*WQVO(L,K,3) ) * WQAOCR  
              ! X-species
              do nsp=1,NXSP
                XMRM = CFCDWQX(nsp)*O2WQ(L)*WQBMX(L,nsp)/(WQKHRX(nsp)
     &                +O2WQ(L)+ 1.E-18)
                WQA19X = WQTTX(nsp) - XMRM  
                XDORRB(L,K) = XDORRB(L,K) - XMRM*WQVOX(L,K,nsp)
     &           * WQAOCR*DTWQO2*DZC(K)*HP(L)  
                XDOALL(L,K) = XDOALL(L,K) - XMRM*WQVOX(L,K,nsp)
     &           * WQAOCR*DTWQO2*DZC(K)*HP(L)  
                WQA19 = WQA19 + (WQA19X*WQVOX(L,K,nsp)) * WQAOCR  
              enddo
C  
C MODIFIED BY MRM 05/23/99 TO ALLOW DIFFERENT AOCR CONSTANTS TO BE APPLI  
C   TO PHOTOSYNTHESIS AND RESPIRATION TERMS FOR MACROALGAE:  
C  
              IF(IDNOTRVA.GT.0.AND.K.EQ.1)THEN  
                WQTTM = (1.3 - 0.3*WQPNM(L)) * WQPM(L)  
                XMRM=(1.0-WQFCDM)*O2WQ(L)*WQBMM(L)/(WQKHRM(IZ)+O2WQ(L)  
     &              +1.E-18)  
                WQA19A = WQTTM * WQVO(L,K,IDNOTRVA) * WQAOCRPM -  
     &            XMRM *  WQVO(L,K,IDNOTRVA) * WQAOCRRM  
                WQA19 = WQA19 + WQA19A  
                XDOPPM(L,K) = XDOPPM(L,K) +  
     &            WQTTM*WQVO(L,K,IDNOTRVA)*WQAOCRPM*DTWQO2*DZC(K)*HP(L)  
                XDOALL(L,K) = XDOALL(L,K) +  
     &            WQTTM*WQVO(L,K,IDNOTRVA)*WQAOCRPM*DTWQO2*DZC(K)*HP(L)  
                XDORRM(L,K) = XDORRM(L,K) -  
     &            XMRM*WQVO(L,K,IDNOTRVA)*WQAOCRRM*DTWQO2*DZC(K)*HP(L)  
                XDOALL(L,K) = XDOALL(L,K) -  
     &            XMRM*WQVO(L,K,IDNOTRVA)*WQAOCRRM*DTWQO2*DZC(K)*HP(L)  
              ENDIF  
              WQRR(L) = WQVO(L,K,19) + DTWQ*WQRR(L) + DTWQO2*( WQA19  
     &         -WQAOCR*WQKHR(L)*WQVO(L,K,6)-WQAONT*WQNIT(L)*WQVO(L,K,14)  
     &          + WQP19(L)*WQVO(L,K,19) ) + WQO18(L)*WQVO(L,K,18)  
              WQV(L,K,19)=SCB(L)*(WQRR(L)*WQKK(L))+(1.-SCB(L))
     &              *WQVO(L,K,19)  
              WQV(L,K,19) = MAX (WQV(L,K,19), 0.0)  
              WQVO(L,K,19) = WQVO(L,K,19)+WQV(L,K,19)  
C  
C COMPUTE AND SAVE D.O. DEFICIT:  
C  
              XMRM = WQDOS(L) - WQV(L,K,19)  
              XDODEF(L,K) = XDODEF(L,K) + XMRM*DTWQ*DZC(K)*HP(L)  
              IF(K.EQ.KC)THEN  
                XDOKAR(L,K) = XDOKAR(L,K) + WQKRDOS(L)*DTWQ*DZC(K)*HP(L)
     &            + WQP19(L)*WQVO(L,K,19)*DTWQO2*DZC(K)*HP(L)  
                XDOALL(L,K) = XDOALL(L,K) + WQKRDOS(L)*DTWQ*DZC(K)*HP(L)
     &            + WQP19(L)*WQVO(L,K,19)*DTWQO2*DZC(K)*HP(L)  
            ENDIF  
              XDODOC(L,K)=XDODOC(L,K) - WQAOCR*WQKHR(L)*WQVO(L,K,6)
     &          *DTWQO2*DZC(K)*HP(L)  
              XDOALL(L,K)=XDOALL(L,K) - WQAOCR*WQKHR(L)*WQVO(L,K,6)  
     &          *DTWQO2*DZC(K)*HP(L)  
              XDONIT(L,K)=XDONIT(L,K)-WQAONT*WQNIT(L)*WQVO(L,K,14)  
     &          *DTWQO2*DZC(K)*HP(L)  
              XDOALL(L,K)=XDOALL(L,K)-WQAONT*WQNIT(L)*WQVO(L,K,14)  
     &          *DTWQO2*DZC(K)*HP(L)  
              XDOCOD(L,K)=XDOCOD(L,K) - WQO18(L)*WQVO(L,K,18)  
     &          *DZC(K)*HP(L)  
              XDOALL(L,K)=XDOALL(L,K) - WQO18(L)*WQVO(L,K,18)  
     &          *DZC(K)*HP(L)  
              XDODZ(L,K) = XDODZ(L,K) + DZC(K)*HP(L)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
            ELSE
              WQV(L,K,19)=WQVO(L,K,19)  
              WQVO(L,K,19) = WQVO(L,K,19)+WQV(L,K,19)  
            ENDIF
          ENDDO  
!} 
        ELSE  
          DO L=2,LA  
            WQV(L,K,19)=WQVO(L,K,19)  
            WQVO(L,K,19) = WQVO(L,K,19)+WQV(L,K,19)  
          ENDDO  
        ENDIF  
C ****  PARAM 20  
        IF(ISTRWQ(20).EQ.1)THEN  
          IF(IWQSRP.EQ.1)THEN  
            DO L=2,LA  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
                WQT20 = - DTWQO2*WQWSSET(L,1)  
                WQKK(L) = 1.0 / (1.0 - WQT20)  
                WQRR(L)=WQVO(L,K,20)+DTWQ*WQR20(L)+WQT20*WQVO(L,K,20)  
C            ENDDO  
                IF(K.NE.KC)THEN  
C              DO L=2,LA  
                  WQRR(L) = WQRR(L) + DTWQO2*WQWSSET(L,2)*WQVO(L,K+1,20)  
C              ENDDO  
                ENDIF  
C            DO L=2,LA  
                WQV(L,K,20)=SCB(L)*( WQRR(L)*WQKK(L) )  
     &              +(1.-SCB(L))*WQVO(L,K,20)  
                WQVO(L,K,20) = WQVO(L,K,20)+WQV(L,K,20)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              ELSE
                WQV(L,K,20)=WQVO(L,K,20)  
                WQVO(L,K,20) = WQVO(L,K,20)+WQV(L,K,20)  
              ENDIF
            ENDDO  
!}
          ENDIF  
        ELSE  
          DO L=2,LA  
            WQV(L,K,20)=WQVO(L,K,20)  
            WQVO(L,K,20) = WQVO(L,K,20)+WQV(L,K,20)  
          ENDDO  
        ENDIF  
C  
C WQTD1FCB=1+DTWQO2*WQS21,WQTD2FCB=1/(1-DTWQO2*S21)  
C  
C ****  PARAM 21  
        IF(ISTRWQ(21).EQ.1)THEN  
          IF(IWQFCB.EQ.1)THEN  
            DO L=2,LA  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              IF(LMASKDRY(L).AND.IWQM.GE.1)THEN
!}
                WQKK(L) = WQTD2FCB(IWQT(L))  
C  
C DEFINITIONS    ATM DRY DEP       LOADS          VOLUMN  
C  
                WQR21= (WQWDSL(L,K,NWQV)+WQWPSL(L,K,NWQV))*VOLWQ(L)  
                IF(K.EQ.KC)THEN  
C  
C DEFINITIONS              ATM WET DEP      VOLUMN  
C  
                  WQR21 = WQR21 + WQATML(L,KC,21) * VOLWQ(L)  
                ENDIF  
                WQRR(L) = WQVO(L,K,NWQV)*WQTD1FCB(IWQT(L)) + DTWQ*WQR21  
                WQV(L,K,21)=SCB(L)*( WQRR(L)*WQKK(L) )  
     &              +(1.-SCB(L))*WQVO(L,K,21)  
                WQVO(L,K,21) = WQVO(L,K,21)+WQV(L,K,21)  
!{GeoSR, YSSONG, WQ WET/DRY, 110915
              ELSE
                WQV(L,K,21)=WQVO(L,K,21)  
                WQVO(L,K,21) = WQVO(L,K,21)+WQV(L,K,21)  
              ENDIF
            ENDDO  
!}
          ENDIF  
        ELSE  
          DO L=2,LA  
            WQV(L,K,21)=WQVO(L,K,21)  
            WQVO(L,K,21) = WQVO(L,K,21)+WQV(L,K,21)  
          ENDDO  
        ENDIF  
!{GeoSR, GROWTH LIMIT AND ALGAL RATE PRINT, YSSONG, 2015.12.10   
        IF(IWQTS.GE.1)THEN
          IF(ISCOMP .EQ. 3. OR. ISCOMP .EQ. 4)THEN
            TIME=DT*FLOAT(N)+TCON*TBEGIN  
            TIME=TIME/TCON 
            WRITE(FLN,"('WQRTS',I2.2,'.DAT')") K
            OPEN(3,FILE=FLN,POSITION='APPEND')
            DO M=1,IWQTS
              LL=LWQTS(M)
              WRITE(3,8999) TIME,WQPC(LL),WQBMC(LL),WQPRC(LL),WQPD(LL),
     &             WQBMD(LL),WQPRD(LL),WQPG(LL),WQBMG(LL),WQPRG(LL)
            ENDDO
            CLOSE(3)
          ENDIF            
        ENDIF
      ENDDO  
C ----------------------------------------------------------------  
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
          ! X-species
          do nsp=1,NXSP
            WQCHL(L,K) = WQCHL(L,K) + WQVX(L,K,nsp)*WQCHLX(nsp)
          enddo
        ENDDO  
      ENDDO  
      IF(IWQSRP.EQ.1)THEN  
        DO K=1,KC  
          DO L=2,LA  
            O2WQ(L) = MAX(WQV(L,K,19), 0.0)  
            WQTAMD = MIN( WQTAMDMX*EXP(-WQKDOTAM*O2WQ(L)), WQV(L,K,20) )  
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
        DO L=2,LA  
          IMWQZ = IWQZMAP(L,1)  
          WQDFBC(L) = SCB(L)*WQWSC(IMWQZ)*WQV(L,1,1)  
          WQDFBD(L) = SCB(L)*WQWSD(IMWQZ)*WQV(L,1,2)  
          WQDFBG(L) = SCB(L)*WQWSG(IMWQZ)*WQV(L,1,3)  
     &        +WQWSM*DZWQ(L)*WQV(L,1,IDNOTRVA)  
          ! X-species
          do nsp=1,NXSP
            if (IWQX(nsp).eq.1) then
              WQDFBC(L) = WQDFBC(L)
     &              + SCB(L)*WQWSX(IMWQZ,nsp)*WQVX(L,1,nsp)
            endif
            if (IWQX(nsp).eq.2) then
              WQDFBD(L) = WQDFBD(L)
     &              + SCB(L)*WQWSX(IMWQZ,nsp)*WQVX(L,1,nsp)
            endif
            if (IWQX(nsp).eq.3) then
              WQDFBG(L) = WQDFBG(L)
     &              + SCB(L)*WQWSX(IMWQZ,nsp)*WQVX(L,1,nsp)
            endif
          enddo
          WQDFRC(L) = SCB(L)*WQWSRP(IMWQZ)*WQV(L,1,4)  
          WQDFLC(L) = SCB(L)*WQWSLP(IMWQZ)*WQV(L,1,5)  
          WQDFRP(L) = SCB(L)*WQWSRP(IMWQZ)*WQV(L,1,7)  
          WQDFLP(L) = SCB(L)*WQWSLP(IMWQZ)*WQV(L,1,8)  
          WQDFRN(L) = SCB(L)*WQWSRP(IMWQZ)*WQV(L,1,11)  
          WQDFLN(L) = SCB(L)*WQWSLP(IMWQZ)*WQV(L,1,12)  
          IF(IWQSI.EQ.1) WQDFSI(L) = SCB(L)*WQWSD(IMWQZ)*WQV(L,1,16)  
        ENDDO  
        IF(IWQSRP.EQ.1)THEN  
          DO L=2,LA  
            IMWQZ = IWQZMAP(L,1)  
            WQDFLP(L) = SCB(L)*( WQDFLP(L)  
     &          + WQWSS(IMWQZ)*( WQV(L,1,10)-WQPO4D(L,1) ) )  
            IF(IWQSI.EQ.1) WQDFSI(L) = SCB(L)*( WQDFSI(L)  
     &          + WQWSS(IMWQZ)*( WQV(L,1,17)-WQSAD(L,1) ) )  
          ENDDO  
        ELSE IF(IWQSRP.EQ.2)THEN  
          DO L=2,LA  
            WQDFLP(L) = SCB(L)*( WQDFLP(L)+WSEDO(NS)*( WQV(L,1,10)  
     &          -WQPO4D(L,1) ) )  
            IF(IWQSI.EQ.1) WQDFSI(L) = SCB(L)*( WQDFSI(L)  
     &          + WSEDO(NS)*( WQV(L,1,17)-WQSAD(L,1) ) )  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C DIURNAL DO ANALYSIS  
C  
      IF(NDDOAVG.GE.1)THEN  
        OPEN(1,FILE='DIURNDO.OUT',POSITION='APPEND')  
        NDDOCNT=NDDOCNT+1  
        NSTPTMP=NDDOAVG*NTSPTC/2  
        RMULTMP=1./FLOAT(NSTPTMP)  
        DO K=1,KC  
          DO L=2,LA  
            DDOMAX(L,K)=MAX(DDOMAX(L,K),WQV(L,K,19))  
            DDOMIN(L,K)=MIN(DDOMIN(L,K),WQV(L,K,19))  
          ENDDO  
        ENDDO  
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
        DO K=1,KC  
          DO L=2,LA  
            RLIGHT1=WQKEB(IMWQZT(L))+WQKETSS*SEDT(L,K)  
            XMRM = WQKECHL*WQCHL(L,K)  
            IF(WQKECHL .LT. 0.0)THEN  
              XMRM = 0.054*WQCHL(L,K)**0.6667 + 0.0088*WQCHL(L,K)  
            ENDIF  
            RLIGHT2 = XMRM  
            RLIGHTT(L,K)=RLIGHTT(L,K)+RLIGHT1  
            RLIGHTC(L,K)=RLIGHTC(L,K)+RLIGHT1+RLIGHT2  
          ENDDO  
        ENDDO  
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
        CLOSE(1)  
      ENDIF  
!{ GEOSR STOKES : YSSONG 2015.08.18      
      do nsp=1,NXSP
        DO K=1,KC  
          DO L=2,LA  
            WQVOXB(L,K,nsp) = WQVOX(L,K,nsp) 
          ENDDO 
        ENDDO         
      enddo

      DO K=1,KC  
        DO L=2,LA  
          WQVOCB(L,K) = WQVO(L,K,1) 
        ENDDO 
      ENDDO               
      
      if (NXSP.gt.0) then !{ GEOSR X-species : jgcho 2015.10.15
        IF(ISSTOKEX(1).EQ.1)THEN
          do i=1,IWQTS
            WRITE(FLN,"('STOKE',I2.2,'.OUT')") i
            OPEN(1,FILE=trim(FLN),POSITION='APPEND')      ! VERTICAL VELOCITY, ALGAL-DENSITY, SOLAR RADIATION, chl-a PRINT AT EACH LAYER
            write(1,1114) TIMTMP
     & ,((WQALSETX(LWQTS(i),k,nsp),nsp=1,NXSP),k=kc,1,-1)
     & ,((WQRHOX(LWQTS(i),k,nsp),nsp=1,NXSP),k=kc,1,-1)
     & ,((WQSOLDAX(LWQTS(i),k,nsp),nsp=1,NXSP),k=kc,1,-1)
     & ,(WQCHL(LWQTS(i),k),k=kc,1,-1)
            close(1)
          enddo
        ENDIF 
      endif !if (NXSP.gt.0) then !{ GEOSR X-species : jgcho 2015.10.15
 1114 FORMAT(F12.6,(E12.4))  
 1111 FORMAT(I12,F10.4)  
 1112 FORMAT(2I5,12F7.2)  
 1113 FORMAT(2I5,12E12.4)  
 8999 FORMAT(F10.5,9E12.4)   
      RETURN  
      END  
