      SUBROUTINE Sub_SPORE(TIMTMP)
C  
C GeoSR, JHLEE 151012: CHANGED CODE TO ALLOW FOR TEMPORALLY  
C READ IN SPATIALLY AND/OR TEMPORALLY VARYING PARAMETERS FOR BENTHIC  
C FLUXES OF Cell number of cyanobacteria
C  
      USE GLOBAL
      
      INTEGER    ICYAM
      
!      ITM=(NAT*3600)/(DT*NWQKDPT)
      
      NNN=NNN+1
           
      IF(ISDYNSTP.EQ.0)THEN  
        TIMTMP=(DT*FLOAT(N)+TCON*TBEGIN)/86400.  
      ELSE  
        TIMTMP=TIMESEC/86400.  
      ENDIF  

      DO L=2,LA

C    Solaradiation
        CYA_Light=CYA_Light*Light_Factor2
        WQ_Light_DEP(L)=(WQHT(1)+DZC(1)/2.)*HP(L)
        WQ_Light(L)=SOLSWRT(L)*EXP(-WQKESS1*WQ_Light_DEP(L))
        
C    Average temperature
        IF(NNAT.GT.0) THEN
          IF(NNN.LE.NNAT) THEN
            IF(NNN.EQ.1) THEN
              TEMAVG0(L,NNN)=TEM(L,1)
              TEMAVG(L)=TEMAVG0(L,NNN)
              LightAVG0(L,NNN)=WQ_Light(L)
              LightAVG(L)=LightAVG0(L,NNN)
            ELSEIF(NNN.GT.1) THEN
              TEMAVG0(L,NNN)=TEM(L,1)
              TEMAVG(L)=(TEMAVG(L)+TEMAVG0(L,NNN))/2.
              LightAVG0(L,NNN)=WQ_Light(L)
              LightAVG(L)=(LightAVG(L)+LightAVG0(L,NNN))/2.
            ENDIF  
          ELSE
            TEMAVG1(L)=0.
            LightAVG1(L)=0.
            DO I=1,NNAT-1
              TEMAVG0(L,I)=TEMAVG0(L,I+1)
              TEMAVG1(L)=TEMAVG1(L)+TEMAVG0(L,I)
              LightAVG0(L,I)=LightAVG0(L,I+1)
              LightAVG1(L)=LightAVG1(L)+LightAVG0(L,I)
            ENDDO
            TEMAVG0(L,NNAT)=TEM(L,1)
            TEMAVG(L)=(TEMAVG1(L)+TEMAVG0(L,I))/NNAT
            LightAVG0(L,NNAT)=WQ_Light(L)
            LightAVG(L)=(LightAVG1(L)+LightAVG0(L,I))/NNAT
          ENDIF
        ELSE
          TEMAVG(L)=TEM(L,1)
          LightAVG(L)=WQ_Light(L)
        ENDIF
        
        ICYAM = ICYAMAP(L)
        GER0(L) = CUM_GER(L)
        IF(TEMAVG(L).GE.CYA_TEM.AND.WQV(L,1,10).GE.CYA_P4D.AND.
     &     WQV(L,1,15).GE.CYA_NO3.AND.LightAVG(L).GE.CYA_Light) THEN
          TIME_NUM(L)=TIME_NUM(L)+1
          IF(TIME_NUM(L).EQ.1) THEN
            CYA_TIME(L)=TIMTMP
          ENDIF
        ENDIF  

C CUMULATIVE GERMINATION RATE
        IF(TIMTMP.GE.CYA_TIME(L).AND.TIMTMP.LE.CYA_TIME(L)+DGTIME) THEN
          GER_DAY(L) = TIMTMP - CYA_TIME(L)
          CUM_GER(L) = 100.*GER_DAY(L)/(KCG+GER_DAY(L))
          GER(L) = CUM_GER(L) - GER0(L)
          
          CYA_ADD(L) = NUM_CELL(ICYAM)*GER(L)/100.*CONCYA*1.E-3/
     &                 WQCHLC*TGERMI/100.                              
        ELSE
          CYA_ADD(L)=0.
          GER_DAY(L)=0.
          CUM_GER(L)=0.
          CYA_TIME(L)=0.
          GER(L)=0.
          GER0(L)=0.
        ENDIF
      ENDDO
      
      CLOSE(1)
C  
      RETURN
      END

