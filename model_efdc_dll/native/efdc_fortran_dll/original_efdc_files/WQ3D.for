      SUBROUTINE WQ3D(ISTL_,IS2TL_)  
C  
C  CONTROL SUBROUTINE FOR WATER QUALITY MODEL  
C  ORGINALLY CODED BY K.-Y. PARK  
C  OPTIMIZED AND MODIFIED BY J. M. HAMRICK  
C CHANGE RECORD  
C  
C     Merged SNL and DS-INTL
      USE GLOBAL  

      REAL TTMP, SECNDS
      REAL, SAVE :: DAYNEXT
      REAL, SAVE :: SUNDAY1, SUNDAY2, SUNSOL1, SUNSOL2
      REAL, SAVE :: SUNFRC1, SUNFRC2
!{ GeoSR, YSSONG. 2012/12/15, RESTART
      REAL, SAVE :: SUNFRC0, SUNSOL0, SUNFRC11, SUNSOL11
      REAL, SAVE :: SUNFRC01,SUNSOL02,WQI0OPT0
      REAL, SAVE :: SUNFRC22,SUNSOL22,SUNFRC33, SUNSOL33
      INTEGER ISUNDAY2,IDAYNEXT
!} GeoSR
      INTEGER*4, SAVE :: M
      
      DATA IWQTICI,IWQTAGR,IWQTSTL,IWQTSUN,IWQTBEN,IWQTPSL,IWQTNPL/7*0/  
      DATA ISMTICI/0/  
      IF(ETIMEDAY.LE.(DTWQ+1.E-8))THEN
        DAYNEXT=FLOAT(INT(TIMEDAY))+1.
!{ GeoSR, YSSONG. 2012/12/15, RESTART
        ISUNDAY2=0
        IDAYNEXT=0
        OPEN(1234,FILE='SOL.DAT')
!} GeoSR
      ENDIF

      ! *** PMC - NEW IMPLEMENTATION TO USE DAILY (FROM HOURLY) SOLAR RADIATION FOR ALGAL GROWTH
      IF(ITNWQ.EQ.0.AND.IWQSUN.GT.1.AND.NASER.GT.0)THEN
        ! *** BUILD THE DAILY AVERAGE SOLAR RADIATION FROM THE ASER DATA
!{ GeoSR, YSSONG. 2012/12/15, RESTART
        !        SUNDAY1 = TIMEDAY+0.5
        SUNDAY1 = FLOAT(INT(TIMEDAY))-0.5
!        SUNDAY2 = DAYNEXT+0.5
        SUNDAY2 = DAYNEXT-0.5
        
        ! *** FIND 1ST POINT
        M = 1
!        DO WHILE (TASER(M,1).LT.SUNDAY1-0.5)
        DO WHILE (TASER(M,1).LT.SUNDAY2-0.5)
          M = M+1
        END DO
!} GeoSR
        
        ! *** BUILD THE AVERAGE DAILY SOLAR RADIATION        
        M1 = 0
        M2 = 0
        SUNSOL1 = 0.0
        DO WHILE (TASER(M,1).LT.
     +      min(SUNDAY2+0.5,TASER(ubound(TASER,1),1)))
          M1 = M1+1
          IF(SOLSWR(M,1).GT.0.)THEN
            M2 = M2+1
            SUNSOL1=SUNSOL1+SOLSWR(M,1)
          ENDIF  
          M = M+1
        END DO
        IF(M1.GT.0)THEN
          SUNFRC1=FLOAT(M2)/FLOAT(M1)
          SUNSOL1=SUNSOL1/FLOAT(M1)
        ELSE
          SUNFRC1=1.0
        ENDIF
!{ Geosr, jgcho, 2015.5.29 solswr
        IF (M.ge.ubound(TASER,1))  then
          SUNSOL2=SUNSOL1
          SUNFRC2=SUNFRC1
        ELSE ! IF (M.gt.ubound(TASER,1)) then
          ! *** BUILD THE AVERAGE DAILY SOLAR RADIATION        
          M1 = 0
          M2 = 0
          SUNSOL2 = 0.
          DO WHILE (TASER(M,1).LT.
     +        min(SUNDAY2+1.5,TASER(ubound(TASER,1),1)))
            M1 = M1+1
            IF(SOLSWR(M,1).GT.0.)THEN
              M2 = M2+1
              SUNSOL2=SUNSOL2+SOLSWR(M,1)
            ENDIF
            M = M+1
          END DO
          IF(M1.GT.0)THEN
            SUNFRC2=FLOAT(M2)/FLOAT(M1)
            SUNSOL2=SUNSOL2/FLOAT(M1)
          ELSE
            SUNFRC2=1.
          ENDIF
        ENDIF ! IF (M.gt.ubound(TASER,1)) then
!} Geosr, jgcho, 2015.5.29 solswr
      ENDIF
!{ GeoSR, YSSONG. 2012/12/15, RESTART
      IF(ITNWQ.EQ.0)THEN
!        IF(ISUNDAY2.EQ.0)THEN
        IF(IWQSUN.GT.1.AND.NASER.GT.0)THEN
         DO NDUM=2,4
           M = 1
           DO WHILE (TASER(M,1).LT.DAYNEXT-(FLOAT(NDUM)))
             M = M+1
           END DO
           M1 = 0
           M2 = 0
           SUNSOL0 = 0.
!{ Geosr, jgcho, 2015.5.29 solswr
           IF(TASER(M,1).LE.DAYNEXT-(FLOAT(NDUM))) Then
!           DO WHILE (TASER(M,1).LT.SUNDAY2-0.5)
             DO WHILE (TASER(M,1).LT.DAYNEXT-(FLOAT(NDUM))+1.0)
               IF(TASER(M,1).GE.DAYNEXT-(FLOAT(NDUM)))THEN
                 M1 = M1+1
                 IF(SOLSWR(M,1).GT.0.)THEN
                   M2 = M2+1
                   SUNSOL0=SUNSOL0+SOLSWR(M,1)    !!! 1 day average 
                 ENDIF
                 M = M+1
               ENDIF
             END DO
             IF(M1.GT.0)THEN
               SUNFRC0=FLOAT(M2)/FLOAT(M1)
               SUNSOL0=SUNSOL0/FLOAT(M1)        !!! avg SUNSOL for timeday
             ELSE
               SUNFRC0=1.0
             ENDIF
           ELSE ! IF(TASER(M,1).LE.DAYNEXT-(FLOAT(NDUM))) Then
             SUNFRC0=SUNFRC1
             SUNSOL0=SUNSOL1
           ENDIF ! IF(TASER(M,1).LE.DAYNEXT-(FLOAT(NDUM))) Then
!} Geosr, jgcho, 2015.5.29 solswr
           IF(NDUM.EQ.2)THEN     ! PREVIOUS DAY
             SUNSOL11=SUNSOL0
             SUNFRC11=SUNFRC0
           ELSEIF(NDUM.EQ.3)THEN ! TWO DAYS AGO 
             SUNSOL22=SUNSOL0
             SUNFRC22=SUNFRC0
           ELSEIF(NDUM.EQ.4)THEN ! THREE DAYS AGO 
             SUNSOL33=SUNSOL0
             SUNFRC33=SUNFRC0
           ENDIF
         END DO
        ENDIF 
!        ENDIF 

!        IF(IDAYNEXT.EQ.0)THEN
        IF(IWQSUN.GT.1.AND.NASER.GT.0)THEN      
         DO NDUM=2,4
           IF(NDUM.EQ.2)THEN     ! PREVIOUS DAY
             SUNSOL01=SUNSOL11
             SUNFRC01=SUNFRC11
             SUNSOL02=SUNSOL1
             SUNFRC02=SUNFRC1
           ELSEIF(NDUM.EQ.3)THEN ! TWO DAYS AGO 
             SUNSOL01=SUNSOL22
             SUNFRC01=SUNFRC22
             SUNSOL02=SUNSOL11
             SUNFRC02=SUNFRC11
           ELSEIF(NDUM.EQ.4)THEN ! THREE DAYS AGO 
             SUNSOL01=SUNSOL33
             SUNFRC01=SUNFRC33
             SUNSOL02=SUNSOL22
             SUNFRC02=SUNFRC22
           ENDIF
           IF(IWQSUN.GT.1)THEN
             RATIO = (TIMEDAY-SUNDAY1)
             SOLARAVG = RATIO*(SUNSOL02-SUNSOL01)+SUNSOL01
             WQFD=RATIO*(SUNFRC02-SUNFRC01)+SUNFRC01
              ! *** SOLAR RADIATION IN LANGLEYS/DAY
             WQI0 = PARADJ*2.065*SOLARAVG
             IF(IWQSUN.EQ.2)THEN
               ! *** OPTIMAL SOLAR RADIATION IS ALWAYS UPDATED BASED ON DAY AVERAGED
               WQI0OPT0 = MAX(WQI0OPT0, WQI0/(WQFD+1.E-18)*0.85) 
                IF(NASER.GT.1.OR.USESHADE)THEN  
                 SOLARAVG=0.
                 DO L=2,LA  
                   SOLARAVG=SOLARAVG+SOLSWRT(L)  
                 ENDDO  
                   SOLARAVG=SOLARAVG/FLOAT(LA-1)
                 ELSE
                 ! *** Spatially Constant Atmospheric Parameters
                   SOLARAVG=SOLSWRT(2)
                 ENDIF  
               ! *** SOLAR RADIATION IN LANGLEYS/DAY
               WQI0 = PARADJ*2.065*SOLARAVG  
               WQFD=1.  
             ELSEIF(IWQSUN.GT.2)THEN
               ! *** OPTIMAL SOLAR RADIATION IS ALWAYS UPDATED BASED ON DAY AVERAGED
               WQI0OPT0 = MAX(WQI0OPT0, WQI0)  
             ENDIF
             IF(NDUM.EQ.2)THEN     ! PREVIOUS DAY
               WQI1=WQI0OPT0 
             ELSEIF(NDUM.EQ.3)THEN ! TWO DAYS AGO 
               WQI2=WQI0OPT0 
             ELSEIF(NDUM.EQ.4)THEN ! THREE DAYS AGO 
               WQI3=WQI0OPT0 
             ENDIF
             IF(IWQSUN.GT.0) WQI0OPT0 = 0.0
           ENDIF
         END DO
        ENDIF
!        ENDIF
      ENDIF      
!} GeoSR, 2012/12/15

C  
C **  READ INITIAL CONDITIONS  
C  
      IF(IWQICI.EQ.1) CALL RWQICI 
      IF(ISDYNSTP.EQ.0)THEN  
        TIMTMP=(DT*FLOAT(N)+TCON*TBEGIN)/86400.  
      ELSE  
        TIMTMP=TIMESEC/86400.  
      ENDIF
C  
C **  READ TIME/SPACE VARYING ALGAE PARAMETERS  
C  
      IF(IWQAGR.EQ.1) THEN
        IF(TIMTMP .GE. AGRDAY) CALL RWQAGR(TIMTMP)  
      ENDIF
C  
C **  READ TIME/SPACE VARYING SETTLING VELOCITIES  
C  
      IF(IWQSTL.EQ.1) THEN
        IF(TIMTMP .GE. STLDAY) CALL RWQSTL(TIMTMP)  
      ENDIF  
C  
C *** READ BENTHIC FLUX IF REQUIRED  
C *** CALL SPATIALLY AND TIME VARYING BENTHIC FLUX HERE.  ONLY CALL RWQBEN2  
C *** IF SIMULATION TIME IS >= THE NEXT TIME IN THE BENTHIC FILE.  
C  
      IF(IWQBEN .EQ. 2)THEN 
        IF(TIMTMP .GE. BENDAY)THEN  
          CALL RWQBEN2(TIMTMP)  
        ENDIF  
      ENDIF  
C  
C **  UPDATE POINT SOURCE LOADINGS  
C  
      IF(IWQPSL.EQ.1)THEN
        CALL RWQPSL  
      ELSEIF(IWQPSL.EQ.2) THEN
        CALL CALCSER(ISTL_)
      ENDIF
C
      CALL RWQATM  
C  
C **  READ SEDIMENT MODEL INITIAL CONDITION  
C  
      IF(IWQBEN.EQ.1)THEN  
        IF(ISMICI.EQ.1 .AND. ITNWQ.EQ.ISMTICI) CALL RSMICI(ISMTICI)  
      ENDIF  
C  
C **  UPDATE OLD CONCENTRATIONS  
C   FOLLOWING THE CALL TO CALWQC MINUS OLD D.O. BEFORE THE CALL).  
C   FIRST SUBTRACT THE OLD D.O. HERE:  
C  
      IF(ISMTSB.LT.ISMTSE)THEN  
        DO K=1,KC  
          DO L=2,LA  
            XMRM = WQV(L,K,19)*DTWQ*DZC(K)*HP(L)  
            XDOTRN(L,K) = XDOTRN(L,K) - XMRM  
            XDOALL(L,K) = XDOALL(L,K) - XMRM  
          ENDDO  
        ENDDO  
      ENDIF
C  
C **  CALCULATE PHYSICAL TRANSPORT  
C **  WQV(L,K,NW) SENT TO PHYSICAL TRANSPORT AND TRANSPORTED  
C **  VALUE RETURNED IN WQV(L,K,NW)  
C 
      CALL CALWQC(ISTL_,IS2TL_) !transports (advects/disperses) WQV
C  
C   FOLLOWING THE CALL TO CALWQC MINUS OLD D.O. BEFORE THE CALL).  
C   NOW ADD THE NEW D.O. HERE:  
C  
      IF(ISMTSB.LT.ISMTSE)THEN  
        DO K=1,KC  
          DO L=2,LA  
            XMRM = WQV(L,K,19)*DTWQ*DZC(K)*HP(L)  
            XDOTRN(L,K) = XDOTRN(L,K) + XMRM  
            XDOALL(L,K) = XDOALL(L,K) + XMRM  
          ENDDO  
        ENDDO  
      ENDIF
C  
C **  UPDATE WATER COLUMN KINETICS AND SEDIMENT MODEL  
C **  OVER LONGER TIME INTERVALS THAN PHYSICAL TRANSPORT  
C **  IF NWQKDPT .GT. 1  
C 
      NWQKCNT=NWQKCNT+1  
      IF(ITNWQ.EQ.0.OR.NWQKCNT.EQ.NWQKDPT)THEN  
        !IF(ITNWQ.NE.0)NWQKCNT=0   PMC
        NWQKCNT=0
        ! **  UPDATE SOLAR RADIATION INTENSITY  
        !   WQI1 = SOLAR RADIATION ON PREVIOUS DAY  
        !   WQI2 = SOLAR RADIATION TWO DAYS AGO  
        !   WQI3 = SOLAR RADIATION THREE DAYS AGO  
        ! ***  UPDATE OCCURS ONLY WHEN THE SIMULATION DAY CHANGES.  
        IF(TIMEDAY.GT.DAYNEXT)THEN  ! *** DSLLC: FORCE A SOLAR DAY UPDATE
!{ GeoSR : 2012/12/15  SOLAR RADIATION FOR RESTART
          IDAYNEXT=1
!} GeoSR :  2012/12/15
          WQI3 = WQI2  
          WQI2 = WQI1  
          WQI1 = WQI0OPT  
          IF(IWQSUN.GT.0)WQI0OPT = 0.0  
          DAYNEXT=DAYNEXT+1.
        ENDIF
        
        IF(IWQSUN.GT.1)THEN  
          IF(TIMEDAY.GT.SUNDAY2)THEN
!{ GeoSR : 2012/12/15  SOLAR RADIATION FOR RESTART
          ISUNDAY2=1
!} GeoSR :  2012/12/15
            ! *** BUILD THE DAILY AVERAGE SOLAR RADIATION FROM THE ASER DATA
            SUNDAY1 = SUNDAY2
            SUNSOL1 = SUNSOL2
            SUNFRC1 = SUNFRC2
!{ Geosr, jgcho, 2015.5.29 solswr
            ! *** FIND 1ST POINT
            M = 1
            DO WHILE (TASER(M,1).LT.(SUNDAY2+0.5-EPS))
              M = M+1
            END DO
            SUNDAY2 = SUNDAY2+1
            ! If date for next day is not provided use values of today
            IF( M.ge.ubound(TASER,1) ) then
              SUNSOL2=SUNSOL1
              SUNFRC2=SUNFRC1
            ELSE ! IF (M.gt.ubound(TASER,1)) then
              ! *** BUILD THE AVERAGE DAILY SOLAR RADIATION        
              M1 = 0
              M2 = 0
              SUNSOL2 = 0.
              DO WHILE (TASER(M,1).LT.
     +            min(SUNDAY2+0.5-EPS, TASER(ubound(TASER,1),1)) )
                M1 = M1+1
                IF(SOLSWR(M,1).GT.0.)THEN
                  M2 = M2+1
                  SUNSOL2=SUNSOL2+SOLSWR(M,1)
                ENDIF
                M = M+1
              END DO
              IF(M1.GT.0)THEN
                SUNFRC2=FLOAT(M2)/FLOAT(M1)
                SUNSOL2=SUNSOL2/FLOAT(M1)
              ELSE
                SUNFRC2=1.
              ENDIF
            ENDIF
!} Geosr, jgcho, 2015.5.29 solswr
          ENDIF
        ENDIF  
  
        ! **  READ SOLAR RADIATION INTENSITY AND DAYLIGHT LENGTH  
        ! NOTE: IWQSUN=1 CALLS SUBROUTINE RWQSUN WHICH READS THE DAILY  
        !                SOLAR RADIATION DATA FROM FILE SUNDAY.INP WHICH  
        !                ARE IN UNITS OF LANGLEYS/DAY.  
        !       IWQSUN=2 USES THE HOURLY SOLAR RADIATION DATA FROM ASER.INP  
        !                COUPLED WITH THE COMPUTED OPTIMAL DAILY LIGHT TO
        !                LIMIT ALGAL GROWTH.
        !       IWQSUN=3 USES THE DAILY AVERAGE SOLAR RADIATION DATA COMPUTED 
        !                FROM THE HOURLY ASER.INP AND THE COMPUTED OPTIMAL DAILY
        !                LIGHT TO LIMIT ALGAL GROWTH.
        !       IWQSUN>1 USES THE DAILY AVERAGE SOLAR RADIATION DATA COMPUTED 
        !                FROM THE HOURLY ASER.INP DATA.  CONVERTS WATTS/M**2 TO
        !                LANGLEYS/DAY USING 2.065.  COMPUTES THE FRACTION OF
        !                DAYLIGHT AND ADJUSTS FOR PHOTOSYNTHETIC ACTIVE RADIATION BY 
        !                PARADJ (~0.43) 
        !  
        IF(IWQSUN.EQ.0)THEN
          WQI0OPT = WQI0
        ELSEIF(IWQSUN.EQ.1)THEN  
          CALL RWQSUN  
          WQI0=SOLSRDT  
          WQFD=SOLFRDT  
          ! *** OPTIMAL SOLAR RADIATION IS ALWAYS UPDATED BASED ON DAY AVERAGED
          WQI0OPT = MAX(WQI0OPT, WQI0)
        ELSEIF(IWQSUN.GT.1)THEN
          RATIO = (TIMEDAY-SUNDAY1)
          SOLARAVG = RATIO*(SUNSOL2-SUNSOL1)+SUNSOL1
          WQFD=RATIO*(SUNFRC2-SUNFRC1)+SUNFRC1

          ! *** SOLAR RADIATION IN LANGLEYS/DAY
          WQI0 = PARADJ*2.065*SOLARAVG  

          IF(IWQSUN.EQ.2)THEN
            ! *** OPTIMAL SOLAR RADIATION IS ALWAYS UPDATED BASED ON DAY AVERAGED
            WQI0OPT = MAX(WQI0OPT, WQI0/(WQFD+1.E-18)*0.85) 

            IF(NASER.GT.1.OR.USESHADE)THEN  
              SOLARAVG=0.  
              DO L=2,LA  
                SOLARAVG=SOLARAVG+SOLSWRT(L)  
              ENDDO  
              SOLARAVG=SOLARAVG/FLOAT(LA-1)
            ELSE
              ! *** Spatially Constant Atmospheric Parameters
              SOLARAVG=SOLSWRT(2)
            ENDIF  
            ! *** SOLAR RADIATION IN LANGLEYS/DAY
            WQI0 = PARADJ*2.065*SOLARAVG  
            WQFD=1.  
          ELSE
            ! *** OPTIMAL SOLAR RADIATION IS ALWAYS UPDATED BASED ON DAY AVERAGED
            WQI0OPT = MAX(WQI0OPT, WQI0)  
          ENDIF
        ENDIF  
C  
C **  LOAD WQV INTO WQVO FOR REACTION CALCULATION  
C  
        NMALG=0  
        IF(IDNOTRVA.GT.0) NMALG=1  
        DO NW=1,NWQV+NMALG  
          IF(ISTRWQ(NW).NE.0)THEN  
            DO K=1,KC  
              DO L=2,LA  
                WQVO(L,K,NW)=WQV(L,K,NW)  
              ENDDO  
            ENDDO
          ENDIF  
        ENDDO  
        ! Also consider x-species
        DO NW=1,NXSP
          DO K=1,KC  
            DO L=2,LA  
              WQVOX(L,K,NW)=WQVX(L,K,NW)  
            ENDDO  
          ENDDO
        ENDDO  
C  
C **    CALCULATE KINETIC SOURCES AND SINKS  
C  

![ GeoSR : 2012/12/15 
        WRITE(1234,*) TIMEDAY,DAYNEXT,WQI1,WQI2,WQI3
! GeoSR :  2012/12/15]
        
        TTMP=SECNDS(0.0)  
        IF(ISWQLVL.EQ.0) CALL WQSKE0  
        IF(ISWQLVL.EQ.1) CALL WQSKE1  
        IF(ISWQLVL.EQ.2) CALL WQSKE2  
        IF(ISWQLVL.EQ.3) CALL WQSKE3  
        IF(ISWQLVL.EQ.4) CALL WQSKE4  
        TWQKIN=TWQKIN+SECNDS(TTMP)  
C  
C **    DIAGNOSE NEGATIVE CONCENTRATIONS  
C  
        IF(IWQNC.EQ.1)CALL WWQNC  
C  
C **    WRITE TIME SERIES  
C  
        IF(ITNWQ.GE.IWQTSB .AND. ITNWQ.LE.IWQTSE.AND.IWQTSE.GT.0)THEN  
          IF(MOD(ITNWQ,IWQTSDT).EQ.0) CALL WWQTS
C  
           CALL WWQTSBIN  !{GeoSR, 2014.10.13 JHLEE, GROWTH LIMIT PRINT
C  
        ENDIF  
c       CALL WWQTSBIN   !{GeoSR, 2014.10.13 JHLEE, GROWTH LIMIT PRINT
C  
C **    CALL SEDIMENT DIAGENSIS MODEL  
C  
        IF(IWQBEN.EQ.1)THEN  
          TTMP=SECNDS(0.0)  
          CALL SMMBE  
          TWQSED=TWQSED+SECNDS(TTMP)  
          IF(ISMTS.GE.1)THEN  
C  
C **      WRITE SEDIMENT MODEL TIME SERIES  
C  
            IF(ITNWQ.GE.ISMTSB .AND. ITNWQ.LE.ISMTSE)THEN  
              IF(MOD(ITNWQ,ISMTSDT).EQ.0) CALL WSMTS
            ENDIF  
          ENDIF  
C  
C **      WRITE SEDIMENT MODEL FLUXES TO BINARY FILE:  
C  
          IF(ITNWQ.GE.ISMTSB .AND. ITNWQ.LE.ISMTSE)THEN  
            CALL WSMTSBIN  
          ENDIF  
        ENDIF  
      ENDIF  
C  
C **  UPDATE TIME IN DAYS  
C  
![ GeoSR : 2010/07/27
c      ITNWQ = ITNWQ + 2  
      ITNWQ = ITNWQ + 1
! GeoSR : 2010/07/27]
C  
C **  ENDIF ON KINETIC AND SEDIMENT UPDATE  
C **  INSERT TIME CALL  
C **  WRITE RESTART FILES  
C  
      RETURN  
      END  

