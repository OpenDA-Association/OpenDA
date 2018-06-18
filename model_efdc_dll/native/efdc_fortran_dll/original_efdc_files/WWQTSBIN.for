C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C
      SUBROUTINE WWQTSBIN
C
C**********************************************************************C
C WRITE TIME-SERIES OUTPUT: WQCHLX=1/WQCHLX TO BINARY FILE
C**********************************************************************C
C
C M. MORTON  28 JUN 1998
C AVERAGES WQ VARIABLES OVER IWQTSDT TIME STEPS (E.G., DAILY AVERAGES).
C ALSO, DETERMINES MAXIMUM AND MINIMUM OVER IWQTSDT TIME STEPS.
C AVERAGED DIURNAL DO VARIABLES OVER IWQDIUDT TIME STEPS
C
C REVISIONS:
C  23 JUL 98 MRM: ADDED BINARY FILE WRITE TO DUMP ALL CELLS TO OUTPUT
C     FILE.  SEE SUBROUTINE INITBIN FOR HEADER RECORD INFORMATION.
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
C **  SHEN'S MODIFICATION TO OUTPUT MACROALGAE
C**********************************************************************C
C
C THESE ARE THE INDEX NUMBERS OF THE PARAMETERS IN WQVO ARRAY:
C WQVO(LL,K, 1) = CYANOBACTERIA AS C  WQVO(LL,K,12) = LABILE PON
C WQVO(LL,K, 2) = DIATOMS AS C        WQVO(LL,K,13) = DISS. ORG. NITROGEN
C WQVO(LL,K, 3) = GREEN ALGAE AS C    WQVO(LL,K,14) = AMMONIA NITROGEN
C WQVO(LL,K, 4) = REFRACTORY POC      WQVO(LL,K,15) = NITRATE NITROGEN
C WQVO(LL,K, 5) = LABILE POC          WQVO(LL,K,16) = PART. BIOGENIC SILICA
C WQVO(LL,K, 6) = DISS. ORG. CARBON   WQVO(LL,K,17) = AVAILABLE SILICA
C WQVO(LL,K, 7) = REFRACTORY POP      WQVO(LL,K,18) = COD
C WQVO(LL,K, 8) = LABILE POP          WQVO(LL,K,19) = DISSOLVED OXYGEN
C WQVO(LL,K, 9) = DISS. ORG. PHOS.    WQVO(LL,K,20) = TOTAL ACTIVE METAL
C WQVO(LL,K,10) = TOT. INORG. PHOS.   WQVO(LL,K,21) = FECAL COLIFORM BACTERIA
C WQVO(LL,K,11) = REFRACTORY PON      WQVO(LL,K,22) = MACROALGAE
C
      USE GLOBAL
C
      LOGICAL ISASCII, IS2OPEN
C
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::TNWQMAX
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::TNWQMIN
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::TNWQSUM
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::TOCWQMAX
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::TOCWQMIN
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::TOCWQSUM
C
      ! GeoSR, GROWTH LIMIT AND ALGAL RATE PRINT, YSSONG, 2015.12.10    
      CHARACTER*11 FLN
      CHARACTER*12 FLNX
      IF(.NOT.ALLOCATED(TNWQMAX))THEN
        ALLOCATE(TNWQMAX(LCMWQ,KCM))  
        ALLOCATE(TNWQMIN(LCMWQ,KCM))  
        ALLOCATE(TNWQSUM(LCMWQ,KCM))  
        ALLOCATE(TOCWQMAX(LCMWQ,KCM))  
        ALLOCATE(TOCWQMIN(LCMWQ,KCM))  
        ALLOCATE(TOCWQSUM(LCMWQ,KCM))  

        TNWQMAX=0.0
        TNWQMIN=9.99E+21
        TNWQSUM=0.0
        TOCWQMAX=0.0
        TOCWQMIN=9.99E+21
        TOCWQSUM=0.0
      ENDIF
C
      IF(ISFIRST .EQ. 0)THEN
        CALL INITBIN
        CALL INITBIN2
        ISFIRST = 1
      ENDIF
C
      IF(ISDYNSTP.EQ.0)THEN
        TIMTMP=DT*FLOAT(N)+TCON*TBEGIN
        TIMTMP=TIMTMP/TCTMSR  
      ELSE
        TIMTMP=TIMESEC/TCTMSR  
      ENDIF
C
C      DO M=1,IWQTS
      DO LL=2,LA
        DO K=1,KC
C          LL=LWQTS(M)
          DO NW=1,NWQV
            IF(ISTRWQ(NW).NE.0)THEN  
              IF(ISWQLVL.EQ.1)THEN  ! PMC - CHANGE ALL WQSKE SUBROUTINES IN FUTURE
                WQVO(LL,K,NW) = (WQVO(LL,K,NW)+WQV(LL,K,NW))*0.5
              ELSE
                WQVO(LL,K,NW) = WQVO(LL,K,NW)*0.5
              ENDIF
            ENDIF
            WQVSUM(LL,K,NW) = WQVSUM(LL,K,NW) + WQVO(LL,K,NW)
            IF(WQVO(LL,K,NW) .LT. WQVMIN(LL,K,NW))THEN
              WQVMIN(LL,K,NW) = WQVO(LL,K,NW)
            ENDIF
            IF(WQVO(LL,K,NW) .GT. WQVMAX(LL,K,NW))THEN
              WQVMAX(LL,K,NW) = WQVO(LL,K,NW)
            ENDIF
          ENDDO
          TBWQ = WQVO(LL,K,1)+WQVO(LL,K,2)+WQVO(LL,K,3)
          XMRM =  WQVO(LL,K,4) + WQVO(LL,K,5) + WQVO(LL,K,6) + TBWQ
          TOCWQSUM(LL,K) = TOCWQSUM(LL,K) + XMRM
          IF(XMRM .GT. TOCWQMAX(LL,K)) TOCWQMAX(LL,K) = XMRM
          IF(XMRM .LT. TOCWQMIN(LL,K)) TOCWQMIN(LL,K) = XMRM
          XMRM = WQVO(LL,K,4) + WQVO(LL,K,5) + TBWQ
          POCSUM(LL,K) = POCSUM(LL,K) + XMRM
          IF(XMRM .GT. POCMAX(LL,K)) POCMAX(LL,K) = XMRM
          IF(XMRM .LT. POCMIN(LL,K)) POCMIN(LL,K) = XMRM
          IF(IDNOTRVA .NE. 0)THEN
C
C HP(LL)= TOTAL WATER DEPTH AT CELL LL
C CHLM  = MACROALGAE BIOMASS IN MICROGRAMS/SQUARE METER:
C            XMRM = WQVO(LL,K,NWQV+1) * WQCHLM * HP(LL)
C CHLM = MACROALGAE BIOMASS IN UG/L:
            XMRM = WQVO(LL,K,NWQV+1) * WQCHLM
            CHLMSUM(LL,K) = CHLMSUM(LL,K) + XMRM
            IF(XMRM .GT. CHLMMAX(LL,K)) CHLMMAX(LL,K) = XMRM
            IF(XMRM .LT. CHLMMIN(LL,K)) CHLMMIN(LL,K) = XMRM
          ENDIF
          PO4DWQ_ = 0.0
          IF(IWQSRP.EQ.1)THEN
            O2WQ_ = MAX(WQVO(LL,K,19), 0.0)
            TAMDWQ = MIN( WQTAMDMX*EXP(-WQKDOTAM*O2WQ_), WQVO(LL,K,20) )
            TAMPWQ = WQVO(LL,K,20) - TAMDWQ
            PO4DWQ_ = WQVO(LL,K,10) / (1.0 + WQKPO4P*TAMPWQ)
            PO4DWQSUM(LL,K) = PO4DWQSUM(LL,K) + PO4DWQ_
            XMRM = WQVO(LL,K,17) / (1.0 + WQKSAP*TAMPWQ)
            SADWQSUM(LL,K) = SADWQSUM(LL,K) + XMRM
            IF(XMRM .GT. SADWQMAX(LL,K)) SADWQMAX(LL,K) = XMRM
            IF(XMRM .LT. SADWQMIN(LL,K)) SADWQMIN(LL,K) = XMRM
          ELSE IF(IWQSRP.EQ.2)THEN
            PO4DWQSUM(LL,K) = PO4DWQSUM(LL,K) +
     *             WQVO(LL,K,10) / (1.0 + WQKPO4P*SEDT(LL,K))
            XMRM = WQVO(LL,K,17) / (1.0 + WQKSAP*SEDT(LL,K))
            SADWQSUM(LL,K) = SADWQSUM(LL,K) + XMRM
            IF(XMRM .GT. SADWQMAX(LL,K)) SADWQMAX(LL,K) = XMRM
            IF(XMRM .LT. SADWQMIN(LL,K)) SADWQMIN(LL,K) = XMRM
          ELSE
            PO4DWQSUM(LL,K) = PO4DWQSUM(LL,K) + WQVO(LL,K,10)
            SADWQSUM(LL,K)  = SADWQSUM(LL,K) + WQVO(LL,K,17)
            XMRM = WQVO(LL,K,17)
            IF(XMRM .GT. SADWQMAX(LL,K)) SADWQMAX(LL,K) = XMRM
            IF(XMRM .LT. SADWQMIN(LL,K)) SADWQMIN(LL,K) = XMRM
          ENDIF
          XPO4DWQ = MAX(PO4DWQ_,0.0)
          APCWQ = 1.0 / (WQCP1PRM + WQCP2PRM*EXP(-WQCP3PRM*XPO4DWQ))
          XMRM = WQVO(LL,K,7)+WQVO(LL,K,8)+WQVO(LL,K,9)+WQVO(LL,K,10)
     *       + APCWQ*TBWQ
          TPWQSUM(LL,K) = TPWQSUM(LL,K) + XMRM
          IF(XMRM .GT. TPWQMAX(LL,K)) TPWQMAX(LL,K) = XMRM
          IF(XMRM .LT. TPWQMIN(LL,K)) TPWQMIN(LL,K) = XMRM
          XMRM = WQVO(LL,K,7) + WQVO(LL,K,8) + APCWQ*TBWQ
          POPSUM(LL,K) = POPSUM(LL,K) + XMRM
          IF(XMRM .GT. POPMAX(LL,K)) POPMAX(LL,K) = XMRM
          IF(XMRM .LT. POPMIN(LL,K)) POPMIN(LL,K) = XMRM
          XMRM = WQVO(LL,K,11) + WQVO(LL,K,12) + WQVO(LL,K,13) +
     *       WQVO(LL,K,14) + WQVO(LL,K,15) + WQANCC*WQVO(LL,K,1) +
     *       WQANCD*WQVO(LL,K,2)+ WQANCG*WQVO(LL,K,3)
          TNWQSUM(LL,K) = TNWQSUM(LL,K) + XMRM
          IF(XMRM .GT. TNWQMAX(LL,K)) TNWQMAX(LL,K) = XMRM
          IF(XMRM .LT. TNWQMIN(LL,K)) TNWQMIN(LL,K) = XMRM
          XMRM = WQVO(LL,K,11) + WQVO(LL,K,12) + WQANCC*WQVO(LL,K,1) +
     *       WQANCD*WQVO(LL,K,2) + WQANCG*WQVO(LL,K,3)
          PONSUM(LL,K) = PONSUM(LL,K) + XMRM
          IF(XMRM .GT. PONMAX(LL,K)) PONMAX(LL,K) = XMRM
          IF(XMRM .LT. PONMIN(LL,K)) PONMIN(LL,K) = XMRM
          IF(IWQSI.EQ.1)THEN
            TSIWQSUM(LL,K) = TSIWQSUM(LL,K) + WQVO(LL,K,16)
     *      +WQVO(LL,K,17) + WQASCD*WQVO(LL,K,2)
           ELSE
            TSIWQSUM(LL,K) = 0.0
          ENDIF
C
C M. MORTON 03/11/96 ADDED BOD5 TERM FOR OUTPUT:
          IZ=IWQZMAP(LL,K)
          BOD5SUM(LL,K) = BOD5SUM(LL,K) +
     +             2.67 * ( WQVO(LL,K,5)*(1.0-EXP(-5.0*WQKLC))
     +           + WQVO(LL,K,6)*(1.0-EXP(-5.0*WQKDC(IZ)))
     +           + WQVO(LL,K,18)*(1.0-EXP(-5.0*WQKCD(IZ)))
     +           + WQVO(LL,K,1)*(1.0-EXP(-5.0*WQBMRC(IZ)))
     +           + WQVO(LL,K,2)*(1.0-EXP(-5.0*WQBMRD(IZ)))
     +           + WQVO(LL,K,3)*(1.0-EXP(-5.0*WQBMRG(IZ))) )
     +           + 4.57 * WQVO(LL,K,14)*(1.0-EXP(-5.0*WQNITM))
C
C KEEP TRACK OF WATER TEMPERATURE:
          XMRM = TEM(LL,K)
          WQTEMSUM(LL,K) = WQTEMSUM(LL,K) + XMRM
          IF(XMRM .GT. WQTEMMAX(LL,K)) WQTEMMAX(LL,K) = XMRM
          IF(XMRM .LT. WQTEMMIN(LL,K)) WQTEMMIN(LL,K) = XMRM
C
C KEEP TRACK OF SALINITY:
          XMRM = MAX(SAL(LL,K), 0.0)
          SALSUM(LL,K) = SALSUM(LL,K) + XMRM
          IF(XMRM .GT. SALMX(LL,K)) SALMX(LL,K) = XMRM
          IF(XMRM .LT. SALMN(LL,K)) SALMN(LL,K) = XMRM
C
C KEEP TRACK OF TOTAL SUSPENDED SOLIDS:
          XMRM = MAX(SEDT(LL,K), 0.0)
          TSSSUM(LL,K) = TSSSUM(LL,K) + XMRM
          IF(XMRM .GT. TSSMX(LL,K)) TSSMX(LL,K) = XMRM
          IF(XMRM .LT. TSSMN(LL,K)) TSSMN(LL,K) = XMRM
C
C KEEP TRACK OF SECCHI DEPTH:
          XMRM = WQKETOT(LL,K)
          WQKETSUM(LL,K) = WQKETSUM(LL,K) + XMRM
          IF(XMRM .GT. WQKETMX(LL,K)) WQKETMX(LL,K) = XMRM
          IF(XMRM .LT. WQKETMN(LL,K)) WQKETMN(LL,K) = XMRM
C
C KEEP TRACK OF VARIABLES FOR DIURNAL DO OUTPUT:
C
          IF(ISDIURDO .GT. 0)THEN
            SWQSUM(LL,K) = SWQSUM(LL,K) + MAX(SAL(LL,K), 0.0)
            TEMSUM(LL,K) = TEMSUM(LL,K) + TEM(LL,K)
            DZSUM(LL,K) = DZSUM(LL,K) + DZC(K)*HP(LL)
            DOOSUM(LL,K) = DOOSUM(LL,K) + MAX(WQVO(LL,K,19), 0.0)
            XMRM = WQVO(LL,K,1) * WQCHLC
            CYASUM(LL,K) = CYASUM(LL,K) + XMRM
            XMRM = WQVO(LL,K,2) * WQCHLD
            DIASUM(LL,K) = DIASUM(LL,K) + XMRM
            XMRM = WQVO(LL,K,3) * WQCHLG
            GRNSUM(LL,K) = GRNSUM(LL,K) + XMRM
            IF(IDNOTRVA .NE. 0)THEN
              XMRM = WQVO(LL,K,NWQV+1) * WQCHLM
              XMACSUM(LL,K) = XMACSUM(LL,K) + XMRM
            ENDIF
C
C KEEP TRACK OF ALGAL PRIMARY PRODUCTION:
C
            XMRM = WQPC(LL)*WQVO(LL,K,1)*0.5*WQAOCR
            PRODSUM(LL,K,1) = PRODSUM(LL,K,1) + XMRM
            XMRM = WQPD(LL)*WQVO(LL,K,2)*0.5*WQAOCR
            PRODSUM(LL,K,2) = PRODSUM(LL,K,2) + XMRM
            XMRM = WQPG(LL)*WQVO(LL,K,3)*0.5*WQAOCR
            PRODSUM(LL,K,3) = PRODSUM(LL,K,3) + XMRM
            IF(K .EQ. 1)THEN
              XMRM = WQPM(LL)*WQVO(LL,K,NWQV+1)*0.5*WQAOCRPM
              PRODSUM(LL,K,4) = PRODSUM(LL,K,4) + XMRM
            ELSE
              PRODSUM(LL,K,4) = 0.0
            ENDIF
C KEEP TRACK OF ALGAL RESPIRATION:
            XMRM = WQBMC(LL)*WQVO(LL,K,1)*0.5*WQAOCR
            RESPSUM(LL,K,1) = RESPSUM(LL,K,1) + XMRM
            XMRM = WQBMD(LL)*WQVO(LL,K,2)*0.5*WQAOCR
            RESPSUM(LL,K,2) = RESPSUM(LL,K,2) + XMRM
            XMRM = WQBMG(LL)*WQVO(LL,K,3)*0.5*WQAOCR
            RESPSUM(LL,K,3) = RESPSUM(LL,K,3) + XMRM
            IF(K .EQ. 1)THEN
              XMRM = WQBMM(LL)*WQVO(LL,K,NWQV+1)*0.5*WQAOCRRM
              RESPSUM(LL,K,4) = RESPSUM(LL,K,4) + XMRM
            ELSE
              RESPSUM(LL,K,4) = 0.0
            ENDIF
C
C COMPUTE DISSOLVED OXYGEN SATURATION CONCENTRATION:
            XMRM = MAX(SAL(LL,K), 0.0)
            TVAL1=1./(TEM(LL,K)+273.15)
            TVAL2=TVAL1*TVAL1
            TVAL3=TVAL1*TVAL2
            TVAL4=TVAL2*TVAL2
            RLNSAT1=-139.3441+(1.575701E+5*TVAL1)-(6.642308E+7*TVAL2)
     &                     +(1.2438E+10*TVAL3)-(8.621949E+11*TVAL4)
            RLNSAT2=RLNSAT1 - XMRM *( 1.7674E-2-(1.0754E+1*TVAL1)
     &                             +(2.1407E+3*TVAL2) )
            WQDOS_ = EXP(RLNSAT2)
            DOSSUM(LL,K) = DOSSUM(LL,K) + WQDOS_
C
C KEEP TRACK OF REAERATION IN SURFACE LAYER ONLY:
            IF(K .EQ. KC)THEN
              WINDREA = WINDSTKA(LL)
              IF(WINDSTKA(LL) .GT. 11.0) WINDREA = 11.0
              WQWREA=0.728*SQRT(WINDREA)+(0.0372*WINDREA-0.317)*WINDREA

              IZ=IWQZMAP(LL,K)
C
C CONSTANT USER-SPECIFIED REAERATION:
C
              IF(IWQKA(IZ) .EQ. 0)THEN
                WQVREA = WQKRO(IZ)
                WQWREA = 0.0
              ENDIF
C
C CONSTANT REAERATION DUE TO WATER VELOCITY:
C
              IF(IWQKA(IZ) .EQ. 1)THEN
                WQVREA = WQKRO(IZ)
              ENDIF
C
C O'CONNOR-DOBBINS (1958) EQUATION FOR REAERATION IS:
C    WQKRO = 3.933 TYPICALLY
C
              IF(IWQKA(IZ) .EQ. 2)THEN
C                XMRM = SQRT(U(LL,K)*U(LL,K) + V(LL,K)*V(LL,K))
                UMRM = MAX(U(LL,K), U(LL+1,K))
                VMRM = MAX(V(LL,K), V(LNC(LL),K))
                XMRM = SQRT(UMRM*UMRM + VMRM*VMRM)
                WQVREA = WQKRO(IZ) * XMRM**0.5 / HP(LL)**1.5
              ENDIF
C
C OWENS AND GIBBS (1964) REAERATION EQUATION:
C    WQKRO = 5.32 TYPICALLY
C
              IF(IWQKA(IZ) .EQ. 3)THEN
C                XMRM = SQRT(U(LL,K)*U(LL,K) + V(LL,K)*V(LL,K))
                UMRM = MAX(U(LL,K), U(LL+1,K))
                VMRM = MAX(V(LL,K), V(LNC(LL),K))
                XMRM = SQRT(UMRM*UMRM + VMRM*VMRM)
                WQVREA = WQKRO(IZ) * XMRM**0.67 / HP(LL)**1.85
              ENDIF
C
C MODIFIED OWENS AND GIBBS REAERATION EQUATION:
C NOTE:  AT A DEPTH OF 1.0 FT, THIS EQUATION IS SAME AS OWENS & GIBBS
C        AT HIGH DEPTHS IT GIVES LARGER REAERATION THAN OWENS & GIBBS
C WQKRO = 5.32 TYPICALLY
C
              IF(IWQKA(IZ) .EQ. 4)THEN
C                XMRM = XMRM * SQRT(HP(LL))/0.3048
C                XMRM = SQRT(U(LL,K)*U(LL,K) + V(LL,K)*V(LL,K))
                UMRM = MAX(U(LL,K), U(LL+1,K))
                VMRM = MAX(V(LL,K), V(LNC(LL),K))
                XMRM = SQRT(UMRM*UMRM + VMRM*VMRM)
                YMRM = HP(LL)*3.0*(1.0 - HP(LL)/(HP(LL)+0.1524))
                WQVREA = WQKRO(IZ) * XMRM**0.67 / YMRM**1.85
              ENDIF
              WQVREA = WQVREA * REAC(IZ)
              WQWREA = WQWREA * REAC(IZ)
C
C NOW COMBINE REAERATION DUE TO WATER VELOCITY AND WIND STRESS:
C
              IWQTMRM = 10.0*TEM(LL,K) + 151
C              DZWQMRM = 1.0 / (DZC(K)*HP(LL))
C              XMRM = - (WQVREA + WQWREA) * DZWQMRM*WQTDKR(IWQTMRM)
              XMRM = - (WQVREA + WQWREA) * WQTDKR(IWQTMRM,IZ)
              RKASUM(LL,K) = RKASUM(LL,K) + XMRM
            ELSE
              RKASUM(LL,K) = 0.0
            ENDIF
C
C KEEP TRACK OF SOD IN BOTTOM LAYER ONLY:
            IF(K .EQ. 1)THEN
              SODSUM(LL,K) = SODSUM(LL,K) + WQBFO2(LL)
            ELSE
              SODSUM(LL,K) = 0.0
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      TIMESUM = TIMESUM + TIMTMP
      NWQCNT = NWQCNT+1
      IF(ISDIURDO .GT. 0)THEN
        TIMESUM2 = TIMESUM2 + TIMTMP
        NDOCNT = NDOCNT + 1
      ENDIF
C
C  CHECK TO SEE IF IT IS TIME TO WRITE OUT RESULTS.  THEN ZERO SUMMATION
C  ARRAYS AFTER WRITING OUT DATA TO GET READY FOR NEXT AVERAGING PERIOD.
C
C      IF(NWQCNT .EQ. IWQTSDT)THEN
      IF( MOD(ITNWQ,IWQTSDT) .EQ. 0 )THEN
        NREC = NREC+1
        TIMTMP = TIMESUM / NWQCNT
C
C OPEN WQ ASCII FILE:
        OPEN(1,FILE='WQWCTS.OUT',STATUS='UNKNOWN',POSITION='APPEND')
C
C OPEN WQ AVERAGE BINARY FILE:
        IF(ISWQAVG .GT. 0)THEN
          INQUIRE(UNIT=3, OPENED=IS2OPEN)
          IF(IS2OPEN)THEN
            WRITE(0,*) 'ERROR:  UNIT=2 ALREADY OPENED.'
            STOP 'EFDC HALTED'
          ENDIF
![GeoSR : 100803
C          OPEN(UNIT=2, FILE='WQWCAVG.BIN', ACCESS='DIRECT',
C     +     FORM='UNFORMATTED', STATUS='UNKNOWN', RECL=MAXRECL)
          OPEN(UNIT=3, FILE='WQWCAVG.BIN', FORM='UNFORMATTED',
     +      POSITION='APPEND')
C
C UPDATE TIME-STAMP INFORMATION IN HEADER SECTION:
C
C          READ(2) NDUM, XDUM, XDUM,
C     +      XDT, IXDT, NPARM, NCELLS, NLAYERS
C          NDUM=NDUM
C          XDUM=XDUM
C          WRITE(2, REC=1) NREC, TBEGAN, TIMTMP,
C     +      XDT, IXDT, NPARM, NCELLS, NLAYERS
C          WRITE(2, REC=NR1) TIMTMP
!{GeoSR, YSSONG, AVERAGE FILED PRINT, 111102
C          WRITE(3) TIMTMP
          WRITE(3) N,TIMTMP
!}
! GeoSR : 100803]

        ENDIF
C
C        DO M=1,IWQTS
        DO LL=2,LA
C CHECK LWQTS ARRAY TO SEE IF THIS CELL SHOULD BE WRITTEN TO THE
C ASCII OUTPUT FILE:
          ISASCII = .FALSE.
          DO M=1,IWQTS
            IF(LWQTS(M) .EQ. LL) ISASCII = .TRUE.
          ENDDO
          DO K=1,KC
C            LL=LWQTS(M)
            DO NW=1,NWQV
              WQVSUM(LL,K,NW) = WQVSUM(LL,K,NW) / NWQCNT
            ENDDO
            TOCWQSUM(LL,K) = TOCWQSUM(LL,K) / NWQCNT
            POCSUM(LL,K) = POCSUM(LL,K) / NWQCNT
            PO4DWQSUM(LL,K) = PO4DWQSUM(LL,K) / NWQCNT
            SADWQSUM(LL,K) = SADWQSUM(LL,K) / NWQCNT
            TPWQSUM(LL,K) = TPWQSUM(LL,K) / NWQCNT
            POPSUM(LL,K) = POPSUM(LL,K) / NWQCNT
            TNWQSUM(LL,K) = TNWQSUM(LL,K) / NWQCNT
            PONSUM(LL,K) = PONSUM(LL,K) / NWQCNT
            TSIWQSUM(LL,K) = TSIWQSUM(LL,K) / NWQCNT
            BOD5SUM(LL,K) = BOD5SUM(LL,K) / NWQCNT
            CHLC = WQVSUM(LL,K,1)*WQCHLC
            CHLD = WQVSUM(LL,K,2)*WQCHLD
            CHLG = WQVSUM(LL,K,3)*WQCHLG
            CHLWQ = CHLC + CHLD + CHLG
            CHLM = CHLMSUM(LL,K) / NWQCNT
            SALSUM(LL,K) = SALSUM(LL,K) / NWQCNT
            WQTEMSUM(LL,K) = WQTEMSUM(LL,K) / NWQCNT
            TSSSUM(LL,K) = TSSSUM(LL,K) / NWQCNT
            WQKETSUM(LL,K) = WQKETSUM(LL,K) / NWQCNT
           IF(IDNOTRVA.EQ.0)THEN
C WRITE TO ASCII FILE WQWCTS.OUT HERE:
!{GeoSR, YSSONG, AVERAGE FILED PRINT, 111102
C            IF(ISASCII)THEN
C             WRITE(1,71) IL(LL),JL(LL),K,TIMTMP, CHLWQ,TOCWQSUM(LL,K),
C     +       WQVSUM(LL,K,6), TPWQSUM(LL,K), WQVSUM(LL,K,9),
C     +       WQVSUM(LL,K,10), PO4DWQSUM(LL,K), CHLC, TNWQSUM(LL,K),
C     +       (WQVSUM(LL,K,NW),NW=13,15), TSIWQSUM(LL,K),
C     +       (WQVSUM(LL,K,NW),NW=16,17), SADWQSUM(LL,K), BOD5SUM(LL,K),
C     +       WQVSUM(LL,K,19), CHLD, CHLG, WQVSUM(LL,K,NWQV)
C            ENDIF
!}
C WRITE TO BINARY FILE WQWCAVG.BIN HERE:
             IF(ISWQAVG .GT. 0)THEN
             WRITE(3) SALSUM(LL,K), CHLWQ, WQVSUM(LL,K,19),
     +       TOCWQSUM(LL,K), WQVSUM(LL,K,6), POCSUM(LL,K),
     +       TNWQSUM(LL,K), (WQVSUM(LL,K,NW),NW=13,15), PONSUM(LL,K),
     +       TPWQSUM(LL,K),WQVSUM(LL,K,9),POPSUM(LL,K),PO4DWQSUM(LL,K),
C     +       TPWQSUM(LL,K),WQVSUM(LL,K,9),POPSUM(LL,K),WQVSUM(LL,K,10),
     +       SADWQSUM(LL,K), WQVSUM(LL,K,21),
     +       WQVSUM(LL,K,18), CHLD, CHLG, TSSSUM(LL,K), WQKETSUM(LL,K),
     +       WQTEMSUM(LL,K)
             ENDIF
           ELSE
!{GeoSR, YSSONG, AVERAGE FILED PRINT, 111102
C            IF(ISASCII)THEN
C             WRITE(1,71) IL(LL),JL(LL),K,TIMTMP, CHLWQ,TOCWQSUM(LL,K),
C     +       WQVSUM(LL,K,6), TPWQSUM(LL,K), WQVSUM(LL,K,9),
C     +       WQVSUM(LL,K,10), PO4DWQSUM(LL,K), CHLC, TNWQSUM(LL,K),
C     +       (WQVSUM(LL,K,NW),NW=13,15), TSIWQSUM(LL,K),
C     +       (WQVSUM(LL,K,NW),NW=16,17), SADWQSUM(LL,K), BOD5SUM(LL,K),
C     +       WQVSUM(LL,K,19), CHLD, CHLG, WQVSUM(LL,K,NWQV),
C     +       CHLM
C
!}            ENDIF
C WRITE TO BINARY FILE WQWCAVG.BIN HERE:
             IF(ISWQAVG .GT. 0)THEN
             WRITE(3) SALSUM(LL,K), CHLWQ, WQVSUM(LL,K,19),
     +       TOCWQSUM(LL,K), WQVSUM(LL,K,6), POCSUM(LL,K),
     +       TNWQSUM(LL,K), (WQVSUM(LL,K,NW),NW=13,15), PONSUM(LL,K),
     +       TPWQSUM(LL,K),WQVSUM(LL,K,9),POPSUM(LL,K),PO4DWQSUM(LL,K),
C     +       TPWQSUM(LL,K),WQVSUM(LL,K,9),POPSUM(LL,K),WQVSUM(LL,K,10),
     +       SADWQSUM(LL,K), WQVSUM(LL,K,21),
     +       WQVSUM(LL,K,18), CHLD, CHLG, TSSSUM(LL,K), WQKETSUM(LL,K),
     +       WQTEMSUM(LL,K), CHLM
             ENDIF
           ENDIF
          ENDDO
        ENDDO
C
   71 FORMAT(3I5,F11.5, 1P, 23E11.3)
C
        CLOSE(1)
        IF(ISWQAVG .GT. 0)THEN
![GeoSR : 100803
C          INQUIRE(UNIT=2, NEXTREC=NR1)
! GeoSR : 100803]
          CLOSE(3)
        ENDIF
C
C OPEN WQ MINIMUM BINARY FILE:
        IF(ISWQMIN .GT. 0)THEN
          OPEN(UNIT=2, FILE='WQWCMIN.BIN', ACCESS='DIRECT',
     +     FORM='UNFORMATTED', STATUS='UNKNOWN', RECL=MAXRECL)

          READ(2, REC=1) NDUM, XDUM, XDUM,
     +      XDT, IXDT, NPARM, NCELLS, NLAYERS
          WRITE(2, REC=1) NREC, TBEGAN, TIMTMP,
     +      XDT, IXDT, NPARM, NCELLS, NLAYERS

          WRITE(2, REC=NR2) TIMTMP
          DO LL=2,LA
           DO K=1,KC
            CHLCMIN = WQVMIN(LL,K,1)*WQCHLC
            CHLDMIN = WQVMIN(LL,K,2)*WQCHLD
            CHLGMIN = WQVMIN(LL,K,3)*WQCHLG
            CHLWQMIN = CHLCMIN + CHLDMIN + CHLGMIN
C WRITE TO BINARY FILE WQWCMIN.BIN HERE:
            IF(IDNOTRVA.EQ.0)THEN
             WRITE(2) SALMN(LL,K), CHLWQMIN, WQVMIN(LL,K,19),
     +       TOCWQMIN(LL,K), WQVMIN(LL,K,6), POCMIN(LL,K),
     +       TNWQMIN(LL,K), (WQVMIN(LL,K,NW),NW=13,15), PONMIN(LL,K),
     +       TPWQMIN(LL,K),WQVMIN(LL,K,9),POPMIN(LL,K),PO4DWQSUM(LL,K),
C     +       TPWQMIN(LL,K),WQVMIN(LL,K,9),POPMIN(LL,K),WQVMIN(LL,K,10),
     +       SADWQMIN(LL,K), WQVMIN(LL,K,21), WQVMIN(LL,K,18),
     +       CHLDMIN, CHLGMIN, TSSMN(LL,K), WQKETMN(LL,K),
     +       WQTEMMIN(LL,K)
            ELSE
             WRITE(2) SALMN(LL,K), CHLWQMIN, WQVMIN(LL,K,19),
     +       TOCWQMIN(LL,K), WQVMIN(LL,K,6), POCMIN(LL,K),
     +       TNWQMIN(LL,K), (WQVMIN(LL,K,NW),NW=13,15), PONMIN(LL,K),
     +       TPWQMIN(LL,K),WQVMIN(LL,K,9),POPMIN(LL,K),PO4DWQSUM(LL,K),
C     +       TPWQMIN(LL,K),WQVMIN(LL,K,9),POPMIN(LL,K),WQVMIN(LL,K,10),
     +       SADWQMIN(LL,K), WQVMIN(LL,K,21),
     +       WQVMIN(LL,K,18), CHLDMIN, CHLGMIN, TSSMN(LL,K),
     +       WQKETMN(LL,K), WQTEMMIN(LL,K), CHLMMIN(LL,K)
            ENDIF
           ENDDO
          ENDDO
          INQUIRE(UNIT=2, NEXTREC=NR2)
          CLOSE(2)
        ENDIF
C
C OPEN WQ MAXIMUM BINARY FILE:
        IF(ISWQMAX .GT. 0)THEN
          OPEN(UNIT=2, FILE='WQWCMAX.BIN', ACCESS='DIRECT',
     +     FORM='UNFORMATTED', STATUS='UNKNOWN', RECL=MAXRECL)

          READ(2, REC=1) NDUM, XDUM, XDUM,
     +      XDT, IXDT, NPARM, NCELLS, NLAYERS
          WRITE(2, REC=1) NREC, TBEGAN, TIMTMP,
     +      XDT, IXDT, NPARM, NCELLS, NLAYERS

          WRITE(2, REC=NR3) TIMTMP
          DO LL=2,LA
           DO K=1,KC
            CHLCMAX = WQVMAX(LL,K,1)*WQCHLC
            CHLDMAX = WQVMAX(LL,K,2)*WQCHLD
            CHLGMAX = WQVMAX(LL,K,3)*WQCHLG
            CHLWQMAX = CHLCMAX + CHLDMAX + CHLGMAX
C WRITE TO BINARY FILE WQWCMAX HERE:
            IF(IDNOTRVA.EQ.0)THEN
             WRITE(2) SALMX(LL,K), CHLWQMAX, WQVMAX(LL,K,19),
     +       TOCWQMAX(LL,K), WQVMAX(LL,K,6), POCMAX(LL,K),
     +       TNWQMAX(LL,K), (WQVMAX(LL,K,NW),NW=13,15), PONMAX(LL,K),
     +       TPWQMAX(LL,K),WQVMAX(LL,K,9),POPMAX(LL,K),PO4DWQSUM(LL,K),
C     +       TPWQMAX(LL,K),WQVMAX(LL,K,9),POPMAX(LL,K),WQVMAX(LL,K,10),
     +       SADWQMAX(LL,K), WQVMAX(LL,K,21), WQVMAX(LL,K,18),
     +       CHLDMAX, CHLGMAX, TSSMX(LL,K), WQKETMX(LL,K),
     +       WQTEMMAX(LL,K)
            ELSE
             WRITE(2) SALMX(LL,K), CHLWQMAX, WQVMAX(LL,K,19),
     +       TOCWQMAX(LL,K), WQVMAX(LL,K,6), POCMAX(LL,K),
     +       TNWQMAX(LL,K), (WQVMAX(LL,K,NW),NW=13,15), PONMAX(LL,K),
     +       TPWQMAX(LL,K),WQVMAX(LL,K,9),POPMAX(LL,K),PO4DWQSUM(LL,K),
C     +       TPWQMAX(LL,K),WQVMAX(LL,K,9),POPMAX(LL,K),WQVMAX(LL,K,10),
     +       SADWQMAX(LL,K), WQVMAX(LL,K,21),
     +       WQVMAX(LL,K,18), CHLDMAX, CHLGMAX, TSSMX(LL,K),
     +       WQKETMX(LL,K), WQTEMMAX(LL,K), CHLMMAX(LL,K)
            ENDIF
           ENDDO
          ENDDO
          INQUIRE(UNIT=2, NEXTREC=NR3)
          CLOSE(2)
        ENDIF
C
C OPEN DO COMPONENT ANALYSIS BINARY FILE:
C
        IF(ISCOMP .NE. 2)THEN
          DO LL=2,LA
            DO K=1,KC
              XLIMNC(LL,K) = XLIMNC(LL,K) / NLIM
              XLIMND(LL,K) = XLIMND(LL,K) / NLIM
              XLIMNG(LL,K) = XLIMNG(LL,K) / NLIM
              XLIMNM(LL,K) = XLIMNM(LL,K) / NLIM
              XLIMPC(LL,K) = XLIMPC(LL,K) / NLIM
              XLIMPD(LL,K) = XLIMPD(LL,K) / NLIM
              XLIMPG(LL,K) = XLIMPG(LL,K) / NLIM
              XLIMPM(LL,K) = XLIMPM(LL,K) / NLIM
              XLIMIC(LL,K) = XLIMIC(LL,K) / NLIM
              XLIMID(LL,K) = XLIMID(LL,K) / NLIM
              XLIMIG(LL,K) = XLIMIG(LL,K) / NLIM
              XLIMIM(LL,K) = XLIMIM(LL,K) / NLIM
              XLIMVM(LL,K) = XLIMVM(LL,K) / NLIM
              XLIMDM(LL,K) = XLIMDM(LL,K) / NLIM
              XLIMTC(LL,K) = XLIMTC(LL,K) / NLIM
              XLIMTD(LL,K) = XLIMTD(LL,K) / NLIM
              XLIMTG(LL,K) = XLIMTG(LL,K) / NLIM
              XLIMTM(LL,K) = XLIMTM(LL,K) / NLIM
              XDODZ(LL,K) = XDODZ(LL,K) / NLIM
              do nsp=1,NXSP
                XLIMIX(LL,K,nsp) = XLIMIX(LL,K,nsp) / NLIM
                XLIMNX(LL,K,nsp) = XLIMNX(LL,K,nsp) / NLIM
                XLIMPX(LL,K,nsp) = XLIMPX(LL,K,nsp) / NLIM
                XLIMTX(LL,K,nsp) = XLIMTX(LL,K,nsp) / NLIM
              enddo
            enddo
          enddo
        endif
        !{GeoSR, YSSONG, GROWTH LIMIT PRINT, 111031
        IF(ISCOMP .EQ. 1 .OR. ISCOMP .EQ. 4)THEN
          OPEN(3,FILE='WQDOCOMP.BIN',FORM='UNFORMATTED',
     &          POSITION='APPEND',STATUS='UNKNOWN')  
C          NREC3 = NREC3 + 1
          TIMTMP = TIMESUM3 / NWQCNT
          WRITE(3) N,TIMTMP
          ! X-species
          IF(NXSP.GT.0) THEN
            OPEN(333,FILE='WQDOCOMPX.BIN',FORM='UNFORMATTED',
     &            POSITION='APPEND',STATUS='UNKNOWN')  
            WRITE(333) N,TIMTMP
          ENDIF
C          OPEN(UNIT=2, FILE='WQDOCOMP.BIN', ACCESS='DIRECT',
C     +     FORM='UNFORMATTED', STATUS='UNKNOWN', RECL=MAXRECL3)
C
C          READ(2, REC=1) NDUM, XDUM, XDUM,
C     +      XDT, IXDT, NPARM, NCELLS, NLAYERS
C          WRITE(2, REC=1) NREC3, TBEGAN, TIMTMP,
C     +      XDT, IXDT, NPARM, NCELLS, NLAYERS
C
C          WRITE(2, REC=NR5) TIMTMP
          DO LL=2,LA
            DO K=1,KC
C              XLIMNC(LL,K) = XLIMNC(LL,K) / NLIM
C              XLIMND(LL,K) = XLIMND(LL,K) / NLIM
C              XLIMNG(LL,K) = XLIMNG(LL,K) / NLIM
C              XLIMNM(LL,K) = XLIMNM(LL,K) / NLIM
C              XLIMPC(LL,K) = XLIMPC(LL,K) / NLIM
C              XLIMPD(LL,K) = XLIMPD(LL,K) / NLIM
C              XLIMPG(LL,K) = XLIMPG(LL,K) / NLIM
C              XLIMPM(LL,K) = XLIMPM(LL,K) / NLIM
C              XLIMIC(LL,K) = XLIMIC(LL,K) / NLIM
C              XLIMID(LL,K) = XLIMID(LL,K) / NLIM
C              XLIMIG(LL,K) = XLIMIG(LL,K) / NLIM
C              XLIMIM(LL,K) = XLIMIM(LL,K) / NLIM
C              XLIMVM(LL,K) = XLIMVM(LL,K) / NLIM
C              XLIMDM(LL,K) = XLIMDM(LL,K) / NLIM
C              XLIMTC(LL,K) = XLIMTC(LL,K) / NLIM
C              XLIMTD(LL,K) = XLIMTD(LL,K) / NLIM
C              XLIMTG(LL,K) = XLIMTG(LL,K) / NLIM
C              XLIMTM(LL,K) = XLIMTM(LL,K) / NLIM
C              XDODZ(LL,K) = XDODZ(LL,K) / NLIM
c              XMRM = 1.0 / XDODZ(LL,K)
C              XDOSAT(LL,K) = XDOSAT(LL,K) *XMRM
C              XDODEF(LL,K) = XDODEF(LL,K) *XMRM
C              XDOALL(LL,K) = XDOALL(LL,K) *XMRM
C              XNUMER = XDOSAT(LL,K) - XDODEF(LL,K)
C              XNUMER = MAX (XNUMER, 0.0)
C              XDENOM = ABS (XDOALL(LL,K))
CC              XDENOM = MAX (XDENOM, 0.1)
C              XRATIO = XNUMER / XDENOM
C              XDODZ(LL,K) = ABS (XDOALL(LL,K)) *XRATIO
C
C              XDOPSL(LL,K) = XDOPSL(LL,K) *XMRM *XRATIO
C              XDOSOD(LL,K) = XDOSOD(LL,K) *XMRM *XRATIO
C              XDOKAR(LL,K) = XDOKAR(LL,K) *XMRM *XRATIO
C              XDODOC(LL,K) = XDODOC(LL,K) *XMRM *XRATIO
C              XDONIT(LL,K) = XDONIT(LL,K) *XMRM *XRATIO
C              XDOCOD(LL,K) = XDOCOD(LL,K) *XMRM *XRATIO
C              XDOPPB(LL,K) = XDOPPB(LL,K) *XMRM *XRATIO
C              XDORRB(LL,K) = XDORRB(LL,K) *XMRM *XRATIO
C              XDOPPM(LL,K) = XDOPPM(LL,K) *XMRM *XRATIO
C              XDORRM(LL,K) = XDORRM(LL,K) *XMRM *XRATIO
C              XDOTRN(LL,K) = XDOTRN(LL,K) *XMRM *XRATIO
C
CC WRITE TO BINARY FILE WQDOCOMP.BIN HERE:
CC
C              WRITE(2) XLIMNC(LL,K), XLIMND(LL,K), XLIMNG(LL,K),
C     +        XLIMNM(LL,K), XLIMPC(LL,K), XLIMPD(LL,K), XLIMPG(LL,K),
C     +        XLIMPM(LL,K), XLIMIC(LL,K), XLIMID(LL,K), XLIMIG(LL,K),
C     +        XLIMIM(LL,K), XLIMTC(LL,K), XLIMTD(LL,K), XLIMTG(LL,K),
C     +        XLIMTM(LL,K), XLIMVM(LL,K), XLIMDM(LL,K),
C     +        XDOSAT(LL,K), XDOPSL(LL,K), XDOSOD(LL,K),
C     +        XDOKAR(LL,K), XDODOC(LL,K), XDONIT(LL,K), XDOCOD(LL,K),
C     +        XDOPPB(LL,K), XDORRB(LL,K), XDOPPM(LL,K), XDORRM(LL,K),
C     +        XDODEF(LL,K), XDOTRN(LL,K), XDOALL(LL,K), XDODZ(LL,K)
              WRITE(3) XLIMNC(LL,K), XLIMND(LL,K), XLIMNG(LL,K),
     +         XLIMNM(LL,K), XLIMPC(LL,K), XLIMPD(LL,K), XLIMPG(LL,K),
     +         XLIMPM(LL,K), XLIMIC(LL,K), XLIMID(LL,K), XLIMIG(LL,K),
     +         XLIMIM(LL,K), XLIMTC(LL,K), XLIMTD(LL,K), XLIMTG(LL,K),
     +         XLIMTM(LL,K), XLIMVM(LL,K), XLIMDM(LL,K)
              ! X-species GROWTH LIMIT PRINT
              IF(NXSP.GT.0) THEN
                WRITE(333) (XLIMNX(LL,K,nsp),nsp=1,NXSP),
     +                     (XLIMPX(LL,K,nsp),nsp=1,NXSP),
     +                     (XLIMIX(LL,K,nsp),nsp=1,NXSP),
     +                     (XLIMTX(LL,K,nsp),nsp=1,NXSP)
              ENDIF
            ENDDO
          ENDDO
C
!{GeoSR, GROWTH LIMIT AND ALGAL RATE PRINT, YSSONG, 2015.12.10  
          CLOSE(3)
          ! X-species file
          IF(NXSP.GT.0) CLOSE(333)
        ENDIF
        
        IF(ISCOMP .EQ. 3. OR. ISCOMP .EQ. 4)THEN
          IF(IWQTS.GE.1)THEN
            TIME=DT*FLOAT(N)+TCON*TBEGIN  
            TIME=TIME/TCON 
            DO K=1,KC
              WRITE(FLN,"('WQLIM',I2.2,'.DAT')") K
              OPEN(3,FILE=FLN,POSITION='APPEND')
              DO M=1,IWQTS
                LL=LWQTS(M)
                WRITE(3,8999) TIME,
     +          XLIMNC(LL,K), XLIMND(LL,K), XLIMNG(LL,K),
     +          XLIMNM(LL,K), XLIMPC(LL,K), XLIMPD(LL,K), XLIMPG(LL,K),
     +          XLIMPM(LL,K), XLIMIC(LL,K), XLIMID(LL,K), XLIMIG(LL,K),
     +          XLIMIM(LL,K), XLIMTC(LL,K), XLIMTD(LL,K), XLIMTG(LL,K),
     +          XLIMTM(LL,K), XLIMVM(LL,K), XLIMDM(LL,K)
              ENDDO
              ! X-species
              IF(NXSP.GT.0) THEN
                WRITE(FLNX,"('WQLIMX',I2.2,'.DAT')") K
                OPEN(333,FILE=FLNX,POSITION='APPEND')
                DO M=1,IWQTS
                  LL=LWQTS(M)
                  WRITE(333,8998) TIME, (XLIMNX(LL,K,nsp),nsp=1,NXSP),
     +                       (XLIMPX(LL,K,nsp),nsp=1,NXSP),
     +                       (XLIMIX(LL,K,nsp),nsp=1,NXSP),
     +                       (XLIMTX(LL,K,nsp),nsp=1,NXSP)
                ENDDO
              ENDIF
            ENDDO
            CLOSE(3)
            ! X-species file
            IF(NXSP.GT.0) CLOSE(333)
          ENDIF            
        ENDIF        
!}GeoSR, GROWTH LIMIT AND ALGAL RATE PRINT, JHLEE, 2016.01.21  
        CALL WQZERO3
!}        
        IF(ISCOMP .EQ. 2. OR. ISCOMP .EQ. 4)THEN

C          INQUIRE(UNIT=2, NEXTREC=NR5)
C          CLOSE(2)
C          CALL WQZERO3
          OPEN(3,FILE='WQRATE.BIN',FORM='UNFORMATTED',
     &        POSITION='APPEND',STATUS='UNKNOWN')  
C        TIMTMP = TIMESUM3 / NWQCNT2
          WRITE(3) N,TIMTMP
          IF(K.EQ.KC) THEN   ! SURFACE LAYER
          DO L=2,LA
            WRITE(3) WQPC(L),WQBMC(L),WQPRC(L),WQBCSET(L,1), 
     &               WQPD(L),WQBMD(L),WQPRD(L),WQBDSET(L,1), 
     &               WQPG(L),WQBMG(L),WQPRG(L),WQBGSET(L,1), 
     &               WQKHR(L),WQNIT(L),WQDENIT(L)
          ENDDO
        ELSE
          DO L=2,LA
           WRITE(3) WQPC(L),WQBMC(L),WQPRC(L),WQBCSET(L,2),
     &              WQPD(L),WQBMD(L),WQPRD(L),WQBDSET(L,2), 
     &              WQPG(L),WQBMG(L),WQPRG(L),WQBGSET(L,2),
     &              WQKHR(L),WQNIT(L),WQDENIT(L)
          ENDDO
        ENDIF
        CLOSE(3)
        ENDIF
!}
C
C NOW ZERO THE SUMMATION ARRAYS:
C
        CALL WQZERO
C
      ENDIF
C
C OPEN DIURNAL DO BINARY DUMP FILE:
C
      IF( MOD(ITNWQ, IWQDIUDT ) .EQ. 0 )THEN
        IF(ISDIURDO .GT. 0)THEN
          NREC2 = NREC2+1
          TIMTMP = TIMESUM2 / NDOCNT
          OPEN(UNIT=2, FILE='WQDIURDO.BIN', ACCESS='DIRECT',
     +     FORM='UNFORMATTED', STATUS='UNKNOWN', RECL=MAXRECL2)

          READ(2, REC=1) NDUM, XDUM, XDUM,
     +      XDT, IXDT, NPARM, NCELLS, NLAYERS
          WRITE(2, REC=1) NREC2, TBEGAN, TIMTMP,
     +      XDT, IXDT, NPARM, NCELLS, NLAYERS

          WRITE(2, REC=NR4) TIMTMP
          DO LL=2,LA
           DO K=1,KC
            SWQSUM(LL,K) = SWQSUM(LL,K) / NDOCNT
            TEMSUM(LL,K) = TEMSUM(LL,K) / NDOCNT
            SODSUM(LL,K) = SODSUM(LL,K) / NDOCNT
            RKASUM(LL,K) = RKASUM(LL,K) / NDOCNT
            DZSUM(LL,K)  = DZSUM(LL,K) / NDOCNT
            DOOSUM(LL,K) = DOOSUM(LL,K) / NDOCNT
            DOSSUM(LL,K) = DOSSUM(LL,K) / NDOCNT
            CYASUM(LL,K) = CYASUM(LL,K) / NDOCNT
            DIASUM(LL,K) = DIASUM(LL,K) / NDOCNT
            GRNSUM(LL,K) = GRNSUM(LL,K) / NDOCNT
            XMACSUM(LL,K) = XMACSUM(LL,K) / NDOCNT
            DO I=1,4
              RESPSUM(LL,K,I) = RESPSUM(LL,K,I) / NDOCNT
              PRODSUM(LL,K,I) = PRODSUM(LL,K,I) / NDOCNT
            ENDDO
            CHLWQ = CYASUM(LL,K) + DIASUM(LL,K) + GRNSUM(LL,K)
C
C WRITE TO BINARY FILE WQDIURDO.BIN HERE:
C
            IF(IDNOTRVA.EQ.0)THEN
             WRITE(2) SWQSUM(LL,K), TEMSUM(LL,K), DOSSUM(LL,K),
     +       DOOSUM(LL,K), SODSUM(LL,K), RKASUM(LL,K), DZSUM(LL,K),
     +       PRODSUM(LL,K,1), RESPSUM(LL,K,1),
     +       PRODSUM(LL,K,2), RESPSUM(LL,K,2),
     +       PRODSUM(LL,K,3), RESPSUM(LL,K,3),
     +       PRODSUM(LL,K,4), RESPSUM(LL,K,4),
     +       CYASUM(LL,K), DIASUM(LL,K), GRNSUM(LL,K), CHLWQ
            ELSE
             WRITE(2) SWQSUM(LL,K), TEMSUM(LL,K), DOSSUM(LL,K),
     +       DOOSUM(LL,K), SODSUM(LL,K), RKASUM(LL,K), DZSUM(LL,K),
     +       PRODSUM(LL,K,1), RESPSUM(LL,K,1),
     +       PRODSUM(LL,K,2), RESPSUM(LL,K,2),
     +       PRODSUM(LL,K,3), RESPSUM(LL,K,3),
     +       PRODSUM(LL,K,4), RESPSUM(LL,K,4),
     +       CYASUM(LL,K), DIASUM(LL,K), GRNSUM(LL,K), CHLWQ,
     +       XMACSUM(LL,K)
            ENDIF
           ENDDO
          ENDDO
          INQUIRE(UNIT=2, NEXTREC=NR4)

          CLOSE(2)
        ENDIF
C
C NOW ZERO THE SUMMATION ARRAYS:
C
        CALL WQZERO2
C
      ENDIF
C
 8999 FORMAT(F10.5,18E12.4)        
 8998 FORMAT(F10.5,(E12.4))
      RETURN
      END
