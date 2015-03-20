      SUBROUTINE SMMBE  
C  
C CHANGE RECORD  
C  CONTROL SUBROUTINE FOR SEDIMENT COMPONENT OF WATER QUALITY MODEL  
C  ORGINALLY CODED BY K.-Y. PARK  
C  OPTIMIZED AND MODIFIED BY J. M. HAMRICK  
C   AND BFSADSUM ARRAYS TO KEEP TRACK OF BENTHIC FLUXES AT ALL CELLS  
C   FOR LATER STORAGE IN BINARY FILE (WQSDTS.BIN).  
C  
      USE GLOBAL  
C  
C SED TEMP., & FIND AN INDEX FOR LOOK-UP TABLE FOR TEMPERATURE  
C  DEPENDENCY  
C  
      DO L=2,LA  
        IZ = ISMZMAP(L)  
        SMT(L) = (SMT(L) + SM1DIFT(IZ)*TEM(L,1)) * SM2DIFT(IZ)   ! *** pmc - won't work for variable DT
        !ISMT(L) = 10.0*SMT(L) + 51  
        ISMT(L)=NINT((SMT(L)-WQTDsMIN)/WQTDsINC)+1  ! *** DSLLC SINGLE LINE
        IF(ISMT(L).LT.1 .OR. ISMT(L).GT.NWQTD)THEN  
          IF(ISDYNSTP.EQ.0)THEN  
            TIMTMP=(DT*FLOAT(N)+TCON*TBEGIN)/86400.  
          ELSE  
            TIMTIM=TIMESEC/86400.  
          ENDIF  
          OPEN(1,FILE='ERROR.LOG',POSITION='APPEND',STATUS='UNKNOWN')  
          WRITE(1,911) TIMTMP, L, IL(L), JL(L), TEM(L,1), SMT(L)  
          CLOSE(1)  
          PRINT *, 'L, TEM(L,1), SMT(L) = ', L,TEM(L,1),SMT(L)  
C  
C ISMT(L) WAS SET EQUAL TO THE BOUNDS IF IT EXCEEDED THE BOUNDS, THUS  
C THE MODEL IS NOW ALLOWED TO CONTINUE TO RUN.  THE USER SHOULD CHECK  
C THE ERROR.LOG FILE FOR SEDIMENT TEMPERATURES OUT OF RANGE.  
C          STOP 'ERROR!! INVALID SEDIMENT TEMPERATURE'  
C  
          IF(ISMT(L) .LT. 1) ISMT(L)=1  
          IF(ISMT(L) .GT. NWQTD) ISMT(L) = NWQTD  
        ENDIF  
      ENDDO  
  911 FORMAT(/,'ERROR: TIME, L, I, J, TEM(L,1), SMT(L) = ',
     &        F10.5, 3I4, 2F10.4) 
      ! *** Algal Source Terms 
      DO M=1,NSMG  
        DO L=2,LA  
          SMDFNA = SMFNBC(M)*WQANCC*WQDFBC(L)  
     &        + SMFNBD(M)*WQANCD*WQDFBD(L) + SMFNBG(M)*WQANCG*WQDFBG(L)  
          SMDFPA = ( SMFPBC(M)*WQDFBC(L) + SMFPBD(M)*WQDFBD(L)  
     &        + SMFPBG(M)*WQDFBG(L) ) * WQAPC(L)  
          SMDFCA = SMFCBC(M)*WQDFBC(L) + SMFCBD(M)*WQDFBD(L)   ! *** Algal P  
     &        + SMFCBG(M)*WQDFBG(L)                         
          SMDFN(L,M) = SMDFNA + SMFNR(ISMZMAP(L),M)*WQDFRN(L)  ! *** RPON
          SMDFP(L,M) = SMDFPA + SMFPR(ISMZMAP(L),M)*WQDFRP(L)  ! *** RPOP
          SMDFC(L,M) = SMDFCA + SMFCR(ISMZMAP(L),M)*WQDFRC(L)  ! *** RPOC
        ENDDO  
      ENDDO  
      DO L=2,LA  
        SMDFN(L,1) = SMDFN(L,1) + WQDFLN(L)   
        SMDFP(L,1) = SMDFP(L,1) + WQDFLP(L)   
        SMDFC(L,1) = SMDFC(L,1) + WQDFLC(L)   
      ENDDO
  
      ! *** APPLY OPEN BOUNDARYS 
      DO LL=1,NBCSOP
        L=LOBCS(LL)
        SMDFN(L,1) = 0.0
        SMDFP(L,1) = 0.0
        SMDFC(L,1) = 0.0
      ENDDO 
C  
C: SMW2 IN M/D,SMW2DTOH(IZ)=1.0+SMW2*SMDTOH  
C  
      ! *** ADD SOURCE TERM FROM ALGAE AND THEN ADJUST FOR BURIAL & DECAY
      DO M=1,NSMG  
        DO L=2,LA  
          SMPON(L,M)=(SMPON(L,M) + SMDFN(L,M)*SMDTOH(ISMZMAP(L)))  
     &        / (SMW2DTOH(ISMZMAP(L)) + SMTDND(ISMT(L),M)*DTWQ+ 1.E-18)  
          SMPOP(L,M)=(SMPOP(L,M) + SMDFP(L,M)*SMDTOH(ISMZMAP(L)))  
     &        / (SMW2DTOH(ISMZMAP(L)) + SMTDPD(ISMT(L),M)*DTWQ+ 1.E-18) 
          ! ***       Org C         Algal Src
          SMPOC(L,M)=(SMPOC(L,M) + SMDFC(L,M)*SMDTOH(ISMZMAP(L)))  
     &        / (SMW2DTOH(ISMZMAP(L)) + SMTDCD(ISMT(L),M)*DTWQ+ 1.E-18)  
        ENDDO  
      ENDDO  
  
CPMC      ! *** APPLY OPEN BOUNDARYS   PMC-KEEP THE CONCENTRATIONS, JUST ZERO THE FLUXES
CPMC      
CPMC      DO LL=1,NBCSOP
CPMC        L=LOBCS(LL)
CPMC        DO M=1,NSMG  
CPMC          SMPON(L,M) = 0.0
CPMC          SMPOP(L,M) = 0.0
CPMC          SMPOC(L,M) = 0.0
CPMC        ENDDO
CPMC      ENDDO 

      DO L=2,LA
        SMDGFN(L) = SMHSED(ISMZMAP(L)) *  
     &       (SMTDND(ISMT(L),1)*SMPON(L,1)+SMTDND(ISMT(L),2)*SMPON(L,2))  
        SMDGFP(L) = SMHSED(ISMZMAP(L)) *  
     &       (SMTDPD(ISMT(L),1)*SMPOP(L,1)+SMTDPD(ISMT(L),2)*SMPOP(L,2))  
        SMDGFC(L) = SMHSED(ISMZMAP(L)) *  
     &       (SMTDCD(ISMT(L),1)*SMPOC(L,1)+SMTDCD(ISMT(L),2)*SMPOC(L,2))  
C  
C COMMON PARAMETERS: SMBST1=1/(1+SMKBST*DTWQ),SM1OKMDP=1/SMKMDP  
C: USE SMTMP(L) TO STORE OLD SMBST(L)  
C  
        XSMO20(L) = MAX( WQV(L,1,19), 3.0 )  
        SMTMP(L) = SMBST(L)  
        IF(XSMO20(L).LT.SMKMDP)THEN  
          SMBST(L) =(SMTMP(L)  
     &          +DTWQ*(1.0-XSMO20(L)*SM1OKMDP)) * SMBST1  
        ELSE  
          SMBST(L) = SMBST(L)*SMBST1  
        ENDIF  
      ENDDO  

      ! *** APPLY OPEN BOUNDARYS 
      DO LL=1,NBCSOP
        L=LOBCS(LL)
        SMDGFN(L) = 0.0
        SMDGFP(L) = 0.0
        SMDGFC(L) = 0.0
        SMTMP(L) = 0.0
        SMBST(L) = 0.0
      ENDDO 

      ! *** BENTHIC MIXING USING HYSTERESIS
      IF(ISMHYST.EQ.1)THEN  
        DO ND=1,NDMWQ  
          LF=2+(ND-1)*LDMWQ  
          LL=LF+LDM-1  
          DO L=LF,LL  
            IF(SCB(L).GT.0.5)THEN  
              IF(SMHYST(L))THEN  
                IF(XSMO20(L).GE.SMO2BS) ISMHYPD(L) = ISMHYPD(L) - 1  
                IF(ISMHYPD(L).EQ.0)THEN  
                  SMHYST(L) = .FALSE.  
                  ISMHYPD(L) = 0  
                ENDIF  
                SMBST(L) = SMTMP(L)  
              ELSE  
                IF(XSMO20(L).LT.SMO2BS) ISMHYPD(L) = ISMHYPD(L) + 1  
                IF(ISMHYPD(L).EQ.ISMTCMBS)THEN  
                  SMHYST(L) = .TRUE.  
                  ISMHYPD(L) = ISMTDMBS  
                ENDIF  
              ENDIF  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C: SMDPMIN(IZ)=SMDPMIN/SMHSED  
C  
      DO L=2,LA
        IZ = ISMZMAP(L)  
        SMW12(L) = SMDP(IZ)*SMTDDP(ISMT(L)) * SMPOC(L,1) * XSMO20(L)  
     &        * (1.0-SMKBST*SMBST(L)) / (SMKMDP+XSMO20(L)+ 1.E-18)  
     &        + SMDPMIN(IZ)  
        SMKL12(L) = SMDD(IZ)*SMTDDD(ISMT(L)) + SMRBIBT*SMW12(L)  
      ENDDO  
      ! *** APPLY OPEN BOUNDARYS 
      DO LL=1,NBCSOP
        L=LOBCS(LL)
        SMKL12(L) = 0.0
      ENDDO 

C  
C NH4, NO3: SMKMO2N=SMKMO2N*2,SMKNH4=SMKNH4^2*SMKMNH4,  
C: SMW2PHODT(IZ)=SMW2+SMHODT,SMK1NO3=SMK1NO3^2  
C  
      DO L=2,LA  
        IF(SCB(L).GT.0.5)THEN  
          IZ = ISMZMAP(L)  
          SMO20 = XSMO20(L)  
          SK1NH4SM = ( SMKNH4(IZ)*SMTDNH4(ISMT(L)) * SMO20 )  
     &        / ( (SMKMO2N+SMO20+ 1.E-12) * (SMKMNH4+SM1NH4(L)) )  
          A1NH4SM = SMKL12(L)*SMFD1NH4 + SMW12(L)*SMFP1NH4 + SMW2(IZ)  
          A2NH4SM = SMKL12(L)*SMFD2NH4 + SMW12(L)*SMFP2NH4  
          A22NH4SM = A2NH4SM + SMW2PHODT(IZ)  
          B1NH4SM = WQV(L,1,14)  
          B2NH4SM = SMDGFN(L) + SMHODT(IZ)*SM2NH4(L)  
          SK1NO3SM = SMK1NO3(IZ)*SMTDNO3(ISMT(L))  
          A1NO3SM = SMKL12(L) + SMW2(IZ)  
          A2NO3SM = SMKL12(L)  
          RK2NO3SM = SMK2NO3(IZ)*SMTDNO3(ISMT(L))  
          A22NO3SM = A2NO3SM + SMW2PHODT(IZ) + RK2NO3SM  
          B1NO3SM = WQV(L,1,15)  
          B2NO3SM = SMHODT(IZ)*SM2NO3(L)  
C  
C H2S/CH4  
C: SMK1H2S=(SMKD1HS^2*SMFD1H2S+SMKP1HS^2*SMFP1H2S)/(2*SMKMH2S)*SMTHH2S**  
C: SMTD1CH4(IT)=SMTD1CH4*20  
C  
          SMO2JC = SMO2C*SMDGFC(L)  
          SMSAL0 = SAL(L,1)  
          IF(SMSAL0.GT.SMCSHSCH)THEN  
            SK1H2SSM = SMK1H2S(ISMT(L)) * SMO20  
            A1H2SSM = SMKL12(L)*SMFD1H2S + SMW12(L)*SMFP1H2S + SMW2(IZ)  
            A2H2SSM = SMKL12(L)*SMFD2H2S + SMW12(L)*SMFP2H2S  
            A22H2SSM = A2H2SSM + SMW2PHODT(IZ)  
            B1H2SSM = 0.0  
            B2H2SSM = SMHODT(IZ)*SM2H2S(L)  
          ELSE  
            SMCH4S = (10.0 + HP(L) + SMHSED(IZ)) * SMTD1CH4(ISMT(L))  
     &          * SMKL12(L)  
            SMK1CH4 = SMTD2CH4(ISMT(L))  
          ENDIF  
C  
C BACK SUBSTITUTION TO GET SMSS  
C  
          SMSOD = ZBRENT(ISMERR)  
          IF(DEBUG)THEN
            IF(ISMERR.EQ.1)THEN  
              OPEN(1,FILE='ZBRENT.LOG',STATUS='UNKNOWN',
     &             POSITION='APPEND')  
              WRITE(1,401) ITNWQ,L,IL(L),JL(L),SMSOD,  
     &          ' ROOT MUST BE BRACKETED FOR ZBRENT  '  
              CLOSE(1)  
            ELSE IF(ISMERR.EQ.2)THEN  
              OPEN(1,FILE='ZBRENT.LOG',STATUS='UNKNOWN',
     &             POSITION='APPEND')  
              WRITE(1,401) ITNWQ,L,IL(L),JL(L),SMSOD,  
     &          ' ZBRENT EXCEEDING MAXIMUM ITERATIONS'  
              CLOSE(1)  
            ENDIF  
          ENDIF
          SMSS(L) = RSMSS  
          SM1NH4(L) = RSM1NH4  
          SM2NH4(L) = RSM2NH4  
          SM1NO3(L) = RSM1NO3  
          SM2NO3(L) = RSM2NO3  
          SM1H2S(L) = RSM1H2S  
          SM2H2S(L) = RSM2H2S  
C  
C        WQBFO2(L) = -SMSOD  
C  
          WQBFO2(L) = -SMSOD * SODMULT(IZ)  
          SMCSOD(L) = -CSODSM  
          SMNSOD(L) = -RNSODSM  
          SMJNIT(L) = RJNITSM  
          SMJDEN(L) = RJDENSM
          ! *** COD BASED ON     H2S   OR   CH4
          SMJAQH2S(L) =       AQJH2SSM + AQJCH4SM  
          SMJGCH4(L) = GJCH4SM  
          WQBFNH4(L) = SMSS(L) * (SMFD1NH4*SM1NH4(L) - WQV(L,1,14))  
          WQBFNO3(L) = SMSS(L) * (SM1NO3(L) - WQV(L,1,15))  
          WQBFCOD(L) = SMJAQH2S(L) - SMSS(L)*WQV(L,1,18)  
        ENDIF  
      ENDDO  
C  
C PO4  
C  
      DO L=2,LA  
        IF(SCB(L).GT.0.5)THEN  
          IF(XSMO20(L).LT.SMCO2PO4)THEN  
            SMP1PO4 = SMP2PO4  
     &          * SMDP1PO4(ISMZMAP(L))**(XSMO20(L)/(SMCO2PO4+ 1.E-18))  
          ELSE  
            SMP1PO4 = SMP2PO4 * SMDP1PO4(ISMZMAP(L))  
          ENDIF  
          SMFD1PO4 = 1.0 / (1.0 + SMM1*SMP1PO4)  
          SMFP1PO4 = 1.0 - SMFD1PO4  
          A1PO4SM = SMKL12(L)*SMFD1PO4 + SMW12(L)*SMFP1PO4  
     &        + SMW2(ISMZMAP(L))  
          A2PO4SM = SMKL12(L)*SMFD2PO4 + SMW12(L)*SMFP2PO4  
          A11PO4SM = SMSS(L)*SMFD1PO4 + A1PO4SM  
          A22PO4SM = A2PO4SM + SMW2PHODT(ISMZMAP(L))  
          B11PO4SM = SMSS(L) * WQPO4D(L,1)  
          B22PO4SM = SMDGFP(L) + SMHODT(ISMZMAP(L))*SM2PO4(L)  
          CALL SOLVSMBE(RSM1PO4,RSM2PO4,A11PO4SM,A22PO4SM,A1PO4SM,  
     &        A2PO4SM,B11PO4SM,B22PO4SM)  
          SMD1PO4(L) = SMFD1PO4*RSM1PO4  
          WQBFPO4D(L) = SMSS(L) * (SMD1PO4(L) - WQPO4D(L,1))  
          SM1PO4(L) = RSM1PO4  
          SM2PO4(L) = RSM2PO4  
        ENDIF  
      ENDDO  
C  
C SI  
C  
      IF(IWQSI.EQ.1)THEN  
        DO L=2,LA
          IF(SCB(L).GT.0.5)THEN  
            SMDFSI(L) = (WQASCD*WQDFBD(L) + WQDFSI(L) + SMJDSI)  
     &            * SMDTOH(ISMZMAP(L))  
            WQTT = DTWQ * SMTDSI(ISMT(L)) * (SMSISAT-SMFD2SI*SM2SI(L))  
     &            / (SMPSI(L)+SMKMPSI+ 1.E-18)  
            SMPSI(L) = (SMPSI(L)+SMDFSI(L)) /  
     &            (SMW2DTOH(ISMZMAP(L))+WQTT+ 1.E-18)  
            IF(XSMO20(L).LT.SMCO2SI)THEN  
              SMP1SI = SMP2SI * SMDP1SI**(XSMO20(L)/(SMCO2SI+ 1.E-18))  
            ELSE  
              SMP1SI = SMP2SI * SMDP1SI  
            ENDIF  
            SMFD1SI = 1.0 / (1.0 + SMM1*SMP1SI)  
            SMFP1SI = 1.0 - SMFD1SI  
            A1SISM = SMKL12(L)*SMFD1SI + SMW12(L)*SMFP1SI  
     &            + SMW2(ISMZMAP(L))  
            A2SISM = SMKL12(L)*SMFD2SI + SMW12(L)*SMFP2SI  
            A11SISM = SMSS(L)*SMFD1SI + A1SISM  
            WQTT = SMTDSI(ISMT(L)) * SMPSI(L) * SMHSED(ISMZMAP(L))  
     &            / (SMPSI(L)+SMKMPSI+ 1.E-18)  
            SMJ2SI = WQTT * SMSISAT  
            A22SISM = A2SISM + SMW2PHODT(ISMZMAP(L)) + WQTT*SMFD2SI  
            B11SISM = SMSS(L) * WQSAD(L,1)  
            B22SISM = SMHODT(ISMZMAP(L))*SM2SI(L) + SMJ2SI  
            CALL SOLVSMBE(RSM1SI,RSM2SI,A11SISM,A22SISM,A1SISM,A2SISM,  
     &            B11SISM,B22SISM)  
            SMD1SI(L) = SMFD1SI*RSM1SI  
            WQBFSAD(L) = SMSS(L) * (SMD1SI(L) - WQSAD(L,1))  
            SM1SI(L)  = RSM1SI  
            SM2SI(L)  = RSM2SI  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C KEEP TRACK OF BENTHIC FLUX RATES HERE FOR LATER SAVE TO BINARY FILE:  
C  
      IF(ISMTSB.LT.ISMTSE)THEN  
        DO L=2,LA  
          BFO2SUM(L)  = BFO2SUM(L)  + WQBFO2(L)  
          BFNH4SUM(L) = BFNH4SUM(L) + WQBFNH4(L)  
          BFNO3SUM(L) = BFNO3SUM(L) + WQBFNO3(L)  
          BFPO4SUM(L) = BFPO4SUM(L) + WQBFPO4D(L)  
          BFSADSUM(L) = BFSADSUM(L) + WQBFSAD(L)  
          BFCODSUM(L) = BFCODSUM(L) + WQBFCOD(L)  
          BFSMTSUM(L) = BFSMTSUM(L) + SMT(L)  
          BFBSTSUM(L) = BFBSTSUM(L) + SMBST(L)  
        ENDDO  
      ENDIF
      NBFCNT = NBFCNT + 1  
      IF(ISDYNSTP.EQ.0)THEN  
        TIMTMP=DT*FLOAT(N)+TCON*TBEGIN  
        TIMTMP=TIMTMP/TCTMSR  
      ELSE  
        TIMTMP=TIMESEC/TCTMSR  
      ENDIF  
      TIMEBF = TIMEBF + TIMTMP  
  401 FORMAT(I8,3I5,E12.3,A36)  

      RETURN  
      END  

