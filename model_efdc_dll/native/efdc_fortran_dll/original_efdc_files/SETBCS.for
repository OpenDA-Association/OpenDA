      SUBROUTINE SETBCS  
C  
C CHANGE RECORD  
C  MODIFIED BOUNDARY CONDITION FLAGS FOR TYPE 2 OPEN BOUNDARIES  
C  ADDED REAL FLAGS RSSBCE(L),RSSBCW(L),RSSBCN(L),RSSBCS(L)  
C  TO MODIFIED CALCULATION OF CELL CENTER BED STRESS (STORED AS QQ(L,0))  
C  AND THE OUTPUTED CELL CENTER VELOCITY FOR CELLS HAVE SOURCE/SINKS  
C **  SUBROUTINE SETBCS SETS BOUNDARY CONDITION SWITCHES  
C  
      USE GLOBAL  

      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SUBEW  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SVBNS  

      IF(.NOT.ALLOCATED(SUBEW))THEN
		ALLOCATE(SUBEW(LCM))
		ALLOCATE(SVBNS(LCM))
	    SUBEW=0.0 
	    SVBNS=0.0 
	ENDIF
C  
C **  SET LAND-WATER BOUNDARY SWITCHES  
C  
      ITRICELL=0  ! PMC
      
      DO L=2,LA  
        I=IL(L)  
        J=JL(L)  
        IF(LCT(L).EQ.1)THEN  
          STCUV(L)=0.
          ITRICELL=1  
          STCAP(L)=0.5  
          IF(IJCT(I-1,J).EQ.1) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.2) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.3) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.4) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.5) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.9) SUB(L)=0.  
          IF(IJCT(I,J-1).EQ.1) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.2) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.3) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.4) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.5) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.9) SVB(L)=0.  
        ENDIF  
        IF(LCT(L).EQ.2)THEN  
          STCUV(L)=0.  
          ITRICELL=1  
          STCAP(L)=0.5  
          IF(IJCT(I-1,J).EQ.1) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.2) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.3) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.4) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.5) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.9) SUB(L)=0.  
          IF(IJCT(I,J-1).EQ.1) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.2) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.3) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.4) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.5) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.9) SVB(L)=0.  
        ENDIF  
        IF(LCT(L).EQ.3)THEN  
          STCUV(L)=0.  
          ITRICELL=1  
          STCAP(L)=0.5  
          IF(IJCT(I-1,J).EQ.1) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.2) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.3) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.4) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.9) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.5) SUB(L)=0.  
          IF(IJCT(I,J-1).EQ.1) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.2) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.3) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.4) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.5) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.9) SVB(L)=0.  
        ENDIF  
        IF(LCT(L).EQ.4)THEN  
          STCUV(L)=0.  
          ITRICELL=1  
          STCAP(L)=0.5  
          IF(IJCT(I-1,J).EQ.1) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.2) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.3) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.4) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.5) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.9) SUB(L)=0.  
          IF(IJCT(I,J-1).EQ.1) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.2) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.3) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.4) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.5) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.9) SVB(L)=0.  
        ENDIF  
        IF(LCT(L).EQ.5)THEN  
          IF(IJCT(I-1,J).EQ.1) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.2) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.3) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.4) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.5) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.9) SUB(L)=0.  
          IF(IJCT(I,J-1).EQ.1) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.2) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.3) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.4) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.5) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.9) SVB(L)=0.  
        ENDIF  
        IF(LCT(L).EQ.6)THEN  
          IF(IJCT(I-1,J).EQ.1) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.2) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.3) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.4) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.5) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.6) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.7) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.9) SUB(L)=0.  
          IF(IJCT(I,J-1).EQ.1) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.2) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.3) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.4) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.5) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.6) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.7) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.9) SVB(L)=0.  
        ENDIF  
        IF(LCT(L).EQ.7)THEN  
          IF(IJCT(I-1,J).EQ.1) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.2) SUB(L)=0.  
          IF(IJCT(I-1,J).EQ.3) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.4) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.5) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.6) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.7) SUB(L)=1.  
          IF(IJCT(I-1,J).EQ.9) SUB(L)=0.  
          IF(IJCT(I,J-1).EQ.1) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.2) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.3) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.4) SVB(L)=0.  
          IF(IJCT(I,J-1).EQ.5) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.6) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.7) SVB(L)=1.  
          IF(IJCT(I,J-1).EQ.9) SVB(L)=0.  
        ENDIF  
      ENDDO  
      SUB(1)=0.  
      SVB(1)=0.  
      SUB(LC)=0.  
      SVB(LC)=0. 
C  
C **  MODIFY LAND-WATER BNDRY CONDS FOR PERIOD GRID IN N-S DIRECTION  
C  
      IF(ISPGNS.GE.1)THEN  
        DO NPN=1,NPNSBP  
          LS=LIJ(ISPNS(NPN),JSPNS(NPN))  
          SVB(LS)=1.  
          SVBO(LS)=1.  
        ENDDO  
      ENDIF  
C  
C **  SET WATER-WATER (P OR SURFACE ELEVATION) BOUNDARY SWITCHES  
C  
      DO LL=1,NPBW  
        I=IPBW(LL)  
        J=JPBW(LL)  
        L=LIJ(I,J)  
        LPBW(LL)=L  
        SPB(L)=0.       ! *** Used for On/Off Rainfall/Evap
        SUB(L)=0.  
        SVB(L)=0.  
        SWB(L)=0.       ! *** Used for On/Off of Vertical Velocities
        SAAX(L)=0.      ! *** Used for On/Off of Horizontal Momentum Stresses (X Dir) PMC-Added
        SAAY(L)=0.      ! *** Used for On/Off of Horizontal Momentum Stresses (Y Dir) PMC-Added
        IF(ISPBW(LL).LE.1) THEN  
          SVB(L+1)=0.  
          SWB(L+1)=0.  
          SCAX(L+1)=0.  ! *** Used for On/Off of Coriolis & Curvature Stresses  
        END IF  
        SAAX(L+1)=0.    ! *** Used for On/Off of Horizontal Momentum Stresses (X Dir)
        SAAY(L+1)=0.    ! *** Used for On/Off of Horizontal Momentum Stresses (Y Dir) PMC-Added
        !SDX(L+1)=0.    ! *** Used for On/Off of Horizontal Momentum Diffusion Stresses PMC-Disabled
      ENDDO  
      DO LL=1,NPBE  
        I=IPBE(LL)  
        J=JPBE(LL)  
        L=LIJ(I,J)  
        LPBE(LL)=L  
        SPB(L)=0.  
        SVB(L)=0.  
        SWB(L)=0.  
        IF(ISPBE(LL).LE.1) THEN  
          SWB(L-1)=0.
          SVB(L-1)=0.  
          SCAX(L)=0.  
        END IF  
        SAAY(L)=0.  ! PMC
        SAAX(L)=0.  
        !SDX(L)=0.  
      ENDDO  
      DO LL=1,NPBS  
        I=IPBS(LL)  
        J=JPBS(LL)  
        L=LIJ(I,J)  
        LPBS(LL)=L  
        LN=LNC(L)  
        SPB(L)=0.  
        SVB(L)=0.  
        SUB(L)=0.  
        SWB(L)=0.  
        IF(ISPBS(LL).LE.1) THEN  
          SUB(LN)=0.  
          SWB(LN)=0.  
          SCAY(LN)=0.  
        END IF  
        SAAX(L)=0.  ! PMC
        SAAY(L)=0.  ! PMC
        SAAX(LN)=0.  ! PMC
        SAAY(LN)=0.  
        !SDY(LN)=0.  
      ENDDO  
      DO LL=1,NPBN  
        I=IPBN(LL)  
        J=JPBN(LL)  
        L=LIJ(I,J)  
        LPBN(LL)=L  
        LS=LSC(L)  
        SPB(L)=0.  
        SUB(L)=0.  
        SWB(L)=0.  
        IF(ISPBN(LL).LE.1) THEN  
          SUB(LS)=0.  
          SWB(LS)=0.  
          SCAY(L)=0.  
        END IF  
        SAAX(L)=0.  ! PMC
        SAAY(L)=0.  
        !SDY(L)=0.  
      ENDDO  
C
C *********************************************************************
C *** SET THE CELL FACES SWITCHES FOR HEAD CONTROL STRUCTURES
      ! *** UPSTREAM CONTROL
      DO NCTL=1,NQCTL  
        IU=IQCTLU(NCTL)  
        JU=JQCTLU(NCTL)  
        L=LIJ(IU,JU)  

        ! *** SET U FACE
        LW=L-1
        DO IQ=1,NQCTL
          IF(IQ.NE.NCTL)THEN
            I=IQCTLU(IQ)  
            J=JQCTLU(IQ)  
            L1=LIJ(I,J)
            IF(L1.EQ.LW)THEN
              SUB(L)=0.0 
              EXIT
            ENDIF
          ENDIF
        ENDDO

        ! *** SET V FACE
        LS=LSC(L)
        DO IQ=1,NQCTL
          IF(IQ.NE.NCTL)THEN
            I=IQCTLU(IQ)  
            J=JQCTLU(IQ)  
            L1=LIJ(I,J)
            IF(L1.EQ.LS)THEN
              SVB(L)=0.0   
              EXIT
            ENDIF
          ENDIF
        ENDDO
      ENDDO  

      ! *** DOWNSTREAM CONTROL
      DO NCTL=1,NQCTL  
        ID=IQCTLD(NCTL)  
        JD=JQCTLD(NCTL)  
        IF(ID.NE.0.AND.JD.NE.0)THEN  
          L=LIJ(ID,JD)  

          ! *** SET U FACE
          LW=L-1
          DO IQ=1,NQCTL
            IF(IQ.NE.NCTL)THEN
              I=IQCTLD(IQ)  
              J=JQCTLD(IQ)
              IF(I.GT.0.AND.J.GT.0)THEN  ! PMC
                L1=LIJ(I,J)
                IF(L1.EQ.LW)THEN
                  SUB(L)=0.0  
                  EXIT
                ENDIF 
              ENDIF
            ENDIF
          ENDDO

          ! *** SET V FACE
          LS=LSC(L)
          DO IQ=1,NQCTL
            IF(IQ.NE.NCTL)THEN
              I=IQCTLD(IQ)  
              J=JQCTLD(IQ)  
              IF(I.GT.0.AND.J.GT.0)THEN  ! PMC
                L1=LIJ(I,J)
                IF(L1.EQ.LS)THEN
                  SVB(L)=0.0  
                  EXIT
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO  
C  
C ** RESET DXU,DYU,DXV,DYV BASED ON BOUNDARY CONDITION SWITCHES  
C  
      DO L=2,LA  
        IF(SUB(L).GT.0.5)THEN  
          DXU(L)=0.5*(DXP(L)+DXP(L-1))  
          DYU(L)=0.5*(DYP(L)+DYP(L-1))  
        ENDIF  
        IF(SUB(L).LT.0.5.AND.SUB(L+1).GT.0.5)THEN  
          DXU(L)=DXP(L)  
          DDYDDDX=2.*(DYP(L+1)-DYP(L))/(DXP(L)+DXP(L+1))  
          DYU(L)=DYP(L)-0.5*DXP(L)*DDYDDDX  
        ENDIF  
        IF(SUB(L).LT.0.5.AND.SUB(L+1).LT.0.5)THEN  
          DXU(L)=DXP(L)  
          DYU(L)=DYP(L)  
        ENDIF  
      ENDDO  
      DO L=2,LA  
        LN=LNC(L)  
        LS=LSC(L)  
        IF(SVB(L).GT.0.5)THEN  
          DXV(L)=0.5*(DXP(L)+DXP(LS))  
          DYV(L)=0.5*(DYP(L)+DYP(LS))  
        ENDIF  
        IF(SVB(L).LT.0.5.AND.SVB(LN).GT.0.5)THEN  
          DDXDDDY=2.*(DXP(LN)-DXP(L))/(DYP(L)+DYP(LN))  
          DXV(L)=DXP(L)-0.5*DYP(L)*DDXDDDY  
          DYV(L)=DYP(L)  
        ENDIF  
        IF(SVB(L).LT.0.5.AND.SVB(LN).LT.0.5)THEN  
          DXV(L)=DXP(L)  
          DYV(L)=DYP(L)  
        ENDIF  
      ENDDO  
C  
C **  SET THIN BARRIERS BY CALLING CELLMASK  
C **  CALL MOVED FROM AAEFDC ON 23 JAN 2004  
C  
      IF(ISMASK.EQ.1) CALL CELLMASK  
C  
C **  SET VOLUMETRIC & CONCENTRATION SOURCE LOCATIONS AND BED STRESS  
C **  AND CELL CENTER BED STRESS AND VELOCITY MODIFERS  
C  
      DO LL=1,NQSIJ  
        I=IQS(LL)  
        J=JQS(LL)  
        LTMP=LIJ(I,J)  
        LQS(LL)=LTMP  
        IF(NQSMUL(LL).EQ.0)RQSMUL(LL)=1.  
        IF(NQSMUL(LL).EQ.1)RQSMUL(LL)=DYP(LTMP)  
        IF(NQSMUL(LL).EQ.2)RQSMUL(LL)=DXP(LTMP)  
        IF(NQSMUL(LL).EQ.3)RQSMUL(LL)=DXP(LTMP)+DYP(LTMP)  
      ENDDO  
      DO NCTL=1,NQCTL  
        RQDW=1.  
        IU=IQCTLU(NCTL)  
        JU=JQCTLU(NCTL)  
        LTMP=LIJ(IU,JU)  
        IF(NQCMUL(NCTL).EQ.0)RQCMUL(NCTL)=1.  
        IF(NQCMUL(NCTL).EQ.1)RQCMUL(NCTL)=DYP(LTMP)  
        IF(NQCMUL(NCTL).EQ.2)RQCMUL(NCTL)=DXP(LTMP)  
        IF(NQCMUL(NCTL).EQ.3)RQCMUL(NCTL)=DXP(LTMP)+DYP(LTMP)  
      ENDDO  
C
C *********************************************************************
C *** SET THE VELOCITY AVERAGING FACTORS

      ! *** DEFAULT CONDITION
      DO L=2,LA  
        RSSBCE(L)=1.0  
        RSSBCW(L)=1.0  
        RSSBCN(L)=1.0  
        RSSBCS(L)=1.0  
        SUBEW(L)=SUB(L)+SUB(L+1)  
        SVBNS(L)=SVB(L)+SVB(LNC(L))  
      ENDDO  

      ! *** FLOW BOUNDARY CONDITIONS
      DO LL=1,NQSIJ  
        L=LQS(LL)  
        LE=L+1  
        LN=LNC(L)  
        IF(SUBEW(L).LT.1.5)THEN  
          IF(SUB(L).LT.0.5.AND.SUB(LE).GT.0.5)THEN  
            RSSBCW(L)=0.0  
            RSSBCE(L)=2.0  
          ENDIF  
          IF(SUB(L).GT.0.5.AND.SUB(LE).LT.0.5)THEN  
            RSSBCW(L)=2.0  
            RSSBCE(L)=0.0  
          ENDIF  
        ENDIF  
        IF(SVBNS(L).LT.1.5)THEN  
          IF(SVB(L).LT.0.5.AND.SVB(LN).GT.0.5)THEN  
            RSSBCS(L)=0.0  
            RSSBCN(L)=2.0  
          ENDIF  
          IF(SVB(L).GT.0.5.AND.SVB(LN).LT.0.5)THEN  
            RSSBCS(L)=2.0  
            RSSBCN(L)=0.0  
          ENDIF  
        ENDIF  
      ENDDO  

      ! *** WEIR STRUCTURE: UPSTREAM
      DO NCTL=1,NQCTL  
        IU=IQCTLU(NCTL)  
        JU=JQCTLU(NCTL)  
        L=LIJ(IU,JU)  
        LE=L+1  
        LN=LNC(L)  
        IF(SUBEW(L).LT.1.5)THEN  
          IF(SUB(L).LT.0.5.AND.SUB(LE).GT.0.5)THEN  
            RSSBCW(L)=0.0  
            RSSBCE(L)=2.0  
          ENDIF  
          IF(SUB(L).GT.0.5.AND.SUB(LE).LT.0.5)THEN  
            RSSBCW(L)=2.0  
            RSSBCE(L)=0.0  
          ENDIF  
        ENDIF  
        IF(SVBNS(L).LT.1.5)THEN  
          IF(SVB(L).LT.0.5.AND.SVB(LN).GT.0.5)THEN  
            RSSBCS(L)=0.0  
            RSSBCN(L)=2.0  
          ENDIF  
          IF(SVB(L).GT.0.5.AND.SVB(LN).LT.0.5)THEN  
            RSSBCS(L)=2.0  
            RSSBCN(L)=0.0  
          ENDIF  
        ENDIF  
      ENDDO  

      ! *** WEIR STRUCTURE: DOWNSTREAM
      DO NCTL=1,NQCTL  
        ID=IQCTLD(NCTL)  
        JD=JQCTLD(NCTL)  
        IF(ID.NE.0.AND.JD.NE.0)THEN  
          L=LIJ(ID,JD)  
          LE=L+1  
          LN=LNC(L)
          IF(SUBEW(L).LT.1.5)THEN  
            IF(SUB(L).LT.0.5.AND.SUB(LE).GT.0.5)THEN  
              RSSBCW(L)=0.0  
              RSSBCE(L)=2.0  
            ENDIF  
            IF(SUB(L).GT.0.5.AND.SUB(LE).LT.0.5)THEN  
              RSSBCW(L)=2.0  
              RSSBCE(L)=0.0  
            ENDIF  
          ENDIF  
          IF(SVBNS(L).LT.1.5)THEN  
            IF(SVB(L).LT.0.5.AND.SVB(LN).GT.0.5)THEN  
              RSSBCS(L)=0.0  
              RSSBCN(L)=2.0
            ENDIF  
            IF(SVB(L).GT.0.5.AND.SVB(LN).LT.0.5)THEN  
              RSSBCS(L)=2.0  
              RSSBCN(L)=0.0  
            ENDIF  
          ENDIF  
        END IF  
      ENDDO  

      ! *** GLOBAL BOUNDARY CELL LIST
      NBCS=0 
 
      ! *** WITHDRAWAL & RETURN BOUNDARY CONDITIONS: UPSTREAM
      DO NWR=1,NQWR
        IU=IQWRU(NWR)  
        JU=JQWRU(NWR)  
        L=LIJ(IU,JU)  
        NBCS=NBCS+1
        LBCS(NBCS)=L
        LBERC(NBCS)=1
        LBNRC(NBCS)=1
        LE=L+1  
        LN=LNC(L)  
        IF(SUBEW(L).LT.1.5)THEN  
          IF(SUB(L).LT.0.5.AND.SUB(LE).GT.0.5)THEN  
            RSSBCW(L)=0.0  
            RSSBCE(L)=2.0  
          ENDIF  
          IF(SUB(L).GT.0.5.AND.SUB(LE).LT.0.5)THEN  
            RSSBCW(L)=2.0  
            RSSBCE(L)=0.0  
          ENDIF  
        ENDIF  
        IF(SVBNS(L).LT.1.5)THEN  
          IF(SVB(L).LT.0.5.AND.SVB(LN).GT.0.5)THEN  
            RSSBCS(L)=0.0  
            RSSBCN(L)=2.0  
          ENDIF  
          IF(SVB(L).GT.0.5.AND.SVB(LN).LT.0.5)THEN  
            RSSBCS(L)=2.0  
            RSSBCN(L)=0.0  
          ENDIF  
        ENDIF  
      ENDDO

      ! *** WITHDRAWAL & RETURN BOUNDARY CONDITIONS: DOWNSTREAM
      DO NWR=1,NQWR  
        ID=IQWRD(NWR)  
        JD=JQWRD(NWR)  
        L=LIJ(ID,JD)  
        NBCS=NBCS+1
        LBCS(NBCS)=L
        LBERC(NBCS)=1
        LBNRC(NBCS)=1
        LE=L+1  
        LN=LNC(L)  
        IF(SUBEW(L).LT.1.5)THEN  
          IF(SUB(L).LT.0.5.AND.SUB(LE).GT.0.5)THEN  
            RSSBCW(L)=0.0  
            RSSBCE(L)=2.0  
          ENDIF  
          IF(SUB(L).GT.0.5.AND.SUB(LE).LT.0.5)THEN  
            RSSBCW(L)=2.0  
            RSSBCE(L)=0.0  
          ENDIF  
        ENDIF  
        IF(SVBNS(L).LT.1.5)THEN  
          IF(SVB(L).LT.0.5.AND.SVB(LN).GT.0.5)THEN  
            RSSBCS(L)=0.0  
            RSSBCN(L)=2.0  
          ENDIF  
          IF(SVB(L).GT.0.5.AND.SVB(LN).LT.0.5)THEN  
            RSSBCS(L)=2.0  
            RSSBCN(L)=0.0  
          ENDIF  
        ENDIF  
      ENDDO  

      ! *** SET BOUNDARY MOMENTUM SWITCHES FOR FLOW & HEAD CONTROL

      ! *** FLOW BC'S
      DO LL=1,NQSIJ  
        I=IQS(LL)  
        J=JQS(LL)  
        L=LIJ(I,J)  
        NBCS=NBCS+1
        LBCS(NBCS)=L
        
        ! *** SET SAAX & SAAY FOR BOUNDARY MOMENTUM FLUXES
        ! *** EAST/WEST MOMENTUM
        LBERC(NBCS)=L
        IF(SUB(L).LT.0.5)THEN
          SAAX(L)=0.  
          SAAY(L)=0.  
        ENDIF
        IF(L.LT.LA-2)THEN
          IF(SUB(L).LT.0.5.AND.(SUB(L+1).GT.0.5.AND.SUB(L+2).GT.0.5))
     &                                                             THEN
            LBERC(NBCS)=L+1
            SAAX(LBERC(NBCS))=0.
            SAAY(LBERC(NBCS))=0.  
          ENDIF
        ENDIF
        IF(L.GT.2.AND.L.LT.LA)THEN
          IF((SUB(L  ).GT.0.5.AND.SUB(L+1).LT.0.5).AND.
     &     (SUB(L-1).GT.0.5.AND.SUB(L-2).GT.0.5))THEN
            LBERC(NBCS)=L-1
            SAAX(LBERC(NBCS))=0.            
            SAAY(LBERC(NBCS))=0.  
            SAAX(L)=0.  
            SAAY(L)=0.    
          ENDIF
        ENDIF
        ! *** NORTH/SOUTH MOMENTUM
        LBNRC(NBCS)=L
        IF(SVB(L).LT.0.5)THEN
          SAAX(L)=0.  
          SAAY(L)=0.
        ENDIF
        IF(SVB(L).LT.0.5.AND.(SVB(LNC(L)).GT.0.5.AND.
     &                        SVB(LNC(LNC(L))).GT.0.5))THEN
          LBNRC(NBCS)=LNC(L)
          SAAX(LBNRC(NBCS))=0. 
          SAAY(LBNRC(NBCS))=0.
        ENDIF
        IF((SVB(L     ).GT.0.5.AND.SVB(LNC(L)).LT.0.5).AND.
     &     (SVB(LSC(L)).GT.0.5.AND.SVB(LSC(LSC(L))).GT.0.5))THEN
          LBNRC(NBCS)=LSC(L)
          SAAX(LBNRC(NBCS))=0.  
          SAAY(LBNRC(NBCS))=0.
          SAAX(L)=0.    
          SAAY(L)=0.
        ENDIF

      ENDDO

      ! *** HEAD CONTROL: UPSTREAM
      DO NCTL=1,NQCTL  
        RQDW=1.  
        IU=IQCTLU(NCTL)  
        JU=JQCTLU(NCTL)  
        L=LIJ(IU,JU)  
        NBCS=NBCS+1
        LBCS(NBCS)=L

        ! *** SET SAAX & SAAY FOR BOUNDARY MOMENTUM FLUXES
        ! *** EAST/WEST MOMENTUM
        LBERC(NBCS)=L
        IF(SUB(L).LT.0.5)THEN
          SAAX(L)=0.  
          SAAY(L)=0.  ! PMC
        ENDIF
c        IF(SUB(L).LT.0.5.AND.(SUB(L+1).GT.0.5.AND.SUB(L+2).GT.0.5))THEN
c          LBERC(NBCS)=L+1
c          SAAX(LBERC(NBCS))=0.
c          SAAY(LBERC(NBCS))=0.  
c        ENDIF
        if(L>=3)then !  added to avoid SUB(0)
          IF((SUB(L  ).GT.0.5.AND.SUB(L+1).LT.0.5).AND.
     &     (SUB(L-1).GT.0.5.AND.SUB(L-2).GT.0.5))THEN
            LBERC(NBCS)=L-1
            SAAX(LBERC(NBCS))=0.
            SAAY(LBERC(NBCS))=0.  
            SAAX(L)=0.  
            SAAY(L)=0.    
          ENDIF
        endif
        ! *** NORTH/SOUTH MOMENTUM
        LBNRC(NBCS)=L
        IF(SVB(L).LT.0.5)THEN
          SAAX(L)=0.  
          SAAY(L)=0.
        ENDIF
c        IF(SVB(L).LT.0.5.AND.(SVB(LNC(L)).GT.0.5.AND.
c     &                        SVB(LNC(LNC(L))).GT.0.5))THEN
c          LBNRC(NBCS)=LNC(L)
c          SAAX(LBNRC(NBCS))=0. 
c          SAAY(LBNRC(NBCS))=0.
c        ENDIF
        IF((SVB(L     ).GT.0.5.AND.SVB(LNC(L)).LT.0.5).AND.
     &     (SVB(LSC(L)).GT.0.5.AND.SVB(LSC(LSC(L))).GT.0.5))THEN
          LBNRC(NBCS)=LSC(L)
          SAAX(LBNRC(NBCS))=0. 
          SAAY(LBNRC(NBCS))=0.
          SAAX(L)=0.   
          SAAY(L)=0.
        ENDIF

      ENDDO  

      ! *** HEAD CONTROL: DOWNSTREAM
      DO NCTL=1,NQCTL  
        ID=IQCTLD(NCTL)  
        JD=JQCTLD(NCTL)  
        IF(ID.NE.0.AND.JD.NE.0)THEN  
          L=LIJ(ID,JD)  
          NBCS=NBCS+1
          LBCS(NBCS)=L
          LBERC(NBCS)=1
          LBNRC(NBCS)=1
        ENDIF
      ENDDO

      ! *** SET BOUNDARY VELOCITY SWITCHES
      ! *** OPEN BOUNDARIES
      NBCSOP=0
      DO LL=1,NPBS  
        I=IPBS(LL)  
        J=JPBS(LL)  
        L=LIJ(I,J)  
        RSSBCS(L)=0.0  
        RSSBCN(L)=2.0  
        ! *** SAVE THE L'S 
        NBCSOP=NBCSOP+1
        LOBCS(NBCSOP)=L
        NBCS=NBCS+1
        LBCS(NBCS)=L
        LBERC(NBCS)=L       ! PMC-CHANGE THE NAME OF LBERC TO LBCE
        LBNRC(NBCS)=LNC(L)  ! PMC-CHANGE THE NAME OF LBNRC TO LBCN
      ENDDO  
      DO LL=1,NPBW  
        I=IPBW(LL)  
        J=JPBW(LL)  
        L=LIJ(I,J)  
        RSSBCW(L)=0.0  
        RSSBCE(L)=2.0  
        ! *** SAVE THE L'S 
        NBCSOP=NBCSOP+1
        LOBCS(NBCSOP)=L
        NBCS=NBCS+1
        LBCS(NBCS)=L
        LBERC(NBCS)=L+1
        LBNRC(NBCS)=L
      ENDDO  
      DO LL=1,NPBE  
        I=IPBE(LL)  
        J=JPBE(LL)  
        L=LIJ(I,J)  
        RSSBCW(L)=2.0  
        RSSBCE(L)=0.0  
        ! *** SAVE THE L'S 
        NBCSOP=NBCSOP+1
        LOBCS(NBCSOP)=L
        NBCS=NBCS+1
        LBCS(NBCS)=L
        LBERC(NBCS)=L-1
        LBNRC(NBCS)=L
      ENDDO  
      DO LL=1,NPBN  
        I=IPBN(LL)  
        J=JPBN(LL)  
        L=LIJ(I,J)  
        RSSBCS(L)=2.0  
        RSSBCN(L)=0.0  
        ! *** SAVE THE L'S 
        NBCSOP=NBCSOP+1
        LOBCS(NBCSOP)=L
        NBCS=NBCS+1
        LBCS(NBCS)=L
        LBERC(NBCS)=L
        LBNRC(NBCS)=LSC(L)
      ENDDO  
C
C *********************************************************************
C ***  SET OPEN BOUNDARY FLAGS FOR CONSTITUENTS
      DO LL=1,NCBS  
        I=ICBS(LL)  
        J=JCBS(LL)  
        LCBS(LL)=LIJ(I,J)  
        L=LIJ(I,J)  
        SCB(L)=0.  
      ENDDO  
      DO LL=1,NCBW  
        I=ICBW(LL)  
        J=JCBW(LL)  
        LCBW(LL)=LIJ(I,J)  
        L=LIJ(I,J)  
        SCB(L)=0.  
      ENDDO  
      DO LL=1,NCBE  
        I=ICBE(LL)  
        J=JCBE(LL)  
        LCBE(LL)=LIJ(I,J)  
        L=LIJ(I,J)  
        SCB(L)=0.  
      ENDDO  
      DO LL=1,NCBN  
        I=ICBN(LL)  
        J=JCBN(LL)  
        LCBN(LL)=LIJ(I,J)  
        L=LIJ(I,J)  
        SCB(L)=0.  
      ENDDO  

C *********************************************************************
C ***  SET JET-PLUME VOLUMES SOURCES 
      DO NJP=1,NQJPIJ  
        L=LIJ(IQJP(NJP),JQJP(NJP))  
        NBCS=NBCS+1
        LBCS(NBCS)=L
        LBERC(NBCS)=1
        LBNRC(NBCS)=1

        IF(ICALJP(NJP).EQ.2)THEN  
          ! *** WITHDRAWAL CELL
          L=LIJ(IUPCJP(NJP),JUPCJP(NJP))  
          NBCS=NBCS+1
          LBCS(NBCS)=L
          LBERC(NBCS)=1
          LBNRC(NBCS)=1
        ENDIF
      ENDDO
      
C  
C **  SET CHANNEL HOST AND GUEST LOCATION MAPPINGS  
C  
      IF(MDCHH.GE.1)THEN  
        DO NMD=1,MDCHH
          L=LIJ(IMDCHH(NMD),JMDCHH(NMD))    
          LMDCHH(NMD)=L
          NBCS=NBCS+1
          LBCS(NBCS)=L
          IF(IMDCHU(NMD).EQ.1.AND.JMDCHU(NMD).EQ.1)THEN  
            LMDCHU(NMD)=1  
          ELSE  
            L=LIJ(IMDCHU(NMD),JMDCHU(NMD))  
            LMDCHU(NMD)=L
          ENDIF  
          IF(IMDCHV(NMD).EQ.1.AND.JMDCHV(NMD).EQ.1)THEN  
            LMDCHV(NMD)=1  
          ELSE  
            L=LIJ(IMDCHV(NMD),JMDCHV(NMD))  
            LMDCHV(NMD)=L
          ENDIF  
          NBCS=NBCS+1
          LBCS(NBCS)=L
        ENDDO  
      ENDIF  
C  
C **  SET CELL FACE WET DEPTHS  
C  
      HUWET(1)=HWET  
      HUWET(LC)=HWET  
      HVWET(1)=HWET  
      HVWET(LC)=HWET  
      HUDRY(1)=HDRY  
      HUDRY(LC)=HDRY  
      HVDRY(1)=HDRY  
      HVDRY(LC)=HDRY  
      DO L=2,LA  
        LS=LSC(L)  
        HUDRY(L)=HDRY+0.5*ABS(BELV(L)-BELV(L-1))  
        HVDRY(L)=HDRY+0.5*ABS(BELV(L)-BELV(LS))  
        HUWET(L)=HWET+0.5*ABS(BELV(L)-BELV(L-1))  
        HVWET(L)=HWET+0.5*ABS(BELV(L)-BELV(LS))  
      ENDDO  
      IF(ISDRY.GT.0)THEN  
        NDRYTMP=MOD(ISDRY,2)  
        IF(NDRYTMP.NE.0)THEN  
          DO L=2,LA  
            HUWET(L)=HWET  
            HVWET(L)=HWET  
            HUDRY(L)=HDRY  
            HVDRY(L)=HDRY  
          ENDDO  
        ENDIF  
      ENDIF  
C 
C *** SET PERMANENT FACE SWITCHES
C
      DO L=1,LC  
        SUBO(L)=SUB(L)  
        SVBO(L)=SVB(L)  
      ENDDO  
C  
C **  DIAGNOSTIC OUTPUT  
C  
      IF(DEBUG)THEN
        OPEN(1,FILE='SETBC.DIA',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='SETBC.DIA')  
        DO L=2,LA  
          WRITE(1,1001)IL(L),JL(L),SUB(L),SUB(L+1),SVB(L),SVB(LNC(L)),  
     &      SPB(L)  
        ENDDO  
        CLOSE(1)  
      ENDIF
 1001 FORMAT(2I5,8E13.4)  
      RETURN  
      END  

