      SUBROUTINE model_restart_init

C     These are the parts of RESTIN1.for after data is read from:
C     RESTART.INP [lines: 213-402]
C     RSTWD.INP   [lines: 417-512]
C     TEMP.RST    [lines: 532-535]

      USE GLOBAL
      
      IMPLICIT NONE
     

C     implicit variables from RESTIN1.for
      INTEGER :: K, L, LS, LN, ISBELVC, IDFLAG, ICORDRY
      REAL :: TMPVAL, RDZC, RDRY
      REAL :: HDRY2, DHPDT
      REAL :: SUBW, SUBE, SVBS, SVBN

      ISBELVC =0
C     INSERTED FROM RESTIN1.for [lines: 213-402] 
      DO K=1,KC  
        SAL(1,K)=0.  
        TEM(1,K)=0.  
        DYE(1,K)=0.  
        SED(1,K,1)=0.  
        SND(1,K,1)=0.  
        TOX(1,K,1)=0.  
        SFL(1,K)=0.  
        CWQ(1,K)=0.  
        VHDX(1,K)=0.  
        UHDY(1,K)=0.  
        SAL1(1,K)=0.  
        TEM1(1,K)=0.  
        DYE1(1,K)=0.  
        SED1(1,K,1)=0.  
        SND1(1,K,1)=0.  
        TOX1(1,K,1)=0.  
        SFL2(1,K)=0.  
        CWQ2(1,K)=0.  
        VHDX1(1,K)=0.  
        UHDY1(1,K)=0.  
        VHDXWQ(1,K)=0.  
        UHDYWQ(1,K)=0.  
        SAL(LC,K)=0.  
        TEM(LC,K)=0.  
        DYE(LC,K)=0.  
        SED(LC,K,1)=0.  
        SND(LC,K,1)=0.  
        TOX(LC,K,1)=0.  
        SFL(LC,K)=0.  
        CWQ(LC,K)=0.  
        VHDX(LC,K)=0.  
        UHDY(LC,K)=0.  
        SAL1(LC,K)=0.  
        TEM1(LC,K)=0.  
        DYE1(LC,K)=0.  
        SED1(LC,K,1)=0.  
        SND1(LC,K,1)=0.  
        TOX1(LC,K,1)=0.  
        SFL2(LC,K)=0.  
        CWQ2(LC,K)=0.  
        VHDX1(LC,K)=0.  
        UHDY1(LC,K)=0.  
        VHDXWQ(LC,K)=0.  
        UHDYWQ(LC,K)=0.  
      ENDDO  
      DO L=2,LA  
        UHDYE(L)=SUB(L)*UHDYE(L)  
        UHDY1E(L)=SUB(L)*UHDY1E(L)  
        VHDXE(L)=SVB(L)*VHDXE(L)  
        VHDX1E(L)=SVB(L)*VHDX1E(L)  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          U(L,K)=SUB(L)*U(L,K)  
          U1(L,K)=SUB(L)*U1(L,K)  
          V(L,K)=SVB(L)*V(L,K)  
          V1(L,K)=SVB(L)*V1(L,K)  
        ENDDO  
      ENDDO  
      DO L=2,LA  
        LS=LSC(L)  
        H1U(L)=0.5*(DXP(L)*DYP(L)*H1P(L)+DXP(L-1)*DYP(L-1)*H1P(L-1))  
     &      /(DXU(L)*DYU(L))  
        H1V(L)=0.5*(DXP(L)*DYP(L)*H1P(L)+DXP(LS )*DYP(L-1)*H1P(LS ))  
     &      /(DXV(L)*DYV(L))  
        P1(L)=G*(H1P(L)+BELV(L))  
        HU(L)=0.5*(DXP(L)*DYP(L)*HP(L)+DXP(L-1)*DYP(L-1)*HP(L-1))  
     &      /(DXU(L)*DYU(L))  
        HV(L)=0.5*(DXP(L)*DYP(L)*HP(L)+DXP(LS )*DYP(L-1)*HP(LS ))  
     &      /(DXV(L)*DYV(L))  
        P(L)=G*(HP(L)+BELV(L))  
        HPI(L)=1./HP(L)  
        HUI(L)=1./HU(L)  
        HVI(L)=1./HV(L)  
        H1UI(L)=1./H1U(L)  
        H1VI(L)=1./H1V(L)  
      ENDDO  
      H1U(1)=H1U(2)  
      H1V(1)=H1V(2)  
      P1(1)=P1(2)  
      HU(1)=HU(2)  
      HV(1)=HV(2)  
      P(1)=P(2)  
      HPI(1)=1./HP(2)  
      HUI(1)=1./HU(2)  
      HVI(1)=1./HV(2)  
      H1UI(1)=1./H1U(2)  
      H1VI(1)=1./H1V(2)  
      H1U(LC)=H1U(LA)  
      H1V(LC)=H1V(LA)  
      P1(LC)=P1(LA)  
      HU(LC)=HU(LA)  
      HV(LC)=HV(LA)  
      P(LC)=P(LA)  
      HPI(LC)=1./HP(LA)  
      HUI(LC)=1./HU(LA)  
      HVI(LC)=1./HV(LA)  
      H1UI(LC)=1./H1U(LA)  
      H1VI(LC)=1./H1V(LA)  
      DO K=1,KC  
        DO L=2,LA  
          UHDY1(L,K)=DYU(L)*H1U(L)*U1(L,K)  
          VHDX1(L,K)=DXV(L)*H1V(L)*V1(L,K)  
          UHDY(L,K)=DYU(L)*HU(L)*U(L,K)  
          VHDX(L,K)=DXV(L)*HV(L)*V(L,K)  
          SAL(L,K)=MAX(SAL(L,K),0.0)  
          SAL1(L,K)=MAX(SAL1(L,K),0.0)  
        ENDDO  
      ENDDO  
C  
C **  CORRECT FOR CHANGED BOTTOM ELEV  
C  
      IF(ISRESTI.EQ.-1.AND.ISBELVC.EQ.1)THEN  
        DO L=2,LA  
          UHE(L)=0.  
          VHE(L)=0.  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
            UHE(L)=UHE(L)+UHDY1(L,K)  
            VHE(L)=VHE(L)+VHDX1(L,K)  
          ENDDO  
        ENDDO  
        DO L=2,LA  
          IF(UHE(L).NE.0.)THEN  
            TMPVAL=UHDY1E(L)/UHE(L)  
            DO K=1,KC  
              U1(L,K)=TMPVAL*U1(L,K)  
            ENDDO  
          ENDIF  
          IF(VHE(L).NE.0.)THEN  
            TMPVAL=VHDX1E(L)/VHE(L)  
            DO K=1,KC  
              V1(L,K)=TMPVAL*V1(L,K)  
            ENDDO  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          UHE(L)=0.  
          VHE(L)=0.  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
            UHE(L)=UHE(L)+UHDY(L,K)  
            VHE(L)=VHE(L)+VHDX(L,K)  
          ENDDO  
        ENDDO  
        DO L=2,LA  
          IF(UHE(L).NE.0.)THEN  
            TMPVAL=UHDYE(L)/UHE(L)  
            DO K=1,KC  
              U(L,K)=TMPVAL*U(L,K)  
            ENDDO  
          ENDIF  
          IF(VHE(L).NE.0.)THEN  
            TMPVAL=VHDXE(L)/VHE(L)  
            DO K=1,KC  
              V(L,K)=TMPVAL*V(L,K)  
            ENDDO  
          ENDIF  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
            UHDY1(L,K)=DYU(L)*H1U(L)*U1(L,K)  
            VHDX1(L,K)=DXV(L)*H1V(L)*V1(L,K)  
            UHDY(L,K)=DYU(L)*HU(L)*U(L,K)  
            VHDX(L,K)=DXV(L)*HV(L)*V(L,K)  
          ENDDO  
        ENDDO  
      ENDIF  
      N=0  
      IF(ISDRY.EQ.0)THEN  
        CALL CALTSXY  
        CALL CALQVS (2)  
      ENDIF  
      DO K=1,KS  
        RDZC=DZC(K)  
        DO L=2,LA  
          LN=LNC(L)  
          W(L,K)=SWB(L)*(W(L,K-1)  
     &        -RDZC*(UHDY(L+1,K)-UHDY(L,K)-UHDYE(L+1)+UHDYE(L)  
     &        +VHDX(LN,K)-VHDX(L,K)-VHDXE(LN)+VHDXE(L))*DXYIP(L))  
     &        +SWB(L)*( QSUM(L,K)-RDZC*QSUME(L) )*DXYIP(L)  
          W1(L,K)=SWB(L)*(W1(L,K-1)  
     &        -RDZC*(UHDY1(L+1,K)-UHDY1(L,K)-UHDY1E(L+1)+UHDY1E(L)  
     &        +VHDX1(LN,K)-VHDX1(L,K)-VHDX1E(LN)+VHDX1E(L))*DXYIP(L))  
     &        +SWB(L)*( QSUM(L,K)-RDZC*QSUME(L) )*DXYIP(L)  
        ENDDO  
      ENDDO   
C     Inserted from RESTIN1.for [lines: 418-512]

C  
C **  SET DRYING AND WETTING FLAGS  
C  
      IF(ISDRY.GT.0.AND.ISDRY.LT.97)THEN  
        DO L=2,LA  
          ISCDRY(L)=0  
          LS=LSC(L)  
          LN=LNC(L)  
          IF(HP(L).LE.HDRY)THEN  
            ISCDRY(L)=1  
            SUB(L)=0.  
            SVB(L)=0.  
            SUB(L+1)=0.  
            SVB(LN)=0.  
            SBX(L)=0.  
            SBY(L)=0.  
            SBX(L+1)=0.  
            SBY(LN)=0.  
          ENDIF  
        ENDDO  
      ENDIF  
      IF(ISDRY.EQ.98)THEN  
        HDRY2=2.*HDRY  
        DO L=2,LA  
          LS=LSC(L)  
          LN=LNC(L)  
          IF(HP(L).LE.HDRY)THEN  
            DHPDT=(HP(L)-H1P(L))/DT  
            IF(DHPDT.GT.0.0)THEN  
              SUBW=SUB(L)  
              SUBE=SUB(L+1)  
              SVBS=SVB(L)  
              SVBN=SVB(LN)  
              SUB(L)=0.0  
              SUB(L+1)=0.0  
              SVB(L)=0.0  
              SVB(LN)=0.0  
              SBX(L)=0.0  
              SBX(L+1)=0.0  
              SBY(L)=0.0  
              SBY(LN)=0.0  
              IF(SUBO(L).GT.0.5)THEN  
                IF(UHDYE(L).GT.0.0.AND.HP(L-1).GT.HDRY2)THEN  
                  SUB(L)=1.  
                  SBX(L)=1.  
                ENDIF  
              ENDIF  
              IF(SUBO(L+1).GT.0.5)THEN  
                IF(UHDYE(L+1).LT.0.0.AND.HP(L+1).GT.HDRY2)THEN  
                  SUB(L+1)=1.  
                  SBX(L+1)=1.  
                ENDIF  
              ENDIF  
              IF(SVBO(L).GT.0.5)THEN  
                IF(VHDXE(L).GT.0.0.AND.HP(LS).GT.HDRY2)THEN  
                  SVB(L)=1.  
                  SBY(L)=1.  
                ENDIF  
              ENDIF  
              IF(SVBO(LN).GT.0.5)THEN  
                IF(VHDXE(LN).LT.0.0.AND.HP(LN).GT.HDRY2)THEN  
                  SVB(LN)=1.  
                  SBY(LN)=1.  
                ENDIF  
              ENDIF  
              RDRY=SUB(L)+SUB(L+1)+SVB(L)+SVB(LN)  
              IF(RDRY.LT.0.5)THEN  
                ISCDRY(L)=1  
              ELSE  
                ISCDRY(L)=0  
              ENDIF  
              IDFLAG=0  
              TMPVAL=ABS(SUB(L)-SUBW)  
              IF(TMPVAL.GT.0.5)ICORDRY=1  
              TMPVAL=ABS(SUB(L+1)-SUBE)  
              IF(TMPVAL.GT.0.5)ICORDRY=1  
              TMPVAL=ABS(SVB(L)-SVBS)  
              IF(TMPVAL.GT.0.5)ICORDRY=1  
              TMPVAL=ABS(SVB(LN)-SVBN)  
              IF(TMPVAL.GT.0.5)ICORDRY=1  
            ELSE  
              SUB(L)=0.0  
              SUB(L+1)=0.0  
              SVB(L)=0.0  
              SVB(LN)=0.0  
              SBX(L)=0.0  
              SBX(L+1)=0.0  
              SBY(L)=0.0  
              SBY(LN)=0.0  
              IF(ISCDRY(L).EQ.0)THEN  
                ISCDRY(L)=1  
              ENDIF  
            ENDIF  
          ENDIF  
        ENDDO  
      ENDIF  
C     Inserted from RESTIN1.for [lines: 532-535]
      DO L=2,LA  
        IF(IMASKDRY(L).EQ.0) LMASKDRY(L)=.TRUE.  
        IF(IMASKDRY(L).GT.0) LMASKDRY(L)=.FALSE.  
      END DO  

      
      END SUBROUTINE model_restart_init
