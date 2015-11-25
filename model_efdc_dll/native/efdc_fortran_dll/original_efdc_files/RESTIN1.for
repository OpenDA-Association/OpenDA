      SUBROUTINE RESTIN1  
C  
C CHANGE RECORD  
C  ADDED CODE TO PROPERLY INITIAL RESTART INPUT FOR DRYING AND WETTING  
C **  SUBROUTINE RESTIN1 READS A RESTART FILE  
C  
      USE GLOBAL  

      REAL,ALLOCATABLE,DIMENSION(:)::TDUMMY  
      ALLOCATE(TDUMMY(KCM))  
      TDUMMY=0.
C  
      PRINT *,'READING RESTART FILE: RESTART.INP'
      OPEN(1,FILE='RESTART.INP',STATUS='UNKNOWN')  
      ISBELVC=0  
      READ(1,908,ERR=1000)NREST  
      DO L=2,LA
        IF(ISRESTI.EQ.1)THEN  
          !IF(IMORPH.EQ.0)THEN    PMC-RESTOUT ALWAYS WRITES BELV
          !  READ(1,*,ERR=1001)HP(L),H1P(L),HWQ(L),H2WQ(L)  
          !ELSE  
            READ(1,*,ERR=1001)HP(L),H1P(L),HWQ(L),H2WQ(L),BELV(L)  
          !ENDIF  
        ELSE  
          READ(1,*,ERR=1002)HP(L),H1P(L),HWQ(L),H2WQ(L),BELTMP  
          IF(BELTMP.NE.BELV(L))THEN  
            ISBELVC=1  
            WRITE(6,600)IL(L),JL(L),BELTMP,BELV(L)  
            HP(L)=HP(L)+BELTMP-BELV(L)  
            H1P(L)=H1P(L)+BELTMP-BELV(L)  
            HWQ(L)=HWQ(L)+BELTMP-BELV(L)  
            H2WQ(L)=H2WQ(L)+BELTMP-BELV(L)  
          ENDIF  
        ENDIF  
        IF(HP(L).LT.0.0.OR.H1P(L).LT.0.0)THEN  
          WRITE(6,9696)L,IL(L),JL(L),HP(L),H1P(L)  
          STOP  
        ELSEIF(HP(L).LT.0.0001.OR.H1P(L).LT.0.0001)THEN  
          WRITE(6,9698)L,IL(L),JL(L),HP(L),H1P(L)  
          HP(L) = HDRY*0.9
          H1P(L)= HDRY*0.9
        ENDIF  
        READ(1,*,ERR=1003)UHDYE(L),UHDY1E(L),VHDXE(L),VHDX1E(L)  
        READ(1,*,ERR=1004)(U(L,K),K=1,KC)  
        READ(1,*,ERR=1005)(U1(L,K),K=1,KC)  
        READ(1,*,ERR=1006)(V(L,K),K=1,KC)  
        READ(1,*,ERR=1007)(V1(L,K),K=1,KC)  
        READ(1,*,ERR=1008)(QQ(L,K),K=0,KC)  
        READ(1,*,ERR=1009)(QQ1(L,K),K=0,KC)  
        READ(1,*,ERR=1010)(QQL(L,K),K=0,KC)  
        READ(1,*,ERR=1011)(QQL1(L,K),K=0,KC)  
        READ(1,*,ERR=1012)(DML(L,K),K=0,KC)  
        IF(ISCI(1).EQ.1)THEN  
          READ(1,*,ERR=1013)(SAL(L,K),K=1,KC)  
          READ(1,*,ERR=1014)(SAL1(L,K),K=1,KC)  
        ENDIF  
        IF(ISCI(2).EQ.1)THEN  
          READ(1,*,ERR=1015)(TEM(L,K),K=1,KC)  
          READ(1,*,ERR=1016)(TEM1(L,K),K=1,KC)  
        ENDIF  
        IF(ISCI(3).EQ.1)THEN  
          READ(1,*,ERR=1017)(DYE(L,K),K=1,KC)  
          READ(1,*,ERR=1018)(DYE1(L,K),K=1,KC)  
        ENDIF  
        IF(ISCI(4).EQ.1)THEN  
C  
C       READ(1,*,ERR=1021)(SFL(L,K),K=1,KC)  
C       READ(1,*,ERR=1022)(SFL2(L,K),K=1,KC)  
C  
          READ(1,*,ERR=1021)SFLSBOT(L),(SFL(L,K),K=1,KC)  
          READ(1,*,ERR=1022)SFLSBOT(L),(SFL2(L,K),K=1,KC)  
        ENDIF  
        IF(ISCI(5).EQ.1)THEN  
          DO NT=1,NTOX  
            READ(1,*,ERR=1019)(TOXB(L,K,NT),K=1,KB)  
            READ(1,*,ERR=1019)(TOX(L,K,NT),K=1,KC)  
            READ(1,*,ERR=1020)(TOXB1(L,K,NT),K=1,KB)  
            READ(1,*,ERR=1020)(TOX1(L,K,NT),K=1,KC)  
          ENDDO  
        ENDIF  
        IF(ISCI(6).EQ.1)THEN  
          DO NS=1,NSED  
            READ(1,*,ERR=1019)(SEDB(L,K,NS),K=1,KB)  
            READ(1,*,ERR=1019)(SED(L,K,NS),K=1,KC)  
            READ(1,*,ERR=1020)(SEDB1(L,K,NS),K=1,KB)  
            READ(1,*,ERR=1020)(SED1(L,K,NS),K=1,KC)  
          ENDDO  
        ENDIF  
        IF(ISCI(7).EQ.1)THEN  
          DO NS=1,NSND  
            READ(1,*,ERR=1019)(SNDB(L,K,NS),K=1,KB)  
            READ(1,*,ERR=1019)(SND(L,K,NS),K=1,KC)  
            READ(1,*,ERR=1020)(SNDB1(L,K,NS),K=1,KB)  
            READ(1,*,ERR=1020)(SND1(L,K,NS),K=1,KC)  
          ENDDO  
        ENDIF  
        IF(ISCI(6).EQ.1.OR.ISCI(7).EQ.1)THEN  
          READ(1,*,ERR=1019)(HBED(L,K),K=1,KB)  
          READ(1,*,ERR=1019)(HBED1(L,K),K=1,KB)  
          READ(1,*,ERR=1019)(VDRBED(L,K),K=1,KB)  
          READ(1,*,ERR=1019)(VDRBED1(L,K),K=1,KB)  
        ENDIF  
      ENDDO  
      
      ! *** BOUNDARY CONDITIONS
      DO M=1,4  
        IF(ISCI(M).EQ.1)THEN  
          DO LL=1,NCBS  
            READ(1,*,ERR=1023)(NLOS(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1024)(CLOS(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBW  
            READ(1,*,ERR=1025)(NLOW(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1026)(CLOW(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBE  
            READ(1,*,ERR=1027)(NLOE(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1028)(CLOE(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBN  
            READ(1,*,ERR=1029)(NLON(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1030)(CLON(LL,K,M),K=1,KC)  
          ENDDO  
        ENDIF  
      ENDDO  
      IF(ISCI(5).EQ.1)THEN  
        DO NT=1,NTOX  
          M=MSVTOX(NT)  
          DO LL=1,NCBS  
            READ(1,*,ERR=1023)(NLOS(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1024)(CLOS(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBW  
            READ(1,*,ERR=1025)(NLOW(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1026)(CLOW(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBE  
            READ(1,*,ERR=1027)(NLOE(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1028)(CLOE(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBN  
            READ(1,*,ERR=1029)(NLON(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1030)(CLON(LL,K,M),K=1,KC)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISCI(6).EQ.1)THEN  
        DO NT=1,NSED  
          M=MSVSED(NT)  
          DO LL=1,NCBS  
            READ(1,*,ERR=1023)(NLOS(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1024)(CLOS(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBW  
            READ(1,*,ERR=1025)(NLOW(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1026)(CLOW(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBE  
            READ(1,*,ERR=1027)(NLOE(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1028)(CLOE(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBN  
            READ(1,*,ERR=1029)(NLON(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1030)(CLON(LL,K,M),K=1,KC)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISCI(7).EQ.1)THEN  
        DO NT=1,NSND  
          M=MSVSND(NT)  
          DO LL=1,NCBS  
            READ(1,*,ERR=1023)(NLOS(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1024)(CLOS(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBW  
            READ(1,*,ERR=1025)(NLOW(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1026)(CLOW(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBE  
            READ(1,*,ERR=1027)(NLOE(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1028)(CLOE(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBN  
            READ(1,*,ERR=1029)(NLON(LL,K,M),K=1,KC)  
            READ(1,*,ERR=1030)(CLON(LL,K,M),K=1,KC)  
          ENDDO  
        ENDDO  
      ENDIF  
      DO L=2,LA  
        READ(1,*,END=1999,ERR=1031)QSUME(L),(QSUM(L,K),K=1,KC)  
      ENDDO  
      IF(MDCHH.GE.1)THEN  
        DO NMD=1,MDCHH  
          READ(1,*,ERR=1032)ITMP1,JTMP1,ITMP2,JTMP2,  
     &        ITMP3,JTMP3,QCHANU(NMD),QCHANV(NMD)  
        ENDDO  
      ELSE  
        DO NMD=1,MDCHH  
          QCHANU(NMD)=0.  
          QCHANV(NMD)=0.  
          QCHANUN(NMD)=0.  
          QCHANVN(NMD)=0.  
        ENDDO  
      ENDIF  
      IF(ISGWIE.GE.1)THEN  
        DO L=2,LA  
          READ(1,*,ERR=1033)AGWELV(L),AGWELV1(L)  
        ENDDO  
      ENDIF  
      CLOSE(1)  
 6666 FORMAT(3I10,F12.6)  
 6667 FORMAT(7I5,2X,E12.4,2X,E12.4)  
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
          SAL(L,K)=MAX(SAL(L,K),0.)  
          SAL1(L,K)=MAX(SAL1(L,K),0.)  
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
C  
C ** READ BED TEMPERATURE  HTBED1 HTBED2  
C *** DSLLC BEGIN BLOCK  
C  
      IF(ISTRAN(2).GT.0.AND.ISCI(2).GE.1)THEN  
C  
C *** DSLLC END BLOCK  
C  
        PRINT *,'READING RESTART FILE: TEMP.RST'
        OPEN(1,FILE='TEMP.RST',STATUS='UNKNOWN')
        DO L=2,LA  
          READ(1,*)LDUM,IDUM,JDUM,(TDUMMY(K),K=1,KC),TEMB(L)  
        ENDDO  
        CLOSE(1)  
      ENDIF  
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
      IF(ISDRY.EQ.99)THEN  
        PRINT *,'READING RESTART FILE: RSTWD.INP'
        OPEN(1,FILE='RSTWD.INP',STATUS='UNKNOWN')  
        OPEN(2,FILE='RSTWD.RCK',STATUS='UNKNOWN')  
        CLOSE(2,STATUS='DELETE')  
        OPEN(2,FILE='RSTWD.RCK',STATUS='UNKNOWN')  
        DO L=2,LA  
          READ(1,*)LDUM,IDUM,JDUM,ISCDRY(L),NATDRY(L),  
     &        IMASKDRY(L),SUB(L),SVB(L)  
          WRITE(2,913)LDUM,IDUM,JDUM,ISCDRY(L),NATDRY(L),  
     &        IMASKDRY(L),SUB(L),SVB(L),SUBO(L),SVBO(L)  
        ENDDO  
        CLOSE(1)  
        CLOSE(2)  
      ENDIF  
  913 FORMAT(6I10,4F7.3)  
C  
C *** DSLLC BEGIN BLOCK  
C  
      DO L=2,LA  
        IF(IMASKDRY(L).EQ.0) LMASKDRY(L)=.TRUE.  
        IF(IMASKDRY(L).GT.0) LMASKDRY(L)=.FALSE.  
      END DO  
C  
C *** DSLLC END BLOCK  
C  
      GOTO 3000  

  101 FORMAT(I5)  
  102 FORMAT(3I5,12F8.2)  
C  
C **  WRITE READ ERRORS ON RESTART  
C  
 1000 WRITE(6,2000)  
      STOP  
 1001 WRITE(6,2001)L  
      STOP  
 1002 WRITE(6,2002)L  
      STOP  
 1003 WRITE(6,2003)L  
      STOP  
 1004 WRITE(6,2004)L  
      STOP  
 1005 WRITE(6,2005)L  
      STOP  
 1006 WRITE(6,2006)L  
      STOP  
 1007 WRITE(6,2007)L  
      STOP  
 1008 WRITE(6,2008)L  
      STOP  
 1009 WRITE(6,2009)L  
      STOP  
 1010 WRITE(6,2010)L  
      STOP  
 1011 WRITE(6,2011)L  
      STOP  
 1012 WRITE(6,2012)L  
      STOP  
 1013 WRITE(6,2013)L  
      STOP  
 1014 WRITE(6,2014)L  
      STOP  
 1015 WRITE(6,2015)L  
      STOP  
 1016 WRITE(6,2016)L  
      STOP  
 1017 WRITE(6,2017)L  
      STOP  
 1018 WRITE(6,2018)L  
      STOP  
 1019 WRITE(6,2019)L  
      STOP  
 1020 WRITE(6,2020)L  
      STOP  
 1021 WRITE(6,2021)L  
      STOP  
 1022 WRITE(6,2022)L  
      STOP  
 1023 WRITE(6,2023)L  
      STOP  
 1024 WRITE(6,2024)L  
      STOP  
 1025 WRITE(6,2025)L  
      STOP  
 1026 WRITE(6,2026)L  
      STOP  
 1027 WRITE(6,2027)L  
      STOP  
 1028 WRITE(6,2028)L  
      STOP  
 1029 WRITE(6,2029)L  
      STOP  
 1030 WRITE(6,2030)L  
      STOP  
 1031 WRITE(6,2031)L  
      STOP  
 1032 WRITE(6,2032)NMD  
      STOP  
 1033 WRITE(6,2033)L  
      STOP  

  600 FORMAT(2X,'I,J,BELVOLD,BELVNEW',2I5,2F12.2)  
  906 FORMAT(5E15.7)  
  907 FORMAT(12E12.4)  
  908 FORMAT(12I10)  
 2000 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1000')  
 2001 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1001 L =',I6)  
 2002 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1002 L =',I6)  
 2003 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1003 L =',I6)  
 2004 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1004 L =',I6)  
 2005 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1005 L =',I6)  
 2006 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1006 L =',I6)  
 2007 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1007 L =',I6)  
 2008 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1008 L =',I6)  
 2009 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1009 L =',I6)  
 2010 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1010 L =',I6)  
 2011 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1011 L =',I6)  
 2012 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1012 L =',I6)  
 2013 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1013 L =',I6)  
 2014 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1014 L =',I6)  
 2015 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1015 L =',I6)  
 2016 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1016 L =',I6)  
 2017 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1017 L =',I6)  
 2018 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1018 L =',I6)  
 2019 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1019 L =',I6)  
 2020 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1020 L =',I6)  
 2021 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1021 L =',I6)  
 2022 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1022 L =',I6)  
 2023 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1023 L =',I6)  
 2024 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1024 L =',I6)  
 2025 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1025 L =',I6)  
 2026 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1026 L =',I6)  
 2027 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1027 L =',I6)  
 2028 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1028 L =',I6)  
 2029 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1029 L =',I6)  
 2030 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1030 L =',I6)  
 2031 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1031 L =',I6)  
 2032 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1032 NMD =',I6)  
 2033 FORMAT('  READ ERROR ON FILE RESTART.INP ERR 1033 L =',I6)  
 9696 FORMAT('  NEGATIVE DEPTH RESTART, L,I,J,HP,H1P = ',  
     &    3I7,2F10.4)  
 9698 FORMAT('  ZERO DEPTH RESTART (WARN), L,I,J,HP,H1P = ',  
     &    3I7,2F10.4)  
 1999 CLOSE(1)      

 3000 CONTINUE  

      RETURN  
      END  

