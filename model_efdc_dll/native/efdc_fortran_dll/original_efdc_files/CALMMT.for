      SUBROUTINE CALMMT  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALMMTF CALCULATES THE MEAN MASS TRANSPORT FIELD  
C  
      USE GLOBAL  
C
      LOGICAL INITIALIZE
      DATA INITIALIZE/.TRUE./
C
C **  INITIALIZE CE-QUAL-ICM INTERFACE  
C  
      IF(ISICM.GE.1.AND.JSWASP.EQ.1) CALL CEQICM  
C
      IF(.NOT.INITIALIZE)GOTO 100
      INITIALIZE=.FALSE.
      IF(NTSMMT.LT.NTSPTC)THEN  
        DO L=1,LC  
          HLPF(L)=0.  
          QSUMELPF(L)=0.  
          UELPF(L)=0.  
          VELPF(L)=0.  
          RAINLPF(L)=0.  
          EVPSLPF(L)=0.  
          EVPGLPF(L)=0.  
          RINFLPF(L)=0.  
          GWLPF(L)=0.  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KB  
            DO L=1,LC  
              SEDBLPF(L,K,NSC)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KB  
            DO L=1,LC  
              SNDBLPF(L,K,NSN)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO K=1,KB  
            DO L=1,LC  
              TOXBLPF(L,K,NT)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        HLPF(1)=HMIN  
        HLPF(LC)=HMIN  
        DO K=1,KS  
          DO L=1,LC  
            ABLPF(L,K)=0.  
            ABEFF(L,K)=0.  
            WLPF(L,K)=0.  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO L=1,LC  
            AHULPF(L,K)=0.  
            AHVLPF(L,K)=0.  
            SALLPF(L,K)=0.  
            TEMLPF(L,K)=0.  
            SFLLPF(L,K)=0.  
            DYELPF(L,K)=0.  
            UHLPF(L,K)=0.  
            VHLPF(L,K)=0.  
            QSUMLPF(L,K)=0.  
          ENDDO  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KC  
            DO L=1,LC  
              SEDLPF(L,K,NSC)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KC  
            DO L=1,LC  
              SNDLPF(L,K,NSN)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO K=1,KC  
            DO L=1,LC  
              TOXLPF(L,K,NT)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO NS=1,NSED+NSND  
            DO K=1,KC  
              DO L=1,LC  
                TXPFLPF(L,K,NS,NT)=0.  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NS=1,NQSER  
          DO K=1,KC  
            QSRTLPP(K,NS)=0.  
            QSRTLPN(K,NS)=0.  
          ENDDO  
        ENDDO  
        DO NS=1,NQCTL  
          DO K=1,KC  
            QCTLTLP(K,NS)=0.  
          ENDDO  
        ENDDO  
        DO NMD=1,MDCHH  
          QCHNULP(NMD)=0.  
          QCHNVLP(NMD)=0.  
        ENDDO  
        DO NWR=1,NQWR  
          QWRSERTLP(NWR)=0.  
        ENDDO  
      ELSE  
        DO L=1,LC  
          HLPF(L)=0.  
          QSUMELPF(L)=0.  
          UELPF(L)=0.  
          VELPF(L)=0.  
          RAINLPF(L)=0.  
          EVPSLPF(L)=0.  
          EVPGLPF(L)=0.  
          RINFLPF(L)=0.  
          GWLPF(L)=0.  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KB  
            DO L=1,LC  
              SEDBLPF(L,K,NSC)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KB  
            DO L=1,LC  
              SNDBLPF(L,K,NSN)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO K=1,KB  
            DO L=1,LC  
              TOXBLPF(L,K,NT)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        HLPF(1)=HMIN  
        HLPF(LC)=HMIN  
        DO K=1,KS  
          DO L=1,LC  
            ABLPF(L,K)=0.  
            WIRT(L,K)=0.  
            WLPF(L,K)=0.  
            WTLPF(L,K)=0.  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO L=1,LC  
            AHULPF(L,K)=0.  
            AHVLPF(L,K)=0.  
            SALLPF(L,K)=0.  
            TEMLPF(L,K)=0.  
            SFLLPF(L,K)=0.  
            DYELPF(L,K)=0.  
            UHLPF(L,K)=0.  
            UIRT(L,K)=0.  
            ULPF(L,K)=0.  
            UTLPF(L,K)=0.  
            VHLPF(L,K)=0.  
            QSUMLPF(L,K)=0.  
            VIRT(L,K)=0.  
            VLPF(L,K)=0.  
            VTLPF(L,K)=0.  
          ENDDO  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KC  
            DO L=1,LC  
              SEDLPF(L,K,NSC)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KC  
            DO L=1,LC  
              SNDLPF(L,K,NSN)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO K=1,KC  
            DO L=1,LC  
              TOXLPF(L,K,NT)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO NS=1,NSED+NSND  
            DO K=1,KC  
              DO L=1,LC  
                TXPFLPF(L,K,NS,NT)=0.  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NS=1,NQSER  
          DO K=1,KC  
            QSRTLPP(K,NS)=0.  
            QSRTLPN(K,NS)=0.  
          ENDDO  
        ENDDO  
        DO NS=1,NQCTL  
          DO K=1,KC  
            QCTLTLP(K,NS)=0.  
          ENDDO  
        ENDDO  
        DO NMD=1,MDCHH  
          QCHNULP(NMD)=0.  
          QCHNVLP(NMD)=0.  
        ENDDO  
        DO NWR=1,NQWR  
          QWRSERTLP(NWR)=0.  
        ENDDO  
      ENDIF  
C  
C **  ACCUMULATE FILTERED VARIABLES AND DISPLACEMENTS  
C  
  100 CONTINUE  
      IF(NTSMMT.LT.NTSPTC)THEN  
        DO L=2,LA  
          LN=LNC(L)  
          HLPF(L)=HLPF(L)+HP(L)  
          QSUMELPF(L)=QSUMELPF(L)+QSUME(L)  
          UTMP1=0.5*(UHDYE(L+1)+UHDYE(L))/(DYP(L)*HP(L))  
          VTMP1=0.5*(VHDXE(LN)+VHDXE(L))/(DXP(L)*HP(L))  
          UTMP=CUE(L)*UTMP1+CVE(L)*VTMP1  
          VTMP=CUN(L)*UTMP1+CVN(L)*VTMP1  
          UELPF(L)=UELPF(L)+UTMP  
          VELPF(L)=VELPF(L)+VTMP  
          RAINLPF(L)=RAINLPF(L)+DXYP(L)*RAINT(L)  
        ENDDO  
        IF(ISGWIE.EQ.0)THEN  
          DO L=2,LA  
            EVPSLPF(L)=EVPSLPF(L)+DXYP(L)*EVAPT(L)  
            EVPGLPF(L)=0.  
            RINFLPF(L)=0.  
            GWLPF(L)=0.  
          ENDDO  
        ELSE  
          DO L=2,LA  
            EVPSLPF(L)=EVPSLPF(L)+EVAPSW(L)  
            EVPGLPF(L)=EVPGLPF(L)+EVAPGW(L)  
            RINFLPF(L)=RINFLPF(L)+RIFTR(L)  
            GWLPF(L)=GWLPF(L)+AGWELV(L)  
          ENDDO  
        ENDIF  
        DO NT=1,NTOX  
          DO K=1,KB  
            DO L=2,LA  
              TOXBLPF(L,K,NT)=TOXBLPF(L,K,NT)+TOXB(L,K,NT)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KB  
            DO L=2,LA  
              SEDBLPF(L,K,NSC)=SEDBLPF(L,K,NSC)+SEDB(L,K,NSC)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KB  
            DO L=2,LA  
              SNDBLPF(L,K,NSN)=SNDBLPF(L,K,NSN)+SNDB(L,K,NSN)  
            ENDDO  
          ENDDO  
        ENDDO  
        IF(ISWASP.EQ.99.OR.ISICM.GE.1)THEN  
          DO K=1,KS  
            DO L=2,LA  
              ABLPF(L,K)=ABLPF(L,K)+(AB(L,K)*HP(L))  
              ABEFF(L,K)=ABEFF(L,K)+AB(L,K)*(SAL(L,K+1)-SAL(L,K))  
              WLPF(L,K)=WLPF(L,K)+W(L,K)  
            ENDDO  
          ENDDO  
        ELSE  
          DO K=1,KS  
            DO L=2,LA  
              ABLPF(L,K)=ABLPF(L,K)+AB(L,K)  
              ABEFF(L,K)=ABEFF(L,K)+AB(L,K)*(SAL(L,K+1)-SAL(L,K))  
              WLPF(L,K)=WLPF(L,K)+W(L,K)  
            ENDDO  
          ENDDO  
        ENDIF  
        DO K=1,KC  
          DO L=2,LA  
            LS=LSC(L)  
            AHULPF(L,K)=AHULPF(L,K)+0.5*(AH(L,K)+AH(L-1,K))  
            AHVLPF(L,K)=AHVLPF(L,K)+0.5*(AH(L,K)+AH(LS,K))  
            SALLPF(L,K)=SALLPF(L,K)+SAL(L,K)  
            TEMLPF(L,K)=TEMLPF(L,K)+TEM(L,K)  
            SFLLPF(L,K)=SFLLPF(L,K)+SFL(L,K)  
            DYELPF(L,K)=DYELPF(L,K)+DYE(L,K)  
            UHLPF(L,K)=UHLPF(L,K)+UHDYWQ(L,K)/DYU(L)  
            VHLPF(L,K)=VHLPF(L,K)+VHDXWQ(L,K)/DXV(L)  
            QSUMLPF(L,K)=QSUMLPF(L,K)+QSUM(L,K)  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO K=1,KC  
            DO L=2,LA  
              TOXLPF(L,K,NT)=TOXLPF(L,K,NT)+TOX(L,K,NT)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KC  
            DO L=2,LA  
              SEDLPF(L,K,NSC)=SEDLPF(L,K,NSC)+SED(L,K,NSC)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KC  
            DO L=2,LA  
              SNDLPF(L,K,NSN)=SNDLPF(L,K,NSN)+SND(L,K,NSN)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO NS=1,NSED+NSND  
            DO K=1,KC  
              DO L=1,LC  
                TXPFLPF(L,K,NS,NT)=TXPFLPF(L,K,NS,NT)+TOXPFW(L,K,NS,NT)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NS=1,NQSER  
          DO K=1,KC  
            QSRTLPP(K,NS)=QSRTLPP(K,NS)+MAX(QSERT(K,NS),0.)  
            QSRTLPN(K,NS)=QSRTLPN(K,NS)+MIN(QSERT(K,NS),0.)  
          ENDDO  
        ENDDO  
        DO NS=1,NQCTL  
          DO K=1,KC  
            QCTLTLP(K,NS)=QCTLTLP(K,NS)+QCTLT(K,NS)  
          ENDDO  
        ENDDO  
        DO NMD=1,MDCHH  
          QCHNULP(NMD)=QCHNULP(NMD)+QCHANU(NMD)  
          QCHNVLP(NMD)=QCHNVLP(NMD)+QCHANV(NMD)  
        ENDDO  
        DO NWR=1,NQWR  
          QWRSERTLP(NWR)=QWRSERTLP(NWR)+QWRSERT(NWR)  
        ENDDO  
      ELSE  
        DO L=2,LA  
          LN=LNC(L)  
          HLPF(L)=HLPF(L)+HP(L)  
          QSUMELPF(L)=QSUMELPF(L)+QSUME(L)  
          UTMP1=0.5*(UHDYE(L+1)+UHDYE(L))/(DYP(L)*HP(L))  
          VTMP1=0.5*(VHDXE(LN)+VHDXE(L))/(DXP(L)*HP(L))  
          UTMP=CUE(L)*UTMP1+CVE(L)*VTMP1  
          VTMP=CUN(L)*UTMP1+CVN(L)*VTMP1  
          UELPF(L)=UELPF(L)+UTMP  
          VELPF(L)=VELPF(L)+VTMP  
          RAINLPF(L)=RAINLPF(L)+DXYP(L)*RAINT(L)  
        ENDDO  
        IF(ISGWIE.EQ.0)THEN  
          DO L=2,LA  
            EVPSLPF(L)=EVPSLPF(L)+DXYP(L)*EVAPT(L)  
            EVPGLPF(L)=0.  
            RINFLPF(L)=0.  
            GWLPF(L)=0.  
          ENDDO  
        ELSE  
          DO L=2,LA  
            EVPSLPF(L)=EVPSLPF(L)+EVAPSW(L)  
            EVPGLPF(L)=EVPGLPF(L)+EVAPGW(L)  
            RINFLPF(L)=RINFLPF(L)+RIFTR(L)  
            GWLPF(L)=GWLPF(L)+AGWELV(L)  
          ENDDO  
        ENDIF  
        DO NT=1,NTOX  
          DO K=1,KB  
            DO L=2,LA  
              TOXBLPF(L,K,NT)=TOXBLPF(L,K,NT)+TOXB(L,K,NT)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KB  
            DO L=2,LA  
              SEDBLPF(L,K,NSC)=SEDBLPF(L,K,NSC)+SEDB(L,K,NSC)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KB  
            DO L=2,LA  
              SNDBLPF(L,K,NSN)=SNDBLPF(L,K,NSN)+SNDB(L,K,NSN)  
            ENDDO  
          ENDDO  
        ENDDO  
        IF(ISWASP.EQ.99.OR.ISICM.GE.1)THEN  
          DO K=1,KS  
            DO L=2,LA  
              ABLPF(L,K)=ABLPF(L,K)+(AB(L,K)*HP(L))  
              ABEFF(L,K)=ABEFF(L,K)+AB(L,K)*(SAL(L,K+1)-SAL(L,K))  
              WIRT(L,K)=WIRT(L,K)+DT*W(L,K)  
              WLPF(L,K)=WLPF(L,K)+W(L,K)  
              WTLPF(L,K)=WTLPF(L,K)+DT*(FLOAT(NMMT)-0.5)*W(L,K)  
            ENDDO  
          ENDDO  
        ELSE  
          DO K=1,KS  
            DO L=2,LA  
              ABLPF(L,K)=ABLPF(L,K)+AB(L,K)  
              ABEFF(L,K)=ABEFF(L,K)+AB(L,K)*(SAL(L,K+1)-SAL(L,K))  
              WIRT(L,K)=WIRT(L,K)+DT*W(L,K)  
              WLPF(L,K)=WLPF(L,K)+W(L,K)  
              WTLPF(L,K)=WTLPF(L,K)+DT*(FLOAT(NMMT)-0.5)*W(L,K)  
            ENDDO  
          ENDDO  
        ENDIF  
        DO K=1,KC  
          DO L=2,LA  
            LS=LSC(L)  
            AHULPF(L,K)=AHULPF(L,K)+0.5*(AH(L,K)+AH(L-1,K))  
            AHVLPF(L,K)=AHVLPF(L,K)+0.5*(AH(L,K)+AH(LS,K))  
            SALLPF(L,K)=SALLPF(L,K)+SAL(L,K)  
            TEMLPF(L,K)=TEMLPF(L,K)+TEM(L,K)  
            SFLLPF(L,K)=SFLLPF(L,K)+SFL(L,K)  
            DYELPF(L,K)=DYELPF(L,K)+DYE(L,K)  
            UHLPF(L,K)=UHLPF(L,K)+UHDYWQ(L,K)/DYU(L)  
            UIRT(L,K)=UIRT(L,K)+DT*U(L,K)  
            ULPF(L,K)=ULPF(L,K)+U(L,K)  
            UTLPF(L,K)=UTLPF(L,K)+DT*(FLOAT(NMMT)-0.5)*U(L,K)  
            VHLPF(L,K)=VHLPF(L,K)+VHDXWQ(L,K)/DXV(L)  
            QSUMLPF(L,K)=QSUMLPF(L,K)+QSUM(L,K)  
            VIRT(L,K)=VIRT(L,K)+DT*V(L,K)  
            VLPF(L,K)=VLPF(L,K)+V(L,K)  
            VTLPF(L,K)=VTLPF(L,K)+DT*(FLOAT(NMMT)-0.5)*V(L,K)  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO K=1,KC  
            DO L=2,LA  
              TOXLPF(L,K,NT)=TOXLPF(L,K,NT)+TOX(L,K,NT)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KC  
            DO L=2,LA  
              SEDLPF(L,K,NSC)=SEDLPF(L,K,NSC)+SED(L,K,NSC)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KC  
            DO L=2,LA  
              SNDLPF(L,K,NSN)=SNDLPF(L,K,NSN)+SND(L,K,NSN)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO NS=1,NSED+NSND  
            DO K=1,KC  
              DO L=1,LC  
                TXPFLPF(L,K,NS,NT)=TXPFLPF(L,K,NS,NT)+TOXPFW(L,K,NS,NT)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NS=1,NQSER  
          DO K=1,KC  
            QSRTLPP(K,NS)=QSRTLPP(K,NS)+MAX(QSERT(K,NS),0.)  
            QSRTLPN(K,NS)=QSRTLPN(K,NS)+MIN(QSERT(K,NS),0.)  
          ENDDO  
        ENDDO  
        DO NS=1,NQCTL  
          DO K=1,KC  
            QCTLTLP(K,NS)=QCTLTLP(K,NS)+QCTLT(K,NS)  
          ENDDO  
        ENDDO  
        DO NMD=1,MDCHH  
          QCHNULP(NMD)=QCHNULP(NMD)+QCHANU(NMD)  
          QCHNVLP(NMD)=QCHNVLP(NMD)+QCHANV(NMD)  
        ENDDO  
        DO NWR=1,NQWR  
          QWRSERTLP(NWR)=QWRSERTLP(NWR)+QWRSERT(NWR)  
        ENDDO  
        DO K=1,KS  
          DO L=2,LA  
            LS=LSC(L)  
            VPX(L,K)=VPX(L,K)+0.25*(V(L,K+1)+V(L,K))*(WIRT(L,K)+
     &          WIRT(LS,K))  
            VPY(L,K)=VPY(L,K)+0.25*(W(L,K)+W(L-1,K))*(UIRT(L,K+1)+
     &          UIRT(L,K))  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
            LS=LSC(L)  
            VPZ(L,K)=VPZ(L,K)+0.25*(U(L,K)+U(LS,K))*(VIRT(L,K)+VIRT(
     &          L-1,K))  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  CHECK FOR END OF FILTER  
C  
      IF(NMMT.LT.NTSMMT) GOTO 200  
C  
C **  COMPLETE THE FILTERING  
C  
      FLTWT=1./FLOAT(NTSMMT)  
      IF(ISICM.GE.1) FLTWT=2.*FLTWT  
      IF(NTSMMT.LT.NTSPTC)THEN  
        DO L=2,LA  
          HLPF(L)=FLTWT*HLPF(L)  
          QSUMELPF(L)=FLTWT*QSUMELPF(L)  
          UELPF(L)=FLTWT*UELPF(L)  
          VELPF(L)=FLTWT*VELPF(L)  
          RAINLPF(L)=FLTWT*RAINLPF(L)  
          EVPSLPF(L)=FLTWT*EVPSLPF(L)  
          EVPGLPF(L)=FLTWT*EVPGLPF(L)  
          RINFLPF(L)=FLTWT*RINFLPF(L)  
          GWLPF(L)=FLTWT*GWLPF(L)  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KB  
            DO L=2,LA  
              SEDBLPF(L,K,NSC)=SEDBLPF(L,K,NSC)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KB  
            DO L=2,LA  
              SNDBLPF(L,K,NSN)=SNDBLPF(L,K,NSN)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO K=1,KB  
            DO L=2,LA  
              TOXBLPF(L,K,NT)=TOXBLPF(L,K,NT)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO K=1,KS  
          DO L=2,LA  
            ABLPF(L,K)=FLTWT*ABLPF(L,K)  
            ABEFF(L,K)=FLTWT*ABEFF(L,K)  
            WLPF(L,K)=FLTWT*WLPF(L,K)  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
            AHULPF(L,K)=AHULPF(L,K)*FLTWT  
            AHVLPF(L,K)=AHVLPF(L,K)*FLTWT  
            SALLPF(L,K)=SALLPF(L,K)*FLTWT  
            TEMLPF(L,K)=TEMLPF(L,K)*FLTWT  
            SFLLPF(L,K)=SFLLPF(L,K)*FLTWT  
            DYELPF(L,K)=DYELPF(L,K)*FLTWT  
            UHLPF(L,K)=FLTWT*UHLPF(L,K)  
            VHLPF(L,K)=FLTWT*VHLPF(L,K)  
            QSUMLPF(L,K)=FLTWT*QSUMLPF(L,K)  
          ENDDO  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KC  
            DO L=2,LA  
              SEDLPF(L,K,NSC)=SEDLPF(L,K,NSC)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KC  
            DO L=2,LA  
              SNDLPF(L,K,NSN)=SNDLPF(L,K,NSN)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO K=1,KC  
            DO L=2,LA  
              TOXLPF(L,K,NT)=TOXLPF(L,K,NT)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO NS=1,NSED+NSND  
            DO K=1,KC  
              DO L=1,LC  
                TXPFLPF(L,K,NS,NT)=TXPFLPF(L,K,NS,NT)*FLTWT  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NS=1,NQSER  
          DO K=1,KC  
            QSRTLPP(K,NS)=FLTWT*QSRTLPP(K,NS)  
            QSRTLPN(K,NS)=FLTWT*QSRTLPN(K,NS)  
          ENDDO  
        ENDDO  
        DO NS=1,NQCTL  
          DO K=1,KC  
            QCTLTLP(K,NS)=FLTWT*QCTLTLP(K,NS)  
          ENDDO  
        ENDDO  
        DO NMD=1,MDCHH  
          QCHNULP(NMD)=FLTWT*QCHNULP(NMD)  
          QCHNVLP(NMD)=FLTWT*QCHNVLP(NMD)  
        ENDDO  
        DO NWR=1,NQWR  
          QWRSERTLP(NWR)=FLTWT*QWRSERTLP(NWR)  
        ENDDO  
      ELSE  
        DO L=2,LA  
          HLPF(L)=FLTWT*HLPF(L)  
          QSUMELPF(L)=FLTWT*QSUMELPF(L)  
          UELPF(L)=FLTWT*UELPF(L)  
          VELPF(L)=FLTWT*VELPF(L)  
          RAINLPF(L)=FLTWT*RAINLPF(L)  
          EVPSLPF(L)=FLTWT*EVPSLPF(L)  
          EVPGLPF(L)=FLTWT*EVPGLPF(L)  
          RINFLPF(L)=FLTWT*RINFLPF(L)  
          GWLPF(L)=FLTWT*GWLPF(L)  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KB  
            DO L=2,LA  
              SEDBLPF(L,K,NSC)=SEDBLPF(L,K,NSC)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KB  
            DO L=2,LA  
              SNDBLPF(L,K,NSN)=SNDBLPF(L,K,NSN)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO K=1,KB  
            DO L=2,LA  
              TOXBLPF(L,K,NT)=TOXBLPF(L,K,NT)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO K=1,KS  
          DO L=2,LA  
            ABLPF(L,K)=FLTWT*ABLPF(L,K)  
            ABEFF(L,K)=FLTWT*ABEFF(L,K)  
            VPX(L,K)=FLTWT*VPX(L,K)  
            VPY(L,K)=FLTWT*VPY(L,K)  
            WLPF(L,K)=FLTWT*WLPF(L,K)  
            WTLPF(L,K)=FLTWT*WTLPF(L,K)  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
            AHULPF(L,K)=AHULPF(L,K)*FLTWT  
            AHVLPF(L,K)=AHVLPF(L,K)*FLTWT  
            SALLPF(L,K)=FLTWT*SALLPF(L,K)  
            TEMLPF(L,K)=FLTWT*TEMLPF(L,K)  
            SFLLPF(L,K)=FLTWT*SFLLPF(L,K)  
            DYELPF(L,K)=FLTWT*DYELPF(L,K)  
            UHLPF(L,K)=FLTWT*UHLPF(L,K)  
            ULPF(L,K)=FLTWT*ULPF(L,K)  
            UTLPF(L,K)=FLTWT*UTLPF(L,K)  
            VHLPF(L,K)=FLTWT*VHLPF(L,K)  
            QSUMLPF(L,K)=FLTWT*QSUMLPF(L,K)  
            VLPF(L,K)=FLTWT*VLPF(L,K)  
            VTLPF(L,K)=FLTWT*VTLPF(L,K)  
            VPZ(L,K)=FLTWT*VPZ(L,K)  
          ENDDO  
        ENDDO  
        DO NSC=1,NSED  
          DO K=1,KC  
            DO L=2,LA  
              SEDLPF(L,K,NSC)=SEDLPF(L,K,NSC)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NSN=1,NSND  
          DO K=1,KC  
            DO L=2,LA  
              SNDLPF(L,K,NSN)=SNDLPF(L,K,NSN)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO K=1,KC  
            DO L=2,LA  
              TOXLPF(L,K,NT)=TOXLPF(L,K,NT)*FLTWT  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO NS=1,NSED+NSND  
            DO K=1,KC  
              DO L=1,LC  
                TXPFLPF(L,K,NS,NT)=TXPFLPF(L,K,NS,NT)*FLTWT  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NS=1,NQSER  
          DO K=1,KC  
            QSRTLPP(K,NS)=FLTWT*QSRTLPP(K,NS)  
            QSRTLPN(K,NS)=FLTWT*QSRTLPN(K,NS)  
          ENDDO  
        ENDDO  
        DO NS=1,NQCTL  
          DO K=1,KC  
            QCTLTLP(K,NS)=FLTWT*QCTLTLP(K,NS)  
          ENDDO  
        ENDDO  
        DO NMD=1,MDCHH  
          QCHNULP(NMD)=FLTWT*QCHNULP(NMD)  
          QCHNVLP(NMD)=FLTWT*QCHNVLP(NMD)  
        ENDDO  
        DO NWR=1,NQWR  
          QWRSERTLP(NWR)=FLTWT*QWRSERTLP(NWR)  
        ENDDO  
        DO K=1,KS  
          DO L=2,LA  
            LS=LSC(L)  
            VPX(L,K)=VPX(L,K)  
     &          -0.25*(VTLPF(L,K+1)+VTLPF(L,K))*(WLPF(L,K)+WLPF(LS,K))  
            VPY(L,K)=VPY(L,K)  
     &          -0.25*(WTLPF(L,K)+WTLPF(L-1,K))*(ULPF(L,K+1)+ULPF(L,K))  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
            LS=LSC(L)  
            LSW=LSWC(L)  
            VPZ(L,K)=VPZ(L,K)  
     &          -0.25*(UTLPF(L,K)+UTLPF(LS,K))*(VLPF(L,K)+VLPF(L-1,K))  
            ! *** DSLLC BEGIN BLOCK
            TMPVAL=1.+SUB(L)+SVB(L)+SUB(L)*SVB(L)
            HPLW =SUB(L)*HP(L-1)  
            HPLS =SVB(L)*HP(LS)
            HPLSW=SUB(L)*SVBO(L)*HP(LSW)
            HMC=(HP(L)+HPLW+HPLS+HPLSW)/TMPVAL
            VPZ(L,K)=VPZ(L,K)*HMC*SUB(L)*SUB(LS)*SVB(L)*SVB(L-1)  
            ! *** DSLLC END BLOCK
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
            LS=LSC(L)  
            LN=LNC(L)  
            UVPT(L,K)=(VPZ(LN,K)-VPZ(L,K))/DYU(L)  
     &          -DZIC(K)*(VPY(L,K)-VPY(L,K-1))  
            VVPT(L,K)=DZIC(K)*(VPX(L,K)-VPX(L,K-1))  
     &          -(VPZ(L+1,K)-VPZ(L,K))/DXV(L)  
          ENDDO  
        ENDDO  
        DO K=1,KS  
          DO L=2,LA  
            LS=LSC(L)  
            LN=LNC(L)  
            WVPT(L,K)=(VPY(L+1,K)-VPY(L,K))/DXP(L)-(VPX(LN,K)-VPX(L,K)
     &          )/DYP(L)  
          ENDDO  
        ENDDO  
      ENDIF  
      QXW=0.  
      QXWVP=0.  
      DO K=1,KC  
        DO LL=1,NPBW  
          L=LPBW(LL)  
          QXW=QXW+UHLPF(L+1,K)*DZC(K)*DYU(L+1)  
          QXWVP=QXWVP+UVPT(L+1,K)*DZC(K)*DYU(L+1)  
        ENDDO  
      ENDDO  
      QXE=0.  
      QXEVP=0.  
      DO K=1,KC  
        DO LL=1,NPBE  
          L=LPBE(LL)  
          QXE=QXE+UHLPF(L,K)*DZC(K)*DYU(L)  
          QXEVP=QXEVP+UVPT(L,K)*DZC(K)*DYU(L)  
        ENDDO  
      ENDDO  
      QYS=0.  
      QYSVP=0.  
      DO K=1,KC  
        DO LL=1,NPBS  
          L=LPBS(LL)  
          LN=LNC(L)  
          QYS=QYS+VHLPF(LN,K)*DZC(K)*DXV(LN)  
          QYSVP=QYSVP+VVPT(LN,K)*DZC(K)*DXV(LN)  
        ENDDO  
      ENDDO  
      QYN=0.  
      QYNVP=0.  
      DO K=1,KC  
        DO LL=1,NPBN  
          L=LPBN(LL)  
          LN=LNC(L)  
          QYN=QYN+VHLPF(L,K)*DZC(K)*DXV(L)  
          QYNVP=QYNVP+VVPT(L,K)*DZC(K)*DXV(L)  
        ENDDO  
      ENDDO  
C  
C **  OUTPUT RESIDUAL TRANSPORT TO FILE RESTRAN.OUT  
C  
      IF(ISSSMMT.EQ.1.AND.N.LT.NTS) GOTO 198  
      IF(ISRESTR.EQ.1)THEN  
        IF(JSRESTR.EQ.1)THEN  
          OPEN(98,FILE='RESTRAN.OUT',STATUS='UNKNOWN')  
          CLOSE(98,STATUS='DELETE')  
          OPEN(98,FILE='RESTRAN.OUT',STATUS='UNKNOWN')  
          JSRESTR=0  
        ELSE  
          OPEN(98,FILE='RESTRAN.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        ENDIF  
        IF(NTSMMT.LT.NTSPTC)THEN  
          DO LT=2,LALT  
            I=ILLT(LT)  
            J=JLLT(LT)  
            L=LIJ(I,J)  
            WRITE(98,907)HMP(L),HLPF(L),QSUMELPF(L)  
            WRITE(98,907)(UHLPF(L,K),K=1,KC)  
            WRITE(98,907)(VHLPF(L,K),K=1,KC)  
            WRITE(98,907)(AHULPF(L,K),K=1,KC)  
            WRITE(98,907)(AHVLPF(L,K),K=1,KC)  
            WRITE(98,907)(SALLPF(L,K),K=1,KC)  
            WRITE(98,907)(ABLPF(L,K),K=1,KS)  
            WRITE(98,907)(ABEFF(L,K),K=1,KS)  
          ENDDO  
        ELSE  
          DO LT=2,LALT  
            I=ILLT(LT)  
            J=JLLT(LT)  
            L=LIJ(I,J)  
            WRITE(98,907)HMP(L),HLPF(L),QSUMELPF(L)  
            WRITE(98,907)(UHLPF(L,K),K=1,KC)  
            WRITE(98,907)(VHLPF(L,K),K=1,KC)  
            WRITE(98,907)(VPZ(L,K),K=1,KC)  
            WRITE(98,907)(AHULPF(L,K),K=1,KC)  
            WRITE(98,907)(AHVLPF(L,K),K=1,KC)  
            WRITE(98,907)(SALLPF(L,K),K=1,KC)  
            WRITE(98,907)(VPX(L,K),K=1,KS)  
            WRITE(98,907)(VPY(L,K),K=1,KS)  
            WRITE(98,907)(ABLPF(L,K),K=1,KS)  
          ENDDO  
        ENDIF  
        CLOSE(98)  
      ENDIF  
  907 FORMAT(12E12.4)  
C  
C **  OUTPUT TO WASP COMPATIABLE FILES  
C  
      IF(ISWASP.EQ.4) CALL WASP4  
      IF(ISWASP.EQ.5) CALL WASP5  
      IF(ISWASP.EQ.6) CALL WASP6  
      IF(ISWASP.EQ.7) CALL WASP7  
      IF(ISWASP.EQ.17) CALL WASP7EPA
      IF(ISRCA.GE.1) CALL RCAHQ  
      IF(ISICM.GE.1) CALL CEQICM  
  198 CONTINUE  
C  
C **  WRITE GRAPHICS FILES FOR RESIDUAL VARIABLES  
C  
      IF(ISSSMMT.EQ.1.AND.N.LT.NTS) GOTO 199  
C  
C **  RESIDUAL SALINITY CONTOURING IN HORIZONTAL: SUBROUTINE RSALPLTH  
C  
      IF(ISRSPH(1).EQ.1.AND.ISTRAN(1).GE.1)THEN  
        CALL RSALPLTH(1,SALLPF)  
      ENDIF  
      IF(ISRSPH(2).EQ.1.AND.ISTRAN(2).GE.1)THEN  
        CALL RSALPLTH(2,TEMLPF)  
      ENDIF  
      IF(ISRSPH(3).EQ.1.AND.ISTRAN(3).GE.1)THEN  
        CALL RSALPLTH(3,DYELPF)  
      ENDIF  
      IF(ISRSPH(4).EQ.1.AND.ISTRAN(4).GE.1)THEN  
        CALL RSALPLTH(4,SFLLPF)  
      ENDIF  
      DO K=2,KB  
        DO L=2,LA  
          SEDBTLPF(L,K)=0.  
          SNDBTLPF(L,K)=0.  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          TVAR1S(L,K)=TOXLPF(L,K,1)  
          SEDTLPF(L,K)=0.  
          SNDTLPF(L,K)=0.  
        ENDDO  
      ENDDO  
      IF(ISRSPH(5).EQ.1.AND.ISTRAN(5).GE.1)THEN  
        DO NT=1,NTOX  
          CALL RSALPLTH(5,TVAR1S)  
        ENDDO  
      ENDIF  
      DO NS=1,NSED  
        DO K=1,KB  
          DO L=2,LA  
            SEDBTLPF(L,K)=SEDBTLPF(L,K)+SEDBLPF(L,K,NS)  
          ENDDO  
        ENDDO  
      ENDDO  
      DO NS=1,NSED  
        DO K=1,KC  
          DO L=2,LA  
            SEDTLPF(L,K)=SEDTLPF(L,K)+SEDLPF(L,K,NS)  
          ENDDO  
        ENDDO  
      ENDDO  
      IF(ISRSPH(6).EQ.1.AND.ISTRAN(6).GE.1)THEN  
        DO NSC=1,NSED  
          CALL RSALPLTH(6,SEDTLPF)  
        ENDDO  
      ENDIF  
      DO NS=1,NSND  
        DO K=1,KB  
          DO L=2,LA  
            SNDBTLPF(L,K)=SNDBTLPF(L,K)+SNDBLPF(L,K,NS)  
          ENDDO  
        ENDDO  
      ENDDO  
      DO NS=1,NSND  
        DO K=1,KC  
          DO L=2,LA  
            SNDTLPF(L,K)=SNDTLPF(L,K)+SNDLPF(L,K,NS)  
          ENDDO  
        ENDDO  
      ENDDO  
      IF(ISRSPH(7).EQ.1.AND.ISTRAN(7).GE.1)THEN  
        DO NSN=1,NSND  
          CALL RSALPLTH(7,SNDTLPF)  
        ENDDO  
      ENDIF  
C  
C **  RESIDUAL VELOCITY VECTOR PLOTTING IN HORIZONTAL PLANES:  
C **  SUBROUTINE RVELPLTH  
C  
      IF(ISRVPH.GE.1) CALL RVELPLTH  
C  
C **  RESIDUAL SURFACE ELEVATION PLOTTING IN HORIZONTAL PLANES:  
C **  SUBROUTINE RVELPLTH  
C  
      IF(ISRPPH.EQ.1) CALL RSURFPLT  
C  
C **  RESIDUAL SALINITY AND VERTICAL MASS DIFFUSIVITY CONTOURING IN  
C **  3 VERTICAL PLANES:  SUBROUTINE RSALPLTV  
C  
      DO ITMP=1,7  
        IF(ISRSPV(ITMP).GE.1) CALL RSALPLTV(ITMP)  
      ENDDO  
C  
C **  RESIDUAL NORMAL AND TANGENTIAL VELOCITY CONTOURING AND AND  
C **  TANGENTIAL VELOCITY VECTOR PLOTTING IN VERTICAL PLANES:  
C **  SUBROUTINE RVELPLTV  
C  
      IF(ISRVPV.GE.1) CALL RVELPLTV  
C  
C **  RESIDUAL 3D SCALAR AND VECTOR OUTPUT FILES  
C  
      IF(ISR3DO.GE.1) CALL ROUT3D  
  199 CONTINUE  
      NMMT=0  
  200 CONTINUE  
      IF(ISICM.GE.1)THEN  
        NMMT=NMMT+2  
      ELSE  
        NMMT=NMMT+1  
      ENDIF  
      RETURN  
      END  

