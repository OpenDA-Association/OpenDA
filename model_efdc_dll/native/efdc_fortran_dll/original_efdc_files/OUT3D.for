      SUBROUTINE OUT3D  
C  
C CHANGE RECORD  
C  
      USE GLOBAL  

      CHARACTER *11 SALFN,TEMFN,DYEFN,SEDFN,UUUFN,VVVFN,WWWFN,  
     &    CMPFN,SNDFN,TOXFN  
      REAL,ALLOCATABLE,DIMENSION(:,:)::AKL  
      REAL,ALLOCATABLE,DIMENSION(:,:)::AIJ  

      ALLOCATE(AKL(KPCM,LCM))  
      ALLOCATE(AIJ(IGM,JGM))  
      AKL=0.
      AIJ=0.
C  
C **  INITIALIZE OUTPUT FILES  
C  
      IAD=I3DMAX-I3DMIN+1  
      JAD=J3DMAX-J3DMIN+1  
      NCALL3D=NCALL3D+1  
      IF(NCALL3D.EQ.1)THEN  
        SALFN='SAL3D01.ASC'  
        TEMFN='TEM3D01.ASC'  
        DYEFN='DYE3D01.ASC'  
        SEDFN='SED3D01.ASC'  
        SNDFN='SND3D01.ASC'  
        TOXFN='TOX3D01.ASC'  
        UUUFN='UUU3D01.ASC'  
        VVVFN='VVV3D01.ASC'  
        WWWFN='WWW3D01.ASC'  
        CMPFN='CMP3D01.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.2)THEN  
        SALFN='SAL3D02.ASC'  
        TEMFN='TEM3D02.ASC'  
        DYEFN='DYE3D02.ASC'  
        SEDFN='SED3D02.ASC'  
        SNDFN='SND3D02.ASC'  
        TOXFN='TOX3D02.ASC'  
        UUUFN='UUU3D02.ASC'  
        VVVFN='VVV3D02.ASC'  
        WWWFN='WWW3D02.ASC'  
        CMPFN='CMP3D02.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.3)THEN  
        SALFN='SAL3D03.ASC'  
        TEMFN='TEM3D03.ASC'  
        DYEFN='DYE3D03.ASC'  
        SEDFN='SED3D03.ASC'  
        SNDFN='SND3D03.ASC'  
        TOXFN='TOX3D03.ASC'  
        UUUFN='UUU3D03.ASC'  
        VVVFN='VVV3D03.ASC'  
        WWWFN='WWW3D03.ASC'  
        CMPFN='CMP3D03.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.4)THEN  
        SALFN='SAL3D04.ASC'  
        TEMFN='TEM3D04.ASC'  
        DYEFN='DYE3D04.ASC'  
        SEDFN='SED3D04.ASC'  
        SNDFN='SND3D04.ASC'  
        TOXFN='TOX3D04.ASC'  
        UUUFN='UUU3D04.ASC'  
        VVVFN='VVV3D04.ASC'  
        WWWFN='WWW3D04.ASC'  
        CMPFN='CMP3D04.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.5)THEN  
        SALFN='SAL3D05.ASC'  
        TEMFN='TEM3D05.ASC'  
        DYEFN='DYE3D05.ASC'  
        SEDFN='SED3D05.ASC'  
        SNDFN='SND3D05.ASC'  
        TOXFN='TOX3D05.ASC'  
        UUUFN='UUU3D05.ASC'  
        VVVFN='VVV3D05.ASC'  
        WWWFN='WWW3D05.ASC'  
        CMPFN='CMP3D05.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.6)THEN  
        SALFN='SAL3D06.ASC'  
        TEMFN='TEM3D06.ASC'  
        DYEFN='DYE3D06.ASC'  
        SEDFN='SED3D06.ASC'  
        SNDFN='SND3D06.ASC'  
        TOXFN='TOX3D06.ASC'  
        UUUFN='UUU3D06.ASC'  
        VVVFN='VVV3D06.ASC'  
        WWWFN='WWW3D06.ASC'  
        CMPFN='CMP3D06.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.7)THEN  
        SALFN='SAL3D07.ASC'  
        TEMFN='TEM3D07.ASC'  
        DYEFN='DYE3D07.ASC'  
        SEDFN='SED3D07.ASC'  
        SNDFN='SND3D07.ASC'  
        TOXFN='TOX3D07.ASC'  
        UUUFN='UUU3D07.ASC'  
        VVVFN='VVV3D07.ASC'  
        WWWFN='WWW3D07.ASC'  
        CMPFN='CMP3D07.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.8)THEN  
        SALFN='SAL3D08.ASC'  
        TEMFN='TEM3D08.ASC'  
        DYEFN='DYE3D08.ASC'  
        SEDFN='SED3D08.ASC'  
        SNDFN='SND3D08.ASC'  
        TOXFN='TOX3D08.ASC'  
        UUUFN='UUU3D08.ASC'  
        VVVFN='VVV3D08.ASC'  
        WWWFN='WWW3D08.ASC'  
        CMPFN='CMP3D08.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.9)THEN  
        SALFN='SAL3D09.ASC'  
        TEMFN='TEM3D09.ASC'  
        DYEFN='DYE3D09.ASC'  
        SEDFN='SED3D09.ASC'  
        SNDFN='SND3D09.ASC'  
        TOXFN='TOX3D09.ASC'  
        UUUFN='UUU3D09.ASC'  
        VVVFN='VVV3D09.ASC'  
        WWWFN='WWW3D09.ASC'  
        CMPFN='CMP3D09.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.10)THEN  
        SALFN='SAL3D10.ASC'  
        TEMFN='TEM3D10.ASC'  
        DYEFN='DYE3D10.ASC'  
        SEDFN='SED3D10.ASC'  
        SNDFN='SND3D10.ASC'  
        TOXFN='TOX3D10.ASC'  
        UUUFN='UUU3D10.ASC'  
        VVVFN='VVV3D10.ASC'  
        WWWFN='WWW3D10.ASC'  
        CMPFN='CMP3D10.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.11)THEN  
        SALFN='SAL3D11.ASC'  
        TEMFN='TEM3D11.ASC'  
        DYEFN='DYE3D11.ASC'  
        SEDFN='SED3D11.ASC'  
        SNDFN='SND3D11.ASC'  
        TOXFN='TOX3D11.ASC'  
        UUUFN='UUU3D11.ASC'  
        VVVFN='VVV3D11.ASC'  
        WWWFN='WWW3D11.ASC'  
        CMPFN='CMP3D11.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.12)THEN  
        SALFN='SAL3D12.ASC'  
        TEMFN='TEM3D12.ASC'  
        DYEFN='DYE3D12.ASC'  
        SEDFN='SED3D12.ASC'  
        SNDFN='SND3D12.ASC'  
        TOXFN='TOX3D12.ASC'  
        UUUFN='UUU3D12.ASC'  
        VVVFN='VVV3D12.ASC'  
        WWWFN='WWW3D12.ASC'  
        CMPFN='CMP3D12.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.13)THEN  
        SALFN='SAL3D13.ASC'  
        TEMFN='TEM3D13.ASC'  
        DYEFN='DYE3D13.ASC'  
        SEDFN='SED3D13.ASC'  
        SNDFN='SND3D13.ASC'  
        TOXFN='TOX3D13.ASC'  
        UUUFN='UUU3D13.ASC'  
        VVVFN='VVV3D13.ASC'  
        WWWFN='WWW3D13.ASC'  
        CMPFN='CMP3D13.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.14)THEN  
        SALFN='SAL3D14.ASC'  
        TEMFN='TEM3D14.ASC'  
        DYEFN='DYE3D14.ASC'  
        SEDFN='SED3D14.ASC'  
        SNDFN='SND3D14.ASC'  
        TOXFN='TOX3D14.ASC'  
        UUUFN='UUU3D14.ASC'  
        VVVFN='VVV3D14.ASC'  
        WWWFN='WWW3D14.ASC'  
        CMPFN='CMP3D14.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.15)THEN  
        SALFN='SAL3D15.ASC'  
        TEMFN='TEM3D15.ASC'  
        DYEFN='DYE3D15.ASC'  
        SEDFN='SED3D15.ASC'  
        SNDFN='SND3D15.ASC'  
        TOXFN='TOX3D15.ASC'  
        UUUFN='UUU3D15.ASC'  
        VVVFN='VVV3D15.ASC'  
        WWWFN='WWW3D15.ASC'  
        CMPFN='CMP3D15.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.16)THEN  
        SALFN='SAL3D16.ASC'  
        TEMFN='TEM3D16.ASC'  
        DYEFN='DYE3D16.ASC'  
        SEDFN='SED3D16.ASC'  
        SNDFN='SND3D16.ASC'  
        TOXFN='TOX3D16.ASC'  
        UUUFN='UUU3D16.ASC'  
        VVVFN='VVV3D16.ASC'  
        WWWFN='WWW3D16.ASC'  
        CMPFN='CMP3D16.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.17)THEN  
        SALFN='SAL3D17.ASC'  
        TEMFN='TEM3D17.ASC'  
        DYEFN='DYE3D17.ASC'  
        SEDFN='SED3D17.ASC'  
        SNDFN='SND3D17.ASC'  
        TOXFN='TOX3D17.ASC'  
        UUUFN='UUU3D17.ASC'  
        VVVFN='VVV3D17.ASC'  
        WWWFN='WWW3D17.ASC'  
        CMPFN='CMP3D17.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.18)THEN  
        SALFN='SAL3D18.ASC'  
        TEMFN='TEM3D18.ASC'  
        DYEFN='DYE3D18.ASC'  
        SEDFN='SED3D18.ASC'  
        SNDFN='SND3D18.ASC'  
        TOXFN='TOX3D18.ASC'  
        UUUFN='UUU3D18.ASC'  
        VVVFN='VVV3D18.ASC'  
        WWWFN='WWW3D18.ASC'  
        CMPFN='CMP3D18.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.19)THEN  
        SALFN='SAL3D19.ASC'  
        TEMFN='TEM3D19.ASC'  
        DYEFN='DYE3D19.ASC'  
        SEDFN='SED3D19.ASC'  
        SNDFN='SND3D19.ASC'  
        TOXFN='TOX3D19.ASC'  
        UUUFN='UUU3D19.ASC'  
        VVVFN='VVV3D19.ASC'  
        WWWFN='WWW3D19.ASC'  
        CMPFN='CMP3D19.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.20)THEN  
        SALFN='SAL3D20.ASC'  
        TEMFN='TEM3D20.ASC'  
        DYEFN='DYE3D20.ASC'  
        SEDFN='SED3D20.ASC'  
        SNDFN='SND3D20.ASC'  
        TOXFN='TOX3D20.ASC'  
        UUUFN='UUU3D20.ASC'  
        VVVFN='VVV3D20.ASC'  
        WWWFN='WWW3D20.ASC'  
        CMPFN='CMP3D20.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.21)THEN  
        SALFN='SAL3D21.ASC'  
        TEMFN='TEM3D21.ASC'  
        DYEFN='DYE3D21.ASC'  
        SEDFN='SED3D21.ASC'  
        SNDFN='SND3D21.ASC'  
        TOXFN='TOX3D21.ASC'  
        UUUFN='UUU3D21.ASC'  
        VVVFN='VVV3D21.ASC'  
        WWWFN='WWW3D21.ASC'  
        CMPFN='CMP3D21.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.22)THEN  
        SALFN='SAL3D22.ASC'  
        TEMFN='TEM3D22.ASC'  
        DYEFN='DYE3D22.ASC'  
        SEDFN='SED3D22.ASC'  
        SNDFN='SND3D22.ASC'  
        TOXFN='TOX3D22.ASC'  
        UUUFN='UUU3D22.ASC'  
        VVVFN='VVV3D22.ASC'  
        WWWFN='WWW3D22.ASC'  
        CMPFN='CMP3D22.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.23)THEN  
        SALFN='SAL3D23.ASC'  
        TEMFN='TEM3D23.ASC'  
        DYEFN='DYE3D23.ASC'  
        SEDFN='SED3D23.ASC'  
        SNDFN='SND3D23.ASC'  
        TOXFN='TOX3D23.ASC'  
        UUUFN='UUU3D23.ASC'  
        VVVFN='VVV3D23.ASC'  
        WWWFN='WWW3D23.ASC'  
        CMPFN='CMP3D23.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.24)THEN  
        SALFN='SAL3D24.ASC'  
        TEMFN='TEM3D24.ASC'  
        DYEFN='DYE3D24.ASC'  
        SEDFN='SED3D24.ASC'  
        SNDFN='SND3D24.ASC'  
        TOXFN='TOX3D24.ASC'  
        UUUFN='UUU3D24.ASC'  
        VVVFN='VVV3D24.ASC'  
        WWWFN='WWW3D24.ASC'  
        CMPFN='CMP3D24.ASC'  
      ENDIF  
      IF(NCALL3D.EQ.1)THEN  
        OPEN(50,FILE='OUT3D.DIA',STATUS='UNKNOWN')  
        CLOSE(50,STATUS='DELETE')  
        OPEN(50,FILE='OUT3D.DIA',STATUS='UNKNOWN')  
        WRITE(50,520)IAD,JAD  
        WRITE(50,530)NCALL3D  
        DO KP=1,KPC  
          WRITE(50,502)KP,ZZP(KP)  
        ENDDO  
      ELSE  
        OPEN(50,FILE='OUT3D.DIA',POSITION='APPEND',STATUS='UNKNOWN')  
        WRITE(50,530)NCALL3D  
      ENDIF  
      IF(IS3DSAL.GE.1)THEN  
        ASALMAX=-99999999.  
        ASALMIN= 99999999.  
        DO K=1,KC  
          DO L=2,LA  
            TMPVAL=SAL(L,K)  
            ASALMAX=MAX(ASALMAX,TMPVAL)  
            ASALMIN=MIN(ASALMIN,TMPVAL)  
          ENDDO  
        ENDDO  
        WRITE(50,521)ASALMAX,ASALMIN  
        IF(JS3DSAL.EQ.0)THEN  
          SAL3DMA=255.  
          SAL3DMI=0.  
        ENDIF  
        IF(JS3DSAL.EQ.1)THEN  
          SAL3DMA=ASALMAX  
          SAL3DMI=ASALMIN  
        ENDIF  
        OPEN(51,FILE=SALFN,STATUS='UNKNOWN')  
        CLOSE(51,STATUS='DELETE')  
        OPEN(51,FILE=SALFN,STATUS='UNKNOWN')  
      ENDIF  
      IF(IS3DTEM.GE.1)THEN  
        ATEMMAX=-99999999.  
        ATEMMIN= 99999999.  
        DO K=1,KC  
          DO L=2,LA  
            TMPVAL=TEM(L,K)  
            ATEMMAX=MAX(ATEMMAX,TMPVAL)  
            ATEMMIN=MIN(ATEMMIN,TMPVAL)  
          ENDDO  
        ENDDO  
        WRITE(50,522)ATEMMAX,ATEMMIN  
        IF(JS3DTEM.EQ.0)THEN  
          TEM3DMA=255.  
          TEM3DMI=0.  
        ENDIF  
        IF(JS3DTEM.EQ.1)THEN  
          TEM3DMA=ATEMMAX  
          TEM3DMI=ATEMMIN  
        ENDIF  
        OPEN(52,FILE=TEMFN,STATUS='UNKNOWN')  
        CLOSE(52,STATUS='DELETE')  
        OPEN(52,FILE=TEMFN,STATUS='UNKNOWN')  
      ENDIF  
      IF(IS3DDYE.GE.1)THEN  
        ADYEMAX=-99999999.  
        ADYEMIN= 99999999.  
        DO K=1,KC  
          DO L=2,LA  
            TMPVAL=DYE(L,K)  
            ADYEMAX=MAX(ADYEMAX,TMPVAL)  
            ADYEMIN=MIN(ADYEMIN,TMPVAL)  
          ENDDO  
        ENDDO  
        WRITE(50,523)ADYEMAX,ADYEMIN  
        IF(JS3DDYE.EQ.0)THEN  
          DYE3DMA=255.  
          DYE3DMI=0.  
        ENDIF  
        IF(JS3DDYE.EQ.1)THEN  
          DYE3DMA=ADYEMAX  
          DYE3DMI=ADYEMIN  
        ENDIF  
        OPEN(53,FILE=DYEFN,STATUS='UNKNOWN')  
        CLOSE(53,STATUS='DELETE')  
        OPEN(53,FILE=DYEFN,STATUS='UNKNOWN')  
      ENDIF  
      IF(IS3DSED.GE.1)THEN  
        ASEDMAX=-99999999.  
        ASEDMIN= 99999999.  
        DO K=1,KC  
          DO L=2,LA  
            TMPVAL=SEDT(L,K)  
            ASEDMAX=MAX(ASEDMAX,TMPVAL)  
            ASEDMIN=MIN(ASEDMIN,TMPVAL)  
          ENDDO  
        ENDDO  
        WRITE(50,524)ASEDMAX,ASEDMIN  
        IF(JS3DSED.EQ.0)THEN  
          SED3DMA=255.  
          SED3DMI=0.  
        ENDIF  
        IF(JS3DSED.EQ.1)THEN  
          SED3DMA=ASEDMAX  
          SED3DMI=ASEDMIN  
        ENDIF  
        OPEN(54,FILE=SEDFN,STATUS='UNKNOWN')  
        CLOSE(54,STATUS='DELETE')  
        OPEN(54,FILE=SEDFN,STATUS='UNKNOWN')  
      ENDIF  
      IF(IS3DUUU.GE.1)THEN  
        AUUUMAX=-99999999.  
        AUUUMIN= 99999999.  
        DO K=1,KC  
          DO L=2,LA  
            TMPVAL=0.5*(U(L,K)+U(L+1,K))  
            AUUUMAX=MAX(AUUUMAX,TMPVAL)  
            AUUUMIN=MIN(AUUUMIN,TMPVAL)  
          ENDDO  
        ENDDO  
        WRITE(50,525)AUUUMAX,AUUUMIN  
        IF(JS3DUUU.EQ.0)THEN  
          UUU3DMA=255.  
          UUU3DMI=0.  
        ENDIF  
        IF(JS3DUUU.EQ.1)THEN  
          UUU3DMA=AUUUMAX  
          UUU3DMI=AUUUMIN  
        ENDIF  
        OPEN(55,FILE=UUUFN,STATUS='UNKNOWN')  
        CLOSE(55,STATUS='DELETE')  
        OPEN(55,FILE=UUUFN,STATUS='UNKNOWN')  
      ENDIF  
      IF(IS3DVVV.GE.1)THEN  
        AVVVMAX=-99999999.  
        AVVVMIN= 99999999.  
        DO K=1,KC  
          DO L=2,LA  
            TMPVAL=0.5*(V(L,K)+V(LNC(L),K))  
            AVVVMAX=MAX(AVVVMAX,TMPVAL)  
            AVVVMIN=MIN(AVVVMIN,TMPVAL)  
          ENDDO  
        ENDDO  
        WRITE(50,526)AVVVMAX,AVVVMIN  
        IF(JS3DVVV.EQ.0)THEN  
          VVV3DMA=255.  
          VVV3DMI=0.  
        ENDIF  
        IF(JS3DVVV.EQ.1)THEN  
          VVV3DMA=AVVVMAX  
          VVV3DMI=AVVVMIN  
        ENDIF  
        OPEN(56,FILE=VVVFN,STATUS='UNKNOWN')  
        CLOSE(56,STATUS='DELETE')  
        OPEN(56,FILE=VVVFN,STATUS='UNKNOWN')  
      ENDIF  
      IF(IS3DWWW.GE.1)THEN  
        AWWWMAX=-99999999.  
        AWWWMIN= 99999999.  
        DO K=1,KC  
          DO L=2,LA  
            LN=LNC(L)  
            LS=LSC(L)  
            TMPVAL=0.5*(W(L,K)+W(L,K-1))  
     &          +GI*ZZ(K)*( DTI*(P(L)-P1(L))  
     &          +0.5*(U(L+1,K)*(P(L+1)-P(L))*DXIU(L+1)  
     &          +U(L,K)*(P(L)-P(L-1))*DXIU(L)  
     &          +V(LN,K)*(P(LN)-P(L))*DYIV(LNC(L))  
     &          +V(L,K)*(P(L)-P(LS))*DYIV(L)) )  
     &          +0.5*(1.-ZZ(K))*( U(L+1,K)*(BELV(L+1)-BELV(L))*DXIU(L+1)  
     &          +U(L,K)*(BELV(L)-BELV(L-1))*DXIU(L)  
     &          +V(LN,K)*(BELV(LN)-BELV(L))*DYIV(LN)  
     &          +V(L,K)*(BELV(L)-BELV(LS))*DYIV(L) )  
            AWWWMAX=MAX(AWWWMAX,TMPVAL)  
            AWWWMIN=MIN(AWWWMIN,TMPVAL)  
          ENDDO  
        ENDDO  
        WRITE(50,527)AWWWMAX,AWWWMIN  
        IF(JS3DWWW.EQ.0)THEN  
          WWW3DMA=255.  
          WWW3DMI=0.  
        ENDIF  
        IF(JS3DWWW.EQ.1)THEN  
          WWW3DMA=AWWWMAX  
          WWW3DMI=AWWWMIN  
        ENDIF  
        OPEN(57,FILE=WWWFN,STATUS='UNKNOWN')  
        CLOSE(57,STATUS='DELETE')  
        OPEN(57,FILE=WWWFN,STATUS='UNKNOWN')  
      ENDIF  
      IF(IS3DSND.GE.1)THEN  
        ASNDMAX=-99999999.  
        ASNDMIN= 99999999.  
        DO K=1,KC  
          DO L=2,LA  
            TMPVAL=SNDT(L,K)  
            ASNDMAX=MAX(ASNDMAX,TMPVAL)  
            ASNDMIN=MIN(ASNDMIN,TMPVAL)  
          ENDDO  
        ENDDO  
        WRITE(50,528)ASNDMAX,ASNDMIN  
        IF(JS3DSND.EQ.0)THEN  
          SND3DMA=255.  
          SED3DMI=0.  
        ENDIF  
        IF(JS3DSED.EQ.1)THEN  
          SND3DMA=ASNDMAX  
          SND3DMI=ASNDMIN  
        ENDIF  
        OPEN(58,FILE=SNDFN,STATUS='UNKNOWN')  
        CLOSE(58,STATUS='DELETE')  
        OPEN(58,FILE=SNDFN,STATUS='UNKNOWN')  
      ENDIF  
      IF(IS3DTOX.GE.1)THEN  
        ATOXMAX=-99999999.  
        ATOXMIN= 99999999.  
        DO K=1,KC  
          DO L=2,LA  
            TMPVAL=TOX(L,K,1)  
            ASEDMAX=MAX(ATOXMAX,TMPVAL)  
            ASEDMIN=MIN(ATOXMIN,TMPVAL)  
          ENDDO  
        ENDDO  
        WRITE(50,529)ATOXMAX,ATOXMIN  
        IF(JS3DTOX.EQ.0)THEN  
          TOX3DMA=255.  
          TOX3DMI=0.  
        ENDIF  
        IF(JS3DTOX.EQ.1)THEN  
          TOX3DMA=ATOXMAX  
          TOX3DMI=ATOXMIN  
        ENDIF  
        OPEN(59,FILE=TOXFN,STATUS='UNKNOWN')  
        CLOSE(59,STATUS='DELETE')  
        OPEN(59,FILE=TOXFN,STATUS='UNKNOWN')  
      ENDIF  
      OPEN(99,FILE=CMPFN,STATUS='UNKNOWN')  
      CLOSE(99,STATUS='DELETE')  
      OPEN(99,FILE=CMPFN,STATUS='UNKNOWN')  
C  
C **  BEGIN LOOP TO LOAD OUTPUT FILES  
C  
      DO L=2,LA  
        LN=LNC(L)  
        LS=LSC(L)  
        DO KP=1,KPC  
          IAP(KP)=0  
          AP(KP)=0.  
        ENDDO  
        DO K=1,KC  
          DO KP=1,KPC  
            APT(KP,K)=0.  
          ENDDO  
        ENDDO  
        DO KP=1,KPC  
          ZZPS=(ZZP(KP)-BELV(L))*HPI(L)  
          IF(ZZPS.GE.0.)THEN  
            KPB(L)=KP  
            GOTO 190  
          ENDIF  
        ENDDO  
  190   CONTINUE  
        DO KP=KPC,1,-1  
          ZZPS=(ZZP(KP)-BELV(L))*HPI(L)  
          IF(ZZPS.LE.1.)THEN  
            KPS(L)=KP  
            GOTO 195  
          ENDIF  
        ENDDO  
  195   CONTINUE  
C  
C*DIAGNOSTIC  
C*DIAGNOSTIC  
C  
        DO KP=KPB(L),KPS(L)  
          ZZPS=(ZZP(KP)-BELV(L))*HPI(L)  
          IF(ZZPS.GE.0.0.AND.ZZPS.LE.1.0)THEN  
            IF(ZZPS.GE.ZZ(KC))THEN  
              APT(KP,KC)= (ZZPS-ZZ(KS))/(ZZ(KC)-ZZ(KS))  
              APT(KP,KS)=-(ZZPS-ZZ(KC))/(ZZ(KC)-ZZ(KS))  
            ELSE  
              IF(ZZPS.LE.ZZ(1))THEN  
                APT(KP,2)= (ZZPS-ZZ(1))/(ZZ(2)-ZZ(1))  
                APT(KP,1)=-(ZZPS-ZZ(2))/(ZZ(2)-ZZ(1))  
              ELSE  
                K=1  
  200           K=K+1  
                IF(ZZPS.GT.ZZ(K-1).AND.ZZPS.LE.ZZ(K))THEN  
                  APT(KP,K)  = (ZZPS-ZZ(K-1))/(ZZ(K)-ZZ(K-1))  
                  APT(KP,K-1)=-(ZZPS-ZZ(K))/(ZZ(K)-ZZ(K-1))  
                ELSE  
                  GOTO 200  
                ENDIF  
              ENDIF  
            ENDIF  
          ENDIF  
        ENDDO  
C  
C*DIAGNOSTIC  
C*DIAGNOSTIC  
C  
        DO K=1,KC  
          TMP3D(K)=1.0  
        ENDDO  
        DO KP=KPB(L),KPS(L)  
          AP(KP)=0.  
          DO K=1,KC  
            AP(KP)=AP(KP)+APT(KP,K)*TMP3D(K)  
          ENDDO  
        ENDDO  
        DO KP=KPB(L),KPS(L)  
          IAP(KP)=NINT(AP(KP))  
        ENDDO  
        WRITE(99,559)IL(L),JL(L),(IAP(K),K=1,KPC)  
        IF(IS3DSAL.GE.1)THEN  
          DO K=1,KC  
            TMP3D(K)=SAL(L,K)  
          ENDDO  
          SCALE3D=254./(SAL3DMA-SAL3DMI)  
          DO KP=KPB(L),KPS(L)  
            AP(KP)=0.  
            DO K=1,KC  
              AP(KP)=AP(KP)+APT(KP,K)*TMP3D(K)  
C  
C*DIAGNOSTIC  
C*DIAGNOSTIC  
C  
            ENDDO  
          ENDDO  
          DO KP=KPB(L),KPS(L)  
            IAP(KP)=NINT((AP(KP)-SAL3DMI)*SCALE3D)+1  
            IF(IAP(KP).GT.255) IAP(KP)=255  
            AP(KP)=SAL3DMA*AP(KP)  
          ENDDO  
          IF(JS3DSAL.LE.2) WRITE(51,501)(IAP(K),K=1,KPC)  
          IF(JS3DSAL.EQ.3) WRITE(51,551)(AP(K),K=1,KPC)  
C  
C*DIAGNOSTIC  
C*DIAGNOSTIC  
C  
        ENDIF  
        IF(IS3DTEM.GE.1)THEN  
          DO K=1,KC  
            TMP3D(K)=TEM(L,K)  
          ENDDO  
          SCALE3D=254./(TEM3DMA-TEM3DMI)  
          DO KP=KPB(L),KPS(L)  
            AP(KP)=0.  
            DO K=1,KC  
              AP(KP)=AP(KP)+APT(KP,K)*TMP3D(K)  
            ENDDO  
          ENDDO  
          DO KP=KPB(L),KPS(L)  
            IAP(KP)=NINT((AP(KP)-TEM3DMI)*SCALE3D)+1  
            IF(IAP(KP).GT.255) IAP(KP)=255  
            AP(KP)=TEM3DMA*AP(KP)  
          ENDDO  
          IF(JS3DTEM.LE.2) WRITE(52,501)(IAP(K),K=1,KPC)  
          IF(JS3DTEM.EQ.3) WRITE(52,551)(AP(K),K=1,KPC)  
        ENDIF  
        IF(IS3DDYE.GE.1)THEN  
          DO K=1,KC  
            TMP3D(K)=DYE(L,K)  
          ENDDO  
          SCALE3D=254./(DYE3DMA-DYE3DMI)  
          DO KP=KPB(L),KPS(L)  
            AP(KP)=0.  
            DO K=1,KC  
              AP(KP)=AP(KP)+APT(KP,K)*TMP3D(K)  
            ENDDO  
          ENDDO  
          DO KP=KPB(L),KPS(L)  
            IAP(KP)=NINT((AP(KP)-DYE3DMI)*SCALE3D)+1  
            IF(IAP(KP).GT.255) IAP(KP)=255  
            AP(KP)=DYE3DMA*AP(KP)  
          ENDDO  
          IF(JS3DDYE.LE.2) WRITE(53,501)(IAP(K),K=1,KPC)  
          IF(JS3DDYE.EQ.3) WRITE(53,551)(AP(K),K=1,KPC)  
        ENDIF  
        IF(IS3DSED.GE.1)THEN  
          DO K=1,KC  
            TMP3D(K)=SEDT(L,K)  
          ENDDO  
          SCALE3D=254./(SED3DMA-SED3DMI)  
          DO KP=KPB(L),KPS(L)  
            AP(KP)=0.  
            DO K=1,KC  
              AP(KP)=AP(KP)+APT(KP,K)*TMP3D(K)  
            ENDDO  
          ENDDO  
          DO KP=KPB(L),KPS(L)  
            IAP(KP)=NINT((AP(KP)-SED3DMI)*SCALE3D)+1  
            IF(IAP(KP).GT.255) IAP(KP)=255  
            AP(KP)=SED3DMA*AP(KP)  
          ENDDO  
          IF(JS3DSED.LE.2) WRITE(54,501)(IAP(K),K=1,KPC)  
          IF(JS3DSED.EQ.3) WRITE(54,551)(AP(K),K=1,KPC)  
        ENDIF  
        IF(IS3DUUU.GE.1)THEN  
          DO K=1,KC  
            TMP3D(K)=0.5*(U(L,K)+U(L+1,K))  
          ENDDO  
          SCALE3D=254./(UUU3DMA-UUU3DMI)  
          DO KP=KPB(L),KPS(L)  
            AP(KP)=0.  
            DO K=1,KC  
              AP(KP)=AP(KP)+APT(KP,K)*TMP3D(K)  
            ENDDO  
          ENDDO  
          DO KP=KPB(L),KPS(L)  
            IAP(KP)=NINT((AP(KP)-UUU3DMI)*SCALE3D)+1  
            IF(IAP(KP).GT.255) IAP(KP)=255  
            AP(KP)=UUU3DMA*AP(KP)  
          ENDDO  
          IF(JS3DUUU.LE.2) WRITE(55,501)(IAP(K),K=1,KPC)  
          IF(JS3DUUU.EQ.3) WRITE(55,551)(AP(K),K=1,KPC)  
        ENDIF  
        IF(IS3DVVV.GE.1)THEN  
          DO K=1,KC  
            TMP3D(K)=0.5*(V(L,K)+V(LN,K))  
          ENDDO  
          SCALE3D=254./(VVV3DMA-VVV3DMI)  
          DO KP=KPB(L),KPS(L)  
            AP(KP)=0.  
            DO K=1,KC  
              AP(KP)=AP(KP)+APT(KP,K)*TMP3D(K)  
            ENDDO  
          ENDDO  
          DO KP=KPB(L),KPS(L)  
            IAP(KP)=NINT((AP(KP)-VVV3DMI)*SCALE3D)+1  
            IF(IAP(KP).GT.255) IAP(KP)=255  
            AP(KP)=VVV3DMA*AP(KP)  
          ENDDO  
          IF(JS3DVVV.LE.2) WRITE(56,501)(IAP(K),K=1,KPC)  
          IF(JS3DVVV.EQ.3) WRITE(56,551)(AP(K),K=1,KPC)  
        ENDIF  
        IF(IS3DWWW.GE.1)THEN  
          DO K=1,KC  
            TMP3D(K)=0.5*(W(L,K)+W(L,K-1))  
     &          +GI*ZZ(K)*( DTI*(P(L)-P1(L))  
     &          +0.5*(U(L+1,K)*(P(L+1)-P(L))*DXIU(L+1)  
     &          +U(L,K)*(P(L)-P(L-1))*DXIU(L)  
     &          +V(LN,K)*(P(LN)-P(L))*DYIV(LNC(L))  
     &          +V(L,K)*(P(L)-P(LS))*DYIV(L)) )  
     &          +0.5*(1.-ZZ(K))*( U(L+1,K)*(BELV(L+1)-BELV(L))*DXIU(L+1)  
     &          +U(L,K)*(BELV(L)-BELV(L-1))*DXIU(L)  
     &          +V(LN,K)*(BELV(LN)-BELV(L))*DYIV(LN)  
     &          +V(L,K)*(BELV(L)-BELV(LS))*DYIV(L) )  
          ENDDO  
          SCALE3D=254./(WWW3DMA-WWW3DMI)  
          DO KP=KPB(L),KPS(L)  
            AP(KP)=0.  
            DO K=1,KC  
              AP(KP)=AP(KP)+APT(KP,K)*TMP3D(K)  
            ENDDO  
          ENDDO  
          DO KP=KPB(L),KPS(L)  
            IAP(KP)=NINT((AP(KP)-WWW3DMI)*SCALE3D)+1  
            IF(IAP(KP).GT.255) IAP(KP)=255  
            AP(KP)=WWW3DMA*AP(KP)  
          ENDDO  
          IF(JS3DWWW.LE.2) WRITE(57,501)(IAP(K),K=1,KPC)  
          IF(JS3DWWW.EQ.3) WRITE(57,551)(AP(K),K=1,KPC)  
        ENDIF  
        IF(IS3DSND.GE.1)THEN  
          DO K=1,KC  
            TMP3D(K)=SNDT(L,K)  
          ENDDO  
          SCALE3D=254./(SND3DMA-SND3DMI)  
          DO KP=KPB(L),KPS(L)  
            AP(KP)=0.  
            DO K=1,KC  
              AP(KP)=AP(KP)+APT(KP,K)*TMP3D(K)  
            ENDDO  
          ENDDO  
          DO KP=KPB(L),KPS(L)  
            IAP(KP)=NINT((AP(KP)-SND3DMI)*SCALE3D)+1  
            IF(IAP(KP).GT.255) IAP(KP)=255  
            AP(KP)=SND3DMA*AP(KP)  
          ENDDO  
          IF(JS3DSND.LE.2) WRITE(58,501)(IAP(K),K=1,KPC)  
          IF(JS3DSND.EQ.3) WRITE(58,551)(AP(K),K=1,KPC)  
        ENDIF  
        IF(IS3DTOX.GE.1)THEN  
          DO K=1,KC  
            TMP3D(K)=TOX(L,K,1)  
          ENDDO  
          SCALE3D=254./(TOX3DMA-TOX3DMI)  
          DO KP=KPB(L),KPS(L)  
            AP(KP)=0.  
            DO K=1,KC  
              AP(KP)=AP(KP)+APT(KP,K)*TMP3D(K)  
            ENDDO  
          ENDDO  
          DO KP=KPB(L),KPS(L)  
            IAP(KP)=NINT((AP(KP)-TOX3DMI)*SCALE3D)+1  
            IF(IAP(KP).GT.255) IAP(KP)=255  
            AP(KP)=TOX3DMA*AP(KP)  
          ENDDO  
          IF(JS3DSND.LE.2) WRITE(59,501)(IAP(K),K=1,KPC)  
          IF(JS3DSND.EQ.3) WRITE(59,551)(AP(K),K=1,KPC)  
        ENDIF  
      ENDDO  
      IF(IS3DSAL.GE.1) CLOSE(51)  
      IF(IS3DTEM.GE.1) CLOSE(52)  
      IF(IS3DDYE.GE.1) CLOSE(53)  
      IF(IS3DSED.GE.1) CLOSE(54)  
      IF(IS3DUUU.GE.1) CLOSE(55)  
      IF(IS3DVVV.GE.1) CLOSE(56)  
      IF(IS3DWWW.GE.1) CLOSE(57)  
      IF(IS3DSND.GE.1) CLOSE(58)  
      IF(IS3DTOX.GE.1) CLOSE(59)  
      CLOSE(99)  
C  
C **  REWRITE OUTPUT ARRAYS INTO CORRECT ORDER IF I3DRW.EQ.1  
C  
      IF(I3DRW.EQ.1)THEN  
        DO J=1,JG  
          DO I=1,IG  
            IAIJ(I,J)=0  
            AIJ(I,J)=0.0  
          ENDDO  
        ENDDO  
        IF(ISCLO.EQ.0.OR.NWGG.EQ.0)THEN  
          IF(IS3DSAL.GE.1)THEN  
            OPEN(51,FILE=SALFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              IF(JS3DSAL.LE.2) READ(51,*)(IAKL(K,L),K=1,KPC)  
              IF(JS3DSAL.EQ.3) READ(51,*)(AKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(51,STATUS='DELETE')  
            OPEN(51,FILE=SALFN,STATUS='UNKNOWN')  
            IF(JS3DSAL.LE.2)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  IAIJ(IL(L),JL(L))=IAKL(K,L)  
                ENDDO  
                IF(K.EQ.1.AND.JS3DSAL.GT.0)THEN  
                  IF(IJCT(I3DMIN,J3DMIN).EQ.0)THEN  
                    IAIJ(I3DMIN,J3DMIN)=255  
                  ENDIF  
                ENDIF  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(51,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
                IAIJ(I3DMIN,J3DMIN)=0  
              ENDDO  
            ENDIF  
            IF(JS3DSAL.GE.3.)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  AIJ(IL(L),JL(L))=AKL(K,L)  
                ENDDO  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(51,551)(AIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
              ENDDO  
            ENDIF  
            CLOSE(51)  
          ENDIF  
          IF(IS3DTEM.GE.1)THEN  
            OPEN(52,FILE=TEMFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              IF(JS3DTEM.LE.2) READ(52,*)(IAKL(K,L),K=1,KPC)  
              IF(JS3DTEM.EQ.3) READ(52,*)(AKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(52,STATUS='DELETE')  
            OPEN(52,FILE=TEMFN,STATUS='UNKNOWN')  
            IF(JS3DTEM.LE.2)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  IAIJ(IL(L),JL(L))=IAKL(K,L)  
                ENDDO  
                IF(K.EQ.1.AND.JS3DTEM.GT.0)THEN  
                  IF(IJCT(I3DMIN,J3DMIN).EQ.0)THEN  
                    IAIJ(I3DMIN,J3DMIN)=255  
                  ENDIF  
                ENDIF  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(52,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
                IAIJ(I3DMIN,J3DMIN)=0  
              ENDDO  
            ENDIF  
            IF(JS3DTEM.GE.3.)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  AIJ(IL(L),JL(L))=AKL(K,L)  
                ENDDO  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(52,551)(AIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
              ENDDO  
            ENDIF  
            CLOSE(52)  
          ENDIF  
          IF(IS3DDYE.GE.1)THEN  
            OPEN(53,FILE=DYEFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              IF(JS3DDYE.LE.2) READ(53,*)(IAKL(K,L),K=1,KPC)  
              IF(JS3DDYE.EQ.3) READ(53,*)(AKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(53,STATUS='DELETE')  
            OPEN(53,FILE=DYEFN,STATUS='UNKNOWN')  
            IF(JS3DDYE.LE.2)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  IAIJ(IL(L),JL(L))=IAKL(K,L)  
                ENDDO  
                IF(K.EQ.1.AND.JS3DDYE.GT.0)THEN  
                  IF(IJCT(I3DMIN,J3DMIN).EQ.0)THEN  
                    IAIJ(I3DMIN,J3DMIN)=255  
                  ENDIF  
                ENDIF  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(53,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
                IAIJ(I3DMIN,J3DMIN)=0  
              ENDDO  
            ENDIF  
            IF(JS3DDYE.GE.3.)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  AIJ(IL(L),JL(L))=AKL(K,L)  
                ENDDO  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(53,551)(AIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
              ENDDO  
            ENDIF  
            CLOSE(53)  
          ENDIF  
          IF(IS3DSED.GE.1)THEN  
            OPEN(54,FILE=SEDFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              IF(JS3DSED.LE.2) READ(54,*)(IAKL(K,L),K=1,KPC)  
              IF(JS3DSED.EQ.3) READ(54,*)(AKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(54,STATUS='DELETE')  
            OPEN(54,FILE=SEDFN,STATUS='UNKNOWN')  
            IF(JS3DSED.LE.2)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  IAIJ(IL(L),JL(L))=IAKL(K,L)  
                ENDDO  
                IF(K.EQ.1.AND.JS3DSED.GT.0)THEN  
                  IF(IJCT(I3DMIN,J3DMIN).EQ.0)THEN  
                    IAIJ(I3DMIN,J3DMIN)=255  
                  ENDIF  
                ENDIF  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(54,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
                IAIJ(I3DMIN,J3DMIN)=0  
              ENDDO  
            ENDIF  
            IF(JS3DSED.GE.3.)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  AIJ(IL(L),JL(L))=AKL(K,L)  
                ENDDO  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(54,551)(AIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
              ENDDO  
            ENDIF  
            CLOSE(54)  
          ENDIF  
          IF(IS3DUUU.GE.1)THEN  
            OPEN(55,FILE=UUUFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              IF(JS3DUUU.LE.2) READ(55,*)(IAKL(K,L),K=1,KPC)  
              IF(JS3DUUU.EQ.3) READ(55,*)(AKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(55,STATUS='DELETE')  
            OPEN(55,FILE=UUUFN,STATUS='UNKNOWN')  
            IF(JS3DUUU.LE.2)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  IAIJ(IL(L),JL(L))=IAKL(K,L)  
                ENDDO  
                IF(K.EQ.1.AND.JS3DUUU.GT.0)THEN  
                  IF(IJCT(I3DMIN,J3DMIN).EQ.0)THEN  
                    IAIJ(I3DMIN,J3DMIN)=255  
                  ENDIF  
                ENDIF  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(55,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
                IAIJ(I3DMIN,J3DMIN)=0  
              ENDDO  
            ENDIF  
            IF(JS3DUUU.GE.3.)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  AIJ(IL(L),JL(L))=AKL(K,L)  
                ENDDO  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(55,551)(AIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
              ENDDO  
            ENDIF  
            CLOSE(55)  
          ENDIF  
          IF(IS3DVVV.GE.1)THEN  
            OPEN(56,FILE=VVVFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              IF(JS3DVVV.LE.2) READ(56,*)(IAKL(K,L),K=1,KPC)  
              IF(JS3DVVV.EQ.3) READ(56,*)(AKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(56,STATUS='DELETE')  
            OPEN(56,FILE=VVVFN,STATUS='UNKNOWN')  
            IF(JS3DVVV.LE.2)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  IAIJ(IL(L),JL(L))=IAKL(K,L)  
                ENDDO  
                IF(K.EQ.1.AND.JS3DVVV.GT.0)THEN  
                  IF(IJCT(I3DMIN,J3DMIN).EQ.0)THEN  
                    IAIJ(I3DMIN,J3DMIN)=255  
                  ENDIF  
                ENDIF  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(56,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
                IAIJ(I3DMIN,J3DMIN)=0  
              ENDDO  
            ENDIF  
            IF(JS3DVVV.GE.3.)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  AIJ(IL(L),JL(L))=AKL(K,L)  
                ENDDO  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(56,551)(AIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
              ENDDO  
            ENDIF  
            CLOSE(56)  
          ENDIF  
          IF(IS3DWWW.GE.1)THEN  
            OPEN(57,FILE=WWWFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              IF(JS3DWWW.LE.2) READ(57,*)(IAKL(K,L),K=1,KPC)  
              IF(JS3DWWW.EQ.3) READ(57,*)(AKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(57,STATUS='DELETE')  
            OPEN(57,FILE=WWWFN,STATUS='UNKNOWN')  
            IF(JS3DWWW.LE.2)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  IAIJ(IL(L),JL(L))=IAKL(K,L)  
                ENDDO  
                IF(K.EQ.1.AND.JS3DWWW.GT.0)THEN  
                  IF(IJCT(I3DMIN,J3DMIN).EQ.0)THEN  
                    IAIJ(I3DMIN,J3DMIN)=255  
                  ENDIF  
                ENDIF  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(57,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
                IAIJ(I3DMIN,J3DMIN)=0  
              ENDDO  
            ENDIF  
            IF(JS3DWWW.GE.3.)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  AIJ(IL(L),JL(L))=AKL(K,L)  
                ENDDO  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(57,551)(AIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
              ENDDO  
            ENDIF  
            CLOSE(57)  
          ENDIF  
          IF(IS3DSND.GE.1)THEN  
            OPEN(58,FILE=SNDFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              IF(JS3DSND.LE.2) READ(58,*)(IAKL(K,L),K=1,KPC)  
              IF(JS3DSND.EQ.3) READ(58,*)(AKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(58,STATUS='DELETE')  
            OPEN(58,FILE=SNDFN,STATUS='UNKNOWN')  
            IF(JS3DSND.LE.2)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  IAIJ(IL(L),JL(L))=IAKL(K,L)  
                ENDDO  
                IF(K.EQ.1.AND.JS3DSND.GT.0)THEN  
                  IF(IJCT(I3DMIN,J3DMIN).EQ.0)THEN  
                    IAIJ(I3DMIN,J3DMIN)=255  
                  ENDIF  
                ENDIF  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(58,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
                IAIJ(I3DMIN,J3DMIN)=0  
              ENDDO  
            ENDIF  
            IF(JS3DSND.GE.3.)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  AIJ(IL(L),JL(L))=AKL(K,L)  
                ENDDO  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(58,551)(AIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
              ENDDO  
            ENDIF  
            CLOSE(58)  
          ENDIF  
          IF(IS3DTOX.GE.1)THEN  
            OPEN(59,FILE=TOXFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              IF(JS3DTOX.LE.2) READ(59,*)(IAKL(K,L),K=1,KPC)  
              IF(JS3DTOX.EQ.3) READ(59,*)(AKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(59,STATUS='DELETE')  
            OPEN(59,FILE=TOXFN,STATUS='UNKNOWN')  
            IF(JS3DTOX.LE.2)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  IAIJ(IL(L),JL(L))=IAKL(K,L)  
                ENDDO  
                IF(K.EQ.1.AND.JS3DTOX.GT.0)THEN  
                  IF(IJCT(I3DMIN,J3DMIN).EQ.0)THEN  
                    IAIJ(I3DMIN,J3DMIN)=255  
                  ENDIF  
                ENDIF  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(54,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
                IAIJ(I3DMIN,J3DMIN)=0  
              ENDDO  
            ENDIF  
            IF(JS3DTOX.GE.3.)THEN  
              DO K=1,KPC  
                DO L=2,LA  
                  AIJ(IL(L),JL(L))=AKL(K,L)  
                ENDDO  
                DO J=J3DMAX,J3DMIN,-1  
                  WRITE(59,551)(AIJ(I,J),I=I3DMAX,I3DMIN,-1)  
                ENDDO  
              ENDDO  
            ENDIF  
            CLOSE(59)  
          ENDIF  
        ELSE  
          IF(IS3DSAL.GE.1)THEN  
            OPEN(51,FILE=SALFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              READ(51,*)(IAKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(51,STATUS='DELETE')  
            OPEN(51,FILE=SALFN,STATUS='UNKNOWN')  
            DO K=1,KPC  
              DO NW=1,NWGG  
                L=LWGG(NW)  
                IAIJ(IWGG(NW),JWGG(NW))=IAKL(K,L)  
              ENDDO  
              IF(K.EQ.1.AND.JS3DSAL.GT.0)THEN  
                IAIJ(I3DMIN,J3DMIN)=255  
              ENDIF  
              DO J=J3DMAX,J3DMIN,-1  
                WRITE(51,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
              ENDDO  
              IAIJ(I3DMIN,J3DMIN)=0  
            ENDDO  
            CLOSE(51)  
          ENDIF  
          IF(IS3DTEM.GE.1)THEN  
            OPEN(52,FILE=TEMFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              READ(52,*)(IAKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(52,STATUS='DELETE')  
            OPEN(52,FILE=TEMFN,STATUS='UNKNOWN')  
            DO K=1,KPC  
              DO NW=1,NWGG  
                L=LWGG(NW)  
                IAIJ(IWGG(NW),JWGG(NW))=IAKL(K,L)  
              ENDDO  
              IF(K.EQ.1.AND.JS3DTEM.GT.0)THEN  
                IAIJ(I3DMIN,J3DMIN)=255  
              ENDIF  
              DO J=J3DMAX,J3DMIN,-1  
                WRITE(52,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
              ENDDO  
              IAIJ(I3DMIN,J3DMIN)=0  
            ENDDO  
            CLOSE(52)  
          ENDIF  
          IF(IS3DDYE.GE.1)THEN  
            OPEN(53,FILE=DYEFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              READ(53,*)(IAKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(53,STATUS='DELETE')  
            OPEN(53,FILE=DYEFN,STATUS='UNKNOWN')  
            DO K=1,KPC  
              DO NW=1,NWGG  
                L=LWGG(NW)  
                IAIJ(IWGG(NW),JWGG(NW))=IAKL(K,L)  
              ENDDO  
              IF(K.EQ.1.AND.JS3DDYE.GT.0)THEN  
                IAIJ(I3DMIN,J3DMIN)=255  
              ENDIF  
              DO J=J3DMAX,J3DMIN,-1  
                WRITE(53,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
              ENDDO  
              IAIJ(I3DMIN,J3DMIN)=0  
            ENDDO  
            CLOSE(53)  
          ENDIF  
          IF(IS3DSED.GE.1)THEN  
            OPEN(54,FILE=SEDFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              READ(54,*)(IAKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(54,STATUS='DELETE')  
            OPEN(54,FILE=SEDFN,STATUS='UNKNOWN')  
            DO K=1,KPC  
              DO NW=1,NWGG  
                L=LWGG(NW)  
                IAIJ(IWGG(NW),JWGG(NW))=IAKL(K,L)  
              ENDDO  
              IF(K.EQ.1.AND.JS3DSED.GT.0)THEN  
                IAIJ(I3DMIN,J3DMIN)=255  
              ENDIF  
              DO J=J3DMAX,J3DMIN,-1  
                WRITE(54,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
              ENDDO  
              IAIJ(I3DMIN,J3DMIN)=0  
            ENDDO  
            CLOSE(54)  
          ENDIF  
          IF(IS3DUUU.GE.1)THEN  
            OPEN(55,FILE=UUUFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              READ(55,*)(IAKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(55,STATUS='DELETE')  
            OPEN(55,FILE=UUUFN,STATUS='UNKNOWN')  
            DO K=1,KPC  
              DO NW=1,NWGG  
                L=LWGG(NW)  
                IAIJ(IWGG(NW),JWGG(NW))=IAKL(K,L)  
              ENDDO  
              IF(K.EQ.1.AND.JS3DUUU.GT.0)THEN  
                IAIJ(I3DMIN,J3DMIN)=255  
              ENDIF  
              DO J=J3DMAX,J3DMIN,-1  
                WRITE(55,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
              ENDDO  
              IAIJ(I3DMIN,J3DMIN)=0  
            ENDDO  
            CLOSE(55)  
          ENDIF  
          IF(IS3DVVV.GE.1)THEN  
            OPEN(56,FILE=VVVFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              READ(56,*)(IAKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(56,STATUS='DELETE')  
            OPEN(56,FILE=VVVFN,STATUS='UNKNOWN')  
            DO K=1,KPC  
              DO NW=1,NWGG  
                L=LWGG(NW)  
                IAIJ(IWGG(NW),JWGG(NW))=IAKL(K,L)  
              ENDDO  
              IF(K.EQ.1.AND.JS3DVVV.GT.0)THEN  
                IAIJ(I3DMIN,J3DMIN)=255  
              ENDIF  
              DO J=J3DMAX,J3DMIN,-1  
                WRITE(56,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
              ENDDO  
              IAIJ(I3DMIN,J3DMIN)=0  
            ENDDO  
            CLOSE(56)  
          ENDIF  
          IF(IS3DWWW.GE.1)THEN  
            OPEN(57,FILE=WWWFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              READ(57,*)(IAKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(57,STATUS='DELETE')  
            OPEN(57,FILE=WWWFN,STATUS='UNKNOWN')  
            DO K=1,KPC  
              DO NW=1,NWGG  
                L=LWGG(NW)  
                IAIJ(IWGG(NW),JWGG(NW))=IAKL(K,L)  
              ENDDO  
              IF(K.EQ.1.AND.JS3DWWW.GT.0)THEN  
                IAIJ(I3DMIN,J3DMIN)=255  
              ENDIF  
              DO J=J3DMAX,J3DMIN,-1  
                WRITE(57,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
              ENDDO  
              IAIJ(I3DMIN,J3DMIN)=0  
            ENDDO  
            CLOSE(57)  
          ENDIF  
          IF(IS3DSND.GE.1)THEN  
            OPEN(58,FILE=SNDFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              READ(58,*)(IAKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(58,STATUS='DELETE')  
            OPEN(58,FILE=SNDFN,STATUS='UNKNOWN')  
            DO K=1,KPC  
              DO NW=1,NWGG  
                L=LWGG(NW)  
                IAIJ(IWGG(NW),JWGG(NW))=IAKL(K,L)  
              ENDDO  
              IF(K.EQ.1.AND.JS3DSND.GT.0)THEN  
                IAIJ(I3DMIN,J3DMIN)=255  
              ENDIF  
              DO J=J3DMAX,J3DMIN,-1  
                WRITE(58,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
              ENDDO  
              IAIJ(I3DMIN,J3DMIN)=0  
            ENDDO  
            CLOSE(58)  
          ENDIF  
          IF(IS3DTOX.GE.1)THEN  
            OPEN(59,FILE=TOXFN,STATUS='UNKNOWN')  
            DO L=2,LA  
              READ(59,*)(IAKL(K,L),K=1,KPC)  
            ENDDO  
            CLOSE(59,STATUS='DELETE')  
            OPEN(59,FILE=TOXFN,STATUS='UNKNOWN')  
            DO K=1,KPC  
              DO NW=1,NWGG  
                L=LWGG(NW)  
                IAIJ(IWGG(NW),JWGG(NW))=IAKL(K,L)  
              ENDDO  
              IF(K.EQ.1.AND.JS3DTOX.GT.0)THEN  
                IAIJ(I3DMIN,J3DMIN)=255  
              ENDIF  
              DO J=J3DMAX,J3DMIN,-1  
                WRITE(59,501)(IAIJ(I,J),I=I3DMAX,I3DMIN,-1)  
              ENDDO  
              IAIJ(I3DMIN,J3DMIN)=0  
            ENDDO  
            CLOSE(59)  
          ENDIF  
        ENDIF  
      ENDIF  
  500 FORMAT(5I5)  
  501 FORMAT(72I4)  
  502 FORMAT(I5,F10.4)  
  505 FORMAT(8F10.5)  
  506 FORMAT(I5,2X,F10.5,5X,I5)  
  510 FORMAT(2I5,4(2X,F10.5))  
  520 FORMAT('IAD = ',I5,'  JAD = ',I5//)  
  521 FORMAT('SALMAX = ',E12.4,'  SALMIN = ',E12.4/)  
  522 FORMAT('TEMMAX = ',E12.4,'  TEMMIN = ',E12.4/)  
  523 FORMAT('DYEMAX = ',E12.4,'  DYEMIN = ',E12.4/)  
  524 FORMAT('SEDMAX = ',E12.4,'  SEDMIN = ',E12.4/)  
  525 FORMAT('UUUMAX = ',E12.4,'  UUUMIN = ',E12.4/)  
  526 FORMAT('VVVMAX = ',E12.4,'  VVVMIN = ',E12.4/)  
  527 FORMAT('WWWMAX = ',E12.4,'  WWWMIN = ',E12.4/)  
  528 FORMAT('SNDMAX = ',E12.4,'  SNDMIN = ',E12.4/)  
  529 FORMAT('TOXMAX = ',E12.4,'  TOXMIN = ',E12.4/)  
  530 FORMAT('NCALL3D = ',I5/)  
  551 FORMAT(72F7.1)  
  559 FORMAT(2I4,2X,72I2)  
      CLOSE(50)  
      RETURN  
      END  

