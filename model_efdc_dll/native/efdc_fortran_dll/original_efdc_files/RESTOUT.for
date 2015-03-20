      SUBROUTINE RESTOUT(IRSTYP)  
C  
C CHANGE RECORD  
C  11/14/2001       JOHN HAMRIC        11/14/2001       JOHN HAMRIC  
C   ADD OUTPUT OF BED LOAD TRANSPORT QSBDLDX  QSBDLDY  
C **  SUBROUTINE RESTOUT WRITES A RESTART FILE  
C  
      USE GLOBAL  

      CHARACTER*64 RESTFN ! { GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.5.23
      REAL HPRES(LCM),H1PRES(LCM),HWQRES(LCM),H2WQRES(LCM) ! NEG. DEP.: JGCHO 2014.9.3

! { GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2011.5.23
      IF (IRSTYP.LE.-20) GOTO 7501
! } GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2011.5.23
! { GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.6.3
      IF (IRSTYP.EQ.-19) GOTO 7502
! } GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.6.3
	IF(IRSTYP.EQ.0)THEN  
        PRINT *,'Restart Snapshot @ Timeday: ',TIMEDAY
        OPEN(99,FILE='RESTART.OUT',STATUS='UNKNOWN')  
        CLOSE(99,STATUS='DELETE')  
        OPEN(99,FILE='RESTART.OUT',STATUS='UNKNOWN')  
      ENDIF  
      IF(IRSTYP.EQ.1)THEN  
        OPEN(99,FILE='CRASHST.OUT',STATUS='UNKNOWN')  
        CLOSE(99,STATUS='DELETE')  
        OPEN(99,FILE='CRASHST.OUT',STATUS='UNKNOWN')  
      ENDIF  
      IF(ISRESTO.EQ.-11)THEN  
        DO L=1,LC  
          HP(L)=-BELV(L)  
          H1P(L)=-BELV(L)  
          HWQ(L)=-BELV(L)  
          H2WQ(L)=-BELV(L)  
          UHDYE(L)=0.  
          UHDY1E(L)=0.  
          VHDXE(L)=0.  
          VHDX1E(L)=0.  
        ENDDO  
        DO K=0,KC  
          DO L=1,LC  
            QQ(L,K)=QQMIN  
            QQSQR(L,K)=SQRT(QQ(L,K))  ! *** DSLLC
            QQ1(L,K)=QQMIN  
            QQL(L,K)=QQLMIN  
            QQL1(L,K)=QQLMIN  
            DML(L,K)=DMLMIN  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO L=1,LC  
            U(L,K)=0.  
            U1(L,K)=0.  
            V(L,K)=0.  
            V1(L,K)=0.  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO L=1,LC  
            SAL(L,K)=MAX(SAL(L,K),0.)  
            SAL1(L,K)=MAX(SAL1(L,K),0.)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=DT*FLOAT(N)+TCON*TBEGIN  
        TIME=TIME/TCON  
      ELSE  
        TIME=TIMESEC/TCON  
      ENDIF  
      WRITE(99,909)N,TIME  
      DO L=2,LA  
!{ NEG. DEP.: JGCHO 2014.9.3
        HPRES(L)=HP(L)
        H1PRES(L)=H1P(L)
        HWQRES(L)=HWQ(L)
        H2WQRES(L)=H2WQ(L)
        if (HPRES(L).lt.HMIN) HPRES(L)=HMIN
        if (H1PRES(L).lt.HMIN) H1PRES(L)=HMIN
        if (HWQRES(L).lt.HMIN) HWQRES(L)=HMIN
        if (H2WQRES(L).lt.HMIN) H2WQRES(L)=HMIN
        WRITE(99,906)HPRES(L),H1PRES(L),HWQRES(L),H2WQRES(L),BELV(L)  
!} NEG. DEP.: JGCHO 2014.9.3
!        WRITE(99,906)HP(L),H1P(L),HWQ(L),H2WQ(L),BELV(L)   ! NEG. DEP.: JGCHO 2014.9.3
        WRITE(99,907)UHDYE(L),UHDY1E(L),VHDXE(L),VHDX1E(L)  
        WRITE(99,907)(U(L,K),K=1,KC)  
        WRITE(99,907)(U1(L,K),K=1,KC)  
        WRITE(99,907)(V(L,K),K=1,KC)  
        WRITE(99,907)(V1(L,K),K=1,KC)  
        WRITE(99,907)(QQ(L,K),K=0,KC)  
        WRITE(99,907)(QQ1(L,K),K=0,KC)  
        WRITE(99,907)(QQL(L,K),K=0,KC)  
        WRITE(99,907)(QQL1(L,K),K=0,KC)  
        WRITE(99,907)(DML(L,K),K=0,KC)  
        IF(ISCO(1).EQ.1)THEN  
          WRITE(99,907)(SAL(L,K),K=1,KC)  
          WRITE(99,907)(SAL1(L,K),K=1,KC)  
        ENDIF  
        IF(ISCO(2).EQ.1)THEN  
          WRITE(99,907)(TEM(L,K),K=1,KC)  
          WRITE(99,907)(TEM1(L,K),K=1,KC)  
        ENDIF  
        IF(ISCO(3).EQ.1)THEN  
          WRITE(99,907)(DYE(L,K),K=1,KC)  
          WRITE(99,907)(DYE1(L,K),K=1,KC)  
        ENDIF  
        IF(ISCO(4).EQ.1)THEN  
          WRITE(99,907)SFLSBOT(L),(SFL(L,K),K=1,KC)  
          WRITE(99,907)SFLSBOT(L),(SFL2(L,K),K=1,KC)  
        ENDIF  
        IF(ISCO(5).EQ.1)THEN  
          DO NT=1,NTOX  
            WRITE(99,907)(TOXB(L,K,NT),K=1,KB)  
            WRITE(99,907)(TOX(L,K,NT),K=1,KC)  
            WRITE(99,907)(TOXB1(L,K,NT),K=1,KB)  
            WRITE(99,907)(TOX1(L,K,NT),K=1,KC)  
          ENDDO  
        ENDIF  
        IF(ISCO(6).EQ.1)THEN  
          DO NS=1,NSED  
            WRITE(99,907)(SEDB(L,K,NS),K=1,KB)  
            WRITE(99,907)(SED1(L,K,NS),K=1,KC)  
            WRITE(99,907)(SEDB1(L,K,NS),K=1,KB)  
            WRITE(99,907)(SED1(L,K,NS),K=1,KC)  
          ENDDO  
        ENDIF  
        IF(ISCO(7).EQ.1)THEN  
          DO NS=1,NSND  
            WRITE(99,907)(SNDB(L,K,NS),K=1,KB)  
            WRITE(99,907)(SND(L,K,NS),K=1,KC)  
            WRITE(99,907)(SNDB1(L,K,NS),K=1,KB)  
            WRITE(99,907)(SND1(L,K,NS),K=1,KC)  
          ENDDO  
        ENDIF  
        IF(ISCO(6).EQ.1.OR.ISCO(7).EQ.1)THEN  
          WRITE(99,907)(HBED(L,K),K=1,KB)  
          WRITE(99,907)(HBED1(L,K),K=1,KB)  
          WRITE(99,907)(VDRBED(L,K),K=1,KB)  
          WRITE(99,907)(VDRBED1(L,K),K=1,KB)  
        ENDIF  
      ENDDO  

      ! *** BOUNDARY CONDITIONS
      DO M=1,4  
        IF(ISCO(M).EQ.1)THEN  
          DO LL=1,NCBS  
            DO K=1,KC  
              NLOS(LL,K,M)=NLOS(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOS(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOS(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBW  
            DO K=1,KC  
              NLOW(LL,K,M)=NLOW(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOW(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOW(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBE  
            DO K=1,KC  
              NLOE(LL,K,M)=NLOE(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOE(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOE(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBN  
            DO K=1,KC  
              NLON(LL,K,M)=NLON(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLON(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLON(LL,K,M),K=1,KC)  
          ENDDO  
        ENDIF  
      ENDDO  
      IF(ISCO(5).EQ.1)THEN  
        DO NT=1,NTOX  
          M=MSVTOX(NT)  
          DO LL=1,NCBS  
            DO K=1,KC  
              NLOS(LL,K,M)=NLOS(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOS(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOS(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBW  
            DO K=1,KC  
              NLOW(LL,K,M)=NLOW(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOW(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOW(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBE  
            DO K=1,KC  
              NLOE(LL,K,M)=NLOE(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOE(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOE(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBN  
            DO K=1,KC  
              NLON(LL,K,M)=NLON(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLON(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLON(LL,K,M),K=1,KC)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISCO(6).EQ.1)THEN  
        DO NT=1,NSED  
          M=MSVSED(NT)  
          DO LL=1,NCBS  
            DO K=1,KC  
              NLOS(LL,K,M)=NLOS(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOS(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOS(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBW  
            DO K=1,KC  
              NLOW(LL,K,M)=NLOW(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOW(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOW(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBE  
            DO K=1,KC  
              NLOE(LL,K,M)=NLOE(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOE(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOE(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBN  
            DO K=1,KC  
              NLON(LL,K,M)=NLON(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLON(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLON(LL,K,M),K=1,KC)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISCO(7).EQ.1)THEN  
        DO NT=1,NSND  
          M=MSVSND(NT)  
          DO LL=1,NCBS  
            DO K=1,KC  
              NLOS(LL,K,M)=NLOS(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOS(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOS(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBW  
            DO K=1,KC  
              NLOW(LL,K,M)=NLOW(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOW(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOW(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBE  
            DO K=1,KC  
              NLOE(LL,K,M)=NLOE(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLOE(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLOE(LL,K,M),K=1,KC)  
          ENDDO  
          DO LL=1,NCBN  
            DO K=1,KC  
              NLON(LL,K,M)=NLON(LL,K,M)-N  
            ENDDO  
            WRITE(99,908)(NLON(LL,K,M),K=1,KC)  
            WRITE(99,907)(CLON(LL,K,M),K=1,KC)  
          ENDDO  
        ENDDO  
      ENDIF  
      DO L=2,LA  
        WRITE(99,907)QSUME(L),(QSUM(L,K),K=1,KC)  
      ENDDO  
      IF(MDCHH.GE.1)THEN  
        DO NMD=1,MDCHH  
          WRITE(99,910)IMDCHH(NMD),JMDCHH(NMD),IMDCHU(NMD),JMDCHU(NMD),  
     &        IMDCHV(NMD),JMDCHV(NMD),QCHANU(NMD),QCHANV(NMD)  
        ENDDO  
      ENDIF  
      IF(ISGWIE.GE.1)THEN  
        DO L=2,LA  
          WRITE(99,907)AGWELV(L),AGWELV1(L)  
        ENDDO  
      ENDIF  
      CLOSE(99)  
C
C *** SPECIAL FILES
C
      IF(ISWAVE.GE.1)THEN  
        OPEN(1,FILE='WVQWCP.OUT',STATUS='UNKNOWN')  
        CLOSE(1, STATUS='DELETE')  
        OPEN(1,FILE='WVQWCP.OUT',STATUS='UNKNOWN')  
        DO L=2,LA  
          WRITE(1,911)IL(L),JL(L),QQWV1(L),QQWV2(L),QQWV3(L),QQWC(L),  
     &        QQWCR(L),QQ(L,0)  
        ENDDO  
        CLOSE(1)  
      ENDIF  
      IF(ISCO(1).GE.1.AND.ISTRAN(1).GT.0)THEN  
        OPEN(1,FILE='SALT.RST',STATUS='UNKNOWN')  
        CLOSE(1, STATUS='DELETE')  
        OPEN(1,FILE='SALT.RST',STATUS='UNKNOWN')  
        DO L=2,LA  
          WRITE(1,912)L,IL(L),JL(L),(SAL(L,K),K=1,KC)  
        ENDDO  
        CLOSE(1)  
      ENDIF  
      IF(ISCO(2).GE.1.AND.ISTRAN(2).GT.0)THEN  
        OPEN(1,FILE='TEMP.RSTO',STATUS='UNKNOWN')  
        CLOSE(1, STATUS='DELETE')  
        OPEN(1,FILE='TEMP.RSTO',STATUS='UNKNOWN')  
        DO L=2,LA  
          WRITE(1,912)L,IL(L),JL(L),(TEM(L,K),K=1,KC),TEMB(L)  
        ENDDO  
        CLOSE(1)  
      ENDIF  
      IF(ISDRY.EQ.99)THEN  
        OPEN(1,FILE='RSTWD.OUT',STATUS='UNKNOWN')  
        CLOSE(1, STATUS='DELETE')  
        OPEN(1,FILE='RSTWD.OUT',STATUS='UNKNOWN')  
        DO L=2,LA  
          WRITE(1,913)L,IL(L),JL(L),ISCDRY(L),NATDRY(L),  
     &        IMASKDRY(L),SUB(L),SVB(L),SUBO(L),SVBO(L)  
        ENDDO  
        CLOSE(1)  
      ENDIF  
C  
C **  OUTPUT SALINITY AND TEMPATURE DATA ASSIMILATION  
C  
      IF(NLCDA.GT.0)THEN
        OPEN(1,FILE='DATAASM.OUT')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='DATAASM.OUT')  
        DO J=1,NLCDA  
          DO I=1,NTC  
            WRITE(1,5678)J,I,FSALASM(I,J),FVOLASM(I,J),FTEMASM(I,J)  
          ENDDO  
        ENDDO  
      ENDIF
 5678 FORMAT(2I6,3E14.5)  
C
      IF(ISTRAN(6).GT.0.OR.ISTRAN(7).GT.0.AND.
     &            ISDTXBUG.EQ.1.AND.N.EQ.NTS)THEN  
        OPEN(1,FILE='BEDRST.SED')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDRST.SED')  
        WRITE(1,111)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(SEDB(L,K,1),K=1,KB)  
          IF(NSED.GT.1) THEN  
            DO NX=2,NSED  
              WRITE(1,102)(SEDB(L,K,NX),K=1,KB)  
            END DO  
          ENDIF  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDRST.SND')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDRST.SND')  
        WRITE(1,112)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(SNDB(L,K,1),K=1,KB)  
          IF(NSND.GT.1)THEN  
            DO NX=2,NSND  
              WRITE(1,102)(SNDB(L,K,NX),K=1,KB)  
            END DO  
          ENDIF  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDRST.VDR')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDRST.VDR')  
        WRITE(1,113)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(VDRBED(L,K),K=1,KB)  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDRST.POR')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDRST.POR')  
        WRITE(1,114)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(PORBED(L,K),K=1,KB)  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDRST.ZHB')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDRST.ZHB')  
        WRITE(1,115)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),ZELBEDA(L),HBEDA(L),(HBED(L,K),K=1,KB)  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDRST.BDN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDRST.BDN')  
        WRITE(1,116)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(BDENBED(L,K),K=1,KB)  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDRST.ELV')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDRST.ELV')  
        WRITE(1,117)  
        RVAL=0.  
        TMP1=0.  
        TMP2=0.  
        TMP3=0.  
        TMP4=0.  
        DO L=2,LA  
          RVAL=RVAL+1.  
          TMP1=TMP1+ZELBEDA(L)  
          TMP2=TMP2+HBEDA(L)  
          TMP3=TMP3+BELV(L)  
          TMP4=TMP4+HP(L)  
          SURF=HP(L)+BELV(L)  
          WRITE(1,101)IL(L),JL(L),ZELBEDA(L),HBEDA(L),BELV(L),HP(L),SURF  
        ENDDO  
        TMP1=TMP1/RVAL  
        TMP2=TMP2/RVAL  
        TMP3=TMP3/RVAL  
        TMP4=TMP4/RVAL  
        IDUM=0  
        JDUM=0  
        WRITE(1,101)IDUM,JDUM,TMP1,TMP2,TMP3,TMP4  
        CLOSE(1)  
        OPEN(1,FILE='WATRST.SED')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='WATRST.SED')  
        WRITE(1,118)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(SED(L,K,1),K=1,KC)  
          IF(NSED.GT.1) THEN  
            DO NX=2,NSED  
              WRITE(1,102)(SED(L,K,NX),K=1,KC)  
            END DO  
          ENDIF  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='WATRST.SND')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='WATRST.SND')  
        WRITE(1,119)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),(SND(L,K,1),K=1,KC)  
          IF(NSND.GT.1)THEN  
            DO NX=2,NSND  
              WRITE(1,102)(SND(L,K,NX),K=1,KC)  
            END DO  
          ENDIF  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDRST.BDL')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDRST.BDL')  
        WRITE(1,120)  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),QSBDLDX(L,1),QSBDLDY(L,1)  
          IF(NSND.GT.1)THEN  
            DO NX=2,NSND  
              WRITE(1,102)QSBDLDX(L,NX),QSBDLDY(L,NX)  
            END DO  
          ENDIF  
        ENDDO  
        CLOSE(1)  
        OPEN(1,FILE='BEDRST.TOX')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDRST.TOX')  
        DO NT=1,NTOX  
          WRITE(1,121)NT  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),(TOXB(L,K,NT),K=1,KB)  
          ENDDO  
        ENDDO  
        CLOSE(1)  
      ENDIF  
  339 FORMAT(2I5,6F14.5)  
  101 FORMAT(2I5,18E13.5)  
  102 FORMAT(10X,18E13.5)  
  111 FORMAT('   IL   JL    SEDBT(K=1,KB)')  
  112 FORMAT('   IL   JL    SNDBT(K=1,KB)')  
  113 FORMAT('   IL   JL    VRDBED(K=1,KB)')  
  114 FORMAT('   IL   JL    PORBED(K=1,KB)')  
  115 FORMAT('   IL   JL    ZBEDB        HBEDT        HBED(K=1,KB)')  
  116 FORMAT('   IL   JL    BDENBED(K=1,KB)')  
  117 FORMAT('   IL   JL    ZBEDB        HBEDT        BELV',  
     &    '        HWCOL        SELV')  
  118 FORMAT('   IL   JL    SEDT(K=1,KC)')  
  119 FORMAT('   IL   JL    SNDT(K=1,KC)')  
  120 FORMAT('   IL   JL    QSBDLDX      QSBDLDY')  
  121 FORMAT('   IL   JL    TOXB(K=1,KB,NT)  NT = ',I5)  
  906 FORMAT(5E17.8)  
  907 FORMAT(13E17.8)  
  908 FORMAT(12I10)  
  909 FORMAT(I20,4X,F12.4)  
  910 FORMAT(6I5,2X,E17.8,2X,E17.8)  
  911 FORMAT(2I5,2X,6E13.4)  
  912 FORMAT(3I5,12F7.3,5(15X,12F7.3))   ! *** DSLLC Single Line  
  913 FORMAT(6I10,4F7.3)                 ! *** DSLLC Single Line 

! { GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2010.11.10
 7501 CONTINUE !      IF (IRSTYP.EQ.-20) GOTO 7501
      IF (IRSTYP.LE.-20 .AND. ISRESTO.LE.-20) THEN
        IF(IRSTYP.EQ.-20)THEN
          OPEN(7510,FILE='EE_HYDRO.OUT',STATUS='UNKNOWN')
          CLOSE(7510,STATUS='DELETE')
          OPEN(7510,FILE='EE_HYDRO.OUT',STATUS='UNKNOWN',  
     &        ACCESS='SEQUENTIAL',FORM='UNFORMATTED')  
          WRITE(7510)IC,JC,KC,LA-1,NQCTL
          WRITE(7510)ISTRAN(1),ISTRAN(2),ISTRAN(3),ISTRAN(4)
          WRITE(7510)ISTRAN(5),ISTRAN(6),ISTRAN(7)
          WRITE(7510)NSED,NSND,KB,NTOX
!          WRITE(*,*)IC,JC,KC,LA-1,NQCTL
!          WRITE(*,*)ISTRAN(1),ISTRAN(2),ISTRAN(3),ISTRAN(4)
!          WRITE(*,*)ISTRAN(5),ISTRAN(6),ISTRAN(7)
!          WRITE(*,*)NSED,NSND,KB,NTOX
          CALL FLUSH(7510)
          CLOSE(7510,STATUS='KEEP')
        ENDIF

        OPEN(7510,FILE='EE_HYDRO.OUT',POSITION='APPEND',STATUS='OLD',  
     &      FORM='UNFORMATTED')
        WRITE (7510)N,TIMEDAY,DELT
!        WRITE (*,*)N,TIMEDAY,DELT
! TOTAL DEPTH
        DO L=2,LA  
          WRITE(7510)HP(L)  
        ENDDO 
! UV FLUX AND W2
        DO L=2,LA  
          WRITE(7510)(UHDY2(L,K),VHDX2(L,K),W2(L,K),K=1,KC)
        ENDDO  
! UV 2011.2.24
        DO L=2,LA  
          WRITE(7510)(U(L,K),V(L,K),K=1,KC)
        ENDDO  
! IMASKDRY 2011.2.24
!        DO L=2,LA  
!          WRITE(7510)IMASKDRY(L)
!        ENDDO  
! SALINITY
        IF(ISTRAN(1).EQ.1) THEN
          DO L=2,LA
            WRITE(7510)(SAL(L,K),K=1,KC)
          ENDDO
        ENDIF
! TEMPERATURE
        IF(ISTRAN(2).EQ.1) THEN
          DO L=2,LA
            WRITE(7510)(TEM(L,K),K=1,KC)
          ENDDO
        ENDIF
! SED
        IF(ISTRAN(6).EQ.1) THEN
          DO L=2,LA
            WRITE(7510)((SED(L,K,NS),K=1,KC),NS=1,NSED)
          ENDDO
        ENDIF
! QCTLT
        IF (NQCTL.GE.1) THEN
          DO NCTL=1,NQCTL
            WRITE(7510)(QCTLT(K,NCTL),K=1,KC)
          ENDDO
        ENDIF

        CALL FLUSH(7510)
        CLOSE(7510,STATUS='KEEP')
      ENDIF
! } GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2010.11.10

! { GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.5.23
 7502 CONTINUE ! IF (IRSTYP.EQ.-19) GOTO 7502
      IF (IRSTYP.EQ.-19) THEN
        WRITE(*,'(A,F10.6,2x,i3.3)')'Restart Snapshot @ Timeday: '
     &                              ,TIMEDAY,NINT(TIMEDAY)

        IF(ISDYNSTP.EQ.0)THEN  
          TIME=DT*FLOAT(N)+TCON*TBEGIN  
          TIME=TIME/TCON  
        ELSE  
          TIME=TIMESEC/TCON  
        ENDIF  

        WRITE(RESTFN,'(A,I3.3,A)') 'RESTART',NINT(TIMEDAY),'.OUT'
        OPEN(99,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  

        WRITE(99,909)N,TIME  
        DO L=2,LA  
!{ NEG. DEP.: JGCHO 2014.9.3
        HPRES(L)=HP(L)
        H1PRES(L)=H1P(L)
        HWQRES(L)=HWQ(L)
        H2WQRES(L)=H2WQ(L)
        if (HPRES(L).lt.HMIN) HPRES(L)=HMIN
        if (H1PRES(L).lt.HMIN) H1PRES(L)=HMIN
        if (HWQRES(L).lt.HMIN) HWQRES(L)=HMIN
        if (H2WQRES(L).lt.HMIN) H2WQRES(L)=HMIN
        WRITE(99,906)HPRES(L),H1PRES(L),HWQRES(L),H2WQRES(L),BELV(L)  
!} NEG. DEP.: JGCHO 2014.9.3
!        WRITE(99,906)HP(L),H1P(L),HWQ(L),H2WQ(L),BELV(L)   ! NEG. DEP.: JGCHO 2014.9.3
          WRITE(99,907)UHDYE(L),UHDY1E(L),VHDXE(L),VHDX1E(L)  
          WRITE(99,907)(U(L,K),K=1,KC)  
          WRITE(99,907)(U1(L,K),K=1,KC)  
          WRITE(99,907)(V(L,K),K=1,KC)  
          WRITE(99,907)(V1(L,K),K=1,KC)  
          WRITE(99,907)(QQ(L,K),K=0,KC)  
          WRITE(99,907)(QQ1(L,K),K=0,KC)  
          WRITE(99,907)(QQL(L,K),K=0,KC)  
          WRITE(99,907)(QQL1(L,K),K=0,KC)  
          WRITE(99,907)(DML(L,K),K=0,KC)  
          IF(ISCO(1).EQ.1)THEN  
            WRITE(99,907)(SAL(L,K),K=1,KC)  
            WRITE(99,907)(SAL1(L,K),K=1,KC)  
          ENDIF  
          IF(ISCO(2).EQ.1)THEN  
            WRITE(99,907)(TEM(L,K),K=1,KC)  
            WRITE(99,907)(TEM1(L,K),K=1,KC)  
          ENDIF  
          IF(ISCO(3).EQ.1)THEN  
            WRITE(99,907)(DYE(L,K),K=1,KC)  
            WRITE(99,907)(DYE1(L,K),K=1,KC)  
          ENDIF  
          IF(ISCO(4).EQ.1)THEN  
            WRITE(99,907)SFLSBOT(L),(SFL(L,K),K=1,KC)  
            WRITE(99,907)SFLSBOT(L),(SFL2(L,K),K=1,KC)  
          ENDIF  
          IF(ISCO(5).EQ.1)THEN  
            DO NT=1,NTOX  
              WRITE(99,907)(TOXB(L,K,NT),K=1,KB)  
              WRITE(99,907)(TOX(L,K,NT),K=1,KC)  
              WRITE(99,907)(TOXB1(L,K,NT),K=1,KB)  
              WRITE(99,907)(TOX1(L,K,NT),K=1,KC)  
            ENDDO  
          ENDIF  
          IF(ISCO(6).EQ.1)THEN  
            DO NS=1,NSED  
              WRITE(99,907)(SEDB(L,K,NS),K=1,KB)  
              WRITE(99,907)(SED1(L,K,NS),K=1,KC)  
              WRITE(99,907)(SEDB1(L,K,NS),K=1,KB)  
              WRITE(99,907)(SED1(L,K,NS),K=1,KC)  
            ENDDO  
          ENDIF  
          IF(ISCO(7).EQ.1)THEN  
            DO NS=1,NSND  
              WRITE(99,907)(SNDB(L,K,NS),K=1,KB)  
              WRITE(99,907)(SND(L,K,NS),K=1,KC)  
              WRITE(99,907)(SNDB1(L,K,NS),K=1,KB)  
              WRITE(99,907)(SND1(L,K,NS),K=1,KC)  
            ENDDO  
          ENDIF  
          IF(ISCO(6).EQ.1.OR.ISCO(7).EQ.1)THEN  
            WRITE(99,907)(HBED(L,K),K=1,KB)  
            WRITE(99,907)(HBED1(L,K),K=1,KB)  
            WRITE(99,907)(VDRBED(L,K),K=1,KB)  
            WRITE(99,907)(VDRBED1(L,K),K=1,KB)  
          ENDIF  
        ENDDO  
  
        ! *** BOUNDARY CONDITIONS
        DO M=1,4  
          IF(ISCO(M).EQ.1)THEN  
            DO LL=1,NCBS  
              DO K=1,KC  
                NLOS(LL,K,M)=NLOS(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOS(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOS(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBW  
              DO K=1,KC  
                NLOW(LL,K,M)=NLOW(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOW(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOW(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBE  
              DO K=1,KC  
                NLOE(LL,K,M)=NLOE(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOE(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOE(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBN  
              DO K=1,KC  
                NLON(LL,K,M)=NLON(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLON(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLON(LL,K,M),K=1,KC)  
            ENDDO  
          ENDIF  
        ENDDO  
        IF(ISCO(5).EQ.1)THEN  
          DO NT=1,NTOX  
            M=MSVTOX(NT)  
            DO LL=1,NCBS  
              DO K=1,KC  
                NLOS(LL,K,M)=NLOS(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOS(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOS(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBW  
              DO K=1,KC  
                NLOW(LL,K,M)=NLOW(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOW(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOW(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBE  
              DO K=1,KC  
                NLOE(LL,K,M)=NLOE(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOE(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOE(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBN  
              DO K=1,KC  
                NLON(LL,K,M)=NLON(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLON(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLON(LL,K,M),K=1,KC)  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISCO(6).EQ.1)THEN  
          DO NT=1,NSED  
            M=MSVSED(NT)  
            DO LL=1,NCBS  
              DO K=1,KC  
                NLOS(LL,K,M)=NLOS(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOS(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOS(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBW  
              DO K=1,KC  
                NLOW(LL,K,M)=NLOW(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOW(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOW(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBE  
              DO K=1,KC  
                NLOE(LL,K,M)=NLOE(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOE(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOE(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBN  
              DO K=1,KC  
                NLON(LL,K,M)=NLON(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLON(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLON(LL,K,M),K=1,KC)  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISCO(7).EQ.1)THEN  
          DO NT=1,NSND  
            M=MSVSND(NT)  
            DO LL=1,NCBS  
              DO K=1,KC  
                NLOS(LL,K,M)=NLOS(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOS(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOS(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBW  
              DO K=1,KC  
                NLOW(LL,K,M)=NLOW(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOW(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOW(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBE  
              DO K=1,KC  
                NLOE(LL,K,M)=NLOE(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLOE(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLOE(LL,K,M),K=1,KC)  
            ENDDO  
            DO LL=1,NCBN  
              DO K=1,KC  
                NLON(LL,K,M)=NLON(LL,K,M)-N  
              ENDDO  
              WRITE(99,908)(NLON(LL,K,M),K=1,KC)  
              WRITE(99,907)(CLON(LL,K,M),K=1,KC)  
            ENDDO  
          ENDDO  
        ENDIF  
        DO L=2,LA  
          WRITE(99,907)QSUME(L),(QSUM(L,K),K=1,KC)  
        ENDDO  
        IF(MDCHH.GE.1)THEN  
          DO NMD=1,MDCHH  
            WRITE(99,910)IMDCHH(NMD),JMDCHH(NMD),IMDCHU(NMD),JMDCHU(NMD)  
     &        ,IMDCHV(NMD),JMDCHV(NMD),QCHANU(NMD),QCHANV(NMD)  
          ENDDO  
        ENDIF  
        IF(ISGWIE.GE.1)THEN  
          DO L=2,LA  
            WRITE(99,907)AGWELV(L),AGWELV1(L)  
          ENDDO  
        ENDIF  
        CLOSE(99)  
C
C *** SPECIAL FILES
C
        IF(ISWAVE.GE.1)THEN  
          WRITE(RESTFN,'(A,I3.3,A)') 'WVQWCP',NINT(TIMEDAY),'.OUT'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          DO L=2,LA  
            WRITE(1,911)IL(L),JL(L),QQWV1(L),QQWV2(L),QQWV3(L),QQWC(L),  
     &        QQWCR(L),QQ(L,0)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
        IF(ISCO(1).GE.1.AND.ISTRAN(1).GT.0)THEN  
          WRITE(RESTFN,'(A,I3.3,A)') 'SALT',NINT(TIMEDAY),'.RST'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          DO L=2,LA  
            WRITE(1,912)L,IL(L),JL(L),(SAL(L,K),K=1,KC)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
        IF(ISCO(2).GE.1.AND.ISTRAN(2).GT.0)THEN  
          WRITE(RESTFN,'(A,I3.3,A)') 'TEMP',NINT(TIMEDAY),'.RST'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          DO L=2,LA  
            WRITE(1,912)L,IL(L),JL(L),(TEM(L,K),K=1,KC),TEMB(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
        IF(ISDRY.EQ.99)THEN  
          WRITE(RESTFN,'(A,I3.3,A)') 'RSTWD',NINT(TIMEDAY),'.OUT'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          DO L=2,LA  
            WRITE(1,913)L,IL(L),JL(L),ISCDRY(L),NATDRY(L),  
     &        IMASKDRY(L),SUB(L),SVB(L),SUBO(L),SVBO(L)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
C  
C **  OUTPUT SALINITY AND TEMPATURE DATA ASSIMILATION  
C  
        IF(NLCDA.GT.0)THEN
          WRITE(RESTFN,'(A,I3.3,A)') 'DATAASM',NINT(TIMEDAY),'.OUT'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          DO J=1,NLCDA  
            DO I=1,NTC  
              WRITE(1,5678)J,I,FSALASM(I,J),FVOLASM(I,J),FTEMASM(I,J)  
            ENDDO  
          ENDDO  
        ENDIF

C
        IF(ISTRAN(6).GT.0 .OR. ISTRAN(7).GT.0 .AND. 
     &            ISDTXBUG.EQ.1.AND.N.EQ.NTS)THEN  

          WRITE(RESTFN,'(A,I3.3,A)') 'BEDRST',NINT(TIMEDAY),'.SED'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          WRITE(1,111)  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),(SEDB(L,K,1),K=1,KB)  
            IF(NSED.GT.1) THEN  
              DO NX=2,NSED  
                WRITE(1,102)(SEDB(L,K,NX),K=1,KB)  
              END DO  
            ENDIF  
          ENDDO  
          CLOSE(1)  
          WRITE(RESTFN,'(A,I3.3,A)') 'BEDRST',NINT(TIMEDAY),'.SND'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          WRITE(1,112)  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),(SNDB(L,K,1),K=1,KB)  
            IF(NSND.GT.1)THEN  
              DO NX=2,NSND  
                WRITE(1,102)(SNDB(L,K,NX),K=1,KB)  
              END DO  
            ENDIF  
          ENDDO  
          CLOSE(1)  
          WRITE(RESTFN,'(A,I3.3,A)') 'BEDRST',NINT(TIMEDAY),'.VDR'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          WRITE(1,113)  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),(VDRBED(L,K),K=1,KB)  
          ENDDO  
          CLOSE(1)  
          WRITE(RESTFN,'(A,I3.3,A)') 'BEDRST',NINT(TIMEDAY),'.POR'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          WRITE(1,114)  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),(PORBED(L,K),K=1,KB)  
          ENDDO  
          CLOSE(1)  
          WRITE(RESTFN,'(A,I3.3,A)') 'BEDRST',NINT(TIMEDAY),'.ZHB'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          WRITE(1,115)  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),ZELBEDA(L),HBEDA(L)
     &                 ,(HBED(L,K),K=1,KB)  
          ENDDO  
          CLOSE(1)  
          WRITE(RESTFN,'(A,I3.3,A)') 'BEDRST',NINT(TIMEDAY),'.BDN'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          WRITE(1,116)  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),(BDENBED(L,K),K=1,KB)  
          ENDDO  
          CLOSE(1)  
          WRITE(RESTFN,'(A,I3.3,A)') 'BEDRST',NINT(TIMEDAY),'.ELV'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          WRITE(1,117)  
          RVAL=0.  
          TMP1=0.  
          TMP2=0.  
          TMP3=0.  
          TMP4=0.  
          DO L=2,LA  
            RVAL=RVAL+1.  
            TMP1=TMP1+ZELBEDA(L)  
            TMP2=TMP2+HBEDA(L)  
            TMP3=TMP3+BELV(L)  
            TMP4=TMP4+HP(L)  
            SURF=HP(L)+BELV(L)  
            WRITE(1,101)IL(L),JL(L),ZELBEDA(L),HBEDA(L),BELV(L)
     &                 ,HP(L),SURF  
          ENDDO  
          TMP1=TMP1/RVAL  
          TMP2=TMP2/RVAL  
          TMP3=TMP3/RVAL  
          TMP4=TMP4/RVAL  
          IDUM=0  
          JDUM=0  
          WRITE(1,101)IDUM,JDUM,TMP1,TMP2,TMP3,TMP4  
          CLOSE(1)  
          WRITE(RESTFN,'(A,I3.3,A)') 'WATRST',NINT(TIMEDAY),'.SED'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          WRITE(1,118)  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),(SED(L,K,1),K=1,KC)  
            IF(NSED.GT.1) THEN  
              DO NX=2,NSED  
                WRITE(1,102)(SED(L,K,NX),K=1,KC)  
              END DO  
            ENDIF  
          ENDDO  
          CLOSE(1)  
          WRITE(RESTFN,'(A,I3.3,A)') 'WATRST',NINT(TIMEDAY),'.SND'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          WRITE(1,119)  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),(SND(L,K,1),K=1,KC)  
            IF(NSND.GT.1)THEN  
              DO NX=2,NSND  
                WRITE(1,102)(SND(L,K,NX),K=1,KC)  
              END DO  
            ENDIF  
          ENDDO  
          CLOSE(1)  
          WRITE(RESTFN,'(A,I3.3,A)') 'BEDRST',NINT(TIMEDAY),'.BDL'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          WRITE(1,120)  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),QSBDLDX(L,1),QSBDLDY(L,1)  
            IF(NSND.GT.1)THEN  
              DO NX=2,NSND  
                WRITE(1,102)QSBDLDX(L,NX),QSBDLDY(L,NX)  
              END DO  
            ENDIF  
          ENDDO  
          CLOSE(1)  
          WRITE(RESTFN,'(A,I3.3,A)') 'BEDRST',NINT(TIMEDAY),'.TOX'
          OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
          DO NT=1,NTOX  
            WRITE(1,121)NT  
            DO L=2,LA  
              WRITE(1,101)IL(L),JL(L),(TOXB(L,K,NT),K=1,KB)  
            ENDDO  
          ENDDO  
          CLOSE(1)  
        ENDIF  

      ENDIF ! IF (IRSTYP.EQ.-19) THEN
! } GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.5.23



      RETURN  
      END  
