      SUBROUTINE SALPLTV(ITMP)  
C  
C CHANGE RECORD  
C **  SUBROUTINE SALPLTV WRITES A FILE FOR VERTICAL PLANE CONTOURING  
C **  OF SALINITY, DYE CONCENTRATION, AND SEDIMENT CONCENTRATION  
C **  ALONG AN ARBITARY SEQUENCE OF (I,J) POINTS  
C  
C *** PMC  THIS ROUTINE USES HMP, THE STATIC IC DEPTH.  SHOULDN'T IT USE HP?

      USE GLOBAL  
      CHARACTER*80 TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6,TITLE7  
      IF(JSSPV(ITMP).NE.1) GOTO 300  
C  
C **  WRITE HEADINGS  
C  
      TITLE1='INSTANTANEOUS SALINITY CONTOURS'  
      TITLE2='INSTANTANEOUS TEMPERATURE CONTOURS'  
      TITLE3='INSTANTANEOUS DYE CONC CONTOURS'  
      TITLE4='INSTANT COHESIVE SED CONC CONTOURS'  
      TITLE5='INSTANT NONCOH SED CONC CONTOURS'  
      TITLE6='INSTANT TOXIC CONTAM CONC CONTOURS'  
      TITLE7='INSTANT SHELLFISH LARVAE CONTOURS'  
      IF(ITMP.EQ.1)THEN  
        IF(ISTRAN(1).GE.1)THEN  
          IF(ISECSPV.GE.1)THEN  
            OPEN(11,FILE='SALCNV1.OUT')  
            CLOSE(11,STATUS='DELETE')  
            OPEN(11,FILE='SALCNV1.OUT')  
          ENDIF  
          IF(ISECSPV.GE.2)THEN  
            OPEN(12,FILE='SALCNV2.OUT')  
            CLOSE(12,STATUS='DELETE')  
            OPEN(12,FILE='SALCNV2.OUT')  
          ENDIF  
          IF(ISECSPV.GE.3)THEN  
            OPEN(13,FILE='SALCNV3.OUT')  
            CLOSE(13,STATUS='DELETE')  
            OPEN(13,FILE='SALCNV3.OUT')  
          ENDIF  
          IF(ISECSPV.GE.4)THEN  
            OPEN(14,FILE='SALCNV4.OUT')  
            CLOSE(14,STATUS='DELETE')  
            OPEN(14,FILE='SALCNV4.OUT')  
          ENDIF  
          IF(ISECSPV.GE.5)THEN  
            OPEN(15,FILE='SALCNV5.OUT')  
            CLOSE(15,STATUS='DELETE')  
            OPEN(15,FILE='SALCNV5.OUT')  
          ENDIF  
          IF(ISECSPV.GE.6)THEN  
            OPEN(16,FILE='SALCNV6.OUT')  
            CLOSE(16,STATUS='DELETE')  
            OPEN(16,FILE='SALCNV6.OUT')  
          ENDIF  
          IF(ISECSPV.GE.7)THEN  
            OPEN(17,FILE='SALCNV7.OUT')  
            CLOSE(17,STATUS='DELETE')  
            OPEN(17,FILE='SALCNV7.OUT')  
          ENDIF  
          IF(ISECSPV.GE.8)THEN  
            OPEN(18,FILE='SALCNV8.OUT')  
            CLOSE(18,STATUS='DELETE')  
            OPEN(18,FILE='SALCNV8.OUT')  
          ENDIF  
          IF(ISECSPV.GE.9)THEN  
            OPEN(19,FILE='SALCNV9.OUT')  
            CLOSE(19,STATUS='DELETE')  
            OPEN(19,FILE='SALCNV9.OUT')  
          ENDIF  
          DO IS=1,ISECSPV  
            LUN=10+IS  
            LINES=NIJSPV(IS)  
            LEVELS=KC  
            WRITE (LUN,99) TITLE1,CCTITLE(LUN)  
            WRITE (LUN,101)LINES,LEVELS  
            WRITE (LUN,250)(ZZ(K),K=1,KC)  
            CLOSE(LUN)  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(ITMP.EQ.2)THEN  
        IF(ISTRAN(2).GE.1)THEN  
          IF(ISECSPV.GE.1)THEN  
            OPEN(21,FILE='TEMCNV1.OUT')  
            CLOSE(21 ,STATUS='DELETE')  
            OPEN(21,FILE='TEMCNV1.OUT')  
          ENDIF  
          IF(ISECSPV.GE.2)THEN  
            OPEN(22,FILE='TEMCNV2.OUT')  
            CLOSE(22,STATUS='DELETE')  
            OPEN(22,FILE='TEMCNV2.OUT')  
          ENDIF  
          IF(ISECSPV.GE.3)THEN  
            OPEN(23,FILE='TEMCNV3.OUT')  
            CLOSE(23,STATUS='DELETE')  
            OPEN(23,FILE='TEMCNV3.OUT')  
          ENDIF  
          IF(ISECSPV.GE.4)THEN  
            OPEN(24,FILE='TEMCNV4.OUT')  
            CLOSE(24,STATUS='DELETE')  
            OPEN(24,FILE='TEMCNV4.OUT')  
          ENDIF  
          IF(ISECSPV.GE.5)THEN  
            OPEN(25,FILE='TEMCNV5.OUT')  
            CLOSE(25,STATUS='DELETE')  
            OPEN(25,FILE='TEMCNV5.OUT')  
          ENDIF  
          IF(ISECSPV.GE.6)THEN  
            OPEN(26,FILE='TEMCNV6.OUT')  
            CLOSE(26,STATUS='DELETE')  
            OPEN(25,FILE='TEMCNV5.OUT')  
          ENDIF  
          IF(ISECSPV.GE.7)THEN  
            OPEN(27,FILE='TEMCNV7.OUT')  
            CLOSE(27,STATUS='DELETE')  
            OPEN(27,FILE='TEMCNV7.OUT')  
          ENDIF  
          IF(ISECSPV.GE.8)THEN  
            OPEN(28,FILE='TEMCNV8.OUT')  
            CLOSE(28,STATUS='DELETE')  
            OPEN(28,FILE='TEMCNV8.OUT')  
          ENDIF  
          IF(ISECSPV.GE.9)THEN  
            OPEN(29,FILE='TEMCNV9.OUT')  
            CLOSE(29,STATUS='DELETE')  
            OPEN(29,FILE='TEMCNV9.OUT')  
          ENDIF  
          DO IS=1,ISECSPV  
            LUN=20+IS  
            LINES=NIJSPV(IS)  
            LEVELS=KC  
            WRITE (LUN,99) TITLE2,CCTITLE(LUN)  
            WRITE (LUN,101)LINES,LEVELS  
            WRITE (LUN,250)(ZZ(K),K=1,KC)  
            CLOSE(LUN)  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(ITMP.EQ.3)THEN  
        IF(ISTRAN(3).GE.1)THEN  
          IF(ISECSPV.GE.1)THEN  
            OPEN(31,FILE='DYECNV1.OUT')  
            CLOSE(31 ,STATUS='DELETE')  
            OPEN(31,FILE='DYECNV1.OUT')  
          ENDIF  
          IF(ISECSPV.GE.2)THEN  
            OPEN(32,FILE='DYECNV2.OUT')  
            CLOSE(32,STATUS='DELETE')  
            OPEN(32,FILE='DYECNV2.OUT')  
          ENDIF  
          IF(ISECSPV.GE.3)THEN  
            OPEN(33,FILE='DYECNV3.OUT')  
            CLOSE(33,STATUS='DELETE')  
            OPEN(33,FILE='DYECNV3.OUT')  
          ENDIF  
          IF(ISECSPV.GE.4)THEN  
            OPEN(34,FILE='DYECNV4.OUT')  
            CLOSE(34,STATUS='DELETE')  
            OPEN(34,FILE='DYECNV4.OUT')  
          ENDIF  
          IF(ISECSPV.GE.5)THEN  
            OPEN(35,FILE='DYECNV5.OUT')  
            CLOSE(35,STATUS='DELETE')  
            OPEN(35,FILE='DYECNV5.OUT')  
          ENDIF  
          IF(ISECSPV.GE.6)THEN  
            OPEN(36,FILE='DYECNV6.OUT')  
            CLOSE(36,STATUS='DELETE')  
            OPEN(36,FILE='DYECNV6.OUT')  
          ENDIF  
          IF(ISECSPV.GE.7)THEN  
            OPEN(37,FILE='DYECNV7.OUT')  
            CLOSE(37,STATUS='DELETE')  
            OPEN(37,FILE='DYECNV7.OUT')  
          ENDIF  
          IF(ISECSPV.GE.8)THEN  
            OPEN(38,FILE='DYECNV8.OUT')  
            CLOSE(38,STATUS='DELETE')  
            OPEN(38,FILE='DYECNV8.OUT')  
          ENDIF  
          IF(ISECSPV.GE.9)THEN  
            OPEN(39,FILE='DYECNV9.OUT')  
            CLOSE(39,STATUS='DELETE')  
            OPEN(39,FILE='DYECNV9.OUT')  
          ENDIF  
          DO IS=1,ISECSPV  
            LUN=30+IS  
            LINES=NIJSPV(IS)  
            LEVELS=KC  
            WRITE (LUN,99) TITLE3,CCTITLE(LUN)  
            WRITE (LUN,101)LINES,LEVELS  
            WRITE (LUN,250)(ZZ(K),K=1,KC)  
            CLOSE(LUN)  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(ITMP.EQ.6)THEN  
        IF(ISTRAN(6).GE.1)THEN  
          IF(ISECSPV.GE.1)THEN  
            OPEN(41,FILE='SEDCNV1.OUT')  
            CLOSE(41,STATUS='DELETE')  
            OPEN(41,FILE='SEDCNV1.OUT')  
          ENDIF  
          IF(ISECSPV.GE.2)THEN  
            OPEN(42,FILE='SEDCNV2.OUT')  
            CLOSE(42,STATUS='DELETE')  
            OPEN(42,FILE='SEDCNV2.OUT')  
          ENDIF  
          IF(ISECSPV.GE.3)THEN  
            OPEN(43,FILE='SEDCNV3.OUT')  
            CLOSE(43,STATUS='DELETE')  
            OPEN(43,FILE='SEDCNV3.OUT')  
          ENDIF  
          IF(ISECSPV.GE.4)THEN  
            OPEN(44,FILE='SEDCNV4.OUT')  
            CLOSE(44,STATUS='DELETE')  
            OPEN(44,FILE='SEDCNV4.OUT')  
          ENDIF  
          IF(ISECSPV.GE.5)THEN  
            OPEN(45,FILE='SEDCNV5.OUT')  
            CLOSE(45,STATUS='DELETE')  
            OPEN(45,FILE='SEDCNV5.OUT')  
          ENDIF  
          IF(ISECSPV.GE.6)THEN  
            OPEN(46,FILE='SEDCNV6.OUT')  
            CLOSE(46,STATUS='DELETE')  
            OPEN(46,FILE='SEDCNV6.OUT')  
          ENDIF  
          IF(ISECSPV.GE.7)THEN  
            OPEN(47,FILE='SEDCNV7.OUT')  
            CLOSE(47,STATUS='DELETE')  
            OPEN(47,FILE='SEDCNV7.OUT')  
          ENDIF  
          IF(ISECSPV.GE.8)THEN  
            OPEN(48,FILE='SEDCNV8.OUT')  
            CLOSE(48,STATUS='DELETE')  
            OPEN(48,FILE='SEDCNV8.OUT')  
          ENDIF  
          IF(ISECSPV.GE.9)THEN  
            OPEN(49,FILE='SEDCNV9.OUT')  
            CLOSE(49,STATUS='DELETE')  
            OPEN(49,FILE='SEDCNV9.OUT')  
          ENDIF  
          DO IS=1,ISECSPV  
            LUN=40+IS  
            LINES=NIJSPV(IS)  
            LEVELS=KC  
            WRITE (LUN,99) TITLE4,CCTITLE(LUN)  
            WRITE (LUN,101)LINES,LEVELS  
            WRITE (LUN,250)(ZZ(K),K=1,KC)  
            CLOSE(LUN)  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(ITMP.EQ.7)THEN  
        IF(ISTRAN(7).GE.1)THEN  
          IF(ISECSPV.GE.1)THEN  
            OPEN(51,FILE='SNDCNV1.OUT')  
            CLOSE(51,STATUS='DELETE')  
            OPEN(51,FILE='SNDCNV1.OUT')  
          ENDIF  
          IF(ISECSPV.GE.2)THEN  
            OPEN(52,FILE='SNDCNV2.OUT')  
            CLOSE(52,STATUS='DELETE')  
            OPEN(52,FILE='SNDCNV2.OUT')  
          ENDIF  
          IF(ISECSPV.GE.3)THEN  
            OPEN(53,FILE='SNDCNV3.OUT')  
            CLOSE(53,STATUS='DELETE')  
            OPEN(53,FILE='SNDCNV3.OUT')  
          ENDIF  
          IF(ISECSPV.GE.4)THEN  
            OPEN(54,FILE='SNDCNV4.OUT')  
            CLOSE(54,STATUS='DELETE')  
            OPEN(54,FILE='SNDCNV4.OUT')  
          ENDIF  
          IF(ISECSPV.GE.5)THEN  
            OPEN(55,FILE='SNDCNV5.OUT')  
            CLOSE(55,STATUS='DELETE')  
            OPEN(55,FILE='SNDCNV5.OUT')  
          ENDIF  
          IF(ISECSPV.GE.6)THEN  
            OPEN(56,FILE='SNDCNV6.OUT')  
            CLOSE(56,STATUS='DELETE')  
            OPEN(56,FILE='SNDCNV6.OUT')  
          ENDIF  
          IF(ISECSPV.GE.7)THEN  
            OPEN(57,FILE='SNDCNV7.OUT')  
            CLOSE(57,STATUS='DELETE')  
            OPEN(57,FILE='SNDCNV7.OUT')  
          ENDIF  
          IF(ISECSPV.GE.8)THEN  
            OPEN(58,FILE='SNDCNV8.OUT')  
            CLOSE(58,STATUS='DELETE')  
            OPEN(58,FILE='SNDCNV8.OUT')  
          ENDIF  
          IF(ISECSPV.GE.9)THEN  
            OPEN(59,FILE='SNDCNV9.OUT')  
            CLOSE(59,STATUS='DELETE')  
            OPEN(59,FILE='SNDCNV9.OUT')  
          ENDIF  
          DO IS=1,ISECSPV  
            LUN=50+IS  
            LINES=NIJSPV(IS)  
            LEVELS=KC  
            WRITE (LUN,99) TITLE5,CCTITLE(LUN)  
            WRITE (LUN,101)LINES,LEVELS  
            WRITE (LUN,250)(ZZ(K),K=1,KC)  
            CLOSE(LUN)  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(ITMP.EQ.5)THEN  
        IF(ISTRAN(5).GE.1)THEN  
          IF(ISECSPV.GE.1)THEN  
            OPEN(61,FILE='TOXCNV1.OUT')  
            CLOSE(61,STATUS='DELETE')  
            OPEN(61,FILE='TOXCNV1.OUT')  
          ENDIF  
          IF(ISECSPV.GE.2)THEN  
            OPEN(62,FILE='TOXCNV2.OUT')  
            CLOSE(62,STATUS='DELETE')  
            OPEN(62,FILE='TOXCNV2.OUT')  
          ENDIF  
          IF(ISECSPV.GE.3)THEN  
            OPEN(63,FILE='TOXCNV3.OUT')  
            CLOSE(63,STATUS='DELETE')  
            OPEN(63,FILE='TOXCNV3.OUT')  
          ENDIF  
          IF(ISECSPV.GE.4)THEN  
            OPEN(64,FILE='TOXCNV4.OUT')  
            CLOSE(64,STATUS='DELETE')  
            OPEN(64,FILE='TOXCNV4.OUT')  
          ENDIF  
          IF(ISECSPV.GE.5)THEN  
            OPEN(65,FILE='TOXCNV5.OUT')  
            CLOSE(65,STATUS='DELETE')  
            OPEN(65,FILE='TOXCNV5.OUT')  
          ENDIF  
          IF(ISECSPV.GE.6)THEN  
            OPEN(66,FILE='TOXCNV6.OUT')  
            CLOSE(66,STATUS='DELETE')  
            OPEN(66,FILE='TOXCNV6.OUT')  
          ENDIF  
          IF(ISECSPV.GE.7)THEN  
            OPEN(67,FILE='TOXCNV7.OUT')  
            CLOSE(67,STATUS='DELETE')  
            OPEN(67,FILE='TOXCNV7.OUT')  
          ENDIF  
          IF(ISECSPV.GE.8)THEN  
            OPEN(68,FILE='TOXCNV8.OUT')  
            CLOSE(68,STATUS='DELETE')  
            OPEN(68,FILE='TOXCNV8.OUT')  
          ENDIF  
          IF(ISECSPV.GE.9)THEN  
            OPEN(69,FILE='TOXCNV9.OUT')  
            CLOSE(69,STATUS='DELETE')  
            OPEN(69,FILE='TOXCNV9.OUT')  
          ENDIF  
          DO IS=1,ISECSPV  
            LUN=60+IS  
            LINES=NIJSPV(IS)  
            LEVELS=KC  
            WRITE (LUN,99) TITLE6,CCTITLE(LUN)  
            WRITE (LUN,101)LINES,LEVELS  
            WRITE (LUN,250)(ZZ(K),K=1,KC)  
            CLOSE(LUN)  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(ITMP.EQ.4)THEN  
        IF(ISTRAN(4).GE.1)THEN  
          IF(ISECSPV.GE.1)THEN  
            OPEN(71,FILE='SFLCNV1.OUT')  
            CLOSE(71,STATUS='DELETE')  
            OPEN(71,FILE='SFLCNV1.OUT')  
          ENDIF  
          IF(ISECSPV.GE.2)THEN  
            OPEN(72,FILE='SFLCNV2.OUT')  
            CLOSE(72,STATUS='DELETE')  
            OPEN(72,FILE='SFLCNV2.OUT')  
          ENDIF  
          IF(ISECSPV.GE.3)THEN  
            OPEN(73,FILE='SFLCNV3.OUT')  
            CLOSE(73,STATUS='DELETE')  
            OPEN(73,FILE='SFLCNV3.OUT')  
          ENDIF  
          IF(ISECSPV.GE.4)THEN  
            OPEN(74,FILE='SFLCNV4.OUT')  
            CLOSE(74,STATUS='DELETE')  
            OPEN(74,FILE='SFLCNV4.OUT')  
          ENDIF  
          IF(ISECSPV.GE.5)THEN  
            OPEN(75,FILE='SFLCNV5.OUT')  
            CLOSE(75,STATUS='DELETE')  
            OPEN(75,FILE='SFLCNV5.OUT')  
          ENDIF  
          IF(ISECSPV.GE.6)THEN  
            OPEN(76,FILE='SFLCNV6.OUT')  
            CLOSE(76,STATUS='DELETE')  
            OPEN(76,FILE='SFLCNV6.OUT')  
          ENDIF  
          IF(ISECSPV.GE.7)THEN  
            OPEN(77,FILE='SFLCNV7.OUT')  
            CLOSE(77,STATUS='DELETE')  
            OPEN(77,FILE='SFLCNV7.OUT')  
          ENDIF  
          IF(ISECSPV.GE.8)THEN  
            OPEN(78,FILE='SFLCNV8.OUT')  
            CLOSE(78,STATUS='DELETE')  
            OPEN(78,FILE='SFLCNV8.OUT')  
          ENDIF  
          IF(ISECSPV.GE.9)THEN  
            OPEN(79,FILE='SFLCNV9.OUT')  
            CLOSE(79,STATUS='DELETE')  
            OPEN(79,FILE='SFLCNV9.OUT')  
          ENDIF  
          DO IS=1,ISECSPV  
            LUN=70+IS
            LINES=NIJSPV(IS)  
            LEVELS=KC  
            WRITE (LUN,99) TITLE7,CCTITLE(LUN)  
            WRITE (LUN,101)LINES,LEVELS  
            WRITE (LUN,250)(ZZ(K),K=1,KC)  
            CLOSE(LUN)  
          ENDDO  
        ENDIF  
      ENDIF  
      JSSPV(ITMP)=0  
  300 CONTINUE  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=DT*FLOAT(N)+TCON*TBEGIN  
        TIME=TIME/TCON  
      ELSE  
        TIME=TIMESEC/TCON  
      ENDIF  
      IF(ITMP.EQ.1.AND.ISTRAN(1).GE.1)THEN  
        IF(ISECSPV.GE.1)  
     &      OPEN(11,FILE='SALCNV1.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.2)  
     &      OPEN(12,FILE='SALCNV2.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.3)  
     &      OPEN(13,FILE='SALCNV3.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.4)  
     &      OPEN(14,FILE='SALCNV4.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.5)  
     &      OPEN(15,FILE='SALCNV5.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.6)  
     &      OPEN(16,FILE='SALCNV6.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.7)  
     &      OPEN(17,FILE='SALCNV7.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.8)  
     &      OPEN(18,FILE='SALCNV8.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.9)  
     &      OPEN(19,FILE='SALCNV9.OUT',POSITION='APPEND')  
        DO IS=1,ISECSPV  
          LUN=10+IS  
          WRITE (LUN,100)N,TIME  
          DO NN=1,NIJSPV(IS)  
            I=ISPV(NN,IS)  
            J=JSPV(NN,IS)  
            L=LIJ(I,J)  
            ZETA=P(L)*GI  
            HBTMP=BELV(L)  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),ZETA,HBTMP  
            WRITE(LUN,250)(SAL(L,K),K=1,KC)  
          ENDDO  
          CLOSE(LUN)  
        ENDDO  
      ENDIF  
      IF(ITMP.EQ.2.AND.ISTRAN(2).GE.1)THEN  
        IF(ISECSPV.GE.1)  
     &      OPEN(21,FILE='TEMCNV1.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.2)  
     &      OPEN(22,FILE='TEMCNV2.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.3)  
     &      OPEN(23,FILE='TEMCNV3.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.4)  
     &      OPEN(24,FILE='TEMCNV4.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.5)  
     &      OPEN(25,FILE='TEMCNV5.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.6)  
     &      OPEN(26,FILE='TEMCNV6.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.7)  
     &      OPEN(27,FILE='TEMCNV7.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.8)  
     &      OPEN(28,FILE='TEMCNV8.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.9)  
     &      OPEN(29,FILE='TEMCNV9.OUT',POSITION='APPEND')  
        DO IS=1,ISECSPV  
          LUN=20+IS  
          WRITE (LUN,100)N,TIME  
          DO NN=1,NIJSPV(IS)  
            I=ISPV(NN,IS)  
            J=JSPV(NN,IS)  
            L=LIJ(I,J)  
            ZETA=P(L)*GI-SBPLTV(ITMP)*(HMP(L)+BELV(L))  
            HBTMP=HMP(L)  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),ZETA,HBTMP  
            WRITE(LUN,250)(TEM(L,K),K=1,KC)  
          ENDDO  
          CLOSE(LUN)  
        ENDDO  
      ENDIF  
      IF(ITMP.EQ.3.AND.ISTRAN(3).GE.1)THEN  
        IF(ISECSPV.GE.1)  
     &      OPEN(31,FILE='DYECNV1.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.2)  
     &      OPEN(32,FILE='DYECNV2.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.3)  
     &      OPEN(33,FILE='DYECNV3.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.4)  
     &      OPEN(34,FILE='DYECNV4.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.5)  
     &      OPEN(35,FILE='DYECNV5.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.6)  
     &      OPEN(36,FILE='DYECNV6.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.7)  
     &      OPEN(37,FILE='DYECNV7.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.8)  
     &      OPEN(38,FILE='DYECNV8.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.9)  
     &      OPEN(39,FILE='DYECNV9.OUT',POSITION='APPEND')  
        DO IS=1,ISECSPV  
          LUN=30+IS  
          WRITE (LUN,100)N,TIME  
          DO NN=1,NIJSPV(IS)  
            I=ISPV(NN,IS)  
            J=JSPV(NN,IS)  
            L=LIJ(I,J)  
            ZETA=P(L)*GI-SBPLTV(ITMP)*(HMP(L)+BELV(L))  
            HBTMP=HMP(L)  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),ZETA,HBTMP  
            WRITE(LUN,250)(DYE(L,K),K=1,KC)  
          ENDDO  
          CLOSE(LUN)  
        ENDDO  
      ENDIF  
      IF(ITMP.EQ.6.AND.ISTRAN(6).GE.1)THEN  
        IF(ISECSPV.GE.1)  
     &      OPEN(41,FILE='SEDCNV1.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.2)  
     &      OPEN(42,FILE='SEDCNV2.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.3)  
     &      OPEN(43,FILE='SEDCNV3.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.4)  
     &      OPEN(44,FILE='SEDCNV4.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.5)  
     &      OPEN(45,FILE='SEDCNV5.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.6)  
     &      OPEN(46,FILE='SEDCNV6.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.7)  
     &      OPEN(47,FILE='SEDCNV7.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.8)  
     &      OPEN(48,FILE='SEDCNV8.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.9)  
     &      OPEN(49,FILE='SEDCNV9.OUT',POSITION='APPEND')  
        DO IS=1,ISECSPV  
          LUN=40+IS  
          WRITE (LUN,100)N,TIME  
          DO NN=1,NIJSPV(IS)  
            I=ISPV(NN,IS)  
            J=JSPV(NN,IS)  
            L=LIJ(I,J)  
            ZETA=P(L)*GI-SBPLTV(ITMP)*(HMP(L)+BELV(L))  
            HBTMP=HMP(L)  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),ZETA,HMP(L)  
            DO NSC=1,NSED  
              WRITE(LUN,250)(SEDT(L,K),K=1,KC)  
            ENDDO  
          ENDDO  
          CLOSE(LUN)  
        ENDDO  
      ENDIF  
      IF(ITMP.EQ.7.AND.ISTRAN(7).GE.1)THEN  
        IF(ISECSPV.GE.1)  
     &      OPEN(51,FILE='SNDCNV1.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.2)  
     &      OPEN(52,FILE='SNDCNV2.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.3)  
     &      OPEN(53,FILE='SNDCNV3.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.4)  
     &      OPEN(54,FILE='SNDCNV4.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.5)  
     &      OPEN(55,FILE='SNDCNV5.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.6)  
     &      OPEN(56,FILE='SNDCNV6.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.7)  
     &      OPEN(57,FILE='SNDCNV7.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.8)  
     &      OPEN(58,FILE='SNDCNV8.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.9)  
     &      OPEN(59,FILE='SNDCNV9.OUT',POSITION='APPEND')  
        DO IS=1,ISECSPV  
          LUN=50+IS  
          WRITE (LUN,100)N,TIME  
          DO NN=1,NIJSPV(IS)  
            I=ISPV(NN,IS)  
            J=JSPV(NN,IS)  
            L=LIJ(I,J)  
            ZETA=P(L)*GI-SBPLTV(ITMP)*(HMP(L)+BELV(L))  
            HBTMP=HMP(L)  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),ZETA,HMP(L)  
            DO NSN=1,NSND  
              WRITE(LUN,250)(SNDT(L,K),K=1,KC)  
            ENDDO  
          ENDDO  
          CLOSE(LUN)  
        ENDDO  
      ENDIF  
      IF(ITMP.EQ.5.AND.ISTRAN(5).GE.1)THEN  
        IF(ISECSPV.GE.1)  
     &      OPEN(61,FILE='TOXCNV1.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.2)  
     &      OPEN(62,FILE='TOXCNV2.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.3)  
     &      OPEN(63,FILE='TOXCNV3.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.4)  
     &      OPEN(64,FILE='TOXCNV4.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.5)  
     &      OPEN(65,FILE='TOXCNV5.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.6)  
     &      OPEN(66,FILE='TOXCNV6.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.7)  
     &      OPEN(67,FILE='TOXCNV7.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.8)  
     &      OPEN(68,FILE='TOXCNV8.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.9)  
     &      OPEN(69,FILE='TOXCNV9.OUT',POSITION='APPEND')  
        DO IS=1,ISECSPV  
          LUN=60+IS  
          WRITE (LUN,100)N,TIME  
          DO NN=1,NIJSPV(IS)  
            I=ISPV(NN,IS)  
            J=JSPV(NN,IS)  
            L=LIJ(I,J)  
            ZETA=P(L)*GI-SBPLTV(ITMP)*(HMP(L)+BELV(L))  
            HBTMP=HMP(L)  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),ZETA,HMP(L)  
            DO NT=1,NTOX  
              WRITE(LUN,250)(TOX(L,K,1),K=1,KC)  
            ENDDO  
          ENDDO  
          CLOSE(LUN)  
        ENDDO  
      ENDIF  
      IF(ITMP.EQ.4.AND.ISTRAN(4).GE.1)THEN  
        IF(ISECSPV.GE.1)  
     &      OPEN(71,FILE='SFLCNV1.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.2)  
     &      OPEN(72,FILE='SFLCNV2.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.3)  
     &      OPEN(73,FILE='SFLCNV3.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.4)  
     &      OPEN(74,FILE='SFLCNV4.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.5)  
     &      OPEN(75,FILE='SFLCNV5.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.6)  
     &      OPEN(76,FILE='SFLCNV6.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.7)  
     &      OPEN(77,FILE='SFLCNV7.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.8)  
     &      OPEN(78,FILE='SFLCNV8.OUT',POSITION='APPEND')  
        IF(ISECSPV.GE.9)  
     &      OPEN(79,FILE='SFLCNV9.OUT',POSITION='APPEND')  
        DO IS=1,ISECSPV  
          LUN=70+IS 
          WRITE (LUN,100)N,TIME  
          DO NN=1,NIJSPV(IS)  
            I=ISPV(NN,IS)  
            J=JSPV(NN,IS)  
            L=LIJ(I,J)  
            ZETA=P(L)*GI-SBPLTV(ITMP)*(HMP(L)+BELV(L))  
            HBTMP=HMP(L)  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),ZETA,HBTMP  
            WRITE(LUN,250)(SFL(L,K),K=1,KC)  
          ENDDO  
          CLOSE(LUN)  
        ENDDO  
      ENDIF  
   99 FORMAT(A40,2X,A20)  
  100 FORMAT(I10,F12.4)  
  101 FORMAT(2I10)  
  200 FORMAT(2I5,1X,6E14.6)  
  250 FORMAT(12E12.4)  
      RETURN  
      END  

