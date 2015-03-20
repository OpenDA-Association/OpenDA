      SUBROUTINE RESTIN2  
C  
C CHANGE RECORD  
C **  SUBROUTINE RESTINP READS A RESTART FILE FOR (KC/2) LAYERS AND  
C **  AND INITIALIZES FOR KC LAYERS  
C  
      USE GLOBAL  

      PRINT *,'READING RESTIN2 FILE: RESTART.INP'
      OPEN(1,FILE='RESTART.INP',STATUS='UNKNOWN')  
      READ(1,908,ERR=1000)NREST  
      DO L=2,LA  
        READ(1,*,ERR=1000)HP(L),H1P(L),HWQ(L),H2WQ(L)  
        READ(1,*,ERR=1000)UHDYE(L),UHDY1E(L),VHDXE(L),VHDX1E(L)  
        READ(1,*,ERR=1000)(U(L,K),K=2,KC,2)  
        READ(1,*,ERR=1000)(U1(L,K),K=2,KC,2)  
        READ(1,*,ERR=1000)(V(L,K),K=2,KC,2)  
        READ(1,*,ERR=1000)(V1(L,K),K=2,KC,2)  
        READ(1,*,ERR=1000)(QQ(L,K),K=0,KC,2)  
        READ(1,*,ERR=1000)(QQ1(L,K),K=0,KC,2)  
        READ(1,*,ERR=1000)(QQL(L,K),K=0,KC,2)  
        READ(1,*,ERR=1000)(QQL1(L,K),K=0,KC,2)  
        READ(1,*,ERR=1000)(DML(L,K),K=0,KC,2)  
        DO K=1,KS,2  
          U(L,K)=U(L,K+1)  
          U1(L,K)=U1(L,K+1)  
          V(L,K)=V(L,K+1)  
          V1(L,K)=V1(L,K+1)  
          QQ(L,K)=0.5*(QQ(L,K-1)+QQ(L,K+1))  
          QQSQR(L,K)=SQRT(QQ(L,K))  ! *** DSLLC
          QQ1(L,K)=0.5*(QQ1(L,K-1)+QQ1(L,K+1))  
          QQL(L,K)=0.5*(QQL(L,K-1)+QQL(L,K+1))  
          QQL1(L,K)=0.5*(QQL1(L,K-1)+QQL1(L,K+1))  
          DML(L,K)=0.5*(DML(L,K-1)+DML(L,K+1))  
        ENDDO  
        IF(ISCI(1).EQ.1)THEN  
          READ(1,*,ERR=1000)(SAL(L,K),K=2,KC,2)  
          READ(1,*,ERR=1000)(SAL1(L,K),K=2,KC,2)  
          DO K=1,KS,2  
            SAL(L,K)=SAL(L,K+1)  
            SAL1(L,K)=SAL1(L,K+1)  
          ENDDO  
        ENDIF  
        IF(ISCI(2).EQ.1)THEN  
          READ(1,*,ERR=1000)(TEM(L,K),K=2,KC,2)  
          READ(1,*,ERR=1000)(TEM1(L,K),K=2,KC,2)  
          DO K=1,KS,2  
            TEM(L,K)=TEM(L,K+1)  
            TEM1(L,K)=TEM1(L,K+1)  
          ENDDO  
        ENDIF  
        IF(ISCI(3).EQ.1)THEN  
          READ(1,*,ERR=1000)(DYE(L,K),K=2,KC,2)  
          READ(1,*,ERR=1000)(DYE1(L,K),K=2,KC,2)  
          DO K=1,KS,2  
            DYE(L,K)=DYE(L,K+1)  
            DYE1(L,K)=DYE1(L,K+1)  
          ENDDO  
        ENDIF  
        IF(ISCI(4).EQ.1)THEN  
          READ(1,*,ERR=1000)SEDB(L,1,1),(SED(L,K,1),K=2,KC,2)  
          READ(1,*,ERR=1000)SEDB1(L,1,1),(SED1(L,K,1),K=2,KC,2)  
          DO K=1,KS,2  
            SED(L,K,1)=SED(L,K+1,1)  
            SED1(L,K,1)=SED1(L,K+1,1)  
          ENDDO  
        ENDIF  
        IF(ISCI(5).EQ.1)THEN  
          READ(1,*,ERR=1000)(SFL(L,K),K=2,KC,2)  
          READ(1,*,ERR=1000)(SFL2(L,K),K=2,KC,2)  
          DO K=1,KS,2  
            SFL(L,K)=SFL(L,K+1)  
            SFL2(L,K)=SFL2(L,K+1)  
          ENDDO  
        ENDIF  
      ENDDO  
      DO M=1,5  
        IF(ISCI(M).EQ.1)THEN  
          DO LL=1,NCBS  
            READ(1,*,ERR=1000)(NLOS(LL,K,M),K=2,KC,2)  
            READ(1,*,ERR=1000)(CLOS(LL,K,M),K=2,KC,2)  
          ENDDO  
          DO LL=1,NCBW  
            READ(1,*,ERR=1000)(NLOW(LL,K,M),K=2,KC,2)  
            READ(1,*,ERR=1000)(CLOW(LL,K,M),K=2,KC,2)  
          ENDDO  
          DO LL=1,NCBE  
            READ(1,*,ERR=1000)(NLOE(LL,K,M),K=2,KC,2)  
            READ(1,*,ERR=1000)(CLOE(LL,K,M),K=2,KC,2)  
          ENDDO  
          DO LL=1,NCBN  
            READ(1,*,ERR=1000)(NLON(LL,K,M),K=2,KC,2)  
            READ(1,*,ERR=1000)(CLON(LL,K,M),K=2,KC,2)  
          ENDDO  
        ENDIF  
      ENDDO  
      DO M=1,5  
        DO K=1,KS,2  
          DO LL=1,NCBS  
            NLOS(LL,K,M)=NLOS(LL,K+1,M)  
            CLOS(LL,K,M)=CLOS(LL,K+1,M)  
          ENDDO  
          DO LL=1,NCBW  
            NLOW(LL,K,M)=NLOW(LL,K+1,M)  
            CLOW(LL,K,M)=CLOW(LL,K+1,M)  
          ENDDO  
          DO LL=1,NCBE  
            NLOE(LL,K,M)=NLOE(LL,K+1,M)  
            CLOE(LL,K,M)=CLOE(LL,K+1,M)  
          ENDDO  
          DO LL=1,NCBN  
            NLON(LL,K,M)=NLON(LL,K+1,M)  
            CLON(LL,K,M)=CLON(LL,K+1,M)  
          ENDDO  
        ENDDO  
      ENDDO  
      CLOSE(1)  
      DO K=1,KC  
        SAL(1,K)=0.  
        TEM(1,K)=0.  
        DYE(1,K)=0.  
        SED(1,K,1)=0.  
        SFL(1,K)=0.  
        CWQ(1,K)=0.  
        VHDX(1,K)=0.  
        UHDY(1,K)=0.  
        SAL1(1,K)=0.  
        TEM1(1,K)=0.  
        DYE1(1,K)=0.  
        SED1(1,K,1)=0.  
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
        SFL(LC,K)=0.  
        CWQ(LC,K)=0.  
        VHDX(LC,K)=0.  
        UHDY(LC,K)=0.  
        SAL1(LC,K)=0.  
        TEM1(LC,K)=0.  
        DYE1(LC,K)=0.  
        SED1(LC,K,1)=0.  
        SFL2(LC,K)=0.  
        CWQ2(LC,K)=0.  
        VHDX1(LC,K)=0.  
        UHDY1(LC,K)=0.  
        VHDXWQ(LC,K)=0.  
        UHDYWQ(LC,K)=0.  
      ENDDO  
      DO L=2,LA  
        LS=LSC(L)  
        H1U(L)=0.5*(H1P(L)+H1P(L-1))  
        H1V(L)=0.5*(H1P(L)+H1P(LS))  
        P1(L)=G*(H1P(L)+BELV(L))  
        HU(L)=0.5*(HP(L)+HP(L-1))  
        HV(L)=0.5*(HP(L)+HP(LS))  
        P(L)=G*(HP(L)+BELV(L))  
        HPI(L)=1./HP(L)  
        HUI(L)=1./HU(L)  
        HVI(L)=1./HV(L)  
        H1UI(L)=1./H1U(L)  
        H1VI(L)=1./H1V(L)  
      ENDDO  
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
      N=0  
      CALL CALQVS (2)  
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
C **  WRITE READ ERRORS ON RESTART  
C  
      GOTO 1002  
 1000 WRITE(6,1001)  
 1001 FORMAT('  READ ERROR ON FILE RESTART.INP ')  
      STOP  
 1002 CONTINUE  
  906 FORMAT(4E15.7)  
  907 FORMAT(12E12.4)  
  908 FORMAT(12I10)  
      RETURN  
      END  

