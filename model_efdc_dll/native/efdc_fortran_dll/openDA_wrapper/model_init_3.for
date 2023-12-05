      SUBROUTINE model_init_3

C     Final part of initialization after one of the RESTART subroutines
C     is called and before the actual time integration takes place HDMT
C     Original EFDC.INP [lines: 951-2002]

C     Actions: 
C **  INTIALIZE SALINITY FIELD IF NOT READ IN FROM RESTART FILE  

C **  INTIALIZE TEMP FIELD IF NOT READ IN FROM RESTART FILE  

C **  INTIALIZE TEMPERATURE BC IF NOT READ IN FROM RESTART FILE  
C       AND CONSTANT INTIAL CONDITION IS USED  

C **  INTIALIZE DYE FIELD IF NOT READ IN FROM RESTART FILE  

C **  INTIALIZE DYE BC IF NOT READ IN FROM RESTART FILE  
C       AND CONSTANT INITIAL CONDITIONS ARE USED  

C **  INTIALIZE TOX AND BC IF NOT READ IN FROM RESTART FILE  
C       AND VARIABLE INITIAL CONDITIONS ARE USED  
     
C **  INTIALIZE TOX BED IF NOT READ IN FROM RESTART FILE  
C       AND VARIABLE INITIAL CONDITIONS ARE USED  

C **  INTIALIZE SED AND BC IF NOT READ IN FROM RESTART FILE  
C       AND VARIABLE INITIAL CONDITIONS ARE USED  

C **  INTIALIZE SED BC IF NOT READ IN FROM RESTART FILE 
C       AND CONSTANT INITIAL CONDITIONS ARE USED  

C **  INTIALIZE SND AND BC IF NOT READ IN FROM RESTART FILE  
C       AND VARIABLE INITIAL CONDITIONS ARE USED  

C **  INTIALIZE SND BC IF NOT READ IN FROM RESTART FILE 
C       AND CONSTANT INITIAL CONDITIONS ARE USED  

C **  INITIALIZE SEDIMENT BED  

C **  INITIALIZE BUOYANCY AND EQUATION OF STATE  

C **  INITIALIZE SFL IF(ISRESTI.EQ.0.AND ISTRAN(4).GE.1)  

C **  ACTIVATE DYE TRACER CONTINUITY CHECK  

C **  SET VERTICAL GRID DEPENDENT ARRAYS AND HARDWIRE DIMENSIONLESS  
C **  MIXING LENGTH  

C **  INITIALIZE UNSTRETCHING PROCEDURE  

C **  CALCULATE CONSTANT HORIZONTAL SPATIAL ARRAYS  

C **  INITIALIZE ZERO DIMENSION VOLUME BALANCE  

C **  INITIALIZE ELEVATION OF ACTIVE GROUNDWATER ZONE FOR COLD START  

C **  CALCULATE CONSTANT C ARRAYS FOR EXTERNAL P SOLUTION  
C       HRU=SUB*HMU*DYU/DXU & HRV=SVB*HMV*DXV/DYV  
C       DXYIP=1/(DXP*DYP)  

C **  SMOOTH INITIAL SALINITY  

C **  INITIALIZE SALINITY AND TEMPATURE DATA ASSIMILATION  

C **  INITIALIZE WATER QUALITY MODEL AND READ INPUT  

C **  INITIALIZE EFDC EXPLORER OUTPUT  

C **  INITIALIZE EFDC HYDRO DISTRIBUTION OUTPUT  

      USE GLOBAL
      
      IMPLICIT NONE

      INTEGER :: I, J, K, KK, KP, L, LL, LF, LN, LS
      INTEGER :: M, NS, NT, NX
      INTEGER :: IISTMP, LBELMIN, NREST, LPBTMP
      integer :: ithds
      REAL :: C1, DELVOL, DZPC, TMP, WTM, WTMP
      REAL :: BELMIN, VOLLDRY
C  
C **  INTIALIZE SALINITY FIELD IF NOT READ IN FROM RESTART FILE  
C  
      IF(ISTRAN(1).GE.1.AND.(ISRESTI.EQ.0                  .OR.  
     &                      (ISRESTI.GE.1.AND.ISCI(1).EQ.0).OR.
     &                      (ISTOPT(1).GT.1)))THEN  ! *** PMC SINGLE LINE - FORCE IC
        IF(ISTOPT(1).GE.1)THEN  
          NREST=0  
          DO K=1,KC  
            DO L=2,LA  
              SAL(L,K)=SALINIT(L,K)  
              SAL1(L,K)=SALINIT(L,K)  
            ENDDO  
          ENDDO  

          DO K=1,KC  
            DO LL=1,NCBS  
              L=LCBS(LL)  
              CLOS(LL,K,1)=SALINIT(L,K)  
              NLOS(LL,K,1)=0  
              IF(NCSERS(LL,1).EQ.0)THEN  
                SAL(L,K)=WTCI(K,1)*CBS(LL,1,1)+WTCI(K,2)*CBS(LL,2,1)  
                SAL1(L,K)=SAL(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              L=LCBW(LL)  
              CLOW(LL,K,1)=SALINIT(L,K)  
              NLOW(LL,K,1)=0  
              IF(NCSERW(LL,1).EQ.0)THEN  
                SAL(L,K)=WTCI(K,1)*CBW(LL,1,1)+WTCI(K,2)*CBW(LL,2,1)  
                SAL1(L,K)=SAL(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              L=LCBE(LL)  
              CLOE(LL,K,1)=SALINIT(L,K)  
              NLOE(LL,K,1)=0  
              IF(NCSERE(LL,1).EQ.0)THEN  
                SAL(L,K)=WTCI(K,1)*CBE(LL,1,1)+WTCI(K,2)*CBE(LL,2,1)  
                SAL1(L,K)=SAL(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              L=LCBN(LL)  
              CLON(LL,K,1)=SALINIT(L,K)  
              NLON(LL,K,1)=0  
              IF(NCSERN(LL,1).EQ.0)THEN  
                SAL(L,K)=WTCI(K,1)*CBN(LL,1,1)+WTCI(K,2)*CBN(LL,2,1)  
                SAL1(L,K)=SAL(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  

        ENDIF  
      ENDIF  
 9101 FORMAT(I5)  
 9102 FORMAT(3I5,12F8.2)  
C  
C **  INTIALIZE TEMP FIELD IF NOT READ IN FROM RESTART FILE  
C  
      IF(ISTRAN(2).GE.1.AND.(ISRESTI.EQ.0                  .OR.  
     &                      (ISRESTI.GE.1.AND.ISCI(2).EQ.0).OR.
     &                      (ISTOPT(2).GT.9)))THEN  ! *** PMC SINGLE LINE - FORCE IC
          ! *** SPATIALLY VARYING TEMPERATURE FIELD
          NREST=0  
          DO K=1,KC  
            DO L=2,LA  
              TEM(L,K)=TEMINIT(L,K)  
              TEM1(L,K)=TEM(L,K)  
            ENDDO  
          ENDDO  

          DO K=1,KC  
            DO LL=1,NCBS  
              L=LCBS(LL)  
              CLOS(LL,K,2)=TEMINIT(L,K)  
              NLOS(LL,K,2)=0  
              IF(NCSERS(LL,2).EQ.0)THEN  
                TEM(L,K)=WTCI(K,1)*CBS(LL,1,2)+WTCI(K,2)*CBS(LL,2,2)  
                TEM1(L,K)=TEM(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              L=LCBW(LL)  
              CLOW(LL,K,2)=TEMINIT(L,K)  
              NLOW(LL,K,2)=0  
              IF(NCSERW(LL,2).EQ.0)THEN  
                TEM(L,K)=WTCI(K,1)*CBW(LL,1,2)+WTCI(K,2)*CBW(LL,2,2)  
                TEM1(L,K)=TEM(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              L=LCBE(LL)  
              CLOE(LL,K,2)=TEMINIT(L,K)  
              NLOE(LL,K,2)=0  
              IF(NCSERE(LL,2).EQ.0)THEN  
                TEM(L,K)=WTCI(K,1)*CBE(LL,1,2)+WTCI(K,2)*CBE(LL,2,2)  
                TEM1(L,K)=TEM(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              L=LCBN(LL)  
              CLON(LL,K,2)=TEMINIT(L,K)  
              NLON(LL,K,2)=0  
              IF(NCSERN(LL,2).EQ.0)THEN  
                TEM(L,K)=WTCI(K,1)*CBN(LL,1,2)+WTCI(K,2)*CBN(LL,2,2)  
                TEM1(L,K)=TEM(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  

      ENDIF  
C  
C **  INTIALIZE TEMPERATURE BC IF NOT READ IN FROM RESTART FILE  
C     AND CONSTANT INTIAL CONDITION IS USED  
C  
      IF(ISRESTI.EQ.0.AND.ISTRAN(2).GE.1)THEN  
        IF(ISTOPT(2).EQ.0)THEN  
          ! *** CONSTANT TEMPERATURE FIELD
          M=2  
          DO K=1,KC  
            DO LL=1,NCBS  
              CLOS(LL,K,M)=TEMO  
              NLOS(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              CLOW(LL,K,M)=TEMO  
              NLOW(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              CLOE(LL,K,M)=TEMO  
              NLOE(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              CLON(LL,K,M)=TEMO  
              NLON(LL,K,M)=0  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  

      ! *** RESET IC OPTION, IF USED
      IF(ISTOPT(2).GT.9)ISTOPT(2)=ISTOPT(2)-10 ! PMC SINGLE LINE
C  
C **  INTIALIZE DYE FIELD IF NOT READ IN FROM RESTART FILE  
C  
      IF(ISTRAN(3).GE.1.AND.(ISRESTI.EQ.0                  .OR.  
     &                      (ISRESTI.GE.1.AND.ISCI(3).EQ.0).OR.
     &                      (ISTOPT(3).GT.1)))THEN  ! *** PMC SINGLE LINE - FORCE IC
        IF(ISTOPT(3).GE.1)THEN  
          ! *** SPATIALLY VARIABLE DYE FIELD
          NREST=0  
          DO K=1,KC  
            DO L=2,LA  
              DYE(L,K)=DYEINIT(L,K)  
              DYE1(L,K)=DYE(L,K)  
            ENDDO  
          ENDDO  

          DO K=1,KC  
            DO LL=1,NCBS  
              L=LCBS(LL)  
              CLOS(LL,K,3)=DYEINIT(L,K)  
              NLOS(LL,K,3)=0  
              IF(NCSERS(LL,3).EQ.0)THEN  
                DYE(L,K)=WTCI(K,1)*CBS(LL,1,3)+WTCI(K,2)*CBS(LL,2,3)  
                DYE1(L,K)=DYE(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              L=LCBW(LL)  
              CLOW(LL,K,3)=DYEINIT(L,K)  
              NLOW(LL,K,3)=0  
              IF(NCSERW(LL,3).EQ.0)THEN  
                DYE(L,K)=WTCI(K,1)*CBW(LL,1,3)+WTCI(K,2)*CBW(LL,2,3)  
                DYE1(L,K)=DYE(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              L=LCBE(LL)  
              CLOE(LL,K,3)=DYEINIT(L,K)  
              NLOE(LL,K,3)=0  
              IF(NCSERE(LL,3).EQ.0)THEN  
                DYE(L,K)=WTCI(K,1)*CBE(LL,1,3)+WTCI(K,2)*CBE(LL,2,3)  
                DYE1(L,K)=DYE(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              L=LCBN(LL)  
              CLON(LL,K,3)=DYEINIT(L,K)  
              NLON(LL,K,3)=0  
              IF(NCSERN(LL,3).EQ.0)THEN  
                DYE(L,K)=WTCI(K,1)*CBN(LL,1,3)+WTCI(K,2)*CBN(LL,2,3)  
                DYE1(L,K)=DYE(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  

        ENDIF  
      ENDIF  
C  
C **  INTIALIZE DYE BC IF NOT READ IN FROM RESTART FILE  
C **  AND CONSTANT INITIAL CONDITIONS ARE USED  
C  
      IF((ISRESTI.EQ.0.AND.ISTRAN(3).GE.1).OR.
     &   (ISRESTI.GE.1.AND.ISCI(3).EQ.0))THEN     ! *** PMC SINGLE LINE
        IF(ISTOPT(3).EQ.0)THEN  
          M=3  
          DO K=1,KC  
            DO LL=1,NCBS  
              CLOS(LL,K,M)=0.  
              NLOS(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              CLOW(LL,K,M)=0.  
              NLOW(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              CLOE(LL,K,M)=0.  
              NLOE(LL,K,M)=0  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              CLON(LL,K,M)=0.  
              NLON(LL,K,M)=0  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE TOX AND BC IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(5).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(5).EQ.1)THEN  
        DO NT=1,NTOX  
          IF(ITXINT(NT).EQ.1.OR.ITXINT(NT).EQ.3)THEN  
            M=4+NT  
            DO K=1,KC  
              DO L=2,LA  
                TOX(L,K,NT)=TOXINIT(L,K,NT)  
                TOX1(L,K,NT)=TOX(L,K,NT)  
              ENDDO  
            ENDDO  

            DO K=1,KC  
              DO LL=1,NCBS  
               L=LCBS(LL)  
               CLOS(LL,K,M)=TOXINIT(L,K,NT)  
               NLOS(LL,K,M)=0  
               IF(NCSERS(LL,M).EQ.0)THEN  
               TOX(L,K,NT)=WTCI(K,1)*CBS(LL,1,M)+WTCI(K,2)*CBS(LL,2,M)  
               TOX1(L,K,NT)=TOX(L,K,NT)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
               L=LCBW(LL)  
               CLOW(LL,K,M)=TOXINIT(L,K,NT)  
               NLOW(LL,K,M)=0  
               IF(NCSERW(LL,M).EQ.0)THEN  
               TOX(L,K,NT)=WTCI(K,1)*CBW(LL,1,M)+WTCI(K,2)*CBW(LL,2,M)  
               TOX1(L,K,NT)=TOX(L,K,NT)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
               L=LCBE(LL)  
               CLOE(LL,K,M)=TOXINIT(L,K,NT)  
               NLOE(LL,K,M)=0  
               IF(NCSERE(LL,3).EQ.0)THEN  
               TOX(L,K,NT)=WTCI(K,1)*CBE(LL,1,M)+WTCI(K,2)*CBE(LL,2,M)  
               TOX1(L,K,NT)=TOX(L,K,NT)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
               L=LCBN(LL)  
               CLON(LL,K,M)=TOXINIT(L,K,NT)  
               NLON(LL,K,M)=0  
               IF(NCSERN(LL,M).EQ.0)THEN  
               TOX(L,K,NT)=WTCI(K,1)*CBN(LL,1,M)+WTCI(K,2)*CBN(LL,2,M)  
               TOX1(L,K,NT)=TOX(L,K,NT)  
               ENDIF  
              ENDDO  
            ENDDO  

          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  INTIALIZE TOX BC IF NOT READ IN FROM RESTART FILE  
C **  AND CONSTANT INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(5).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(5).EQ.1)THEN  
        DO NT=1,NTOX  
          IF(ITXINT(NT).EQ.0.OR.ITXINT(NT).EQ.2)THEN  
            M=4+NT  
            DO K=1,KC  
              DO LL=1,NCBS  
                CLOS(LL,K,M)=TOXINTW(NT)  
                NLOS(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
                CLOW(LL,K,M)=TOXINTW(NT)  
                NLOW(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
                CLOE(LL,K,M)=TOXINTW(NT)  
                NLOE(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
                CLON(LL,K,M)=TOXINTW(NT)  
                NLON(LL,K,M)=0  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  INTIALIZE TOX BED IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(5).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(5).EQ.1)THEN  
        DO NT=1,NTOX  
          IF(ITXINT(NT).EQ.2.OR.ITXINT(NT).EQ.3)THEN  
            DO K=1,KB  
              DO L=2,LA  
                TOXB(L,K,NT)=TOXBINIT(L,K,NT)  
                TOXB1(L,K,NT)=TOXB(L,K,NT)  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  INTIALIZE SED AND BC IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(6).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(6).EQ.1)THEN  
        IF(ISEDINT.EQ.1.OR.ISEDINT.EQ.3)THEN  
          DO NS=1,NSED  
            M=4+NTOX+NS  
            DO K=1,KC  
              DO L=2,LA  
                SED(L,K,NS)=SEDINIT(L,K,NS)  
                SED1(L,K,NS)=SED(L,K,NS)  
              ENDDO  
            ENDDO  

            DO K=1,KC  
              DO LL=1,NCBS  
               L=LCBS(LL)  
               CLOS(LL,K,M)=SEDINIT(L,K,NS)  
               NLOS(LL,K,M)=0  
               IF(NCSERS(LL,M).EQ.0)THEN  
               SED(L,K,NS)=WTCI(K,1)*CBS(LL,1,M)+WTCI(K,2)*CBS(LL,2,M)  
               SED1(L,K,NS)=SED(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
               L=LCBW(LL)  
               CLOW(LL,K,M)=SEDINIT(L,K,NS)  
               NLOW(LL,K,M)=0  
               IF(NCSERW(LL,M).EQ.0)THEN  
               SED(L,K,NS)=WTCI(K,1)*CBW(LL,1,M)+WTCI(K,2)*CBW(LL,2,M)  
               SED1(L,K,NS)=SED(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
               L=LCBE(LL)  
               CLOE(LL,K,M)=SEDINIT(L,K,NS)  
               NLOE(LL,K,M)=0  
               IF(NCSERE(LL,3).EQ.0)THEN  
               SED(L,K,NS)=WTCI(K,1)*CBE(LL,1,M)+WTCI(K,2)*CBE(LL,2,M)  
               SED1(L,K,NS)=SED(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
               L=LCBN(LL)  
               CLON(LL,K,M)=SEDINIT(L,K,NS)  
               NLON(LL,K,M)=0  
               IF(NCSERN(LL,M).EQ.0)THEN  
               SED(L,K,NS)=WTCI(K,1)*CBN(LL,1,M)+WTCI(K,2)*CBN(LL,2,M)  
               SED1(L,K,NS)=SED(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  

          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE SED BC IF NOT READ IN FROM RESTART FILE AND  
C **  CONSTANT INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(6).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(6).EQ.1)THEN  
        IF(ISEDINT.EQ.0.OR.ISEDINT.EQ.2)THEN  
          DO NS=1,NSED  
            M=4+NTOX+NS  
            DO K=1,KC  
              DO LL=1,NCBS  
                CLOS(LL,K,M)=SEDO(NS)  
                NLOS(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
                CLOW(LL,K,M)=SEDO(NS)  
                NLOW(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
                CLOE(LL,K,M)=SEDO(NS)  
                NLOE(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
                CLON(LL,K,M)=SEDO(NS)  
                NLON(LL,K,M)=0  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE SED BED IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(6).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(6).EQ.1)THEN  
        IF(ISEDINT.EQ.2.OR.ISEDINT.EQ.3)THEN  
          DO NS=1,NSED  
            DO K=1,KB  
              DO L=2,LA  
                SEDB(L,K,NS)=SEDBINIT(L,K,NS)  
                SEDB1(L,K,NS)=SEDB(L,K,NS)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE SND AND BC IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(7).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(7).EQ.1)THEN  
        IF(ISEDINT.EQ.1.OR.ISEDINT.EQ.3)THEN  
          DO NS=1,NSND  
            M=4+NTOX+NSED+NS  
            DO K=1,KC  
              DO L=2,LA  
                SND(L,K,NS)=SNDINIT(L,K,NS)  
                SND1(L,K,NS)=SND(L,K,NS)  
              ENDDO  
            ENDDO  

            DO K=1,KC  
              DO LL=1,NCBS  
               L=LCBS(LL)  
               CLOS(LL,K,M)=SNDINIT(L,K,NS)  
               NLOS(LL,K,M)=0  
               IF(NCSERS(LL,M).EQ.0)THEN  
               SND(L,K,NS)=WTCI(K,1)*CBS(LL,1,M)+WTCI(K,2)*CBS(LL,2,M)  
               SND1(L,K,NS)=SND(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
               L=LCBW(LL)  
               CLOW(LL,K,M)=SNDINIT(L,K,NS)  
               NLOW(LL,K,M)=0  
               IF(NCSERW(LL,M).EQ.0)THEN  
               SND(L,K,NS)=WTCI(K,1)*CBW(LL,1,M)+WTCI(K,2)*CBW(LL,2,M)  
               SND1(L,K,NS)=SND(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
               L=LCBE(LL)  
               CLOE(LL,K,M)=SNDINIT(L,K,NS)  
               NLOE(LL,K,M)=0  
               IF(NCSERE(LL,3).EQ.0)THEN  
               SND(L,K,NS)=WTCI(K,1)*CBE(LL,1,M)+WTCI(K,2)*CBE(LL,2,M)  
               SND1(L,K,NS)=SND(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
               L=LCBN(LL)  
               CLON(LL,K,M)=SNDINIT(L,K,NS)  
               NLON(LL,K,M)=0  
               IF(NCSERN(LL,M).EQ.0)THEN  
               SND(L,K,NS)=WTCI(K,1)*CBN(LL,1,M)+WTCI(K,2)*CBN(LL,2,M)  
               SND1(L,K,NS)=SED(L,K,NS)  
               ENDIF  
              ENDDO  
            ENDDO  

          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE SND BC IF NOT READ IN FROM RESTART FILE AND  
C **  CONSTANT INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(7).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(7).EQ.1)THEN  
        IF(ISEDINT.EQ.0.OR.ISEDINT.EQ.2)THEN  
          DO NX=1,NSND  
            NS=NSED+NX  
            M=4+NTOX+NSED+NX  
            DO K=1,KC  
              DO LL=1,NCBS  
                CLOS(LL,K,M)=SEDO(NS)  
                NLOS(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBW  
                CLOW(LL,K,M)=SEDO(NS)  
                NLOW(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBE  
                CLOE(LL,K,M)=SEDO(NS)  
                NLOE(LL,K,M)=0  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO LL=1,NCBN  
                CLON(LL,K,M)=SEDO(NS)  
                NLON(LL,K,M)=0  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INTIALIZE SND BED IF NOT READ IN FROM RESTART FILE  
C **  AND VARIABLE INITIAL CONDITIONS ARE USED  
C  
      IISTMP=1  
      IF(ISRESTI.EQ.0) IISTMP=0  
      IF(ISRESTI.GE.1.AND.ISCI(7).EQ.0) IISTMP=0  
      IF(IISTMP.EQ.0.AND.ISTRAN(7).EQ.1)THEN  
        IF(ISEDINT.EQ.2.OR.ISEDINT.EQ.3)THEN  
          DO NX=1,NSND  
            DO K=1,KB  
              DO L=2,LA  
                SNDB(L,K,NX)=SNDBINIT(L,K,NX)  
                SNDB1(L,K,NX)=SNDB(L,K,NX)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  INITIALIZE SEDIMENT BED  
C  
      IF(ISTRAN(6).GE.1.OR.ISTRAN(7).GE.1) CALL BEDINIT  
C  
C **  INITIALIZE BUOYANCY AND EQUATION OF STATE  
C  
      CALL CALBUOY
C  
C **  INITIALIZE SFL IF(ISRESTI.EQ.0.AND ISTRAN(4).GE.1)  
C  
      IF(ISRESTI.EQ.0.AND.ISTRAN(4).GE.1)THEN  
        IF(ISTOPT(4).EQ.11)THEN  
          DO K=1,KC  
            DO L=1,LC  
              SFL(L,K)=SAL(L,K)  
              SFL2(L,K)=SAL(L,K)  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBS  
              L=LCBS(LL)  
              CLOS(LL,K,5)=SALINIT(L,K)  
              NLOS(LL,K,5)=0  
              SFL(L,K)=WTCI(K,1)*CBS(LL,1,5)+WTCI(K,2)*CBS(LL,2,5)  
              SFL2(L,K)=SFL(L,K)  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBW  
              L=LCBW(LL)  
              CLOW(LL,K,5)=SALINIT(L,K)  
              NLOW(LL,K,5)=0  
              SFL(L,K)=WTCI(K,1)*CBW(LL,1,5)+WTCI(K,2)*CBW(LL,2,5)  
              SFL2(L,K)=SFL(L,K)  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBE  
              L=LCBE(LL)  
              CLOE(LL,K,5)=SALINIT(L,K)  
              NLOE(LL,K,5)=0  
              SFL(L,K)=WTCI(K,1)*CBE(LL,1,5)+WTCI(K,2)*CBE(LL,2,5)  
              SFL2(L,K)=SFL(L,K)  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO LL=1,NCBN  
              L=LCBN(LL)  
              CLON(LL,K,5)=SALINIT(L,K)  
              NLON(LL,K,5)=0  
              SFL(L,K)=WTCI(K,1)*CBN(LL,1,5)+WTCI(K,2)*CBN(LL,2,5)  
              SFL2(L,K)=SFL(L,K)  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  ACTIVATE DYE TRACER CONTINUITY CHECK  
C  
      IF(ISMMC.EQ.1)THEN  
        DO K=1,KC  
          DO L=1,LC  
            DYE(L,K)=1.  
            DYE1(L,K)=1.  
          ENDDO  
        ENDDO  
        DO K=1,KC  
          DO LL=1,NCBS  
            CLOS(LL,K,3)=1.  
            NLOS(LL,K,3)=0  
          ENDDO  
          DO LL=1,NCBW  
            CLOW(LL,K,3)=1.  
            NLOW(LL,K,3)=0  
          ENDDO  
          DO LL=1,NCBE  
            CLOE(LL,K,3)=1.  
            NLOE(LL,K,3)=0  
          ENDDO  
          DO LL=1,NCBN  
            CLON(LL,K,3)=1.  
            NLON(LL,K,3)=0  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  MASK CELLS TO BE CONVERTED FROM WATER TO LAND  
C **  CALL MOVED TO SETBCS ON 23 JAN 2004  
C **  SET VERTICAL GRID DEPENDENT ARRAYS AND HARDWIRE DIMENSIONLESS  
C **  MIXING LENGTH  
C  
      DO K=1,KC  
        DZIC(K)=1./DZC(K)  
      ENDDO  
      DZIG(0)=0.  
      DZIG(KC)=0.  
      DO K=1,KS  
        DZG(K)=0.5*(DZC(K)+DZC(K+1))  
        DZIG(K)=1./DZG(K)  
        DZIGSD4(K)=0.25*DZIG(K)*DZIG(K)  
        CDZU(K)=-DZC(K)/(DZC(K)+DZC(K+1))  
        CDZL(K)=-DZC(K+1)/(DZC(K)+DZC(K+1))  
        CDZF(K)=DZC(K)*DZC(K+1)/(DZC(K)+DZC(K+1))  
        CDZM(K)=0.5*DZC(K)*DZC(K+1)  
      ENDDO  
      CDZR(1)=DZC(1)-1.  
      CDZD(1)=DZC(1)  
      DO K=2,KS  
        CDZR(K)=DZC(K)+CDZR(K-1)  
        CDZD(K)=DZC(K)+CDZD(K-1)  
      ENDDO  
      DO K=1,KS  
        CDZR(K)=CDZR(K)*DZG(K)*CDZL(1)  
      ENDDO  
      CDZKMK(1)=0.  
      DO K=2,KC  
        CDZKMK(K)=DZIG(K-1)*DZIC(K)  
      ENDDO  
      DO K=1,KS  
        CDZKK(K)=DZIC(K)*DZIG(K)  
        CDZKKP(K)=DZIG(K)*DZIC(K+1)  
      ENDDO  
      CDZKK(KC)=0.  
      Z(0)=0.  
      IF(KC.GT.1)THEN  
        DO K=1,KS  
          Z(K)=Z(K-1)+DZC(K)     ! *** TOP OF LAYER Z
          ZZ(K)=Z(K)-0.5*DZC(K)  ! *** MID LAYER Z
          FPROX(K)=(1./(VKC*Z(K))**2)+0.25*(1./(VKC*(1.-Z(K)))**2)/CTE2  
        ENDDO  
      ENDIF  
      Z(KC)=Z(KS)+DZC(KC)  
      ZZ(KC)=Z(KC)-0.5*DZC(KC)  
      IF(ISRESTI.EQ.0)THEN  
        DO K=0,KC  
          DO L=1,LC  
            DML(L,K)=VKC*Z(K)*(1.-Z(K))  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  INITIALIZE UNSTRETCHING PROCEDURE  
C  
      
      DZPC=(SELVMAX-BELVMIN)/FLOAT(KPC)  
      IF(DZPC.NE.0.)THEN
        ZP(0)=BELVMIN  
        DO KP=1,KPC  
          ZP(KP)=ZP(KP-1)+DZPC  
        ENDDO  
        DO KP=1,KPC  
          ZZP(KP)=0.5*(ZP(KP)+ZP(KP-1))  
        ENDDO  
        DO L=2,LA  
          TMP=(BELV(L)-BELVMIN)/DZPC  
          KPB(L)=NINT(0.5+TMP)  
        ENDDO  
      ENDIF
C  
C **  CALCULATE CONSTANT HORIZONTAL SPATIAL ARRAYS  
C  
      DO L=2,LA  
        DXYU(L)=DXU(L)*DYU(L)  
        DXYV(L)=DXV(L)*DYV(L)  
        DXYP(L)=STCAP(L)*DXP(L)*DYP(L)  
        DXIU(L)=1./DXU(L)  
        DYIU(L)=1./DYU(L)  
        DXIV(L)=1./DXV(L)  
        DYIV(L)=1./DYV(L)  
        DXYIP(L)=1./(STCAP(L)*DXP(L)*DYP(L))  
        DXYIU(L)=1./(DXU(L)*DYU(L))  
        DXYIV(L)=1./(DXV(L)*DYV(L))  
        HRU(L)=SUB(L)*HMU(L)*DYU(L)*DXIU(L)  
        HRV(L)=SVB(L)*HMV(L)*DXV(L)*DYIV(L)  
        HRUO(L)=SUBO(L)*DYU(L)*DXIU(L)  
        HRVO(L)=SVBO(L)*DXV(L)*DYIV(L)
        !HRXYU(L)=DXU(L)/DYU(L)    ! PMC - NOT USED
        !HRXYV(L)=DXV(L)/DYV(L)    ! PMC - NOT USED
C        SBX(L)=0.5*SBX(L)*DYU(L)  ! TT ORIGINAL
C        SBY(L)=0.5*SBY(L)*DXV(L)  ! TT ORIGINAL
        SBX(L)=0.5*SUB(L)*DYU(L)  !PMC 
        SBY(L)=0.5*SVB(L)*DXV(L)  !PMC
        SBXO(L)=SBX(L)  
        SBYO(L)=SBY(L)  
        SNLPX(L)=GID2*SNLPX(L)*DYU(L)  
        SNLPY(L)=GID2*SNLPY(L)*DXV(L)  
      ENDDO  

      ! *** DSLLC BEGIN SEEPAGE
      IF(ISGWIT.EQ.3)THEN
        DO L=2,LA
          RIFTR(L)=RIFTR(L)*DXYP(L)  ! M^3/S
        ENDDO
      ENDIF
      ! *** DSLLC END
C  
C **  DEACTIVATE DRY CELLS  
C  
 6902 FORMAT('  DRYING AT N,I,J =',I10,2I6,'  H,H1,H2 =',3(2X,E12.4))  
C  
C **  INITIALIZE ZERO DIMENSION VOLUME BALANCE  
C  
      IF(ISDRY.GE.1.AND.ISDRY.LE.98)THEN  
        OPEN(1,FILE='ZVOLBAL.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='AVSEL.OUT',STATUS='UNKNOWN')  
        LPBTMP=0  
        DO L=2,LA  
          TVAR3C(L)=0  
          IF(SPB(L).EQ.0)THEN  
            LPBTMP=LPBTMP+1  
            TVAR3C(L)=1  
          ENDIF  
          LORDER(L)=0  
        ENDDO  
        TVAR3C(1)=1  
        TVAR3C(LC)=1  
        LORMAX=LC-2-LPBTMP  
        DO LS=1,LORMAX  
          BELMIN=100000.  
          DO L=2,LA  
            IF(SPB(L).NE.0.AND.TVAR3C(L).NE.1)THEN  
              IF(BELV(L).LT.BELMIN)THEN  
                LBELMIN=L  
                BELMIN=BELV(L)  
              ENDIF  
            ENDIF  
          ENDDO  
          LORDER(LS)=LBELMIN  
          TVAR3C(LBELMIN)=1  
        ENDDO  
        WRITE(1,5300)  
        LS=1  
        L=LORDER(LS)  
        BELSURF(LS)=BELV(L)  
        ASURFEL(LS)=DXYP(L)  
        VOLSEL(LS)=0.  
        WRITE(1,5301)LS,BELSURF(LS),ASURFEL(LS),VOLSEL(LS)  
        DO LS=2,LORMAX  
          L=LORDER(LS)  
          BELSURF(LS)=BELV(L)  
          ASURFEL(LS)=ASURFEL(LS-1)+DXYP(L)  
          VOLSEL(LS)=VOLSEL(LS-1)+0.5*(BELSURF(LS)-BELSURF(LS-1))*  
     &        (ASURFEL(LS)+ASURFEL(LS-1))  
          WRITE(1,5301)LS,BELSURF(LS),ASURFEL(LS),VOLSEL(LS)  
        ENDDO  
        LS=LORMAX+1  
        BELSURF(LS)=BELV(L)+10.0  
        ASURFEL(LS)=ASURFEL(LS-1)  
        VOLSEL(LS)=VOLSEL(LS-1)+0.5*(BELSURF(LS)-BELSURF(LS-1))*  
     &      (ASURFEL(LS)+ASURFEL(LS-1))  
        WRITE(1,5301)LS,BELSURF(LS),ASURFEL(LS),VOLSEL(LS)  
        VOLZERD=0.  
        VOLLDRY=0.  
        DO L=2,LA  
          IF(SPB(L).NE.0)THEN  
            VOLZERD=VOLZERD+DXYP(L)*HP(L)  
            IF(HP(L).GT.HDRY) VOLLDRY=VOLLDRY+DXYP(L)*HP(L)  
          ENDIF  
        ENDDO  
        DO LS=1,LORMAX  
          IF(VOLZERD.GE.VOLSEL(LS).AND.VOLZERD.LT.VOLSEL(LS+1))THEN  
            WTM=VOLSEL(LS+1)-VOLZERD  
            WTMP=VOLZERD-VOLSEL(LS)  
            DELVOL=VOLSEL(LS+1)-VOLSEL(LS)  
            WTM=WTM/DELVOL  
            WTMP=WTMP/DELVOL  
            SELZERD=WTM*BELSURF(LS)+WTMP*BELSURF(LS+1)  
            ASFZERD=WTM*ASURFEL(LS)+WTMP*ASURFEL(LS+1)  
          ENDIF  
        ENDDO  
        VETZERD=VOLZERD  
        WRITE(1,5302)  
        WRITE(1,5303) SELZERD,ASFZERD,VOLZERD,VOLLDRY  
        CLOSE(1)  
      ENDIF  
 5300 FORMAT('   M    BELSURF     ASURFEL     ',  
     &    '   VOLSEL',/)  
 5301 FORMAT(1X,I3,2X,F10.5,2X,E12.4,2X,E12.4)  
 5302 FORMAT(/)  
 5303 FORMAT(2X,F10.5,3(2X,E12.4))  
C  
C **  INITIALIZE ELEVATION OF ACTIVE GROUNDWATER ZONE FOR COLD START  
C  
      IF(ISGWIE.GE.1.AND.ISRESTI.EQ.0)THEN  
        DO L=2,LA  
          IF(HP(L).GT.HDRY)THEN  
            AGWELV(L)=BELV(L)  
          ELSE  
            IF(BELAGW(L).LT.SELZERD)THEN  
              AGWELV(L)=SELZERD  
              AGWELV(L)=MIN(AGWELV(L),BELV(L))  
            ELSE  
              AGWELV(L)=BELAGW(L)  
            ENDIF  
          ENDIF  
        ENDDO  
        DO L=2,LA  
          AGWELV1(L)=AGWELV(L)  
          AGWELV2(L)=AGWELV(L)  
        ENDDO  
        OPEN(1,FILE='GWELV.OUT',STATUS='UNKNOWN')  
        WRITE(1,5400)  
        WRITE(1,5402)  
        DO L=2,LA  
          WRITE(1,5401)IL(L),JL(L),BELV(L),BELAGW(L),AGWELV(L)  
        ENDDO  
        CLOSE(1)  
      ENDIF  
 5400 FORMAT('   I   J    BELELV      BELAGW     ',  
     &    '   AGWELV',/)  
 5401 FORMAT(1X,2I5,2X,F10.5,2X,F10.5,2X,F10.5)  
 5402 FORMAT(/)  
C  
C **  CALCULATE CONSTANT C ARRAYS FOR EXTERNAL P SOLUTION  
C **  HRU=SUB*HMU*DYU/DXU & HRV=SVB*HMV*DXV/DYV  
C **  DXYIP=1/(DXP*DYP)  
C  
      IF(IRVEC.NE.9)THEN  
        DO L=2,LA  
          CC(L)=1.  
          CCC(L)=1.  
        ENDDO  
        IF(ISRLID.EQ.1)THEN  
          DO L=2,LA  
            CC(L)=0.  
            CCC(L)=0.  
            IF(SPB(L).EQ.0.) CC(L)=1.  
            IF(SPB(L).EQ.0.) CCC(L)=1.  
          ENDDO  
        ENDIF  
        DO L=2,LA  
          LN=LNC(L)  
          C1=-G*DT*DT*SPB(L)*DXYIP(L)  
          CS(L)=C1*HRV(L)  
          CW(L)=C1*HRU(L)  
          CE(L)=C1*HRU(L+1)  
          CN(L)=C1*HRV(LN)  
          CC(L)=CC(L)-CS(L)-CW(L)-CE(L)-CN(L)  
          CCI(L)=1./CC(L)  
          CCS(L)=0.25*CS(L)  
          CCW(L)=0.25*CW(L)  
          CCE(L)=0.25*CE(L)  
          CCN(L)=0.25*CN(L)  
          CCC(L)=CCC(L)-CCS(L)-CCW(L)-CCE(L)-CCN(L)  
          CCCI(L)=1./CCC(L)  
        ENDDO  
        !DO LR=1,NRC  
          !L=LRC(LR)  
          !CCSR(LR)=CCS(L)*CCCI(L)    ! PMC - NOT USED
          !CCWR(LR)=CCW(L)*CCCI(L)    ! PMC - NOT USED
          !CCER(LR)=CCE(L)*CCCI(L)    ! PMC - NOT USED
          !CCNR(LR)=CCN(L)*CCCI(L)    ! PMC - NOT USED
          !CSR(LR)=CS(L)*CCI(L)       ! PMC - NOT USED
          !CWR(LR)=CW(L)*CCI(L)       ! PMC - NOT USED
          !CER(LR)=CE(L)*CCI(L)       ! PMC - NOT USED
          !CNR(LR)=CN(L)*CCI(L)       ! PMC - NOT USED
        !ENDDO  
        !DO LB=1,NBC  
          !L=LBC(LB)                   ! PMC - NOT USED
          !CCSB(LB)=CCS(L)*CCCI(L)     ! PMC - NOT USED
          !CCWB(LB)=CCW(L)*CCCI(L)     ! PMC - NOT USED
          !CCEB(LB)=CCE(L)*CCCI(L)     ! PMC - NOT USED
          !CCNB(LB)=CCN(L)*CCCI(L)     ! PMC - NOT USED
          !CSB(LB)=CS(L)*CCI(L)        ! PMC - NOT USED
          !CWB(LB)=CW(L)*CCI(L)        ! PMC - NOT USED
          !CEB(LB)=CE(L)*CCI(L)        ! PMC - NOT USED
          !CNB(LB)=CN(L)*CCI(L)        ! PMC - NOT USED
        !ENDDO  
      ENDIF  
C  
C **  SMOOTH INITIAL SALINITY  
C  
      IF(NSBMAX.GE.1)THEN  
        CALL SALTSMTH_mpi
      ENDIF  
C  
C **  OUTPUT INITIAL DEPTH AND SALINITY FIELDS  
C **  PLOT SMOOTHED CELL CENTER STATIC DEPTHS  
C  
      DO L=2,LA  
        PAM(L)=HMP(L)  
      ENDDO  
      WRITE (7,16)  
      CALL PPLOT (2)  
      IF(DEBUG)CALL DEPPLT  
C  
C **  PLOT INITIAL SALINITY IN SURFACE AND BOTTOM LAYERS  
C  
      DO KK=1,KC,KS  
        DO L=2,LA  
          PAM(L)=SAL(L,KK)  
        ENDDO  
        WRITE (7,316) KK  
        CALL PPLOT (1)  
      ENDDO  
   16 FORMAT (1H1,' CELL CENTER STATIC DEPTHS',//)  
  316 FORMAT (1H1,'INITIAL SALINITY IN LAYER',I5,//)  
C  
C **  INITIALIZE SALINITY AND TEMPATURE DATA ASSIMILATION  
C  
      DO J=1,NLDAM  
        DO I=1,NDDAM  
          FSALASM(I,J)=0.0  
          FVOLASM(I,J)=0.0  
          FTEMASM(I,J)=0.0  
        ENDDO  
      ENDDO  
C  
C **  INITIALIZE WATER QUALITY MODEL AND READ INPUT  
C  
      IF(ISTRAN(8).GE.1) CALL WQ3DINP  
C  
C **  INITIALIZE EFDC EXPLORER OUTPUT  
C  
      IF(IBIN_TYPE.EQ.1)THEN
          IF(ISSPH(8).EQ.1.OR.ISBEXP.EQ.1) CALL EEXPOUT_mpi(1)  
      ELSEIF(IBIN_TYPE.EQ.0)THEN
          IF(ISSPH(8).EQ.0.OR.ISBEXP.EQ.1) CALL EEXPOUT_opt_mpi(1)
      ENDIF
! { GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2010.11.10
C **  INITIALIZE EFDC HYDRO DISTRIBUTION OUTPUT  
!     IF(ISRESTO.LT.-20)THEN
!       CALL RESTOUT(-20)
!     ENDIF
! } GEOSR WRITE HYDRO FIELD FOR WQ ALONE : JGCHO 2010.11.10

      END SUBROUTINE model_init_3
