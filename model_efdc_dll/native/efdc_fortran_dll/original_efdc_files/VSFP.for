      SUBROUTINE VSFP  
C  
C CHANGE RECORD  
C **  SUBROUTINES VSFP WRITES INSTANTANEOUS VERTICAL SCALAR FIELD  
C **  PROFILES AT SPECIFIED HORIZONTAL SPACE-TIME LOCATIONS TO  
C **  FILE VSFP.OUT  
C  
      USE GLOBAL  

      REAL,ALLOCATABLE,DIMENSION(:)::DABVBT  
      REAL,ALLOCATABLE,DIMENSION(:)::DBLWSF  
      REAL,ALLOCATABLE,DIMENSION(:)::DYEOT  
      REAL,ALLOCATABLE,DIMENSION(:)::DYEVS  
      REAL,ALLOCATABLE,DIMENSION(:)::SALOT  
      REAL,ALLOCATABLE,DIMENSION(:)::SALVS  
      REAL,ALLOCATABLE,DIMENSION(:)::SFLOT  
      REAL,ALLOCATABLE,DIMENSION(:)::SFLVS  
      REAL,ALLOCATABLE,DIMENSION(:)::TEMOT  
      REAL,ALLOCATABLE,DIMENSION(:)::TEMVS  
      REAL,ALLOCATABLE,DIMENSION(:,:)::SEDOT  
      REAL,ALLOCATABLE,DIMENSION(:,:)::SEDVS  
      REAL,ALLOCATABLE,DIMENSION(:,:)::SNDOT  
      REAL,ALLOCATABLE,DIMENSION(:,:)::SNDVS  
      REAL,ALLOCATABLE,DIMENSION(:,:)::TOXOT  
      REAL,ALLOCATABLE,DIMENSION(:,:)::TOXVS  

      ALLOCATE(DABVBT(KCM))  
      ALLOCATE(DBLWSF(KCM))  
      ALLOCATE(DYEOT(MDVSM))  
      ALLOCATE(DYEVS(KCM))  
      ALLOCATE(SALOT(MDVSM))  
      ALLOCATE(SALVS(KCM))  
      ALLOCATE(SEDOT(MDVSM,NSCM))  
      ALLOCATE(SEDVS(KCM,NSCM))  
      ALLOCATE(SFLOT(MDVSM))  
      ALLOCATE(SFLVS(KCM))  
      ALLOCATE(SNDOT(MDVSM,NSNM))  
      ALLOCATE(SNDVS(KCM,NSNM))  
      ALLOCATE(TEMOT(MDVSM))  
      ALLOCATE(TEMVS(KCM))  
      ALLOCATE(TOXOT(MDVSM,NTXM))  
      ALLOCATE(TOXVS(KCM,NTXM))
  
      ! *** INTIALIZE LOCAL ARRAYS
      DABVBT=0.
      DBLWSF=0.
      DYEOT=0.
      DYEVS=0.
      SALOT=0.
      SALVS=0.
      SEDOT=0.
      SEDVS=0.
      SFLOT=0.
      SFLVS=0.
      SNDOT=0.
      SNDVS=0.
      TEMOT=0.
      TEMVS=0.
      TOXOT=0.
      TOXVS=0.
C  
      IF(JSVSFP.EQ.0) GOTO 100  
      OPEN(1,FILE='VSFP1.OUT')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='VSFP1.OUT')  
      CLOSE(1)  
      OPEN(1,FILE='VSFP2.OUT')  
      CLOSE(1,STATUS='DELETE')  
      JSVSFP=0  
  100 CONTINUE  
      DO ML=1,MLVSFP  
        IF(NTVSFP(ML).EQ.N)THEN  
          MDMAX=1  
          I=IVSFP(ML)  
          J=JVSFP(ML)  
          L=LIJ(I,J)  
          DABVBT(1)=0.5*HP(L)*DZC(1)  
          DO K=2,KC  
            DABVBT(K)=DABVBT(K-1)+0.5*HP(L)*(DZC(K)+DZC(K-1))  
          ENDDO  
          DO K=1,KC  
            DBLWSF(K)=HP(L)-DABVBT(K)  
          ENDDO  
          DO K=1,KC  
            SALVS(K)=SAL(L,K)  
            TEMVS(K)=TEM(L,K)  
            DYEVS(K)=DYE(L,K)  
            SFLVS(K)=SFL(L,K)  
          ENDDO  
          DO NT=1,NTOX  
            DO K=1,KC  
              TOXVS(K,NT)=TOX(L,K,NT)  
            ENDDO  
          ENDDO  
          DO NS=1,NSED  
            DO K=1,KC  
              SEDVS(K,NS)=SED(L,K,NS)  
            ENDDO  
          ENDDO  
          DO NX=1,NSND  
            DO K=1,KC  
              SNDVS(K,NX)=SND(L,K,NX)  
            ENDDO  
          ENDDO  
          DO MD=1,MDVSFP  
            IF(DMVSFP(MD).LT.HP(L)) MDMAX=MD  
          ENDDO  
          DO MD=1,MDMAX  
            ZZSVSFP=(HP(L)-DMVSFP(MD))*HPI(L)  
            IF(ZZSVSFP.GE.0.0.AND.ZZSVSFP.LE.1.0)THEN  
              IF(ZZSVSFP.GE.ZZ(KC))THEN  
                SOUTMPU= (ZZSVSFP-ZZ(KS))/(ZZ(KC)-ZZ(KS))  
                SOUTMPB=-(ZZSVSFP-ZZ(KC))/(ZZ(KC)-ZZ(KS))  
                SALOT(MD)=SOUTMPU*SALVS(KC)+SOUTMPB*SALVS(KS)  
                TEMOT(MD)=SOUTMPU*TEMVS(KC)+SOUTMPB*TEMVS(KS)  
                DYEOT(MD)=SOUTMPU*DYEVS(KC)+SOUTMPB*DYEVS(KS)  
                SFLOT(MD)=SOUTMPU*SFLVS(KC)+SOUTMPB*SFLVS(KS)  
                DO NT=1,NTOX  
                  TOXOT(MD,NT)=SOUTMPU*TOXVS(KC,NT)+SOUTMPB*TOXVS(KS,NT)  
                ENDDO  
                DO NS=1,NSED  
                  SEDOT(MD,NS)=SOUTMPU*SEDVS(KC,NS)+SOUTMPB*SEDVS(KS,NS)  
                ENDDO  
                DO NX=1,NSND  
                  SNDOT(MD,NX)=SOUTMPU*SNDVS(KC,NX)+SOUTMPB*SNDVS(KS,NX)  
                ENDDO  
              ELSE  
                IF(ZZSVSFP.LE.ZZ(1))THEN  
                  SOUTMPU= (ZZSVSFP-ZZ(1))/(ZZ(2)-ZZ(1))  
                  SOUTMPB=-(ZZSVSFP-ZZ(2))/(ZZ(2)-ZZ(1))  
                  SALOT(MD)=SOUTMPU*SALVS(2)+SOUTMPB*SALVS(1)  
                  TEMOT(MD)=SOUTMPU*TEMVS(2)+SOUTMPB*TEMVS(1)  
                  DYEOT(MD)=SOUTMPU*DYEVS(2)+SOUTMPB*DYEVS(1)  
                  SFLOT(MD)=SOUTMPU*SFLVS(2)+SOUTMPB*SFLVS(1)  
                  DO NT=1,NTOX  
                    TOXOT(MD,NT)=SOUTMPU*TOXVS(2,NT)+SOUTMPB*TOXVS(1,NT)  
                  ENDDO  
                  DO NS=1,NSED  
                    SEDOT(MD,NS)=SOUTMPU*SEDVS(2,NS)+SOUTMPB*SEDVS(1,NS)  
                  ENDDO  
                  DO NX=1,NSND  
                    SNDOT(MD,NX)=SOUTMPU*SNDVS(2,NX)+SOUTMPB*SNDVS(1,NX)  
                  ENDDO  
                ELSE  
                  K=1  
  200             K=K+1  
                  IF(ZZSVSFP.GT.ZZ(K-1).AND.ZZSVSFP.LE.ZZ(K))THEN  
                    SOUTMPU= (ZZSVSFP-ZZ(K-1))/(ZZ(K)-ZZ(K-1))  
                    SOUTMPB=-(ZZSVSFP-ZZ(K))/(ZZ(K)-ZZ(K-1))  
                    SALOT(MD)=SOUTMPU*SALVS(K)+SOUTMPB*SALVS(K-1)  
                    TEMOT(MD)=SOUTMPU*TEMVS(K)+SOUTMPB*TEMVS(K-1)  
                    DYEOT(MD)=SOUTMPU*DYEVS(K)+SOUTMPB*DYEVS(K-1)  
                    SFLOT(MD)=SOUTMPU*SFLVS(K)+SOUTMPB*SFLVS(K-1)  
                    DO NT=1,NTOX  
                      TOXOT(MD,NT)=SOUTMPU*TOXVS(K,NT)+SOUTMPB*TOXVS(
     &                    K-1,NT)  
                    ENDDO  
                    DO NS=1,NSED  
                      SEDOT(MD,NS)=SOUTMPU*SEDVS(K,NS)+SOUTMPB*SEDVS(
     &                    K-1,NS)  
                    ENDDO  
                    DO NX=1,NSND  
                      SNDOT(MD,NX)=SOUTMPU*SNDVS(K,NX)+SOUTMPB*SNDVS(
     &                    K-1,NX)  
                    ENDDO  
                  ELSE  
                    GOTO 200  
                  ENDIF  
                ENDIF  
              ENDIF  
            ENDIF  
          ENDDO  
          OPEN(1,FILE='VSFP1.OUT',POSITION='APPEND')  
          WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
          WRITE(1,104)  
          DO MD=1,MDMAX  
          WRITE(1,105)DMVSFP(MD),SALOT(MD),TEMOT(MD),DYEOT(MD),SFLOT(MD)  
          ENDDO  
          WRITE(1,102)  
          WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
          WRITE(1,114)  
          DO MD=1,MDMAX  
            WRITE(1,105)DMVSFP(MD),(SEDOT(MD,NS),NS=1,NSED),  
     &          (SNDOT(MD,NX),NX=1,NSND)  
          ENDDO  
          WRITE(1,102)  
          NTXX=MIN(NTOX,6)  
          WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
          WRITE(1,124)  
          DO MD=1,MDMAX  
            WRITE(1,105)DMVSFP(MD),(TOXOT(MD,NT),NT=1,NTXX)  
          ENDDO  
          WRITE(1,102)  
          IF(NTOX.GT.6)THEN  
            NTXX=MIN(NTOX,12)  
            WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
            WRITE(1,134)  
            DO MD=1,MDMAX  
              WRITE(1,105)DMVSFP(MD),(TOXOT(MD,NT),NT=7,NTXX)  
            ENDDO  
            WRITE(1,102)  
          ENDIF  
          IF(NTOX.GT.12)THEN  
            NTXX=MIN(NTOX,18)  
            WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
            WRITE(1,144)  
            DO MD=1,MDMAX  
              WRITE(1,105)DMVSFP(MD),(TOXOT(MD,NT),NT=13,NTXX)  
            ENDDO  
            WRITE(1,102)  
          ENDIF  
          IF(NTOX.GT.18)THEN  
            NTXX=MIN(NTOX,24)  
            WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
            WRITE(1,154)  
            DO MD=1,MDMAX  
              WRITE(1,105)DMVSFP(MD),(TOXOT(MD,NT),NT=19,NTXX)  
            ENDDO  
            WRITE(1,102)  
          ENDIF  
          CLOSE(1)  
          OPEN(1,FILE='VSFP2.OUT',POSITION='APPEND')  
          WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
          WRITE(1,204)  
          DO K=KC,1,-1  
           WRITE(1,106)K,DBLWSF(K),DABVBT(K),SALVS(K),TEMVS(K),DYEVS(K),  
     &          SFLVS(K)  
          ENDDO  
          WRITE(1,102)  
          WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
          WRITE(1,214)  
          DO K=KC,1,-1  
            WRITE(1,106)K,DBLWSF(K),DABVBT(K),(SEDVS(K,NS),NS=1,NSED),  
     &          (SNDVS(K,NX),NX=1,NSND)  
          ENDDO  
          WRITE(1,102)  
          NTXX=MIN(NTOX,6)  
          WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
          WRITE(1,224)  
          DO K=KC,1,-1  
            WRITE(1,106)K,DBLWSF(K),DABVBT(K),(TOXVS(K,NT),NT=1,NTXX)  
          ENDDO  
          WRITE(1,102)  
          IF(NTOX.GT.6)THEN  
            NTXX=MIN(NTOX,12)  
            WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
            WRITE(1,234)  
            DO K=KC,1,-1  
              WRITE(1,106)K,DBLWSF(K),DABVBT(K),(TOXVS(K,NT),NT=7,NTXX)  
            ENDDO  
            WRITE(1,102)  
          ENDIF  
          IF(NTOX.GT.12)THEN  
            NTXX=MIN(NTOX,18)  
            WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
            WRITE(1,244)  
            DO K=KC,1,-1  
              WRITE(1,106)K,DBLWSF(K),DABVBT(K),(TOXVS(K,NT),NT=13,NTXX)  
            ENDDO  
            WRITE(1,102)  
          ENDIF  
          IF(NTOX.GT.18)THEN  
            NTXX=MIN(NTOX,24)  
            WRITE(1,103)TIMVSFP(ML),NTVSFP(ML),I,J,HP(L)  
            WRITE(1,254)  
            DO K=KC,1,-1  
              WRITE(1,106)K,DBLWSF(K),DABVBT(K),(TOXVS(K,NT),NT=19,NTXX)  
            ENDDO  
            WRITE(1,102)  
          ENDIF  
          CLOSE(1)  
        ENDIF  
      ENDDO  
  101 FORMAT('  INSTANTANEOUS VERTICAL SCALAR FIELD PROFILES')  
  102 FORMAT(/)  
  103 FORMAT('  TIME = ',F12.4,' N = ',I8,'  I,J = ',2I4,  
     &    '  H = ',F10.2)  
  104 FORMAT('  DEP BEL SURF     SAL,TEM,DYE,SFL',/)  
  114 FORMAT('  DEP BEL SURF     SED(N=1,NSED),SND(N=1,NSND)',/)  
  124 FORMAT('  DEP BEL SURF     TOX(N= 1, 6)',/)  
  134 FORMAT('  DEP BEL SURF     TOX(N= 7,12)',/)  
  144 FORMAT('  DEP BEL SURF     TOX(N=13,18)',/)  
  154 FORMAT('  DEP BEL SURF     TOX(N=19,24)',/)  
  204 FORMAT('  LAY     DBS     HAB       SAL,TEM,DYE,SFL',/)  
  214 FORMAT('  LAY     DBS     HAB       ',  
     &    'SED(N=1,NSED),SND(N=1,NSND)',/)  
  224 FORMAT('  LAY     DBS     HAB       TOX(N= 1, 6)',/)  
  234 FORMAT('  LAY     DBS     HAB       TOX(N= 7,12)',/)  
  244 FORMAT('  LAY     DBS     HAB       TOX(N=13,18)',/)  
  254 FORMAT('  LAY     DBS     HAB       TOX(N=19,24)',/)  
  105 FORMAT(1X,F8.2,4X,6F11.4)  
  106 FORMAT(1X,I2,2X,2F8.2,2X,6F11.4)  
      RETURN  
      END  

