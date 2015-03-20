      SUBROUTINE SALTSMTH  
C  
C CHANGE RECORD  
C  
      USE GLOBAL  
      IF(NSBMAX.GT.10)  GOTO 1001  
C  
C      ELSE  
C       GOTO 1001  
C  
      DO K=1,KC  
        DO L=2,LA  
          TVAR3S(L)=SAL(L,K)  
        ENDDO  
        DO NSM=1,NSBMAX  
          DO L=2,LA  
            IF(LCT(L).GT.0.AND.LCT(L).LT.9)THEN  
              I=IL(L)  
              J=JL(L)  
              HTN=TVAR3S(LNC(L))  
              HTS=TVAR3S(LSC(L))  
              HTE=TVAR3S(L+1)  
              HTW=TVAR3S(L-1)  
              IF(IJCT(I  ,J+1).EQ.9) HTN=TVAR3S(L)  
              IF(IJCT(I  ,J-1).EQ.9) HTS=TVAR3S(L)  
              IF(IJCT(I+1,J  ).EQ.9) HTE=TVAR3S(L)  
              IF(IJCT(I-1,J  ).EQ.9) HTW=TVAR3S(L)  
              TVAR3N(L)=(1.-WSMB)*TVAR3S(L)+0.25*WSMB*(HTN+HTS+HTE+HTW)  
            ENDIF  
          ENDDO  
          DO L=2,LA  
            TVAR3S(L)=TVAR3N(L)  
          ENDDO  
        ENDDO  
        DO L=2,LA  
          SAL(L,K)=TVAR3N(L)  
          SAL1(L,K)=TVAR3N(L)  
        ENDDO  
      ENDDO  
      GOTO 2000  
C  
C **  IMPLEMENT SPECIAL SALINITY INITIALIZATION, VERSION 1  
C 1000 CONTINUE  
C **  IMPLEMENT SPECIAL SALINITY INITIALIZATION, VERSION 2  
C  
 1001 CONTINUE  
      DO K=1,KC  
        DO L=2,LA  
          TVAR3S(L)=SAL(L,K)  
        ENDDO  
        DO NSM=1,NSBMAX  
          DO L=2,LA  
            LN=LNC(L)  
            LS=LSC(L)  
            TVAR3N(L)=TVAR3S(L)+(WSMB/HMP(L))  
     &          *( HRU(L+1)*(TVAR3S(L+1)-TVAR3S(L  ))  
     &          -HRU(L  )*(TVAR3S(L  )-TVAR3S(L-1))  
     &          +HRV(LN )*(TVAR3S(LN )-TVAR3S(L  ))  
     &          -HRV(L  )*(TVAR3S(L  )-TVAR3S(LS )) )  
          ENDDO  
          DO L=2,LA  
            IF(SALINIT(L,K).GT.0.0) TVAR3N(L)=SALINIT(L,K)  
          ENDDO  
          DO L=2,LA  
            TVAR3S(L)=TVAR3N(L)  
          ENDDO  
        ENDDO  
        DO L=2,LA  
          SAL(L,K)=TVAR3N(L)  
          SAL1(L,K)=TVAR3N(L)  
        ENDDO  
        WRITE(6,6001)K,NSM  
      ENDDO  
      OPEN(1,FILE='NEWSALT.INP',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='NEWSALT.INP',STATUS='UNKNOWN')  
      IONE=1  
      WRITE(1,9101)IONE  
      DO L=2,LC-1  
        WRITE(1,9102)L,IL(L),JL(L),(SAL(L,K),K=1,KC)  
      ENDDO  
      CLOSE(1)  
 6000 FORMAT(' COMPLE V1 SMOOTHING LAYER ',I5,' NSM = ',I5/)  
 6001 FORMAT(' COMPLE V2 SMOOTHING LAYER ',I5,' NSM = ',I5/)  
 9101 FORMAT(I5)  
 9102 FORMAT(3I5,12F6.2)  
 2000 CONTINUE  
      RETURN  
      END  

