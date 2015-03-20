      SUBROUTINE BUDGET1  
C  
C **  ADDED BY DON KINGERY, CH2M-HILL ON 15 OCTOBER 1996  
C CHANGE RECORD  
C **  SUBROUTINES BUDGETN CALCULATE SEDIMENT BUDGET (TOTAL SEDIMENTS)  
C  
      USE GLOBAL  
	IMPLICIT NONE
	INTEGER::K,L,NS
      IF(NBUD.GT.1) RETURN  
C  
C **  INITIALIZE VOLUME, SALT MASS, SEDIMENT, AND ASSOCIATED FLUXES  
C  
      VOLMBEG=0.  
      SMASSBEG=0.  
      BSEDBEG=0.  
      SSEDBEG=0.  
      SEDIN=0.  
      SEDOUT=0.  
      VOLMOUT=0.  
      SMASSOUT=0.  
      VOLMIN=0.  
      SMASSIN=0.  
      DO K=1,KB  
        DO L=2,LA  
          SEDBT(L,K)=0.  
          SNDBT(L,K)=0.  
        ENDDO  
      ENDDO  
      IF(N.LE.1)THEN  
        DO NS=1,NSED  
          DO K=1,KB  
            DO L=2,LA  
              SEDBT(L,K)=SEDBT(L,K)+SEDB1(L,K,NS)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NS=1,NSND  
          DO K=1,KB  
            DO L=2,LA  
              SNDBT(L,K)=SNDBT(L,K)+SNDB1(L,K,NS)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(N.GT.1)THEN  
        DO NS=1,NSED  
          DO K=1,KB  
            DO L=2,LA  
              SEDBT(L,K)=SEDBT(L,K)+SEDB(L,K,NS)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NS=1,NSND  
          DO K=1,KB  
            DO L=2,LA  
              SNDBT(L,K)=SNDBT(L,K)+SNDB(L,K,NS)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C!VOLBW3 IS TO ACCUMULATE WC-SEDBED FLUX  
C  
      DO K=1,KB  
        DO L=2,LA  
          VOLBW3(L,K)=0.  
          BSEDBEG=BSEDBEG+SCB(L)*DXYP(L)*(SEDBT(L,K)+SNDBT(L,K))  
        ENDDO  
      ENDDO  
      IF(N.LE.1)THEN  
        DO NS=1,NSED  
          DO K=1,KC  
            DO L=2,LA  
              SSEDBEG=SSEDBEG+SCB(L)*DXYP(L)*H1P(L)*SED1(L,K,NS)*DZC(K)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NS=1,NSND  
          DO K=1,KC  
            DO L=2,LA  
              SSEDBEG=SSEDBEG+SCB(L)*DXYP(L)*H1P(L)*SND1(L,K,NS)*DZC(K)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(N.GT.1)THEN  
        DO NS=1,NSED  
          DO K=1,KC  
            DO L=2,LA  
              SSEDBEG=SSEDBEG+SCB(L)*DXYP(L)*HP(L)*SED(L,K,NS)*DZC(K)  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NS=1,NSND  
          DO K=1,KC  
            DO L=2,LA  
              SSEDBEG=SSEDBEG+SCB(L)*DXYP(L)*HP(L)*SND(L,K,NS)*DZC(K)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
      SEDBEG=BSEDBEG+SSEDBEG  
      IF(N.LE.1)THEN  
        DO L=2,LA  
          VOLMBEG=VOLMBEG+SPB(L)*DXYP(L)*H1P(L)  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
            SMASSBEG=SMASSBEG+SCB(L)*DXYP(L)*H1P(L)*SAL1(L,K)*DZC(K)  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(N.GT.1)THEN  
        DO L=2,LA  
          VOLMBEG=VOLMBEG+SPB(L)*DXYP(L)*HP(L)  
        ENDDO  
        DO K=1,KC  
          DO L=2,LA  
            SMASSBEG=SMASSBEG+SCB(L)*DXYP(L)*HP(L)*SAL(L,K)*DZC(K)  
          ENDDO  
        ENDDO  
      ENDIF  
      RETURN  
      END  

