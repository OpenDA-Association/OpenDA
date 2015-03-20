      SUBROUTINE RELAX2T  
C  
C CHANGE RECORD  
C  ADDED THIS SUBROUTINE RELAX2T  
C **  SUBROUTINE RELAX SOLVES THE FINITE DIFFERENCE FORM  
C **  OF A PSEUDO HEMHOLTZ EQUATION  
C **  
C **              CS(L)*P(LS)+CW(L)*P(L-1)  
C **              +CC(L)*P(L)+CE(L)*P(L+1)  
C **  
C **  BY SUCCESSIVE OVER RELAXATION USING A RED-BLACK ORDERING  
C **  WITH CONVERGENCE MEASURED BY A GLOBAL SQUARE ERROR RSQ.  
C **  NON-CONVERGENCE IS SIGNALED WHEN THE ITERATIONS EXCEED A  
C **  MAXIMUM.  
C  
      USE GLOBAL  
      RJ2=RP  
C  
C      PAVG=0.0  
C  
      FPSQ=0.  
      DO L=2,LA  
        FPSQ=FPSQ+FPTMP(L)*FPTMP(L)  
      ENDDO  
      ITER=1  
  200 CONTINUE  
      RSQ=0.  
C  
C **  RED CELL LOOP  
C  
      IF(ITER.EQ.1) RPT=1.0  
      IF(ITER.GT.1) RPT=1.0/(1.0-0.25*RJ2*RPT)  
      DO L=2,LA  
        K=IL(L)+JL(L)  
        IVAL=MOD(K,2)  
        IF(IVAL.EQ.0)THEN  
          LN=LNC(L)  
          LS=LSC(L)  
          RSD=CCC(L)*P(L)+CCS(L)*P(LS)+CCW(L)*P(L-1)+CCE(L)*P(L+1)  
     &        +CCN(L)*P(LN)-FPTMP(L)  
          P(L)=P(L)-RPT*RSD/CCC(L)  
          RSQ=RSQ+RSD*RSD  
        ENDIF  
      ENDDO  
C  
C **  BLACK CELL LOOP  
C  
      IF(ITER.EQ.1) RPT=1.0/(1.0-0.5*RJ2)  
      IF(ITER.GT.1) RPT=1.0/(1.0-0.25*RJ2*RPT)  
      DO L=2,LA  
        K=IL(L)+JL(L)  
        IVAL=MOD(K,2)  
        IF(IVAL.NE.0)THEN  
          LN=LNC(L)  
          LS=LSC(L)  
          RSD=CCC(L)*P(L)+CCS(L)*P(LS)+CCW(L)*P(L-1)+CCE(L)*P(L+1)  
     &        +CCN(L)*P(LN)-FPTMP(L)  
          P(L)=P(L)-RPT*RSD/CCC(L)  
          RSQ=RSQ+RSD*RSD  
        ENDIF  
      ENDDO  
C  
C **  CHECK SQUARED RESIDUAL CONVERGENCE CRITERIA  
C  
      RSQ=SQRT(RSQ)/SQRT(FPSQ)  
      IF(RSQ .LE. RSQM) GOTO 400  
C  
C **  CHECK MAXIMUM ITERATION CRITERIA  
C  
      IF(ITER .GE. ITERM)THEN  
        WRITE(6,600)  
        WRITE(6,601)RSQ  
        WRITE(8,600)  
        WRITE(8,601)RSQ  
        DO L=2,LA  
          WRITE(8,800)L,CCS(L),CCW(L),CCC(L),CCE(L),CCN(L),P(L),FPTMP(L)  
        ENDDO  
        STOP  
      ENDIF  
      ITER=ITER+1  
      GOTO 200  
  400 CONTINUE  
  600 FORMAT(' MAX ITERATIONS EXCEEDED IN EXTERNAL SOLUTION, RELAX2T')  
  601 FORMAT(' RSQ = ',E14.5)  
  800 FORMAT(I6,7E13.5)  
      RETURN  
      END  

