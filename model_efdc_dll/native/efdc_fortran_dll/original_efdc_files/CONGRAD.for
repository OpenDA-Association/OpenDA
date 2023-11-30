      SUBROUTINE CONGRAD (ISTL_)  
C  
C CHANGE RECORD  
C **  SUBROUTINE CONGRAD SOLVES THE EXTERNAL MODE BY A CONJUGATE  
C **  GRADIENT SCHEME  
C  
      USE GLOBAL 

      REAL TTMP, SECNDS
 
      ! *** DSLLC
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::PNORTH  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::PSOUTH  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TMPCG  
      IF(.NOT.ALLOCATED(PNORTH))THEN
        ALLOCATE(PNORTH(LCM))
        ALLOCATE(PSOUTH(LCM))
		ALLOCATE(TMPCG(LCM))
        PNORTH=0.0 
        PSOUTH=0.0 
		TMPCG=0.0 
	ENDIF
      ! *** DSLLC
C  
      TTMP=SECOND()  
      DO L=2,LA
        PNORTH(L)=P(LNC(L))
        PSOUTH(L)=P(LSC(L))
      ENDDO
      DO L=2,LA
        RCG(L)=FPTMP(L)-CCC(L)*P(L)-CCN(L)*PNORTH(L)-CCS(L)*PSOUTH(L)  
     &      -CCW(L)*P(L-1)-CCE(L)*P(L+1)  
      ENDDO
      DO L=2,LA
        PCG(L)=RCG(L)*CCCI(L)  
      ENDDO
      RPCG=0.0
      DO L=2,LA
        RPCG=RPCG+RCG(L)*PCG(L)  
      ENDDO 
      IF(RPCG.EQ.0.0)RETURN   ! *** DSLLC SINGLE LINE
      ITER=0  
  100 CONTINUE  
      ITER=ITER+1  
      DO L=2,LA
        PNORTH(L)=PCG(LNC(L))
        PSOUTH(L)=PCG(LSC(L))
      ENDDO
      DO L=2,LA
        APCG(L)=CCC(L)*PCG(L)+CCS(L)*PSOUTH(L)+CCN(L)*PNORTH(L)  
     &      +CCW(L)*PCG(L-1)+CCE(L)*PCG(L+1)
      ENDDO
      PAPCG=0.0
      DO L=2,LA
        PAPCG=PAPCG+APCG(L)*PCG(L)  
      ENDDO  
      ALPHA=RPCG/PAPCG  
      DO L=2,LA
        P(L)=P(L)+ALPHA*PCG(L)  
      ENDDO  
      DO L=2,LA
        RCG(L)=RCG(L)-ALPHA*APCG(L)  
      ENDDO  
      DO L=2,LA
        TMPCG(L)=CCCI(L)*RCG(L)  
      ENDDO  
      RPCGN=0.  
      RSQ=0.  
      DO L=2,LA
        RPCGN=RPCGN+RCG(L)*TMPCG(L)  
        RSQ=RSQ+RCG(L)*RCG(L)  
      ENDDO  
      IF(RSQ .LE. RSQM) GOTO 200  
      IF(ITER .GE. ITERM)THEN  
        WRITE(6,600)  
C  
C *** PMC BEGIN BLOCK  
C  
      WRITE(8,*)'  I    J       CCS          CCW          CCC  
     &    CCE          CCN        CDIADOM       FPTMP         HU  
     &    HV'  
C  
C *** PMC END BLOCK  
C  
        DO L=1,LC  
          CDIADOM=CCC(L)+CCE(L)+CCN(L)+CCS(L)+CCW(L)  
          WRITE(8,808)IL(L),JL(L),CCS(L),CCW(L),CCC(L),CCE(L),CCN(L),  
     &        CDIADOM,FPTMP(L),HU(L),HV(L)  
        END DO  
        CLOSE(8)  
        STOP  
      ENDIF  
      BETA=RPCGN/RPCG  
      RPCG=RPCGN  
      DO L=2,LA  
        PCG(L)=TMPCG(L)+BETA*PCG(L)  
      ENDDO  
      GOTO 100  
  600 FORMAT('  MAXIMUM ITERATIONS EXCEEDED IN EXTERNAL SOLUTION')  
C  
C ** CALCULATE FINAL RESIDUAL  
C  
  200 CONTINUE
      ! *** DSLLC BEGIN BLOCK
      IF(ISLOG.GE.1)THEN  
        DO L=2,LA  
          PNORTH(L)=P(LNC(L))  
          PSOUTH(L)=P(LSC(L))  
        ENDDO  
        RSQ=0.  
        DO L=2,LA  
          RCG(L)=CCC(L)*P(L)+CCS(L)*PSOUTH(L)+CCN(L)*PNORTH(L)  
     &      +CCW(L)*P(L-1)+CCE(L)*P(L+1)-FPTMP(L)  
        ENDDO  
        DO L=2,LA  
          RCG(L)=RCG(L)*CCCI(L)  
        ENDDO  
        DO L=2,LA  
          RSQ=RSQ+RCG(L)*RCG(L)  
        ENDDO  
      ENDIF
      ! *** DSLLC END BLOCK
      TCONG=TCONG+TTMP-SECOND()  
C 800 FORMAT(I5,8E13.4)  
  808 FORMAT(2I5,9E13.4)  
      RETURN  
      END  

