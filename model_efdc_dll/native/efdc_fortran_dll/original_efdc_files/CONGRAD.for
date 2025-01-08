      SUBROUTINE CONGRAD (ISTL_)  
C  
C CHANGE RECORD  
C **  SUBROUTINE CONGRAD SOLVES THE EXTERNAL MODE BY A CONJUGATE  
C **  GRADIENT SCHEME  
C  
      USE GLOBAL 

      REAL TTMP,T1TMP
 
      ! *** DSLLC
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TMPCG  
      IF(.NOT.ALLOCATED(TMPCG))THEN
		ALLOCATE(TMPCG(LCM))
		TMPCG=0.0 
	ENDIF
      ! *** DSLLC
C  
      CALL CPU_TIME(TTMP)  
      RPCG=0.  
!$OMP PARALLEL DO PRIVATE(LF,LL) REDUCTION(+:RPCG)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO L=LF,LL
        RCG(L)=FPTMP(L)-CCC(L)*P(L)
     &      -CCN(L)*P(LNC(L))-CCS(L)*P(LSC(L))
     &      -CCW(L)*P(L-1)-CCE(L)*P(L+1)  
        PCG(L)=RCG(L)*CCCI(L)  
        RPCG=RPCG+RCG(L)*PCG(L)  
      ENDDO 

c
      enddo

      IF(RPCG.EQ.0.0)RETURN   ! *** DSLLC SINGLE LINE
      ITER=0  
  100 CONTINUE  
      ITER=ITER+1  
      PAPCG=0.  
!$OMP PARALLEL DO PRIVATE(LF,LL) REDUCTION(+:PAPCG)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO L=LF,LL
        APCG(L)=CCC(L)*PCG(L)
     &      +CCS(L)*PCG(LSC(L))+CCN(L)*PCG(LNC(L))
     &      +CCW(L)*PCG(L-1)+CCE(L)*PCG(L+1)
        PAPCG=PAPCG+APCG(L)*PCG(L)  
      ENDDO  

      enddo

      ALPHA=RPCG/PAPCG  

      RPCGN=0.  
      RSQ=0.  
!$OMP PARALLEL DO PRIVATE(LF,LL) REDUCTION(+:RPCGN,RSQ)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO L=LF,LL
        P(L)=P(L)+ALPHA*PCG(L)  
        RCG(L)=RCG(L)-ALPHA*APCG(L)  
        TMPCG(L)=CCCI(L)*RCG(L)  
        RPCGN=RPCGN+RCG(L)*TMPCG(L)  
        RSQ=RSQ+RCG(L)*RCG(L)  
      ENDDO  
c
      enddo


      IF(RSQ .LE. RSQM) GOTO 200  
      IF(ITER .LT. ITERM)THEN  
      BETA=RPCGN/RPCG  
      RPCG=RPCGN  
!$OMP PARALLEL DO PRIVATE(LF,LL)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
      DO L=LF,LL
        PCG(L)=TMPCG(L)+BETA*PCG(L)  
      ENDDO  
c
      enddo
      GOTO 100  
      ENDIF
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
  600 FORMAT('  MAXIMUM ITERATIONS EXCEEDED IN EXTERNAL SOLUTION')  
C  
C ** CALCULATE FINAL RESIDUAL  
C  
  200 CONTINUE
      ! *** DSLLC BEGIN BLOCK
      IF(ISLOG.GE.1)THEN  
        RSQ=0.  
!$OMP PARALLEL DO PRIVATE(LF,LL) REDUCTION(+:RSQ)
      do ithds=0,nthds-1
         LF=jse(1,ithds)
         LL=jse(2,ithds)
c
        DO L=LF,LL
          RCG(L)=CCC(L)*P(L)
     &      +CCS(L)*P(LSC(L))+CCN(L)*P(LNC(L))
     &      +CCW(L)*P(L-1)+CCE(L)*P(L+1)-FPTMP(L)  
          RCG(L)=RCG(L)*CCCI(L)  
          RSQ=RSQ+RCG(L)*RCG(L)  
        ENDDO  
c
      enddo

      ENDIF
      ! *** DSLLC END BLOCK
      CALL CPU_TIME(T1TMP)
      TCONG=TCONG+T1TMP-TTMP
C 800 FORMAT(I5,8E13.4)  
  808 FORMAT(2I5,9E13.4)  
      RETURN  
      END  

