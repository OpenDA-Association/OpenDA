      SUBROUTINE CONGRADC (ISTL_)  
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
      IF(MDCHH.GE.1)THEN  
        DO NMD=1,MDCHH  
          LHOST=LMDCHH(NMD)  
          LCHNU=LMDCHU(NMD)  
          LCHNV=LMDCHV(NMD)  
C  
C         X-DIRECTION CHANNEL  
C  
          IF(MDCHTYP(NMD).EQ.1)THEN  
            RCG(LCHNU)=RCG(LCHNU)+CCCCHH(NMD)*P(LHOST)  
            RCG(LHOST)=RCG(LHOST)+CCCCHH(NMD)*P(LCHNU)  
          ENDIF  
C  
C         Y-DIRECTION CHANNEL  
C  
          IF(MDCHTYP(NMD).EQ.2)THEN  
            RCG(LCHNV)=RCG(LCHNV)+CCCCHH(NMD)*P(LHOST)  
            RCG(LHOST)=RCG(LHOST)+CCCCHH(NMD)*P(LCHNV)  
          ENDIF  
        ENDDO  
      ENDIF  
      DO L=2,LA  
        PCG(L)=RCG(L)*CCCI(L)  
      ENDDO  
      RPCG=0.  
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
      IF(MDCHH.GE.1)THEN  
        DO NMD=1,MDCHH  
          LHOST=LMDCHH(NMD)  
          LCHNU=LMDCHU(NMD)  
          LCHNV=LMDCHV(NMD)  
C  
C         X-DIRECTION CHANNEL  
C  
          IF(MDCHTYP(NMD).EQ.1)THEN  
            APCG(LCHNU)=APCG(LCHNU)+CCCCHH(NMD)*PCG(LHOST)  
            APCG(LHOST)=APCG(LHOST)+CCCCHH(NMD)*PCG(LCHNU)  
          ENDIF  
C  
C         Y-DIRECTION CHANNEL  
C  
          IF(MDCHTYP(NMD).EQ.2)THEN  
            APCG(LCHNV)=APCG(LCHNV)+CCCCHH(NMD)*PCG(LHOST)  
            APCG(LHOST)=APCG(LHOST)+CCCCHH(NMD)*PCG(LCHNV)  
          ENDIF  
        ENDDO  
      ENDIF  
      PAPCG=0.  
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
      WRITE(8,*)'  I   J   CCS   CCW    CCC    CCE    CCN   FPTMP' ! PM  
        DO L=2,LA  
          WRITE(8,800)IL(L),JL(L),CCS(L),CCW(L),CCC(L),CCE(L),CCN(L),  
     &        FPTMP(L)  
        ENDDO  
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
        IF(MDCHH.GE.1)THEN  
          DO NMD=1,MDCHH  
            LHOST=LMDCHH(NMD)  
            LCHNU=LMDCHU(NMD)  
            LCHNV=LMDCHV(NMD)  
C  
C         X-DIRECTION CHANNEL  
C  
            IF(MDCHTYP(NMD).EQ.1)THEN  
              RCG(LCHNU)=RCG(LCHNU)-CCCCHH(NMD)*P(LHOST)  
              RCG(LHOST)=RCG(LHOST)-CCCCHH(NMD)*P(LCHNU)  
            ENDIF  
C  
C         Y-DIRECTION CHANNEL  
C  
            IF(MDCHTYP(NMD).EQ.2)THEN  
              RCG(LCHNV)=RCG(LCHNV)-CCCCHH(NMD)*P(LHOST)  
              RCG(LHOST)=RCG(LHOST)-CCCCHH(NMD)*P(LCHNV)  
            ENDIF  
          ENDDO  
        ENDIF  
        DO L=2,LA  
          RCG(L)=RCG(L)*CCCI(L)  
        ENDDO  
        DO L=2,LA  
          RSQ=RSQ+RCG(L)*RCG(L)  
        ENDDO  
      ENDIF
      ! *** DSLLC END BLOCK
      TCONG=TCONG+TTMP-SECOND()  
  800 FORMAT(2I6,6E13.4)  
      RETURN  
      END  

