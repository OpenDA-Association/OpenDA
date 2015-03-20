      SUBROUTINE LSQHARM  
C  
C CHANGE RECORD  
C **  SUBROUTINE LSQHARM PERFORMS A LEAST SQUARES HARMONIC ANALYSIS  
C  
      USE GLOBAL  

      CHARACTER*80 TITLE,TITNT,TITRT  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::AMATMP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::PPPTMP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::P0

      IF(.NOT.ALLOCATED(AMATMP))THEN
		ALLOCATE(AMATMP(MTM))
		ALLOCATE(PPPTMP(MTM))
		ALLOCATE(P0(MLM))
	    AMATMP=0.0 
	    PPPTMP=0.0 
	    P0=0.0 
	ENDIF
C  
      TITRT='TREND, P0+P1DT1*T, REMOVED'  
      TITNT='TREND NOT REMOVED'  
      IF(JSLSHA.EQ.0) GOTO 100  
      JSLSHA=0  
C  
C **  INITIALIZE COEFFICIENT ARRAYS AND CONSTANTS  
C  
      RMLSHA=0.  
      RTLSHA=0.  
      RTTLSHA=0.  
      DO MC=1,MTIDE  
        CLSHA(MC)=0.  
        SLSHA(MC)=0.  
        CTLSHA(MC)=0.  
        STLSHA(MC)=0.  
        DO MR=1,MTIDE  
          CCLSHA(MR,MC)=0.  
          SSLSHA(MR,MC)=0.  
          CSLSHA(MR,MC)=0.  
          SCLSHA(MR,MC)=0.  
        ENDDO  
      ENDDO  
C  
C **  INITIALIZE RHS ARRAYS  
C  
      DO ML=1,MLLSHA  
        I=ILLSHA(ML)  
        J=JLLSHA(ML)  
        LLSHA(ML)=LIJ(I,J)  
      ENDDO  
      DO ML=1,MLLSHA  
        PLSHA(ML)=0.  
        PTLSHA(ML)=0.  
        UELSHA(ML)=0.  
        UETLSHA(ML)=0.  
        VELSHA(ML)=0.  
        VETLSHA(ML)=0.  
        DO K=1,KC  
          BLSHA(K,ML)=0.  
          BTLSHA(K,ML)=0.  
          ULSHA(K,ML)=0.  
          UTLSHA(K,ML)=0.  
          VLSHA(K,ML)=0.  
          VTLSHA(K,ML)=0.  
        ENDDO  
      ENDDO  
      DO MC=1,MTIDE  
        DO ML=1,MLLSHA  
          PCLSHA(ML,MC)=0.  
          PSLSHA(ML,MC)=0.  
          UECLSHA(ML,MC)=0.  
          UESLSHA(ML,MC)=0.  
          VECLSHA(ML,MC)=0.  
          VESLSHA(ML,MC)=0.  
          DO K=1,KC  
            BCLSHA(K,ML,MC)=0.  
            BSLSHA(K,ML,MC)=0.  
            UCLSHA(K,ML,MC)=0.  
            USLSHA(K,ML,MC)=0.  
            VCLSHA(K,ML,MC)=0.  
            VSLSHA(K,ML,MC)=0.  
          ENDDO  
        ENDDO  
      ENDDO  
  100 CONTINUE  
      IF(LSLSHA.EQ.1) GOTO 200  
C  
C **  ACCUMULATE ARRAYS  
C  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=DT*FLOAT(N)+TCON*TBEGIN  
      ELSE  
        TIME=TIMESEC  
      ENDIF  
      DO M=1,MTIDE  
        TMOD=MOD(TIME,TCP(M))  
        TMOD=PI2*TMOD/TCP(M)  
        CCCC(M)=COS(TMOD)  
        SSSS(M)=SIN(TMOD)  
      ENDDO  
C  
C **  ACCUMULATE COEFFICIENT ARRAYS  
C  
      RMLSHA=RMLSHA+1.  
      RTLSHA=RTLSHA+TIME  
      RTTLSHA=RTTLSHA+TIME*TIME  
      DO MC=1,MTIDE  
        CLSHA(MC)=CLSHA(MC)+CCCC(MC)  
        SLSHA(MC)=SLSHA(MC)+SSSS(MC)  
        CTLSHA(MC)=CTLSHA(MC)+TIME*CCCC(MC)  
        STLSHA(MC)=STLSHA(MC)+TIME*SSSS(MC)  
        DO MR=1,MTIDE  
          CCLSHA(MR,MC)=CCLSHA(MR,MC)+CCCC(MR)*CCCC(MC)  
          SSLSHA(MR,MC)=SSLSHA(MR,MC)+SSSS(MR)*SSSS(MC)  
          CSLSHA(MR,MC)=CSLSHA(MR,MC)+CCCC(MR)*SSSS(MC)  
          SCLSHA(MR,MC)=SCLSHA(MR,MC)+SSSS(MR)*CCCC(MC)  
        ENDDO  
      ENDDO  
C  
C **  ACCUMULATE RHS ARRAYS  
C  
      DO ML=1,MLLSHA  
        L=LLSHA(ML)  
        LN=LNC(L)  
        PLSHA(ML)=PLSHA(ML)+GI*P(L)  
        PTLSHA(ML)=PTLSHA(ML)+GI*TIME*P(L)  
        UTMP1=0.5*(UHDYE(L+1)+UHDYE(L))/(DYP(L)*HP(L))  
        VTMP1=0.5*(VHDXE(LN)+VHDXE(L))/(DXP(L)*HP(L))  
        UTMP=CUE(L)*UTMP1+CVE(L)*VTMP1  
        VTMP=CUN(L)*UTMP1+CVN(L)*VTMP1  
        UELSHA(ML)=UELSHA(ML)+UTMP  
        UETLSHA(ML)=UETLSHA(ML)+TIME*UTMP  
        VELSHA(ML)=VELSHA(ML)+VTMP  
        VETLSHA(ML)=VETLSHA(ML)+TIME*VTMP  
        DO K=1,KC  
          UTMP1=0.5*(U(L+1,K)+U(L,K))  
          VTMP1=0.5*(V(LN,K)+V(L,K))  
          UTMP=CUE(L)*UTMP1+CVE(L)*VTMP1  
          VTMP=CUN(L)*UTMP1+CVN(L)*VTMP1  
          BLSHA(K,ML)=BLSHA(K,ML)+B(L,K)  
          BTLSHA(K,ML)=BTLSHA(K,ML)+TIME*B(L,K)  
          ULSHA(K,ML)=ULSHA(K,ML)+UTMP  
          UTLSHA(K,ML)=UTLSHA(K,ML)+TIME*UTMP  
          VLSHA(K,ML)=VLSHA(K,ML)+VTMP  
          VTLSHA(K,ML)=VTLSHA(K,ML)+TIME*VTMP  
        ENDDO  
      ENDDO  
      DO ML=1,MLLSHA  
        L=LLSHA(ML)  
        LN=LNC(L)  
        DO MC=1,MTIDE  
          PCLSHA(ML,MC)=PCLSHA(ML,MC)+GI*P(L)*CCCC(MC)  
          PSLSHA(ML,MC)=PSLSHA(ML,MC)+GI*P(L)*SSSS(MC)  
          UTMP1=0.5*(UHDYE(L+1)+UHDYE(L))/(DYP(L)*HP(L))  
          VTMP1=0.5*(VHDXE(LN)+VHDXE(L))/(DXP(L)*HP(L))  
          UTMP=CUE(L)*UTMP1+CVE(L)*VTMP1  
          VTMP=CUN(L)*UTMP1+CVN(L)*VTMP1  
          UECLSHA(ML,MC)=UECLSHA(ML,MC)+CCCC(MC)*UTMP  
          UESLSHA(ML,MC)=UESLSHA(ML,MC)+SSSS(MC)*UTMP  
          VECLSHA(ML,MC)=VECLSHA(ML,MC)+CCCC(MC)*VTMP  
          VESLSHA(ML,MC)=VESLSHA(ML,MC)+SSSS(MC)*VTMP  
          DO K=1,KC  
            UTMP1=0.5*(U(L+1,K)+U(L,K))  
            VTMP1=0.5*(V(LN,K)+V(L,K))  
            UTMP=CUE(L)*UTMP1+CVE(L)*VTMP1  
            VTMP=CUN(L)*UTMP1+CVN(L)*VTMP1  
            BCLSHA(K,ML,MC)=BCLSHA(K,ML,MC)+B(L,K)*CCCC(MC)  
            BSLSHA(K,ML,MC)=BSLSHA(K,ML,MC)+B(L,K)*SSSS(MC)  
            UCLSHA(K,ML,MC)=UCLSHA(K,ML,MC)+CCCC(MC)*UTMP  
            USLSHA(K,ML,MC)=USLSHA(K,ML,MC)+SSSS(MC)*UTMP  
            VCLSHA(K,ML,MC)=VCLSHA(K,ML,MC)+CCCC(MC)*VTMP  
            VSLSHA(K,ML,MC)=VSLSHA(K,ML,MC)+SSSS(MC)*VTMP  
          ENDDO  
        ENDDO  
      ENDDO  
      RETURN  
C  
C *** COMPLETE ANALYSIS  
C  
  200 CONTINUE  
      OPEN(97,FILE='LSHA.OUT',STATUS='UNKNOWN')  
      CLOSE(97,STATUS='DELETE')  
      OPEN(97,FILE='LSHA.OUT',STATUS='UNKNOWN')  
      IF(ISLSTR.EQ.1) GOTO 500  
C  
C *** COMPUTE SOLUTION WITH NO TREND REMOVAL  
C *** LOAD GLOBAL COEFFICIENT MATRIX  
C  
      MG=2*MTIDE+1  
      GLSHA(1,1)=RMLSHA  
      DO MC=1,MTIDE  
        MCP1=MC+1  
        MCPP=MC+1+MTIDE  
        GLSHA(1,MCP1)=CLSHA(MC)  
        GLSHA(1,MCPP)=SLSHA(MC)  
        GLSHA(MCP1,1)=CLSHA(MC)  
        GLSHA(MCPP,1)=SLSHA(MC)  
        DO MR=1,MTIDE  
          MRP1=MR+1  
          MRPP=MR+1+MTIDE  
          GLSHA(MRP1,MCP1)=CCLSHA(MR,MC)  
          GLSHA(MRPP,MCPP)=SSLSHA(MR,MC)  
          GLSHA(MRP1,MCPP)=CSLSHA(MR,MC)  
          GLSHA(MRPP,MCP1)=SCLSHA(MR,MC)  
        ENDDO  
      ENDDO  
C  
C *** PERFORM SVD ON GLSHA  
C  
      CALL SVDCMP (GLSHA,MG,MG,MGM,MGM,WLSHA,VVLSHA)  
      WRITE(97,10)TITNT  
      WRITE(97,11)  
      DO M=1,MG  
        WRITE(97,12)WLSHA(M)  
      ENDDO  
      WRITE(97,13)  
C  
C *** SOLVE BY BACK SUBSTITUTION AND OUTPUT RESULTS  
C  
      TITLE='FREE SURFACE DISPLACEMENT, NO TREND REMOVED'  
      DO ML=1,MLLSHA  
        IF(LSHAP(ML).EQ.1)THEN  
          WRITE(97,10)TITLE  
          WRITE(97,14)CLSL(ML),ILLSHA(ML),JLLSHA(ML)  
          RHS(1)=PLSHA(ML)  
          DO MC=1,MTIDE  
            MCP1=MC+1  
            MCPP=MC+1+MTIDE  
            RHS(MCP1)=PCLSHA(ML,MC)  
            RHS(MCPP)=PSLSHA(ML,MC)  
          ENDDO  
          CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
          WRITE(97,16)RSOL(1)  
          WRITE(97,22)  
          DO MC=1,MTIDE  
            MCP1=MC+1  
            MCPP=MC+1+MTIDE  
            AMP=SQRT(RSOL(MCP1)*RSOL(MCP1)  
     &          +RSOL(MCPP)*RSOL(MCPP))  
            IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MCP1).EQ.0.0)THEN  
              PHI=999999.  
            ELSE  
              PHI=ATAN2(RSOL(MCPP),RSOL(MCP1))  
            ENDIF  
            PHASE=TCP(MC)*PHI/PI2  
            IF(PHASE.LT.0.) PHASE=PHASE+TCP(MC)  
            AMATMP(MC)=AMP  
            PPPTMP(MC)=PHASE/3600.  
            WRITE(97,230)AMP,PHASE,RSOL(MCP1),RSOL(MCPP),SYMBOL(MC)  
          ENDDO  
          WRITE(97,41)  
          DO MC=1,MTIDE  
            WRITE(97,40)AMATMP(MC)  
          ENDDO  
          WRITE(97,41)  
          DO MC=1,MTIDE  
            PPPTMP(MC)=3600.*PPPTMP(MC)  
            WRITE(97,42)PPPTMP(MC)  
          ENDDO  
          WRITE(97,13)  
        ENDIF  
      ENDDO  
      TITLE='SALINITY, NO TREND REMOVED'  
      DO ML=1,MLLSHA  
        IF(LSHAB(ML).EQ.1)THEN  
          DO K=1,KC  
            WRITE(97,10)TITLE  
            WRITE(97,15)CLSL(ML),ILLSHA(ML),JLLSHA(ML),K  
            RHS(1)=BLSHA(K,ML)  
            DO MC=1,MTIDE  
              MCP1=MC+1  
              MCPP=MC+1+MTIDE  
              RHS(MCP1)=BCLSHA(K,ML,MC)  
              RHS(MCPP)=BSLSHA(K,ML,MC)  
            ENDDO  
            CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
            WRITE(97,16)RSOL(1)  
            WRITE(97,22)  
            DO MC=1,MTIDE  
              MCP1=MC+1  
              MCPP=MC+1+MTIDE  
              AMP=SQRT(RSOL(MCP1)*RSOL(MCP1)  
     &            +RSOL(MCPP)*RSOL(MCPP))  
              IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MCP1).EQ.0.0)THEN  
                PHI=999999.  
              ELSE  
                PHI=ATAN2(RSOL(MCPP),RSOL(MCP1))  
              ENDIF  
              PHASE=TCP(MC)*PHI/PI2  
              IF(PHASE.LT.0.) PHASE=PHASE+TCP(MC)  
              AMATMP(MC)=AMP  
              PPPTMP(MC)=PHASE/3600.  
              WRITE(97,23)SYMBOL(MC),TCP(MC),AMP,PHASE  
            ENDDO  
            WRITE(97,41)  
            DO MC=1,MTIDE  
              WRITE(97,40)AMATMP(MC)  
            ENDDO  
            WRITE(97,41)  
            DO MC=1,MTIDE  
              WRITE(97,40)PPPTMP(MC)  
            ENDDO  
            WRITE(97,13)  
          ENDDO  
        ENDIF  
      ENDDO  
      TITLE='HORIZONTAL EXTERNAL MODE VELOCITY, NO TREND REMOVED'  
      DO ML=1,MLLSHA  
        IF(LSHAUE(ML).EQ.1)THEN  
          RHS(1)=UELSHA(ML)  
          DO MC=1,MTIDE  
            MCP1=MC+1  
            MCPP=MC+1+MTIDE  
            RHS(MCP1)=UECLSHA(ML,MC)  
            RHS(MCPP)=UESLSHA(ML,MC)  
          ENDDO  
          CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
          UMEAN=RSOL(1)  
          DO MC=1,MTIDE  
            MCP1=MC+1  
            MCPP=MC+1+MTIDE  
            AMPU(MC)=SQRT(RSOL(MCP1)*RSOL(MCP1)  
     &          +RSOL(MCPP)*RSOL(MCPP))  
            UCOS(MC)=RSOL(MCP1)  
            USIN(MC)=RSOL(MCPP)  
            IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MCP1).EQ.0.0)THEN  
              PHI=999999.  
            ELSE  
              PHI=ATAN2(RSOL(MCPP),RSOL(MCP1))  
            ENDIF  
            PHASEU(MC)=TCP(MC)*PHI/PI2  
            IF(PHASEU(MC).LT.0.) PHASEU(MC)=PHASEU(MC)+TCP(MC)  
          ENDDO  
          RHS(1)=VELSHA(ML)  
          DO MC=1,MTIDE  
            MCP1=MC+1  
            MCPP=MC+1+MTIDE  
            RHS(MCP1)=VECLSHA(ML,MC)  
            RHS(MCPP)=VESLSHA(ML,MC)  
          ENDDO  
          CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
          VMEAN=RSOL(1)  
          DO MC=1,MTIDE  
            MCP1=MC+1  
            MCPP=MC+1+MTIDE  
            AMPV(MC)=SQRT(RSOL(MCP1)*RSOL(MCP1)  
     &          +RSOL(MCPP)*RSOL(MCPP))  
            VCOS(MC)=RSOL(MCP1)  
            VSIN(MC)=RSOL(MCPP)  
            IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MCP1).EQ.0.0)THEN  
              PHI=999999.  
            ELSE  
              PHI=ATAN2(RSOL(MCPP),RSOL(MCP1))  
            ENDIF  
            PHASEV(MC)=TCP(MC)*PHI/PI2  
            IF(PHASEV(MC).LT.0.) PHASEV(MC)=PHASEV(MC)+TCP(MC)  
          ENDDO  
          DO MC=1,MTIDE  
            TERM1=UCOS(MC)+VSIN(MC)  
            TERM2=VCOS(MC)-USIN(MC)  
            TERM3=UCOS(MC)-VSIN(MC)  
            TERM4=VCOS(MC)+USIN(MC)  
            RPLUS=0.5*SQRT(TERM1*TERM1+TERM2*TERM2)  
            RMINS=0.5*SQRT(TERM3*TERM3+TERM4*TERM4)  
            IF(TERM1.EQ.0.0.AND.TERM2.EQ.0.0)THEN  
              APLUS=999999.  
            ELSE  
              APLUS=ATAN2(TERM2,TERM1)  
            ENDIF  
            IF(TERM3.EQ.0.0.AND.TERM4.EQ.0.0)THEN  
              AMINS=999999.  
            ELSE  
              AMINS=ATAN2(TERM4,TERM3)  
            ENDIF  
            RMAJ(MC)=RPLUS+RMINS  
            RMIN(MC)=RPLUS-RMINS  
            ACCWX(MC)=(90./PI)*(APLUS+AMINS)  
            PHASEE(MC)=(0.25/PI)*TCP(MC)*(AMINS-APLUS)  
            IF(PHASEE(MC).LT.0.) PHASEE(MC)=PHASEE(MC)+TCP(MC)  
          ENDDO  
          WRITE(97,10)TITLE  
          WRITE(97,14)CLSL(ML),ILLSHA(ML),JLLSHA(ML)  
          WRITE(97,17)UMEAN,VMEAN  
          WRITE(97,24)  
          DO M=1,MTIDE  
            WRITE(97,25)SYMBOL(M),TCP(M),AMPU(M),PHASEU(M),AMPV(M)
     &          ,PHASEV(M)  
          ENDDO  
          WRITE(97,13)  
          WRITE(97,26)  
          DO M=1,MTIDE  
            WRITE(97,25)SYMBOL(M),TCP(M),RMAJ(M),RMIN(M),ACCWX(M)
     &          ,PHASEE(M)  
          ENDDO  
          WRITE(97,41)  
          DO M=1,MTIDE  
            WRITE(97,40)RMAJ(M)  
          ENDDO  
          WRITE(97,41)  
          DO MC=1,MTIDE  
            WRITE(97,40)RMIN(M)  
          ENDDO  
          WRITE(97,41)  
          DO MC=1,MTIDE  
            WRITE(97,40)ACCWX(M)  
          ENDDO  
          WRITE(97,41)  
          DO MC=1,MTIDE  
            WRITE(97,42)PPPTMP(MC)  
          ENDDO  
          WRITE(97,13)  
        ENDIF  
      ENDDO  
      TITLE='HORIZONTAL VELOCITY, NO TREND REMOVED'  
      DO ML=1,MLLSHA  
        IF(LSHAU(ML).EQ.1)THEN  
          DO K=1,KC  
            RHS(1)=ULSHA(K,ML)  
            DO MC=1,MTIDE  
              MCP1=MC+1  
              MCPP=MC+1+MTIDE  
              RHS(MCP1)=UCLSHA(K,ML,MC)  
              RHS(MCPP)=USLSHA(K,ML,MC)  
            ENDDO  
            CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
            UMEAN=RSOL(1)  
            DO MC=1,MTIDE  
              MCP1=MC+1  
              MCPP=MC+1+MTIDE  
              AMPU(MC)=SQRT(RSOL(MCP1)*RSOL(MCP1)  
     &            +RSOL(MCPP)*RSOL(MCPP))  
              UCOS(MC)=RSOL(MCP1)  
              USIN(MC)=RSOL(MCPP)  
              IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MCP1).EQ.0.0)THEN  
                PHI=999999.  
              ELSE  
                PHI=ATAN2(RSOL(MCPP),RSOL(MCP1))  
              ENDIF  
              PHASEU(MC)=TCP(MC)*PHI/PI2  
              IF(PHASEU(MC).LT.0.) PHASEU(MC)=PHASEU(MC)+TCP(MC)  
            ENDDO  
            RHS(1)=VLSHA(K,ML)  
            DO MC=1,MTIDE  
              MCP1=MC+1  
              MCPP=MC+1+MTIDE  
              RHS(MCP1)=VCLSHA(K,ML,MC)  
              RHS(MCPP)=VSLSHA(K,ML,MC)  
            ENDDO  
            CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
            VMEAN=RSOL(1)  
            DO MC=1,MTIDE  
              MCP1=MC+1  
              MCPP=MC+1+MTIDE  
              AMPV(MC)=SQRT(RSOL(MCP1)*RSOL(MCP1)  
     &            +RSOL(MCPP)*RSOL(MCPP))  
              VCOS(MC)=RSOL(MCP1)  
              VSIN(MC)=RSOL(MCPP)  
              IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MCP1).EQ.0.0)THEN  
                PHI=999999.  
              ELSE  
                PHI=ATAN2(RSOL(MCPP),RSOL(MCP1))  
              ENDIF  
              PHASEV(MC)=TCP(MC)*PHI/PI2  
              IF(PHASEV(MC).LT.0.) PHASEV(MC)=PHASEV(MC)+TCP(MC)  
            ENDDO  
            DO MC=1,MTIDE  
              TERM1=UCOS(MC)+VSIN(MC)  
              TERM2=VCOS(MC)-USIN(MC)  
              TERM3=UCOS(MC)-VSIN(MC)  
              TERM4=VCOS(MC)+USIN(MC)  
              RPLUS=0.5*SQRT(TERM1*TERM1+TERM2*TERM2)  
              RMINS=0.5*SQRT(TERM3*TERM3+TERM4*TERM4)  
              IF(TERM1.EQ.0.0.AND.TERM2.EQ.0.0)THEN  
                APLUS=999999.  
              ELSE  
                APLUS=ATAN2(TERM2,TERM1)  
              ENDIF  
              IF(TERM3.EQ.0.0.AND.TERM4.EQ.0.0)THEN  
                AMINS=999999.  
              ELSE  
                AMINS=ATAN2(TERM4,TERM3)  
              ENDIF  
              RMAJ(MC)=RPLUS+RMINS  
              RMIN(MC)=RPLUS-RMINS  
              ACCWX(MC)=(90./PI)*(APLUS+AMINS)  
              PHASEE(MC)=(0.25/PI)*TCP(MC)*(AMINS-APLUS)  
              IF(PHASEE(MC).LT.0.) PHASEE(MC)=PHASEE(MC)+TCP(MC)  
            ENDDO  
            WRITE(97,10)TITLE  
            WRITE(97,15)CLSL(ML),ILLSHA(ML),JLLSHA(ML),K  
            WRITE(97,17)UMEAN,VMEAN  
            WRITE(97,24)  
            DO M=1,MTIDE  
              WRITE(97,25)SYMBOL(M),TCP(M),AMPU(M),PHASEU(M),AMPV(M)
     &            ,PHASEV(M)  
            ENDDO  
            WRITE(97,13)  
            WRITE(97,26)  
            DO M=1,MTIDE  
              WRITE(97,25)SYMBOL(M),TCP(M),RMAJ(M),RMIN(M),ACCWX(M)
     &            ,PHASEE(M)  
            ENDDO  
            WRITE(97,41)  
            DO M=1,MTIDE  
              WRITE(97,40)RMAJ(M)  
            ENDDO  
            WRITE(97,41)  
            DO MC=1,MTIDE  
              WRITE(97,40)RMIN(M)  
            ENDDO  
            WRITE(97,41)  
            DO MC=1,MTIDE  
              WRITE(97,40)ACCWX(M)  
            ENDDO  
            WRITE(97,41)  
            DO MC=1,MTIDE  
              PPPTMP(M)=PHASEE(M)/3600.  
              WRITE(97,40)PPPTMP(MC)  
            ENDDO  
            WRITE(97,13)  
          ENDDO  
        ENDIF  
      ENDDO  
      GOTO 501  
  500 CONTINUE  
C  
C *** COMPUTE SOLUTION WITH TREND REMOVAL  
C *** COMPUTE TREND  
C  
      DET=RMLSHA*RTTLSHA-RTLSHA*RTLSHA  
      DO ML=1,MLLSHA  
        P0(ML)=(PLSHA(ML)*RTTLSHA-PTLSHA(ML)*RTLSHA)/DET  
        P1DT1(ML)=(PTLSHA(ML)*RMLSHA-PLSHA(ML)*RTLSHA)/DET  
        UE0(ML)=(UELSHA(ML)*RTTLSHA-UETLSHA(ML)*RTLSHA)/DET  
        UE1DT1(ML)=(UETLSHA(ML)*RMLSHA-UELSHA(ML)*RTLSHA)/DET  
        VE0(ML)=(VELSHA(ML)*RTTLSHA-VETLSHA(ML)*RTLSHA)/DET  
        VE1DT1(ML)=(VETLSHA(ML)*RMLSHA-VELSHA(ML)*RTLSHA)/DET  
        DO K=1,KC  
          B0(K,ML)=(BLSHA(K,ML)*RTTLSHA-BTLSHA(K,ML)*RTLSHA)/DET  
          B1DT1(K,ML)=(BTLSHA(K,ML)*RMLSHA-BLSHA(K,ML)*RTLSHA)/DET  
          U0(K,ML)=(ULSHA(K,ML)*RTTLSHA-UTLSHA(K,ML)*RTLSHA)/DET  
          U1DT1(K,ML)=(UTLSHA(K,ML)*RMLSHA-ULSHA(K,ML)*RTLSHA)/DET  
          V0(K,ML)=(VLSHA(K,ML)*RTTLSHA-VTLSHA(K,ML)*RTLSHA)/DET  
          V1DT1(K,ML)=(VTLSHA(K,ML)*RMLSHA-VLSHA(K,ML)*RTLSHA)/DET  
        ENDDO  
      ENDDO  
C  
C *** LOAD GLOBAL COEFFICIENT MATRIX  
C  
      MG=2*MTIDE  
      DO MC=1,MTIDE  
        MCPP=MC+MTIDE  
        DO MR=1,MTIDE  
          MRPP=MR+MTIDE  
          GLSHA(MR,MC)=CCLSHA(MR,MC)  
          GLSHA(MRPP,MCPP)=SSLSHA(MR,MC)  
          GLSHA(MR,MCPP)=CSLSHA(MR,MC)  
          GLSHA(MRPP,MC)=SCLSHA(MR,MC)  
        ENDDO  
      ENDDO  
C  
C *** PERFORM SVD ON GLSHA  
C  
      CALL SVDCMP (GLSHA,MG,MG,MGM,MGM,WLSHA,VVLSHA)  
      WRITE(97,10)TITRT  
      WRITE(97,11)  
      DO M=1,MG  
        WRITE(97,12)WLSHA(M)  
      ENDDO  
      WRITE(97,13)  
C  
C *** SOLVE BY BACK SUBSTITUTION AND OUTPUT RESULTS  
C  
      TITLE='FREE SURFACE DISPLACEMENT, TREND REMOVED'  
      DO ML=1,MLLSHA  
        IF(LSHAP(ML).EQ.1)THEN  
          WRITE(97,10)TITLE  
          WRITE(97,14)CLSL(ML),ILLSHA(ML),JLLSHA(ML)  
          WRITE(97,18)P0(ML)  
          WRITE(97,19)P1DT1(ML)  
          WRITE(97,22)  
          DO MC=1,MTIDE  
            MCPP=MC+MTIDE  
            RHS(MC)=PCLSHA(ML,MC)-P0(ML)-P1DT1(ML)*CTLSHA(MC)  
            RHS(MCPP)=PSLSHA(ML,MC)-P0(ML)-P1DT1(ML)*STLSHA(MC)  
          ENDDO  
          CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
          DO MC=1,MTIDE  
            MCPP=MTIDE+MC  
            AMP=SQRT(RSOL(MC)*RSOL(MC)  
     &          +RSOL(MCPP)*RSOL(MCPP))  
            IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MC).EQ.0.0)THEN  
              PHI=999999.  
            ELSE  
              PHI=ATAN2(RSOL(MCPP),RSOL(MC))  
            ENDIF  
            PHASE=TCP(MC)*PHI/PI2  
            IF(PHASE.LT.0.) PHASE=PHASE+TCP(MC)  
            AMATMP(MC)=AMP  
            PPPTMP(MC)=PHASE/3600.  
            WRITE(97,23)SYMBOL(MC),TCP(MC),AMP,PHASE  
          ENDDO  
          WRITE(97,41)  
          DO MC=1,MTIDE  
            WRITE(97,40)AMATMP(MC)  
          ENDDO  
          WRITE(97,41)  
          DO MC=1,MTIDE  
            WRITE(97,40)PPPTMP(MC)  
          ENDDO  
          WRITE(97,13)  
        ENDIF  
      ENDDO  
      TITLE='SALINITY, TREND REMOVED'  
      DO ML=1,MLLSHA  
        IF(LSHAB(ML).EQ.1)THEN  
          DO K=1,KC  
            WRITE(97,10)TITLE  
            WRITE(97,15)CLSL(ML),ILLSHA(ML),JLLSHA(ML),K  
            WRITE(97,18)B0(K,ML)  
            WRITE(97,19)B1DT1(K,ML)  
            WRITE(97,22)  
            DO MC=1,MTIDE  
              MCPP=MC+MTIDE  
              RHS(MC)=BCLSHA(K,ML,MC)-B0(K,ML)-B1DT1(K,ML)*CTLSHA(MC)  
              RHS(MCPP)=BSLSHA(K,ML,MC)-B0(K,ML)-B1DT1(K,ML)*STLSHA(MC)  
            ENDDO  
            CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
            DO MC=1,MTIDE  
              MCPP=MC+MTIDE  
              AMP=SQRT(RSOL(MC)*RSOL(MC)  
     &            +RSOL(MCPP)*RSOL(MCPP))  
              IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MC).EQ.0.0)THEN  
                PHI=999999.  
              ELSE  
                PHI=ATAN2(RSOL(MCPP),RSOL(MC))  
              ENDIF  
              PHASE=TCP(MC)*PHI/PI2  
              IF(PHASE.LT.0.) PHASE=PHASE+TCP(MC)  
              AMATMP(MC)=AMP  
              PPPTMP(MC)=PHASE/3600.  
              WRITE(97,23)SYMBOL(MC),TCP(MC),AMP,PHASE  
            ENDDO  
            WRITE(97,41)  
            DO MC=1,MTIDE  
              WRITE(97,40)AMATMP(MC)  
            ENDDO  
            WRITE(97,41)  
            DO MC=1,MTIDE  
              WRITE(97,40)PPPTMP(MC)  
            ENDDO  
            WRITE(97,13)  
          ENDDO  
        ENDIF  
      ENDDO  
      TITLE='HORIZONTAL EXTERNAL MODE VELOCITY, TREND REMOVED'  
      DO ML=1,MLLSHA  
        IF(LSHAU(ML).EQ.1)THEN  
          DO MC=1,MTIDE  
            MCPP=MC+MTIDE  
            RHS(MC)=UECLSHA(ML,MC)-UE0(ML)-UE1DT1(ML)*CTLSHA(MC)  
            RHS(MCPP)=UESLSHA(ML,MC)-UE0(ML)-UE1DT1(ML)*STLSHA(MC)  
          ENDDO  
          CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
          DO MC=1,MTIDE  
            MCPP=MC+MTIDE  
            AMPU(MC)=SQRT(RSOL(MC)*RSOL(MC)  
     &          +RSOL(MCPP)*RSOL(MCPP))  
            UCOS(MC)=RSOL(MC)  
            USIN(MC)=RSOL(MCPP)  
            IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MC).EQ.0.0)THEN  
              PHI=999999.  
            ELSE  
              PHI=ATAN2(RSOL(MCPP),RSOL(MC))  
            ENDIF  
            PHASEU(MC)=TCP(MC)*PHI/PI2  
            IF(PHASEU(MC).LT.0.) PHASEU(MC)=PHASEU(MC)+TCP(MC)  
          ENDDO  
          DO MC=1,MTIDE  
            MCPP=MC+MTIDE  
            RHS(MC)=VECLSHA(ML,MC)-VE0(ML)-VE1DT1(ML)*CTLSHA(MC)  
            RHS(MCPP)=VESLSHA(ML,MC)-UE0(ML)-VE1DT1(ML)*STLSHA(MC)  
          ENDDO  
          CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
          DO MC=1,MTIDE  
            MCPP=MC+MTIDE  
            AMPV(MC)=SQRT(RSOL(MC)*RSOL(MC)  
     &          +RSOL(MCPP)*RSOL(MCPP))  
            VCOS(MC)=RSOL(MC)  
            VSIN(MC)=RSOL(MCPP)  
            IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MC).EQ.0.0)THEN  
              PHI=999999.  
            ELSE  
              PHI=ATAN2(RSOL(MCPP),RSOL(MC))  
            ENDIF  
            PHASEV(MC)=TCP(MC)*PHI/PI2  
            IF(PHASEV(MC).LT.0.) PHASEV(MC)=PHASEV(MC)+TCP(MC)  
          ENDDO  
          DO MC=1,MTIDE  
            TERM1=UCOS(MC)+VSIN(MC)  
            TERM2=VCOS(MC)-USIN(MC)  
            TERM3=UCOS(MC)-VSIN(MC)  
            TERM4=VCOS(MC)+USIN(MC)  
            RPLUS=0.5*SQRT(TERM1*TERM1+TERM2*TERM2)  
            RMINS=0.5*SQRT(TERM3*TERM3+TERM4*TERM4)  
            IF(TERM1.EQ.0.0.AND.TERM2.EQ.0.0)THEN  
              APLUS=999999.  
            ELSE  
              APLUS=ATAN2(TERM2,TERM1)  
            ENDIF  
            IF(TERM3.EQ.0.0.AND.TERM4.EQ.0.0)THEN  
              AMINS=999999.  
            ELSE  
              AMINS=ATAN2(TERM4,TERM3)  
            ENDIF  
            RMAJ(MC)=RPLUS+RMINS  
            RMIN(MC)=RPLUS-RMINS  
            ACCWX(MC)=(90./PI)*(APLUS+AMINS)  
            PHASEE(MC)=(0.25/PI)*TCP(MC)*(AMINS-APLUS)  
            IF(PHASEE(MC).LT.0.) PHASEE(MC)=PHASEE(MC)+TCP(MC)  
          ENDDO  
          WRITE(97,10)TITLE  
          WRITE(97,14)CLSL(ML),ILLSHA(ML),JLLSHA(ML)  
          WRITE(97,20)UE0(ML),VE0(ML)  
          WRITE(97,21)UE1DT1(ML),VE1DT1(ML)  
          WRITE(97,24)  
          DO M=1,MTIDE  
            WRITE(97,25)SYMBOL(M),TCP(M),AMPU(M),PHASEU(M),AMPV(M)
     &          ,PHASEV(M)  
          ENDDO  
          WRITE(97,13)  
          WRITE(97,26)  
          DO M=1,MTIDE  
            WRITE(97,25)SYMBOL(M),TCP(M),RMAJ(M),RMIN(M),ACCWX(M)
     &          ,PHASEE(M)  
          ENDDO  
          WRITE(97,41)  
          DO M=1,MTIDE  
            WRITE(97,40)RMAJ(M)  
          ENDDO  
          WRITE(97,41)  
          DO MC=1,MTIDE  
            WRITE(97,40)RMIN(M)  
          ENDDO  
          WRITE(97,41)  
          DO MC=1,MTIDE  
            WRITE(97,40)ACCWX(M)  
          ENDDO  
          WRITE(97,41)  
          DO MC=1,MTIDE  
            PPPTMP(M)=PHASEE(M)/3600.  
            WRITE(97,40)PPPTMP(MC)  
          ENDDO  
          WRITE(97,13)  
        ENDIF  
      ENDDO  
      TITLE='HORIZONTAL VELOCITY, TREND REMOVED'  
      DO ML=1,MLLSHA  
        IF(LSHAU(ML).EQ.1)THEN  
          DO K=1,KC  
            DO MC=1,MTIDE  
              MCPP=MC+MTIDE  
              RHS(MC)=UCLSHA(K,ML,MC)-U0(K,ML)-U1DT1(K,ML)*CTLSHA(MC)  
              RHS(MCPP)=USLSHA(K,ML,MC)-U0(K,ML)-U1DT1(K,ML)*STLSHA(MC)  
            ENDDO  
            CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
            DO MC=1,MTIDE  
              MCPP=MC+MTIDE  
              AMPU(MC)=SQRT(RSOL(MC)*RSOL(MC)  
     &            +RSOL(MCPP)*RSOL(MCPP))  
              UCOS(MC)=RSOL(MC)  
              USIN(MC)=RSOL(MCPP)  
              IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MC).EQ.0.0)THEN  
                PHI=999999.  
              ELSE  
                PHI=ATAN2(RSOL(MCPP),RSOL(MC))  
              ENDIF  
              PHASEU(MC)=TCP(MC)*PHI/PI2  
              IF(PHASEU(MC).LT.0.) PHASEU(MC)=PHASEU(MC)+TCP(MC)  
            ENDDO  
            DO MC=1,MTIDE  
              MCPP=MC+MTIDE  
              RHS(MC)=VCLSHA(K,ML,MC)-V0(K,ML)-V1DT1(K,ML)*CTLSHA(MC)  
              RHS(MCPP)=VSLSHA(K,ML,MC)-U0(K,ML)-V1DT1(K,ML)*STLSHA(MC)  
            ENDDO  
            CALL SVBKSB (GLSHA,WLSHA,VVLSHA,MG,MG,MGM,MGM,RHS,RSOL)  
            DO MC=1,MTIDE  
              MCPP=MC+MTIDE  
              AMPV(MC)=SQRT(RSOL(MC)*RSOL(MC)  
     &            +RSOL(MCPP)*RSOL(MCPP))  
              VCOS(MC)=RSOL(MC)  
              VSIN(MC)=RSOL(MCPP)  
              IF(RSOL(MCPP).EQ.0.0.AND.RSOL(MC).EQ.0.0)THEN  
                PHI=999999.  
              ELSE  
                PHI=ATAN2(RSOL(MCPP),RSOL(MC))  
              ENDIF  
              PHASEV(MC)=TCP(MC)*PHI/PI2  
              IF(PHASEV(MC).LT.0.) PHASEV(MC)=PHASEV(MC)+TCP(MC)  
            ENDDO  
            DO MC=1,MTIDE  
              TERM1=UCOS(MC)+VSIN(MC)  
              TERM2=VCOS(MC)-USIN(MC)  
              TERM3=UCOS(MC)-VSIN(MC)  
              TERM4=VCOS(MC)+USIN(MC)  
              RPLUS=0.5*SQRT(TERM1*TERM1+TERM2*TERM2)  
              RMINS=0.5*SQRT(TERM3*TERM3+TERM4*TERM4)  
              IF(TERM1.EQ.0.0.AND.TERM2.EQ.0.0)THEN  
                APLUS=999999.  
              ELSE  
                APLUS=ATAN2(TERM2,TERM1)  
              ENDIF  
              IF(TERM3.EQ.0.0.AND.TERM4.EQ.0.0)THEN  
                AMINS=999999.  
              ELSE  
                AMINS=ATAN2(TERM4,TERM3)  
              ENDIF  
              RMAJ(MC)=RPLUS+RMINS  
              RMIN(MC)=RPLUS-RMINS  
              ACCWX(MC)=(90./PI)*(APLUS+AMINS)  
              PHASEE(MC)=(0.25/PI)*TCP(MC)*(AMINS-APLUS)  
              IF(PHASEE(MC).LT.0.) PHASEE(MC)=PHASEE(MC)+TCP(MC)  
            ENDDO  
            WRITE(97,10)TITLE  
            WRITE(97,15)CLSL(ML),ILLSHA(ML),JLLSHA(ML),K  
            WRITE(97,20)U0(K,ML),V0(K,ML)  
            WRITE(97,21)U1DT1(K,ML),V1DT1(K,ML)  
            WRITE(97,24)  
            DO M=1,MTIDE  
              WRITE(97,25)SYMBOL(M),TCP(M),AMPU(M),PHASEU(M),AMPV(M)
     &            ,PHASEV(M)  
            ENDDO  
            WRITE(97,13)  
            WRITE(97,26)  
            DO M=1,MTIDE  
              WRITE(97,25)SYMBOL(M),TCP(M),RMAJ(M),RMIN(M),ACCWX(M)
     &            ,PHASEE(M)  
            ENDDO  
            WRITE(97,41)  
            DO M=1,MTIDE  
              WRITE(97,40)RMAJ(M)  
            ENDDO  
            WRITE(97,41)  
            DO MC=1,MTIDE  
              WRITE(97,40)RMIN(M)  
            ENDDO  
            WRITE(97,41)  
            DO MC=1,MTIDE  
              WRITE(97,40)ACCWX(M)  
            ENDDO  
            WRITE(97,41)  
            DO MC=1,MTIDE  
              PPPTMP(M)=PHASEE(M)/3600.  
              WRITE(97,40)PPPTMP(MC)  
            ENDDO  
            WRITE(97,13)  
          ENDDO  
        ENDIF  
      ENDDO  
   10 FORMAT(1X,A80,//)  
   11 FORMAT('  SINGULAR VALUES',//)  
   12 FORMAT(1X,E12.4)  
   13 FORMAT(////)  
   14 FORMAT(1X,A20,4X,'I=',I5,4X,'J=',I5,//)  
   15 FORMAT(1X,A20,4X,'I=',I5,4X,'J=',I5,4X,'K=',I5,//)  
   16 FORMAT('  MEAN=',2X,E12.4,//)  
   17 FORMAT('  U MEAN=',2X,E12.4,4X,'V MEAN=',2X,E12.4,//)  
   18 FORMAT('  INTERSEPT=',2X,E12.4,//)  
   19 FORMAT('  SLOPE=',2X,E12.4,//)  
   20 FORMAT('  U INTERSEPT=',2X,E12.4,4X,'V INTERSEPT=',2X,E12.4,//)  
   21 FORMAT('  U SLOPE=',2X,E12.4,4X,'V SLOPE=',2X,E12.4,//)  
   22 FORMAT('  SYMBOL',3X,'TCP',12X,'AMPLITUDE',6X,'PHASE',/)  
   23 FORMAT(1X,A5,3X,E12.4,3X,E12.4,3X,E12.4,/)  
   24 FORMAT('  SYMBOL',3X,'TCP',12X,'AMU',12X,'PHU',12X,'AMV',  
     &    12X,'PHV',/)  
   25 FORMAT(1X,A5,3X,E12.4,3X,E12.4,3X,E12.4,3X,E12.4,3X,E12.4,/)  
   26 FORMAT('  SYMBOL',3X,'TCP',12X,'MAJ',12X,'MIN',12X,'ANG',  
     &    12X,'PHE',/)  
   40 FORMAT(1X,F12.3)  
   41 FORMAT(//)  
   42 FORMAT(F10.0)  
  230 FORMAT(4E14.5,4X,A2)  
  501 CONTINUE  
      CLOSE(97)  
      RETURN  
      END  

