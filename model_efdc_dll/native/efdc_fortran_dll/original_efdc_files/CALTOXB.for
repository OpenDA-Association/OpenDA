      SUBROUTINE CALTOXB  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALSND CALCULATES NONCOHESIVER SEDIMENT SETTLING,  
C **  DEPOSITION AND RESUSPENSION AND IS CALLED FOR SSEDTOX  
C  
      USE GLOBAL  

      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::DIFTOXBW  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::PARTDIF  
      IF(.NOT.ALLOCATED(DIFTOXBW))THEN
		ALLOCATE(DIFTOXBW(LCM))
		ALLOCATE(PARTDIF(LCM,KBM))
		DIFTOXBW=0.0 
		PARTDIF=0.0 
	ENDIF
C  
C **  UPDATE TOTAL PARTICULATE FRACTION OF EACH TOXIC IN THE BED  
C  
      DO NT=1,NTOX  
        NSP2(NT)=NSED+NSND  
        IF(ISTOC(NT).EQ.1) NSP2(NT)=NSP2(NT)+2  
        IF(ISTOC(NT).EQ.2) NSP2(NT)=NSP2(NT)+1  
      END DO  
      DO NT=1,NTOX  
        DO NS=1,NSP2(NT)  
          DO K=1,KB  
            DO L=2,LA  
              TOXPFB(L,K,NS,NT)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDDO  
      DO NT=1,NTOX  
        IF(ISTRAN(6).GE.1)THEN  
          DO NS=1,NSED  
            DO K=1,KB  
              DO L=2,LA  
                TOXPFB(L,K,NS,NT)=SEDB(L,K,NS)*STFPOCB(L,K,NS)  
     &              *TOXPARB(NS,NT)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTRAN(7).GE.1)THEN  
          DO NX=1,NSND  
            NS=NX+NSED  
            DO K=1,KB  
              DO L=2,LA  
                TOXPFB(L,K,NS,NT)=SNDB(L,K,NX)*STFPOCB(L,K,NS)  
     &              *TOXPARB(NS,NT)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTOC(NT).EQ.1.OR.ISTOC(NT).EQ.2)THEN  
          NS=1+NSED+NSND  
          DO K=1,KB  
            DO L=2,LA  
              TOXPFB(L,K,NS,NT)=PORBED(L,K)*HBED(L,K)  
     &            *STDOCB(L,K)*TOXPARBC(1,NT)  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISTOC(NT).EQ.1)THEN  
          NS=2+NSED+NSND  
          DO K=1,KB  
            DO L=2,LA  
              TOXPFB(L,K,NS,NT)=HBED(L,K)  
     &            *STPOCB(L,K)*TOXPARBC(2,NT)  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDDO  
      DO NT=1,NTOX  
        DO K=1,KB  
          DO L=2,LA  
            TOXPFTB(L,K,NT)=0.  
          ENDDO  
        ENDDO  
        DO NS=1,NSP2(NT)  
          DO K=1,KB  
            DO L=2,LA  
              TOXPFTB(L,K,NT)=TOXPFTB(L,K,NT)+TOXPFB(L,K,NS,NT)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDDO  
      DO NT=1,NTOX  
        DO NS=1,NSP2(NT)  
          DO K=1,KB  
            DO L=2,LA  
              IF(HBED(L,K).GT.0.0)THEN  
                TOXPFB(L,K,NS,NT)=TOXPFB(L,K,NS,NT)/  
     &              (PORBED(L,K)*HBED(L,K)+TOXPFTB(L,K,NT))  
              ELSE  
                TOXPFB(L,K,NS,NT)=0.  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDDO  
      NFD=NSED+NSND+1  
      DO NT=1,NTOX  
        DO K=1,KB  
          DO L=2,LA  
            IF(HBED(L,K).GT.0.0)THEN  
              TOXFDFB(L,K,NT)=PORBED(L,K)*HBED(L,K)  
     &            /(PORBED(L,K)*HBED(L,K)+TOXPFTB(L,K,NT))  
              TOXCDFB(L,K,NT)=TOXPFB(L,K,NFD,NT)  
            ELSE  
              TOXFDFB(L,K,NT)=0.  
              TOXCDFB(L,K,NT)=0.  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDDO  
      DO NT=1,NTOX  
        DO K=1,KB  
          DO L=2,LA  
            IF(HBED(L,K).GT.0.0)THEN  
              TOXPFTB(L,K,NT)=( TOXPFTB(L,K,NT)  
     &            /(PORBED(L,K)*HBED(L,K)+TOXPFTB(L,K,NT)) )  
     &            -TOXPFB(L,K,NFD,NT)  
            ELSE  
              TOXPFTB(L,K,NT)=0.  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDDO  
C  
C **  ADVECT AND DIFFUSE TOXICS IN BED AND INTO BOTTOM WATER  
C **  COLUMN LAYER  
C  
      IF(ISTRAN(5).GE.1)THEN  
        DO NT=1,NTOX  
C  
C **  ADD PARTICLE MIXING AND SCALE PARTICAL DIFFUSION FOR SOLUTION  
C  
          DO L=2,LA  
            DEPINBED=0.  
            PARTDIF(L,KBT(L))=0.0  
            IF(KBT(L).GT.2)THEN  
              DO K=KBT(L),2,-1  
                KM=K-1  
                DEPINBED=DEPINBED+HBED(L,K)  
                PARTDIF(L,KM)=0.0  
                IF(DEPINBED.LT.DPDIFTOX(NT))THEN  
                  PARTDIF(L,KM)=2.*PDIFTOX(NT)/  
     &                (2.0-TOXPFTB(L,K,NT)-TOXPFTB(L,KM,NT))  
                ENDIF  
              ENDDO  
            ENDIF  
            IF(KBT(L).EQ.2)THEN  
              K=2  
              KM=K-1  
              DEPINBED=DEPINBED+HBED(L,K)  
              PARTDIF(L,KM)=0.0  
              IF(DEPINBED.LT.DPDIFTOX(NT))THEN  
                PARTDIF(L,KM)=2.*PDIFTOX(NT)/  
     &              (2.0-TOXPFTB(L,K,NT)-TOXPFTB(L,KM,NT))  
              ENDIF  
            ENDIF  
          ENDDO  
          DO L=2,LA  
            DIFTOXBW(L)=0.0  
          ENDDO  
          DO L=2,LA  
            IF(LMASKDRY(L)) DIFTOXBW(L)=DIFTOXS(NT)  
          ENDDO  
          DO L=2,LA  
	      DIFBWFAC=2./HBED(L,KBT(L))
	      IF(ISDIFBW(NT).EQ.1)DIFBWFAC=1.0
            TOXBBALO(L)=0.  
            KBTP1=KBT(L)+1  
            ALOW(L,1)=0.  
            CUPP(L,KBTP1)=0.  
            DO K=1,KBT(L)-1  
              CUPP(L,K)=MIN(QWTRBED(L,K),0.)  
     &           -(DIFTOX(NT)+PARTDIF(L,K))*(PORBED(L,K)+PORBED(L,K+1))/  
     &            (HBED(L,K)+HBED(L,K+1))  
            ENDDO  
            CUPP(L,KBT(L))=MIN(QWTRBED(L,KBT(L)),0.)
     &                 -DIFBWFAC*DIFTOXBW(L)*PORBED(L,KBT(L))
            DO K=2,KBTP1  
              ALOW(L,K)=-MAX(QWTRBED(L,K-1),0.)  
     &            -(DIFTOX(NT)+PARTDIF(L,K-1))*(PORBED(L,K-1)+PORBED(
     &                  L,K))/(HBED(L,K-1)+HBED(L,K))  
            ENDDO  
            ALOW(L,KBTP1)=-MAX(QWTRBED(L,KBT(L)),0.)
     &                 -DIFBWFAC*DIFTOXBW(L)*PORBED(L,KBT(L))
            DO K=1,KBT(L)  
              BMNN(L,K)=DELTI*HBED(L,K)*PORBED(L,K)/  
     &            (1.-TOXPFTB(L,K,NT))  
            ENDDO  
            BMNN(L,1)=BMNN(L,1)-MIN(QWTRBED(L,0),0.)  
            BMNN(L,KBTP1)=DELTI*DZC(1)*HP(L)/(1.-TOXPFTW(L,1,NT))  
            DO K=1,KBTP1  
              BMNN(L,K)=BMNN(L,K)-ALOW(L,K)-CUPP(L,K)  
            ENDDO  
            DO K=1,KBT(L)  
              RRHS(L,K)=DELTI*TOXB(L,K,NT)  
              TOXBBALO(L)=TOXBBALO(L)+TOXB(L,K,NT)  
            ENDDO  
            RRHS(L,1)=RRHS(L,1)+MAX(QWTRBED(L,0),0.)*CONGW(L,NT+4)  
            RRHS(L,KBTP1)=DELTI*DZC(1)*HP(L)*TOX(L,1,NT)  
            TOXWBALO(L)=DZC(1)*HP(L)*TOX(L,1,NT)  
          ENDDO  
          DO L=2,LA  
            KBTP1=KBT(L)+1  
            BETTMP=BMNN(L,1)  
            TOXTMP(L,1)=RRHS(L,1)/BETTMP  
            DO KK=2,KBTP1  
              GAMTMP(L,KK)=CUPP(L,KK-1)/BETTMP  
              BETTMP=BMNN(L,KK)-ALOW(L,KK)*GAMTMP(L,KK)  
              TOXTMP(L,KK)=(RRHS(L,KK)-ALOW(L,KK)*TOXTMP(L,KK-1))/  
     &            BETTMP  
            ENDDO  
            DO KK=KBT(L),1,-1  
              TOXTMP(L,KK)=TOXTMP(L,KK)-GAMTMP(L,KK+1)*TOXTMP(L,KK+1)  
            ENDDO  
          ENDDO  
          DO L=2,LA  
            TOXBBALN(L)=0.0  
            KBTP1=KBT(L)+1  
            DO K=1,KBT(L)  
              TOXB(L,K,NT)=HBED(L,K)*PORBED(L,K)*TOXTMP(L,K)/  
     &            (1.-TOXPFTB(L,K,NT))  
              TOXBBALN(L)=TOXBBALN(L)+TOXB(L,K,NT)  
            ENDDO  
            TOX(L,1,NT)=TOXTMP(L,KBTP1)/(1.-TOXPFTW(L,1,NT))  
            TOXWBALN(L)=DZC(1)*HP(L)*TOX(L,1,NT)  
          ENDDO  
          DO L=2,LA  
            ERRT=TOXBBALN(L)+TOXWBALN(L)-TOXBBALO(L)-TOXWBALO(L)  
            ERRB=ERRT*TOXBBALN(L)/(TOXBBALN(L)+TOXWBALN(L))  
            ERRW=ERRT-ERRB  
            TOX(L,1,NT)=TOX(L,1,NT)-ERRW/(DZC(1)*HP(L))  
            DO K=1,KBT(L)  
              DERRB(K)=TOXB(L,K,NT)/TOXBBALN(L)  
            ENDDO  
            DO K=1,KBT(L)  
              TOXB(L,K,NT)=TOXB(L,K,NT)-DERRB(K)*ERRB  
            ENDDO  
            TADFLUX(L,NT)=DELTI*(DZC(1)*HP(L)*TOX(L,1,NT)-TOXWBALO(L))  
          ENDDO  
        ENDDO  
      ENDIF  
 8999 FORMAT(' TAD  ',2I10,5E14.5,2F10.5)  
      RETURN  
      END  

