      SUBROUTINE BUDGET2  
C  
C **  ADDED BY DON KINGERY, CH2M-HILL ON 15 OCTOBER 1996  
C CHANGE RECORD  
C **  SUBROUTINES BUDGETN CALCULATE SEDIMENT BUDGET (TOTAL SEDIMENTS)  
C  
      USE GLOBAL  
	IMPLICIT NONE
	INTEGER::LS,NT,LL,K,L,LN
C  
C **  ACCUMULATE FLUXES ACROSS OPEN BOUNDARIES  
C  
      DO K=1,KC  
        DO LL=1,NCBS  
          L=LCBS(LL)  
          LN=LNC(L)  
          VOLMOUT=VOLMOUT-VHDX2(LN,K)*DZC(K)  
          SMASSOUT=SMASSOUT-MIN(VHDX2(LN,K),0.)*SAL1(LN,K)*DZC(K)  
     &        -MAX(VHDX2(LN,K),0.)*SAL1(L,K)*DZC(K)  
C  
C  ADDED SEDIMENT FLUXES         DLK 9/27  
C  
          DO NT=1,NSED  
            SEDOUT=SEDOUT-MIN(VHDX2(LN,K),0.)*SED1(LN,K,NT)*DZC(K)  
     &          -MAX(VHDX2(LN,K),0.)*SED1(L,K,NT)*DZC(K)  
          ENDDO  
          DO NT=1,NSND  
            SEDOUT=SEDOUT-MIN(VHDX2(LN,K),0.)*SND1(LN,K,NT)*DZC(K)  
     &          -MAX(VHDX2(LN,K),0.)*SND1(L,K,NT)*DZC(K)  
          ENDDO  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO LL=1,NCBW  
          L=LCBW(LL)  
          VOLMOUT=VOLMOUT-UHDY2(L+1,K)*DZC(K)  
          SMASSOUT=SMASSOUT-MIN(UHDY2(L+1,K),0.)*SAL1(L+1,K)*DZC(K)  
     &        -MAX(UHDY2(L+1,K),0.)*SAL1(L,K)*DZC(K)  
C  
C  ADDED SEDIMENT FLUXES         DLK 10/15  
C  
          DO NT=1,NSED  
            SEDOUT=SEDOUT-MIN(UHDY2(L+1,K),0.)*SED1(L+1,K,NT)*DZC(K)  
     &          -MAX(UHDY2(L+1,K),0.)*SED1(L,K,NT)*DZC(K)  
          ENDDO  
          DO NT=1,NSND  
            SEDOUT=SEDOUT-MIN(UHDY2(L+1,K),0.)*SND1(L+1,K,NT)*DZC(K)  
     &          -MAX(UHDY2(L+1,K),0.)*SND1(L,K,NT)*DZC(K)  
          ENDDO  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO LL=1,NCBE  
          L=LCBE(LL)  
          VOLMOUT=VOLMOUT+UHDY2(L,K)*DZC(K)  
          SMASSOUT=SMASSOUT+MIN(UHDY2(L,K),0.)*SAL1(L,K)*DZC(K)  
     &        +MAX(UHDY2(L,K),0.)*SAL1(L-1,K)*DZC(K)  
C  
C  ADDED SEDIMENT FLUXES         DLK 10/15  
C  
          DO NT=1,NSED  
            SEDOUT=SEDOUT+MIN(UHDY2(L,K),0.)*SED1(L,K,NT)*DZC(K)  
     &          +MAX(UHDY2(L,K),0.)*SED1(L-1,K,NT)*DZC(K)  
          ENDDO  
          DO NT=1,NSND  
            SEDOUT=SEDOUT+MIN(UHDY2(L,K),0.)*SND1(L,K,NT)*DZC(K)  
     &          +MAX(UHDY2(L,K),0.)*SND1(L-1,K,NT)*DZC(K)  
          ENDDO  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO LL=1,NCBN  
          L=LCBN(LL)  
          LS=LSC(L)  
          VOLMOUT=VOLMOUT+VHDX2(L,K)*DZC(K)  
          SMASSOUT=SMASSOUT+MIN(VHDX2(L,K),0.)*SAL1(L,K)*DZC(K)  
     &        +MAX(VHDX2(L,K),0.)*SAL1(LS,K)*DZC(K)  
C  
C  ADDED SEDIMENT FLUXES         DLK 10/15  
C  
          DO NT=1,NSED  
            SEDOUT=SEDOUT+MIN(VHDX2(L,K),0.)*SED1(L,K,NT)*DZC(K)  
     &          +MAX(VHDX2(L,K),0.)*SED1(LS,K,NT)*DZC(K)  
          ENDDO  
          DO NT=1,NSND  
            SEDOUT=SEDOUT+MIN(VHDX2(L,K),0.)*SND1(L,K,NT)*DZC(K)  
     &          +MAX(VHDX2(L,K),0.)*SND1(LS,K,NT)*DZC(K)  
          ENDDO  
        ENDDO  
      ENDDO  
C  
C **  ACCUMULATE FLUX OF SED POSITIVE FROM BED TO WATER COLUMN  
C  
      RETURN  
      END  

