      SUBROUTINE CBALOD2  
C  
C CHANGE RECORD  
C **  SUBROUTINES CBALOD CALCULATE GLOBAL VOLUME, MASS, MOMENTUM,  
C **  AND ENERGY BALANCES  
C  
      USE GLOBAL  
C  
C **  ACCUMULATE FLUXES ACROSS OPEN BOUNDARIES  
C  
      DO K=1,KC  
        DO LL=1,NCBS  
          L=LCBS(LL)  
          LN=LNC(L)  
          VOLOUTO=VOLOUTO-VHDX2(LN,K)*DZC(K)  
          SALOUTO=SALOUTO-MIN(VHDX2(LN,K),0.)*SAL1(LN,K)*DZC(K)  
     &        -MAX(VHDX2(LN,K),0.)*SAL1(L,K)*DZC(K)  
          DYEOUTO=DYEOUTO-MIN(VHDX2(LN,K),0.)*DYE1(LN,K)*DZC(K)  
     &        -MAX(VHDX2(LN,K),0.)*DYE1(L,K)*DZC(K)  
          PPEOUTO=PPEOUTO-VHDX2(LN,K)*G*DZC(K)*( 0.5*(BELV(L)+BELV(LN))  
     &        +0.125*(HP(L)+H2P(L)+HP(LN)+H2P(LN))*(Z(K)+Z(K-1)) )  
          BBEOUTO=BBEOUTO-MIN(VHDX2(LN,K),0.)*DZC(K)*GP*( BELV(LN)  
     &        +0.5*HP(LN)*(Z(K)+Z(K-1)) )*B1(LN,K)  
     &        -MAX(VHDX2(LN,K),0.)*DZC(K)*GP*( BELV(L)  
     &        +0.5*HP(L)*(Z(K)+Z(K-1)) )*B1(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO LL=1,NCBW  
          L=LCBW(LL)  
          VOLOUTO=VOLOUTO-UHDY2(L+1,K)*DZC(K)  
          SALOUTO=SALOUTO-MIN(UHDY2(L+1,K),0.)*SAL1(L+1,K)*DZC(K)  
     &        -MAX(UHDY2(L+1,K),0.)*SAL1(L,K)*DZC(K)  
          DYEOUTO=DYEOUTO-MIN(UHDY2(L+1,K),0.)*DYE1(L+1,K)*DZC(K)  
     &        -MAX(UHDY2(L+1,K),0.)*DYE1(L,K)*DZC(K)  
          PPEOUTO=PPEOUTO-UHDY2(L+1,K)*G*DZC(K)*(0.5*(BELV(L)+BELV(L+1))  
     &        +0.125*(HP(L)+H2P(L)+HP(L+1)+H2P(L+1))*(Z(K)+Z(K-1)) )  
          BBEOUTO=BBEOUTO-MIN(UHDY2(L+1,K),0.)*DZC(K)*GP*( BELV(L+1)  
     &        +0.5*HP(L+1)*(Z(K)+Z(K-1)) )*B1(L+1,K)  
     &        -MAX(UHDY2(L+1,K),0.)*DZC(K)*GP*( BELV(L)  
     &        +0.5*HP(L)*(Z(K)+Z(K-1)) )*B1(L,K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO LL=1,NCBE  
          L=LCBE(LL)  
          VOLOUTO=VOLOUTO+UHDY2(L,K)*DZC(K)  
          SALOUTO=SALOUTO+MIN(UHDY2(L,K),0.)*SAL1(L,K)*DZC(K)  
     &        +MAX(UHDY2(L,K),0.)*SAL1(L-1,K)*DZC(K)  
          DYEOUTO=DYEOUTO+MIN(UHDY2(L,K),0.)*DYE1(L,K)*DZC(K)  
     &        +MAX(UHDY2(L,K),0.)*DYE1(L-1,K)*DZC(K)  
          PPEOUTO=PPEOUTO+UHDY2(L,K)*G*DZC(K)*( 0.5*(BELV(L)+BELV(L-1))  
     &        +0.125*(HP(L)+H2P(L)+HP(L-1)+H2P(L-1))*(Z(K)+Z(K-1)) )  
          BBEOUTO=BBEOUTO+MIN(UHDY2(L,K),0.)*DZC(K)*GP*(BELV(L)  
     &        +0.5*HP(L)*(Z(K)+Z(K-1)) )*B1(L,K)  
     &        +MAX(UHDY2(L,K),0.)*DZC(K)*GP*(BELV(L-1)  
     &        +0.5*HP(L-1)*(Z(K)+Z(K-1)) )*B1(L-1,K)  
        ENDDO  
      ENDDO  
      DO K=1,KC  
        DO LL=1,NCBN  
          L=LCBN(LL)  
          LS=LSC(L)  
          VOLOUTO=VOLOUTO+VHDX2(L,K)*DZC(K)  
          SALOUTO=SALOUTO+MIN(VHDX2(L,K),0.)*SAL1(L,K)*DZC(K)  
     &        +MAX(VHDX2(L,K),0.)*SAL1(LS,K)*DZC(K)  
          DYEOUTO=DYEOUTO+MIN(VHDX2(L,K),0.)*DYE1(L,K)*DZC(K)  
     &        +MAX(VHDX2(L,K),0.)*DYE1(LS,K)*DZC(K)  
          PPEOUTO=PPEOUTO+VHDX2(L,K)*G*DZC(K)*( 0.5*(BELV(L)+BELV(LS))  
     &        +0.125*(HP(L)+H2P(L)+HP(LS)+H2P(LS))*(Z(K)+Z(K-1)) )  
          BBEOUTO=BBEOUTO+MIN(VHDX2(L,K),0.)*DZC(K)*GP*( BELV(L)  
     &        +0.5*HP(L)*(Z(K)+Z(K-1)) )*B1(L,K)  
     &        +MAX(VHDX2(L,K),0.)*DZC(K)*GP*( BELV(LS)  
     &        +0.5*HP(LS)*(Z(K)+Z(K-1)) )*B1(LS,K)  
        ENDDO  
      ENDDO  
      RETURN  
      END  

