      SUBROUTINE CBALEV1  
C  
C CHANGE RECORD  
C **  SUBROUTINES CBALEV CALCULATE GLOBAL VOLUME, MASS, MOMENTUM,  
C **  AND ENERGY BALANCES  
C  
      USE GLOBAL  
      IF(NBALE.GT.1) RETURN  
C  
C **  INITIALIZE VOLUME, SALT MASS, DYE MASS, MOMENTUM, KINETIC ENERGY  
C **  AND POTENTIAL ENERGY, AND ASSOCIATED FLUXES  
C  
      VOLBEGE=0.  
      SALBEGE=0.  
      DYEBEGE=0.  
      UMOBEGE=0.  
      VMOBEGE=0.  
      UUEBEGE=0.  
      VVEBEGE=0.  
      PPEBEGE=0.  
      BBEBEGE=0.  
      VOLOUTE=0.  
      SALOUTE=0.  
      DYEOUTE=0.  
      UMOOUTE=0.  
      VMOOUTE=0.  
      UUEOUTE=0.  
      VVEOUTE=0.  
      PPEOUTE=0.  
      BBEOUTE=0.  
      DO L=2,LA  
        LN=LNC(L)  
        VOLBEGE=VOLBEGE+SPB(L)*DXYP(L)*H1P(L)  
        UMOBEGE=UMOBEGE+SPB(L)*0.5*DXYP(L)*H1P(L)  
     &      *(DYIU(L)*UHDY1E(L)/H1U(L)+DYIU(L+1)*UHDY1E(L+1)/H1U(L+1))  
        VMOBEGE=VMOBEGE+SPB(L)*0.5*DXYP(L)*H1P(L)  
     &      *(DXIV(L)*VHDX1E(L)/H1V(L)+DXIV(LN)*VHDX1E(LN)/H1V(LN))  
        PPEBEGE=PPEBEGE+SPB(L)*0.5*DXYP(L)  
     &      *(GI*P1(L)*P1(L)-G*BELV(L)*BELV(L))  
      ENDDO  
      AMOBEGE=SQRT(UMOBEGE*UMOBEGE+VMOBEGE*VMOBEGE)  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L)  
          SALBEGE=SALBEGE+SCB(L)*DXYP(L)*H1P(L)*SAL1(L,K)*DZC(K)  
          DYEBEGE=DYEBEGE+SCB(L)*DXYP(L)*H1P(L)*DYE1(L,K)*DZC(K)  
          UUEBEGE=UUEBEGE+SPB(L)*0.125*DXYP(L)*H1P(L)*DZC(K)  
     &        *( (U1(L,K)+U1(L+1,K))*(U1(L,K)+U1(L+1,K)) )  
          VVEBEGE=VVEBEGE+SPB(L)*0.125*DXYP(L)*H1P(L)*DZC(K)  
     &        *( (V1(L,K)+V1(LN,K))*(V1(L,K)+V1(LN,K)) )  
          BBEBEGE=BBEBEGE+SPB(L)*GP*DXYP(L)*H1P(L)*DZC(K)*( BELV(L)  
     &        +0.5*H1P(L)*(Z(K)+Z(K-1)) )*B1(L,K)  
        ENDDO  
      ENDDO  
      RETURN  
      END  

