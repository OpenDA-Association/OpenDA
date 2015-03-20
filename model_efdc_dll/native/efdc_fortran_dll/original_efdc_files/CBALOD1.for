      SUBROUTINE CBALOD1  
C  
C CHANGE RECORD  
C **  SUBROUTINES CBALOD CALCULATE GLOBAL VOLUME, MASS, MOMENTUM,  
C **  AND ENERGY BALANCES  
C  
      USE GLOBAL  
      IF(NBALO.GT.1) RETURN  
C  
C **  INITIALIZE VOLUME, SALT MASS, DYE MASS, MOMENTUM, KINETIC ENERGY  
C **  AND POTENTIAL ENERGY, AND ASSOCIATED FLUXES  
C  
      VOLBEGO=0.  
      SALBEGO=0.  
      DYEBEGO=0.  
      UMOBEGO=0.  
      VMOBEGO=0.  
      UUEBEGO=0.  
      VVEBEGO=0.  
      PPEBEGO=0.  
      BBEBEGO=0.  
      VOLOUTO=0.  
      SALOUTO=0.  
      DYEOUTO=0.  
      UMOOUTO=0.  
      VMOOUTO=0.  
      UUEOUTO=0.  
      VVEOUTO=0.  
      PPEOUTO=0.  
      BBEOUTO=0.  
      DO L=2,LA  
        LN=LNC(L)  
        VOLBEGO=VOLBEGO+SPB(L)*DXYP(L)*H1P(L)  
        UMOBEGO=UMOBEGO+SPB(L)*0.5*DXYP(L)*H1P(L)  
     &      *(DYIU(L)*UHDY1E(L)/H1U(L)+DYIU(L+1)*UHDY1E(L+1)/H1U(L+1))  
        VMOBEGO=VMOBEGO+SPB(L)*0.5*DXYP(L)*H1P(L)  
     &      *(DXIV(L)*VHDX1E(L)/H1V(L)+DXIV(LN)*VHDX1E(LN)/H1V(LN))  
        PPEBEGO=PPEBEGO+SPB(L)*0.5*DXYP(L)  
     &      *(GI*P1(L)*P1(L)-G*BELV(L)*BELV(L))  
      ENDDO  
      AMOBEGO=SQRT(UMOBEGO*UMOBEGO+VMOBEGO*VMOBEGO)  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L)  
          SALBEGO=SALBEGO+SCB(L)*DXYP(L)*H1P(L)*SAL1(L,K)*DZC(K)  
          DYEBEGO=DYEBEGO+SCB(L)*DXYP(L)*H1P(L)*DYE1(L,K)*DZC(K)  
          UUEBEGO=UUEBEGO+SPB(L)*0.125*DXYP(L)*H1P(L)*DZC(K)  
     &        *( (U1(L,K)+U1(L+1,K))*(U1(L,K)+U1(L+1,K)) )  
          VVEBEGO=VVEBEGO+SPB(L)*0.125*DXYP(L)*H1P(L)*DZC(K)  
     &        *( (V1(L,K)+V1(LN,K))*(V1(L,K)+V1(LN,K)) )  
          BBEBEGO=BBEBEGO+SPB(L)*GP*DXYP(L)*H1P(L)*DZC(K)*( BELV(L)  
     &        +0.5*H1P(L)*(Z(K)+Z(K-1)) )*B1(L,K)  
        ENDDO  
      ENDDO  
      RETURN  
      END  

