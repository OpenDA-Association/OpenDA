      SUBROUTINE CALBAL1  
C  
C CHANGE RECORD  
C **  SUBROUTINES CALBAL CALCULATE GLOBAL VOLUME, MASS, MOMENTUM,  
C **  AND ENERGY BALANCES  
C  
      USE GLOBAL  
	IMPLICIT NONE
	INTEGER::L,LN,K
      IF(NBAL.GT.1) RETURN  
C  
C **  INITIALIZE VOLUME, SALT MASS, DYE MASS, MOMENTUM, KINETIC ENERGY  
C **  AND POTENTIAL ENERGY, AND ASSOCIATED FLUXES  
C  
      VOLBEG=0.  
      SALBEG=0.  
      DYEBEG=0.  
      UMOBEG=0.  
      VMOBEG=0.  
      UUEBEG=0.  
      VVEBEG=0.  
      PPEBEG=0.  
      BBEBEG=0.  
      VOLOUT=0.  
      SALOUT=0.  
      DYEOUT=0.  
      UMOOUT=0.  
      VMOOUT=0.  
      UUEOUT=0.  
      VVEOUT=0.  
      PPEOUT=0.  
      BBEOUT=0.  
      DO L=2,LA  
        LN=LNC(L)  
        VOLBEG=VOLBEG+SPB(L)*DXYP(L)*HP(L)  
        UMOBEG=UMOBEG+SPB(L)*0.5*DXYP(L)*HP(L)*(DYIU(L)*HUI(L)*UHDYE(L)  
     &      +DYIU(L+1)*HUI(L+1)*UHDYE(L+1))  
        VMOBEG=VMOBEG+SPB(L)*0.5*DXYP(L)*HP(L)*(DXIV(L)*HVI(L)*VHDXE(L)  
     &      +DXIV(LN)*HVI(LN)*VHDXE(LN))  
        PPEBEG=PPEBEG+SPB(L)*0.5*DXYP(L)  
     &      *(GI*P(L)*P(L)-G*BELV(L)*BELV(L))  
      ENDDO  
      AMOBEG=SQRT(UMOBEG*UMOBEG+VMOBEG*VMOBEG)  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L)  
          SALBEG=SALBEG+SCB(L)*DXYP(L)*HP(L)*SAL(L,K)*DZC(K)  
          DYEBEG=DYEBEG+SCB(L)*DXYP(L)*HP(L)*DYE(L,K)*DZC(K)  
          UUEBEG=UUEBEG+SPB(L)*0.125*DXYP(L)*HP(L)*DZC(K)  
     &        *( (U(L,K)+U(L+1,K))*(U(L,K)+U(L+1,K)) )  
          VVEBEG=VVEBEG+SPB(L)*0.125*DXYP(L)*HP(L)*DZC(K)  
     &        *( (V(L,K)+V(LN,K))*(V(L,K)+V(LN,K)) )  
          BBEBEG=BBEBEG+SPB(L)*GP*DXYP(L)*HP(L)*DZC(K)*( BELV(L)  
     &        +0.5*HP(L)*(Z(K)+Z(K-1)) )*B(L,K)  
        ENDDO  
      ENDDO  
      RETURN  
      END  

