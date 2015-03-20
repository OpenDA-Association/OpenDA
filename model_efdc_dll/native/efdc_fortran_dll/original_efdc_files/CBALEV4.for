      SUBROUTINE CBALEV4  
C  
C CHANGE RECORD  
C **  SUBROUTINES CBALEV CALCULATE GLOBAL VOLUME, MASS, MOMENTUM,  
C **  AND ENERGY BALANCES  
C  
      USE GLOBAL  
C  
C **  CALCULATE MOMENTUM AND ENERGY DISSIPATION  
C  
      DO L=2,LA  
        LN=LNC(L)  
        UUEOUTE=UUEOUTE+0.5*SPB(L)*DXYP(L)*(U(L,1)*TBX(L)  
     &      +U(L+1,1)*TBX(L+1)  
     &      -U(L,KC)*TSX(L)-U(L+1,KC)*TSX(L+1))  
        VVEOUTE=VVEOUTE+0.5*SPB(L)*DXYP(L)*(V(L,1)*TBY(L)+V(LN,1)*
     &       TBX(LN)-V(L,KC)*TSY(L)-V(LN,KC)*TSX(LN))  
      ENDDO  
      DO K=1,KS  
        DO L=2,LA  
          LN=LNC(L)  
          DUTMP=0.5*( U(L,K+1)+U(L+1,K+1)-U(L,K)-U(L+1,K) )  
          DVTMP=0.5*( V(L,K+1)+V(LN,K+1)-V(L,K)-V(LN,K) )  
          UUEOUTE=UUEOUTE+SPB(L)*2.0*DXYP(L)*AV(L,K)  
     &        *( DUTMP*DUTMP )/(DZC(K+1)+DZC(K))  
          VVEOUTE=VVEOUTE+SPB(L)*2.0*DXYP(L)*AV(L,K)  
     &        *( DVTMP*DVTMP )/(DZC(K+1)+DZC(K))  
          BBEOUTE=BBEOUTE+SCB(L)*DXYP(L)*HP(L)  
     &        *GP*AB(L,K)*(B(L,K+1)-B(L,K))  
        ENDDO  
      ENDDO  
      RETURN  
      END  

