      SUBROUTINE CALEBI  
C  
C CHANGE RECORD  
C **  CALEBI CALCULATES THE EXTERNAL BUOYANCY INTEGRALS  
C  
      USE GLOBAL  
	IMPLICIT NONE
      INTEGER::K,L,LLCM
      REAL::DBK,DZCBK

      REAL*4 DZCB(KCM)
      REAL*4 BK(KCM)

      PARAMETER(LLCM=200)

      DO L=2,LA 

        BI1(L)=0.
        BI2(L)=0.
        BE(L)=0.
        
        DO K=1,KC
          DZCB(K)=DZC(K)*B(L,K)
        ENDDO

        DBK=0.  
        DO K=KC,1,-1
          DBK=DBK+DZCB(K)         !DZC(K)*B(L,K)
          BK(K)=DBK-0.5*DZCB(K)   !DZC(K)*B(L,K)
        ENDDO

        !Z(0)=0.
        !Z(K)=Z(K-1)+DZC(K)
        DO K=1,KC
          BE(L) =BE(L)+DZCB(K)   !DZC(K)*B(L,K)
          DZCBK =DZC(K)*BK(K)
          BI1(L)=BI1(L)+DZCBK  
          BI2(L)=BI2(L)+(DZCBK+0.5*(Z(K)+Z(K-1))*DZCB(K)) 
        ENDDO

      ENDDO

      RETURN  
      END  

