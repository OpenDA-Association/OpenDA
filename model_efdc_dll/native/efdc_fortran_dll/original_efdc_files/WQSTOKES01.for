      SUBROUTINE WQSTOKES01(WQKESS1,L,K,nsp)
C  GEOSR STOKES : YSSONG 2015.08.18   
C: STOKES VELOCITY CALCULATION REFERRED TO Kromkamp and Walsby(1990) 
C: LAST MODIFIED BY GEOSR YSSONG ON 4 SEPTEMBER 2015

      USE GLOBAL  
      
C    ALGAL DENSITY
            WQSOLDEP=(WQHT(K)+DZC(K)/2.)*HP(L)
            WQSOL1=SOLSWRT(L)*EXP(-WQKESS1*WQSOLDEP)
            WQSOL=WQSOL1*Light_Factor1*F_PAR

C    SOLARADIATION AVERAGE DURING DAYTIME
            IF(WQSOL.GT.0.1)THEN                      ! DAYLIGHT-TIME    
              WQSOLDAX(L,K,nsp)=0.0
              WQSOLSUMX(L,K,nsp)=WQSOLSUMX(L,K,nsp)+WQSOL
              NSOLDAX(L,K,nsp)=NSOLDAX(L,K,nsp)+1
              IDLIGHTX(L,K,nsp)=1
            ELSEIF(WQSOL.LE.0.1.AND.IDLIGHTX(L,K,nsp).EQ.1)THEN    ! SUNSET-TIME
              WQSOLDAX(L,K,nsp)=WQSOLSUMX(L,K,nsp)
     &                         /FLOAT(NSOLDAX(L,K,nsp))
              NSOLDAX(L,K,nsp)=0
              WQSOLSUMX(L,K,nsp)=0.0
              IDLIGHTX(L,K,nsp)=0
            ENDIF    

            WQRHOX(L,K,nsp)=WQRHOX(L,K,nsp)+DTWQ*
     &           (WQCOEF1X(nsp)*(WQSOL/(WQIRHALFX(nsp)+WQSOL)))
     &           +DTWQ*(-WQCOEF2X(nsp)*WQSOLDAX(L,K,nsp)-WQCOEF3X(nsp))
            WQRHOX(L,K,nsp)=MIN(WQRHOX(L,K,nsp),WQRHOMXX(nsp))
            WQRHOX(L,K,nsp)=MAX(WQRHOX(L,K,nsp),WQRHOMNX(nsp))
            
C    VISCOSITY OF THE WATER
            WQVIS=(10.*EXP(-1.65+(262./(TWQ(L)+139.))))/1000.
            
C    SETTLING VELOCITY (SINKING VELOCITY)
!{ GEOSR X-species : jgcho 2015.10.08
!            WQALSET(L,K,1) = 2.*9.81*WQR*WQR*(WQRHO(L,K)-1000.)*WQA  
!     &                      /(9.*WQRESIS*WQVIS)
!            WQALSET(L,K,1) = WQALSET(L,K,1)*DZWQ(L)*86400. 
            WQALSETX(L,K,nsp) = 2.*9.81*WQRX(nsp)*WQRX(nsp)
     &                         *(WQRHOX(L,K,nsp)-1000.)*WQAX(nsp)  
     &                         /(9.*WQRESISX(nsp)*WQVIS)
            WQALSETX(L,K,nsp) = WQALSETX(L,K,nsp)*DZWQ(L)*86400. 
!} GEOSR X-species : jgcho 2015.10.08
                 
      RETURN  
      END  
