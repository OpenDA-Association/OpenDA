      SUBROUTINE TOXCHEM  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALSND CALCULATES NONCOHESIVER SEDIMENT SETTLING,  
C **  DEPOSITION AND RESUSPENSION AND IS CALLED FOR SSEDTOX  
C  
C
C GEOSR MODIFY 2010.11.02, 2014.09.11
C
C ** ADD THE DECAY CONDITION (Volatilization, Hydrolysis, Photolysis, Biodegradation
C

      USE GLOBAL  
!{GeoSR, 2014.09.16. YSSONG
      INTEGER::L,K,NT
      REAL TXKL,TXKLL
      TXKL=0.0
      TXKLL=0.0
!}      
      IF(ISTRAN(5).GE.1)THEN  

        DO NT=1,NTOX  
         RKTOXWH=0.
         RKTOXWB=0.
         DO L=2,LA
          RKTOXWV(L)=0.
          RKTOXWP(L)=0.
         ENDDO
C  
C **     NOTES:  
C        BULK DECAY COEFFICIENT  
C        VOLITIZATION  
C          VOLTOX(NT)  
C          RMOLTX(NT)=MOLECULAR WEIGHT  
C        PHOTOLOSIS  
C          RKTOXP(NT)=BASE RATE  
C          SKTOXP(NT)=SOLAR RADIATION AT BASE RATE  
C  

!{GeoSR, YSSONG, TOX, 101029
         IF(ISTXD1ST.NE.1)THEN                    
C Volatilization ----------------------------------------------------------
C OXYGEN TRANSPORT COEFF.
  ! ISTXKLL : OPTION FOR OXYGEN TRANSPORT COEFF. CALCULATION
  ! TXKLL   : OXYGEN TRANSPORT COEFF.
  ! TXSC    : SCHEMIT NUMBER(500)
  ! TXWSPD  : WIND SPEED

          IF(ISTXDCV.EQ.1)THEN                   
           IF(ISTXKLL.EQ.1)THEN
             TXKLL=0.864*TXWSPD  ! Broecker et al.(1978)
           ELSEIF(ISTXKLL.EQ.2)THEN
             TXKLL=0.728*TXWSPD**0.5-0.317*TXWSPD+0.0372*TXWSPD**2.  ! Banks(1975), Banks and Herrera(1977)
           ELSEIF(ISTXKLL.EQ.3)THEN
             TXKLL=0.108*TXWSPD**1.64*(TXSC/600.)**0.5                 ! Wanninkhof et al.(1991)
           ENDIF
C LIQUID FILM COEFF.
  ! ISTXKL   : OPTION FOR LIQUID FILM COEFF. CALCULATION
  ! TXKL     : LIQUID FILM COEFF.
  ! TXMW     : MOLECULAR WEIGHT OF TOXICANT
  ! RKTOXWV  : Volatilization DECAY RATE
           IF(ISTXKL.EQ.1)THEN
             TXKL=(32./TXMW)**0.25 * TXKLL  ! MILLS(1982)
           ! ELSEIF(ISTXKL.EQ.1)THEN
           ELSEIF(ISTXKL.EQ.2)THEN  !GEOSR 2013. 4.30
             TXKL=0.655 * TXKLL                 ! Rathbun and Tai(1982)
           ENDIF

C GAS FILM COEFF.
  ! TXKG     : GAS FILM COEFF.
           TXKG=168.*(18./TXMW)**0.25*TXWSPD

C Volatilization RATE
           TXHE1=TXHE/(8.206*0.00001*(TXTEM+273.15))
           TXKALI=1./TXKL+1./(TXKG*TXHE1)     ! Volatilization RATE
           TXKAL=1./TXKALI

C Volatilization DECAY RATE
           DO L=2,LA 
!             RKTOXWV(L)=TXKAL !/HP(L)   ! ??? /HP(L)  ???
             RKTOXWV(L)=TXKAL/HP(L)   ! GEOSR 2013.4.30
           ENDDO
          ENDIF
C
C Hydrolysis --------------------------------------------------------------
C
  ! TXKH     : Hydrolysis RATE
  ! RKTOXWH  : Hydrolysis DECAY RATE
!          IF(ISTXDCH.EQ.1.AND.N.EQ.1)THEN                    
          IF(ISTXDCH.EQ.1)THEN !GEOSR 2013. 4.30
           RKTOXWH=TXKH
          ENDIF     
C 
C Photolysis --------------------------------------------------------------
C
  ! TXKDO    : LIGHT DECAY RATE AT SURFACE LAYER
  ! TXI      : I
  ! TXI0     : I0
  ! RKTOXWP  : Photolysis DECAY RATE
  ! TXD      : RADIANCE DISTRIBUTION FUNCTION  (1.2~1.6)
  ! TXD0     : RADIANCE DISTRIBUTION FUNCTION AT SURFACE (1.2)
  ! TXKE     : EXTINCTION COEFF. AT LAMBDA_MAX
  ! TXAW     : ATTENUATION COEFF. OF WATER
  ! TXAA     : ATTENUATION COEFF. OF CHL-A
  ! TXAC     : ATTENUATION COEFF. OF DOC
  ! TXAS     : ATTENUATION COEFF. OF SSC
  ! TXCHLA   : CHL-A CONC.
  ! TXDOC    : DOC CONC.
  ! TXCSSC   : SSC

          IF(ISTXDCP.EQ.1)THEN                    
           TXKE=TXD*(TXAW+TXAA*TXCHLA+TXAC*TXDOC+TXAS*TXSSC)
           TXKP1=TXKDO*TXI/TXI0*TXD/TXD0
           DO L=2,LA 
             TXKP=TXKP1*(1.-EXP(-TXKE*HP(L)))/(TXKE*HP(L))
             RKTOXWP(L)=TXKP
           ENDDO
          ENDIF
C
C Biodegradation ----------------------------------------------------------
C
  ! TXKB     : REFERENCE Biodegradation RATE
  ! TXTHETA  : TEM. COMPENSATION COEFF.
  ! TXTEM    : WATER TEM.
      
!          IF(ISTXDCB.EQ.1.AND.N.EQ.1)THEN                    
          IF(ISTXDCB.EQ.1)THEN                    !GEOSR 2013.4.30
           RKTOXWB=(TXKB)*TXTHETA**(TXTEM-20.)
          ENDIF

C SUMMATION ---------------------------------------------------------------
!{GeoSR, 2014.09.16. YSSONG
C          DO L=2,LA 
C            RKTOXWT(L,NT)=(RKTOXWV(L)+RKTOXWH+RKTOXWP(L)+RKTOXWB)/86400.
C          ENDDO
          DO K=1,KC     
          DO L=2,LA 
            RKTOXWT(L,K,NT)=(RKTOXWV(L)+RKTOXWH+RKTOXWP(L)+RKTOXWB)
     &                     /86400.
          ENDDO
          ENDDO
!}
         ELSE
C
C 1ST ORDER DECAY----------------------------------------------------------
C
!{GeoSR, 2014.09.16. YSSONG
C         DO L=2,LA
C          RKTOXWT(L,NT)=TX1ST/86400.
C         ENDDO
         DO K=1,KC  
         DO L=2,LA
          RKTOXWT(L,K,NT)=TX1ST/86400.
         ENDDO
         ENDDO
!}
         ENDIF
C
C DECAY RATE PRINT AT TIMESTEP=1
C
         IF(N.EQ.1)THEN
          OPEN(2,FILE='DECAYR.OUT',STATUS='UNKNOWN')
          WRITE(2,8999)
!{GeoSR, 2014.09.16. YSSONG
C          DO L=2,LA     
C  	      WRITE(2,8998) L,RKTOXWT(L,1)*86400.,RKTOXWV(L),RKTOXWH,
C     &                      RKTOXWP(L),RKTOXWB,HP(L)
          DO K=1,KC  
          DO L=2,LA     
  	      WRITE(2,8998) L,RKTOXWT(L,K,1)*86400.,RKTOXWV(L),RKTOXWH,
     &                      RKTOXWP(L),RKTOXWB,HP(L)
! GeoSR}
          ENDDO
          ENDDO
 8999    FORMAT('    L     TOTAL         VOL         HYD          PHT
     &              BIO          DEP')
 8998    FORMAT(I5,1X,6(E12.5,1X))
          CLOSE(2)
         ENDIF

          DO K=1,KC  
            DO L=2,LA  
              CDECAYW(L,K)=1./(1.+DELT*RKTOXWT(L,K,NT))  
            ENDDO  
          ENDDO  
          DO K=1,KC  
            DO L=2,LA  
              TOX(L,K,NT)=CDECAYW(L,K)*TOX(L,K,NT)  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              CDECAYB(L,K)=1./(1.+DELT*RKTOXB(NT))  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              TOXB(L,K,NT)=CDECAYB(L,K)*TOXB(L,K,NT)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  

      RETURN  
      END  