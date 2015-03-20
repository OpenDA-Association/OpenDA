!{ GEOSR, OIL, CWCHO, 101102

      SUBROUTINE READOIL

      USE GLOBAL  

      IMPLICIT NONE

      INTEGER(4):: NP1, I, J, K
      REAL(RKD) :: XC(4), YC(4), AREA2, RANVAL
      REAL(8), EXTERNAL::DRAND
  
      REAL(RKD) :: OILAREAP
      REAL(RKD) :: ACCRAD
      REAL(RKD) :: AUXX, ANG, DIST, XDIF, YDIF
  
      OILCHEMN=1  

!************* READ OIL.INFO *****************************************
      OPEN(ULOC,FILE='OIL.INFO',ACTION='READ')
!       CALL READSTR(ULOC)
!       READ(ULOC,*) LA_ZCAL,LA_PRAN,LA_DIFOP,LA_HORDIF,LA_VERDIF, 
!     &              DEPOP, DEPOP1 
       CALL READSTR(ULOC)
       READ(ULOC,*) LA_FREQ            
       LA_FREQ = LA_FREQ/1440.
       CALL READSTR(ULOC)
       READ(ULOC,*) NPD
       CALL READSTR(ULOC)
       READ(ULOC,*) THICKLIMIT
       CALL READSTR(ULOC)
       READ(ULOC,*) OILEVAP, OILDISS
	
	IF(IDTOX==4441) THEN       !GASOLINE
       FINGAS_EVAP_CONST1 = 13.2
       FINGAS_EVAP_CONST2 = 0.21
	ELSEIF(IDTOX==4442) THEN   !DISEL
	 FINGAS_EVAP_CONST1 = 0.31
       FINGAS_EVAP_CONST2 = 0.018
	ELSEIF(IDTOX==4443) THEN   !BUNKER-C
	 FINGAS_EVAP_CONST1 = 0.35
       FINGAS_EVAP_CONST2 = 0.013
   	ELSEIF(IDTOX==4444) THEN   !USER DEFINE COEFF.
       CALL READSTR(ULOC)
       READ(ULOC,*) FINGAS_EVAP_CONST1, FINGAS_EVAP_CONST2
	ELSE
	 WRITE(*,*) 'CHECK TOXIC ID'
	 STOP
	ENDIF
	
      CLOSE(ULOC)
!*********************************************************************
      if (.not. allocated(XLA)) then 
      ALLOCATE(XLA(NPD),YLA(NPD),ZLA(NPD),DLA(NPD))
      ALLOCATE(LLA(NPD),KLA(NPD),HPLA(NPD),BELVLA(NPD))
      end if
      if (.not. allocated(OILCONC)) then 
	ALLOCATE(OILCONC(LA,KC,1))
      end if

	 LA_ZCAL=0
	 LA_PRAN=1
       LA_DIFOP=1
	 LA_HORDIF=0.0001
	 LA_VERDIF=0.00001
	 DEPOP=0.0
       DEPOP1=0.0

       OILCONC=0.
       LLA = 0
       KLA = 0  
       HPLA= 0

      IF(LA_PRAN>0) RANVAL = DRAND(1)
       XLA(1)=XTX
       YLA(1)=YTX
      IF (DEPOP==1) THEN
       DLA=DEPOP1 
      ELSE
       ZLA=DEPOP1
      ENDIF

       DELTARHO             = 1-OILSW
       OILRHO               = OILSW*1000.
       OILMASS              = OILVOL*OILRHO
       OILMASSINI           = OILMASS
	 OILVOLINI            = OILVOL
       SOLUBILITYOILINWATER = SOLUBILITYFRESHOIL
	 

       OILAREA  = PI*(CFAY_2**4/CFAY_1**2)*(1./4.)*
     &            ((OILVOL**5*G*DELTARHO/WKVISC**2)**(1./6.))

       OILAREAP = OILAREA / FLOAT(NPD)


	 BETA1=PI*(CFAY_2**2)*(DELTARHO*G*(OILVOLINI**2)/
     &       SQRT(WKVISC))**(1.0/3.0)

	 BETA1_OLD=BETA1

       OILTHICK             = OILVOL/OILAREA

      !Radius of the Accident
       ACCRAD = sqrt(OILAREA / PI)

      DO NP1=2,NPD
       CALL RANDOM_NUMBER(AUXX)
       ANG = AUXX * PI * 2.0

      !Aleatory Distance [0, ACCRAD]
       CALL RANDOM_NUMBER(AUXX)
       DIST = AUXX * ACCRAD

      !Distance from origin in meters
       XDIF = DIST * COS(ANG)
       YDIF = DIST * SIN(ANG)

      !New Position
       XLA(NP1) = XTX + XDIF
       YLA(NP1) = YTX + YDIF
      ENDDO
                
      OPEN(21,FILE='OILEVENT.LOG',POSITION='APPEND',STATUS='UNKNOWN')  
       WRITE(21,8999) OILAREA
	 WRITE(21,8998) NPD
	CLOSE(21)

	 CALL READWIMS2

      OPEN(21,FILE='OILEVENT.LOG',POSITION='APPEND',STATUS='UNKNOWN')  
	DO NP1=1,NPD
	 WRITE(21,*) XLA(NP1), YLA(NP1), ZLA(NP1)
	ENDDO

8999  FORMAT('FAY AREA [M2]        :', F12.3)
8998  FORMAT('NUMBER OF PARTICLES  :',2X, I10)
      CLOSE(21)

      RETURN
  999 STOP 'OIL.INFO READING ERROR!'
      END SUBROUTINE


      SUBROUTINE READSTR(UINP)
      INTEGER(4),INTENT(IN)::UINP
      CHARACTER(200)::STR
      DO WHILE (.TRUE.)
      READ(UINP,'(A)') STR
      STR=ADJUSTL(STR)
      IF (STR(1:1).NE.'*') THEN
      BACKSPACE(UINP)
      RETURN
      ENDIF
      ENDDO
      END SUBROUTINE

!}
