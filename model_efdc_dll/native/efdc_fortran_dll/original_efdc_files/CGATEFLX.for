! Advanced EFDC Hydraulic structure ! GEOSR. 2011. 12. JGCHO
!
      SUBROUTINE CGATEFLX
C  
C CHANGE RECORD  
C ** SUBROUTINE CGATEFLX
C    GATE CONTROL FLUX
C  
      USE GLOBAL  
      implicit none
      integer:: I,K, LG, NCMP, NCTL, NS
      integer :: id, iu, jd, ju, ld, ldu, lu
      integer :: m1, m2
      integer :: IAGT, NGATET 
      REAL:: SEL1T, SEL2T
      REAL :: TIDCHK1, DELCH, DEPDW0, DEPUP0,DUMPTMP
      REAL:: H11, H22, HDW0, HUP0
      REAL :: GHILEV, GLOLEV,GQ1, GQPHI,GKFIX,GQPLO
      REAL :: GQSUMT, GUPHT, GAAA, GBBB
      REAL :: RMAXQ
      REAL :: TDIFF, TCDIFF
      REAL :: TMPSILL, TIDETMP
      REAL :: WTM1, WTM2, WCTM1, WCTM2
      
      
      INTEGER IGCHECK(NQCTLM)
      REAL GKMULT(KCM) ! GEOSR JGCHO 2011.10.27       ALLOCATE(GKMUL(NDQCLT,KCM,NQCTLM)) ! GEOSR JGCHO 2011.10.27
      REAL GQT(NQCTLM),LUA(NQCTLM),LDA(NQCTLM)
      REAL CG10
      REAL CQ(LCM),CV(LCM)				! GEOSR UNG 2014.11.12 Warning message writing
      CHARACTER*80 CTLE1
      ! open time control : jgcho 2010.8.17 temporary
!      IF (N.EQ.1) GATEOTM=1.0
!      GTIMENOW=TIMEDAY !N*DT/86400.

      IGCHECK=0              ! GATE FORMULA ID
      GQT=0.
      LUA=0
      LDU=0
      DUMPG(:)=0.0d0
      DUMPG2(:)=0.0d0 ! GEOSR jgcho 2016.07.14
      CQ(:)=0.0d0					! GEOSR UNG 2014.11.12 Warning message writing
      CV(:)=0.0d0					! GEOSR UNG 2014.11.12 Warning message writing
      
      DO K=1,KC
        DO NCTL=1,NQCTL
          IU=IQCTLU(NCTL)                   ! I CELL INDEX UPSTREAM
          JU=JQCTLU(NCTL)                   ! J CELL INDEX UPSTREAM
          LU=LIJ(IU,JU)                     ! L CELL INDEX UPSTREAM
          ID=IQCTLD(NCTL)                   ! I CELL INDEX DOWNSTREAM
          JD=JQCTLD(NCTL)                   ! J CELL INDEX DOWNSTREAM
! { GEOSR ESTURAY DIKE : JGCHO 2010.11.15
          IF((ID.EQ.0.AND.JD.EQ.0) .OR. NQCTYP(NCTL).EQ.6)THEN  
            LD=LC  
          ELSE  
            LD=LIJ(ID,JD)                   ! L CELL INDEX DOWNSTREAM
          ENDIF  
! } GEOSR ESTURAY DIKE : JGCHO 2010.11.15
        ENDDO
      ENDDO

      IF (ISINK.EQ.1) THEN   ! READY SINK#.OUT
        FSINK='SINK.OUT'
        OPEN(711,FILE=TRIM(FSINK),STATUS='UNKNOWN')  ! OPEN OLD FILE
        CLOSE(711,STATUS='DELETE')             ! DELETE OLD FILE
        OPEN(711,FILE=FSINK,STATUS='UNKNOWN')  ! OPEN NEW FILE
        WRITE(711,7101) 
 7101   FORMAT('       N      TIME  '
     &        ,<NQCTL>('ID     HUP     HDW     DIF    Q',<8*(KC-1)+5>X
     &                ,20X))
        CLOSE(711)

        OPEN(712,FILE='SINKT.OUT',STATUS='UNKNOWN')  ! OPEN OLD FILE
        CLOSE(712,STATUS='DELETE')             ! DELETE OLD FILE
        OPEN(712,FILE='SINKT.OUT',STATUS='UNKNOWN')  ! OPEN NEW FILE
        WRITE(712,7102) '       N      TIME',(NS,NS=1,NQCTL)
 7102   FORMAT(A,<NQCTL>I8)
 
        ! GEOSR GATE: SINK2
        OPEN(713,FILE='SINK2.OUT',STATUS='UNKNOWN')  ! OPEN OLD FILE
        CLOSE(713,STATUS='DELETE')             ! DELETE OLD FILE
        OPEN(713,FILE='SINK2.OUT',STATUS='UNKNOWN')  ! OPEN NEW FILE
        write(713,7103) '       N      TIME',
     &                  ((NS,'_K',k,k=1,KC),NS,'_O',00,NS=1,NQCTL)
 7103   FORMAT(A,<1000>(3x,i2.2,a,i2.2))


        ISINK=2            ! READY TO WRITE SINK##.OUT 
        SNKW=DTSNK*60./DT  ! WRITING TIME INTERVAL
      ENDIF                ! IF (ISINK.EQ.1) THEN
C
      DO LG=1,NQCTL              ! GATE TYPE
        DUMPG(LG)=0.               ! INIT. GATE FLUX VALUE
        DEPUP0=0.                  ! INIT. UPSTREAM TOTAL DEPTH
        HUP0=0.                    ! INIT. UPSTREAM ELEV.
        DO NCMP=1,NICMP(LG)        ! COMPARE CELL OF UPSTREAM ELEV.
          IUC=ICMPI(NCMP,LG)        ! I CELL COMPARE INDEX UPSTREAM
          JUC=JCMPI(NCMP,LG)        ! J CELL COMPARE INDEX UPSTREAM
          LUC=LIJ(IUC,JUC)            ! L CELL COMPARE INDEX UPSTREAM
          HUP0=HUP0 + HP(LUC)+BELV(LUC)  ! SUM OF UPSTEAM ELEV. FOR AVERAGE
          DEPUP0=DEPUP0 + HP(LUC)   ! SUM OF UPSTEAM DEPTH FOR AVERAGE
        ENDDO                      ! DO NCMP=1,NICMP(LG)
        HUP0=HUP0/FLOAT(NICMP(LG))       ! UPSTREAM ELEV.
        
        IF (MOD(N,IATS(LG)).eq.0) THEN
          HUPG(LG)=(AHUP(LG) + HUP0)/float(IATS(LG))
          AHUP(LG)=0.
        ELSE IF (N.EQ.1.and.(.not.HUPG_HDWG_INITIALIZED)) THEN
          HUPG(LG)=HUP0
          AHUP(LG)=HUP0
        ELSE
          AHUP(LG)=AHUP(LG) + HUP0
        ENDIF !IF (MOD(N,IATS).eq.0) THEN
        
!        DEPUPG(LG)=DEPUP0/FLOAT(NICMP(LG))   ! UPSTREAM TOTAL DEPTH
        DEPUPG(LG)=HUPG(LG) - SILL(LG)       ! UPSTREAM TOTAL DEPTH (ELEV. - SILL HEIGHT)
      ENDDO                        ! DO LG=1,NGTYPES
      HUPG_HDWG_INITIALIZED =.true.
      
      DO LG=1,NQCTL             ! GATE TYPE
        DEPDW0=0.                 ! INIT. DOWNSTREAM TOTAL DEPTH
        HDW0=0.                   ! INIT. DOWNSTREAM ELEV.
        DO NCMP=1,NOCMP(LG)       ! COMPARE CELL OF DOWNSTREAM ELEV.
          IDC=ICMPO(NCMP,LG)       ! I CELL COMPARE INDEX DOWNSTREAM
          JDC=JCMPO(NCMP,LG)       ! J CELL COMPARE INDEX DOWNSTREAM
! { GEOSR ESTURAY DIKE : JGCHO 2010.11.15
          IF(IDC.EQ.0.AND.JDC.EQ.0)THEN  
            LDC=LIJ(ICMPI(NCMP,LG),JCMPI(NCMP,LG))
! ESTURARY OUTER TIDE INTERPOLATION START ********************************
            M1=MTIDELAST(LG)
  100       CONTINUE
            M2=M1+1
            IF((TIMEDAY-EPS).GT.ESTIME(LG,M2))THEN  
              M1=M2  
              GOTO 100  
            ELSE  
              MTIDELAST(LG)=M1  
            ENDIF  
            TDIFF=ESTIME(LG,M2)-ESTIME(LG,M1)  
            WTM1=(ESTIME(LG,M2)-TIMEDAY)/TDIFF  
            WTM2=(TIMEDAY-ESTIME(LG,M1))/TDIFF  
            TIDETMP=WTM1*ESTIDE(LG,M1)+WTM2*ESTIDE(LG,M2)  
! ESTURARY OUTER TIDE INTERPOLATION END   ********************************
            HDW0=HDW0+TIDETMP
          ELSE  
            LDC=LIJ(IDC,JDC)           ! L CELL COMPARE INDEX DOWNSTREAM
            HDW0=HDW0 + HP(LDC)+BELV(LDC)  ! SUM OF DOWNSTREAM ELEV. FOR AVERAGE
            DEPDW0=DEPDW0 + HP(LDC)  ! SUM OF DOWNSTEAM DEPTH FOR AVERAGE
          ENDIF  
! } GEOSR ESTURAY DIKE : JGCHO 2010.11.15
        ENDDO                     ! DO NCMP=1,NOCMP(LG)
        HDW0=HDW0/FLOAT(NOCMP(LG))       ! DOWNSTREAM ELEV.
        
        IF (MOD(N,IATS(LG)).eq.0) THEN
          HDWG(LG)=(AHDW(LG) + HDW0)/float(IATS(LG))
          AHDW(LG)=0.
        ELSE IF ((N.EQ.1).and.(.not.HUPG_HDWG_INITIALIZED)) THEN
          HDWG(LG)=HDW0
          AHDW(LG)=HDW0
        ELSE
          AHDW(LG)=AHDW(LG) + HDW0
        ENDIF !IF (MOD(N,IATS).eq.0) THEN
        
!        DEPDWG(LG)=DEPDW0/FLOAT(NOCMP(LG))   ! DOWNSTREAM TOTAL DEPTH
        DEPDWG(LG)=HDWG(LG) - SILL(LG)       ! DOWNSTREAM TOTAL DEPTH (ELEV. - SILL HEIGHT)
      ENDDO                       ! DO LG=1,NGTYPES

      IF (N.EQ.1) THEN            ! FIRST TIME STEP ELEV.
        DO LG=1,NQCTL           ! GATE TYPE
          ELPREV(LG)=HDWG(LG)     ! DOWNSTREAM ELEV. FOR CURRENT TIME
          ELPREV1(LG)=HDWG(LG)    ! DOWNSTREAM ELEV. FOR PREVIOUS TIME
        ENDDO                     ! DO LG=1,NGTYPES
      ENDIF                       ! IF (N.EQ.1) THEN
C
      DO LG=1,NQCTL              ! GATE TYPE
        TIDCHK1=TIDCHK(LG)*60./DT  ! TIME INTERVAL FOR DOWNSTREAM ELEV. COMPARE
        IF ( MOD(FLOAT(N),TIDCHK1).EQ.0.  ) THEN  ! DOWNSTREAM ELEV. CHECK TIMESTEP
          ELPREV(LG)=ELPREV1(LG)   ! PREVIOUS DOWNSTREAM ELEV.
          ELPREV1(LG)=HDWG(LG)     ! NOW UPSTREAM ELEV. FOR NEXT TIDCHK TIME
        ENDIF                      ! IF ( MOD(FLOAT(N),TIDCHK1).EQ.0.  ) THEN
      ENDDO                        ! DO LG=1,NGTYPES
C
      DO LG=1,NQCTL                  ! GATE TYPE
        DIFEL(LG)=ELPREV(LG)-HDWG(LG)  ! DIFFERENCE OF ELEV. DOWNSTREAM SIDE
        DELHG(LG)=abs(HUPG(LG)-HDWG(LG))  ! DIFFERENCE BETWEEN UPSTREAM AND DOWNSTREAM ELEV.
      ENDDO                            ! DO LG=1,NGTYPES
C 

      NGATE_EF=0                          ! EBB or FLOOD SWITCH  (3:EBB, 4:FLOOD)

      DO NCTL=1,NQCTL                     ! GATE CELL
        LG=NCTL !NGTYP(NCTL)-IWSYS*100          ! GATE TYPE ID
        IGCHECK(LG)=0

        IU=IQCTLU(NCTL)                   ! I CELL INDEX UPSTREAM
        JU=JQCTLU(NCTL)                   ! J CELL INDEX UPSTREAM
        LU=LIJ(IU,JU)                     ! L CELL INDEX UPSTREAM
        ID=IQCTLD(NCTL)                   ! I CELL INDEX DOWNSTREAM
        JD=JQCTLD(NCTL)                   ! J CELL INDEX DOWNSTREAM
! { GEOSR ESTUARY DIKE : JGCHO 2010.11.15
        IF(ID.EQ.0.AND.JD.EQ.0)THEN  
          LD=LC  
        ELSE  
          LD=LIJ(ID,JD)                   ! L CELL INDEX DOWNSTREAM
        ENDIF  
! } GEOSR ESTUARY DIKE : JGCHO 2010.11.15
C
! { GEOSR FIXED WEIR TS remove : jgcho 2014.10.14
        if (NQCTLQ(NCTL).eq.0) then
          IAGT=1
          SEL1T=SILL(NCTL)
          SEL2T=0.
          GUPHT=0.
          GQSUMT=-1.
          DO K=1,KC
            GKMULT(K)=1./float(kc)
          ENDDO
        else

          M1=MGTLAST(NQCTLQ(NCTL))
  200     CONTINUE
          M2=M1+1
          IF ((TIMEDAY-EPS).GT.GCSER(M2,NQCTLQ(NCTL))) THEN
            M1=M2
            GOTO 200
          ELSE
            MGTLAST(NQCTLQ(NCTL))=M1
          ENDIF

          IAGT=IAG(M1,NQCTLQ(NCTL)) !0:CLOSE, 1:AUTO, 2:USER_DEFINE
          SEL1T=SEL1(M1,NQCTLQ(NCTL))
          SEL2T=SEL2(M1,NQCTLQ(NCTL))
          GUPHT=GUPH(M1,NQCTLQ(NCTL)) !/100.
          GQSUMT=GQSUM(M1,NQCTLQ(NCTL))
          NGATET=NGATE(M1,NQCTLQ(NCTL))
          DO K=1,KC
            GKMULT(K)=GKMUL(M1,K,NCTL)
          ENDDO
          
        endif

          IF (GUPHT.LE.0.) THEN
            GUPHT=0. ! 2014.11.09 JGCHO SILLHH(LG) ! GATE OPEN HEIGHT
          ENDIF
          IF (IAGT.EQ.2 .AND. IAGUSER(NCTL).LT.M1) THEN
            IAGUSER(NQCTLQ(NCTL))=M1          
            GGQSUM(NQCTLQ(NCTL))=0.
          ENDIF
          IF (GQSUMT.EQ.-1.) GQSUMT=9.0E+20

          IF (NQCTYP(NCTL).EQ.9) THEN
            GLOLEV=SEL1(M1,NQCTLQ(NCTL))   !X1
            GHILEV=SEL2(M1,NQCTLQ(NCTL))   !X2
            GQPLO=GUPH(M1,NQCTLQ(NCTL))   !Y1
            GQPHI=GQSUM(M1,NQCTLQ(NCTL)) !Y2
          ENDIF
! } GEOSR FIXED WEIR TS remove : jgcho 2014.10.14

C###########################################################
C#      UPSTREAM -> DOWNSTREAM (START)
C###########################################################
        IF( (NQCTYP(NCTL).GE.31 .AND. NQCTYP(NCTL).LE.33)
     &   													.AND. NGCCHECK(LG).EQ.0)THEN   			! UPSTREAM -> DOWNSTREAM
          IF (TIDCHK(LG).EQ.-1.) DIFEL(LG)=1.  ! NOT CONSIDER TIDE LEVLE
                         IF (IAGT.EQ.0) THEN
                           NGCHECK(LG)=0                 
                           CALL EBBMASKRE
                         ELSEIF (IAGT.EQ.1) THEN

          IF (DIFEL(LG).GT.0.02) THEN         ! EBB PERIOD
            IF (DELHG(LG).GE.DELHINOUT(LG)
     &         .AND. HUPG(LG).GT.HDWG(LG)) THEN ! UPSTREAM LEVEL HIGHER THAN DOWNSTREAM
              IF (HUPG(LG).GE.SEL1T   ! SILL(LG) -> SEL1T !GEOSR 2014.12.24 jgcho
     &           .AND. (HUPG(LG)-SEL1T).GE.DELHSEL1(LG)) THEN  ! UPSTREAM LEVEL HIGHER THAN TARGET LOW LEVEL     ! SILL(LG) -> SEL1T !GEOSR 2014.12.24 jgcho
C
              ! open time control : jgcho 2010.8.17 temporary
!              IF (N*DT/86400. .GE. GATEOTM) THEN

                  NGATE_EF=3                                        ! EBB SWITCH  (3:EBB, 4:FLOOD) ?

                IF (NGCHECK(LG).EQ.0) THEN                        ! GATE OPEN CHECK
                  NGCOUNT(LG)=1                                   ! TIME STEP COUNT AFTER GATE OPEN
                  NGCHECK(LG)=1                                   ! GATE OPEN CHECK
                ELSE
                  NGCOUNT(LG)=NGCOUNT(LG)+1                       ! ADD TIME STEP 
                ENDIF
C
                IF (GOTIME(LG).GT.0.) THEN
                  GRAMPUP(LG)=FLOAT(NGCOUNT(LG)) / 
     &                        (NGATEC(NCTL)*GOTIME(LG)*60./DT)  ! GATE OPEN TIME INTERVAL RAMPUP TERM
                ELSE
                  GRAMPUP(LG)=1.
                ENDIF
C
                GRAMPUP(LG) = MIN(GRAMPUP(LG), 1.0)  ! ADJUST GATE RAMPUP VALUE
C
C                SILLHHO(LG)=SILLHH(LG)*GRAMPUP(LG)                     ! GATE HEIGHT AFTER GATE OPEN (NOT USE)
C
C++++++++++++++ BEGIN NORMAL FORMULA
                !!20141024-UNG
                IF (NQCTYP(NCTL).EQ.31) THEN                            ! Elevator type rise and fall system
                  IF (HUPG(LG).LE.GUPHT+SILL(LG)) THEN  ! 2014.11.08 jgcho  DEPUPG(LG) -> HUPG(LG)   +SILL(LG)
                    IF (DELHG(LG).GE.DEPUPG(LG)/3.) THEN                ! COMPLETE OVERFLOW(WEIR)
                      IGCHECK(LG)=313
                      DUMPG(LG)=1.704*CG3(LG)*GATEWI(LG)            
     &                         *(DEPUPG(LG)**(3./2.))                
                    ELSEIF (DELHG(LG).LT.DEPUPG(LG)/3.) THEN            ! SUBMERGED WEIR
                      IGCHECK(LG)=314
                      DUMPG(LG)=CG4(LG)*(DEPUPG(LG)-DELHG(LG))
     &                         *GATEWI(LG)*SQRT(2.*9.806*DELHG(LG))
                    ENDIF   ! IF (DELHG(LG).GE.DEPUPG(LG)/3.) THEN
                  ELSE
                   IF ((GUPHT+SILL(LG)).GT.HDWG(LG)) THEN               ! COMPLETE ORIFICE
                    IF (SILL(LG).LT.HDWG(LG))	THEN
                     DELCH=DEPUPG(LG)-GUPHT
                     IGCHECK(LG)=311
                     IF (NCG3FOM(LG).EQ.1) THEN
                       GQ1=CG5(LG)*(GUPHT+SILL(LG)-HDWG(LG))
     &                    *GATEWI(LG)*SQRT(2.*9.806*(DELHG(LG)-DELCH))
                     ELSEIF (NCG3FOM(LG).EQ.2) THEN
                       GQ1=2./3.*CG5(LG)*GATEWI(LG)*SQRT((2.
     &                      *9.806)*(DELHG(LG)**(3./2.)-DELCH**(3./2.)))
                     ENDIF
                     DUMPG(LG)=GQ1+CG5(LG)*(DEPUPG(LG)-DELHG(LG))
     &                        *GATEWI(LG)*SQRT(2.*9.806*DELHG(LG))
                    ELSE
                     IGCHECK(LG)=315
                     DUMPG(LG)=CG7(LG)*GUPHT*GATEWI(LG)
     &                        *SQRT(2.*9.806*(DEPUPG(LG)-GUPHT/2.))
                    ENDIF
                   ELSE                                              		! SUBMERGED ORIFICE
                    IGCHECK(LG)=312
                    DUMPG(LG)=CG6(LG)*GUPHT*GATEWI(LG)
     &                       *SQRT(2.*9.806*DELHG(LG))
                   ENDIF  ! IF ((GUPHT+SILL(LG)).GT.HDWG(LG)) THEN
                  ENDIF   ! IF (DEPUPG(LG).LE.GUPHT) THEN

! GEOSR GATE rough translation : Monday - higher than the upstream water level above the moving beam,
!                                even if you apply a flexible security operations in the flow
                  if (HUPG(LG).gt.SILLHH(LG)+GUPHT) then 
                  ! Rough translation: If the upstream water level is higher 
                  !                    than the compensation operation only
                    tmpsill=SILLHH(LG)+GUPHT
                    tcdiff=CGH2(LG)-CGH1(LG)
                    h11=HUPG(LG)-tmpsill
                    wctm1=(CGH2(LG)-h11)/tcdiff
                    wctm2=(h11-CGH1(LG))/tcdiff
                    CG10=wctm1*CG1(LG) + wctm2*CG2(LG)
                    IF((HUPG(LG)-tmpsill).LT.CGH1(LG)) THEN
                      CG10=CG1(LG)
                    ELSEIF((HUPG(LG)-tmpsill).GT.CGH2(LG)) THEN
                      CG10=CG2(LG)
                    ENDIF

                    IF (HDWG(LG) .LT. tmpsill ) THEN
                      DUMPG2(LG)=CG10*GATEWI(LG)*(HUPG(LG)-tmpsill)
     &                 *SQRT(2.*9.806*(HUPG(LG)-tmpsill))
                    else
                      DUMPG2(LG)=CG10*2.6*GATEWI(LG)*(HDWG(LG)
     &                          -tmpsill)*SQRT(2.*9.806
     &                  *((HUPG(LG)-tmpsill)-(HDWG(LG)-tmpsill)))
                    endif
                  endif !if (HUPG(LG).gt.SILLHH(LG)+GUPHT) then

                ENDIF  ! IF (NQCTYP(NCTL).EQ.31) THEN
                IF (NQCTYP(NCTL).EQ.32) THEN																			! Turning type floodgate

                  IF (HUPG(LG).GT.(SILLHH(LG)-GUPHT)) THEN ! JGCHO 2014.11.11 ADD SILL(LG)+
!                     
                    IF (HDWG(LG).LT.SILLHH(LG)-GUPHT) THEN !DELCH) THEN  JGCHO 2014.11.11
                      IGCHECK(LG)=321
                      h11=HUPG(LG)-(SILLHH(LG)-GUPHT)
                      DUMPG(LG)=CG8(LG)*GATEWI(LG)*h11
     &                         *SQRT(2.*9.806*h11)
                    ELSE
                      IGCHECK(LG)=322
                      h11=HUPG(LG)-(SILLHH(LG)-GUPHT)
                      h22=HDWG(LG)-(SILLHH(LG)-GUPHT)
                      DUMPG(LG)=2.6*CG8(LG)
     &                         *GATEWI(LG)*h22
     &                         *SQRT(2.*9.806*(h11-h22))
                    ENDIF
                 ENDIF
!                	write(*,*)DUMPG(LG)				testwriting-ung 1105
!                	write(*,*)CG8(LG),NGTYP(LG),DEPUPG(LG),DELCH
!                	write(*,*)GATEWI(LG)
!                	write(*,*)GATEHI(LG),GUPHT,SILL(LG)
!                	PAUSE
                ENDIF  ! IF (NQCTYP(NCTL).EQ.32) THEN
                IF (NQCTYP(NCTL).EQ.33) THEN                 ! Roller floodgate
                  IF (GUPHT.GE.0) THEN                       ! OVERFLOW
                    IF (HUPG(LG).GT.(SILLHH(LG)-GUPHT)) THEN ! JGCHO 2014.11.08 ADD SILL(LG)+
                      IF (HDWG(LG).LT.SILLHH(LG)-GUPHT) THEN
                        IGCHECK(LG)=336
                        h11=HUPG(LG)-(SILLHH(LG)-GUPHT)
                        DUMPG(LG)=CG8(LG)*GATEWI(LG)*h11
     &                           *SQRT(2.*9.806*h11)
                      ELSE
                        IGCHECK(LG)=337
                        h11=HUPG(LG)-(SILLHH(LG)-GUPHT)
                        h22=HDWG(LG)-(SILLHH(LG)-GUPHT)
                        DUMPG(LG)=CG8(LG)*GATEWI(LG)*h11
     &                           *SQRT(2.*9.806*h11)
!                              DUMPG(LG)=2.6*CG8(LG)
!     &                            *GATEWI(LG)*h22
!     &                         *SQRT(2.*9.806*(h11-h22))
                      ENDIF  !  IF (HDWG(LG).LT.DELCH) THEN
                    ENDIF    !  IF (HUPG(LG).GT.(GATEHI(LG)-GUPHT)) THEN
                  ELSE       !  IF (GUPHT.GE.0) THEN
                    IF (DEPUPG(LG).LE.GUPHT*(-1.)) THEN
                      IF (DELHG(LG).GE.DEPUPG(LG)/3.) THEN                ! COMPLETE OVERFLOW(WEIR)
                        IGCHECK(LG)=333
                        DUMPG(LG)=1.704*CG3(LG)*GATEWI(LG)            
     &                       *(DEPUPG(LG)**(3./2.))                
                      ELSEIF (DELHG(LG).LT.DEPUPG(LG)/3.) THEN            ! SUBMERGED WEIR
                        IGCHECK(LG)=334
                        DUMPG(LG)=CG4(LG)*(DEPUPG(LG)-DELHG(LG))
     &                       *GATEWI(LG)*SQRT(2.*9.806*DELHG(LG))
                      ENDIF   ! IF (DELHG(LG).GE.DEPUPG(LG)/3.) THEN
                    ELSE		!  IF (DEPUPG(LG).LE.GUPHT*(-1.)) THEN
                      IF ((GUPHT*(-1.)+SILL(LG)).GT.HDWG(LG)) THEN                 				! COMPLETE ORIFICE
                        IF (SILL(LG).LT.HDWG(LG))	THEN
                          DELCH=DEPUPG(LG)-GUPHT*(-1.)
                          IGCHECK(LG)=331
                          IF (NCG3FOM(LG).EQ.1) THEN
                            GQ1=CG5(LG)*(GUPHT*(-1.)+SILL(LG)
     &                         -HDWG(LG))*GATEWI(LG)
     &                         *SQRT(2.*9.806*(DELHG(LG)-DELCH))
                          ELSEIF (NCG3FOM(LG).EQ.2) THEN
                            GQ1=2./3.*CG5(LG)*GATEWI(LG)*SQRT
     &                       (2.*9.806)*(DELHG(LG)**(3./2.)
     &                       -DELCH**(3./2.))
                          ENDIF
                          DUMPG(LG)=GQ1+CG5(LG)*(DEPUPG(LG)
     &                        -DELHG(LG))*GATEWI(LG)
     &                        *SQRT(2.*9.806*DELHG(LG))
                        ELSE                                             ! DIFF SILL
                          IGCHECK(LG)=335
                          DUMPG(LG)=CG7(LG)*GUPHT*(-1.)*GATEWI(LG)
     &                      *SQRT(2.*9.806*(DEPUPG(LG)-GUPHT*(-1.)/2.))
                        ENDIF                                         !  IF (SILL(LG).LT.HDWG(LG))	THEN
                      ELSE                                                ! SUBMERGED ORIFICE
                        IGCHECK(LG)=332
                        DUMPG(LG)=CG6(LG)*GUPHT*(-1.)*GATEWI(LG)
     &                       *SQRT(2.*9.806*DELHG(LG))
                      ENDIF  ! IF ((GUPHT+SILL(LG)).GT.HDWG(LG)) THEN
!
! GEOSR GATE : Rough translation: Apply Mon-flow even when the upstream water level higher
!                                 than the upper movable beam during rotary operation
!                                 security operations
                      if (HUPG(LG).gt.SILLHH(LG)+GUPHT*(-1.)) then
                        ! Rough translation: If the upstream water level is higher 
                        !                    than the compensation oepration only
                        tmpsill=SILLHH(LG)+GUPHT*(-1.)
                        tcdiff=CGH2(LG)-CGH1(LG)
                        h11=HUPG(LG)-tmpsill
                        wctm1=(CGH2(LG)-h11)/tcdiff
                        wctm2=(h11-CGH1(LG))/tcdiff
                        CG10=wctm1*CG1(LG) + wctm2*CG2(LG)
                        IF((HUPG(LG)-tmpsill).LT.CGH1(LG)) THEN
                          CG10=CG1(LG)
                        ELSEIF((HUPG(LG)-tmpsill).GT.CGH2(LG)) THEN
                          CG10=CG2(LG)
                        ENDIF
                          
                        IF (HDWG(LG) .LT. tmpsill ) THEN
                          DUMPG2(LG)=CG10*GATEWI(LG)*(HUPG(LG)-tmpsill)
     &                       *SQRT(2.*9.806*(HUPG(LG)-tmpsill))
                        else
                           DUMPG2(LG)=CG10*2.6*GATEWI(LG)*(HDWG(LG)
     &                        -tmpsill)*SQRT(2.*9.806
     &                        *((HUPG(LG)-tmpsill)-(HDWG(LG)-tmpsill)))
                        endif
                      endif !if (HUPG(LG).gt.SILLHH(LG)+GUPHT) then
!
                    ENDIF   ! IF (DEPUPG(LG).LE.GUPHT) THEN
                  ENDIF     !  IF (GUPHT.GE.0) THEN
                ENDIF  ! IF (NQCTYP(NCTL).EQ.33) THEN
                !!20141024-UNG
                
C++++++++++++++++ END NORMAL FORMULA
C
C
!     CHECK MAXIMUM GATE FLUX
                DUMPG(LG)=DUMPG(LG)*FLOAT(NGATET)    ! CONSIDER GATE NUMBER     !NGATET 2014.10.14 jgcho
                RMAXQ=FLOAT(NGATET)*MAXQ(LG)/FLOAT(NGATEM(NCTL))                !NGATET 2014.10.14 jgcho
                IF (RMAXQ.GT.0. .AND. DUMPG(LG).GT.RMAXQ) THEN  ! MAX. FLUX CHECK
                  DUMPG(LG)=RMAXQ/FLOAT(NGATEC(NCTL))  ! APPLIED MAX. FLUX AND CONSIDER CELL NO.
                ELSE
                  DUMPG(LG)=DUMPG(LG)/FLOAT(NGATEC(NCTL)) ! CONSIDER CELL NO.
                ENDIF                  
C DUMP=DUMP*FLOAT(NGGATE(NCTL))/FLOAT(NGGATEC(NCTL))
                DUMPG(LG)=DUMPG(LG)*GRAMPUP(LG)           ! ADJUST GATE FLUX
                DUMPGPREV(LG)=DUMPG(LG)                   ! SAVE GATE FLUX FOR CONTROL AFTER CLOSE
C
! GEOSR GATE : G2 variables
                DUMPG2(LG)=DUMPG2(LG)*FLOAT(NGATET)
                DUMPG2(LG)=DUMPG2(LG)/FLOAT(NGATEC(NCTL))
                DUMPG2(LG)=DUMPG2(LG)*GRAMPUP(LG)
                DUMPG2PREV(LG)=DUMPG2(LG)

                DO K=1,KC  
                  QCTLT(K,NCTL)=DUMPG(LG)*GKMULT(K) !DZC(K)       ! SAVE GATE FLUX
                  if (K.eq.KC) then
                    GKFIX=1.
                  else
                    GKFIX=0.
                  endif
                  QCTLT(K,NCTL)=QCTLT(K,NCTL) + DUMPG2(LG)*GKFIX
                ENDDO  


C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK START (EBB)
                CALL EBBMASK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK END

!              ELSE  ! IF (TIME.GE.GATEOTM) THEN open time control : jgcho 2010.8.17 temporary
!                NGCHECK(LG)=0
!                CALL EBBMASKRE
!			ENDIF ! IF (TIME.GE.GATEOTM) THEN open time control : jgcho 2010.8.17 temporary

              ELSE                      ! IF (HUPG(LG).GE.SEL1(LG) ...
                NGCHECK(LG)=0
                CALL EBBMASKRE
              ENDIF                     ! IF (HUPG(LG).GE.SEL1(LG) ...
            ELSE                        ! IF (DELHG(LG).GE.DELHINOUT(LG) ...
              NGCHECK(LG)=0
              CALL EBBMASKRE
            ENDIF                       ! IF (DELHG(LG).GE.DELHINOUT(LG) ...
          ELSE                          ! IF (DIFEL(LG).GT.0.02) THEN
            NGCHECK(LG)=0                 
            CALL EBBMASKRE
          ENDIF                         ! IF (DIFEL(LG).GT.0.02) THEN


      !###########################################################
      !#      UPSTREAM -> DOWNSTREAM : USER DEFINE (START)
      !###########################################################
                         ELSEIF (IAGT.EQ.2) THEN !ELSEIF (IAGT.EQ.1) THEN
          IF (GGQSUM(NCTL).LE.GQSUMT) THEN

            IF (DIFEL(LG).GT.0.02) THEN         ! EBB PERIOD
              IF (DELHG(LG).GE.DELHINOUT(LG)
     &           .AND. HUPG(LG).GT.HDWG(LG)) THEN ! UPSTREAM LEVEL HIGHER THAN DOWNSTREAM
                IF (HUPG(LG).GE.SEL1T
     &             .AND. (HUPG(LG)-SEL1T).GE.DELHSEL1(LG)) THEN  ! UPSTREAM LEVEL HIGHER THAN TARGET LOW LEVEL

                  IF (NGCHECK(LG).EQ.0) THEN                        ! GATE OPEN CHECK
                    NGCOUNT(LG)=1                                   ! TIME STEP COUNT AFTER GATE OPEN
                    NGCHECK(LG)=1                                   ! GATE OPEN CHECK
                  ELSE
                    NGCOUNT(LG)=NGCOUNT(LG)+1                       ! ADD TIME STEP 
                  ENDIF

                  IF (GOTIME(LG).GT.0.) THEN
                    GRAMPUP(LG)=FLOAT(NGCOUNT(LG)) / 
     &                          (NGATEC(NCTL)*GOTIME(LG)*60./DT)  ! GATE OPEN TIME INTERVAL RAMPUP TERM
                  ELSE
                    GRAMPUP(LG)=1.
                  ENDIF
C
                  GRAMPUP(LG) = MIN(GRAMPUP(LG), 1.0)  ! ADJUST GATE RAMPUP VALUE

C++++++++++++++ BEGIN NORMAL FORMULA
                  IF (DEPUPG(LG).LE.GUPHT) THEN                    ! OVERFLOW OR WEIR
                    IF (DELHG(LG).GE.DEPUPG(LG)/3.) THEN                ! COMPLETE OVERFLOW
                      IGCHECK(LG)=41
                      DUMPG(LG)=1.704*CG1(LG)*GATEWI(LG)                
     &                         *(DEPUPG(LG)**(3./2.))                
                    ELSEIF (DELHG(LG).LT.DEPUPG(LG)/3.) THEN            ! SUBMERGED WEIR
                      IGCHECK(LG)=42
                      DUMPG(LG)=CG2(LG)*(DEPUPG(LG)-DELHG(LG))
     &                         *GATEWI(LG)*SQRT(2.*9.806*DELHG(LG))
                    ENDIF
                  ELSE                                                  ! ORIFICE CONDITION
                    IF (GUPHT.GT.DEPDWG(LG)) THEN                  ! INCOMPLETE ORIFICE
                      DELCH=DEPUPG(LG)-GUPHT
                      IF (NCG3FOM(LG).EQ.1) THEN
                        IGCHECK(LG)=43
                        GQ1=CG3(LG)*(GUPHT-DEPDWG(LG))
     &                      *GATEWI(LG)*SQRT(2.*9.806*(DELHG(LG)-DELCH))
                      ELSEIF (NCG3FOM(LG).EQ.2) THEN
                        IGCHECK(LG)=44
                        GQ1=2./3.*CG3(LG)*GATEWI(LG)*SQRT((2.
     &                      *9.806)*(DELHG(LG)**(3./2.)-DELCH**(3./2.)))
                      ENDIF
                      DUMPG(LG)=GQ1 + CG4(LG)*(DEPUPG(LG)
     &                   -DELHG(LG))*GATEWI(LG)*SQRT(2.*9.806*DELHG(LG))
                    ELSE                                                ! SUBMERGED ORIFICE
                      IGCHECK(LG)=45
                      DUMPG(LG)=CG5(LG)*GUPHT*GATEWI(LG)
     &                         *SQRT(2.*9.806*DELHG(LG))
                    ENDIF  ! IF (DELHG(LG).LT.DEPUPG(LG)/3.) THEN
                  ENDIF  ! IF (DEPUPG(LG).GT.GUPHT) THEN
C++++++++++++++++ END NORMAL FORMULA

!     CHECK MAXIMUM GATE FLUX
                  DUMPG(LG)=DUMPG(LG)*FLOAT(NGATE(M1,NCTL))    ! CONSIDER GATE NUMBER
                RMAXQ=FLOAT(NGATE(M1,NCTL))*MAXQ(LG)/FLOAT(NGATEM(NCTL))
                  IF (RMAXQ.GT.0. .AND. DUMPG(LG).GT.RMAXQ) THEN  ! MAX. FLUX CHECK
                    DUMPG(LG)=RMAXQ/FLOAT(NGATEC(NCTL))  ! APPLIED MAX. FLUX AND CONSIDER CELL NO.
                  ELSE
                    DUMPG(LG)=DUMPG(LG)/FLOAT(NGATEC(NCTL)) ! CONSIDER CELL NO.
                  ENDIF                  
                  DUMPG(LG)=DUMPG(LG)*GRAMPUP(LG)           ! ADJUST GATE FLUX
                  DUMPGPREV(LG)=DUMPG(LG)                   ! SAVE GATE FLUX FOR CONTROL AFTER CLOSE

                  DO K=1,KC  
                    QCTLT(K,NCTL)=DUMPG(LG)*GKMULT(K) !DZC(K)       ! SAVE GATE FLUX
                    GGQSUM(NCTL)=GGQSUM(NCTL)+QCTLT(K,NCTL)*DT
                  ENDDO  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK START (EBB)
                  CALL EBBMASK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK END
                ELSE                      ! IF (HUPG(LG).GE.SEL1(LG) ...
                  NGCHECK(LG)=0
                  CALL EBBMASKRE
                ENDIF                     ! IF (HUPG(LG).GE.SEL1(LG) ...
              ELSE                        ! IF (DELHG(LG).GE.DELHINOUT(LG) ...
                NGCHECK(LG)=0
                CALL EBBMASKRE
              ENDIF                       ! IF (DELHG(LG).GE.DELHINOUT(LG) ...
            ELSE                          ! IF (DIFEL(LG).GT.0.02) THEN
              NGCHECK(LG)=0                 
              CALL EBBMASKRE
            ENDIF                         ! IF (DIFEL(LG).GT.0.02) THEN

          ELSE ! IF (GGQSUM(LG).LE.GQSUMT) THEN
            NGCHECK(LG)=0                 
            CALL EBBMASKRE
          ENDIF
      !###########################################################
      !#      UPSTREAM -> DOWNSTREAM : USER DEFINE (END)
      !###########################################################
                         ENDIF !ELSEIF (IAGT.EQ.1) THEN
        ENDIF                             ! IF(NQCTYP(NCTL).EQ.3 .AND. N.GE. ...
C###########################################################
C#      UPSTREAM -> DOWNSTREAM (END)
C###########################################################
C
C
C
C###########################################################
C#      DOWNSTREAM -> UPSTREAM (START)
C###########################################################
        IF(NQCTYP(NCTL).EQ.4 .AND. NGCCHECK(LG).EQ.0) THEN  ! DOWNSTREAM -> UPSTREAM
          IF (TIDCHK(LG).EQ.-1.) DIFEL(LG)=-1.  ! NOT CONSIDER TIDE LEVLE

                         IF (IAGT.EQ.0) THEN
                           NGCHECK(LG)=0                 
                           CALL FLDMASKRE
                         ELSEIF (IAGT.EQ.1) THEN

          IF (DIFEL(LG).LT.-0.02) THEN         ! FLOOD PERIOD
            IF (DELHG(LG).GE.DELHINOUT(LG)
     &         .AND. HDWG(LG).GT.HUPG(LG)) THEN ! DOWNSTREAM LEVEL HIGHER THAN UPSTREAM
              IF (HUPG(LG).LT.SEL2T
     &           .AND. (SEL2T-HUPG(LG)).GE.DELHSEL1(LG)) THEN  ! UPSTREAM LEVEL LOWER THAN TARGET HIGH LEVEL

                NGATE_EF=4                                        ! FLOOD SWITCH  (3:EBB, 4:FLOOD)

                IF (NGCHECK(LG).EQ.0) THEN                        ! GATE OPEN CHECK
                  NGCOUNT(LG)=1                                   ! TIME STEP COUNT AFTER GATE OPEN
                  NGCHECK(LG)=1                                   ! GATE OPEN CHECK
                ELSE
                  NGCOUNT(LG)=NGCOUNT(LG)+1                       ! ADD TIME STEP 
                ENDIF

                IF (GOTIME(LG).GT.0.) THEN
                  GRAMPUP(LG)=FLOAT(NGCOUNT(LG)) / 
     &                        (NGATEC(NCTL)*GOTIME(LG)*60./DT)  ! GATE OPEN TIME INTERVAL RAMPUP TERM
                ELSE
                  GRAMPUP(LG)=1.
                ENDIF

                GRAMPUP(LG) = MIN(GRAMPUP(LG), 1.0)  ! ADJUST GATE RAMPUP VALUE
c                SILLHHO(LG)=GUPHT*GRAMPUP(LG)                    ! GATE HEIGHT AFTER GATE OPEN (NOT USE)

C++++++++++++++ BEGIN NORMAL FORMULA
                IF (DEPDWG(LG).LE.GUPHT) THEN                    ! OVERFLOW OR WEIR
                  IF (DELHG(LG).GE.DEPDWG(LG)/3.) THEN                ! COMPLETE OVERFLOW
                    IGCHECK(LG)=51
                    DUMPG(LG)=1.704*CG1(LG)*GATEWI(LG)                
     &                       *(DEPDWG(LG)**(3./2.))                
                  ELSEIF (DELHG(LG).LT.DEPDWG(LG)/3.) THEN            ! SUBMERGED WEIR
                    IGCHECK(LG)=52
                    DUMPG(LG)=CG2(LG)*(DEPDWG(LG)-DELHG(LG))
     &                       *GATEWO(LG)*SQRT(2.*9.806*DELHG(LG))     
                  ENDIF
                ELSE                                                  ! ORIFICE CONDITION
                  IF (GUPHT.GT.DEPUPG(LG)) THEN                  ! INCOMPLETE ORIFICE
                    DELCH=DEPDWG(LG)-GUPHT
                    IF (NCG3FOM(LG).EQ.1) THEN
                      IGCHECK(LG)=53
                      GQ1=CG3(LG)*(GUPHT-DEPUPG(LG))
     &                    *GATEWO(LG)*SQRT(2.*9.806*(DELHG(LG)-DELCH))
                    ELSEIF (NCG3FOM(LG).EQ.2) THEN
                      IGCHECK(LG)=54
                      GQ1=2./3.*CG3(LG)*GATEWO(LG)*SQRT(2.*9.806)
     &                       *( DELHG(LG)**(3./2.) - DELCH**(3./2.) )
                    ENDIF
                    DUMPG(LG)=GQ1 + CG4(LG)*(DEPDWG(LG)-DELHG(LG))
     &                        *GATEWO(LG)*SQRT(2.*9.806*DELHG(LG))
                  ELSE                                                ! SUBMERGED ORIFICE
                    IGCHECK(LG)=55
                    DUMPG(LG)=CG5(LG)*GUPHT*GATEWO(LG)
     &                       *SQRT(2.*9.806*DELHG(LG))
                  ENDIF  ! IF (DELHG(LG).LT.DEPUPG(LG)/3.) THEN
                ENDIF  ! IF (DEPDWG(LG).GT.GUPHT) THEN
C++++++++++++++++ END NORMAL FORMULA
C
C
!     CHECK MAXIMUM GATE FLUX
                DUMPG(LG)=DUMPG(LG)*FLOAT(NGATE(M1,NCTL))    ! CONSIDER GATE NUMBER
                RMAXQ=FLOAT(NGATE(M1,NCTL))*MAXQ(LG)/FLOAT(NGATEM(NCTL))
                IF (RMAXQ.GT.0. .AND. DUMPG(LG).GT.RMAXQ) THEN  ! MAX. FLUX CHECK
                  DUMPG(LG)=RMAXQ/FLOAT(NGATEC(NCTL))  ! APPLIED MAX. FLUX AND CONSIDER CELL NO.
                ELSE
                  DUMPG(LG)=DUMPG(LG)/FLOAT(NGATEC(NCTL)) ! CONSIDER CELL NO.
                ENDIF                  

                DUMPG(LG)=DUMPG(LG)*GRAMPUP(LG)           ! ADJUST GATE FLUX
                DUMPGPREV(LG)=DUMPG(LG)                   ! SAVE GATE FLUX FOR CONTROL AFTER CLOSE

                DO K=1,KC  
                  QCTLT(K,NCTL)=DUMPG(LG)*GKMULT(K) !*DZC(K)       ! SAVE GATE FLUX
                ENDDO  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK START (FLOOD)
                CALL FLDMASK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK END

              ELSE                      ! IF (HUPG(LG).LT.SEL2(LG) ...
                NGCHECK(LG)=0
                CALL FLDMASKRE
              ENDIF                     ! IF (HUPG(LG).LT.SEL2(LG) ...
            ELSE  !IF (DELHG(LG).GE.DELHINOUT(LG)             
              NGCHECK(LG)=0
              CALL FLDMASKRE
            ENDIF !IF (DELHG(LG).GE.DELHINOUT(LG)
          ELSE   ! IF (DIFEL(LG).LT.-0.02) THEN
            NGCHECK(LG)=0
            CALL FLDMASKRE
          ENDIF  ! IF (DIFEL(LG).LT.-0.02) THEN

      !###########################################################
      !#      DOWNSTREAM -> UPSTREAM : USER DEFINE (START)
      !###########################################################
                         ELSEIF (IAGT.EQ.2) THEN !ELSEIF (IAGT.EQ.1) THEN
          IF (GGQSUM(NCTL).LE.GQSUMT) THEN
            IF (DIFEL(LG).LT.-0.02) THEN         ! FLOOD PERIOD
              IF (DELHG(LG).GE.DELHINOUT(LG)
     &           .AND. HDWG(LG).GT.HUPG(LG)) THEN ! DOWNSTREAM LEVEL HIGHER THAN UPSTREAM
                IF (HUPG(LG).LT.SEL2T
     &             .AND. (SEL2T-HUPG(LG)).GE.DELHSEL1(LG)) THEN  ! UPSTREAM LEVEL LOWER THAN TARGET HIGH LEVEL

                  IF (NGCHECK(LG).EQ.0) THEN                        ! GATE OPEN CHECK
                    NGCOUNT(LG)=1                                   ! TIME STEP COUNT AFTER GATE OPEN
                    NGCHECK(LG)=1                                   ! GATE OPEN CHECK
                  ELSE
                    NGCOUNT(LG)=NGCOUNT(LG)+1                       ! ADD TIME STEP 
                  ENDIF

                  IF (GOTIME(LG).GT.0.) THEN
                    GRAMPUP(LG)=FLOAT(NGCOUNT(LG)) / 
     &                          (NGATEC(NCTL)*GOTIME(LG)*60./DT)  ! GATE OPEN TIME INTERVAL RAMPUP TERM
                  ELSE
                    GRAMPUP(LG)=1.
                  ENDIF
                  GRAMPUP(LG) = MIN(GRAMPUP(LG), 1.0)  ! ADJUST GATE RAMPUP VALUE

c                SILLHHO(LG)=GUPHT*GRAMPUP(LG)                    ! GATE HEIGHT AFTER GATE OPEN (NOT USE)

C++++++++++++++ BEGIN NORMAL FORMULA
                  IF (DEPDWG(LG).LE.GUPHT) THEN                    ! OVERFLOW OR WEIR
                    IF (DELHG(LG).GE.DEPDWG(LG)/3.) THEN                ! COMPLETE OVERFLOW
                      IGCHECK(LG)=61
                      DUMPG(LG)=1.704*CG1(LG)*GATEWI(LG)                
     &                       *(DEPDWG(LG)**(3./2.))                
                    ELSEIF (DELHG(LG).LT.DEPDWG(LG)/3.) THEN            ! SUBMERGED WEIR
                      IGCHECK(LG)=62
                      DUMPG(LG)=CG2(LG)*(DEPDWG(LG)-DELHG(LG))
     &                       *GATEWO(LG)*SQRT(2.*9.806*DELHG(LG))     
                    ENDIF
                  ELSE                                                  ! ORIFICE CONDITION
                    IF (GUPHT.GT.DEPUPG(LG)) THEN                  ! INCOMPLETE ORIFICE
                      DELCH=DEPDWG(LG)-GUPHT
                      IF (NCG3FOM(LG).EQ.1) THEN
                        IGCHECK(LG)=63
                        GQ1=CG3(LG)*(GUPHT-DEPUPG(LG))
     &                    *GATEWO(LG)*SQRT(2.*9.806*(DELHG(LG)-DELCH))
                      ELSEIF (NCG3FOM(LG).EQ.2) THEN
                        IGCHECK(LG)=64
                        GQ1=2./3.*CG3(LG)*GATEWO(LG)*SQRT((2.
     &                      *9.806)*(DELHG(LG)**(3./2.)-DELCH**(3./2.)))
                      ENDIF
                      DUMPG(LG)=GQ1+CG4(LG)*(DEPDWG(LG)
     &                   -DELHG(LG))*GATEWO(LG)*SQRT(2.*9.806*DELHG(LG))
                    ELSE                                                ! SUBMERGED ORIFICE
                      IGCHECK(LG)=65
                      DUMPG(LG)=CG5(LG)*GUPHT*GATEWO(LG)
     &                       *SQRT(2.*9.806*DELHG(LG))
                    ENDIF  ! IF (DELHG(LG).LT.DEPUPG(LG)/3.) THEN
                  ENDIF  ! IF (DEPDWG(LG).GT.GUPHT) THEN
C++++++++++++++++ END NORMAL FORMULA
C
C
!     CHECK MAXIMUM GATE FLUX
                  DUMPG(LG)=DUMPG(LG)*FLOAT(NGATE(M1,NCTL))    ! CONSIDER GATE NUMBER
                RMAXQ=FLOAT(NGATE(M1,NCTL))*MAXQ(LG)/FLOAT(NGATEM(NCTL))
                  IF (RMAXQ.GT.0. .AND. DUMPG(LG).GT.RMAXQ) THEN  ! MAX. FLUX CHECK
                    DUMPG(LG)=RMAXQ/FLOAT(NGATEC(NCTL))  ! APPLIED MAX. FLUX AND CONSIDER CELL NO.
                  ELSE
                    DUMPG(LG)=DUMPG(LG)/FLOAT(NGATEC(NCTL)) ! CONSIDER CELL NO.
                  ENDIF                  
 
                  DUMPG(LG)=DUMPG(LG)*GRAMPUP(LG)           ! ADJUST GATE FLUX
                  DUMPGPREV(LG)=DUMPG(LG)                   ! SAVE GATE FLUX FOR CONTROL AFTER CLOSE

                  DO K=1,KC  
                    QCTLT(K,NCTL)=DUMPG(LG)*GKMULT(K) !*DZC(K)       ! SAVE GATE FLUX
                  ENDDO  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK START (FLOOD)
                  CALL FLDMASK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK END

                ELSE                      ! IF (HUPG(LG).LT.SEL2(LG) ...
                  NGCHECK(LG)=0
                  CALL FLDMASKRE
                ENDIF                     ! IF (HUPG(LG).LT.SEL2(LG) ...
              ELSE  !IF (DELHG(LG).GE.DELHINOUT(LG)             
                NGCHECK(LG)=0
                CALL FLDMASKRE
              ENDIF !IF (DELHG(LG).GE.DELHINOUT(LG)
            ELSE   ! IF (DIFEL(LG).LT.-0.02) THEN
              NGCHECK(LG)=0
              CALL FLDMASKRE
            ENDIF  ! IF (DIFEL(LG).LT.-0.02) THEN
          ELSE ! IF (GGQSUM(LG).LE.GQSUMT) THEN
            NGCHECK(LG)=0                 
            CALL FLDMASKRE
          ENDIF
                         ENDIF  !ELSEIF (IAGT.EQ.2) THEN

        ENDIF      ! IF(NQCTYP(NCTL).EQ.4 ...
      !###########################################################
      !#      DOWNSTREAM -> UPSTREAM : USER DEFINE (END)
      !###########################################################


C###########################################################
C#      DOWNSTREAM -> UPSTREAM (END)
C###########################################################
C
C
C
C###########################################################
C#      UPSTREAM -> DOWNSTREAM : ESTUARY DIKE (START)
C###########################################################
        IF(NQCTYP(NCTL).EQ.6 .AND. NGCCHECK(LG).EQ.0
     &     .AND. N.GE.INT(GARTM*60./DT))THEN   ! UPSTREAM -> DOWNSTREAM
          IF (TIDCHK(LG).EQ.-1.) DIFEL(LG)=1.  ! NOT CONSIDER TIDE LEVLE
          IF (DIFEL(LG).GT.0.02) THEN         ! EBB PERIOD
            IF (DELHG(LG).GE.DELHINOUT(LG)
     &         .AND. HUPG(LG).GT.HDWG(LG)) THEN ! UPSTREAM LEVEL HIGHER THAN DOWNSTREAM
              IF (HUPG(LG).GE.SEL1T
     &           .AND. (HUPG(LG)-SEL1T).GE.DELHSEL1(LG)) THEN  ! UPSTREAM LEVEL HIGHER THAN TARGET LOW LEVEL
C
              ! open time control : jgcho 2010.8.17 temporary
!              IF (N*DT/86400. .GE. GATEOTM) THEN

                  NGATE_EF=3                                        ! EBB SWITCH  (3:EBB, 4:FLOOD) ?

                IF (NGCHECK(LG).EQ.0) THEN                        ! GATE OPEN CHECK
                  NGCOUNT(LG)=1                                   ! TIME STEP COUNT AFTER GATE OPEN
                  NGCHECK(LG)=1                                   ! GATE OPEN CHECK
                ELSE
                  NGCOUNT(LG)=NGCOUNT(LG)+1                       ! ADD TIME STEP 
                ENDIF
C
                IF (GOTIME(LG).GT.0.) THEN
                  GRAMPUP(LG)=FLOAT(NGCOUNT(LG)) / 
     &                        (NGATEC(NCTL)*GOTIME(LG)*60./DT)  ! GATE OPEN TIME INTERVAL RAMPUP TERM
                ELSE
                  GRAMPUP(LG)=1.
                ENDIF
C
                GRAMPUP(LG) = MIN(GRAMPUP(LG), 1.0)  ! ADJUST GATE RAMPUP VALUE
C
C                SILLHHO(LG)=GUPHT*GRAMPUP(LG)                    ! GATE HEIGHT AFTER GATE OPEN (NOT USE)
C
C++++++++++++++ BEGIN NORMAL FORMULA
                IF (DEPUPG(LG).LE.GUPHT) THEN                    ! OVERFLOW OR WEIR
                  IF (DELHG(LG).GE.DEPUPG(LG)/3.) THEN                ! COMPLETE OVERFLOW
                    IGCHECK(LG)=1
                    DUMPG(LG)=1.704*CG1(LG)*GATEWI(LG)                
     &                       *(DEPUPG(LG)**(3./2.))                
                  ELSEIF (DELHG(LG).LT.DEPUPG(LG)/3.) THEN            ! SUBMERGED WEIR
                    IGCHECK(LG)=2
                    DUMPG(LG)=CG2(LG)*(DEPUPG(LG)-DELHG(LG))
     &                       *GATEWI(LG)*SQRT(2.*9.806*DELHG(LG))
                  ENDIF
                ELSE                                                  ! ORIFICE CONDITION
                  IF (GUPHT.GT.DEPDWG(LG)) THEN                  ! INCOMPLETE ORIFICE
                    DELCH=DEPUPG(LG)-GUPHT
                    IF (NCG3FOM(LG).EQ.1) THEN
                      IGCHECK(LG)=3
                      GQ1=CG3(LG)*(GUPHT-DEPDWG(LG))
     &                    *GATEWI(LG)*SQRT(2.*9.806*(DELHG(LG)-DELCH))
                    ELSEIF (NCG3FOM(LG).EQ.2) THEN
                      IGCHECK(LG)=4
                      GQ1=2./3.*CG3(LG)*GATEWI(LG)*SQRT(2.*9.806)
     &                       *( DELHG(LG)**(3./2.) - DELCH**(3./2.) )
                    ENDIF
                    DUMPG(LG)=GQ1 + CG4(LG)*(DEPUPG(LG)-DELHG(LG))
     &                        *GATEWI(LG)*SQRT(2.*9.806*DELHG(LG))
                  ELSE                                                ! SUBMERGED ORIFICE
                    IGCHECK(LG)=5
                    DUMPG(LG)=CG5(LG)*GUPHT*GATEWI(LG)
     &                       *SQRT(2.*9.806*DELHG(LG))
                  ENDIF  ! IF (DELHG(LG).LT.DEPUPG(LG)/3.) THEN
                ENDIF  ! IF (DEPUPG(LG).GT.GUPHT) THEN
C++++++++++++++++ END NORMAL FORMULA
C
C
!     CHECK MAXIMUM GATE FLUX
                DUMPG(LG)=DUMPG(LG)*FLOAT(NGATE(M1,NCTL))    ! CONSIDER GATE NUMBER
                RMAXQ=FLOAT(NGATE(M1,NCTL))*MAXQ(LG)/FLOAT(NGATEM(NCTL))
                IF (RMAXQ.GT.0. .AND. DUMPG(LG).GT.RMAXQ) THEN  ! MAX. FLUX CHECK
                  DUMPG(LG)=RMAXQ/FLOAT(NGATEC(NCTL))  ! APPLIED MAX. FLUX AND CONSIDER CELL NO.
                ELSE
                  DUMPG(LG)=DUMPG(LG)/FLOAT(NGATEC(NCTL)) ! CONSIDER CELL NO.
                ENDIF                  
C DUMP=DUMP*FLOAT(NGGATE(NCTL))/FLOAT(NGGATEC(NCTL))
                DUMPG(LG)=DUMPG(LG)*GRAMPUP(LG)           ! ADJUST GATE FLUX
                DUMPGPREV(LG)=DUMPG(LG)                   ! SAVE GATE FLUX FOR CONTROL AFTER CLOSE
C
                DO K=1,KC  
                  QCTLT(K,NCTL)=DUMPG(LG)*GKMULT(K) !*DZC(K)       ! SAVE GATE FLUX
                ENDDO  

C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK START (EBB)
                CALL EBBMASK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK END

!              ELSE  ! IF (TIME.GE.GATEOTM) THEN open time control : jgcho 2010.8.17 temporary
!                NGCHECK(LG)=0
!                CALL EBBMASKRE
!			ENDIF ! IF (TIME.GE.GATEOTM) THEN open time control : jgcho 2010.8.17 temporary

			ELSE                      ! IF (HUPG(LG).GE.SEL1(LG) ...
                NGCHECK(LG)=0
                CALL EBBMASKRE

              ENDIF                     ! IF (HUPG(LG).GE.SEL1(LG) ...
            ELSE                        ! IF (DELHG(LG).GE.DELHINOUT(LG) ...
              NGCHECK(LG)=0
              CALL EBBMASKRE
            ENDIF                       ! IF (DELHG(LG).GE.DELHINOUT(LG) ...
          ELSE                          ! IF (DIFEL(LG).GT.0.02) THEN
            NGCHECK(LG)=0                 
            CALL EBBMASKRE
          ENDIF                         ! IF (DIFEL(LG).GT.0.02) THEN
        ENDIF                             ! IF(NQCTYP(NCTL).EQ.3 .AND. N.GE. ...
C###########################################################
C#      UPSTREAM -> DOWNSTREAM : ESTUARY DIKE (END)
C###########################################################

C###########################################################
C#      UPSTREAM -> DOWNSTREAM : WEIR (START) NQCTYP(NCTL).EQ.7 (2011.3.4 jgcho)
C###########################################################
        IF(NQCTYP(NCTL).EQ.7 .AND. NGCCHECK(LG).EQ.0) THEN

          !SEL1T=SILL(LG)-GUPHT ! 2014.10.14 jgcho ! ! GEOSR 2015.01.09 UNG ERROR::To using a 'sel1t' at weir
            
                         IF (IAGT.EQ.0) THEN
                           NGCHECK(LG)=0                 
                           CALL EBBMASKRE
                         ELSEIF (IAGT.EQ.1) THEN
          IF (HUPG(LG).GT.HDWG(LG)
     &      .AND. HUPG(LG).GT.SILL(LG) )THEN   ! UPSTREAM -> DOWNSTREAM 
            IF (NGCHECK(LG).EQ.0) THEN                        ! GATE OPEN CHECK
              NGCOUNT(LG)=1                                   ! TIME STEP COUNT AFTER GATE OPEN
              NGCHECK(LG)=1                                   ! GATE OPEN CHECK
            ELSE
              NGCOUNT(LG)=NGCOUNT(LG)+1                       ! ADD TIME STEP 
            ENDIF
C
            IF (GOTIME(LG).GT.0.) THEN
              GRAMPUP(LG)=FLOAT(NGCOUNT(LG)) / 
     &                    (NGATEC(NCTL)*GOTIME(LG)*60./DT)  ! GATE OPEN TIME INTERVAL RAMPUP TERM
            ELSE
              GRAMPUP(LG)=1.
            ENDIF
C 
            GRAMPUP(LG) = MIN(GRAMPUP(LG), 1.0)                   ! ADJUST GATE RAMPUP VALUE

C++++++++++++++ BEGIN NORMAL FORMULA
            IF (HDWG(LG) .LT. SILL(LG) ) THEN   ! (1) IF (DEPDWG(LG) .LT. SILL(LG))
              IGCHECK(LG)=71
              tcdiff=CGH2(LG)-CGH1(LG)
              h11=HUPG(LG)-SILL(LG)
              wctm1=(CGH2(LG)-h11)/tcdiff
              wctm2=(h11-CGH1(LG))/tcdiff
              CG10=wctm1*CG1(LG) + wctm2*CG2(LG)
!              CG10=((HUPG(LG)-SEL1T)/(CGH2(LG)-CGH1(LG))		! 2014.11.03 ung CG1(LG) -> CG1(LG),CG2(LG),CGH1(LG),CGH2(LG)
!     &             *(CG2(LG)-CG1(LG))+CG1(LG))							! 2014.11.03 ung CG1(LG) -> CG1(LG),CG2(LG),CGH1(LG),CGH2(LG)
     					IF((HUPG(LG)-SILL(LG)).LT.CGH1(LG)) THEN
     						CG10=CG1(LG)
     					ELSEIF((HUPG(LG)-SILL(LG)).GT.CGH2(LG)) THEN
     						CG10=CG2(LG)
     					ENDIF		
              DUMPG(LG)=CG10*GATEWI(LG)*(HUPG(LG)-SILL(LG))    ! 2014.10.14 jgcho SILL(LG) -> SEL1T
     &                 *SQRT(2.*9.806*(HUPG(LG)-SILL(LG)))     ! 2014.10.14 jgcho SILL(LG) -> SEL1T
            ELSE                                 ! (2) DEPDWG(LG) .GE. SILL(LG)
              IGCHECK(LG)=72
              tcdiff=CGH2(LG)-CGH1(LG)
              h11=HUPG(LG)-SEL1T
              wctm1=(CGH2(LG)-h11)/tcdiff
              wctm2=(h11-CGH1(LG))/tcdiff
              CG10=wctm1*CG1(LG) + wctm2*CG2(LG)
!              CG10=((HUPG(LG)-SEL1T)/(CGH2(LG)-CGH1(LG))		! 2014.11.03 ung CG1(LG) -> CG1(LG),CG2(LG),CGH1(LG),CGH2(LG)
!     &             *(CG2(LG)-CG1(LG))+CG1(LG))							! 2014.11.03 ung CG1(LG) -> CG1(LG),CG2(LG),CGH1(LG),CGH2(LG)
     					IF((HUPG(LG)-SILL(LG)).LT.CGH1(LG)) THEN
     						CG10=CG1(LG)
     					ELSEIF((HUPG(LG)-SILL(LG)).GT.CGH2(LG)) THEN
     						CG10=CG2(LG)
     					ENDIF
              DUMPG(LG)=CG10*2.6*GATEWI(LG)*(HDWG(LG)-SILL(LG))  ! 2014.11.03 ung CG1(LG) -> CG1(LG),CG2(LG),CGH1(LG),CGH2(LG)
     &           				*SQRT(2.*9.806   											! 2014.10.14 jgcho SILL(LG) -> SEL1T
     &                  *((HUPG(LG)-SILL(LG))-(HDWG(LG)-SILL(LG)))) ! 2014.10.14 jgcho SILL(LG) -> SEL1T
            ENDIF                                ! (1) IF (DEPDWG(LG) .LT. SILL(LG))
C++++++++++++++++ END NORMAL FORMULA
! { 2014.10.14 jgcho 
            if (NQCTLQ(LG).eq.0) then
              DUMPG(LG)=DUMPG(LG)*FLOAT(NGATEM(NCTL))  ! CONSIDER GATE NUMBER
            else
              DUMPG(LG)=DUMPG(LG)*FLOAT(NGATET)  ! CONSIDER GATE NUMBER
            endif
! } 2014.10.14 jgcho 
            DUMPG(LG)=DUMPG(LG)/FLOAT(NGATEC(NCTL)) ! CONSIDER CELL NO.
            DUMPG(LG)=DUMPG(LG)*GRAMPUP(LG)           ! ADJUST GATE FLUX
            
! GEOSR GATE. Suggestion: Change subsequent lines into:
!           QCTLT(1:KC-1,NCTL) = 0.
!           QCTLT(KC,NCTL) = DUMPG(LG)
!
!
!           Code using DUMPGPREV is not used anymore, has this become obsolete?
!
!            IF (N.eq.1) DUMPGPREV(LG)=0.
!            IF (MOD(N+1,IATS(LG)).eq.0) DUMPGPREV(LG)=DUMPG(LG)
            
!            IF (NGCOUNT(LG).lt.IATS(LG)/10) THEN
!              wctm1=float(IATS(LG)/10-NGCOUNT(LG))/float(IATS(LG)/10)
!              wctm2=float(NGCOUNT(LG)-1)/float(IATS(LG)/10)
!              DUMPTMP=wctm1*DUMPGPREV(LG) + wctm2*DUMPG(LG)
!            ELSE
              DUMPTMP=DUMPG(LG)
!            ENDIF           
            DO K=1,KC  
              if (K.eq.KC) then
                GKFIX=1.
              else
                GKFIX=0.
              endif
              QCTLT(K,NCTL)=DUMPTMP*GKFIX ! GKMULT(K) !*DZC(K)       ! SAVE GATE FLUX
            ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK START (EBB)
            CALL EBBMASK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK END

          ELSE  ! IF (HUPG(LG).GT.HDWG(LG)
            NGCHECK(LG)=0
            CALL EBBMASKRE
          ENDIF ! IF (HUPG(LG).GT.HDWG(LG)
                         ENDIF !IF (IAGT.EQ.0) THEN

        ENDIF      ! IF(NQCTYP(NCTL).EQ.7 ...
C###########################################################
C#      UPSTREAM -> DOWNSTREAM : WEIR (END) NQCTYP(NCTL).EQ.7 (2011.3.4 jgcho)
C###########################################################


C###########################################################
C#      UPSTREAM -> DOWNSTREAM : FISH WAY (START) NQCTYP(NCTL).EQ.8 (2011.10.27 jgcho)
C###########################################################
        IF(NQCTYP(NCTL).EQ.8 .AND. NGCCHECK(LG).EQ.0) THEN

                         IF (IAGT.EQ.0) THEN
                           NGCHECK(LG)=0                 
                           CALL EBBMASKRE
                         ELSEIF (IAGT.EQ.1) THEN

          IF (HUPG(LG).GT.HDWG(LG)
     &      .AND. HUPG(LG).GT.SEL1T )THEN   ! UPSTREAM -> DOWNSTREAM 

            IF (NGCHECK(LG).EQ.0) THEN                        ! GATE OPEN CHECK
              NGCOUNT(LG)=1                                   ! TIME STEP COUNT AFTER GATE OPEN
              NGCHECK(LG)=1                                   ! GATE OPEN CHECK
            ELSE
              NGCOUNT(LG)=NGCOUNT(LG)+1                       ! ADD TIME STEP 
            ENDIF
C
            IF (GOTIME(LG).GT.0.) THEN
              GRAMPUP(LG)=FLOAT(NGCOUNT(LG)) / 
     &                    (NGATEC(NCTL)*GOTIME(LG)*60./DT)  ! GATE OPEN TIME INTERVAL RAMPUP TERM
            ELSE
              GRAMPUP(LG)=1.
            ENDIF
C
            GRAMPUP(LG) = MIN(GRAMPUP(LG), 1.0)  ! ADJUST GATE RAMPUP VALUE

C++++++++++++++ BEGIN NORMAL FORMULA
            IGCHECK(LG)=8
            DUMPG(LG)=GQSUMT
C++++++++++++++++ END NORMAL FORMULA

            DUMPG(LG)=DUMPG(LG)*FLOAT(NGATE(M1,NCTL))  ! CONSIDER GATE NUMBER
            DUMPG(LG)=DUMPG(LG)/FLOAT(NGATEC(NCTL)) ! CONSIDER CELL NO.
            DUMPG(LG)=DUMPG(LG)*GRAMPUP(LG)           ! ADJUST GATE FLUX
            DUMPGPREV(LG)=DUMPG(LG)                 ! SAVE GATE FLUX FOR CONTROL AFTER CLOSE
            DO K=1,KC  
              QCTLT(K,NCTL)=DUMPG(LG)*GKMULT(K) !*DZC(K)       ! SAVE GATE FLUX
            ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK START (EBB)
            CALL EBBMASK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK END

          ELSE  ! IF (HUPG(LG).GT.HDWG(LG)
            NGCHECK(LG)=0
            CALL EBBMASKRE
          ENDIF ! IF (HUPG(LG).GT.HDWG(LG)
                         ENDIF !IF (IAGT.EQ.0) THEN

        ENDIF      ! IF(NQCTYP(NCTL).EQ.8 ...
C###########################################################
C#      UPSTREAM -> DOWNSTREAM : FISH WAY (END) NQCTYP(NCTL).EQ.8 (2011.10.27 jgcho)
C###########################################################


C###########################################################
C#      UPSTREAM -> DOWNSTREAM : POWER GENERATOR (START) NQCTYP(NCTL).EQ.9 (2011.10.27 jgcho)
C###########################################################
        IF(NQCTYP(NCTL).EQ.9 .AND. NGCCHECK(LG).EQ.0) THEN

            GAAA=(GQPHI-GQPLO)/(GHILEV-GLOLEV)
            GBBB=(GHILEV*GQPLO - GLOLEV*GQPHI)/(GHILEV-GLOLEV)
            
		      GQSUMT=GAAA*HUPG(LG)+GBBB
            if (GQSUMT.gt.GQPHI) GQSUMT=GQPHI
!      WRITE(*,*) GLOLEV,GHILEV,GQPLO,GQPHI,DELHG(LG),GQSUMT
!      STOP
                         IF (IAGT.EQ.0) THEN
                           NGCHECK(LG)=0                 
                           CALL EBBMASKRE
                         ELSEIF (IAGT.EQ.1) THEN

          IF (HUPG(LG).GT.HDWG(LG)
     &      .AND. HUPG(LG).GT.SEL1T )THEN   ! UPSTREAM -> DOWNSTREAM 

            IF (NGCHECK(LG).EQ.0) THEN                        ! GATE OPEN CHECK
              NGCOUNT(LG)=1                                   ! TIME STEP COUNT AFTER GATE OPEN
              NGCHECK(LG)=1                                   ! GATE OPEN CHECK
            ELSE
              NGCOUNT(LG)=NGCOUNT(LG)+1                       ! ADD TIME STEP 
            ENDIF
C
            IF (GOTIME(LG).GT.0.) THEN
              GRAMPUP(LG)=FLOAT(NGCOUNT(LG)) / 
     &                    (NGATEC(NCTL)*GOTIME(LG)*60./DT)  ! GATE OPEN TIME INTERVAL RAMPUP TERM
            ELSE
              GRAMPUP(LG)=1.
            ENDIF
C
            GRAMPUP(LG) = MIN(GRAMPUP(LG), 1.0)  ! ADJUST GATE RAMPUP VALUE

C++++++++++++++ BEGIN NORMAL FORMULA
            IGCHECK(LG)=9
            DUMPG(LG)=GQSUMT
C++++++++++++++++ END NORMAL FORMULA

            DUMPG(LG)=DUMPG(LG)*FLOAT(NGATE(M1,NCTL))  ! CONSIDER GATE NUMBER
            DUMPG(LG)=DUMPG(LG)/FLOAT(NGATEC(NCTL)) ! CONSIDER CELL NO.
            DUMPG(LG)=DUMPG(LG)*GRAMPUP(LG)           ! ADJUST GATE FLUX
            DUMPGPREV(LG)=DUMPG(LG)                 ! SAVE GATE FLUX FOR CONTROL AFTER CLOSE
            DO K=1,KC  
              QCTLT(K,NCTL)=DUMPG(LG)*GKMULT(K) !*DZC(K)       ! SAVE GATE FLUX
            ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK START (EBB)
            CALL EBBMASK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK END

          ELSE  ! IF (HUPG(LG).GT.HDWG(LG)
            NGCHECK(LG)=0
            CALL EBBMASKRE
          ENDIF ! IF (HUPG(LG).GT.HDWG(LG)
                         ENDIF !IF (IAGT.EQ.0) THEN

        ENDIF      ! IF(NQCTYP(NCTL).EQ.9 ...
C###########################################################
C#      UPSTREAM -> DOWNSTREAM : POWER GENERATOR (END) NQCTYP(NCTL).EQ.9 (2011.10.27 jgcho)
C###########################################################

C###########################################################
C#      UPSTREAM -> DOWNSTREAM : USE FLUX TABLE (END) NQCTYP(NCTL).EQ.13 (2014.9 ung)
C###########################################################
        IF(NQCTYP(NCTL).EQ.13 .AND. NGCCHECK(LG).EQ.0) THEN
                         IF (IAGT.EQ.0) THEN
                           NGCHECK(LG)=0
                           CALL EBBMASKRE
                         ELSEIF (IAGT.EQ.1) THEN
          IF (HUPG(LG).GT.HDWG(LG)
     &      .AND. HUPG(LG).GT.SEL1T )THEN   ! UPSTREAM -> DOWNSTREAM 
            IF (NGCHECK(LG).EQ.0) THEN                        ! GATE OPEN CHECK
              NGCOUNT(LG)=1                                   ! TIME STEP COUNT AFTER GATE OPEN
              NGCHECK(LG)=1                                   ! GATE OPEN CHECK
            ELSE
              NGCOUNT(LG)=NGCOUNT(LG)+1                       ! ADD TIME STEP 
            ENDIF
C
            IF (GOTIME(LG).GT.0.) THEN
              GRAMPUP(LG)=FLOAT(NGCOUNT(LG)) / 
     &                    (NGATEC(NCTL)*GOTIME(LG)*60./DT)  ! GATE OPEN TIME INTERVAL RAMPUP TERM
            ELSE
              GRAMPUP(LG)=1.
            ENDIF
C
            GRAMPUP(LG) = MIN(GRAMPUP(LG), 1.0)  ! ADJUST GATE RAMPUP VALUE
! { GEOSR 2015.01.09 UNG !fixed interpolation
C++++++++++++++ BEGIN NORMAL FORMULA
            EP=0;EPG=0
            DO I=1,NOELE(NGTYP(LG))
              IF(HUPG(LG).GE.ELET(NGTYP(LG),I)) THEN
                EP=I
              ENDIF
            ENDDO
            DO I=1,NOGELE(NGTYP(LG))
              IF(GUPHT.GE.GELE(NGTYP(LG),I)) THEN
                EPG=I
              ENDIF
            ENDDO
            IF(EPG.EQ.0) THEN
              DUMPG(LG)=0.
            ELSE
             IF(NOELE(NGTYP(LG)).eq.EP) then
               DUMPG(LG)=(QT(NGTYP(LG),EP,EPG)+(QT(NGTYP(LG),EP,EPG+1)
     &        -QT(NGTYP(LG),EP,EPG))*(GUPHT-GELE(NGTYP(LG),EPG))
     &        /(GELE(NGTYP(LG),EPG+1)-GELE(NGTYP(LG),EPG)))
             ELSE
               DUMPG(LG)=(QT(NGTYP(LG),EP,EPG)+(QT(NGTYP(LG),EP,EPG+1)
     &        -QT(NGTYP(LG),EP,EPG))*(GUPHT-GELE(NGTYP(LG),EPG))
     &        /(GELE(NGTYP(LG),EPG+1)-GELE(NGTYP(LG),EPG)))+
     &         ((QT(NGTYP(LG),EP+1,EPG)+(QT(NGTYP(LG),EP+1,EPG+1)
     &        -QT(NGTYP(LG),EP+1,EPG))*(GUPHT-GELE(NGTYP(LG),EPG))
     &        /(GELE(NGTYP(LG),EPG+1)-GELE(NGTYP(LG),EPG)))-
     &         (QT(NGTYP(LG),EP,EPG)+(QT(NGTYP(LG),EP,EPG+1)
     &        -QT(NGTYP(LG),EP,EPG))*(GUPHT-GELE(NGTYP(LG),EPG))
     &        /(GELE(NGTYP(LG),EPG+1)-GELE(NGTYP(LG),EPG))))*
     &         (HUPG(LG)-ELET(NGTYP(LG),EP))
     &               /(ELET(NGTYP(LG),EP+1)-ELET(NGTYP(LG),EP))
             ENDIF
            ENDIF
C++++++++++++++++ END NORMAL FORMULA
! } GEOSR 2015.01.09 UNG !fixed interpolation
            DUMPG(LG)=DUMPG(LG)*FLOAT(NGATE(M1,NCTL))  ! CONSIDER GATE NUMBER
            DUMPG(LG)=DUMPG(LG)/FLOAT(NGATEC(NCTL)) ! CONSIDER CELL NO.
            DUMPG(LG)=DUMPG(LG)*GRAMPUP(LG)           ! ADJUST GATE FLUX
            DUMPGPREV(LG)=DUMPG(LG)                 ! SAVE GATE FLUX FOR CONTROL AFTER CLOSE

            DO K=1,KC  
              QCTLT(K,NCTL)=DUMPG(LG)*GKMULT(K) !*DZC(K)       ! SAVE GATE FLUX
            ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK START (EBB)
            CALL EBBMASK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MASK END

          ELSE  ! IF (HUPG(LG).GT.HDWG(LG)
            NGCHECK(LG)=0
            CALL EBBMASKRE
          ENDIF ! IF (HUPG(LG).GT.HDWG(LG)
                         ENDIF !IF (IAGT.EQ.0) THEN

        ENDIF      ! IF(NQCTYP(NCTL).EQ.13 ...
C###########################################################
C#      UPSTREAM -> DOWNSTREAM : USE FLUX TABLE (END) NQCTYP(NCTL).EQ.13 (2014.9 ung)
C###########################################################

!###########################################################
!# Movable weir convert to Fixed weir Geosr jgcho 2014.9.18 START
!###########################################################
        IF( (NQCTYP(NCTL).GE.31 .or. NQCTYP(NCTL).EQ.7)
     &      .AND. IAGT.EQ.0) THEN
          GRAMPUP(LG)=1.                ! ADJUST GATE RAMPUP VALUE

C++++++++++++++ BEGIN NORMAL FORMULA         ! Below SILLHH(LG) edited by jgcho 2014.11.08
          IF (HUPG(LG).GT.HDWG(LG)
     &      .AND. HUPG(LG).GT. SILLHH(LG) )THEN ! 2014.11.08 jgcho   .AND. HUPG(LG).GT.SEL1T )THEN   ! UPSTREAM -> DOWNSTREAM           
            IF (HDWG(LG) .LT. SILLHH(LG) )THEN ! 2014.11.08 jgcho SEL1T ) THEN   ! (1) IF (DEPDWG(LG) .LT. SILL(LG))
              IGCHECK(LG)=73
              CG10=((HUPG(LG)-SILLHH(LG))/(CGH2(LG)-CGH1(LG))		! 2014.11.03 ung CG1(LG) -> CG1(LG),CG2(LG),CGH1(LG),CGH2(LG)
     &             *(CG2(LG)-CG1(LG))+CG1(LG))							! 2014.11.03 ung CG1(LG) -> CG1(LG),CG2(LG),CGH1(LG),CGH2(LG)
              IF((HUPG(LG)-SILLHH(LG)).LT.CGH1(LG)) THEN       
                CG10=CG1(LG)
              ELSEIF((HUPG(LG)-SILLHH(LG)).GT.CGH2(LG)) THEN
                CG10=CG2(LG)
              ENDIF              
              DUMPG(LG)=CG10*GATEWI(LG)
     &               *(HUPG(LG)-SILLHH(LG))
     &               *SQRT(2.*9.806*(HUPG(LG)-SILLHH(LG)))
            ELSE                                 ! (2) DEPDWG(LG) .GE. SILL(LG)
              IGCHECK(LG)=74
              CG10=((HUPG(LG)-SILLHH(LG))/(CGH2(LG)-CGH1(LG))		! 2014.11.03 ung CG1(LG) -> CG1(LG),CG2(LG),CGH1(LG),CGH2(LG)
     &             *(CG2(LG)-CG1(LG))+CG1(LG))							! 2014.11.03 ung CG1(LG) -> CG1(LG),CG2(LG),CGH1(LG),CGH2(LG)
              IF((HUPG(LG)-SILLHH(LG)).LT.CGH1(LG)) THEN
                CG10=CG1(LG)
              ELSEIF((HUPG(LG)-SILLHH(LG)).GT.CGH2(LG)) THEN
                CG10=CG2(LG)
              ENDIF
              DUMPG(LG)=2.6*CG10*GATEWI(LG)*(HDWG(LG)-SILLHH(LG))
     &               *SQRT(2.*9.806
     &               *((HUPG(LG)-SILLHH(LG))-(HDWG(LG)-SILLHH(LG))))
            ENDIF                                ! (1) IF (DEPDWG(LG) .LT. SILL(LG))
C++++++++++++++++ END NORMAL FORMULA

            DUMPG(LG)=DUMPG(LG)*FLOAT(NGATEM(NCTL))  ! CONSIDER GATE NUMBER  ! 2014.10.14 NGATEM
            DUMPG(LG)=DUMPG(LG)/FLOAT(NGATEC(NCTL)) ! CONSIDER CELL NO.
            DUMPG(LG)=DUMPG(LG)*GRAMPUP(LG)           ! ADJUST GATE FLUX
            DUMPGPREV(LG)=0. !DUMPG(LG)                 ! SAVE GATE FLUX FOR CONTROL AFTER CLOSE
            ! GEOSR GATE. Suggestion, also here
            ! QCTLT(1:KC-1,NCTL) = 0.
            ! QCTLT(KC,NCTL) = DUMPG(LG)
            DO K=1,KC  
               if (K.eq.KC) then
                 GKFIX=1.
               else
                 GKFIX=0.
              endif
              QCTLT(K,NCTL)=DUMPG(LG)*GKFIX ! GKMULT(K) !*DZC(K)       ! SAVE GATE FLUX
            ENDDO
          ENDIF !IF (HUPG(LG).GT.HDWG(LG)
        ENDIF      ! IF(NQCTYP(NCTL).EQ.3 ...
!###########################################################
!# Movable weir convert to Fixed weir Geosr jgcho 2014.9.18 END
!###########################################################


!     BEGIN --- GRADUALLY DECREASE GATE FLUX AFTER GATE CLOSE
        IF (NGCHECK(LG).EQ.0 .AND. DUMPGPREV(LG).GT.0.1) THEN
          IF (NGCCHECK(LG).EQ.0) THEN                   ! GATE CLOSE CHECK

            ! open time control : jgcho 2010.8.17 temporary
!            GATEOTM=N*DT/86400.+1.0 ! NOW + 1 DAY

            NGCOUNT(LG)=1                               ! TIME STEP COUNT AFTER GATE CLOSE
            NGCCHECK(LG)=1                              ! GATE CLOSE CHECK
            DUMPGVAL(LG)=DUMPGPREV(LG)                  ! SAVE GATE FLUX
          ELSE
            NGCOUNT(LG)=NGCOUNT(LG)+1                   ! ADD TIME STEP 
          ENDIF
          IF (GCTIME(LG).GT.0.) THEN
            GRAMPUP(LG)=FLOAT(NGCOUNT(LG)) / 
     &                  (NGATEC(NCTL)*GCTIME(LG)*60./DT)  ! GATE CLOSE TIME INTERVAL RAMPUP TERM
          ELSE
            GRAMPUP(LG)=1.
          ENDIF
          GRAMPUP(LG) = MIN(GRAMPUP(LG), 1.0)  ! ADJUST GATE RAMPUP VALUE

          DUMPG(LG)=DUMPGVAL(LG) - (DUMPGVAL(LG)*GRAMPUP(LG))  ! GATE FLUX AFTER CLOSE
          DUMPGPREV(LG)=DUMPG(LG)                              ! SAVE NOW GATE FLUX FOR NEXT TIME
          IF (DUMPG(LG).LE.0.1) THEN
              NGCCHECK(LG)=0                  ! GATE CLOSE COMPLETELY
              DUMPG(LG)=0.
              DUMPGPREV(LG)=DUMPG(LG)
          ENDIF

          DO K=1,KC
              QCTLT(K,NCTL)=DUMPG(LG)*GKMULT(K) !*DZC(K)       ! SAVE GATE FLUX
          ENDDO
        ENDIF
!     END   --- GRADUALLY DECREASE GATE FLUX AFTER GATE CLOSE

      ENDDO                               ! DO NCTL=1,NQCTL
C

          DO NCTL=1,NQCTL                 ! GATE CELL
            IU=IQCTLU(NCTL)               ! I CELL INDEX UPSTREAM
            JU=JQCTLU(NCTL)               ! J CELL INDEX UPSTREAM
            LU=LIJ(IU,JU)                 ! L CELL INDEX UPSTREAM
            LUA(NCTL)=LU
            ID=IQCTLD(NCTL)               ! I CELL INDEX DOWNSTREAM
            JD=JQCTLD(NCTL)               ! J CELL INDEX DOWNSTREAM
! { GEOSR ESTURAY DIKE : JGCHO 2010.11.15
            IF((ID.EQ.0.AND.JD.EQ.0) .OR. NQCTYP(NCTL).EQ.6)THEN  
              LD=LC  
            ELSE  
              LD=LIJ(ID,JD)               ! L CELL INDEX DOWNSTREAM
            ENDIF  
            LDA(NCTL)=LD
! } GEOSR ESTURAY DIKE : JGCHO 2010.11.15

            DO K=1,KC
              IF (NCTL.EQ.1) THEN
                QSUM(LU,K)=-1.*QCTLT(K,NCTL)
                QSUM(LD,K)=QCTLT(K,NCTL)
              ELSEIF(NCTL.GE.2) THEN
                IF (LU.EQ.LUA(NCTL-1)) THEN
                  QSUM(LU,K)=-1.*QCTLT(K,NCTL)+QSUM(LU,K)
                ELSE
                  QSUM(LU,K)=-1.*QCTLT(K,NCTL)
                ENDIF
                IF (LD.EQ.LDA(NCTL-1)) THEN
                  QSUM(LD,K)=QCTLT(K,NCTL)+QSUM(LD,K)
                ELSE
                  QSUM(LD,K)=QCTLT(K,NCTL)
                ENDIF
              ENDIF
            ENDDO
          	CQ(LU)=CQ(LU)+ABS(SUM(QSUM(LU,:)))           ! GEOSR UNG 2014.11.12 Warning message writing
            CV(LU)=DXP(LU)*DYP(LU)*HMP(LU)                 ! GEOSR UNG 2014.11.12 Warning message writing
!            WRITE(*,*)DXP(LU),DYP(LU),HMP(LU)
!            WRITE(*,*)LU,NCTL
!            PAUSE
!      IF (NCTL.EQ.4) WRITE(*,*) N,(QSUM(LU,K),K=1,KC)
      ENDDO                           ! DO NCTL=1,NQCTL

      DO NCTL=1,NQCTL                                      ! GEOSR UNG 2014.11.12 Warning message writing
!       WRITE(*,*)CQ(LIJ(IQCTLU(NCTL),JQCTLU(NCTL)))
!       WRITE(*,*)CV(LIJ(IQCTLU(NCTL),JQCTLU(NCTL)))
!       PAUSE
        IF((CQ(LIJ(IQCTLU(NCTL),JQCTLU(NCTL))).GE.
     &    CV(LIJ(IQCTLU(NCTL),JQCTLU(NCTL))))
     &      .AND. (CQ(LIJ(IQCTLU(NCTL),JQCTLU(NCTL))).LT.0.0)) THEN	   ! GEOSR UNG 2014.11.12 Warning message writing
          WRITE(713,*) "QSUM is greater than UPSTREAM CELL WATER VOLUME" ! GEOSR UNG 2014.11.12 Warning message writing
          WRITE(713,*) N,TIMEDAY,NCTL,IQCTLU(NCTL),JQCTLU(NCTL),
     &      "QSUM=",CQ(LIJ(IQCTLU(NCTL),JQCTLU(NCTL)))                   ! GEOSR UNG 2014.11.12 Warning message writing
     &       ,",       CellVOL.="
     &       ,CV(LIJ(IQCTLU(NCTL),JQCTLU(NCTL)))
        ENDIF                                                            ! GEOSR UNG 2014.11.12 Warning message writing
      ENDDO                                                              ! GEOSR UNG 2014.11.12 Warning message writing
!     START: WRITE SINK#.OUT
      IF (ISINK.EQ.2) THEN
        IF (MOD(FLOAT(N),SNKW).EQ.0. .OR. DTSNK.EQ.-1.) THEN
C
          FSINK='SINK.OUT'
          OPEN(711,FILE=TRIM(FSINK),POSITION='APPEND')  
          WRITE(711,7110) N,TIMEDAY,(IGCHECK(NS),HUPG(NS),HDWG(NS)
     &                   ,DELHG(NS),(QCTLT(K,NS),K=1,KC),GGQSUM(NS)
     &                   ,NS=1,NQCTL) !,(GGQSUM(NS),NS=1,NQCTL)
 7110 FORMAT(I8,F10.4,<NQCTL>(I4,3F8.2,<KC>F8.2,F20.1))
          CLOSE(711)
C
          OPEN(712,FILE='SINKT.OUT',POSITION='APPEND')  ! OPEN NEW FILE
          DO NS=1,NQCTL
            DO K=1,KC
              GQT(NS)=GQT(NS)+QCTLT(K,NS)
            ENDDO
          ENDDO
          WRITE(712,7120) N,TIMEDAY,(GQT(NS),NS=1,NQCTL)
 7120 FORMAT(I8,F10.4,<NQCTL>F8.2)

! GEOSR GATE: write sink2.out
          OPEN(713,FILE='SINK2.OUT',POSITION='APPEND')  
          WRITE(713,7130) N,TIMEDAY,((QCTLT(K,NS),K=1,KC-1)
     &                   ,QCTLT(KC,NS)-DUMPG2(NS),DUMPG2(NS)
     &                   ,NS=1,NQCTL)
 7130 FORMAT(I8,F10.5,<NQCTL>(<KC+1>F9.2))
          CLOSE(713)

        ENDIF  ! IF (MOD(FLOAT(N),SNKW).EQ.0.) THEN
      ENDIF    ! IF (ISINK.EQ.2) THEN
!     END: WRITE SINK.OUT

      RETURN
      END


      SUBROUTINE EBBMASK
      USE GLOBAL  

                IF (NGTMSKE.GE.1 .AND. MSKEFLG.EQ.0) THEN
                  DO LMSK=1,NGTMSKE
                    IF(MGTMSKTYPE(LMSK).EQ.5)THEN  
                      IF (MSKEFLG.EQ.0) THEN
                        TMPSUB(LGTMSKE(LMSK))=SUB(LGTMSKE(LMSK))
                        TMPSUBO(LGTMSKE(LMSK))=SUBO(LGTMSKE(LMSK))
                      ENDIF
                      SUB(LGTMSKE(LMSK))=0.  
                      SUBO(LGTMSKE(LMSK))=0.  
                    ENDIF  
                    IF(MGTMSKTYPE(LMSK).EQ.6)THEN  
                      IF (MSKEFLG.EQ.0) THEN
                        TMPSVB(LGTMSKE(LMSK))=SVB(LGTMSKE(LMSK))
                        TMPSVBO(LGTMSKE(LMSK))=SVBO(LGTMSKE(LMSK))
                      ENDIF
                      SVB(LGTMSKE(LMSK))=0.  
                      SVBO(LGTMSKE(LMSK))=0.  
                    ENDIF  
                  ENDDO
                  MSKEFLG=1
                ENDIF !IF (NGTMSKE.GE.1 .AND. MSKEFLG.EQ.0) THEN

      RETURN
      END


      SUBROUTINE FLDMASK
      USE GLOBAL  

        IF (NGTMSKF.GE.1 .AND. MSKFFLG.EQ.0) THEN
            DO LMSK=1,NGTMSKF
            IF(MGTMSKTYPF(LMSK).EQ.7)THEN  
                IF (MSKFFLG.EQ.0) THEN
                TMPSUB(LGTMSKF(LMSK))=SUB(LGTMSKF(LMSK))
                TMPSUBO(LGTMSKF(LMSK))=SUBO(LGTMSKF(LMSK))
                ENDIF
                SUB(LGTMSKF(LMSK))=0.  
                SUBO(LGTMSKF(LMSK))=0.  
            ENDIF  
            IF(MGTMSKTYPF(LMSK).EQ.8)THEN  
                IF (MSKFFLG.EQ.0) THEN
                TMPSVB(LGTMSKF(LMSK))=SVB(LGTMSKF(LMSK))
                TMPSVBO(LGTMSKF(LMSK))=SVBO(LGTMSKF(LMSK))
                ENDIF
                SVB(LGTMSKF(LMSK))=0.  
                SVBO(LGTMSKF(LMSK))=0.  
            ENDIF  
            ENDDO
            MSKFFLG=1
        ENDIF !IF (NGTMSKF.GE.1 .AND. MSKFFLG.EQ.0)

      RETURN
      END

      SUBROUTINE EBBMASKRE
      USE GLOBAL  

        IF (NGTMSKE.GE.1 .AND. MSKEFLG.EQ.1) THEN
            DO LMSK=1,NGTMSKE
            IF(MGTMSKTYPE(LMSK).EQ.5)THEN  
                SUB(LGTMSKE(LMSK))=TMPSUB(LGTMSKE(LMSK))
                SUBO(LGTMSKE(LMSK))=TMPSUBO(LGTMSKE(LMSK))
            ENDIF  
            IF(MGTMSKTYPE(LMSK).EQ.6)THEN  
                SVB(LGTMSKE(LMSK))=TMPSVB(LGTMSKE(LMSK))
                SVBO(LGTMSKE(LMSK))=TMPSVBO(LGTMSKE(LMSK))
            ENDIF
            ENDDO
            MSKEFLG=0
        ENDIF ! IF (MSKFFLG.EQ.1) THEN

      RETURN
      END

      SUBROUTINE FLDMASKRE
      USE GLOBAL  

      IF (NGTMSKF.GE.1 .AND. MSKFFLG.EQ.1) THEN
        DO LMSK=1,NGTMSKF
          IF(MGTMSKTYPF(LMSK).EQ.7)THEN  
            SUB(LGTMSKF(LMSK))=TMPSUB(LGTMSKF(LMSK))
            SUBO(LGTMSKF(LMSK))=TMPSUBO(LGTMSKF(LMSK))
          ENDIF  
          IF(MGTMSKTYPF(LMSK).EQ.8)THEN  
            SVB(LGTMSKF(LMSK))=TMPSVB(LGTMSKF(LMSK))
            SVBO(LGTMSKF(LMSK))=TMPSVBO(LGTMSKF(LMSK))
          ENDIF
        ENDDO
        MSKFFLG=0
      ENDIF ! IF (MSKFFLG.EQ.1) THEN

      RETURN
      END
