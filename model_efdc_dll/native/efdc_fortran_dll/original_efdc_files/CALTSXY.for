      SUBROUTINE CALTSXY  
C  
C CHANGE RECORD  
C ** SUBROUTINE CALTSXY UPDATES TIME VARIABLE SURFACE WIND STRESS  
C  
      USE GLOBAL  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::CLOUDTT  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::EVAPTT  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::PATMTT  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RAINTT  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::RHAT  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SOLSWRTT  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::SVPAT  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TATMTT  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TWETTT  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::VPAT  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WINDE  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WINDN  
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WINDSXX
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WINDSXY
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WINDSYX
C     REAL,SAVE,ALLOCATABLE,DIMENSION(:)::WINDSYY
C
      IF(.NOT.ALLOCATED(CLOUDTT))THEN
        ALLOCATE(CLOUDTT(NASERM))  
        ALLOCATE(EVAPTT(NASERM))  
        ALLOCATE(PATMTT(NASERM))  
        ALLOCATE(RAINTT(NASERM))  
        ALLOCATE(RHAT(NASERM))  
        ALLOCATE(SOLSWRTT(NASERM))  
        ALLOCATE(SVPAT(NASERM))  
        ALLOCATE(TATMTT(NASERM))  
        ALLOCATE(TWETTT(NASERM))  
        ALLOCATE(VPAT(NASERM))  
        ALLOCATE(WINDE(NWSERM))  
        ALLOCATE(WINDN(NWSERM))  
        ALLOCATE(WINDSXX(LCM))  
        ALLOCATE(WINDSXY(LCM))  
        ALLOCATE(WINDSYX(LCM))  
        ALLOCATE(WINDSYY(LCM))  

        CLOUDTT=0.0   
        EVAPTT=0.0   
        PATMTT=0.0   
        RAINTT=0.0   
        RHAT=0.0 
        SOLSWRTT=0.0   
        SVPAT=0.0  
        TATMTT=0.0   
        TWETTT=0.0   
        VPAT=0.0  
        WINDE=0.0   
        WINDN=0.0 
        WINDSXX=0.0
        WINDSXY=0.0
        WINDSYX=0.0
        WINDSYY=0.0

        ! *** ONE TIME SPATIAL DISTRIBUTION,\
C *** OOPS, REVC & RCHC NOT SAVED FOR EACH SERIES
C        DO L=2,LA  
C          CLEVAP(L)=0.  
C          CCNHTT(L)=0.  
C        ENDDO  
C        DO NA=1,NASER  
C          CLEVAPT=0.001*ABS(REVC)
C          CCNHTTT=0.001*ABS(RCHC)
C          DO L=2,LA  
C            CLEVAP(L)=CLEVAP(L)+ATMWHT(L,NA)*CLEVAPT  
C            CCNHTT(L)=CCNHTT(L)+ATMWHT(L,NA)*CCNHTTT  
C          ENDDO  
C        ENDDO  
        DO L=2,LA  
          CLEVAP(L)=0.001*ABS(REVC)
          CCNHTT(L)=0.001*ABS(RCHC)  
        ENDDO  
      ENDIF
C
C**********************************************************************C
C
C INITIALIZE WIND SHELTERED SURFACE GAS TRANSFER
C
      IF(N.EQ.-1.AND.NWSER.GT.0)THEN
        IF(DEBUG)THEN
          OPEN(1,FILE='WINDSHELT.OUT')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='WINDSHELT.OUT')  
        ENDIF
        DO L=2,LA  
          I=IL(L)  
          J=JL(L)  
          IF(WINDSTKA(L).GT.0.0)THEN  
            ! ** IF WINDSTKA > 0 BOTH X AND Y COMPONENTS ARE APPLIED  
            WINDSXX(L)=CVN(L)  
            WINDSXY(L)=-CVE(L)  
            WINDSYX(L)=-CUN(L)  
            WINDSYY(L)=CUE(L)  
          ELSE  
            ! ** IF WINDSTKA < 0 SLECTIVELY APPLY X AND Y COMPONENTS  
            ! ** FIRST CASE IS FULLY OPEN WATER  
            WINDSXX(L)=CVN(L)  
            WINDSXY(L)=-CVE(L)  
            WINDSYX(L)=-CUN(L)  
            WINDSYY(L)=CUE(L)  
            LS=LSC(L)  
            LN=LNC(L)  
            ! ** SECOND CASE IS 1D CHANNEL IN COMP X DIRECTION  
            IF(SVB(L).LT.0.5.AND.IJCT(I,J-1).NE.5)THEN  
              IF(SVB(LN).LT.0.5.AND.IJCT(I,J+1).NE.5)THEN  
                WINDSXX(L)=CVN(L)  
                WINDSXY(L)=-CVE(L)  
                WINDSYX(L)=-1000.  
                WINDSYY(L)=0.  
              ENDIF  
            ENDIF  
            ! ** THIRD CASE IS 1D CHANNEL IN COMP Y DIRECTION  
            IF(SUB(L).LT.0.5.AND.IJCT(I-1,J).NE.5)THEN  
              IF(SUB(L+1).LT.0.5.AND.IJCT(I+1,J).NE.5)THEN  
                WINDSXX(L)=0.  
                WINDSXY(L)=-1000.  
                WINDSYX(L)=-CUN(L)  
                WINDSYY(L)=CUE(L)  
              ENDIF  
            ENDIF  
          ENDIF
          IF(DEBUG)WRITE(1,1111)IL(L),JL(L),WINDSTKA(L),WINDSXX(L),
     &                          WINDSXY(L),WINDSYX(L),WINDSYY(L)  
        ENDDO  
        IF(DEBUG)CLOSE(1)  
      ENDIF  
 1111 FORMAT(2I5,10F10.6)  
C
C**********************************************************************C
C
      IF(NWSER.GT.0)THEN  
        ! *** UPDATE THE FORCING WIND DATA TO THE CURRENT TIME
        DO NA=1,NWSER  
          IF(ISDYNSTP.EQ.0)THEN  
            TIME=DT*FLOAT(N)/TCWSER(NA)+TBEGIN*(TCON/TCWSER(NA))  
          ELSE  
            TIME=TIMESEC/TCWSER(NA)  
          ENDIF  
          M1=MWTLAST(NA)  
          MSAVE=M1  
  200     CONTINUE  
          M2=M1+1  
          IF((TIME-EPS).GT.TWSER(M2,NA))THEN  
            M1=M2  
            GOTO 200  
          ELSE  
            MWTLAST(NA)=M1  
          ENDIF  
          TDIFF=TWSER(M2,NA)-TWSER(M1,NA)  
          WTM1=(TWSER(M2,NA)-TIME)/TDIFF  
          WTM2=(TIME-TWSER(M1,NA))/TDIFF  
          DEGM1=90.-WINDD(M1,NA)  
          DEGM2=90.-WINDD(M2,NA)  
          WINDS1=WTM1*WINDS(M1,NA)+WTM2*WINDS(M2,NA)  
          WINDS2=WTM1*WINDS(M1,NA)+WTM2*WINDS(M2,NA)  
          WINDE1=WINDS(M1,NA)*COS(DEGM1/57.29578)  
          WINDN1=WINDS(M1,NA)*SIN(DEGM1/57.29578)  
          WINDE2=WINDS(M2,NA)*COS(DEGM2/57.29578)  
          WINDN2=WINDS(M2,NA)*SIN(DEGM2/57.29578)  
          WINDE(NA)=WTM1*WINDE1+WTM2*WINDE2  
          WINDN(NA)=WTM1*WINDN1+WTM2*WINDN2  
        ENDDO  

        ! *** CALCULATE THE WIND STRESS
        IF(NWSER.GT.1)THEN
          DO L=2,LA  
            WNDVELE(L)=0.  
            WNDVELN(L)=0.  
          ENDDO  
          DO NA=1,NWSER  
            DO L=2,LA  
              WNDVELE(L)=WNDVELE(L)+WNDWHT(L,NA)*WINDE(NA)  
              WNDVELN(L)=WNDVELN(L)+WNDWHT(L,NA)*WINDN(NA)  
            ENDDO  
          ENDDO  
        ELSE    !IF(NWSER.EQ.1)THEN
          DO L=2,LA  
            WNDVELE(L)=WINDE(1)  
            WNDVELN(L)=WINDN(1)  
          ENDDO  
        ENDIF
C
        DO L=2,LA  
          ! ** CASE 0 MAGNITUDE SHELTERING AND NO DIRECTIONAL SHELTERING  
          IF(WINDSTKA(L).GT.0.0)THEN  
            WNDFAC=ABS(WINDSTKA(L))  
            WNDVELE(L)=WNDFAC*WNDVELE(L)  
            WNDVELN(L)=WNDFAC*WNDVELN(L)  
            WINDST(L)=SQRT( WNDVELE(L)*WNDVELE(L)  
     &          +WNDVELN(L)*WNDVELN(L) )  
!{GeoSR, 2014.07.04 YSSONG, WIND DRAG COEFF.
!            C2=1.2E-6*(0.8+0.065*WINDST(L))
            IF(ISCD.EQ.1)THEN
                C2=1.2E-6*(0.26+0.46*WINDST(L)/CDDN(L))             ! Dean(1997)
            ELSEIF(ISCD.EQ.2)THEN
                IF(WINDST(L).NE.0.0)THEN
                  IF(WINDST(L).GE.WNDCR)THEN 
                    CD10=(WNDCM*WINDST(L)+WNDB)**2/WINDST(L)**2     ! Foreman(2012)
                  ELSE
                    CD10=(WNDCM*WNDCR+WNDB)**2/WNDCR**2             ! Foreman(2012)
                  ENDIF
                  C2=1.2E-3*CD10                                     ! Foreman(2012)
                ELSE
                  C2=0.0
                ENDIF  
            ELSE
                C2=1.2E-6*(WNDCM+WNDB*WINDST(L))
            ENDIF    
!} GeoSR, 2014.07.04 YSSONG, WIND DRAG COEFF.
            TSEAST=C2*WINDST(L)*WNDVELE(L)  
            TSNORT=C2*WINDST(L)*WNDVELN(L)  
            TSX(L)=WINDSXX(L)*TSEAST+WINDSXY(L)*TSNORT  
            TSY(L)=WINDSYX(L)*TSEAST+WINDSYY(L)*TSNORT  
  
          ELSEIF(WINDSTKA(L).LT.0.0)THEN  
            ! ** CASE 1 MAGNITUDE SHELTERING AND DIRECTIONAL SHELTERING, OPEN WATER  
            IF(WINDSYX(L).GT.-99.0.AND.WINDSXY(L).GT.-99.0)THEN  
              WNDFAC=ABS(WINDSTKA(L))  
              WNDVELE(L)=WNDFAC*WNDVELE(L)  
              WNDVELN(L)=WNDFAC*WNDVELN(L)  
              WINDST(L)=SQRT( WNDVELE(L)*WNDVELE(L)  
     &            +WNDVELN(L)*WNDVELN(L) )
!{GeoSR, 2014.07.04 YSSONG, WIND DRAG COEFF.
!              C2=1.2E-6*(0.8+0.065*WINDST(L))
            IF(ISCD.EQ.1)THEN
                C2=1.2E-6*(0.26+0.46*WINDST(L)/CDDN(L))             ! Dean(1997)
            ELSEIF(ISCD.EQ.2)THEN
                IF(WINDST(L).NE.0.0)THEN
                  IF(WINDST(L).GE.WNDCR)THEN 
                    CD10=(WNDCM*WINDST(L)+WNDB)**2/WINDST(L)**2      ! Foreman(2012)
                  ELSE
                    CD10=(WNDCM*WNDCR+WNDB)**2/WNDCR**2              ! Foreman(2012)
                  ENDIF
                  C2=1.2E-3*CD10                                     !Foreman(2012)
                ELSE
                  C2=0.0
                ENDIF  
            ELSE
                C2=1.2E-6*(WNDCM+WNDB*WINDST(L))                
            ENDIF
!} GeoSR, 2014.07.04 YSSONG, WIND DRAG COEFF.            
              TSEAST=C2*WINDST(L)*WNDVELE(L)  
              TSNORT=C2*WINDST(L)*WNDVELN(L)  
              TSX(L)=WINDSXX(L)*TSEAST+WINDSXY(L)*TSNORT  
              TSY(L)=WINDSYX(L)*TSEAST+WINDSYY(L)*TSNORT  
            ENDIF  
        
            ! ** CASE 2 MAGNITUDE SHELTERING AND DIRECTIONAL SHELTERING, X CHANNEL  
            IF(WINDSYX(L).LT.-99.0)THEN  
              WINDXX=WINDSXX(L)*WNDVELE(L)+WINDSXY(L)*WNDVELN(L)  
              WNDFAC=ABS(WINDSTKA(L))  
              WINDXX=WNDFAC*WNDVELE(L)  
              WINDST(L)=ABS(WINDXX)  
!{GeoSR, YSSONG, ICE COVER, 1111031
              IF(PSHADE(L).NE.1.0) WINDST(L)=0.0
!}
!{GeoSR, 2014.07.04 YSSONG, WIND DRAG COEFF.              
!              TSX(L)=1.2E-6*(0.8+0.065*WINDST(L))*WINDST(L)*WINDXX  
            IF(ISCD.EQ.1)THEN
              TSX(L)=1.2E-6*(0.26+0.46*WINDST(L)/CDDN(L))             ! Dean(1997)
     &               *WINDST(L)*WINDXX 
            ELSEIF(ISCD.EQ.2)THEN
              IF(WINDST(L).NE.0.0)THEN
                IF(WINDST(L).GE.WNDCR)THEN 
                  CD10=(WNDCM*WINDST(L)+WNDB)**2/WINDST(L)**2            ! Foreman(2012)
                ELSE
                  CD10=(WNDCM*WNDCR+WNDB)**2/WNDCR**2                    ! Foreman(2012)                   
                ENDIF
                TSX(L)=1.2E-3*CD10*WINDST(L)*WINDXX                    ! Foreman(2012)                  
              ELSE
                TSX(L)=0.0
              ENDIF  
            ELSE
              TSX(L)=1.2E-6*(WNDCM+WNDB*WINDST(L))*WINDST(L)*WINDXX
            ENDIF               
!} GeoSR, 2014.07.04 YSSONG, WIND DRAG COEFF.            
              TSY(L)=0.  
            ENDIF  
  
            ! ** CASE 3 MAGNITUDE SHELTERING AND DIRECTIONAL SHELTERING, Y CHANNEL  
            IF(WINDSXY(L).LT.-99.0)THEN  
              WINDYY=WINDSYX(L)*WNDVELE(L)+WINDSYY(L)*WNDVELN(L)  
              WNDFAC=ABS(WINDSTKA(L))  
              WINDYY=WNDFAC*WINDYY  
              WINDST(L)=ABS(WINDYY)  
              TSX(L)=0 
!{GeoSR, 2014.07.04 YSSONG, WIND DRAG COEFF.              
!              TSY(L)=1.2E-6*(0.8+0.065*WINDST(L))*WINDST(L)*WINDYY  
            IF(ISCD.EQ.1)THEN
              TSY(L)=1.2E-6*(0.26+0.46*WINDST(L)/CDDN(L))             ! Dean(1997)
     &               *WINDST(L)*WINDYY 
            ELSEIF(ISCD.EQ.2)THEN
              IF(WINDST(L).NE.0.0)THEN
                IF(WINDST(L).GE.WNDCR)THEN 
                  CD10=(WNDCM*WINDST(L)+WNDB)**2/WINDST(L)**2            ! Foreman(2012)
                ELSE 
                  CD10=(WNDCM*WNDCR+WNDB)**2/WNDCR**2                    ! Foreman(2012)
                ENDIF                  
                TSY(L)=1.2E-3*CD10*WINDST(L)*WINDYY                     ! Foreman(2012)                 
              ELSE
              TSX(L)=0.0  
              ENDIF  
            ELSE
              TSY(L)=1.2E-6*(WNDCM+WNDB*WINDST(L))*WINDST(L)*WINDYY
            ENDIF               
!} GeoSR, 2014.07.04 YSSONG, WIND DRAG COEFF.
            ENDIF  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C          CFTSX=1.  
C          CFTSY=1.  
C          HHUU=2.*HUWET(L)  
C          HHVV=2.*HVWET(L)  
C  
      IF(NASER.GT.0)THEN  
        DO NA=1,NASER  
          IF(ISDYNSTP.EQ.0)THEN  
            TIME=DT*FLOAT(N)/TCASER(NA)+TBEGIN*(TCON/TCASER(NA))  
          ELSE  
            TIME=TIMESEC/TCASER(NA)  
          ENDIF  
          M1=MATLAST(NA)  
  100     CONTINUE  
          M2=M1+1  
          IF((TIME-EPS).GT.TASER(M2,NA))THEN  
            M1=M2  
            GOTO 100  
          ELSE  
            MATLAST(NA)=M1  
          ENDIF  
          TDIFF=TASER(M2,NA)-TASER(M1,NA)  
          WTM1=(TASER(M2,NA)-TIME)/TDIFF  
          WTM2=(TIME-TASER(M1,NA))/TDIFF  
          PATMTT(NA)=WTM1*PATM(M1,NA)+WTM2*PATM(M2,NA)  
          TATMTT(NA)=WTM1*TDRY(M1,NA)+WTM2*TDRY(M2,NA)  
          TWETTT(NA)=WTM1*TWET(M1,NA)+WTM2*TWET(M2,NA)  
          RAINTT(NA)=WTM1*RAIN(M1,NA)+WTM2*RAIN(M2,NA)  
          EVAPTT(NA)=WTM1*EVAP(M1,NA)+WTM2*EVAP(M2,NA)  
          SOLSWRTT(NA)=WTM1*SOLSWR(M1,NA)+WTM2*SOLSWR(M2,NA)  
          CLOUDTT(NA)=WTM1*CLOUD(M1,NA)+WTM2*CLOUD(M2,NA)  
          SVPAT(NA)=  
     &       10.**((0.7859+0.03477*TATMTT(NA))/(1.+0.00412*TATMTT(NA)))  
          IF(IRELH(NA).EQ.0.AND.ISTOPT(2).NE.4)THEN  
C            RHAT(NA)=1.  
C     &          -0.00066*(PATMTT(NA)/SVPAT(NA))*(TATMTT(NA)-TWETTT(NA))
            ! *** DSLLC Begin
            ! *** (Correct RHA Computation from wet bulb)
            TMPVAL=0.00066*(1.0+0.00115*TWETTT(NA))
            SVPWET=  
     &        10.**((0.7859+0.03477*TWETTT(NA))/(1.+0.00412*TWETTT(NA)))  
            TMPVL1=SVPWET-TMPVAL*PATMTT(NA)*(TATMTT(NA)-TWETTT(NA))
            RHAT(NA)=MAX(TMPVL1/ SVPAT(NA),.01)
            ! *** DSLLC End
          ELSE  
            RHAT(NA)=TWETTT(NA)  
          ENDIF  
          VPAT(NA)=RHAT(NA)*SVPAT(NA)  
        ENDDO  

C      ENDIF 
C      IF(NASER.GT.0)THEN

        IF(NASER.GT.1)THEN  
          DO L=2,LA  
            PATMT(L)=0.  
            TATMT(L)=0.  
            RAINT(L)=0.  
            EVAPT(L)=0.  
            SOLSWRT(L)=0.  
            CLOUDT(L)=0.  
            SVPA(L)=0.  
            RHA(L)=0.  
            VPA(L)=0.  
          ENDDO  
          DO NA=1,NASER  
            DO L=2,LA  
              PATMT(L)=PATMT(L)+ATMWHT(L,NA)*PATMTT(NA)  
              TATMT(L)=TATMT(L)+ATMWHT(L,NA)*TATMTT(NA)  
              RAINT(L)=RAINT(L)+ATMWHT(L,NA)*RAINTT(NA)  
              EVAPT(L)=EVAPT(L)+ATMWHT(L,NA)*EVAPTT(NA)  
              SOLSWRT(L)=SOLSWRT(L)+ATMWHT(L,NA)*SOLSWRTT(NA)  
              CLOUDT(L)=CLOUDT(L)+ATMWHT(L,NA)*CLOUDTT(NA)  
              SVPA(L)=SVPA(L)+ATMWHT(L,NA)*SVPAT(NA)  
              RHA(L)=RHA(L)+ATMWHT(L,NA)*RHAT(NA)  
              VPA(L)=VPA(L)+ATMWHT(L,NA)*VPAT(NA)  
            ENDDO  
          ENDDO  
        ELSE
          DO L=2,LA  
            PATMT(L)=PATMTT(1)  
            TATMT(L)=TATMTT(1)  
            RAINT(L)=RAINTT(1)  
            EVAPT(L)=EVAPTT(1)  
            SOLSWRT(L)=SOLSWRTT(1)  
            CLOUDT(L)=CLOUDTT(1)  
            SVPA(L)=SVPAT(1)  
            RHA(L)=RHAT(1)  
            VPA(L)=VPAT(1)  
          ENDDO  
        ENDIF  

        ! *** PMC - MOVED ALL TIME INVARIANT PARAMETERS TO KEEP FROM COMPUTING EVERY TIME
        IF(REVC.LT.0.)THEN  
          CLEVAPTMP=0.001*ABS(REVC)
          DO L=2,LA  
            CLEVAP(L)=1.E-3*(0.8+0.065*WINDST(L))  
            CLEVAP(L)=MAX(CLEVAP(L),CLEVAPTMP)  
          ENDDO  
        ENDIF

        IF(RCHC.LT.0.)THEN  
          CCNHTTTMP=0.001*ABS(RCHC)
          DO L=2,LA  
            CCNHTT(L)=1.E-3*(0.8+0.065*WINDST(L))  
            CCNHTT(L)=MAX(CCNHTT(L),CCNHTTTMP)  
          ENDDO  
        ENDIF
      ENDIF  
C
      RETURN  
      END  

