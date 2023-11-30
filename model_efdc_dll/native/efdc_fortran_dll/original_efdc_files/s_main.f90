!**********************************************************************!
SUBROUTINE SEDZLJ_MAIN
  USE GLOBAL
  IMPLICIT NONE
  DOUBLE PRECISION,DIMENSION(LCM)::WVEL,CLEFT,CRIGHT,GRADSED,SEDAVG,CRNUM,CRAIG
  INTEGER::L,K,NS
  DOUBLE PRECISION::AA11,AA12,AA21,AA22,BB11,BB22,DETI
  ! PT: real values are written in DOUBLE PRECISION. 7/16/08
!**********************************************************************!
!
! **  SUBROUTINE CALSED CALCULATES COHESIVE SEDIMENT SETTLING,
! **  DEPOSITION AND RESUSPENSION According to SEDZLJ MODEL
!
!  REVISION DATE :  May 24, 2006
!  Craig Jones and Scott James

!**********************************************************************!
!

        DO NS=1,NSED
          DO K=1,KC
            DO L=2,LA
              IF(SED(L,K,NS).LT.-1.0)THEN
                WRITE(6,*)'SSEDTOX_MAIN',NS,IL(L),JL(L),SED(L,K,NS)
              SED(L,K,NS)=0.0
              ENDIF
            ENDDO
            ENDDO
        ENDDO  

!Calculates the shear stresses from the velocity output of the hydraulic model.
  CALL SEDZLJ_SHEAR
!
!Calculating the morphology before sediment transport of each time step.
  IF(IMORPH_SEDZLJ==1)FORALL(L=2:LA)HBED(L,1:KB)=0.01*(TSED(1:KB,L)/BULKDENS(1:KB,L))
!TSED-sediment layer's thickness. HBED-Bed height. BULKDENS-Density of sediment and water in layer.
!
!Setting the sediment concentration in the current.  SEDS is a saved version of SED (the sediment
!concentration).  SEDS and SED could be used to calculate the difference between the two time
!steps.  If SED = SEDS + SEDF (the flux) then mass is conserved.
  SEDS(2:LA,1:KC,1:NSCM)=SED(2:LA,1:KC,1:NSCM)

!calling bed load calculation.  BEDLOADJ subroutine provides CBL, the sediment concentration 
!load.
  IF(NCALC_BL==1.AND.N>=ISEDTIME) CALL BEDLOADJ
  !Calls the sedflume transport. NOTE: ISEDTIME is the time difference between when sediment
  !transport begins versus the beginning of hydraulic transport.
  IF(NSEDFLUME==1.AND.N>=ISEDTIME) THEN
!**********************************************************************!
!   SEDZLJ Sediment Transport only
!
!WSETA - temporary settling velocity.  The division of DWS by 100.0 probably has to do with unit conversion.
!going from cm/s to m/s.
     FORALL(K=0:KS,L=2:LA)WSETA(L,K,1:NSCM)=DWS(1:NSCM)/100.0
     SEDF(2:LA,0:KS,1:NSCM)=0.0
     !if(minval(SEDF) < 0.0) then
        !print *, 'Negative SEDF 0'
        !pause
     !endif
     SSGI(1:NSCM)=1.0/(1.0E6*SSG(1:NSCM)) 
!----------------------------------------------------------------------!
!
! **  HORIZONTAL LOOPS
!These loops account for sediment diffusion between the water layers.
!PT: added if loops to make code run faster when KC = 1 and KC = 2
     KCNOT1LOOP: IF(KC .NE. 1) THEN
        DO NS=1,NSCM
           FORALL(L=2:LA) !K=KC
              WVEL(L)=DELT*HPI(L)*DZIC(KC)
              CLEFT(L)=1.0+WSETA(L,KC-1,NS)*WVEL(L)
              CRIGHT(L)=MAX(SED(L,KC,NS),0.0)
              SED(L,KC,NS)=CRIGHT(L)/CLEFT(L)
              SEDF(L,KC-1,NS)=-WSETA(L,KC-1,NS)*SED(L,KC,NS)
           ENDFORALL
!PT: added if loop to allow code to run faster for KC = 2 case.
           KCNOT2LOOP: IF(KC .NE. 2) THEN
              DO K=KC-1,2,-1
                 FORALL(L=2:LA)
                    WVEL(L)=DELT*HPI(L)*DZIC(K)
                    CLEFT(L)=1.0+WSETA(L,K-1,NS)*WVEL(L)
                    CRIGHT(L)=MAX(SED(L,K,NS),0.0)-SEDF(L,K,NS)*WVEL(L)
                    SED(L,K,NS)=CRIGHT(L)/CLEFT(L)
                    SEDF(L,K-1,NS)=-WSETA(L,K-1,NS)*SED(L,K,NS)
                 ENDFORALL
              ENDDO
           ENDIF KCNOT2LOOP
        ENDDO
     ENDIF KCNOT1LOOP
   
     !if(minval(SEDF) < 0.0) then
        !print *, 'Negative SEDF 1'
        !pause
     !endif

!
!   Call Bedload subroutines here
!
     DO L=2,LA
!  **   Calculate total flux from bed and determine if 
!       there is enough sediment to erode from SEDZLJ
        IF(ISCDRY(L).EQ.0)THEN !Only Call for wet cells (CRAIG JONES)
          CALL SEDZLJ(L)
        ENDIF
!  **   Update the bed thickness based on the flux and calculate
!       the flux into the water column.
!
        QSBDTOP(L)=SUM(SSGI(1:NSCM)*SEDF(L,0,1:NSCM))
        DO NS=1,NSCM
           QWBDTOP(L)=QWBDTOP(L)+SSGI(NS)*(VDRBED(L,KBT(L))*MAX(SEDF(L,0,NS),0.0)+VDRDEPO(NS)*MIN(SEDF(L,0,NS),0.0))
        ENDDO

  ! delme
  if( l==341)then
    write(77,7777) L,N,timeday,tsed(1:kb,l)
7777 format(I5,I5,f12.6,5f10.3)
  endif

     ENDDO
!
!---------- ------------------------------------------------------------!
!
! **  ANTI-DIFFUSION OF COHESIVE SEDIMENT  KC.GT.3 

     DO NS=1,NSCM
        IF(KC .GT. 3) THEN
           IF(ISTOPT(6)==1) THEN
              DO K=1,KC-1
                 FORALL(L=2:LA)
                    CRNUM(L)=1.0+DELT*WSETA(L,K,NS)*HPI(L)*DZIC(K+1)
                    GRADSED(L)=(SED(L,K+1,NS)-SED(L,K,NS))/(DZC(K+1)+DZC(K))
                    SEDAVG(L)=0.5*(SED(L,K+1,NS)+SED(L,K,NS)+1.0E-16)
                    WSETA(L,K,NS)=-CRNUM(L)*DZC(K+1)*WSETA(L,K,NS)*GRADSED(L)/SEDAVG(L)
                 ENDFORALL
              ENDDO
!
!     TVAR1S=LOWER DIAGONAL
              TVAR1S(2:LA,1)=0.0
              DO K=2,KC
                 TVAR1S(2:LA,K)=MIN(WSETA(2:LA,K-1,NS),0.0)
              ENDDO
!
!     TVAR1N=UPPER DIAGONAL
              TVAR1N(2:LA,KC)=0.0
              DO K=1,KC-1
                 TVAR1N(2:LA,K)=-MAX(WSETA(2:LA,K,NS),0.0)
              ENDDO
!     TVAR1W=MAIN DIAGONAL
              TVAR1W(2:LA,1)=DELTI*DZC(1)*HP(2:LA)-MIN(WSETA(2:LA,1,NS),0.0)
              DO K=2,KC-1
                 TVAR1W(2:LA,K)=DELTI*DZC(KC)*HP(2:LA)+MAX(WSETA(2:LA,K-1,NS),0.0)-MIN(WSETA(2:LA,K,NS),0.0)
              ENDDO
              TVAR1W(2:LA,KC)=DELTI*DZC(KC)*HP(2:LA)+MAX(WSETA(2:LA,KC-1,NS),0.0)
!     TVAR1E=RIGHT HAND SIDE
              DO K=1,KC
                 TVAR1E(2:LA,K)=DELTI*DZC(KC)*HP(2:LA)*SED(2:LA,K,NS)
              ENDDO
!
!     TVAR3S=BET,TVAR2N=U,TVAR2S=GAM ARE WORKING ARRAYS
              TVAR3S(2:LA)=TVAR1W(2:LA,1)
              TVAR2N(2:LA,1)=TVAR1E(2:LA,1)/TVAR3S(2:LA)
              DO K=2,KC
                 TVAR2S(2:LA,K)=TVAR1N(2:LA,K-1)/TVAR3S(2:LA)
                 TVAR3S(2:LA)=TVAR1W(2:LA,K)-TVAR1S(2:LA,K)*TVAR2S(2:LA,K)
                 TVAR2N(2:LA,K)=(TVAR1E(2:LA,K)-TVAR1S(2:LA,K)*TVAR2N(2:LA,K-1))/TVAR3S(2:LA)
              ENDDO
              DO K=KC-1,1,-1
                 TVAR2N(2:LA,K)=TVAR2N(2:LA,K)-TVAR2S(2:LA,K+1)*TVAR2N(2:LA,K+1)
              ENDDO
              SED(2:LA,K,NS)=TVAR2N(2:LA,K)
           ENDIF
!
!----------------------------------------------------------------------!
!
! **  FINAL FLUX KC.GT.3 for SEDZLJ. Note: KS=KC-1
!
           SEDF(2:LA,KS,NS)=DELTI*DZC(KC)*HP(2:LA)*(SED(2:LA,KC,NS)-SEDS(2:LA,KC,NS)) 
           !if(minval(SEDF) < 0.0) then
              !print *, 'Negative SEDF 2'
              !pause
           !endif
           DO K=KS-1,1,-1
              SEDF(2:LA,K,NS)=DELTI*DZC(K+1)*HP(2:LA)*(SED(2:LA,K+1,NS)-SEDS(2:LA,K+1,NS))+SEDF(2:LA,K+1,NS)
           ENDDO
           !if(minval(SEDF) < 0.0) then
              !print *, 'Negative SEDF 3'
              !pause
           !endif
        ENDIF
!                *************************************                 !
        IF(KC .EQ. 3) THEN
! **  ANTI-DIFFUSION OF COHESIVE SEDIMENT  KC.EQ.3
!
           IF(ISTOPT(6).EQ.1) THEN

              DO K=1,2
                 CRNUM=1.+DELT*WSETA(2:LA,K,NS)*HPI(2:LA)*DZIC(K+1)
                 GRADSED=(SED(2:LA,K+1,NS)-SED(2:LA,K,NS))/(DZC(K+1)+DZC(K))
                 SEDAVG=0.5*(SED(2:LA,K+1,NS)-SED(2:LA,K,NS)+1.E-16)
                 WSETA(2:LA,K,NS)=-CRNUM*DZC(K+1)*WSETA(2:LA,K,NS)*GRADSED/SEDAVG
              ENDDO
!
!     TVAR1S=LOWER DIAGONAL
              TVAR1S(2:LA,1)=0.0
              DO K=2,KC
                 TVAR1S(2:LA,K)=MIN(WSETA(2:LA,K-1,NS),0.)
              ENDDO
!     TVAR1N=UPPER DIAGONAL
              TVAR1N(2:LA,KC)=0.0
              DO K=1,KS
                 TVAR1N(2:LA,K)=-MAX(WSETA(2:LA,K,NS),0.)
              ENDDO
!     TVAR1W=MAIN DIAGONAL
              TVAR1W(2:LA,1)=DELTI*DZC(1)*HP(2:LA)-MIN(WSETA(2:LA,1,NS),0.)
              TVAR1W(2:LA,KC)=DELTI*DZC(KC)*HP(2:LA)+MAX(WSETA(2:LA,KC-1,NS),0.)
              DO K=2,KS
                 TVAR1W(2:LA,K)=DELTI*DZC(KC)*HP(2:LA)+MAX(WSETA(2:LA,K-1,NS),0.)-MIN(WSETA(2:LA,K,NS),0.)
              ENDDO
!     TVAR1E=RIGHT HAND SIDE
              DO K=1,KC
                 TVAR1E(2:LA,K)=DELTI*DZC(KC)*HP(2:LA)*SED(2:LA,K,NS)
              ENDDO
!
!     TVAR3S=BET,TVAR2N=U,TVAR2S=GAM ARE WORKING ARRAYS
              TVAR3S(2:LA)=TVAR1W(2:LA,1)
              TVAR2N(2:LA,1)=TVAR1E(2:LA,1)/TVAR3S(2:LA)
              DO K=2,KC
                 TVAR2S(2:LA,K)=TVAR1N(2:LA,K-1)/TVAR3S(2:LA)
                 TVAR3S(2:LA)=TVAR1W(2:LA,K)-TVAR1S(2:LA,K)*TVAR2S(2:LA,K)
                 TVAR2N(2:LA,K)=(TVAR1E(2:LA,K)-TVAR1S(2:LA,K)*TVAR2N(2:LA,K-1))/TVAR3S(2:LA)
              ENDDO
              DO K=KS,1,-1
                 TVAR2N(2:LA,K)=TVAR2N(2:LA,K)-TVAR2S(2:LA,K+1)*TVAR2N(2:LA,K+1)
              ENDDO
              DO K=1,KC
                 SED(2:LA,K,NS)=TVAR2N(2:LA,K)
              ENDDO
           ENDIF
!
!----------------------------------------------------------------------C
!
! **  FINAL FLUX KC=3
!
           SEDF(2:LA,KC-1,NS)=DELTI*DZC(KC)*HP(2:LA)*(SED(2:LA,KC,NS)-SEDS(2:LA,KC,NS)) 
           !if(minval(SEDF) < 0.0) then
              !print *, 'Negative SEDF 4'
              !pause
           !endif
           SEDF(2:LA,1,NS)=DELTI*DZC(KC-1)*HP(2:LA)*(SED(2:LA,KC-1,NS)-SEDS(2:LA,KC-1,NS))+SEDF(2:LA,KC-1,NS)
           !if(minval(SEDF) < 0.0) then
              !print *, 'Negative SEDF 5'
              !pause
           !endif
        ENDIF
 !                *************************************                 !
 !end of if loop (KC .EQ. 3).
 !starting if KC .EQ. 2 loop.
        IF(KC .EQ. 2) THEN

! **  ANTI-DIFFUSION OF COHESIVE SEDIMENT  KC.EQ.2 
!
           IF(ISTOPT(6).EQ.1) THEN
              DO L=2,LA
                 DO K=1,1
                    CRNUM=1.+DELT*WSETA(L,1,NS)*HPI(L)*DZIC(KC)
                    GRADSED=(SED(L,KC,NS)-SED(L,1,NS))/(DZC(KC)+DZC(1))
                    SEDAVG=0.5*(SED(L,KC,NS)+SED(L,1,NS)+1.E-16)
                    ! WSETA(L,K,NS)=-CRNUM*DZC(KC)*WSETA(L,K,NS)*GRADSED/SEDAVG
                 ENDDO
              ENDDO
!
              DO L=2,LA
                 AA11=DELTI*DZC(1)*HP(L)-MIN(WSETA(L,1,NS),0.)
                 AA12=-MAX(WSETA(L,1,NS),0.)
                 AA21=MIN(WSETA(L,1,NS),0.)
                 AA22=DELTI*DZC(KC)*HP(L)+MAX(WSETA(L,1,NS),0.)
                 BB11=DELTI*DZC(1)*HP(L)*SED(L,1,NS)
                 BB22=DELTI*DZC(KC)*HP(L)*SED(L,KC,NS)
                 DETI=1./(AA11*AA22-AA12*AA21)
                 SED(L,1,NS)=DETI*( BB11*AA22-BB22*AA12 )
                 SED(L,KC,NS)=DETI*( AA11*BB22-AA21*BB11 )
              ENDDO
           ENDIF
!
!----------------------------------------------------------------------C
!
! **  FINAL FLUX KC=2
!
           DO L=2,LA
              SEDF(L,1,NS)=DELTI*DZC(KC)*HP(L)*(SED(L,KC,NS)-SEDS(L,KC,NS))
           ENDDO
           !if(minval(SEDF) < 0.0) then
              !print *, 'Negative SEDF 6'
              !pause
           !endif
        ENDIF
!                *************************************                 !
!end of if loop (KC .EQ. 2).
     ENDDO !end do of NS=1,NSCM loop
!PT: the rest of the IF loop is NSEDFLUME == 2 for toxic.  Not concerning sedimentary only.
  ELSEIF(NSEDFLUME==2) THEN
!**********************************************************************!
!   SEDZLJ Sediment and Contaminant Transport model
!
     DO NS=1,NSCM
        FORALL(L=2:LA,K=0:KS)WSETA(L,K,NS)=DWS(NS)/100.
!----------------------------------------------------------------------!
!
! **  HORIZONTAL LOOPS
! 
        K=KC
        SEDF(2:LA,K,NS)=0.0
        WVEL(2:LA)=DELT*HPI(2:LA)*DZIC(K)
        CLEFT(2:LA)=1.0+WSETA(2:LA,K-1,NS)*WVEL(2:LA)
        CRIGHT(2:LA)=MAX(SED(2:LA,K,NS),0.0)
        SED(2:LA,K,NS)=CRIGHT(2:LA)/CLEFT(2:LA)
        SEDF(2:LA,K-1,NS)=-WSETA(2:LA,K-1,NS)*SED(2:LA,K,NS)
        !if(minval(SEDF) < 0.0) then
           !print *, 'Negative SEDF 7'
           !pause
        !endif

        DO K=KC-1,2,-1
           WVEL(2:LA)=DELT*HPI(2:LA)*DZIC(K)
           CLEFT(2:LA)=1.0+WSETA(2:LA,K-1,NS)*WVEL(2:LA)
           CRIGHT(2:LA)=MAX(SED(2:LA,K,NS),0.0)-SEDF(2:LA,K,NS)*WVEL(2:LA)
           SED(2:LA,K,NS)=CRIGHT(2:LA)/CLEFT(2:LA)
           SEDF(2:LA,K-1,NS)=-WSETA(2:LA,K-1,NS)*SED(2:LA,K,NS)
           !if(minval(SEDF) < 0.0) then
              !print *, 'Negative SEDF 8'
              !pause
           !endif
        ENDDO
     ENDDO
!
!   Call Bedload subroutines here
!
     DO L=2,LA
!  **   Calculate total flux from bed and determine if 
!       there is enough sediment to erode from SEDZLJ
!			CALL CHEMBEDFLX(L)
!  **   Update the bed thickness based on the flux and calculate
!       the flux into the water column.
!
        QSBDTOP(L)=SUM(SSGI(1:NSCM)*SEDF(L,0,1:NSCM))
        QWBDTOP(L)=VDRBED(L,KBT(L))*SUM(SSGI(1:NSCM)*SEDF(L,0,1:NSCM),SEDF(L,0,1:NSCM)>0.0) &
                   +SUM(SSGI(1:NSCM)*VDRDEPO(1:NSCM)*SEDF(L,0,1:NSCM),SEDF(L,0,1:NSCM)<0.0)
        !DO NS=1,NSCM
        !QWBDTOP(L)=QWBDTOP(L)+SSGI(1:NSCM)*(VDRBED(L,KBT(L))*MAX(SEDF(L,0,NS),0.0)+VDRDEPO(NS)*MIN(SEDF(L,0,NS),0.0))
        !ENDDO
     ENDDO

!-----------------------------------------------------------------------!
!---------- ------------------------------------------------------------!
! The stuff below is for SEDTOX only.
!
! **  ANTI-DIFFUSION OF COHESIVE SEDIMENT  KC.GT.3 
!
     DO NS=1,NSCM
        IF(ISTOPT(6)==1) THEN
           DO K=1,KC-1
              FORALL(L=2:LA)
                 CRNUM(L)=1.0+DELT*WSETA(L,K,NS)*HPI(L)*DZIC(K+1)
                 GRADSED(L)=(SED(L,K+1,NS)-SED(L,K,NS))/(DZC(K+1)+DZC(K))
                 SEDAVG(L)=0.5*(SED(L,K+1,NS)+SED(L,K,NS)+1.0E-16)
                 WSETA(L,K,NS)=-CRNUM(L)*DZC(K+1)*WSETA(L,K,NS)*GRADSED(L)/SEDAVG(L)
              ENDFORALL
           ENDDO
!
!     TVAR1S=LOWER DIAGONAL
           TVAR1S(2:LA,1)=0.0
!	   TVAR1S(2:LA,K)=MIN(WSETA(2:LA,K-1,NS),0.0)
           DO K=2,KC
              TVAR1S(2:LA,K)=MIN(WSETA(2:LA,K-1,NS),0.0)
           ENDDO
!     TVAR1N=UPPER DIAGONAL
           TVAR1N(2:LA,KC)=0.0
           DO K=1,KC-1
              TVAR1N(2:LA,K)=-MAX(WSETA(2:LA,K,NS),0.0)
           ENDDO
!     TVAR1W=MAIN DIAGONAL
           TVAR1W(2:LA,1)=DELTI*DZC(1)*HP(2:LA)-MIN(WSETA(2:LA,1,NS),0.0)
           DO K=2,KC-1
              TVAR1W(2:LA,K)=DELTI*DZC(KC)*HP(2:LA)+MAX(WSETA(2:LA,K-1,NS),0.0)-MIN(WSETA(2:LA,K,NS),0.0)
           ENDDO
           TVAR1W(2:LA,KC)=DELTI*DZC(KC)*HP(2:LA)+MAX(WSETA(2:LA,KC-1,NS),0.0)
!     TVAR1E=RIGHT HAND SIDE
           DO K=1,KC
              TVAR1E(2:LA,K)=DELTI*DZC(KC)*HP(2:LA)*SED(2:LA,K,NS)
           ENDDO
!
!     TVAR3S=BET,TVAR2N=U,TVAR2S=GAM ARE WORKING ARRAYS
           TVAR3S(2:LA)=TVAR1W(2:LA,1)
           TVAR2N(2:LA,1)=TVAR1E(2:LA,1)/TVAR3S(2:LA)
           DO K=2,KC
              TVAR2S(2:LA,K)=TVAR1N(2:LA,K-1)/TVAR3S(2:LA)
              TVAR3S(2:LA)=TVAR1W(2:LA,K)-TVAR1S(2:LA,K)*TVAR2S(2:LA,K)
              TVAR2N(2:LA,K)=(TVAR1E(2:LA,K)-TVAR1S(2:LA,K)*TVAR2N(2:LA,K-1))/TVAR3S(2:LA)
           ENDDO
           DO K=KC-1,1,-1
              TVAR2N(2:LA,K)=TVAR2N(2:LA,K)-TVAR2S(2:LA,K+1)*TVAR2N(2:LA,K+1)
           ENDDO
           FORALL(L=2:LA,K=1:KC)SED(L,K,NS)=TVAR2N(L,K)
        ENDIF
!
!----------------------------------------------------------------------!
!
! **  FINAL FLUX KC.GT.3  for SEDZLJ Note: KS=KC-1
!
        SEDF(2:LA,KS,NS)=DELTI*DZC(KC)*HP(2:LA)*(SED(2:LA,KC,NS)-SEDS(2:LA,KC,NS))
        !if(minval(SEDF) < 0.0) then
           !print *, 'Negative SEDF 9'
           !pause
        !endif
        DO K=KS-1,1,-1
           SEDF(2:LA,K,NS)=DELTI*DZC(K+1)*HP(2:LA)*(SED(2:LA,K+1,NS)-SEDS(2:LA,K+1,NS))+SEDF(2:LA,K+1,NS)
        ENDDO
        !if(minval(SEDF) < 0.0) then
           !print *, 'Negative SEDF 10'
           !pause
        !endif
     ENDDO
  ENDIF

!-------------------------------end of sedtox and sed if loop: IF(NSEDFLUME==1.AND.N>=ISEDTIME) ----!

  ! *** Perform a little clean up, if needed
  DO NS=1,NSED
    DO K=1,KC
      DO L=2,LA
        IF(SED(L,K,NS).LT.-1.0)THEN
          WRITE(6,*)'SSEDTOX_MAIN',NS,IL(L),JL(L),SED(L,K,NS),'2'
          SED(L,K,NS)=0.0
        ENDIF
      ENDDO
    ENDDO
  ENDDO  

  !This is for the changing the hydraulic boundary condition/morphorlogy due to sediment transport.
  IF(IMORPH_SEDZLJ==1)THEN
  
    CALL MORPHJ
  
    DO NS=1,NSED
      DO K=1,KC
        DO L=2,LA
          IF(SED(L,K,NS).LT.-1.0)THEN
            WRITE(6,*)'SSEDTOX_MAIN',NS,HP(L),IL(L),JL(L),SED(L,K,NS),'3'
            SED(L,K,NS)=0.0            
          ENDIF
        ENDDO
      ENDDO
    ENDDO  
  ENDIF
  
  RETURN
END SUBROUTINE SEDZLJ_MAIN
