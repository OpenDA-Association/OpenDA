SUBROUTINE TECPLOT

!
! Tecplot customized output
! Calculate shear stress here even for non-sediment cases
!
! CALL SEDZLJ_SHEAR
!
! REVISION DATE:  May 24, 2006
! Craig Jones and Scott James
!***************************************************************
!*********************************************
	USE GLOBAL
	IMPLICIT NONE
	INTEGER::I,J,LN,L,K,KK,ITEMPMSK,CRAIG
	REAL::UTMP,VTMP,TEMPMSK,FLUXLOAD
	REAL::UTMPA,VTMPA,TEMPSTINC,LONTEMP,LATTEMP
	REAL,DIMENSION(KC)::CTEMP1
	REAL,DIMENSION(LCM,KC)::UTECPLOT,VTECPLOT
	REAL,DIMENSION(LCM)::UTMPS,VTMPS
	integer,save::nstep
	real::deltat
!	character(LEN=5)::string
	deltat=tidalp/float(ntsptc)
!	print*,'Time step:',deltat,' s'
	nstep=nstep+1
!
! Tecplot customized output
	TEMPMSK=-1.0
	ITEMPMSK=-1
!  Calculate shear stress here even for non-sediment cases

	CALL SEDZLJ_SHEAR

! DETERMINE INITIAL SED. THICKNESS
! CALC. FINAL SEDIMENT THICKNESS, INITIAL SEDIMENT-WATER INTERFACE
! IS AT ZERO
	FORALL(L=2:LA)
		TSET0T(L)=SUM(TSED0(1:KB,L)/BULKDENS(1:KB,L))
		TSEDT(L)=SUM(TSED(1:KB,L)/BULKDENS(1:KB,L))
	ENDFORALL
	FORALL(L=2:LA)THCK(L)=TSEDT(L)-TSET0T(L)
!
!
!*********************************************
!    OUTPUT FOR TECPLOT, SEDIMENT DATA
!
!	WRITE(110,*)'ZONE T="',tbegin+float(nstep-1)*deltat*float(ishprt)/86400.0,'" I= ' ,IC-4,' J= ' ,JC-4,' K = ',KC,' F=POINT'
	WRITE(111,*)'ZONE T="',tbegin+float(nstep-1)*deltat*float(ishprt)/86400.0,'" I= ' ,IC-4,' J= ' ,JC-4,' F=POINT'
	WRITE(112,*)'ZONE T="',tbegin+float(nstep-1)*deltat*float(ishprt)/86400.0,'" I= ' ,IC-4,' J= ' ,JC-4,' F=POINT'

    IF(STINC.LT.1)THEN
	  TEMPSTINC=1
	ELSE
	  TEMPSTINC=STINC
	ENDIF

	DO  L=2,LA
		I=IL(L)
		J=JL(L)
		IF(IWRSP(1)<1) NCORENO(I,J)=0

		CBLTOT(L)=1.0E6*SUM(CBL(1,L,1:NSCM))
		FORALL(KK=1:KC)CAVG(L,KK)=SUM(SED(L,KK,1:NSCM))
		FORALL(K=1:KC-1)HEIGHT(I,J,K)=-(HP(L)-HP(L)*Z(K))
		HEIGHT(I,J,KC)=0.0
		FORALL(K=1:KC)
			CTEMP1(K)=SUM(SED(L,K,1:NSCM))
		ENDFORALL
		CAVGT(L)=SUM(CTEMP1(1:KC)*DZC(1:KC))

		FORALL(KS=1:NSCM)
    		CAVGS(L,KS)=SUM(SED(L,1:KC,KS)*DZC(1:KC))
		ENDFORALL


	ENDDO 
	DO L=2,LA
		LN=LNC(L)

		DO K=1,KC
!		  UTMPS=U(LIJ(I,J),K) ! m/s
!		  VTMPS=V(LIJ(I,J),K)  
		  UTMPS(L)=0.5*STCUV(L)*(RSSBCE(L)*U(L+1,K)+RSSBCW(L)*U(L,K))  ! m/s
		  VTMPS(L)=0.5*STCUV(L)*(RSSBCN(L)*V(LN ,K)+RSSBCS(L)*V(L,K))  
!		  UTECPLOT(L,K)=CUE(L)*UTMPS+CVE(L)*VTMPS  
!		  VTECPLOT(L,K)=CUN(L)*UTMPS+CVN(L)*VTMPS
		ENDDO
	ENDDO

! 2 Dimensional Output
!
	DO J=3,JC-2
		DO I=3,IC-2
		
	 IF(LIJ(I,J)>0) THEN
        L=LIJ(I,J)	
        UTMPA=0.0
        VTMPA=0.0
		DO K=1,KC
!		  UTMPS=U(LIJ(I,J),K) ! m/s
!		  VTMPS=V(LIJ(I,J),K) 
		  UTMPA=UTMPS(LIJ(I,J))*DZC(K)+UTMPA
		  VTMPA=VTMPS(LIJ(I,J))*DZC(K)+VTMPA
!		  UTMPA=UTECPLOT(LIJ(I,J),K)*DZC(K)+UTMPA
!		  VTMPA=UTECPLOT(LIJ(I,J),K)*DZC(K)+VTMPA
		ENDDO	


				WRITE(111,'(I4,1X,I4,1X,12E17.7)')I,J,DLON(LIJ(I,J)),DLAT(LIJ(I,J))&
				,TAU(LIJ(I,J)),D50AVG(LIJ(I,J)),THCK(LIJ(I,J)),HP(LIJ(I,J)),UTMPA,VTMPA,&
                BELV(LIJ(I,J)),CAVGT(L)
!				ETOTO(LIJ(I,J))-DEPO(LIJ(I,J)),CAVGT(L)!

!				UBL(LIJ(I,J),1),VBL(LIJ(I,J),1)
!				WRITE(111,'(I4,1X,I4,1X,12E17.7)')I,J,DLON(LIJ(I,J)),DLAT(LIJ(I,J))&
!				,TAU(LIJ(I,J)),UTMPA,VTMPA

				WRITE(112,'(14E13.4)')DLON(LIJ(I,J)),DLAT(LIJ(I,J)),D50AVG(LIJ(I,J)),THCK(LIJ(I,J)),&
				CAVGT(L),CBLTOT(L),(PER(KK,1,LIJ(I,J)),KK=1,NSCM),(PER(KK,2,LIJ(I,J)),KK=1,NSCM)
		
     ELSE
     

				WRITE(111,'(I4,1X,I4,1X,12E13.4)')I,J,TEMPMSK,TEMPMSK,TEMPMSK,TEMPMSK,TEMPMSK,&
				TEMPMSK,0,0,TEMPMSK,TEMPMSK
				



	  ENDIF

      ENDDO
	ENDDO


	RETURN
END SUBROUTINE TECPLOT
