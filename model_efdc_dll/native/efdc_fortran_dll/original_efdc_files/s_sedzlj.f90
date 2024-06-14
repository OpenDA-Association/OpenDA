SUBROUTINE SEDZLJ(L)
  USE GLOBAL
  IMPLICIT NONE
  INTEGER::LL,K,L
  INTEGER::NSC0,NSC1,NTAU0,NTAU1
  REAL::T1TMP, T2TMP
  DOUBLE PRECISION::WDTDZ
  DOUBLE PRECISION::SN00
  DOUBLE PRECISION::SN01
  DOUBLE PRECISION::SN10
  DOUBLE PRECISION::SN11
  DOUBLE PRECISION,DIMENSION(NSCM)::PX,PY,PFY,PROB,SMASS,CSEDSS
  DOUBLE PRECISION::D50TMPP,TEMP,TEMP2
  DOUBLE PRECISION::ESED
  
  INTEGER CRAIG
  INTEGER SURFACE
  NTAU0=0
  NTAU1=0
  NSC0=0
  NSC1=0

  IF(IS_TIMING)THEN  
    CALL CPU_TIME(T1TMP)  
  ENDIF 
  
  !
  ! UCSB, Craig Jones and Wilbert Lick
  !
  ! CALCULATES DEPOSITION, ENTRAINMENT, NET FLUX, TOTAL THICKNESS,
  ! LAYER ORDER, AND COMPONENT LAYER FRACTION
  ! Assuming ISEDDT in EFDC is 1 for now
  ! REVISION DATE :  August 29th, 2007
  ! Craig Jones and Scott James
  ! Updated to fix Active Layer issues
  !**********************************************************************
  
  ! CALCULATE EROSION/DEPOSITION FOR TOP LAYER
  ! FOR ALL GRID POINTS
  ETOTO(L)=0.0 !initialize no total erosion for a cell
  DEP(L)=0.0   !initialize no total deposition for a cell
  HT(L)=HP(L)*100.0 ! Convert depth (HP) from Meters to Centimeters
  ! Convert Bottom Shear from (m/s)^2 to dynes/cm^2
  ! Only when shear from EFDC is used
  
  IF (L.EQ.331) THEN
    CRAIG=0
  ENDIF
  
  
  !TAU(L)=TAUBSED(L)*1000.0*10.0
  D(1:NSCM,L)=0.0    !initialize deposition from suspended load
  DBL(1:NSCM,L)=0.0  !initialize deposition from bedload
  ! Convert Bottom Concentration from mg/l=g/m^3 to g/cm^3  
  
  IF (KC == 1) THEN
     DO K=1,NSCM
        VZDIF(L)=MAX(20.D0,0.067*HT(L)*USTAR(L)*100.) !PT due to units of USTAR calc. in s_shear.f90
        TEMP2=HT(L)*DWS(K)/VZDIF(L)
        CTB(K,L)=SED(L,1,K)*TEMP2*(1.0/(1.0-EXP(-TEMP2)))*1.0E-06
     ENDDO
  ELSE
     CTB(1:NSCM,L)=SED(L,1,1:NSCM)*1.0E-06 
  ENDIF
  E (1:NSCM,L)=0.0  !initialize erosion rates for each size class and each cell
  ELAY(1:NSCM)=0.0  !initialize top-layer erosion rates for each sediment size class
  SMASS(1:NSCM)=0.0 !initializes sediment mass available for deposition
  !**********************************************
  ! CALCULATE DEPOSITION
  ! Temporarily calculate the SMASS in the active layer (g/cm^3)
  ! so that percentages can be determined after deposition.
  TTEMP(1:NSCM,L)=PER(1:NSCM,1,L)*TSED(1,L) !temporary thickness variable for each size class equals the mass fraction times the top-layer thickness
  ! Now calculate DEPOSITION
  ! Deposition from suspended load
  DO K=1,NSCM !loop through all sediment sizes
     IF(CTB(K,L)<1.0E-10)CYCLE !if there is no sediment of that size available in the water column, then it cannot be deposited
     ! Calculate probability for suspended load deposition
     ! based on Gessler (1965) if D50 > 200um or Krone if < 200 um  
     IF(D50(K)>=200.0)THEN
        PY(K)=DABS(1.7544*(TCRSUS(K)/(TAU(L)+1.0d-18)-1.0))   ! PMC - Handle Tau=0
!        PFY(K)=0.39894*EXP(-0.5*PY(K)*PY(K)) !CAJ's old formula A&S 7.1.25 (wrong)
!        PX(K)=1.0/(1.0+0.3327*PY(K)) !CAJ's old formulation A&S 7.1.25 (wrong)
        PFY(K)=EXP(-0.25d0*PY(K)*PY(K)) !PFY(K)=0.39894*EXP(-0.5*PY(K)*PY(K)) CAJ's old formula A&S 7.1.25
        PX(K)=1.0/(1.0+0.235235*PY(K)) !PX(K)=1.0/(1.0+0.3327*PY(K)) CAJ's old formulation A&S 7.1.25
        PROB(K)=PFY(K)*(0.34802*PX(K)-0.09587*PX(K)*PX(K)+0.74785*PX(K)**3) !A&S 7.1.25 a1, a2, a3 used
        IF(TAU(L)>TCRSUS(K))PROB(K)=1.0-PROB(K) !account for negatives
     ELSEIF(TAU(L)<=TCRSUS(K))THEN !if the local shear is less than the critical shear for that size class
        PROB(K)=1.0-TAU(L)/(TCRSUS(K)) !deposition probability is calculated
     ELSE
        PROB(K)=0.0
     ENDIF
     ! Calculate SMASS of sediment present in water
     ! and allow only that much to be deposited.
     SMASS(K)=CTB(K,L)*DZC(1)*HT(L)*MAXDEPLIMIT !SMASS(K) is the total sediment mass in the first layer.  It is calculated as a precaution so that no more than the total amount of mass in the first layer can deposit onto the sediment bed per time step.
     D(K,L)=PROB(K)*(DWS(K)*DT)*CTB(K,L) !deposition of a size class is equal to the probability of deposition times the settling rate times the time step times the sediment concentration
  ENDDO
  D(1:NSCM,L)=MIN(MAX(D(1:NSCM,L),0.D0),SMASS(1:NSCM)) !do not allow more sediment to deposit than is available in the water-column layer above the bed
  ! Deposition from Bedload
  DO K=1,NSCM
     IF(CBL(1,L,K)>1.0E-10.AND.BLVEL(L,K)>0.0 .AND. BLFLAG(L,K)==1)THEN !if there is bedload
        CSEDSS(K)=EBL(L)/(DWS(K)*DT) !concentration eroded into bedload from the last time step
        CSEDVR(K,L)=(0.18*2.65*TRANS(L,K)/DISTAR(K)*0.65) !van Rijn's (1981, Eq. 21) equilibrium bedload concentration (here TRANS is the transport parameter for bedload calculations)
        !			CSEDVR(K,L)=(0.18*TRANS(L,K)/DISTAR(K)*0.65)
        IF(CSEDVR(K,L)<=0.0)THEN !if there is no equilibrium bedload available
           PROBVR(K,L)=1.0 !then deposition probability is unity
        ELSE
           PROBVR(K,L)=MIN(CSEDSS(K)/CSEDVR(K,L),1.D0) !van Rijn probability of deposition from bedload
        ENDIF

! In case Ebl = 0  The deposition from bedload reverts to Gessler's for
! That particle size.
        IF(CSEDSS(K)<=0.0)PROBVR(K,L)=PROB(K) !error prevention
        ! Calculate Bedload Deposition (DBL)
        SMASS(K)=CBL(1,L,K)*DZBL(L,K) !local mass available for deposition is the bedload concentration times the bedload height
        DBL(K,L)=PROBVR(K,L)*CBL(1,L,K)*(DWS(K)*DT) !deposition from bedload is the van Rijn probability time bedload concentration time settling velocity times the time step
     ENDIF
  ENDDO
  DBL(1:NSCM,L)=MIN(MAX(DBL(1:NSCM,L),0.D0),SMASS(1:NSCM)) !do not allow more bedload deposition than bedload mass available
  ! TOTAL DEPOSITION
  DEP(L)=SUM(DBL(1:NSCM,L),BLVEL(L,1:NSCM)>0.0)+SUM(D(1:NSCM,L)) !total depositopn is the sume of bedload and suspended load depositions for all size classes
  DEPO(L)=DEP(L)/(DT) !calculate the deposition rate
  ! ADD DEPOSITION TO TOP LAYER
  IF(DEP(L)>0.0)THEN !if there is deposition, calculate the new layer thickness and mass fractions
     ! LAYER: logical variable that is 0 if a layer is present at cell (this variable exists for each sediment bed layer initially defined)
     LAYER(1,L)=1 !top sediment bed layer exists (because there is deposition)
     TSED(1,L)=TSED(1,L)+DEP(L)
     WHERE(BLVEL(L,1:NSCM)>0.0) !where there is bedload possible
        PER(1:NSCM,1,L)=(TTEMP(1:NSCM,L)+DBL(1:NSCM,L)+D(1:NSCM,L))/(TSED(1,L)) !recalculate mass fractions
     ELSEWHERE
        PER(1:NSCM,1,L)=(TTEMP(1:NSCM,L)+D(1:NSCM,L))/(TSED(1,L)) 
     ENDWHERE
  ENDIF

!******************************************************
! Get things set up for Erosion Calculations
! Find the next layer (SLLN) of sediment below the top layer
  IF(LAYER(2,L)==1)THEN !check to ensure that there are at least 2 sediment layers present here
     SLLN(L)=2 !layer number below active layer (always 3 to start after SEDIC, always 2 when there at least 2 layers)
  ELSE
     DO LL=3,KB
        IF(LAYER(LL,L)==1.AND.LAYER(LL-1,L)/=1)THEN
           SLLN(L)=LL !next layer is renumbered as necessary (happens if active layer is eroded completely)
           EXIT
        ENDIF
     ENDDO
  ENDIF

  ! Calculate Average particle size of surface layer So we can calculate
  ! Active layer Thickness
  IF(LAYER(1,L)==1)THEN !is the top layer present?
     SURFACE=1 !surface variable established
  ELSE
     SURFACE=SLLN(L) !otherwise the top layer is SLLN
  ENDIF
  D50AVG(L)=SUM(PER(1:NSCM,SURFACE,L)*D50(1:NSCM)) !calculate local d50 at sediment bed surface
  FORALL(LL=1:KB)SEDDIA50(L,LL)=REAL(SUM(PER(1:NSCM,LL,L)*D50(1:NSCM)),KIND(SEDDIA50)) !EFDC variable
 
  ! Identify Size Class interval to use for Taucrit erosion calculation
  DO K=1,NSICM-1
     IF(D50AVG(L)>=SCND(K).AND.D50AVG(L)<SCND(K+1))THEN
        NSCD(1)=INT(SCND(K),KIND(NSCD))
        NSCD(2)=INT(SCND(K+1),KIND(NSCD))
        NSC0=K
        NSC1=K+1
        EXIT
     ENDIF
  ENDDO

  ! Calculate TAUCRIT Based on the Average Particle Size of Surface
  ! Then calculate the Active Layer Thickness from it.
  ! Ta =  Tam * Davg * (Tau/Taucr)

  IF(SURFACE==1.OR.SURFACE==2)THEN
     TAUCRIT(L)=TAUCRITE(NSC0)+(TAUCRITE(NSC1)-TAUCRITE(NSC0))/(NSCD(2)-NSCD(1))*(D50AVG(L)-NSCD(1))
  ELSE
     TAUCRIT(L)=TAUCOR(SURFACE,L)
  ENDIF
  IF(TAU(L)/TAUCRIT(L)<1.0)THEN
     TACT(L)=TACTM*D50AVG(L)*(BULKDENS(1,L)/10000.0)
  ELSE
     TACT(L)=TACTM*D50AVG(L)*(TAU(L)/TAUCRIT(L))*(BULKDENS(1,L)/10000.0)
  ENDIF

! This is where we determine if there is an un-erodeable
! Size class.  If there is one, we need an active layer.
  NACTLAY=0
  DO LL=1,KB !search through all layers
     IF(LAYER(LL,L)>0)THEN !if the layer is present
        DO K=1,NSCM
           IF(PER(K,LL,L)>0.0.AND.TAU(L)<TCRE(K).AND.TAU(L)>TAUCRIT(L))THEN !if the size class is present and the sediment bed is eroding and there is insufficient shear stress for suspension
              NACTLAY=1    !there is an active layer (logical variable)
              LAYER(1,L)=1 !there is an active layer
           ENDIF
        ENDDO
        EXIT
     ENDIF
  ENDDO

  ! If the layer exposed can erode, then 
  ! use the active layer model, otherwise just put deposited material
  ! on top.  Also if there is a size class present in the top layer
  ! that will not erode, create an active layer.
  IF(TSED(1,L)>0.0.OR.NACTLAY/=0)THEN !if there is mass in the active layer, we must go through the sorting routine
     !no active layer for pure erosion (active layer needed for coarsening and deposition)

     ! Sort layers so that the active layer is always Ta thick.
     ! Recalculate the mass fractions after borrowing from lower layers
     IF(LAYER(1,L)==1)THEN
        IF(TSED(1,L)>TACT(L))THEN !this happens when there is net deposition over this time step
           DO K=1,NSCM
           PER(K,2,L)=(PER(K,2,L)*TSED(2,L)+PER(K,1,L)*(TSED(1,L)-TACT(L)))/(TSED(2,L)+(TSED(1,L)-TACT(L))) !recalculate mass fractions
           ENDDO
           LAYER(2,L)=1 !ensure that the second layer logical is turned on
           SLLN(L)=2 !next lower layer is 2
           TSED(2,L)=TSED(2,L)+TSED(1,L)-TACT(L) !add layer thickness in excess of active-layer thickness to next lower layer
           TSED(1,L)=TACT(L) !reset top layer thickness to active layer thickness
        ELSEIF(TSED(1,L)<TACT(L).AND.TSED(1,L)+TSED(SLLN(L),L)>TACT(L).AND.TAU(L)>TAUCOR(SLLN(L),L))THEN !this happens when there is net erosion over this time step and there is sufficient sediment below to reconstitute the active layer
           DO K=1,NSCM
           PER(K,1,L)=(PER(K,1,L)*TSED(1,L)+PER(K,SLLN(L),L)*(TACT(L)-TSED(1,L)))/TACT(L) !recalculate the mass fraction
           ENDDO
           TSED(SLLN(L),L)=TSED(SLLN(L),L)-(TACT(L)-TSED(1,L)) !borrow thickness from lower layer
           TSED(1,L)=TACT(L) !reset top layer thickness to active layer thickness
        ELSEIF(TSED(1,L)<TACT(L).AND.TSED(1,L)+TSED(SLLN(L),L)<=TACT(L).AND.TAU(L)>TAUCOR(SLLN(L),L))THEN !this happens when there is net erosion over this time step and there is NOT sufficient sediment below to reconstitute the active layer
           FORALL(K=1:NSCM)
              PER(K,1,L)=(PER(K,1,L)*TSED(1,L)+PER(K,SLLN(L),L)*(TSED(SLLN(L),L)))/(TSED(1,L)+TSED(SLLN(L),L)) !recalculate the mass fraction
              PER(K,SLLN(L),L)=0.0 !no more sediment available in next lower layer
           ENDFORALL
           TSED(1,L)=TSED(1,L)+TSED(SLLN(L),L) !add available sediment to layer
           TSED(SLLN(L),L)=0.0 !no more sediment available below this
           LAYER(SLLN(L),L)=0 !layer has been eliminated in the logical variable
           SLLN(L)=SLLN(L)+1 !set next layer lower 
           IF (SLLN(L)+1>KB) SLLN(L)=KB !do not allow specification of the next lower layer to be below the bottom sediment layer
        ENDIF
     ENDIF
  ENDIF
  !**************************************************************
  ! Now calculate the Erosion Rates
  ALL_LAYERS:DO LL=1,KB !go through all sediment layers so that they are properly eroded and sorted
     IF(LAYER(LL,L)==0)CYCLE !if the layer is gone don't consider it
     IF(LAYER(1,L)==1.AND.LL/=1)EXIT !if it is depositional, there is no need to consider erosion
     IF(LL/=1)THEN
        IF(LAYER(LL-1,L)==1)EXIT
     ENDIF
     D50AVG(L)=SUM(PER(1:NSCM,LL,L)*D50(1:NSCM)) ! Find mean diameter of Layer
     ! Find upper and lower limits of size classes on mean bed diameter
     DO K=1,NSICM-1
        IF(D50AVG(L)>=SCND(K).AND.D50AVG(L)<SCND(K+1))THEN
           NSCD(1)=INT(SCND(K), KIND(NSCD))
           NSCD(2)=INT(SCND(K+1), KIND(NSCD))
           NSC0=K
           NSC1=K+1
           EXIT
        ENDIF
     ENDDO
     ! Calculate TAUCRIT Based on the D50 of the bed or from Sedflume Data
     IF(LL==1.OR.LL==2)THEN !for active layer
        TAUCRIT(L)=TAUCRITE(NSC0)+(TAUCRITE(NSC1)-TAUCRITE(NSC0))/(NSCD(2)-NSCD(1))*(D50AVG(L)-NSCD(1)) !interpolation
        TAUCOR(LL,L)=TAUCRIT(L)
     ELSE
        SN01=TSED(LL,L)/TSED0(LL,L)               !weighting factor 1 for interpolation
        SN11=(TSED0(LL,L)-TSED(LL,L))/TSED0(LL,L) !weighting factor 2
        IF(LL+1<=KB)THEN !Avoid array exceedance when LL=KB and used in TAUCOR(LL+1,L)
           TAUCRIT(L)=SN01*TAUCOR(LL,L)+SN11*TAUCOR(LL+1,L) !
        ELSE
           TAUCRIT(L)=1.0e6 !set arbitrarily high
        ENDIF
     ENDIF
     ! Now, calculate erosion rates
     IF(TAU(L)<TAUCRIT(L))EXIT
     ! Find the upper and lower limits of the Shear Stress for the interpolation
     DO K=1,ITBM-1
        IF(TAU(L)>=TAULOC(K).AND.TAU(L)<TAULOC(K+1))THEN
           TAUDD(1)=TAULOC(K)
           TAUDD(2)=TAULOC(K+1)
           NTAU0=K
           NTAU1=K+1
           EXIT
        ELSEIF(TAU(L)>=TAULOC(ITBM))THEN
           TAUDD(1)=TAULOC(ITBM-1)
           TAUDD(2)=TAULOC(ITBM)
           NTAU0=ITBM-1
           NTAU1=ITBM
        ENDIF
     ENDDO
     ! Interpolate the erosion rates for shear stress and depth.
     ! This utilizes normal sedflume data for deeper layers.
     IF(LL>2)THEN !calculate erosion rates of deeper layers
        SN00=(TAUDD(2)-TAU(L))/(TAUDD(2)-TAUDD(1)) !weighting factor 1 for interpolation
        SN10=(TAUDD(1)-TAU(L))/(TAUDD(1)-TAUDD(2)) !weighting factor 2
        SN01=TSED(LL,L)/TSED0(LL,L)                !weighting factor 3
        SN11=(TSED0(LL,L)-TSED(LL,L))/TSED0(LL,L)  !weighting factor 4
        
        IF(LL+1<=KB)THEN !modeled erosion rate
           ERATEMOD(L)=(SN00*EXP(SN11*LOG(ERATE(LL+1,L,NTAU0))+SN01*LOG(ERATE(LL,L,NTAU0)))&
                +SN10*EXP(SN11*LOG(ERATE(LL+1,L,NTAU1))+SN01*LOG(ERATE(LL,L,NTAU1))))*BULKDENS(LL,L)
        ELSE !do not allow erosion through the bottom layer
           ERATEMOD(L)=(SN00*EXP(SN11*LOG(1e-9)+SN01*LOG(ERATE(LL,L,NTAU0)))&
                +SN10*EXP(SN11*LOG(1e-9)+SN01*LOG(ERATE(LL,L,NTAU1))))*BULKDENS(LL,L)
        ENDIF
     ELSE

        ! For Layers One and Two (the newly deposited sediments)
        ! The erosion rate for these layers is determined from 
        ! Sedflume experiments and is based on average particle
        ! Size (D50AVG) 
        NSCTOT=NSCD(2)-NSCD(1) !difference in interpolant size class
        D50TMPP=D50AVG(L)-NSCD(1) !difference from local size class and lower interpolant
        SN00=(TAUDD(2)-TAU(L))/(TAUDD(2)-TAUDD(1)) !weighting factor 1 for interpolation
        SN10=(TAUDD(1)-TAU(L))/(TAUDD(1)-TAUDD(2)) !weigthing factor 2
        SN01=D50TMPP/NSCTOT                        !weighting factor 3
        SN11=(NSCTOT-D50TMPP)/NSCTOT               !weighting factor 4
        ERATEMOD(L)=(SN00*EXP(SN11*LOG(ERATEND(NSC0,NTAU0))+SN01*LOG(ERATEND(NSC1,NTAU0)))+SN10*EXP(SN11*LOG(ERATEND(NSC0,NTAU1)) &
                    +SN01*LOG(ERATEND(NSC1,NTAU1))))*BULKDENS(LL,L) !log-linear interpolation
     ENDIF

     ! Sort out Thicknesses and Erosion Rates
     ! Assuming ISEDDT in EFDC is 1 for now
     EBD(LL,L)=ERATEMOD(L)*DT !amount eroded this time step for this layer

     ! If the shear stress is less than the critical shear stress for a
     ! particular size class, then it is not eroded from the bed.
     !
     ! Calculate New SMASS (TTEMP) of each sediment in Bed (g/cm^2)
     ! Conservation of SMASS, you can only erode as much as is there.
     ! E(K,L)  = Total erosion at this cell of size class k
     ! ETOTO(L) = Total erosion at this cell
     ! ELAY(K) = Erosion from this layer of size class k
     ! ESED  =  Total erosion from layer
     
     WHERE(TAU(L)>=TCRE(1:NSCM))
        E(1:NSCM,L)=E(1:NSCM,L)+PER(1:NSCM,LL,L)*EBD(LL,L)
        ELAY(1:NSCM)=PER(1:NSCM,LL,L)*EBD(LL,L)
        TTEMP(1:NSCM,L)=PER(1:NSCM,LL,L)*TSED(LL,L)-ELAY(1:NSCM) !thickness due to each size class
     ELSEWHERE
        E(1:NSCM,L)=0.0
        ELAY(1:NSCM)=0.0
        TTEMP(1:NSCM,L)=PER(1:NSCM,LL,L)*TSED(LL,L)
     ENDWHERE
     WHERE(TTEMP(1:NSCM,L)<0.0) !if the thickness due to a size class is negative
        TTEMP(1:NSCM,L)=0.0 !set thickness to zero
        E(1:NSCM,L)=E(1:NSCM,L)-PER(1:NSCM,LL,L)*EBD(LL,L)+PER(1:NSCM,LL,L)*TSED(LL,L) !recalculate erosions
        ELAY(1:NSCM)=PER(1:NSCM,LL,L)*TSED(LL,L)
     ENDWHERE
     ESED=SUM(ELAY(1:NSCM))    !total erosion from the layer
     ETOTO(L)=SUM(E(1:NSCM,L)) !total erosion in cell

     ! Subtract total erosion from layer thickness
     ! Then Calculate new percentages
     TEMP=TSED(LL,L)-ESED !eroded layer thickness (recall that this is in a loop and calculated for each layer)
     IF(TEMP<=D50(1)/1.0e6)THEN !if the layer eroded (or smaller than the smallest size class), set its thicknesses to zero
        TSED(LL,L)=0.0  !this layer has no thickness
        LAYER(LL,L)=0   !this layer is absent
        FORALL(K=1:NSCM)PER(K,LL,L)=0.0 !zero mass fractions
     ELSE
        TSED(LL,L)=TEMP !new layer thickness
        FORALL(K=1:NSCM)PER(K,LL,L)=TTEMP(K,L)/TSED(LL,L) !new mass fractions
     ENDIF
     IF(IMORPH==0)HBED(L,KB+1-LL)=REAL(0.01*TSED(LL,L)/BULKDENS(LL,L),KIND(HBED))    !if there is no morphology, save the EFDC bed thickness
  ENDDO ALL_LAYERS

! DETERMINE TOTAL SEDIMENT FLUX
  EBL(L)=0.0  !zero erosion into bedload
  ESUS(L)=0.0 !zero erosion into suspended load
  DO K=1,NSCM
     IF(BLFLAG(L,K)==1)THEN !if there is bedload
        QBSED(L,K)=(1.0-PSUS(L,K))*E(K,L)-DBL(K,L) !flux of sediment from the bed into bedload
        BED_SED_FLX(L,K)=PSUS(L,K)*E(K,L)-D(K,L)   !flux into suspended load
     ELSE
        BED_SED_FLX(L,K)=E(K,L)-D(K,L) !if there is no bedload there is only erosion into suspended load
     ENDIF
  ENDDO
  EBL(L)=SUM((1.0-PSUS(L,1:NSCM))*E(1:NSCM,L),BLVEL(L,1:NSCM)>0.0) !reclaculate erosion into bedload
  ESUS(L)=SUM(PSUS(L,1:NSCM)*E(1:NSCM,L),BLVEL(L,1:NSCM)>0.0)      !recalculate erosion into suspended load

  ! Flux calculations for EFDC
  ! Convert the BED_SED_FLX from g/cm^2 to g/m^2*s
  WDTDZ=DT*HPI(L)*DZIC(1) !Detla t over Delta z
  SEDF(L,0,1:NSCM)=REAL(BED_SED_FLX(L,1:NSCM)*10000.0/DT,KIND(SEDF)) !EFDC variable for sediment flux
  SED(L,1,1:NSCM)=REAL(SEDS(L,1,1:NSCM)+(SEDF(L,0,1:NSCM)-SEDF(L,1,1:NSCM))*WDTDZ,KIND(SED)) !EFDC variable for suspended sediment concentration
  ETOTO(L)=ETOTO(L)/(DT) !total erosion rate

  IF(IS_TIMING)THEN  
    CALL CPU_TIME(T2TMP)
    TSSEDZLJ=TSSEDZLJ+T2TMP-T1TMP   
  ENDIF 
  
  RETURN
END SUBROUTINE SEDZLJ

