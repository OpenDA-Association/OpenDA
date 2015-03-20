SUBROUTINE BEDLOADJ
  USE GLOBAL     
  IMPLICIT NONE 
  INTEGER::I,J,L,K
  !PT: real values are written in DOUBLE PRECISION 7/16/08.
  DOUBLE PRECISION,DIMENSION(LCM)::VELMAG  
  !
  !  University of California, Santa Barbara
  !  Craig Jones and Wilbert Lick
  !
  !  Bedload transport subroutine based on Van Rijn's transport
  !  Equations.
  !
  !  REVISION DATE :  May 24, 2006
  !  Craig Jones and Scott James
  !***************************************************************
  !  Calculate Percentage of erosion into suspension PSUS
  !  and whether the cell has bedload or not BLFLAG
  FORALL(L=2:LA)
     FORALL(K=1:NSCM)
        USW(L,K)=SQRT(TAU(L))/DWS(K) !DWS is settling speed.  USW is the shear velocity.
     ENDFORALL
  ENDFORALL
  !Loop below determines if bedload exists or not.  There are three regimes of tranport in this loop.
  !the first conditional in the where check for a large enough particle's diameter and small enough
  !shear velocity.  If the particle is too small or the shear velocity is too large, then all the sediment 
  !transport is in the suspended load, specified by suspended probability (PSUS = 1).  If the particle
  !is large enough and the shear velocity is small enough then we have two situations. In the first case, 
  !shear stress tau is smaller than the critical shear velocity or if shear velocity is negative or zero
  !then there is neither bedload transport or suspended load transport.  Otherwise, both bedload and suspended
  !load transport exists.  Also calculated is the probability of suspension for suspended load PSUS (eqn. 8).  
  LCM_LOOP_1:DO L=2,LA
     WHERE(D50(1:NSCM)>=200.0.AND.USW(L,1:NSCM)<4.0)
        WHERE(TAU(L)<=TCRE(1:NSCM).OR.USW(L,1:NSCM)<=0.0)
           BLFLAG(L,1:NSCM)=0
           PSUS(L,1:NSCM)=0.0
        ELSEWHERE                              
           BLFLAG(L,1:NSCM)=1
           PSUS(L,1:NSCM)=MAX((LOG(USW(L,1:NSCM))-LOG(SQRT(TCRSUS(1:NSCM))/DWS(1:NSCM)))/(LOG(4.0)-LOG(SQRT(TCRSUS(1:NSCM))/DWS(1:NSCM))),0.D0)
        ENDWHERE
     ELSEWHERE
        BLFLAG(L,1:NSCM)=0
        PSUS(L,1:NSCM)=1.0
     ENDWHERE
  ENDDO LCM_LOOP_1
  SED_LOOP:DO K=1,NSCM
     LCM_LOOP_2:DO L=2,LA
        VELMAG(L)=SQRT(U(L,1)**2+V(L,1)**2)
        TRANS(L,K)=MAX((TAU(L)-TCRE(K))/TCRE(K),0.D0) !eqn. 21
        DZBL(L,K)=MIN(100.D0*HP(L),D50(K)/10000.0*0.3*DISTAR(K)**0.7*SQRT(TRANS(L,K))) !(eqn. 20b) don't allow bedload height to exceed water column depth
        IF(DZBL(L,K)/=0.0.AND.DZBL_LAST(L,K)/=0.0)CBL(1,L,K)=CBL(1,L,K)*DZBL_LAST(L,K)/DZBL(L,K)
        BLVEL(L,K)=1.5*TRANS(L,K)**0.6*SQRT(((SEDDENS/WATERDENS) -1.0)*980.0*D50(K)/10000.0)      !eqn. 20a
     ENDDO LCM_LOOP_2
     WHERE(VELMAG(2:LA)>0.0)
        UBL(2:LA,K)=BLVEL(2:LA,K)*U(2:LA,1)/VELMAG(2:LA)
        VBL(2:LA,K)=BLVEL(2:LA,K)*V(2:LA,1)/VELMAG(2:LA)
     ELSEWHERE           
        UBL(2:LA,K)=0.0
        VBL(2:LA,K)=0.0
     ENDWHERE
     
     !**********************************************************************!
     ! All the equations below are solving the pde in eqn.18.
     ! X Bedload flux at I-1/2 interface
     LCM_LOOP_3:DO L=2,LA
        I=IL(L)
        J=JL(L)
        IF(LIJ(I-1,J)>0) THEN
           IF(UBL(L,K).GT.0) THEN
              XBLFLUX(L,K)=DT/(DXP(L)*100.0)*CBL(1,LIJ(I-1,J),K)*UBL(LIJ(I-1,J),K)*DZBL(LIJ(I-1,J),K)
           ELSE
              XBLFLUX(L,K)=DT/(DXP(L)*100.0)*CBL(1,LIJ(I,J),K)*UBL(LIJ(I,J),K)*DZBL(LIJ(I,J),K)
           ENDIF
        ELSE
           XBLFLUX(L,K)=0.0
        ENDIF
     ENDDO LCM_LOOP_3
     
     ! Y Bedload flux at J-1/2 interface
     LCM_LOOP_5:DO L=2,LA
        I=IL(L)
        J=JL(L)
        IF(VBL(L,K).GE.0) THEN
           YBLFLUX(L,K)=DT/(DYP(L)*100.0)*CBL(1,LSC(L),K)*VBL(LSC(L),K)*DZBL(LSC(L),K)
        ELSE
           YBLFLUX(L,K)=DT/(DYP(L)*100.0)*CBL(1,LIJ(I,J),K)*VBL(LIJ(I,J),K)*DZBL(LIJ(I,J),K)
        ENDIF
     ENDDO LCM_LOOP_5
     
     !**********************************************************************!
     !  Transport Equation Interior Elements
     ! 
     LCM_LOOP_4:DO L=2,LA
        I=IL(L)
        J=JL(L)
        IF(BLFLAG(L,K)==1) THEN
           IF(LIJ(I+1,J)>0) THEN
              CBL(2,L,K)=CBL(1,L,K)+(XBLFLUX(L,K)-XBLFLUX(LIJ(I+1,J),K)+YBLFLUX(L,K)-YBLFLUX(LNC(L),K)+QBSED(L,K))/DZBL(L,K)
           ELSE
              CBL(2,L,K)=CBL(1,L,K)+(YBLFLUX(L,K)-YBLFLUX(LNC(L),K)+QBSED(L,K))/DZBL(L,K)
           ENDIF
        ELSE
           CBL(2,L,K)=0.0 
        ENDIF
        IF(DZBL(L,K)==0.OR.CBL(2,L,K)<0)CBL(2,L,K)=0.0 
     ENDDO LCM_LOOP_4
  ENDDO SED_LOOP
  CBL(1,2:LA,1:NSCM)=CBL(2,2:LA,1:NSCM)
  
  !CQBEDLOADX(:,:)=XBLFLUX(:,:)
  !CQBEDLOADY(:,:)=YBLFLUX(:,:)
  DZBL_LAST(:,:)=DZBL(:,:)
  RETURN 
END SUBROUTINE BEDLOADJ
