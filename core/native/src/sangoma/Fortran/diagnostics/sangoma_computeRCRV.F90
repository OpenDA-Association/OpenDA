!=====================================================================

      SUBROUTINE sangoma_computeRCRV(ens,ana,sig0,missing,m,nens,b,d) &
      BIND(C, name="sangoma_computercrv_")
!
!=====================================================================
!
!    Mean and Standard Deviation of the variable:
!
!             y=(o-xm)/SQRT(xs^2+so^2)
!
!    WITH     o: obs, xm: ens. mean, xs: ens. spread, so: obs. error
!
!    (last modifications: G. Candille, jan 2014)
!     
!    Check the indistinguishability between the observations and the ensemble members : reliability.
!    Mean of y : b = weighted bias of the ensembles
!    S-D  of y : d = agreement between the mean error and the ensemble SPREAD (+ obs. error)
!    
!    A perfectly reliable system gives : b = 0 and d = 1
!
!
!     Input:   m                       Size of verification set
!              nens                    Size of each ensemble
!              ens(m,nens)             Ensemble of anomalies
!              ana(m)                  Analysis (or observation) of anomaly
!              sig0                    obs. error
!              missing(m)              0 : the observation is OK
!                                      1 : the observation should
!                                          not be used
!
!     Output:  b & d                 mean & std of y
!
!=====================================================================
!   
      USE, INTRINSIC :: ISO_C_BINDING
      USE sangoma_base, ONLY: REALPREC, INTPREC

      IMPLICIT NONE 

!     ARGUMENTS
      INTEGER(INTPREC), INTENT(in) :: m
      INTEGER(INTPREC), INTENT(in) :: nens
      REAL(REALPREC), INTENT(in)   :: ens(m,nens)
      REAL(REALPREC), INTENT(in)   :: ana(m)
      REAL(REALPREC), INTENT(in)   :: sig0
      INTEGER(INTPREC), INTENT(in) :: missing(m)
      REAL(REALPREC), INTENT(inout)   :: b
      REAL(REALPREC), INTENT(inout)   :: d

!     Local variables
      INTEGER ngood, k, i
      REAL ww(m), xmk, xsk
      REAL y(m)

      b=0. ; d=0.

!     initialize missing
      ngood=0
      DO k=1,m
         IF (missing(k).EQ.0) ngood=ngood+1
      ENDDO

      IF (ngood .GT. 1) THEN
         DO k=1,m
            ww(k)=1.0/REAL(ngood)
         ENDDO
!     define y (RCRV) and compute b (bias)
         b=0.
         DO k=1,m
            IF (missing(k).EQ.0) THEN
!              xmk & xsk : ensemble mean & ensemble spread
               xmk=0.
               xsk=0.
               DO i=1,nens
                  xmk=xmk+ens(k,i)
               ENDDO
               xmk=xmk/REAL(nens)
               DO i=1,nens
                  xsk=xsk+(xmk-ens(k,i))**2
               ENDDO
               xsk=SQRT(xsk/REAL(nens-1))
               IF (xsk**2+sig0**2 .EQ. 0.) THEN
                  y(k)=ana(k)-xmk
               ELSE
                  y(k)=(ana(k)-xmk)/SQRT(xsk**2+sig0**2)
               ENDIF
               b=b+y(k)*ww(k)
            ENDIF
         ENDDO
!     compute d (dispersion)
         d=0.
         DO k=1,m
            IF (missing(k).EQ.0) d=d+ww(k)*(b-y(k))**2
         ENDDO
         d=SQRT(REAL(ngood)*d/REAL(ngood-1))
      ELSE
         b=0. ; d=-2.
      ENDIF

      RETURN
    END SUBROUTINE sangoma_computeRCRV
