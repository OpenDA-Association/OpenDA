!=====================================================================
!
      SUBROUTINE sangoma_computeCRPS(ENS,ANA,MISSING,NCASES,NENS,CRPS, &
           RELI,RESOL,UNCERT,BB,AA, &
           CB_SORT, CB_SORT2) &
           BIND(C, name="sangoma_computecrps_")
!
!=====================================================================
!
!     Exact evaluation of the Continuous Ranked Probability Score,
!     and its decomposition:
!                               CRPS = RELI + RESOL
!
!     See H. Hersbach (2000) Wea. Forecasting, Vol 15, pp 559-570
!     (note: RESOL here is equivalent to CRPS_pot = UNCERT - RESOL in Hersbach 2000)
!
!     modification by P. Houtekamer and G. Candille on December 2004
!     for missing values
!     (last modifications: G. Candille dec 2013)
!
!     CRPS : global score evaluating both reliability (RELI) and resolution (RESOL) of the system
!            A perfectly reliable system gives RELI = 0
!            An informative system gives RESOL << UNCERT
!            (UNCERT is the value of the score based on the verification sample only)
!
!
!     Input:   NCASES                  Size of verification set
!              NENS                    Size of each ensemble
!              ENS(NCASES,NENS)        Ensemble of anomalies
!              ANA(NCASES)             Analysis of anomaly
!              missing(ncases)         0 : the observation is OK
!                                      1 : the observation should
!                                          not be used
!
!     Output:  CRPS, RELI            
!              UNCERT,RESOL            CRPS & decopomsition Based on sample set of ANA
!              BB(0:NENS), AA(0:NENS)  Detailed info on CRPS
!
!=====================================================================
!

!     USE
      USE, INTRINSIC :: ISO_C_BINDING
      USE sangoma_base, ONLY: REALPREC, INTPREC

      IMPLICIT NONE

!     ARGUMENTS
      INTEGER(INTPREC), INTENT(in) :: NCASES
      INTEGER(INTPREC), INTENT(in) :: NENS
      REAL(REALPREC), INTENT(in)   :: ENS(NCASES,NENS)
      REAL(REALPREC), INTENT(in)   :: ANA(NCASES)
      INTEGER(INTPREC), INTENT(in) :: missing(NCASES)
      REAL(REALPREC), INTENT(inout)   :: CRPS
      REAL(REALPREC), INTENT(inout)   :: RELI
      REAL(REALPREC), INTENT(inout)   :: RESOL
      REAL(REALPREC), INTENT(inout)   :: UNCERT
      REAL(REALPREC), INTENT(inout)   :: BB(0:NENS)
      REAL(REALPREC), INTENT(inout)   :: AA(0:NENS)

!     External routines for sorting
!     (use your own external routines, e.g. sort routines from Numerical Recipes) 
!     CB_SORT(N,V) 
!          N = size of the sub-vector to be sorted ; 
!          V = input vector is replaced by its sorted vector
      INTERFACE
         SUBROUTINE CB_SORT(NENS, V) BIND(C)
           USE sangoma_base, ONLY: REALPREC, INTPREC
           INTEGER(INTPREC),INTENT(in)  :: NENS    ! Size of vector
           REAL(REALPREC),INTENT(inout) :: V(NENS) ! Input/output vector 
         END SUBROUTINE CB_SORT
      END INTERFACE

!     CB_SORT2(N,V1,V2) : same as CB_SORT with V2 rearranged as V1 is sorted
      INTERFACE
         SUBROUTINE CB_SORT2(NENS, V1, V2) BIND(C)
           USE sangoma_base, ONLY: REALPREC, INTPREC
           INTEGER(INTPREC),INTENT(in)  :: NENS    ! Size of vector
           REAL(REALPREC),INTENT(inout) :: V1(NENS) ! Input/output vector 
           REAL(REALPREC),INTENT(inout) :: V2(NENS) ! 2nd Input/output vector 
         END SUBROUTINE CB_SORT2
      END INTERFACE

!     Local variables
      INTEGER I,ICASE,ngood 
      REAL WW(NCASES)
      REAL GI, OI, P
      REAL A, W
      REAL e(NENS),acli(NCASES),wcli(NCASES)

!---------------------------------------------------------------------
!
!  1. Initialize
      DO I=0,NENS
         BB(I)=0. ; AA(I)=0.
      ENDDO

      CRPS=0.  ; RELI=0.  ; RESOL=0. ; UNCERT=0.

!     re-initialize weight depending on the missing
      ngood=0
      DO ICASE=1,NCASES
         IF (missing(icase).EQ.0) ngood=ngood+1
      ENDDO
      IF (ngood .NE. 0) THEN
         DO ICASE=1,NCASES
            ww(icase)=1./REAL(ngood)
         ENDDO

         ngood=0
!     2. Loop over cases, determine relevant quantities
         DO ICASE=1,NCASES
            IF (missing(icase).EQ.0) THEN
               ngood=ngood+1
               W=WW (ICASE)
               A=ANA(ICASE)
               DO I=1,NENS
                  E(I)=ENS(ICASE,I)
               ENDDO
               CALL CB_SORT(NENS,E)     
               IF(A.LT.E(1)) AA(0)=AA(0)+W*(E(1)-A)
               IF(A.LT.E(1)) BB(0)=BB(0)+W
               DO I=1,NENS-1
                  BB(I)=BB(I)+W*MAX(MIN(A,E(I+1))-E(I),0.)
                  AA(I)=AA(I)+W*MAX(E(I+1)-MAX(A,E(I)),0.)
               ENDDO
               IF(A.GT.E(NENS)) BB(NENS)=BB(NENS)+W*(A-E(NENS))
               IF(A.GT.E(NENS)) AA(NENS)=AA(NENS)+W

!     Update climate set
               ACLI(ngood)=A
               WCLI(ngood)=W
            ENDIF
         ENDDO

!     3. Reinterpretation of BB(0) and AA(NENS)
         IF(BB(0   ).NE.0.) BB(0   )=AA(0   )*(1./BB(0   )-1.)
         IF(AA(NENS).NE.0.) AA(NENS)=BB(NENS)*(1./AA(NENS)-1.)
!     (Note that these do not contribute to the CRPS)

!     4. Calculate uncertainty
         CALL CB_SORT2(ngood,ACLI,WCLI)
         UNCERT=0.
         P=0.
         DO ICASE=1,ngood-1
           P=P+WCLI(ICASE)
           UNCERT=UNCERT+(ACLI(ICASE+1)-ACLI(ICASE))*P*(1.-P)
         ENDDO

!     5. Compute CRPS, reliability and resolution
         CRPS=0.  ; RELI=0.  ; RESOL=0.

         DO I=0,NENS
            GI=BB(I)+AA(I)
            IF(GI.NE.0.)OI=AA(I)/GI
            P=(I+0.)/NENS

            CRPS = CRPS+BB(I)*P*P+AA(I)*(1.-P)*(1.-P)
            RELI = RELI + GI*(OI-P)**2.
            RESOL= RESOL+ GI*OI*(1.-OI)
         ENDDO

      ELSE
         CRPS=0.  ; RELI=0.  ; RESOL=0. ; UNCERT=0.
      ENDIF

    END SUBROUTINE sangoma_computecrps
