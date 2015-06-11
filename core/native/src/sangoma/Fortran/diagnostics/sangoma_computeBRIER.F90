!=====================================================================

SUBROUTINE sangoma_computeBRIER(m, nens, ens, ana, xth, &
     br, brc, brv, unc, pc, s, sunc, pp, g, pr) &
     BIND(C, name="sangoma_computebrier_")
!
!=====================================================================
!
!    This subroutine computes scores related to 1 binary event associated 
!    with 1 threshold vector xth (externally defined by the user).
!    First, 'predicted' probabilities of occurrence of the event are computed
!            with their distribution and their associated 'predictable' probabilities
!            (these can be used to draw reliability and sharpness diagrams).
!    Then, Brier (skill) score and partition, and entropy, are computed:
!
!    B = E[(p-p'])^2] - E[(p'-pc])^2] + pc(1-pc) and BS = 1 - B/pc(1-pc)
!
!    S = -E[p'log(p')]
!
!    (see details in deliverable 4.1).
!
!    (last modification G. Candille jan 2014)
!
!
!    Input:  m                   size of verification set
!            nens                size of each ensemble
!            ens(m,nens)         ensemble
!            ana(m)              analysis or observation (verification)
!            xth(m)              threshold
!
!
!    Output: br                  Brier global skill score
!            brc                 reliability component
!            brv                 resolution component
!            unc                 uncertainty
!            pc                  'climatological' probability
!            s                   entropy
!            sunc                'climatological' entropy
!            pp(nens+1)          predicted probabilities
!            g(nens+1)           distribution of pp
!            pr(nens+1)          predictable probabilities
!
!
!=====================================================================
!   
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC
  IMPLICIT NONE 

!     ARGUMENTS
  INTEGER(INTPREC), INTENT(in)    :: m
  INTEGER(INTPREC), INTENT(in)    :: nens
  REAL(REALPREC), INTENT(in)      :: ens(m,nens)
  REAL(REALPREC), INTENT(in)      :: ana(m)
  REAL(REALPREC), INTENT(in)      :: xth(m)
  REAL(REALPREC), INTENT(inout)   :: br
  REAL(REALPREC), INTENT(inout)   :: brc
  REAL(REALPREC), INTENT(inout)   :: brv
  REAL(REALPREC), INTENT(inout)   :: unc
  REAL(REALPREC), INTENT(inout)   :: pc
  REAL(REALPREC), INTENT(inout)   :: s
  REAL(REALPREC), INTENT(inout)   :: sunc
  REAL(REALPREC), INTENT(inout)   :: pp(nens+1)
  REAL(REALPREC), INTENT(inout)   :: g(nens+1)
  REAL(REALPREC), INTENT(inout)   :: pr(nens+1)

!     Local variables
  INTEGER :: i, k
  REAL    :: p(m), o(m)
  REAL    :: xk, dc

  br=0.0 ; brc=0.0 ; brv=0.0 ; pc=0.0 ; unc=0.0
  s=0.0 ; sunc=0.0 ; o = 0.0 ; pr = 0.0

!     define probabilities and observed occurrences
  DO k=1,m
     xk=0.0
     DO i=1,nens
        IF (ens(k,i) < xth(k)) xk=xk+1.0
     ENDDO
     p(k)=xk/REAL(nens)
     IF (ana(k) < xth(k)) o(k)=1.0
  ENDDO

!     uncertainty
  DO k=1,m
     pc=pc+o(k)
  ENDDO
  pc=pc/REAL(m)
  unc=pc*(1.0-pc)

!     predictable probabilities
  DO i=1,nens+1
     g(i)=0.0
     dc=REAL(i-1)
     pp(i)=dc/REAL(nens)
     DO k=1,m
        IF (p(k) == pp(i)) THEN
           g(i)=g(i)+1.
           pr(i)=pr(i)+o(k)
        ENDIF
     ENDDO
     IF (g(i) /= 0.0) THEN
        pr(i)=pr(i)/g(i)
     ELSE
        pr(i)=0.0
     ENDIF
     g(i)=g(i)/REAL(m)
  ENDDO

!     Brier skill score and decomposition
  DO i=1,nens+1
     brc=brc+g(i)*(pp(i)-pr(i))**2
     brv=brv+g(i)*(pr(i)-pc)**2
  ENDDO
  brc=brc/unc
  brv=1.0-brv/unc
  br=1.0-(brc+brv)

!     Entropy
  DO i=1,nens+1
     IF (pr(i) > 0.0) s=s-g(i)*pr(i)*LOG(pr(i))
  ENDDO
  sunc=-pc*LOG(pc)

END SUBROUTINE sangoma_computeBRIER
