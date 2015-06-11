! Copyright (c) 2013 Paul Kirchgessner, paul.kirchgessner@awi.de 
!
! This routine is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, either version
! 3 of the License, or (at your option) any later version.
!!
! You should have received a copy of the GNU Lesser General Public
! License along with this software.  If not, see <http://www.gnu.org/licenses/>.
!
!
!BOP
!
! !ROUTINE: sangoma_ComputeRE --- Compute relative entropy
!
! !INTERFACE:
SUBROUTINE sangoma_computeRE(dim_ens,post_w,prior_w,RE, status) &
      BIND(C, name="sangoma_computere_")

! !DESCRIPTION:
! Calculates relative entropy within a partical filter.
! To be used after weights have been updated by the observations.
!
! OUTPUT:       RE - scalar, relative entropy of posterior
!                               given the prior
!
! METHOD:   RE is given by integral(p(x|y)ln[p(x|y)/p(x)])dx
!           If the particle positions are unchanged during the
!           assimilation, and only the weights are updated, RE can be
!           approximated in terms of the relative weights.
!           RE approx. Sum(post_w*ln(post_w/prior_w))

! !REVISION HISTORY:
! 2014-03 - P. Kirchgessner - Initial Fortran code; based on
!                             the Matlab routine by Alison Fowler
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in) :: dim_ens          ! Ensemble size
  REAL(REALPREC), INTENT(in)   :: post_w(dim_ens)  ! Posterior weights
  REAL(REALPREC), INTENT(inout)   :: prior_w(dim_ens) ! Prior weights
  REAL(REALPREC), INTENT(out)  :: RE               ! Relative Entropy
  INTEGER(INTPREC), INTENT(out) :: status                  ! Status flag (0=success)
!EOP


! *** local variables ***
  INTEGER :: i            ! Counters
  REAL :: w_ratio         ! Temporary ratio of weights


 ! if weights are all zero, set to 1/N
 if (sum(prior_w)== 0) then
      prior_w = 1./dim_ens
 endif  


 ! Initialize RE
  RE= 0


 do i= 1, dim_ens
       w_ratio = post_w(i)/prior_w(i);
       if ( w_ratio> 0 ) then
           RE = RE + post_w(i)*LOG(w_ratio)
       endif
 enddo

 status = 0


END SUBROUTINE sangoma_computeRE
