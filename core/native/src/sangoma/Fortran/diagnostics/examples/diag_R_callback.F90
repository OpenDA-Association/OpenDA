! $Id: obs_op_callback.F90 305 2014-03-14 08:56:44Z larsnerger $
!BOP
!
! !Program: obs_op --- Example call-back routine for providing diagonal of R
!
! !INTERFACE:
SUBROUTINE diag_R(step, dim_obs, R)

! !DESCRIPTION:
! This is a simple example for a call-back routine
! implementing an observation operator. The
! observation operator acts on the input state vector
! 'state' and returns the observed state 'm_state'.
!
! !REVISION HISTORY:
! 2014-01 - P. Kirchgessner - Initial code

! !USES:
  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER, INTENT(in) :: step           ! Currrent time step
  INTEGER, INTENT(in) :: dim_obs        ! Dimension of observed state
  REAL, INTENT(out) :: R(dim_obs) ! PE-local observed state
!EOP
! Local variables
  INTEGER :: i
! *********************************************
! *** Provide variances of the observations ***
! *********************************************
  do i =1,dim_obs 
     R(i) = 0.1
  enddo
END SUBROUTINE diag_R
