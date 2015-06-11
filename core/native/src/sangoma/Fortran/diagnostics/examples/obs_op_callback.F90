! $Id: obs_op_callback.F90 305 2014-03-14 08:56:44Z larsnerger $
!BOP
!
! !Program: obs_op --- Example observation operator call-back routine
!
! !INTERFACE:
SUBROUTINE obs_op(step, dim,dim_obs, state, m_state)

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
  INTEGER, INTENT(in) :: dim            ! PE-local dimension
  INTEGER, INTENT(in) :: dim_obs        ! Dimension of observed state
  REAL, INTENT(in)    :: state(dim)     ! PE-local model state
  REAL, INTENT(out) :: m_state(dim_obs) ! PE-local observed state
!EOP


! *********************************************
! *** Perform application of observation    ***
! *** operator H on vector or matrix column ***
! *********************************************

! For this example:
! H =     0     1     0     0
!         0     0     0     1
!
  m_state(1) = state(2)
  m_state(2) = state(4)

END SUBROUTINE obs_op
