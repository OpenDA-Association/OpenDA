! Copyright (c) 2004-2015 Lars Nerger, lars.nerger@awi.de
!
! This routine is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, either version
! 3 of the License, or (at your option) any later version.
!
! This tool is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this software.  If not, see <http://www.gnu.org/licenses/>.
!
!$Id: sangoma_ComputeHistogram.F90 287 2014-02-07 11:12:23Z larsnerger $
!BOP
!
! !ROUTINE: sangoma_CheckEnsSpread --- Check whether ensemble spread is realistic
!
! !INTERFACE:
SUBROUTINE sangoma_checkensspread(dim, dim_ens, state, ens, std_ens, std_state) &
     BIND(C, name="sangoma_checkensspread_")

! !DESCRIPTION:
! This routine computes the ensemble spread (standard deviation)
! and the RMS deviation of the provided state (representing the
! truth) from the ensemble mean state.
! Both values should be similar for a representative ensemble.

! !REVISION HISTORY:
! 2015-04 - Lars Nerger - Initial code
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in) :: dim           ! state dimension
  INTEGER(INTPREC), INTENT(in) :: dim_ens       ! ensemble size
  REAL(REALPREC), INTENT(in) :: state(dim)      ! state vector (the 'truth')
  REAL(REALPREC), INTENT(in) :: ens(dim, dim_ens) ! Ensemble array
  REAL(REALPREC), INTENT(out) :: std_ens        ! standard deviation of ensemble spread
  REAL(REALPREC), INTENT(out) :: std_state      ! RMS deviation of mean state from input state

! *** local variables ***
  INTEGER :: i, member                          ! Counters
  REAL :: variance                              ! Temporary variable
  REAL(REALPREC), ALLOCATABLE :: meanstate(:)   ! ensemble mean state


! ***********************************
! *** Compute ensemble mean state ***
! ***********************************
 
  ALLOCATE(meanstate(dim))

  ! local 
  meanstate = 0.0
  DO member = 1, dim_ens
     DO i = 1, dim
        meanstate(i) = meanstate(i) + ens(i, member)
     END DO
  END DO
  meanstate(:) = meanstate(:) / REAL(dim_ens)


! *******************************************
! *** Compute ensemble standard deviation ***
! *******************************************

  std_ens = 0.0
  DO i = 1, dim
     variance = 0.0
     DO member = 1, dim_ens
        variance = variance &
             + (ens(i, member) - meanstate(i)) &
             * (ens(i, member) - meanstate(i))
     END DO

     std_ens = std_ens + variance / REAL(dim-1)
     
  END DO
  std_ens = SQRT(std_ens / REAL(dim))


! ***************************************************************
! *** Compute RMS deviation of ensemble mean from input state ***
! ***************************************************************

  std_state = 0.0
  DO i = 1, dim
     std_state = std_state &
             + (state(i) - meanstate(i)) * (state(i) - meanstate(i))
  END DO
  std_state = SQRT(std_state / REAL(dim))


! *** Clean up ***

  DEALLOCATE(meanstate)

END SUBROUTINE sangoma_CheckEnsSpread
