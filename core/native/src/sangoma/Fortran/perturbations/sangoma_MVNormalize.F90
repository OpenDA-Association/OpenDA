! Copyright (c) 2004-2012 Lars Nerger, lars.nerger@awi.de
!
! This routine is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, either version
! 3 of the License, or (at your option) any later version.
!
! PDAF is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this software.  If not, see <http://www.gnu.org/licenses/>.
!
!$Id: sangoma_MVNormalize.F90 305 2014-03-14 08:56:44Z larsnerger $
!BOP
!
! !ROUTINE: sangoma_MVNormalize --- Perform multivariate normalization
!
! !INTERFACE:
SUBROUTINE sangoma_mvnormalize(mode, dim_state, dim_field, offset, &
     ncol, states, stddev, status) &
     BIND(C, name="sangoma_mvnormalize_")

! !DESCRIPTION:
! This routine performs multivariate normalization and re-scaling.
! It has two modes:
!
! mode=1: 
! In this case, the routine computes the standard deviation of a field
! inside the array 'states' holding in each column a state vector. The standard
! deviation is computed over all columns if the state vector array. Then, the
! field is normalized for unit standard deviation by dividing the values by
! the standard deviation. The standard deviation is provided on output
! together with the scaled array 'states'
!
! mode=2:
! In this case the input variable 'stddev' is used to rescale the
! corresponding part of the array 'states'. Usually 'stddev' is obtained by
! a call with mode=1 before. 
!
! !REVISION HISTORY:
! 2012-09 - Lars Nerger - Initial code for SANGOMA based on PDAF
! 2013-11 - L. Nerger - Adaption to SANGOMA data model
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in) :: mode       ! Mode: (1) normalize, (2) re-scale
  INTEGER(INTPREC), INTENT(in) :: dim_state  ! Dimension of state vector
  INTEGER(INTPREC), INTENT(in) :: dim_field  ! Dimension of a field in state vector
  INTEGER(INTPREC), INTENT(in) :: offset     ! Offset of field in state vector
  INTEGER(INTPREC), intent(in) :: ncol       ! Number of columns in array states
  REAL(REALPREC), INTENT(inout) :: states(dim_state, ncol)  ! State vector array
  REAL(REALPREC), INTENT(inout) :: stddev    ! Standard deviation of field
     ! stddev is output for mode=1 and input for mode=2
  INTEGER(INTPREC), INTENT(out) :: status    ! Status flag (0=success)
!EOP

! *** local variables ***
  INTEGER :: i, j       ! Counters


  modes: IF (mode == 1) THEN

! *****************************
! *** Perform normalization ***
! *****************************

     ! *** Compute STDDEV of field ***
     stddev = 0.0

     DO j = 1, ncol
        DO i = 1, dim_field
           stddev = stddev + states(i+offset, j)**2
        END DO
     END DO

     stddev = SQRT(stddev / REAL(ncol * dim_field))

     ! *** Normalize field ***
     DO j = 1, ncol
        DO i = 1, dim_field
           states(i+offset, j) = states(i+offset, j) / stddev
        END DO
     END DO

     ! Set status flag for success
     status = 0

  ELSE IF (mode == 2) THEN modes

! **************************
! *** Perform re-scaling ***
! **************************

     DO j = 1, ncol
        DO i = 1, dim_field
           states(i+offset, j) = states(i+offset, j) * stddev
        END DO
     END DO

     ! Set status flag for success
     status = 0

  ELSE modes

     ! invalid 'mode'
     status = 1

  END IF modes


END SUBROUTINE sangoma_MVNormalize
