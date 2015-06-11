! Copyright (c) 2004-2012 Lars Nerger, lars.nerger@awi.de
!
! This routine is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, either version
! 3 of the License, or (at your option) any later version.
!
! This code is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this software.  If not, see <http://www.gnu.org/licenses/>.
!
!$Id: sangoma_ComputeHistogram.F90 605 2015-04-29 07:17:11Z larsnerger $
!BOP
!
! !ROUTINE: sangoma_ComputeHistogram --- Increment rank histogram
!
! !INTERFACE:
SUBROUTINE sangoma_computehistogram(ncall, dim, dim_ens, element, &
     state, ens, hist, delta, status) &
     BIND(C, name="sangoma_computehistogram_")

! !DESCRIPTION:
! This routine increments information on an ensemble rank histogram. 
! Inputs are the ensemble array and a state vector about which the histogram
! is computed. In addition, the index of the element has to be specified for
! which the histogram is computed. If this is 0, the histogram information
! is collected over all elements. Also, the value 'ncall' has to be set. It 
! gives the number of calls used to increment the histogram and is needed to
! compute the delta-measure that described the deviation from the ideal histogram.
!
! The input/output array 'hist' has to be allocated externally. In
! addition, it has to be initialized with zeros before the first call.

! !REVISION HISTORY:
! 2012-08 - Lars Nerger - Initial code for SANGOMA based on PDAF
! 2013-11 - L. Nerger - Adaption to SANGOMA data model
! 2014-02 - L. Nerger - Addition of delta measure
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in) :: ncall              ! Number of calls to routine
  INTEGER(INTPREC), INTENT(in) :: dim                ! State dimension
  INTEGER(INTPREC), INTENT(in) :: dim_ens            ! Ensemble size
  INTEGER(INTPREC), INTENT(in) :: element            ! Element of vector used for histogram
       ! If element=0, all elements are used
  REAL(REALPREC), INTENT(in)   :: state(dim)         ! State vector
  REAL(REALPREC), INTENT(in)   :: ens(dim, dim_ens)  ! State ensemble
  INTEGER(INTPREC), INTENT(inout) :: hist(dim_ens+1) ! Histogram about the state
  REAL(REALPREC), INTENT(out)     :: delta           ! deviation measure from flat histogram
  INTEGER(INTPREC), INTENT(out)   :: status          ! Status flag (0=success)
!EOP

! *** local variables ***
  INTEGER :: i, elem     ! Counters
  INTEGER :: rank        ! Rank of current ensemble


! ********************************
! *** Increment rank histogram ***
! ********************************


  IF (element > 0 .AND. element < dim) THEN

     ! Increment histogram for single element
     rank = 0
     DO i = 1, dim_ens
        IF (ens(element, i) < state(element)) rank = rank + 1
     END DO

     hist(rank+1) = hist(rank+1) + 1

     ! Set status flag for success
     status = 0

  ELSE IF (element == 0) THEN

     ! Increment histogram over all elements
     DO elem = 1, dim
        rank = 0
        DO i = 1, dim_ens
           IF (ens(elem,i) < state(elem)) rank = rank + 1
        END DO

        hist(rank+1) = hist(rank+1) + 1
     END DO

     ! Set status flag for success
     status = 0

  ELSE

     ! Choice of 'element' not valid
     status = 1

  END IF


! ***********************************************************
! *** Compute delta (deviation from idealy flat histogram ***
! ***********************************************************

  IF (element > 0 .AND. element < dim) THEN

     ! delta for histogram for single element
     delta = 0.0
     DO elem = 1, dim_ens+1
        delta = delta + (hist(elem) - REAL(ncall)/REAL(dim_ens+1))**2
     END DO
     delta = delta * REAL(dim_ens+1) / REAL(ncall) / REAL(dim_ens)

  ELSE IF (element == 0) THEN

     ! delta for histogram over all elements
     delta = 0.0
     DO elem = 1, dim_ens+1
        delta = delta + (hist(elem) - REAL(ncall*dim)/REAL(dim_ens+1))**2
     END DO
     delta = delta * REAL(dim_ens+1) / REAL(ncall) / REAL(dim_ens)

  ELSE

     ! Choice of 'element' not valid
     status = 1

  END IF


END SUBROUTINE sangoma_computehistogram
