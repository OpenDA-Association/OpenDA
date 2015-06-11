! Copyright (c) 2014 Paul Kirchgessner, paul.kirchgessner@awi.de
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
!$Id: sangoma_ComputeHistogram.F90 287 2014-02-07 11:12:23Z larsnerger $
!BOP
!
! !ROUTINE: sangoma_ComputeHistogram --- Increment rank histogram
!
! !INTERFACE:
SUBROUTINE sangoma_computeESS(dim_sample,weights, effSample, status)  &
    BIND(C, name="sangoma_computeess_")

! !DESCRIPTION
! This routine computes the effective sample size of a particle
! filter as defined in Doucet et al. 2001 p. 333 


! !REVISION HISTORY:
! 2014-06 - Paul Kirchgessner - Initial code for SANGOMA 
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in) :: dim_sample          ! Sample size

  REAL(REALPREC), INTENT(in)   :: weights(dim_sample) ! weights of the samples
  REAL(REALPREC), INTENT(out)  :: effsample           ! effecfive sample size
  INTEGER(INTPREC), INTENT(out)   :: status          ! Status flag (0=success)
!EOP

! *** local variables ***
  INTEGER :: i     ! Counters

  IF (dim_sample < 1 ) THEN
     WRITE(*,*) 'Dimension of Sample size invalid!'
     status = 0
  ENDIF

  effsample = 0
  DO i = 1, dim_sample
     effsample = effsample + weights(i)**2
  ENDDO
  
  effsample = 1./effsample



END SUBROUTINE sangoma_computeESS
