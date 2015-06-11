! Copyright (c) 2015 Jean-Michel Brankart jean-michel.brankart@legi.grenoble-inp.fr
!
! This routine is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, either version
! 3 of the License, or (at your option) any later version.
!
! It is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this software.  If not, see <http://www.gnu.org/licenses/>.
!
!$Id: sangoma_ComputeQuantiles.F90 553 2015-03-03 16:23:51Z larsnerger $
!BOP
!
! !ROUTINE: sangoma_ComputeQuantiles --- Compute ensemble quantiles
!
! !INTERFACE:
SUBROUTINE sangoma_computequantiles(dim, dim_ens, dim_qua, &
     qua_def, ens, qua, CB_SORT) &
     BIND(C, name="sangoma_computequantiles_")

! !DESCRIPTION:
! This routine computes the quantiles of an ensemble.
! Inputs are the ensemble array and the quantile definition (between 0 and 1)
! For instance: 0    -> minimum of the ensemble
!               0.25 -> first quartile
!               0.5  -> median
!               0.75 -> last quartile
!               1    -> maximum of the ensemble
!
! WARNING: This routine provides a fast computation of ensemble quantiles
!          which are appropriate to define approximate anamorphosis transformations
!          (by remapping them on the quantiles of a target distribution).
!          They can only be considered as a rough (slightly biased) estimate
!          of the quantiles of the underlying distribution.
!          The reason is that the (small) probability of being
!          outside the range of the ensemble range is here neglected. 
!
! !REVISION HISTORY:
! 2015-02 - J.-M. Brankart - Initial SANGOMA code
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in)  :: dim               ! Vector (state or obs) dimension
  INTEGER(INTPREC), INTENT(in)  :: dim_ens           ! Ensemble size
  INTEGER(INTPREC), INTENT(in)  :: dim_qua           ! Number of quantiles
  REAL(REALPREC), INTENT(in)    :: qua_def(dim_qua)  ! Quantile definition
  REAL(REALPREC), INTENT(in)    :: ens(dim, dim_ens) ! Ensemble
  REAL(REALPREC), INTENT(out)   :: qua(dim, dim_qua) ! Ensemble quantiles

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

! *** local variables ***
  INTEGER(INTPREC) :: i, j, jj    ! Counters
  REAL(REALPREC) :: ww            ! Interpolation weight
  REAL(REALPREC), ALLOCATABLE, DIMENSION(:) :: ens1  ! Ensemble for 1 variable
  REAL(REALPREC), ALLOCATABLE, DIMENSION(:) :: qua1  ! Quantiles (between 1 and dim_ens)

! *************************
! *** Compute quantiles ***
! *************************

! Allocate temporary arrays
  ALLOCATE(ens1(dim_ens))
  ALLOCATE(qua1(dim_qua))

! Rescale quantiles between 1 and dim_ens
  qua1(1:dim_qua)=1+qua_def(1:dim_qua)*(dim_ens-1)

! Loop on the variables (state or observation)
  DO i=1, dim

     ! Sort ensemble
     ens1(1:dim_ens)=ens(i,1:dim_ens)
     CALL CB_SORT(dim_ens,ens1)

     ! Loop on required quantiles
     DO j=1, dim_qua

       ! Compute interpolation weight
       jj = INT(qua1(j))
       ww = qua1(j) - jj

       ! Compute quantile
       qua(i,j) = (1-ww) * ens1(jj) + ww * ens1(jj+1)

     ENDDO

  ENDDO

  DEALLOCATE(ens1,qua1)

END SUBROUTINE sangoma_computequantiles
