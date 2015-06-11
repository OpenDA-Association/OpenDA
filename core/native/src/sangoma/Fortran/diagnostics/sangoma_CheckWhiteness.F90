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
! !ROUTINE: sangoma_CheckWhiteness --- Test for whiteness of innovations
!
! !INTERFACE:
SUBROUTINE sangoma_checkwhiteness(dim, dimt, inno, Qval, pval) &
     BIND(C, name="sangoma_checkwhiteness_")

! !DESCRIPTION:
! This routines checks whether the a set of innovations is white in time.
! The innovations are stored as columns of the array 'inno'. The check
! is done using the Ljung-Box test.

! !REVISION HISTORY:
! 2015-04 - Lars Nerger - Initial code
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in) :: dim           ! state dimension
  INTEGER(INTPREC), INTENT(in) :: dimt          ! number innovation times
  REAL(REALPREC), INTENT(in) :: inno(dim, dimt) ! array of innovations
  REAL(REALPREC), INTENT(out) :: Qval           ! Q-value of Ljung-Box test
  REAL(REALPREC), INTENT(out) :: Pval           ! P-value of significance

! *** local variables ***
  INTEGER :: row, col, lag                      ! Counters
  REAL(REALPREC), ALLOCATABLE :: stds(:)        ! standard deviation for each lag
  REAL(REALPREC), ALLOCATABLE :: means(:)       ! vector of mean innovations
  REAL(REALPREC) :: rho                         ! correlation of a pair of innovations
  REAL(REALPREC) :: qchi                        ! q-value of cumulative chi-square function


! **************************************************************
! *** Compute Ljung-Box test value                           ***
! *** Q = dimt(dimt+2) sum_k=1,dim_t [ ( rho_k^2 / dimt-k )] ***
! **************************************************************
 
  ! *** Compute mean and standard deviation for each column of 'inno'

  ALLOCATE(stds(dimt))
  ALLOCATE(means(dimt))
  stds = 0.0
  means = 0.0

  DO col = 1, dimt
     DO row = 1, dim
        means(col) = means(col) + inno(row,col)
     END DO
     means(col) = means(col) / dim
  END DO

  DO col = 1, dimt
     DO row = 1, dim
        stds(col) = stds(col) + (inno(row, col)-means(col))*(inno(row, col)-means(col))
     END DO
     stds(col) = SQRT(1.0/REAL(dim-1) * stds(col))
  END DO


  ! *** Compute Q-value of Ljung-Box test

  Qval = 0.0
  DO lag = 1, dimt-1

     ! Compute rho
     rho = 0.0
     DO row = 1, dim
        rho = rho + ((inno(row, 1)-means(1)) / stds(1) &
             * (inno(row, lag+1)-means(lag+1)) / stds(lag+1))
     END DO
     rho = rho / REAL(dim-1)

     ! Increment Qval
     Qval = Qval + rho**2 / REAL(dimt-lag)

  END DO

  ! Finaliza computation of Qval
  Qval = Qval * REAL(dimt * (dimt+2))


! **********************************************************************
! *** Compute significance according to cumulate chi-square function ***
! **********************************************************************

  ! Call the routine for cumilative chi-square function from CDFLIB
  call cumchi(Qval, REAL(dimt), pval, qchi)


! *** Clean up ***

  DEALLOCATE(stds, means)

END SUBROUTINE sangoma_checkwhiteness
