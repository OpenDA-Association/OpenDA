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
!$Id: sangoma_ComputeSensitivity.F90 287 2014-02-07 11:12:23Z larsnerger $
!BOP
!
! !ROUTINE: sangoma_ComputeSensitivity --- Compute sensitivity
!
! !INTERFACE:
SUBROUTINE sangoma_computeSensitivity(dim, dim_ens,dim_obs, &
     obs_cov, H, ens, post_w,sensitivity, post_cov_mat, status) &
      BIND(C, name="sangoma_computesensitivity_")

! !DESCRIPTION:
! Calculates sensitivity of the posterior mean to the observations 
! (assuming Gaussian observation error and linear observation operator) 
! within a partical filter.
! To be used after weights have been updated by the observations.
!
! The sensitivity of the analysis to the observations can be
! calculated exactly as the ratio of the posterior variance in 
! observation space to the observation error covariance. See
! Fowler and van Leeuwen (Tellus 64A (2012) 17192).
!
! !REVISION HISTORY:
! 2013-12 - P. Kirchgessner - Initial Fortran code; based on
!                             the Matlab routine by Alison Fowler
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in) :: dim                      ! State dimension
  INTEGER(INTPREC), INTENT(in) :: dim_ens                  ! Ensemble size
  INTEGER(INTPREC), INTENT(in) :: dim_obs                  ! Number of observations
  REAL(REALPREC), INTENT(in)   :: obs_cov(dim_obs,dim_obs) ! Covariance matrix of observations
  REAL(REALPREC), INTENT(in)   :: H(dim_obs, dim)          ! Linear observation operator
  REAL(REALPREC), INTENT(in)   ::  ens(dim, dim_ens)       ! ensemble of particles
  REAL(REALPREC), INTENT(in)   :: post_w(dim_ens)          ! Posterior weights
                                     !If no weights are given, initialize with 1/dim_ens
  REAL(REALPREC), INTENT(out)  :: sensitivity(dim_obs,dim_obs)  ! Sensitivity array
  REAL(REALPREC), INTENT(out)  :: post_cov_mat(dim,dim)    ! Posterior covariance matrix
  INTEGER(INTPREC), INTENT(out) :: status                  ! Status flag (0=success)
!EOP


! *** local variables ***
  INTEGER :: i,j,k                     ! Counters
  REAL :: post_mean(dim)               ! Posterior mean
  INTEGER :: ipiv(dim_obs)             ! Help variable for LU decomposition
  INTEGER :: work 
  REAL :: lwork(100*dim_obs)
  REAL :: tmp_mat(dim_obs,dim)
  REAL :: ens_m(dim, dim_ens)          ! mean of ensemble of particles
  REAL :: tmp_obs_cov(dim_obs,dim_obs)

 
! **********************************
! *** Compute the posterior mean ***
! **********************************

  post_mean = 0 ! Initialise with zeros
  DO i = 1,dim_ens
     post_mean = post_mean + (post_w(i) * ens(:,i))
  END DO

  ! subtract the mean from all ensemble members (store back in ensemble)
  DO i = 1,dim_ens
     ens_m(:,i) = ens(:,i) - post_mean
  END DO
  post_cov_mat = 0 ! Initialise cov. matrix with zeros

  ! calculate the posterior cov mat.
  DO i = 1,dim_ens
     DO j=1,dim
        DO k=1,dim
           post_cov_mat(j,k) =post_cov_mat(j,k) + post_w(i)*ens_m(j,i)*ens_m(k,i)
        END DO
     END DO
  END DO

  ! Calculate tmp_mat = H*P_a
  CALL dgemm('N', 'N', dim_obs, dim, dim, 1.0, H, &
       dim_obs, post_cov_mat, dim, 0.0, tmp_mat, dim_obs)

  ! Calculate S = tmp_mat*Hi^T
  CALL dgemm('N', 'T', dim_obs, dim_obs ,dim, 1.0, tmp_mat, &
       dim_obs, H, dim_obs, 0.0, sensitivity, dim_obs)

  ! Copy cov_matrix so it does not change using dgesv
  tmp_obs_cov = obs_cov

  ! compute sensitivity = sensitivity* R^(-1)
  CALL dgesv(dim_obs, dim_obs, tmp_obs_cov, dim_obs, ipiv, &
       sensitivity, dim_obs, status)

  IF (.NOT. (status == 0)) THEN
     WRITE(*,*) 'Error in calculation of sensitivity'
  ENDIF

END SUBROUTINE sangoma_computeSensitivity
