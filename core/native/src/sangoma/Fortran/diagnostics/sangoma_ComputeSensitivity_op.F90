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
!$Id: sangoma_ComputeSensitivity_op.F90 289 2014-02-10 18:09:58Z larsnerger $
!BOP
!
! !ROUTINE: sangoma_ComputeSensitivity_op --- Compute sensitivity
!
! !INTERFACE:
SUBROUTINE sangoma_computeSensitivity_op(dim, dim_ens, dim_obs, &
     obs_cov, ens, post_w, sensitivity, post_cov_mat, CB_obs_op, status) &
     BIND(C, name="sangoma_computesensitivity_op_")

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
  REAL(REALPREC), INTENT(in)   ::  ens(dim, dim_ens)       ! ensemble of particles
  REAL(REALPREC), INTENT(in)   :: post_w(dim_ens)          ! Posterior weights
  !If no weights are given, initialize with 1/dim_ens
  REAL(REALPREC), INTENT(out)  :: sensitivity(dim_obs,dim_obs) ! Sensitivity array
  REAL(REALPREC), INTENT(out)  :: post_cov_mat(dim,dim)    ! Posterior covariance matrix
  INTEGER(INTPREC), INTENT(out) :: status                  ! Status flag (0=success)
  
  ! Call-back routine provinding observation operator
  INTERFACE
     SUBROUTINE CB_obs_op(step, dim, dim_obs, state, Hstate) BIND(C)
        USE sangoma_base, ONLY: REALPREC, INTPREC
        INTEGER(INTPREC),INTENT(in) :: step           ! Time step
        INTEGER(INTPREC),INTENT(in) :: dim            ! State dimension
        INTEGER(INTPREC),INTENT(in) :: dim_obs        ! Number of observations
        REAL(REALPREC),INTENT(in)  :: state(dim)      ! State ensemble
        REAL(REALPREC),INTENT(out) :: Hstate(dim_obs) ! Observed ensemble
     END SUBROUTINE
  END INTERFACE
!EOP


! *** local variables ***
  INTEGER :: i,j,k                     ! Counters
  REAL :: post_mean(dim)               ! Posterior mean
  INTEGER :: ipiv(dim_obs)             ! Help variable for LU decomposition
  INTEGER :: work 
  REAL :: Hp(dim_obs,dim)
  REAL :: Hpt(dim, dim_obs)
  REAL :: ens_m(dim, dim_ens)          ! mean of ensemble of particles
  REAL :: tmp_obs_cov(dim_obs,dim_obs)


! **********************************
! *** Compute the posterior mean ***
! **********************************

  post_mean = 0 ! Initialize with zeros
  ens_m = 0
  do i = 1,dim_ens
     post_mean = post_mean + (post_w(i) * ens(:,i))
  end do

  ! subtract the mean from all ensemble members (store back in ensemble)
  do i = 1,dim_ens
     ens_m(:,i) = ens(:,i) - post_mean
  end do
  post_cov_mat = 0 ! Initialize cov. matrix with zeros

  ! calculate the posterior cov mat.
  do i = 1,dim_ens
     do j=1,dim
        do k=1,dim
           post_cov_mat(j,k) =post_cov_mat(j,k) + post_w(i)*ens_m(j,i)*ens_m(k,i)
        end do
     end do
  end do

  ! Call observation operator on all columns of the posterior cov. mat.
  do i = 1, dim
     CALL CB_obs_op(0, dim, dim_obs, post_cov_mat(:,i),Hp(:,i))
  enddo

  ! Calculate transpose of H*P.
  do j = 1, dim
     do i = 1,dim_obs
        Hpt(j,i) = Hp(i,j)
     enddo
  enddo

  ! Call observation operator on (Hp)T
  do i = 1, dim_obs
     CALL CB_obs_op(0,dim,dim_obs,Hpt(:,i),sensitivity(:,i))
  enddo
 


 !Copy cov_matrix so it does not change using dgesv
  tmp_obs_cov = obs_cov

  ! Compute sensitivity = sensitivity* R^(-1)
  CALL dgesv(dim_obs, dim_obs, tmp_obs_cov, dim_obs, ipiv, &
       sensitivity, dim_obs, status)

  if (.not. (status == 0)) then
     write(*,*) 'Error in calculation of sensitivity'
  endif

END SUBROUTINE sangoma_computesensitivity_op

