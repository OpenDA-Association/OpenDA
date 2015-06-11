! $Id: equal_weights_step.f90 558 2015-03-25 13:44:47Z larsnerger $


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!    A subroutine to compute the equal weights step for particles in 
!!!    equivalent weights particle filter (EWPF) method.
!!!    Copyright (C) 2015  Sanita Vetra-Carvalho
!!!
!!!    This program is distributed under the Lesser General Public License (LGPL) version 3,
!!!    for more details see <https://www.gnu.org/licenses/lgpl.html>.
!!!
!!!    Email: s.vetra-carvalho @ reading.ac.uk
!!!    Mail:  School of Mathematical and Physical Sciences,
!!!    	      University of Reading,
!!!	      Reading, UK
!!!	      RG6 6BB
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine equal_weight_step(Ne,Nx,Ny,weight,x_n,y, &
           cb_H, cb_HT,cb_solve_r, cb_solve_hqht_plus_r,cb_Qhalf) bind(C, name="equal_weight_step_")
  use, intrinsic :: ISO_C_BINDING
  use sangoma_base, only: REALPREC, INTPREC
  use user_base
  implicit none

  integer(INTPREC), intent(in) :: Nx,Ny,Ne               ! dims of state, obs and ensemble
  real(REALPREC), intent(inout), dimension(Ne) :: weight ! vector holding particle weights
  real(REALPREC), intent(inout), dimension(Nx,Ne) :: x_n ! matrix of states for each particle at current timestep n
  real(REALPREC), intent(in), dimension(Ny)    :: y      ! vector of observation data at current time step n

! Uses following external user defined callback routines:
!    cb_H - to return a vector in observation space given a vector in state space, i.e. y = h(x)
!    cb_HT - to return a vector in state space given a vector in observation space, i.e. x = h^T(y)
!    cb_solve_hqht_plus_r - scale given vector in observation space by (HQH^T+R)^{-1}
!    cb_solve_r - to return a vector scaled by R^{-1} (all in observation space)
!    cb_Qhalf - to return a vector scaled by Q^{1/2} (all in state space)

      INTERFACE
        subroutine cb_H(Ne,Nx,Ny,vec_in,vec_out) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none
        
          integer(INTPREC), intent(in) :: Nx,Ny,Ne                    ! dims of state, obs and ensemble
          real(REALPREC), intent(in), dimension(Nx,Ne) :: vec_in      ! input vector in state space to which
                                                                      ! to apply the observation operator h, e.g. h(x)
          real(REALPREC), intent(inout), dimension(Ny,Ne) :: vec_out  ! resulting vector in observation space
        end subroutine cb_H
      END INTERFACE

      INTERFACE
        subroutine cb_HT(Ne,Nx,Ny,vec_in,vec_out) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Nx,Ny,Ne                   ! dims of state, obs and ensemble
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vec_in     ! input vector in observation space to which
                                                                     ! to apply the observation operator h, e.g. h^T(x)
          real(REALPREC), intent(inout), dimension(Nx,Ne) :: vec_out ! resulting vector in state space
        end subroutine cb_HT
      END INTERFACE

      INTERFACE
        subroutine cb_solve_hqht_plus_r(Ne,Ny,vec_in,vec_out) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Ny,Ne                       ! dims of obs and ensemble
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vec_in      ! vector in observation space to which to
                                                                      ! apply the observation error covariances R,
                                                                      ! e.g. (HQH^T+R)^{-1}(d)
          real(REALPREC), intent(inout), dimension(Ny,Ne) :: vec_out  ! resulting vector in observation space
        end subroutine cb_solve_hqht_plus_R
      END INTERFACE

      INTERFACE
        subroutine cb_solve_r(Ne,Ny,vec_in,vec_out) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Ny,Ne                      ! dims of obs and ensemble
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vec_in     ! input vector in observation space 
                                                                     ! which to apply the inverse observation error
                                                                     ! covariances R, e.g. R^{-1}(d)
          real(REALPREC), intent(inout), dimension(Ny,Ne) :: vec_out ! resulting vector in observation space
        end subroutine cb_solve_r
      END INTERFACE 

      INTERFACE
        subroutine cb_Qhalf(Ne,Nx,vec_in,vec_out) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Nx,Ne                      ! dims of state and ensemble
          real(REALPREC), intent(in), dimension(Nx,Ne) :: vec_in     ! vector in state space to which to apply
                                                                     ! the squarerooted model error covariances 
                                                                     ! Q^{1/2}, e.g. Q^{1/2}(d)
          real(REALPREC), intent(inout), dimension(Nx,Ne) :: vec_out ! resulting vector in state space!!!
        end subroutine cb_Qhalf
      END INTERFACE

  ! LOCAL VARIABLES
  ! 1) Define local paramters 
       real(REALPREC) :: efac                        ! efac parameter for uniform distribution
       integer(INTPREC) :: Ne_keep                   ! number of particles kept
       real(REALPREC), parameter :: pi = 4.0*atan(1.0)
  ! 2) Define local variables
      real(REALPREC), allocatable :: a(:),b(:),alpha(:)
      real(REALPREC), dimension(Ne) ::c                 ! max weight for each particle 
      real(REALPREC), dimension(Ne) :: csorted          ! sorted c in ascending order
      real(REALPREC) :: ctarget                         ! target weight particle should reach to be kept
      integer(INTPREC) :: particle,i                    ! counters
      real(REALPREC), dimension(Nx,Ne) :: x_n_sorted    ! sorted x^n 
      integer(INTPREC) :: idxsorted(Ne)                 ! index  of sorted x_n
      real(REALPREC), dimension(Ne) :: weightSorted     ! sorted weights
      real(REALPREC), dimension(Ny,Ne) :: Hx_n          ! H(x^n) 
      real(REALPREC), dimension(Ny,Ne) :: y_Hx_n        ! y-H(x^n) 
      real(REALPREC), dimension(Nx,Ne) :: gain          ! QH^T(HQH^T+R)^{-1}(y-H(x^n))
      real(REALPREC), dimension(Nx,Ne) :: betan         ! the mixture random variable
      real(REALPREC), dimension(Ne) :: innerInovation_n ! innerInovation_n = (y_Hx_n)^t(HQH^T + R)^(-1) y_Hx_n
      real(REALPREC), dimension(Ne) :: innerR_n         ! innerR_n = (y_Hx_n)^t R^(-1)(y_Hx_n)
      logical, dimension(Ne) :: uniform                 ! true if uniform, false if Gaussian
      real(REALPREC), dimension(Nx,Ne) :: statev        ! temporary state space vector 
      real(REALPREC), dimension(Ny,Ne) :: obsv,obsvv    ! temporary  obs  space vector
      real(REALPREC) :: obsvv_v(Ny), y_Hx_n_v(Ny)       ! temporary arrays

       efac = efacNum/real(Ne)
       Ne_keep = nint(keep*Ne)

       ALLOCATE(a(Ne_keep),b(Ne_keep),alpha(Ne_keep))


  ! Given current model state x_n, get Hx_n (model predicted
  ! observations at current observation time, i.e. h(x^n). 
  call cb_H(Ne,Nx,Ny,x_n,Hx_n)

  !normalise the weights
  weight = exp(-weight+maxval(weight))
  weight = weight/sum(weight)
  weight = -log(weight)
    
 
  ! compute c for each particle, i.e. maximum weight each particle can achieve
  ! without being moved
  do i = 1,Ne
     y_Hx_n(:,i) = y - Hx_n(:,i)
     
  end do

  ! for all particles compute d^T(HQH^T + R)^(-1)d and return it into innerInovation_n
  innerInovation_n = 0.0
  call innerHQHt_plus_R_1(Ne,Ny,y_Hx_n,innerInovation_n, &
                             cb_solve_hqht_plus_r) 

  
  ! Compute the maximum weight each particle can achieve   
  c = weight + 0.5*innerInovation_n

  ! Sort all the particles by the maximum weight they achieved 
  ! (i.e. by their c_i's
  

  csorted = c ! assign unsorted list to sort

  ! Set index array as second array to be sorted
  do i = 1, Ne
     idxsorted(i) = i
  end do

  call quicksort_d(csorted,idxsorted,Ne) 

  ! Set ctarget to be the target maximum weight all particles that will be 
  ! kept must achieve when repositioned
  ctarget = csorted(Ne_keep)

  !compute the kalman gain and apply it to the innovation vector y_Hx_n,
  ! i.e. K[d] = QH^T(HQH^T+R)^{-1} [y - h(M(x^{n-1}))]. Return the result
  ! in a variable gain. 
  call Kgain(Ne,Nx,Ny,y_Hx_n,gain, &
             cb_HT, cb_solve_hqht_plus_r, cb_Qhalf)

  ! Given gain compute h(gain) = obsv, i.e. compute h(Kd) in a_i(s) below
  call cb_H(Ne,Nx,Ny,gain,obsv)

  ! Given obsv = h(Kd) compute R^{-1}h(Kd) and return result in obsvv
  call cb_solve_r(Ne,Ny,obsv,obsvv)

  !compute a_j for each particle j that is kept
  do i = 1,Ne_keep
    particle = idxsorted(i) ! index of the current particle that we kept
    obsvv_v = obsvv(:,particle)
    y_Hx_n_v = y_Hx_n(:,particle)
      
    a(i) = 0.5*dot_product(obsvv_v,y_Hx_n_v)
  end do
     
  ! calculate e = d R^{-1} d, where d = y-Hx
  call innerR_1(Ne,Ny,y_Hx_n,innerR_n, &
                cb_solve_r)

  !compute alpha_j for each particle j that is kept
  do i = 1,Ne_keep
     particle = idxsorted(i) ! index of the current particle that we kept
     b(i) = 0.5*innerR_n(particle) - ctarget + weight(particle)
     !note the plus sign in the below equation. See Ades & van Leeuwen 2012.
     alpha(i) = 1.0 + sqrt(1.0 - b(i)/a(i) + 1.0e-6)
  end do


  !draw from a mixture density for the random noise then correlate it
  call MixtureRandomNumbers2D(nmean,nstd,ufac,efac,Nx,Ne,statev,uniform)

  ! Given random noise in statev (computed in MixtureRandomNumbers2D)
  ! compute correlated noise betan with correlation matrix Q^{1/2}
  call cb_Qhalf(Ne,Nx,statev,betan)

  !update the weights and the new state
  do i = 1,Ne_keep
    particle = idxsorted(i)
    if(uniform(i)) then   
      weight(particle) = weight(particle) +&
                         (alpha(i)**2.0 - 2.0*alpha(i))*a(i) + & 
                         0.5 *innerR_n(particle)
    else
      weight(particle) = weight(particle) +&
                         (alpha(i)**2.0  - 2.0 *alpha(i))*a(i) + &
                         0.5 *innerR_n(particle) &
                         + 2**(-real(Nx)/2.0 )*pi**(real(Nx)&
                         &/2.0 )*nstd*ufac**(-real(Nx))*((1.0 &
                         &-efac)/efac)*exp(0.5 *(sum(betan(:,i)*betan(:,i))))        
    end if !if(uniform)
        
    !now do the following perform the determinsitic move plus mixed density jiggle
    !x^n = M(x^(n-1)) + alpha(i) K (y-H(M(x_i^n-1))) + betan
    x_n(:,particle) = x_n(:,particle) + alpha(i)*gain(:,particle) + betan(:,i)
  end do



  ! Resample the particles to make a full ensemble and with equal weights. 
  weightSorted = weight(idxsorted)
  x_n_sorted = x_n(:,idxsorted)


  call resample(Ne,Nx,Ne_keep,weightSorted,x_n_sorted)
  x_n = x_n_sorted
  weight = weightSorted

  DEALLOCATE(a)
  DEALLOCATE(b)
  DEALLOCATE(alpha)
 
end subroutine equal_weight_step



!*************************************************************************
!!! RESAMPLING SUBROUTINE
!*************************************************************************
subroutine resample(Ne,Nx,Ne_keep,weight,x_n)
! This takes the subset, x_in, of x_n with the particles which were kept (i.e. could
! reach the max chosen weight with the corresponding weights, weight, which are 
! stored as -log(w_j).
! Returned is the full particle ensemble, x_out, with Ne resampled particles with 
! completely equal weights.
use sangoma_base
use random
implicit none

integer(INTPREC), intent(in) :: Ne,Nx                   ! dims of state, obs and ensemble
integer(INTPREC), intent(in) :: Ne_keep                 ! number of kept particles
real(REALPREC), intent(inout), dimension(Nx,Ne) :: x_n  ! ensemble particle matrix
real(REALPREC), intent(inout), dimension(Ne) :: weight  ! particle weight 
real(REALPREC), dimension(Nx,Ne) :: x_in
real(REALPREC), dimension(Ne_keep) :: norm_weight       ! normalised weights
real(REALPREC), dimension(Ne_keep) :: cumweights        ! accumulated weights
real(REALPREC) :: uniformn,draw                         ! random numberh
integer(INTPREC) :: ensIdx                              ! ensemble index for resampling
integer :: j                                            ! counter


x_in = x_n ! copy intput to the input matrix
x_n = 0.0

!normalise the weights and store them as the actual values.
norm_weight = exp(-weight(1:Ne_keep) + minval(weight(1:Ne_keep)) )
norm_weight = norm_weight/sum(norm_weight)


!compute the cumulative weights, so cumweights(j) = sum_i=1^j weights(i)

cumweights = 0.0 
cumweights(1) = norm_weight(1)
do j = 2,Ne_keep
   cumweights(j) = cumweights(j-1) + norm_weight(j)
end do

!now make the draw of the single random variable
!distributed normally between 0 and 1/(number of ensemble members)
call random_number(draw)
uniformn = draw/Ne

! Resample a new FULL set of particles
!-------------------------------------
  Do j=1,Ne
    ensIdx = 1 ! start from first bin 
    DO WHILE (uniformn .gt. cumweights(ensIdx)) ! keep looking for the bin where it fits
      ensIdx=ensIdx+1 ! find the particle index that fits into the bin
    END DO
    x_n(:,j) = x_in(:,ensIdx)  ! copy the particle with index ensIdx(j) into the resampled ensemble
    uniformn = uniformn + 1.0/Ne ! get the new uniform number 
 End Do

  !we have just resampled, so by construction the particles
  !have equal weights.
  weight = 1.0/real(Ne)
  weight = -log(weight)


end subroutine resample
