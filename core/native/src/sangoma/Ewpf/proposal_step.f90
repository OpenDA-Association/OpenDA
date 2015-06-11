! $Id: proposal_step.f90 547 2015-02-19 12:32:23Z sp709689 $

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!    Subroutines needed to nudge particles at each model time step 
!!!    towards future observations. Part of the equivalent weights 
!!!    particle filter code supplied for SANGOMA project by 
!!!    Sanita Vetra-Carvalho. Code given here was developed by 
!!!    Philip A. Browne for EMPIRE <http://www.met.reading.ac.uk/~darc/empire>.
!!!
!!!    Collection of subroutines needed for equivalent weights particle filter
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

! proposal_step nudges particles at each model time step towards the next set of future observations. 
! It also accumulates weight of each particle due to the nudging.
subroutine proposal_step(Ne,Nx,Ny,weight,x_n,y,t_model,obsVec,dt_obs, &
           cb_H, cb_HT, cb_Qhalf, cb_solve_r) bind(C, name="proposal_step_")
  use sangoma_base, only: REALPREC, INTPREC ! use sangoma defined precision for C-Bind
  use user_base                             ! use user defined parameters 
  implicit none

  integer(INTPREC), intent(in) :: Ne,Nx,Ny                  ! dims of state, obs and ensemble
  integer(INTPREC), intent(in) :: t_model                   ! current model timestep
  integer(INTPREC),intent(in) :: obsVec                     ! model time step at which we have next 
                                                            ! observations, i.e. next analysis time
  integer(INTPREC),intent(in) :: dt_obs                     ! model timesteps between last and next
                                                            ! observation setst
  real(REALPREC), intent(in), dimension(Ny)    :: y         ! vector of the next set of observations (in future)
  real(REALPREC), intent(inout), dimension(Nx,Ne) :: x_n    ! state matrix at the current time step n
  real(REALPREC), intent(inout), dimension(Ne) :: weight    ! vector containing weights for all particles  
  real(REALPREC) :: pWeight                                 ! weight corresponding to proposal density
  real(REALPREC), dimension(Nx,Ne) :: normaln               ! matrix to store uncorrelated random error 
                                                            ! for all particles, normaln ~ N(0,I)
  real(REALPREC), dimension(Nx,Ne) :: betan                 ! matrix to store sqrtQ correlated random error 
                                                            ! for all particles, betan ~ N(0,Q)
  real(REALPREC), dimension(Ny,Ne) :: Hx_n                  ! H(x^n)
  real(REALPREC), dimension(Ny,Ne) :: y_Hx_n                ! y-H(x^n)
  real(REALPREC), dimension(Nx,Ne) :: kgain                 ! QH^T(HQH^T+R)^(-1)(y-H(x^n))
  real(REALPREC), dimension(Nx,Ne) :: Qkgain                
  integer(INTPREC) :: j                                     ! counter over all particles
   
! Uses following external user defined callback routines:
!    cb_H - to return a vector in observation space given a vector in state space, i.e. y = h(x)
!    cb_HT - to return a vector in state space given a vector in observation space, i.e. x = h^T(y).
!    cb_solve_r - to return a vector scaled by R^{-1} (all in observation space)
!    cb_Qhalf - apply Q^{1/2} to  the given state vector and return the result

      INTERFACE
        subroutine cb_H(Ne,Nx,Ny,vecIn,vecOut) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Nx,Ny,Ne                  ! dims of state, obs and ensemble
          real(REALPREC), intent(in), dimension(Nx,Ne) :: vecIn     ! input vector in state space to which
                                                                    ! to apply the observation operator h, e.g. h(x)
          real(REALPREC), intent(inout), dimension(Ny,Ne) :: vecOut ! resulting vector in observation space
        end subroutine cb_H
      END INTERFACE

      INTERFACE
        subroutine cb_HT(Ne,Nx,Ny,vecIn,vecOut) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Nx,Ny,Ne                   ! dims of state, obs and ensemble
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vecIn      ! input vector in observation space to which
                                                                     ! to apply the observation operator h, e.g. h^T(x)
          real(REALPREC), intent(inout), dimension(Nx,Ne) :: vecOut  ! resulting vector in state space
        end subroutine cb_HT
      END INTERFACE

      INTERFACE
        subroutine cb_solve_r(Ne,Ny,vecIn,vecOut) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Ny,Ne                      ! dims of obs and ensemble
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vecIn      ! input vector in observation space 
                                                                     ! which to apply the inverse observation error
                                                                     ! covariances R, e.g. R^{-1}(d)
          real(REALPREC), intent(inout), dimension(Ny,Ne) :: vecOut  ! resulting vector in observation space
        end subroutine cb_solve_r
      END INTERFACE

      INTERFACE
        subroutine cb_Qhalf(Ne,Nx,vecIn,vecOut) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Nx,Ne                     ! dims of state and ensemble
          real(REALPREC), intent(in), dimension(Nx,Ne) :: vecIn     ! vector in state space to which to apply
                                                                    ! the squarerooted model error covariances 
                                                                    ! Q^{1/2}, e.g. Q^{1/2}(d)
          real(REALPREC), intent(inout), dimension(Nx,Ne) :: vecOut ! resulting vector in state space!!!
        end subroutine cb_Qhalf
      END INTERFACE


    ! Given previous model state x_n, get Hx_n (model predicted
    ! observations at previous model time step, i.e. h(x^n). 
    call cb_H(Ne,Nx,Ny,x_n,Hx_n)  

     do j = 1,Ne
        y_Hx_n(:,j) = y - Hx_n(:,j)  !compute y - h(x^n) for each particle j
     end do

  !Get a Gaussian random noise with 0 mean and std 1 for all particles
    call NormalRandomNumbers(nmean,nstd,Nx,Ne,normaln)
  
  !compute the relaxation or nudging term Qkgain, the intermediate
  !term kgain and apply correlation to noise
    call Bprime(Ne,Nx,Ny,y_Hx_n,kgain,Qkgain,normaln,betan,t_model,obsVec,dt_obs, & 
          cb_Qhalf, cb_solve_r, cb_HT)

  !Nudge the state and update weights based on these terms
    DO j = 1,Ne
       x_n(:,j) = x_n(:,j) + Qkgain(:,j) + betan(:,j)
       pweight = sum(Qkgain(:,j)*kgain(:,j))+2.0*sum(betan(:,j)*kgain(:,j))
       weight(j) = weight(j) + 0.5*pWeight
    end DO

end subroutine proposal_step



!!! Compute the nudging term QHtR_1d and random forcing betan to nudge particles towards 
!!! future observations.
!!! Also return kgain part of the nudging term which is used in weight adjustment due 
!!! to the nudging. 
subroutine Bprime(Ne,Nx,Ny,d,kgain,QHtR_1d,normaln,betan,t_model,obsVec,dt_obs, & 
           cb_Qhalf, cb_solve_r, cb_HT) 

use sangoma_base, only: REALPREC, INTPREC
use user_base
implicit none

integer(INTPREC), intent(in) :: Ne,Nx,Ny                ! dims of state, obs and ensemble
integer(INTPREC),intent(in) :: t_model                  ! current model timestep
integer(INTPREC),intent(in) :: obsVec                   ! model time step at which we have next 
                                                        ! observations, i.e. next analysis time
integer(INTPREC),intent(in) :: dt_obs                   ! model timesteps between last and next
                                                        ! observation sets
real(REALPREC), dimension(Ny,Ne), intent(in) :: d       ! a vector d = (y-H(x)) from proposal_filter
                                                        ! Note, that d is the difference between current particle states
                                                        ! and future observation
real(REALPREC), dimension(Nx,Ne), intent(out) :: kgain  ! kgain in the nudging 
real(REALPREC), dimension(Ny,Ne) :: R_1d                ! R^{-1}*d
real(REALPREC), dimension(Nx,Ne) :: HtR_1d              ! H^T R^{-1}*d
real(REALPREC), dimension(Nx,Ne), intent(out) :: QHtR_1d! QH^TR^{-1}*d
real(REALPREC), dimension(Nx,Ne), intent(in) :: normaln ! normaln ~ N(0,I)
real(REALPREC), dimension(Nx,Ne), intent(out) :: betan  ! betan ~ N(0,Q)
real(REALPREC), dimension(Nx,Ne) :: Btemp               ! temporary variable to store Q^{1/2}normaln
real(REALPREC) :: p,tau                                 ! parameters


! Uses following external user defined callback routines:
!    cb_HT - to return a vector in state space given a vector in observation space, i.e. x = h^T(y).
!    cb_solve_r - to return a vector scaled by R^{-1} (all in observation space)
!    cb_Qhalf - apply Q^{1/2} to  the given state vector and return the result

      INTERFACE
        subroutine cb_HT(Ne,Nx,Ny,vecIn,vecOut) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Nx,Ny,Ne                   ! state, observation and ensemble dimensions
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vecIn      ! input vector in observation space to which
                                                                     ! to apply the observation operator h, e.g. h^T(x)
          real(REALPREC), intent(inout), dimension(Nx,Ne) :: vecOut  ! resulting vector in state space
        end subroutine cb_HT
      END INTERFACE

      INTERFACE
        subroutine cb_solve_r(Ne,Ny,vecIn,vecOut) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Ny,Ne                      ! observation and ensemble dimensions
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vecIn      ! input vector in observation space 
                                                                     ! which to apply the inverse observation error
                                                                     ! covariances R, e.g. R^{-1}(d)
          real(REALPREC), intent(inout), dimension(Ny,Ne) :: vecOut  ! resulting vector in observation space
        end subroutine cb_solve_r
      END INTERFACE

      INTERFACE
        subroutine cb_Qhalf(Ne,Nx,vecIn,vecOut) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Nx,Ne                      ! state and ensemble dimensions
          real(REALPREC), intent(in), dimension(Nx,Ne) :: vecIn      ! vector in state space to which to apply
                                                                     ! the squarerooted model error covariances 
                                                                     ! Q^{1/2}, e.g. Q^{1/2}(d
          real(REALPREC), intent(inout), dimension(Nx,Ne) :: vecOut  ! resulting vector in state space!!!
        end subroutine cb_Qhalf
      END INTERFACE

! Compute tau, i.e. the fraction that tells us how close we are to the next set of observations
! in comparison to the last set 
tau = 1.0 - real(obsVec -t_model)/dt_obs


   ! until tau < freetime we do not nudge the particle 
   ! i.e. we only nudge the particle once we get closer 
   ! to the next set of observations 
   if(tau .le. freetime) then
      ! No deterministic forcing as we are still too far in time from next observation in comparison 
      ! to last set of observations
      kgain = 0.0 
      QHtR_1d = 0.0 

      ! Use only correlated random forcing betan = Q(normaln) (using cb_Qhalf twice)
      call cb_Qhalf(Ne,Nx,normaln,Btemp)
      call cb_Qhalf(Ne,Nx,Btemp,betan)
   else
      ! increase the nudging strength linearly with time
      ! NOTE, some models can be very sensitive to the strength of nudging - 
      ! tune this parameter depending on your model
      p = nudgefac*(tau-freetime)/(1.0d0-freetime)  

      call cb_solve_r(Ne,Ny,d,R_1d)  ! Compute R_1d = R^{-1}(y - h(x)) 
                                     ! Note, that d is the difference between current particle states 
                                     ! and future observation set

      ! Compute HtR_1d = h^t(R_1d) = R^{-1}(y - h(x)) 
      call cb_HT(Ne,Nx,Ny,R_1d,HtR_1d)

      ! Compute Kalman gain used to nudge particles towards future observations
      ! including the nudging force parameter p
      kgain = p*HtR_1d

      ! Compute QHtR_1d = Q*H^T*R^{-1}*(y-h(x)), i.e. apply Q to kgain above
      call cb_Qhalf(Ne,Nx,kgain,Btemp)
      call cb_Qhalf(Ne,Nx,Btemp,QHtR_1d)

      ! Compute correlated random forcing betan = Q(normaln)
      call cb_Qhalf(Ne,Nx,normaln,Btemp)
      call cb_Qhalf(Ne,Nx,Btemp,betan)
   end if

end subroutine Bprime
