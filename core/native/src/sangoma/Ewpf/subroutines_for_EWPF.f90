! $Id: subroutines_for_EWPF.f90 547 2015-02-19 12:32:23Z sp709689 $

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Time-stamp: <2014-11-5 16:30:16 vetra-carvalho>
!!!
!!!    Subsection of subroutines needed for the equivalent weights 
!!!    particle filter code supplied for SANGOMA project by 
!!!    Sanita Vetra-Carvalho. Code given here was developed by 
!!!    Philip A. Browne for EMPIRE <http://www.met.reading.ac.uk/~darc/empire>.
!!!
!!!    Collection of subroutines needed for equivalent weights particle filter
!!!    Copyright (C) 2015  Sanita Vetra-Carvalho
!!!
!!!    This program is distributed under the Lesser General Public License (LGPL)
!!!    version 3, for more details see <https://www.gnu.org/licenses/lgpl.html>.
!!!
!!!    Email: s.vetra-carvalho @ reading.ac.uk
!!!    Mail:  School of Mathematical and Physical Sciences,
!!!    	      University of Reading,
!!!	      Reading, UK
!!!	      RG6 6BB
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

!!!************************************************************************
!!!************************************************************************
subroutine Kgain(Ne,Nx,Ny,vecIn,vecOut, &
           cb_HT, cb_solve_hqht_plus_r, cb_Qhalf)
  !subroutine to apply the operator K to a vector vecIn in obs space and return
  !the vector vecOut in full state space.
  use sangoma_base, only: REALPREC, INTPREC
  implicit none
  
  integer(INTPREC), intent(in) :: Nx,Ny,Ne                ! dims of state, obs & ensemble
  real(REALPREC), dimension(Ny,Ne), intent(in) :: vecIn   ! matrix of all  particle states
  real(REALPREC), dimension(Nx,Ne), intent(out) :: vecOut ! resulting matrix in observation space (of all particles)
  real(REALPREC), dimension(Ny,Ne) :: vecTemp1            ! matrix to store vector solutions of v_i = (HQH^T+R)^{-1}*H(x_i), i = 1,...,Ne
  real(REALPREC), dimension(Nx,Ne) :: vecTemp2            ! matrix to store vector solutions of vv_i - QH^T 
  real(REALPREC), dimension(Nx,Ne) :: vecTemp3            ! temp matrix to store Q^{1/2}vv


! Uses following external user defined callback routines:
!    cb_HT - to return a vector in state space given a vector in observation space, i.e. x = h^T(y).
!    cb_solve_hqht_plus_r - scale given vector in observation space by (HQH^T+R)^{-1}
!    cb_Qhalf - apply Q^{1/2} to  the given state vector and return the result

      INTERFACE
        subroutine cb_HT(Ne,Nx,Ny,vecIn,vecOut) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          use user_base
          implicit none

          integer(INTPREC), intent(in) :: Nx,Ny,Ne            ! state, observation and ensemble dimensions
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vecIn     ! input vector in observation space to which
                                                                     ! to apply the observation operator h, e.g. h^T(x)
          real(REALPREC), intent(inout), dimension(Nx,Ne) :: vecOut ! resulting vector in state space
        end subroutine cb_HT
      END INTERFACE

      INTERFACE
        subroutine cb_solve_hqht_plus_r(Ne,Ny,vecIn,vecOut) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          use user_base
          implicit none

          integer(INTPREC), intent(in) :: Ny,Ne                ! observation and ensemble dimensions
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vecIn      ! vector in observation space to which to
                                                                      ! apply the observation error covariances R,
                                                                      ! e.g. (HQH^T+R)^{-1}(d)
          real(REALPREC), intent(inout), dimension(Ny,Ne) :: vecOut  ! resulting vector in observation space
        end subroutine cb_solve_hqht_plus_r
      END INTERFACE

      INTERFACE
        subroutine cb_Qhalf(Ne,Nx,vecIn,vecOut) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          use user_base
          implicit none

          integer(INTPREC), intent(in) :: Nx,Ne               ! state and ensemble dimensions
          real(REALPREC), intent(in), dimension(Nx,Ne) :: vecIn     ! vector in state space to which to apply
                                                                     ! the squarerooted model error covariances 
                                                                     ! Q^{1/2}, e.g. Q^{1/2}(d)
          real(REALPREC), intent(inout), dimension(Nx,Ne) :: vecOut ! resulting vector in state space!!!
        end subroutine cb_Qhalf
      END INTERFACE

    !****************
    ! Apply (HQH^T+R)^{-1} operator to the given vectors vecIn(i) and return resulting 
    ! vectors v(i). solve_hght_plus_r has to be implemented by the user for their 
    ! specific forecast model.
    !****************
      !**********************************************************************
      call cb_solve_hqht_plus_r(Ne,Ny,vecIn,vecTemp1)
      !**********************************************************************


  !****************
  ! Apply H^T operator to the given vectors v (one for each particle)
  ! and return resulting vectors vv. HT has to be implemented by the user 
  ! for their specific forecast model.
  !****************
    !**********************************************************************
    call cb_HT(Ne,Nx,Ny,vecTemp1,vecTemp2)
    !**********************************************************************
  
  ! Apply Q operator to the given vectors vv (one for each particle)
  ! and return resulting vectors x. Do this by calling cb_Qhalf twice.
    call cb_Qhalf(Ne,Nx,vecTemp2,vecTemp3)
    call cb_Qhalf(Ne,Nx,vecTemp3,vecOut)

end subroutine Kgain
!!!************************************************************************



!!!************************************************************************
!!!************************************************************************
subroutine innerR_1(Ne,Ny,vecIn,vecOut, cb_solve_r) 
! Subroutine to take a vector (collection of vectors) vecIn and return 
! vecOut_i = vecIn_i^T R^(-1) vecIn_i.

  use sangoma_base, only: REALPREC, INTPREC
  implicit none

  integer(INTPREC), intent(in) :: Ny,Ne                   ! dims of observations & ensemble
  real(REALPREC), dimension(Ny,Ne), intent(in) :: vecIn   ! input array
  real(REALPREC), dimension(Ne), intent(out) :: vecOut    ! output array - inner product
  real(REALPREC), dimension(Ny,Ne) :: vecTemp             ! temporary array
  integer(INTPREC) :: i                                   ! counter 


! Uses following external user defined callback routines:
!    cb_solve_r - given a vector in observation space return a vector scaled by R^{-1}

     INTERFACE
       subroutine cb_solve_r(Ne,Ny,vecIn,vecOut) bind(C)
         use, intrinsic :: ISO_C_BINDING
         use sangoma_base, only: REALPREC, INTPREC
         implicit none

         integer(INTPREC), intent(in) :: Ny,Ne               ! observation and ensemble dimensions
         real(REALPREC), intent(in), dimension(Ny,Ne) :: vecIn      ! input vector in observation space 
                                                                    ! which to apply the inverse observation error
                                                                    ! covariances R, e.g. R^{-1}(d)
         real(REALPREC), intent(inout), dimension(Ny,Ne) :: vecOut  ! resulting vector in observation space
      end subroutine cb_solve_r
    END INTERFACE

  !****************
  ! Apply R^{-1} operator to the given vectors y (one for each particle)
  ! and return resulting vectors v. solve_r has to be implemented by the user 
  ! for their specific forecast model.
  !****************
    !**********************************************************************
    call cb_solve_r(Ne,Ny,vecIn,vecTemp)
    !**********************************************************************
  
  ! Compute inner products over all particles and store in vector w
  do i = 1,Ne
     vecOut(i) = dot_product(vecIn(:,i),vecTemp(:,i))
  end do

end subroutine innerR_1
!!!************************************************************************



!!!************************************************************************
!!!************************************************************************
! Subroutine to take an observation vector y and return w = y^T (HQH^T+R)^{-1} y
!!!************************************************************************
subroutine innerHQHt_plus_R_1(Ne,Ny,vecIn,vecOut, cb_solve_hqht_plus_r)  
  use sangoma_base, only: REALPREC, INTPREC
  implicit none
  
  integer(INTPREC), intent(in) :: Ny,Ne                  ! dims of observations & ensemble
  real(REALPREC), dimension(Ny,Ne), intent(in) :: vecIn  ! input vector
  real(REALPREC), dimension(Ne), intent(inout) :: vecOut ! resulting value
  real(REALPREC), dimension(Ny,Ne) :: vecTemp            ! intermediate temporary storage vector
  integer(INTPREC) :: i                                  ! counter

! Uses following external user defined callback routine:
!    cb_solve_hqht_plus_r - scale given vector in observation space by (HQH^T+R)^{-1}

      INTERFACE
        subroutine cb_solve_hqht_plus_r(Ne,Ny,vecIn,vecOut) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Ny,Ne                    ! observation and ensemble dimensions
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vecIn    ! vector in observation space to which to
                                                                   ! apply the observation error covariances R,
                                                                   ! e.g. (HQH^T+R)^{-1}(d)
          real(REALPREC), intent(inout), dimension(Ny,Ne) :: vecOut   ! resulting vector in observation space
        end subroutine
      END INTERFACE
 


  vecTemp = 0.0
  !****************
  ! Apply (HQH^T+R)^{-1} operator to the given vector y and return resulting 
  ! vector v. solve_hght_plus_r has to be implemented by the user for their 
  ! specific forecast model.
  !****************
    !**********************************************************************
    call cb_solve_hqht_plus_r(Ne,Ny,vecIn,vecTemp)
    !**********************************************************************
  
  ! compute the inner product y^T (HQH^T+R)^{-1} y
  do i = 1,Ne
    vecOut(i) = dot_product(vecIn(:,i),vecTemp(:,i))
  end do

end subroutine innerHQHt_plus_R_1
!!!************************************************************************


