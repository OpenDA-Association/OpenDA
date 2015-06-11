!$id$

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!    A collection of USER DEFINED CALLBACK subroutines used in 
!!!    equivalent weights particle filter (EWPF) code.
!!!    Copyright (C) 2015  Sanita Vetra-Carvalho
!!!
!!!    This program is distributed under the Lesser General Public License (LGPL)
!!!    version 3, for more details see <https://www.gnu.org/licenses/lgpl.html>.
!!!
!!!    Email: s.vetra-carvalho @ reading.ac.uk
!!!    Mail:  School of Mathematical and Physical Sciences,
!!!    	      University of Reading,
!!!	      Reading, UK
!!!	      RG6 6BB!!!   
!!!
!!!    A collection of call-back routines that user has to define to apply
!!!    operators to some vector, i.e. 
!!!       * vec_out = cb_H(vec_in)
!!!       * vec_out = cb_HT(vec_in)
!!!	  * vec_out = cb_Qhalf(vec_in)
!!!	  * vec_out = cb_solve_r(vec_int)
!!! 	  * vec_out = cb_solve_hqht_plus_r(vec_in)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!       S U B R O U T I N E     D E F I N I T I O N S
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!*********************************************
subroutine cb_H(Ne,Nx,Ny,vecIn,vecOut) bind(C)
!*********************************************
use, intrinsic :: ISO_C_BINDING
use sangoma_base, only: REALPREC, INTPREC
implicit none

integer(INTPREC), intent(in) :: Nx,Ny,Ne                   ! state, observation and ensemble dimensions
real(REALPREC), intent(in), dimension(Nx,Ne) :: vecIn      ! input vector in state space to which
                                                           ! to apply the observation operator h, e.g. h(x)
real(REALPREC), intent(inout), dimension(Ny,Ne) :: vecOut  ! resulting vector in observation space

end subroutine cb_H
!*********************************************
!*********************************************


!*********************************************
subroutine cb_HT(Ne,Nx,Ny,vecIn,vecOut) bind(C)
!*********************************************
use, intrinsic :: ISO_C_BINDING
use sangoma_base, only: REALPREC, INTPREC
implicit none

integer(INTPREC), intent(in) :: Nx,Ny,Ne            ! state, observation and ensemble dimensions
real(REALPREC), intent(in), dimension(Ny,Ne) :: vecIn     ! input vector in observation space to which
                                                           ! to apply the observation operator h, e.g. h^T(x)
real(REALPREC), intent(inout), dimension(Nx,Ne) :: vecOut ! resulting vector in state spaceelements


end subroutine cb_HT
!*********************************************
!*********************************************

!*********************************************
subroutine cb_solve_r(Ne,Ny,vecIn,vecOut) bind(C)
!*********************************************
use, intrinsic :: ISO_C_BINDING
use sangoma_base, only: REALPREC, INTPREC
use user_base
implicit none

integer(INTPREC), intent(in) :: Ny,Ne               ! observation and ensemble dimensions
real(REALPREC), intent(in), dimension(Ny,Ne) :: vecIn     ! input vector in observation space 
                                                           ! which to apply the inverse observation error
                                                           ! covariances R, e.g. R^{-1}(d)
real(REALPREC), intent(inout), dimension(Ny,Ne) :: vecOut ! resulting vector in observation space

end subroutine cb_solve_r
!*********************************************
!*********************************************

!*********************************************
subroutine cb_solve_hqht_plus_r(Ne,Ny,vecIn,vecOut) bind(C)
!*********************************************
use, intrinsic :: ISO_C_BINDING
use sangoma_base, only: REALPREC, INTPREC
use user_base
implicit none

integer(INTPREC), intent(in) :: Ny,Ne                ! observation and ensemble dimensions
real(REALPREC), intent(in), dimension(Ny,Ne) :: vecIn       ! vector in observation space to which to
                                                            ! apply the observation error covariances R,
                                                            ! e.g. (HQH^T+R)^{-1}(d)
real(REALPREC), intent(inout), dimension(Ny,Ne) :: vecOut   ! resulting vector in observation space


end subroutine cb_solve_hqht_plus_r
!*********************************************


!*********************************************
subroutine cb_Qhalf(Ne,Nx,vecIn,vecOut) bind(C)
!*********************************************
use, intrinsic :: ISO_C_BINDING
use sangoma_base, only: REALPREC, INTPREC
use user_base
implicit none

integer(INTPREC), intent(in) :: Nx,Ne                ! state and ensemble dimensions
real(REALPREC), intent(in), dimension(Nx,Ne) :: vecIn       ! vector in state space to which to apply
                                                            ! the squarerooted model error covariances 
                                                            ! Q^{1/2}, e.g. Q^{1/2}(d)
real(REALPREC), intent(inout), dimension(Nx,Ne) :: vecOut   ! resulting vector in state space


end subroutine cb_Qhalf
!*********************************************
!*********************************************

