!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_handles.c $
! $Revision: 3406 $, $Date: 2012-08-16 15:25:53 +0200 (Thu, 16 Aug 2012) $
!
! Copyright (c) 2012 OpenDA Association
! All rights reserved.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!

! @author Nils van Velzen
!
! OpenDA wrapper to SANGOMA implementation of the equivalent weights particle filter
!
module oda_EWPF_wrapper
use iso_c_binding
use sangoma_base, only: REALPREC, INTPREC
implicit none

interface
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
   end subroutine proposal_step
END INTERFACE



INTERFACE
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
   end subroutine
      
END INTERFACE


!Needed for handling the root of the error covariance matrix (cb_Qhalf/cb_solve_hqht_plus_r)
!And passing stuff to the core routines
real(REALPREC), dimension(:),   allocatable, save ::diagR   
real(REALPREC), dimension(:,:), allocatable, save ::L       ! Ensembles
real(REALPREC), dimension(:,:), allocatable, save ::L_mean  ! Ensembles with mean subtracted  
real(REALPREC), dimension(:,:), allocatable, save ::S_root

logical, private ::debug=.false.


contains


subroutine oda_EWFP_set_diagR(newDiagR, Ny) bind(C, name="oda_ewfp_set_diagr")
          use, intrinsic :: ISO_C_BINDING

implicit none
integer(INTPREC),              INTENT(IN) ::Ny
real(REALPREC), DIMENSION(Ny), INTENT(IN) ::newDiagR

   if (allocated(diagR)) deallocate(diagR)
   allocate(diagR(Ny))
   diagR=newDiagR

end subroutine oda_EWFP_set_diagR



subroutine oda_EWFP_set_L(newL, iEns, Nx, Ne) bind(C, name="oda_ewfp_set_l")
          use, intrinsic :: ISO_C_BINDING

implicit none
integer(INTPREC),              INTENT(IN) ::Nx,Ne
real(REALPREC), DIMENSION(Nx), INTENT(IN) ::newL
integer(INTPREC),              INTENT(IN) ::iEns
real(REALPREC), DIMENSION(Ne,Ne) ::LTL
real(REALPREC), DIMENSION(Ne,Ne) ::U,VT
real(REALPREC), DIMENSION(Ne)    ::S    !Singular values
real(REALPREC), DIMENSION(5*Ne*Ne) ::work
integer ::info

logical, parameter ::loc_debug=.true.



!> Loop counter over ensembles
integer        ::i
!> Mean of the ensemble (only used when setting last ensemble)
real(REALPREC) ::Lmean(Nx)
real(REALPREC) ::Lstd(Nx)
!> Only set the old states equal to this state at start of algorithm

   if (debug .and. loc_debug) then
      print *,'Welcome in oda_EWFP_set_L:',iEns
   endif
  
   if (iEns==1) then
      if (allocated(L))      deallocate (L)
      if (allocated(L_mean)) deallocate (L_mean)
      allocate(L(Nx,Ne),L_mean(Nx,Ne))
   endif
   
   L(:,iEns) = newL

   if (iEns==Ne) then
      if (debug .and. loc_debug) then
         print *,'Last member is set:'
         print *,'Compute mean and ensemble -mean'
      endif

      Lmean=SUM(L,DIM=2)/dble(Ne)

      if (debug .and. loc_debug) then
         print *,'Lmean=',Lmean
      endif
 
      do i=1,Ne
         L_mean(:,i)=L(:,i)-Lmean
      enddo

      
   
      if (debug .and. loc_debug) then
         Lstd=SUM(L_mean*L_mean,DIM=2)/dble(Ne)
         Lstd=SQRT(Lstd)

         call print_matrix('L',L)
         call print_vector('Lmean',Lmean)
         call print_vector('L-std',Lstd)

      endif

      ! Make it root covariance
      L_mean=L_mean/SQRT(DBLE(Ne)-1.0)




      
   endif
   
   if (debug .and. loc_debug) then
      print *,'Leaving oda_EWFP_set_L'
   endif
   
end subroutine oda_EWFP_set_L

subroutine oda_EWFP_get_L(newL, iEns, Nx, Ne) bind(C, name="oda_ewfp_get_l")
          use, intrinsic :: ISO_C_BINDING

implicit none
integer(INTPREC),              INTENT(IN)  ::Nx,Ne
real(REALPREC), DIMENSION(Nx), INTENT(OUT) ::newL
integer(INTPREC),              INTENT(IN)  ::iEns


real(REALPREC), DIMENSION(Ne,Ne) ::LTL
real(REALPREC), DIMENSION(Ne,Ne) ::U,VT
real(REALPREC), DIMENSION(Ne)    ::S    !Singular values
real(REALPREC), DIMENSION(5*Ne*Ne) ::work
integer ::info

integer        ::i
real(REALPREC) ::Lmean(Nx)
logical, parameter ::loc_debug=.false.

   if (debug .and. loc_debug) then
      print *,'Welcome in oda_EWFP_get_L'
   endif


   newL=L(1:Nx,iEns)

   if (debug .and. loc_debug) then
      print *,'Leaving oda_EWFP_get_L'
   endif
   
end subroutine oda_EWFP_get_L







subroutine oda_EWFP_SetupRootError(Ne) bind(C, name="oda_ewfp_setuprooterror")
          use, intrinsic :: ISO_C_BINDING

implicit none
integer(INTPREC),              INTENT(IN) ::Ne

real(REALPREC), DIMENSION(Ne,Ne) ::LTL
real(REALPREC), DIMENSION(Ne,Ne) ::U,VT
real(REALPREC), DIMENSION(Ne)    ::S    !Singular values
real(REALPREC), DIMENSION(5*Ne*Ne) ::work
integer ::info
integer ::iEns
!Testing
integer ::iX, Nx
real(REALPREC), DIMENSION(:,:), ALLOCATABLE ::LLT, QhI, Imat

INTERFACE
   SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )

     CHARACTER          JOBU, JOBVT
     INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
      DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),VT( LDVT, * ), WORK( * )
      END SUBROUTINE DGESVD
END INTERFACE

   if (debug) then
      print *,'Welcome in oda_EWFP_SetupRootError'
   endif
   if (allocated(S_root)) deallocate(S_root)
   allocate(S_root(Ne,Ne))

   !Prepare stuff for root covariance matrix
   LTL=MATMUL(TRANSPOSE(L_mean),L_mean)

   if (debug) then
      call print_matrix('LTL',LTL)
   endif
   
   !Compute SVD from LTL
   if (REALPREC==8) THEN
      CALL DGESVD('A','A',Ne,Ne,LTL,Ne,S,U,Ne,VT,Ne,work,5*Ne*Ne,info)
   else
      CALL SGESVD('A','A',Ne,Ne,LTL,Ne,S,U,Ne,VT,Ne,work,5*Ne*Ne,info)
   endif

   if (info/=0) then
      print *,'Error computing SVD'
      call exit(-1)
   endif

   do iEns=1,Ne
      S_root(:,iEns)=U(:,iEns)*1.0/sqrt(S(iEns))
   enddo
   S_root=MATMUL(S_root,TRANSPOSE(U))

   !if (debug) then
   !   call print_matrix('S_root',S_root)
   !   call print_matrix('S*S',MATMUL(S_root,TRANSPOSE(S_root)))
   !
   !   print *,'Leaving oda_EWFP_SetupRootError'
   !endif

   if (.false.) then
   !Check in state space
      Nx=SIZE(L_mean,1)
      print *,'Nx=',Nx


      allocate(Imat(Nx,Nx))
      allocate(QhI(Nx,Nx))
      allocate(LLT(Nx,Nx))
      Imat=0.0
      do iX=1,nX; Imat(iX,iX)=1.0; enddo
      call oda_Qhalf(Nx,Nx,Imat,QhI)
      call oda_Qhalf(Nx,Nx,QhI,LLT)
      call print_matrix('LLT-root',LLT)
      call print_matrix('LLT',MATMUL(L_mean,TRANSPOSE(L_mean)))

   endif


end subroutine oda_EWFP_SetupRootError



subroutine oda_proposal_step(Ne,Nx,Ny,weight,y,timestep,obsstep,steps_btw_obs)  bind(C, name="oda_proposal_step")
          use, intrinsic :: ISO_C_BINDING
  use sangoma_base, only: REALPREC, INTPREC
  implicit none

  integer(INTPREC), intent(in) :: Nx,Ny,Ne                  ! state, observation, ensemble dimensions
  integer(INTPREC), intent(in) :: timestep                  ! current model timestep
  integer(INTPREC), intent(in) :: obsstep,steps_btw_obs     ! number of next observation set
  real(REALPREC), intent(in), dimension(Ny)    :: y         ! vector of the next set of observations (in future)
  real(REALPREC), intent(inout), dimension(Ne) :: weight    ! vector containing weights for all particles  

  print *,'Welcome in oda_proposal_step'
  call proposal_step(Ne,Nx,Ny,weight,L,y,timestep,obsstep,steps_btw_obs, &
           oda_H, oda_HT, oda_Qhalf, oda_solve_r)
           
   print *,'Done oda_proposal_step'        
end subroutine oda_proposal_step



subroutine oda_equal_weight_step(Ne,Nx,Ny,weight,y) bind(C, name="oda_equal_weight_step")
  use, intrinsic :: ISO_C_BINDING
  use sangoma_base, only: REALPREC, INTPREC
  use user_base
  implicit none

  integer(INTPREC), intent(in)                   :: Nx,Ny,Ne ! dims of state, obs and ensemble
  real(REALPREC),   intent(inout), dimension(Ne) :: weight   ! vector holding particle weights
  real(REALPREC),   intent(in),    dimension(Ny) :: y  

   print *,'Welcome in oda_equal_weight_step'

   call equal_weight_step(Ne,Nx,Ny,weight,L,y, &
           oda_H, oda_HT,oda_solve_r, oda_solve_hqht_plus_r,oda_Qhalf)

   print *,'Done in oda_equal_weight_step'
end subroutine oda_equal_weight_step



        !Apply interpolation operator to ensemble:
        !Note :We augment the state. Hence the observations are always the last Ny elements of the state
        subroutine oda_H(Ne,Nx,Ny,vec_in,vec_out) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Nx,Ny,Ne          ! state, observation and ensemble dimensions
          real(REALPREC), intent(in), dimension(Nx,Ne) :: vec_in      ! input vector in state space to which
                                                                   ! to apply the observation operator h, e.g. h(x)
          real(REALPREC), intent(inout), dimension(Ny,Ne) :: vec_out  ! resulting vector in observation space
  
          print *,'Welcome in oda_H'
          print *,'Nx=',Nx,'Ny=',Ny,'Ne=',Ne

          vec_out=vec_in(Nx-Ny+1:Nx,:)
        end subroutine oda_H
 

        !Apply transpose interpolation operator to ensemble of predictions:
        !Note :We augment the state. Hence the observations are always the last Ny elements of the state    
        subroutine oda_HT(Ne,Nx,Ny,vec_in,vec_out) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none
          
          integer(INTPREC), intent(in) :: Nx,Ny,Ne         ! state, observation and ensemble dimensions
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vec_in     ! input vector in observation space to which
                                                                  ! to apply the observation operator h, e.g. h^T(x)
          real(REALPREC), intent(inout), dimension(Nx,Ne) :: vec_out ! resulting vector in state space
          
            print *,'Welcome in oda_HT'
          
          vec_out=0.0
          vec_out(Nx-Ny+1:Nx,:)=vec_in
          
          
        end subroutine oda_HT



        subroutine oda_solve_r(Ne,Ny,vec_in,vec_out) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Ny,Ne               ! observation and ensemble dimensions
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vec_in     ! input vector in observation space 
                                                                     ! which to apply the inverse observation error
                                                                     ! covariances R, e.g. R^{-1}(d)
          real(REALPREC), intent(inout), dimension(Ny,Ne) :: vec_out ! resulting vector in observation space
          
          integer ::iEns

         print *,'Welcome in oda_solve_r'
          
          
          !Check dimensions
          if (allocated(diagR)) then
             if (size(diagR)==Ny) then
               do iEns=1,Ne
                   vec_out(:,iEns)=vec_in(:,iEns)/diagR
               enddo
             else
                print *,'ERROR in oda_solve_r: Dimensions do not correspond.'
                print *,'Ny=',Ny
                print *,'Size(DiagR)=',size(diagR)
                call exit(-1)
             endif
          else
             print *,'ERROR in oda_solve_r: Obvervation error covariance  is not initialized.'
             call exit(-1)          
          endif
        end subroutine oda_solve_r



        subroutine oda_Qhalf(Ne,Nx,vec_in,vec_out) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Nx,Ne               ! state and ensemble dimensions
          real(REALPREC), intent(in), dimension(Nx,Ne) :: vec_in     ! vector in state space to which to apply
                                                                     ! the squarerooted model error covariances 
                                                                     ! Q^{1/2}, e.g. Q^{1/2}(d)
          real(REALPREC), intent(inout), dimension(Nx,Ne) :: vec_out ! resulting vector in state space!!!


          real(REALPREC), dimension(Ne,Ne) ::LTL


          real(REALPREC), dimension(size(L_mean,2),Ne) ::T1
          real(REALPREC), dimension(size(L_mean,2),Ne) ::T2

          !print *,'Welcome in oda_Qhalf'

          T1=MATMUL(TRANSPOSE(L_mean),vec_in)

          T2=MATMUL(S_root,T1)
          vec_out=MATMUL(L_mean,T2)



! P=(1/(n-1)) [xi_1-xi_avg, ... xi_Ne - xi_avg] [xi_1-xi_avg, ... xi_Ne -
! xi_avg]' = L * L'
! Let
! Psi = L'L
! with svd or symmetric eigenvalue decomposition
! Psi = U*D*U'
! sqrt(P)=L*U*inv(sqrt(D))*U'*L'
! inv(sqrt(D)) is diagonal and operations can be performed elementwise.

! The multiplication with an ensemble as the routine requires is performed
! right to left, ie
! T1=L'*VecIn
! S=U*inv(sqrt(D))*U'
! T2=S*T1
! VecOut=L*T2

        end subroutine oda_Qhalf

        
         subroutine oda_solve_hqht_plus_r(Ne,Ny,vec_in,vec_out) bind(C)
          use, intrinsic :: ISO_C_BINDING
          use sangoma_base, only: REALPREC, INTPREC
          implicit none

          integer(INTPREC), intent(in) :: Ny,Ne                       ! dims of obs and ensemble
          real(REALPREC), intent(in), dimension(Ny,Ne) :: vec_in      ! vector in observation space to which to
                                                                      ! apply the observation error covariances R,
                                                                      ! e.g. (HQH^T+R)^{-1}(d)
          real(REALPREC), intent(inout), dimension(Ny,Ne) :: vec_out  ! resulting vector in observation space


          real(REALPREC), dimension(Ne,Ne) ::A
          integer                          ::Nx,Hx1
          integer                          ::iObs
          integer                          ::info
          
          
          print *,'Welcome in oda_solve_hqht_plus_r'
          Nx=size(L_mean,1)
          Hx1=Nx-Ny+1

          A=MATMUL(L_mean(Hx1:Nx,:),TRANSPOSE(L_mean(Hx1:Nx,:)))

          do iObs=1,Ny
            A(iObs,iObs)=A(iObs,iObs)+diagR(iObs)
          enddo

          vec_out=vec_in
          CALL DPOSV( 'U', Ny, 1, A, Ny, vec_out, Ny, info )

          if (info/=0) then
              print *,'Error in DPOSV computing (HQH^T+R)^{-1}(d): info=',info
              call exit(-1)
          endif
          
          
        end subroutine oda_solve_hqht_plus_r
        
        
        subroutine print_vector(name,a)
        implicit none
        character(len=*),               intent(in) ::name
        real(realprec), dimension(:), intent(in) ::a


        print *,trim(name),'='
        print *, a

        end subroutine print_vector
         
        subroutine print_matrix(name,a)
        implicit none
        character(len=*),               intent(in) ::name
        real(realprec), dimension(:,:), intent(in) ::a

        integer ::m,n,i,j

        m=size(a,1)
        n=size(a,2)
        print *,trim(name),'='
        do j=1,n
           print *, a(j,:)
        enddo

        end subroutine print_matrix
        
        
        
        

end module oda_EWPF_wrapper
