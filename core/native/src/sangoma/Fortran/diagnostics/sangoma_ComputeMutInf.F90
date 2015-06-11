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
!BOP
!
! !ROUTINE: sangoma_ComputeMutInf --- Compute mutual information
!
! !INTERFACE:
SUBROUTINE sangoma_computeMutInf(dim, dim_ens, dim_obs, dim_sample, R, &
      observations, ens, prior_w, CB_obs_op, MI, status) &
      BIND(C, name="sangoma_computemutinf_")

! !DESCRIPTION:
! Mutual information is the relative entropy averaged over
! observation space, MI=int(RE*p(y))dy. Where p(y)=int(p(y|x)*p(x))dy, 
! given approximately by the sum of the posterior weights. 
! MI can then be approximated in two ways: 
!   1) Quadrature: Discretise ob space into M points. 
!       MI=sum_{i=1}^{M}(RE_i*p(y)_i)*Dy.
!   2) random sampling: Sample M random points from p(y|x). 
!       MI=sum_i(RE_i*p(y)_i)
! See Fowler and van Leeuwen (Tellus 64A (2012) 17192).
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
  INTEGER(INTPREC), INTENT(in)  :: dim                ! PE-local state dimension
  INTEGER(INTPREC), INTENT(in)  :: dim_ens            ! Ensemble size
  INTEGER(INTPREC), INTENT(in)  :: dim_obs            ! Number of observations
  INTEGER(INTPREC), INTENT(in)  :: dim_sample	      ! Sample size for observation space
  REAL(REALPREC), INTENT(in)    :: R(dim_obs,dim_obs) ! Observation covariance
  REAL(REALPREC), INTENT(in)    :: observations(dim_obs) ! Observations
  REAL(REALPREC), INTENT(in)    :: ens(dim, dim_ens)  ! ensemble of particles
  REAL(REALPREC), INTENT(in)    :: prior_w(dim_ens)   ! prior weights
  REAL(REALPREC), INTENT(out)   :: MI	              ! Mutual Information 
  INTEGER(INTPREC), INTENT(out) :: status             ! Status flag (0=success)

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
  INTEGER :: i,j,k       ! Counters
  REAL, DIMENSION(dim_sample) :: re_y, p_y
  REAL, DIMENSION(dim_sample,dim_ens) :: wpp 
  REAL, DIMENSION(dim_obs, dim_sample) :: sample
  INTEGER, DIMENSION(4) :: seed
  REAL, DIMENSION(dim_obs,dim_obs) :: rootR
  REAL :: detR 
  REAL, DIMENSION(dim_obs) :: Rtsp
  
  REAL, DIMENSION(dim_obs) :: Hens
  REAL, DIMENSION(dim_obs) :: RinvHens 
  REAL :: p_y_x
  REAL, ALLOCATABLE :: work(:)
  REAL, ALLOCATABLE :: svals(:)
  INTEGER :: ldwork = 0
  REAL :: wdp
  REAL :: pi,oneDdetR
  REAL, DIMENSION(dim_obs,dim_obs) :: R_tmp,rootR_tmp

 



 
! ****************************
! *** Initialize variables ***
! ****************************

  re_y = 0
  p_y = 0
  p_y_x = 0
  MI = 0
  pi= 4.0 * ATAN(1.0)


!init random seed
  seed(1) = 4
  seed(2) = 2034
  seed(3) = 0
  seed(4) = 3

 
! **********************************
! *** Calculate square root of R ***
! **********************************

  ! Compute symmetric square-root by SVD

  ALLOCATE(svals(dim_obs))
  ALLOCATE(work(3*dim_obs))
  ldwork = 3*dim_obs
  R_tmp = R

  CALL dsyev('v','l',dim_obs,R_tmp,dim_obs, svals,work,ldwork, status)

  DO j = 1,dim_obs
     DO i = 1, dim_obs
        rootR_tmp(j,i) = R_tmp(j,i) * SQRT( svals(i))
     END DO
  END DO
   
  CALL dgemm('n', 'n', dim_obs, dim_obs, dim_obs, 1.0, &
       rootR_tmp, dim_obs, R_tmp, dim_obs, 0.0, rootR, dim_obs) 


! *******************************:
! *** Calculate Sample Points ***
! *******************************

  DO i = 1,dim_sample       
     !generate observations from random sampling
     CALL dlarnv(3, seed, dim_obs,sample(:,i))
       
     !Multiply square root with random numbers
     CALL dgemv('N', dim_obs, dim_obs, 1.0, rootR, &
          dim_obs, sample(:,i), 1, 0.0, Rtsp, 1) 
     sample(:,i) = observations +  Rtsp   
  ENDDO
		

 
! *******************************************
! *** Use random sampling for integration ***
! *******************************************

  R_tmp = R
!Calculate cholesky decompotion of R in order to calculate the determinant
  ! R = L^T *L  (stored back in R)
  CALL dpotrf('U',dim_obs,R_tmp,dim_obs,status)
 
  
  IF ( status .NE. 0 ) THEN
     WRITE(*,*) 'Error in calculating the cholesky decomposition'
  ENDIF

 
  !Calculate the product of the trace of R
  detR = R_tmp(1,1)
  DO i = 2,dim_obs
     detR = detR * R_tmp(i,i)
  ENDDO
  detR = detR**2
  oneDdetR = 1./SQRT((2*pi)**dim_obs*detR)

  !Calculate the weights
  DO i = 1,dim_sample
     DO j = 1,dim_ens
   
        CALL CB_obs_op(0,dim,dim_obs,ens(:,j),Hens)
        Hens = Hens-sample(:,i)

        !solve R*x = (Hx_j-yi) using the chol decomposition computed before
        RinvHens = Hens ! store right hand side

        IF ( status == 0 ) THEN
           CALL dpotrs('U',dim_obs,1,R_tmp,dim_obs,RinvHens,dim_obs,status)
        ELSE 
           WRITE(*,*) 'Error in solving Y = Rinv*(hx-y)'
        ENDIF

        ! Calculate x = -0.5*(H(x)-y)^T * Rinv (H(x)-y)
        CALL dgemm('T','N', 1 ,1,dim_obs,-0.5, &
             Hens,dim_obs,RinvHens,dim_obs,0.,p_y_x,1)	

        if (status .ne. 0 ) then
               WRITE(*,*) 'Error in solvin -0.5*(H(x)-y)^T * Rinv (H(x)-y)'
        endif

        p_y_x = EXP(p_y_x)*oneDdetR
        wpp(i,j) = p_y_x * prior_w(j)


     ENDDO
     
     !Calculate relative entropy as a function of y
     !caculate marignal distribution
     p_y(i) = SUM(wpp(i,:))
     wpp(i,:) = wpp(i,:)/p_y(i)
     re_y(i) = 0
     DO j = 1,dim_ens
        wdp = wpp(i,j) / prior_w(j)
        IF(  wdp > 0 ) THEN
           re_y(i) = re_y(i) + wpp(i,j)* LOG(wdp)
        ENDIF
     ENDDO
  ENDDO
  p_y = p_y/SUM(p_y)

  MI = 0
  DO i = 1,dim_sample
     MI = MI+ p_y(i) * re_y(i)
  ENDDO

END SUBROUTINE sangoma_computeMutInf
