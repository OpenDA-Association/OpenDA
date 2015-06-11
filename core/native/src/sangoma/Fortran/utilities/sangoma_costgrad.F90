!
! !ROUTINE: sangoma_costgrad --- Perform reduced order simulation and 
!                                returns cost funcation and gradient vector values!
!
SUBROUTINE sangoma_costgrad(nmodes, nparam, nsteps, dim_state, nobs, nanalysis, Mx, Malpha, alpha, &
     obs, obsstd, observer, cost, grad, analysis, status) &
     BIND(C, name="sangoma_costgrad_")

! !DESCRIPTION:
! This routine computes the objective function and gradient vector using 
! Reduced order model. The reduced order model is obtained by projecting dominent 
! eigen modes in dynamic operater M and M_alpha for state and parameters respectively.
!
!
! To use this routine, one has to generate dominant eigen modes from an ensemble and
! construct a reduced model operators for states and parameters respectively
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in) :: nmodes                        ! Dimension of the leading eigenvector
  INTEGER(INTPREC),  INTENT(in) :: nparam                       ! Dimension of parameters
  INTEGER(INTPREC),  INTENT(in) :: nsteps                       ! Number of timesteps
  INTEGER(INTPREC),  INTENT(in) :: dim_state                    ! Dimension of state vector
  INTEGER(INTPREC),  INTENT(in) :: nobs                         ! Number of observations (fixed)
  INTEGER(INTPREC),  INTENT(in) :: nanalysis                    ! Number of analysis steps

  REAL(REALPREC), INTENT(in)  :: Mx(nsteps, nmodes, nmodes)     ! Reduced State operator
  REAL(REALPREC), INTENT(in)  :: Malpha(nsteps, nmodes, nparam) ! Reduced parameter operator
  REAL(REALPREC), INTENT(in)  :: alpha(nparam)                  ! Initial paramters
  REAL(REALPREC), INTENT(in)  :: obs(nanalysis, nobs)           ! Observations
  REAL(REALPREC), INTENT(in)  :: obsstd(nanalysis, nobs)        ! Observation standard deviation
  REAL(REALPREC), INTENT(in)  :: observer(nobs, nmodes)         !observation operator  
  REAL(REALPREC), INTENT(out)  :: cost                          ! value of the objective function
  REAL(REALPREC), INTENT(out)  :: grad(nparam)                  ! gradient Vector
  INTEGER(INTPREC), INTENT(in)  :: analysis(nanalysis)          ! analysis steps
 
  INTEGER(INTPREC), INTENT(out) :: status                       ! Status flag
!EOP


! *** local variables ***
INTEGER :: i, k       ! Counter
INTEGER :: stat       ! internal status flag
INTEGER :: ldwork     ! variable for SVD routine 
INTEGER ::imode       ! Counter over all ensemble modes
INTEGER ::itime       ! Counter over timesteps
INTEGER ::istate
INTEGER ::iparam      ! Counter of number of parameters
INTEGER ::nstate      ! Number of states
INTEGER ::cnt         ! CTA_VECTOR vector of size of eigenmodes
INTEGER ::j           ! CTA_VECTOR vector of size of states
REAL :: cost_temp     ! total cost, working variable
REAL :: costi         ! cost at step time-step i
REAL :: x(nmodes)     ! state at each time step
REAL :: xinc(nmodes)  ! state increment at each timestep
REAL :: sgrad(nparam) ! gradient increment at each timestep
REAL :: rx(nmodes,nmodes)        ! linear state operator at each time step
REAL :: ralpha(nmodes, nparam)   ! linear parameter operator at each timestep
REAL :: y1(nobs)                 ! observations at analysis times
REAL :: y(nobs)                  ! observation increments
REAL :: error(nmodes)            ! observation increments
REAL :: adjforc(nanalysis, nobs) ! adjoint forcing at analysis steps
REAL :: lambda(nmodes)
REAL :: lambda_inc(nmodes)
REAL :: a
REAL :: b
REAL :: c


! ************************************************
! *** Compute Cost                             ***
! ************************************************

  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*      Compute the objective function     *'
  WRITE (*,'(10x,a)') '*                value                    *'
  WRITE (*,'(10x,a/)') '*******************************************'


  ! -Initial cost:
  cost_temp = 0.0

  ! Initialize x
  x = 0.0

  ! set observation counter
  cnt = 1

  !  Main loop

  DO itime= 1,nsteps
   
!    make local matrices for perturbations with respect to state and parameters.

     DO i = 1,nmodes
        DO  j = 1,nmodes
           rx(i,j) = Mx(itime,i,j) 
        ENDDO

        DO  k = 1,nparam
           ralpha(i,k) = Malpha(itime,i,k) 
        ENDDO
     ENDDO

!   run reduced model and collect state values in a vector

     xinc = MATMUL(rx,x) + MATMUL(ralpha,alpha)
     x = xinc

!   Check whether it is analysis time and if it is compute cost function
     costi = 0.0
     
     IF(itime == analysis(cnt) ) THEN
           
!   Compute the HX vector at analysis times

        y1 = MATMUL(observer,x)

!  Compute Inovations obs - y1

        y = obs(cnt,1:nobs) - y1

!  Divide each observation by std deviation           
        DO i = 1,nobs

! Divide each observation with std deviation and then self multiplty
           a = y(i) / obsstd(cnt,i)
           b = a * a
           costi = costi + b

! Good place to compute adjforcing as well (obs / obsstd ^2)

           c = a / obsstd(cnt,i)
           adjforc(cnt,i) = c

        ENDDO
        cnt = cnt + 1

     ENDIF

! increment the cost values

     cost_temp = costi + cost_temp

  ENDDO

! Values of the objective function

  cost = cost_temp


! *************************************************
! *** Compute the gradient by Backward Integration*
! *************************************************

  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*      Compute the gradient with respect  *'
  WRITE (*,'(10x,a)') '*                To parameters            *'
  WRITE (*,'(10x,a/)') '*******************************************'

  ! Initialize lambda
  lambda = 0.0
  grad = 0.0

  !  Main loop
  cnt = nanalysis
  DO itime= nsteps,1,-1
   
!    make local matrices for perturbations with respect to state and parameters.

     DO i = 1,nmodes
        DO  j = 1,nmodes
           rx(i,j) = Mx(itime,i,j) 
        ENDDO
                
        DO  k = 1,nparam
           ralpha(i,k) = Malpha(itime,i,k) 
        ENDDO
     ENDDO

!   run reduced model and compute gradient
     IF(itime == analysis(cnt) ) THEN

        lambda = MATMUL(TRANSPOSE(rx),lambda) 

        error = MATMUL(TRANSPOSE(observer), adjforc(cnt,1:nobs))

        lambda_inc = lambda + error

        sgrad = MATMUL(TRANSPOSE(ralpha),lambda) 

        grad = grad + sgrad
        lambda = lambda_inc
        cnt = cnt - 1

     ELSE
       
        lambda_inc = MATMUL(TRANSPOSE(rx),lambda) 
        sgrad = MATMUL(TRANSPOSE(ralpha),lambda) 

        lambda = lambda_inc
        grad = grad + sgrad

     ENDIF
  ENDDO

  status = stat

END SUBROUTINE sangoma_costgrad
