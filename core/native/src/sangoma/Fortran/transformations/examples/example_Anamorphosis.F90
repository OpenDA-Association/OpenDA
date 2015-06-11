! !Program: example_Anamorphosis.F90 --- Illustrate anamorphosis transformation
!
! !INTERFACE:
PROGRAM example_anamorphosis

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_ComputeQuantiles to compute ensemble quantiles, and
! sangoma_Anamorphosis using these quantiles to perform
!        anamorphosis transformation (forward and backward)
!
! !REVISION HISTORY:
! 2015-02 - J.-M. BRankart - Initial coding

! !USES:
  USE sangoma_base, ONLY: REALPREC, INTPREC
  IMPLICIT NONE

! Local variables
  INTEGER(INTPREC) :: i        ! Counters
  INTEGER(INTPREC) :: status   ! Output status of SANGOMA subroutines
  INTEGER(INTPREC) :: dim      ! State vector dimension
  INTEGER(INTPREC) :: m        ! Ensemble size
  INTEGER(INTPREC) :: q        ! Number of quantiles
  INTEGER(INTPREC) :: dir      ! Forward (+1) or backward (-1) transformation

  REAL(REALPREC), ALLOCATABLE :: xens(:,:)  ! Array holding state ensemble
  REAL(REALPREC), ALLOCATABLE :: yens(:,:)  ! Array holding observation ensemble
  REAL(REALPREC), ALLOCATABLE :: xqua(:,:)  ! Array holding state ensemble quantiles
  REAL(REALPREC), ALLOCATABLE :: yqua(:,:)  ! Array holding observation ensemble quantiles
  REAL(REALPREC), ALLOCATABLE :: qua_def(:) ! Definition of quantiles
  REAL(REALPREC), ALLOCATABLE :: qua_ref(:) ! Quantiles of the target distribution
  REAL(REALPREC) :: xmean                   ! State ensemble mean
  REAL(REALPREC) :: ymean                   ! Observation ensemble mean
  REAL(REALPREC) :: xstd                    ! State ensemble mean
  REAL(REALPREC) :: ystd                    ! Observation ensemble mean
  REAL(REALPREC) :: xycov                   ! Covariance between state and observation
  REAL(REALPREC) :: xycor                   ! Linear correlation between state and observation

  ! Call-back routines for sorting
  EXTERNAL :: sisort

! ************************************************
! *** Configuration                            ***
! ************************************************

  ! Ensemble size
  m = 200

  ! Number and definition of quantiles
  q = 11
  ALLOCATE(qua_def(q))
  qua_def = (/ 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0 /)

  ! Quantiles of the target distribution,
  ! where the above quantiles of the ensemble will be remapped
  ! (chosen here so that the transformed ensemble is close to Gaussian)
  ALLOCATE(qua_ref(q))
  qua_ref = (/ -2.576, -1.282, -0.842, -0.524, -0.253, 0.0, 0.253, 0.524, 0.842, 1.282, 2.576 /)

! ************************************************
! *** Init                                     ***
! ************************************************

  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*        example_Anamorphosis              *'
  WRITE (*,'(10x,a)') '*                                          *'
  WRITE (*,'(10x,a)') '*  Illustrate anamorphosis transformation  *'
  WRITE (*,'(10x,a)') '*******************************************'

! ************************
! *** Prior ensemble   ***
! ************************

  WRITE (*,'(/1x,a)') '------- Generate prior ensemble -------'

  WRITE (*,'(1x,a,i10)') 'Ensemble size:',m

  dim = 1  ! State vector dimension equal to 1 in this example
  ALLOCATE(xens(dim,m),yens(dim,m))

  WRITE (*,'(1x,a)') 'Bounded distribution in [0,1]'

  ! generate random state ensemble
  CALL random_number(xens)   ! uniform distribution in [0,1]
  xens=4*xens*(1-xens)       ! nonlinear transformation to have a "weird" prior
  xens=4*xens*(1-xens)       ! distribution, still defined in the interval [0,1]

  WRITE (*,'(1x,a)') 'Nonlinear (monotonic) observation operator'

  ! compute observation equivalent of every member
  yens=xens**4               ! nonlinear observation operator (must be monotonic)

! *******************************************************************
! *** Define anamorphosis transformation using ensemble quantiles ***
! *******************************************************************

  WRITE (*,'(/1x,a)') '------- Quantiles of the prior ensemble -------'

  ! Quantiles of state ensemble
  ALLOCATE(xqua(dim,q))
  CALL sangoma_computequantiles(dim,m,q,qua_def,xens,xqua,sisort)

  ! Quantiles of observation ensemble
  ALLOCATE(yqua(dim,q))
  CALL sangoma_computequantiles(dim,m,q,qua_def,yens,yqua,sisort)

  WRITE (*,'(1x,a)') '  QUANTILE   STATE      OBSERVATION'
  DO i=1,q
    WRITE(*,'(3f11.6)') , qua_def(i),xqua(1,i), yqua(1,i)
  ENDDO

  ! Compute correlation between state and observation
  xmean = SUM(xens)/m
  ymean = SUM(yens)/m
  xstd  = SQRT(SUM((xens-xmean)*(xens-xmean))/(m-1))
  ystd  = SQRT(SUM((yens-ymean)*(yens-ymean))/(m-1))
  xycov = SUM((xens-xmean)*(yens-ymean))/(m-1)
  xycor = xycov / (xstd*ystd)

  WRITE (*,'(1x,a,f11.6)') 'Linear correlation between state and observation:',xycor

! *************************************************
! *** Apply forward anamorphosis transformation ***
! *************************************************
  dir = +1  ! forward anamorphosis

  WRITE (*,'(/1x,a)') '------- Transform the ensemble to the Gaussian-like variables -------'

  ! On the ensemble of states
  DO i=1,m
    CALL sangoma_anamorphosis(dim,q,dir,qua_ref,xqua,xens(:,i),status)
  ENDDO

  ! On the ensemble of observations
  DO i=1,m
    CALL sangoma_anamorphosis(dim,q,dir,qua_ref,yqua,yens(:,i),status)
  ENDDO

  ! Compute correlation between state and observation
  xmean = SUM(xens)/m
  ymean = SUM(yens)/m
  xstd  = SQRT(SUM((xens-xmean)*(xens-xmean))/(m-1))
  ystd  = SQRT(SUM((yens-ymean)*(yens-ymean))/(m-1))
  xycov = SUM((xens-xmean)*(yens-ymean))/(m-1)
  xycor = xycov / (xstd*ystd)

  WRITE (*,'(1x,a,f11.6)') 'Linear correlation between state and observation:',xycor

! ***************************************
! *** Update the transformed ensemble ***
! ***************************************

  WRITE (*,'(/1x,a)') '------- Update of the transformed ensemble -------'

  ! Assume that the transformed ensemble,
  ! which should be distributed as N(0,1),
  ! is updated linearly according to:
  xens = 0.5 * xens  ! standard deviation divided by 2
  xens = xens - 1.0  ! mean shifted by -1

  WRITE (*,'(1x,a)') 'Standard deviation is divided by 2'
  WRITE (*,'(1x,a)') 'Mean is shifted by -1'

! **************************************************
! *** Apply backward anamorphosis transformation ***
! **************************************************
  dir = -1  ! backward anamorphosis

  WRITE (*,'(/1x,a)') '------- Transform the posterior ensemble back to the original variables -------'

  ! On the ensemble of states
  DO i=1,m
    CALL sangoma_anamorphosis(dim,q,dir,qua_ref,xqua,xens(:,i),status)
  ENDDO

  WRITE (*,'(/1x,a)') '------- Quantiles of the posterior ensemble -------'

  ! Quantiles of the updated ensemble
  CALL sangoma_computequantiles(dim,m,q,qua_def,xens,xqua,sisort)

  WRITE (*,'(1x,a)') '  QUANTILE   STATE'
  DO i=1,q
    WRITE(*,'(2f11.6)') , qua_def(i),xqua(1,i)
  ENDDO

  WRITE (*,'(1x,a)') 'The updated ensemble is still bounded in [0,1]'
  WRITE (*,'(1x,a)') 'Median is shifted; dispersion is smaller'

END PROGRAM example_anamorphosis

