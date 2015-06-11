! $Id: example_ComputeMutInf.F90 322 2014-03-20 17:18:40Z paulki $
!BOP
!
! !Program: example_computeMutInf --- Compute mutual information
!
! !INTERFACE:
PROGRAM example_computeMutInf

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_ComputeHistogram to compute a histrogram of
! the ensemble distribution.
!
! The ensemble is read in from simple ASCII files.
!
! !REVISION HISTORY:
! 2014-01 - P. Kirchgessner - Initial code

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER :: i, iter                   ! Counters
  INTEGER :: dim                       ! Size of model field
  INTEGER :: dim_ens                   ! Number of input files (=number of model states)
  CHARACTER(len=120) :: inpath, infile ! Path to and name stub of input files
  REAL, ALLOCATABLE :: states(:, :)    ! Array holding model states
  CHARACTER(len=2) :: ensstr           ! String for ensemble member
  INTEGER :: dim_obs                   ! number of observations
  INTEGER :: dim_sample                ! Number of sample points
  REAL, ALLOCATABLE :: obs(:)          ! observations
  REAL, ALLOCATABLE :: R(:,:)          ! Observation error covariance matrix
  REAL, ALLOCATABLE :: weights(:)      ! Prior weights
  REAL :: state_true(4)                ! True state


  ! Output variables of routine for mutual information
  REAL :: MI                           ! Output variable: The mutual information
  INTEGER :: status = 0                ! Output of sangoma routine: status flag

  EXTERNAL :: obs_op  ! Call-back routine for observation operator


! ************************************************
! *** Configuration                            ***
! ************************************************

  ! Number of state files to be read
  dim_ens = 5
  ! State dimension
  dim = 4
  ! Observation dimension
  dim_obs = 2
  
  ! Sample dimension
  dim_sample = 4
  
  ! Initialize weights (equal weights)

  ! Path to and name of file holding model trajectory
  inpath = 'inputs/'
  infile = 'fieldA_'
! ALLOCATE FIELDS
  ALLOCATE(obs(dim_obs))
  ALLOCATE(weights(dim_ens))
  ALLOCATE(R(dim_obs,dim_obs))


! ************************************************
! *** Init                                     ***
! ************************************************

  ! Initialize R
  !       | 0.5 0.25 |
  !  R =  | 0.25 0.5 |

  R(1,1) = 0.5
  R(1,2) = 0.25
  R(2,2) = 0.5
  R(2,1) = 0.25



  ! set all weights equal
  weights = 1./REAL(dim_ens)  
  

  ! initialize true state
  state_true(1) = 2.1
  state_true(2) = 2.3
  state_true(3) = 4.0
  state_true(4) = 1.3

  ! initialize observations by perturbing the true state
  call obs_op(1,dim,dim_obs,state_true,obs)
  

   
  ! add random error
  obs(1) = obs(1) + 0.1063
  obs(2) = obs(2) - 0.4359


  WRITE (*,'(10x,a)') '******************************************'
  WRITE (*,'(10x,a)') '*         example_ComputeMutInf          *'
  WRITE (*,'(10x,a)') '*                                        *'
  WRITE (*,'(10x,a)') '*       Compute mutual information       *'
  WRITE (*,'(10x,a/)') '******************************************'


! ************************
! *** Read state files ***
! ************************

  WRITE (*,'(/1x,a)') '------- Read states -------------'
  WRITE (*,*) 'Read states from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  ALLOCATE(states(dim, dim_ens))

  read_in: DO iter = 1, dim_ens

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstr)//'.txt', status='old')
 
     DO i = 1, dim
        READ (11, *) states(i, iter)
     END DO

     CLOSE(11)

  END DO read_in


! ********************************************
! *** Call routine to perform Sensetivity  ***
! ********************************************

  WRITE (*,'(/1x,a)') '------- Compute mutual information -------------'

  CALL sangoma_computeMutInf(dim, dim_ens, dim_obs, dim_sample, R, &
       obs, states, weights, obs_op, MI, status) 


!If succesful, print matrix 
  IF ( status == 0 ) THEN

     WRITE (*,'(/1x,a)') '------- Results: -------------'

     WRITE (*,*) 'Mutual information for this example:' 
     WRITE (*,'(5es12.4)')  MI
  	  
  ELSE 

     WRITE(*,*) 'Problem with computing mutual information'

  END IF


! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(states, obs, weights, R)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_computeMutInf
