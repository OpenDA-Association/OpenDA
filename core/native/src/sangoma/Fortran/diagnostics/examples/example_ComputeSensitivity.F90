! $Id: example_ComputeSensitivity.F90 305 2014-03-14 08:56:44Z larsnerger $
!BOP
!
! !Program: example_ComputeSensitivity --- Compute sensitivity
!
! !INTERFACE:
PROGRAM example_computeSensitivity

! !DESCRIPTION:
! This is an example showing how to use the routines
! sangoma_ComputeSensitivity and
! sangoma_Compute_sensitivity_op to compute the 
! sensitivity of the posterior ensemble mean to the
! observations
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
  INTEGER :: nstate                    ! Size of model field
  INTEGER :: nfiles                    ! Number of input files (=number of model states)
  CHARACTER(len=120) :: inpath, infile ! Path to and name stub of input files
  REAL, ALLOCATABLE :: states(:, :)    ! Array holding model states
  CHARACTER(len=2) :: ensstr           ! String for ensemble member
  INTEGER :: nobs                      ! number of observations
  REAL, ALLOCATABLE :: obs(:)          ! observations
  REAL, ALLOCATABLE :: H(:,:)          ! Linear observation matrix
  REAL, ALLOCATABLE :: weights(:)      ! particle weights
  REAL, ALLOCATABLE :: R(:,:)          ! observation error covariance matrix

  ! Output variables of Sensitivity routine
  INTEGER :: status = 0                ! Output of sangoma routine: status flag
  INTEGER :: status2 = 0               ! Output of sangoma routine: status flag
  REAL, ALLOCATABLE :: sensitivity(:,:)    ! Sensitivity matrix
    REAL, ALLOCATABLE :: sensitivity2(:,:) ! Sensitivity matrix
  REAL, ALLOCATABLE :: PA(:, :)        ! Posterior matrix
  REAL, ALLOCATABLE :: PA2(:, :)       ! Posterior matrix

  EXTERNAL :: obs_op  ! Call-back routine for observation operator


! ************************************************
! *** Configuration                            ***
! ************************************************

  ! Number of state files to be read
  nfiles = 5
  ! State dimension
  nstate = 4
  ! Observation dimension
  nobs = 2
  ! Initialize weights (equal weights)
  
  ! Path to and name of file holding model trajectory
  inpath = 'inputs/'
  infile = 'fieldA_'

! ALLOCATE FIELDS
  ALLOCATE(obs(nobs))
  ALLOCATE(H(nobs,nstate))
  ALLOCATE(weights(nfiles))
  ALLOCATE(R(nobs,nobs))
  !output
  ALLOCATE(sensitivity(nobs,nobs))
  ALLOCATE(sensitivity2(nobs,nobs))
  ALLOCATE(PA(nstate, nstate))
  ALLOCATE(PA2(nstate, nstate))


! ************************************************
! *** Init                                     ***
! ************************************************

  ! Initialize H
  !       | 0 1 0 0 |
  !  H =  | 0 0 0 1 |

  H = 0.0
  H(1,2) =1.0 
  H(2,4) =1.0

  ! Initialize R
  !       | 0.5 0.25 |
  !  R =  | 0.25 0.5 |

  R(1,1) = 0.5
  R(1,2) = 0.25
  R(2,2) = 0.5
  R(2,1) = 0.25

  ! Initialize weights (equal weights)
  weights = 1./REAL(nfiles)


  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*        example_ComputeSensitivity       *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*       Compute ensemble sensitivity      *'
  WRITE (*,'(10x,a/)') '******************************************'


! ************************
! *** Read state files ***
! ************************

  WRITE (*,'(/1x,a)') '------- Read states -------------'
  WRITE (*,*) 'Read states from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  ALLOCATE(states(nstate, nfiles))

  read_in: DO iter = 1, nfiles

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstr)//'.txt', status='old')
 
     DO i = 1, nstate
        READ (11, *) states(i, iter)
     END DO

     CLOSE(11)

  END DO read_in


! ********************************************
! *** Call routine to perform Sensetivity  ***
! ********************************************

  WRITE (*,'(/1x,a)') '------- Compute sensitivity -------------'

  ! Sensitivity routine with observation operator given by matrix
  CALL sangoma_computesensitivity(nstate, nfiles,nobs, R, H, states,weights, &
       sensitivity, PA, status)

  ! Sensitivity routine with observation operator provided in call-back routine
  CALL sangoma_computesensitivity_op(nstate, nfiles,nobs, R, states,weights, &
       sensitivity2, PA2, obs_op, status2)

  ! If succesful, print matrix 
  IF ( status == 0 .AND. status2==0) THEN
     WRITE (*,'(/1x,a)') '----------- Results: -----------'
     WRITE (*,'(/1x,a)') '--------- H as matrix ----------'

     WRITE (*,*) 'Sensitivity matrix: '
     DO i = 1, nobs
        WRITE (*,'(2es12.4)')  sensitivity(:,i)
     END DO
  
     WRITE (*,'(/1x,a)') 'Posterior Covariance matrix:' 
     DO i = 1,nstate
        WRITE (*,'(5es12.4)')  PA(:,i)  
     ENDDO

     WRITE (*,'(1x,a)') '--------------------------------'
     WRITE (*,'(/1x,a)') '-------- H as operator----------'
 
     WRITE (*,*) 'Sensitivity matrix: '
     DO i = 1, nobs
        WRITE (*,'(2es12.4)')  sensitivity2(:,i)
     END DO

     WRITE (*,'(/1x,a)') 'Posterior Covariance matrix:' 
     DO i = 1,nstate
        WRITE (*,'(5es12.4)')  PA2(:,i)  
     ENDDO
  
     WRITE (*,'(1x,a)') '--------------------------------'
     WRITE (*,'(/1x,a)') '--------- Difference: ----------'
  
     WRITE (*,*) 'Sensitivity matrix: '
     DO i = 1, nobs
        WRITE (*,'(2es12.4)')  sensitivity(:,i)-sensitivity2(:,i)
     END DO
 
     WRITE (*,'(/1x,a)') 'Posterior Covariance matrix:' 
     DO i = 1,nstate
        WRITE (*,'(5es12.4)')  PA(:,i)-PA2(:,i)  
     ENDDO
     WRITE (*,'(1x,a)') '--------------------------------'

  ELSE 
     WRITE(*,*) 'Problem with computing sensitivity'
  END IF


! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(states,obs,H,weights,R,sensitivity,PA,sensitivity2,PA2)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_computeSensitivity
