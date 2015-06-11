! $Id: example_ComputeEnsStats.F90 305 2014-03-14 08:56:44Z larsnerger $
!BOP
!
! !Program: example_compute_ensstats --- Compute ensemble statistics
!
! !INTERFACE:
PROGRAM example_compute_ensstats

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_ComputeEnsStats to compute skewness and
! kurtosis of an ensemble.
!
! The ensemble is read in from simple ASCII files.
!
! !REVISION HISTORY:
! 2013-11 - L. Nerger - Initial coding

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER :: i, iter              ! Counters
  INTEGER :: nstate               ! Size of model field
  INTEGER :: nfiles               ! Number of input files (=number of model states)
  CHARACTER(len=120) :: inpath, infile ! Path to and name stub of input files
  REAL, ALLOCATABLE :: states(:, :)    ! Array holding model states
  CHARACTER(len=2) :: ensstr           ! String for ensemble member

  ! Output variables of EOF routine
  INTEGER :: status = 0             ! Output of sangoma routine: status flag
  REAL :: skewness, kurtosis        ! Skewness and kurtosis of ensemble
  REAL, ALLOCATABLE :: meanstate(:) ! Array for mean state


! ************************************************
! *** Configuration                            ***
! ************************************************

  ! Number of state files to be read
  nfiles = 5

  ! State dimension
  nstate = 4

  ! Path to and name of file holding model trajectory
  inpath = 'inputs/'
  infile = 'fieldA_'


! ************************************************
! *** Init                                     ***
! ************************************************

  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*         example_ComputeEnsStats         *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*    Compute skewness and kurtosis        *'
  WRITE (*,'(10x,a)') '*            of an ensemble               *'
  WRITE (*,'(10x,a/)') '*******************************************'


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


! *************************
! *** Remove mean state ***
! *************************

  ALLOCATE(meanstate(nstate))

  WRITE (*,'(/1x,a)') '------- Compute and subtract mean state -------------'

  ! *** compute mean state ***
  meanstate = 0.0
  DO i = 1, nfiles
     meanstate(:) = meanstate(:) + states(:, i) / REAL(nfiles)
  END DO

  ! *** get peturbation matrix ***
  DO i = 1, nfiles
     states(:,i) = states(:,i) - meanstate(:)
  END DO


! *************************************************
! *** Call routine to perform EOF decomposition ***
! *************************************************

  call sangoma_computeensstats(nstate, nfiles, 0, &
       meanstate, states, skewness, kurtosis, status)

  WRITE (*,'(5x,a,es12.4)') 'skewness: ', skewness
  WRITE (*,'(5x,a,es12.4)') 'kurtosis: ', kurtosis


! ****************
! *** Clean up ***
! ****************

   DEALLOCATE(states, meanstate)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_compute_ensstats

