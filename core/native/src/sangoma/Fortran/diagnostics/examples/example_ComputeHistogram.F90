! $Id: example_ComputeHistogram.F90 305 2014-03-14 08:56:44Z larsnerger $
!BOP
!
! !Program: example_computehistogram --- Compute ensemble histogram
!
! !INTERFACE:
PROGRAM example_computehistogram

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_ComputeHistogram to compute a histrogram of
! the ensemble distribution.
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

  ! Output variables of histogram routine
  INTEGER :: status = 0                ! Output of sangoma routine: status flag
  INTEGER :: ncall                     ! Number of calls to histogram routine
  REAL :: delta                        ! delta value for histogram quality
  REAL, ALLOCATABLE :: centralstate(:) ! Central state
  INTEGER, ALLOCATABLE :: hist(:)      ! Histogram array


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
  WRITE (*,'(10x,a)') '*        example_ComputeHistogram         *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*       Compute ensemble histogram        *'
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


! ********************************************
! *** Declare central state for comparison ***
! ********************************************

  ALLOCATE(centralstate(nstate))

  centralstate(:) = 1.0


! *************************************************
! *** Call routine to perform EOF decomposition ***
! *************************************************

  ALLOCATE(hist(nfiles+1))
  hist = 0.0
  ncall = 1    ! Inital call to the histogram routine

  call sangoma_computehistogram(ncall, nstate, nfiles, 0, &
       centralstate, states, hist, delta, status)

  WRITE (*,*) 'histogram: ', hist



! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(states, centralstate, hist)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_computehistogram

