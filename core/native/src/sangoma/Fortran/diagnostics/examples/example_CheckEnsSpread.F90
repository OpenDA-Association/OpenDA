! $Id: example_ComputeHistogram.F90 305 2014-03-14 08:56:44Z larsnerger $
!BOP
!
! !Program: example_CheckEnsSpread --- Compute ensemble histogram
!
! !INTERFACE:
PROGRAM example_CheckEnsSpread

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_CheckEnsSpread to check the ensemble spread
! against the deviation of the ensemble mean from 
! a true state.
!
! The ensemble is read in from simple ASCII files.
!
! !REVISION HISTORY:
! 2015-04 - L. Nerger - Initial coding

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER :: i, iter              ! Counters
  INTEGER :: nstate               ! Size of model field
  INTEGER :: nfiles               ! Number of input files (=number of model states)
  CHARACTER(len=120) :: inpath, infile ! Path to and name stub of input files
  REAL, ALLOCATABLE :: states(:, :)    ! Array holding model states
  REAL, ALLOCATABLE :: truestate(:)    ! True state
  CHARACTER(len=2) :: ensstr           ! String for ensemble member

  ! Output variables of ensemble checking routine
  REAL :: std_ens                      ! Ensemble standard deviation
  REAL :: std_state                    ! RMS deviation of ensembl emean state from truth


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
  WRITE (*,'(10x,a)') '*         example_CheckEnsSpread          *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*          Check ensmeble spread          *'
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


! *****************************************
! *** Declare true state for comparison ***
! *****************************************

  ALLOCATE(truestate(nstate))

  truestate(:) = 1.0


! *************************************************
! *** Call routine to perform EOF decomposition ***
! *************************************************

  call sangoma_checkensspread(nstate, nfiles, truestate, &
       states, std_ens, std_state)

  WRITE (*,'(/1x,a)') '------- Output of sangoma_CheckEnsSpread -------------'
  WRITE (*,*) 'ensemble standard deviation: ', std_ens
  WRITE (*,*) 'RMS deviation of mean state: ', std_state


! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(states, truestate)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_CheckEnsSpread

