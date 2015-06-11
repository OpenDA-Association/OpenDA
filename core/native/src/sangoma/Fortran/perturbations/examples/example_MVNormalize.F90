! $Id: generate_covar.F90 1165 2011-09-14 13:38:14Z lnerger $
!BOP
!
! !Program: example_mvnormalize --- Perform multivariate normalization
!
! !INTERFACE:
PROGRAM example_mvnormalize

! !DESCRIPTION:
! This program computes a multivariate normalization of a
! sequence of states consisting of several fields of different
! scale and variability. The model states are stored as
! vectors in simple ASCII files. All states are read into a
! matrix. Then the mean state is subtracted. The variability
! of each field is computed. Finally states are normalized so
! that all fields have unit variability. This example uses
! simple states with two different fields.
!
! Usually, the multivariate normalization is done in preparation
! of an EOF decomposition of the matrix holding the state 
! deviations about a mean state. For this, see the examples for 
! sangoma_EOFCovar.
!
! !REVISION HISTORY:
! 2013-11 - L. Nerger - Initial coding

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER :: i, iter              ! Counters
  INTEGER :: nstate               ! Size of model field
  INTEGER :: nfields              ! Number of model fields
  INTEGER :: nfiles               ! Number of input files (=number of model states)
  CHARACTER(len=120) :: inpath, infileA, infileB ! Path to and name stub of input files
  REAL, ALLOCATABLE :: states(:, :)    ! Array holding model states
  CHARACTER(len=2) :: ensstr           ! String for ensemble member

  ! Output variables of EOF routine
  INTEGER :: status = 0           ! Output of EOF routine: status flag
  REAL, ALLOCATABLE :: stddev(:)  ! Output of EOF routine: multivariate std. deviations of model fields
  REAL, ALLOCATABLE :: meanstate(:) ! Output of EOF routine: mean state
  INTEGER, ALLOCATABLE :: dim_fields(:)  ! Array holding field dimensions
  INTEGER, ALLOCATABLE :: offsets(:)     ! Array holding offsets of fields in state vectors


! ************************************************
! *** Configuration                            ***
! ************************************************

  ! Number of state files to be read
  nfiles = 5

  ! Number of model fields
  nfields = 2

  ! Size of a model field
  nstate = 4

  ! Path to and name of file holding model trajectory
  inpath = './inputs/'             ! Path to input files
  infileA = 'fieldA_'              ! Names of files for field A
  infileB = 'fieldB_'              ! Names of files for field B


! ************************************************
! *** Init                                     ***
! ************************************************

  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*           example_MVNormalize           *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*    Compute multivariate normalization   *'
  WRITE (*,'(10x,a)') '*         of a sequence of states.        *'
  WRITE (*,'(10x,a/)') '*******************************************'


! ************************
! *** Read state files ***
! ************************

  WRITE (*,'(/1x,a)') '------- Read states -------------'
  WRITE (*,*) 'Read states from files:  ',TRIM(inpath)//TRIM(infileA),'*.txt'
  WRITE (*,*) 'Read states from files:  ',TRIM(inpath)//TRIM(infileB),'*.txt'

  ALLOCATE(states(nfields*nstate, nfiles))

  read_in: DO iter = 1, nfiles

     ! Field A
     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(inpath)//TRIM(infileA)//TRIM(ensstr)//'.txt', status='old')
 
     DO i = 1, nstate
        READ (11, *) states(i, iter)
     END DO

     CLOSE(11)

     ! Field B
     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(inpath)//TRIM(infileB)//TRIM(ensstr)//'.txt', status='old')
 
     DO i = 1, nstate
        READ (11, *) states(i+nstate, iter)
     END DO

     CLOSE(11)

  END DO read_in


! *************************
! *** Remove mean state ***
! *************************

  ALLOCATE(meanstate(nfields*nstate))

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


! **********************************************************
! *** Call routine to perform multivariate normalization ***
! **********************************************************

  ALLOCATE(stddev(nfields))
  ALLOCATE(dim_fields(nfields))
  ALLOCATE(offsets(nfields))

  ! Set array of field dimensions
  dim_fields(:) = nstate
 
  ! Set array of field offsets
  offsets(1) = 0
  offsets(2) = nstate

  DO i = 1, nfields
     CALL sangoma_mvnormalize(1, nfields*nstate, dim_fields(i), offsets(i), &
          nfiles, states, stddev(i), status) 

     WRITE (*,'(5x,a,i5,a,es10.4)') 'Field', i, ': standard deviation ', stddev(i)
  END DO


! ****************
! *** Clean up ***
! ****************

   DEALLOCATE(states, meanstate)
   DEALLOCATE(stddev, dim_fields, offsets)


  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_mvnormalize
