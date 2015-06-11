! $Id: generate_covar.F90 1165 2011-09-14 13:38:14Z lnerger $
!BOP
!
! !Program: example_SampleEns --- Generate an ensemble from a covar. matrix
!
! !INTERFACE:
PROGRAM example_SampleEns

! !DESCRIPTION:
! This program generates a random ensemble of model states based
! on the output from the tool sangoma_eofcovar. 
! The information on the covariance matrix (EOFs and singular
! values) and the central state of the ensemble are read from files.
! Then sangoma_SampleEns is called to generate the random ensemble
! with the prescribed mean state and ensemble covariance matrix
! identical to the covariance matrix given by the EOFs and singular 
! values.
!
! The size of the ensemble is always equal to the number of EOFs
! plus one. In the example sangoma_SampleEns is called providing
! all EOFs, but one could reduce the number of EOFs to reduce the
! size of the generated ensemble.
!
! !REVISION HISTORY:
! 2015-04 - L. Nerger - Initial coding

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER :: i, iter              ! Counters
  INTEGER :: dim_state            ! Size of state vector
  INTEGER :: neofs                ! Number of input files (=number of model states)
  CHARACTER(len=120) :: infile_eof, infile_svals  ! Names of input files for covariance matrix 
  CHARACTER(len=120) :: infile_mstate  ! Name of input file for ensemble mean state
  CHARACTER(len=120) :: outfile_ens    ! Names of output files
  CHARACTER(len=2) :: ensstr           ! String for ensemble member

  ! Output variables of ensemble sampling routine
  INTEGER :: status = 0           ! Output of EOF routine: status flag
  REAL, ALLOCATABLE :: svals(:)   ! Output of EOF routine: Singular values
  REAL, ALLOCATABLE :: svecs(:,:) ! Output of EOF routine: Singular vectors
  REAL, ALLOCATABLE :: meanstate(:) ! Output of EOF routine: mean state
  REAL, ALLOCATABLE :: ens(:,:)   ! Output of Ensemble sampling routine: State ensemble


! ************************************************
! *** Configuration                            ***
! ************************************************

  ! Number of EOFs to be read
  neofs = 4

  ! Size of state vector
  dim_state = 8  ! To be compatible with example_EOFcovar_mv this has to be nfields*nstate

  ! Names of output files
  infile_eof = 'eof_'             ! Files holding EOFs
  infile_svals = 'svals.txt'      ! Files holding singular values
  infile_mstate = 'meanstate.txt' ! Files holding mean state
  outfile_ens = 'ensB_'           ! Files holding ensemble states


! ************************************************
! *** Init                                     ***
! ************************************************

  WRITE (*,'(10x,a)') '**************************************************'
  WRITE (*,'(10x,a)') '*               example_SampleEns                *'
  WRITE (*,'(10x,a)') '*                                                *'
  WRITE (*,'(10x,a)') '*      Generate a random ensemble of model       *'
  WRITE (*,'(10x,a)') '*    states from an EOF-decomposed covariance    *'
  WRITE (*,'(10x,a)') '*      matrix and a prescribed mean state.       *'
  WRITE (*,'(10x,a)') '*                                                *'
  WRITE (*,'(10x,a)') '*  Example for output from example_EOFcovar_mv.  *'
  WRITE (*,'(10x,a)') '*                                                *'
  WRITE (*,'(10x,a/)') '**************************************************'


  ALLOCATE(svals(neofs))
  ALLOCATE(svecs(dim_state, neofs))
  ALLOCATE(meanstate(dim_state))


! ************************************************
! *** Read covariance matrix and cnetral state ***
! ************************************************

  WRITE (*,'(/1x,a)') '------- Read decomposed covariance matrix and mean state -------------'
  WRITE (*,'(1x,a)') 'NOTE: If you do not have input files:'
  WRITE (*,'(1x,a)') 'The inputs are the output files example_EOFcovar_mv. Please run this first.'

  ! *** Read singular values ***
  WRITE (*,*) 'Read singular values from file: ',TRIM(infile_svals)
  OPEN(11, file = TRIM(infile_svals), status='old')
  DO i = 1, neofs
     READ (11, *) svals(i)
  END DO
  CLOSE(11)

  ! *** READ EOFs ***
  WRITE (*,*) 'Read EOFs from files: ',TRIM(infile_eof),'*.txt'
  reading: DO iter = 1, neofs

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(infile_eof)//TRIM(ensstr)//'.txt', status='old')
 
     DO i = 1, dim_state
        READ (11, *) svecs(i, iter)
     END DO

     CLOSE(11)

  END DO reading

  ! *** Read mean state ***
  WRITE (*,*) 'Read meanstate from file: ',TRIM(infile_mstate)
  OPEN(11, file = TRIM(infile_mstate), status='old')
  DO i = 1, dim_state
     READ (11, *) meanstate(i)
  END DO
  CLOSE(11)


! ***************************************************
! *** Call routine to generate ensemble of states ***
! ***************************************************

  WRITE (*,'(/1x,a)') '------- Generate ensemble states --------------------------------------'

  ALLOCATE(ens(dim_state, neofs+1))

  CALL sangoma_SampleEns(dim_state, neofs+1, svecs, svals, meanstate, &
       ens, status)

  ! *** Write EOFs ***
  WRITE (*,'(/1x,a)') '------- Write state ensemble ------------------------------------------'
  WRITE (*,*) 'Write ensemble states to files: ',TRIM(outfile_ens),'*.txt'
  writing2: DO iter = 1, neofs+1

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(outfile_ens)//TRIM(ensstr)//'.txt', status='replace')
 
     DO i = 1, dim_state
        WRITE (11, *) ens(i, iter)
     END DO

     CLOSE(11)

  END DO writing2


! ********************
! *** Finishing up ***
! ********************

   DEALLOCATE(meanstate)
   DEALLOCATE(svals, svecs)
   DEALLOCATE(ens)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_SampleEns

