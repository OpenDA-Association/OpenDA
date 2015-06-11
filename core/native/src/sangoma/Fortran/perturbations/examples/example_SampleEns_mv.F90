! $Id: generate_covar.F90 1165 2011-09-14 13:38:14Z lnerger $
!BOP
!
! !Program: example_SampleEns_mv --- Generate a multivariate ensemble
!
! !INTERFACE:
PROGRAM example_SampleEns_mv

! !DESCRIPTION:
! This program is an extension of example_eofcovar_mv.
! Here, a set of state is read from ASCII files. Then an
! EOF decomposition is computed using sangoma_eofcovar.
! The output from this tool is used to call sangoma_SampleEns
! which generates a random ensemble with mean given by the 
! prescribed state, and covariance matrix as given by the
! EOFs and scaled singular vectors.
!
! The size of the ensemble is always equal to the number of EOFs
! plus one. In the example sangoma_SampleEns is called providing
! all EOFs, but one could reduce the number of EOFs to reduce the
! size of the genrated ensemble.
!
! !REVISION HISTORY:
! 2015-04 - L. Nerger - Initial coding

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER :: i, iter              ! Counters
  INTEGER :: nstate               ! Size of model field
  INTEGER :: nfields              ! Number of model fields
  INTEGER :: nfiles               ! Number of input files (=number of model states)
  CHARACTER(len=120) :: inpath, infileA, infileB ! Path to and name stub of input files
  CHARACTER(len=120) :: outfile_eof, outfile_mstate ! Names of output files
  CHARACTER(len=120) :: outfile_svals, outfile_ens  ! Names of output files
  REAL, ALLOCATABLE :: states(:, :)    ! Array holding model states
  CHARACTER(len=2) :: ensstr           ! String for ensemble member

  ! Output variables of EOF routine
  INTEGER :: status = 0           ! Output of EOF routine: status flag
  INTEGER :: do_mv                ! 1: to Perform multivariate normalization
  REAL, ALLOCATABLE :: stddev(:)  ! Output of EOF routine: multivariate std. deviations of model fields
  REAL, ALLOCATABLE :: svals(:)   ! Output of EOF routine: Singular values
  REAL, ALLOCATABLE :: svecs(:,:) ! Output of EOF routine: Singular vectors
  REAL, ALLOCATABLE :: meanstate(:) ! Output of EOF routine: mean state
  REAL, ALLOCATABLE :: ens(:,:)   ! Output of Ensemble sampling routine: State ensemble
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

  ! Flag for multivariate normalization
  do_mv = 1

  ! Path to and name of file holding model trajectory
  inpath = './inputs/'             ! Path to input files
  infileA = 'fieldA_'              ! Names of files for field A
  infileB = 'fieldB_'              ! Names of files for field B

  ! Names of output files
  outfile_eof = 'eof_'             ! Files holding EOFs
  outfile_svals = 'svals.txt'      ! Files holding singular values
  outfile_mstate = 'meanstate.txt' ! Files holding mean state
  outfile_ens = 'ens_'             ! Files holding ensemble states


! ************************************************
! *** Init                                     ***
! ************************************************

  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*          example_SampleEns_MV           *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*   Generate a random ensemble of model   *'
  WRITE (*,'(10x,a)') '*    states from a sequence of states.    *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*     Example for multivariate EOFs.      *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a/)') '*******************************************'

! *********************************************************************
! *** The first part is the EOF decomposition with sangoma_eofcovar ***
! *** This is identical to exmaple_EOFcovar_mv!                     ***
! *********************************************************************


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


! *************************************************
! *** Call routine to perform EOF decomposition ***
! *************************************************

  ALLOCATE(svals(nfiles))
  ALLOCATE(svecs(nfields*nstate, nfiles))
  ALLOCATE(meanstate(nfields*nstate))
  ALLOCATE(stddev(nfields))
  ALLOCATE(dim_fields(nfields))
  ALLOCATE(offsets(nfields))

  ! Set array of field dimensions
  dim_fields(:) = nstate
 
  ! Set array of field offsets
  offsets(1) = 0
  offsets(2) = nstate

  CALL sangoma_eofcovar(nfields*nstate, nfiles, nfields, dim_fields, offsets, &
       1, do_mv, states, stddev, svals, svecs, meanstate, status)

  WRITE (*,'(5x,a)') 'Scaled singular values: '
  DO i = 1, nfiles-1
    WRITE (*, '(10x, i4, es12.3)') i, svals(i)
  END DO


! *********************************************************
! *** Write mean state and decomposed covariance matrix ***
! *********************************************************

  WRITE (*,'(/1x,a)') '------- Write decomposed covariance matrix and mean state -------------'

  ! *** Write singular values ***
  WRITE (*,*) 'Write singular values to file: ',TRIM(outfile_svals)
  OPEN(11, file = TRIM(outfile_svals), status='replace')
  DO i = 1, nfiles-1
     WRITE (11, *) svals(i)
  END DO
  CLOSE(11)

  ! *** Write EOFs ***
  WRITE (*,*) 'Write eofs to files: ',TRIM(outfile_eof),'*.txt'
  writing: DO iter = 1, nfiles-1

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(outfile_eof)//TRIM(ensstr)//'.txt', status='replace')
 
     DO i = 1, nfields*nstate
        WRITE (11, *) svecs(i, iter)
     END DO

     CLOSE(11)

  END DO writing

  ! *** Write mean state ***
  WRITE (*,*) 'Write meanstate to file: ',TRIM(outfile_mstate)
  OPEN(11, file = TRIM(outfile_mstate), status='replace')
  DO i = 1, nfields*nstate
     WRITE (11, *) meanstate(i)
  END DO
  CLOSE(11)


! ***************************************************
! *** Call routine to generate ensemble of states ***
! ***************************************************

  WRITE (*,'(/1x,a)') '------- Generate ensemble states --------------------------------------'

  ALLOCATE(ens(nfields*nstate, nfiles))

  CALL sangoma_SampleEns(nfields*nstate, nfiles, svecs, svals, meanstate, &
       ens, status)

  ! *** Write EOFs ***
  WRITE (*,'(/1x,a)') '------- Write state ensemble ------------------------------------------'
  WRITE (*,*) 'Write ensemble states to files: ',TRIM(outfile_ens),'*.txt'
  writing2: DO iter = 1, nfiles

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(outfile_ens)//TRIM(ensstr)//'.txt', status='replace')
 
     DO i = 1, nfields*nstate
        WRITE (11, *) ens(i, iter)
     END DO

     CLOSE(11)

  END DO writing2


! ********************
! *** Finishing up ***
! ********************

   DEALLOCATE(states, meanstate)
   DEALLOCATE(svals, svecs)
   DEALLOCATE(stddev, dim_fields, offsets)
   DEALLOCATE(ens)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_SampleEns_mv

