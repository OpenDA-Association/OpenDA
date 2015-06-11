! !Program: example_ComputeRCRV.F90 --- Compute ensemble bias/dispersion from RCRV
!
! !INTERFACE:
PROGRAM example_computercrv

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_ComputeRCRV to compute the bias and the dispersion of
! the RCRV 
!
! The ensemble is read in from simple ASCII files.
!
! !REVISION HISTORY:
! 2014-04 - G. Candille - Initial coding

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER :: k, i                 ! Counters
  INTEGER :: nens                 ! Number of input files (=ensemble size)
  INTEGER :: m                    ! Number of ensemble realizations 
  CHARACTER(len=120) :: inpath, infile ! Path to and name stub of input files
  CHARACTER(len=2) :: ensstr           ! String for ensemble member

  REAL, ALLOCATABLE :: xens(:, :)    ! Array holding model ensembles
  REAL, ALLOCATABLE :: xobs(:)       ! Array holding observations
  REAL, ALLOCATABLE :: missing(:)    ! Array holding missing flags
  REAL :: sig0                       ! observation error

  ! Output variables of rcrv routine
  REAL :: b      ! bias from RCRV
  REAL :: d      ! dispersion from RCRV


! ************************************************
! *** Configuration                            ***
! ************************************************

  ! Number of files to be read = ensemble size
  nens = 5
  ! Number of realizations of the ensemble process
  m = 4

  ! Path to and name of file holding model trajectory
  inpath = 'inputs/'
  infile = 'fieldA_'

! ************************************************
! *** Init                                     ***
! ************************************************

  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*        example_ComputeRCRV              *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*    Compute RCRV- bias/dispersion        *'
  WRITE (*,'(10x,a/)') '*******************************************'


! ************************
! *** Read state files ***
! ************************

  WRITE (*,'(/1x,a)') '------- Read states -------------'
  WRITE (*,*) 'Read states from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  ALLOCATE(xens(m, nens))
  ALLOCATE(xobs(m))
  ALLOCATE(missing(m))

  read_in: DO i = 1, nens

     WRITE (ensstr, '(i1)') i
     OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstr)//'.txt', status='old')
 
     DO k = 1, m
        READ (11, *) xens(k, i)
     END DO

     CLOSE(11)

  END DO read_in

  ! Observations and error
  xobs = (/ 0.78, 2.47, 3.21, 1.13 /)
  sig0 = 0.26
  ! No missing
  DO k = 1, m
     missing(k) = 0.
  ENDDO


! ****************************************************
! *** Call routine to compute RCRV bias/dispersion ***
! ****************************************************

  call sangoma_computeRCRV(xens,xobs,sig0,missing,m,nens,b,d)

  WRITE (*,*) 'bias: ', b
  WRITE (*,*) 'dispersion: ', d


! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(xens,xobs,missing)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_computercrv

