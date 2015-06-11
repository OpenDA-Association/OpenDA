! !Program: example_ComputeCRPS.F90 --- Compute CRPS & reliability/resolution partition
!
! !INTERFACE:
PROGRAM example_computecrps

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_ComputeCRPS to compute the CRPS and its partition
!
! The ensemble is read in from simple ASCII files.
!
! !REVISION HISTORY:
! 2014-04 - G. Candille - Initial coding
!           L. Nerger   - Sort routines

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

  ! Output variables of rcrv routine
  REAL :: crps       ! global score CRPS
  REAL :: reli       ! reliability part of the CRPS
  REAL :: resol      ! resolution part of the CRPS
  REAL :: unc        ! uncertainty part of the CRPS
  REAL, ALLOCATABLE :: aa(:)    ! coef for computing CRPS partition
  REAL, ALLOCATABLE :: bb(:)    ! coef for computing CRPS partition

  ! Call-back routines for sorting
  EXTERNAL :: sisort, &
       sisort2


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
  WRITE (*,'(10x,a)') '*        example_ComputeCRPS               *'
  WRITE (*,'(10x,a)') '*                                          *'
  WRITE (*,'(10x,a)') '*    Compute CRPS & decomposition          *'
  WRITE (*,'(10x,a/)') '*******************************************'


! ************************
! *** Read state files ***
! ************************

  WRITE (*,'(/1x,a)') '------- Read states -------------'
  WRITE (*,*) 'Read states from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  ALLOCATE(xens(m, nens))
  ALLOCATE(xobs(m))
  ALLOCATE(missing(m))
  ALLOCATE(aa(nens+1))
  ALLOCATE(bb(nens+1))

  read_in: DO i = 1, nens

     WRITE (ensstr, '(i1)') i
     OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstr)//'.txt', status='old')
 
     DO k = 1, m
        READ (11, *) xens(k, i)
     END DO

     CLOSE(11)

  END DO read_in

  ! Observations
  xobs = (/ 0.78, 2.47, 3.21, 1.13 /)
  ! No missing
  DO k = 1, m
     missing(k) = 0.
  ENDDO


! ****************************************************
! ***        Call routine to compute CRPS          ***
! ****************************************************

  call sangoma_computeCRPS(xens,xobs,missing,m,nens,crps,reli,resol,unc,bb,aa,sisort,sisort2)

  WRITE (*,*) 'CRPS = Reli + Resol: ', crps, reli, resol
  WRITE (*,*) 'uncertainty: ', unc


! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(xens,xobs,missing,aa,bb)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_computecrps
