! !Program: example_ComputeBRIER.F90 --- Compute Brier score and reliability/resolution decomposition
!
! !INTERFACE:
PROGRAM example_computebrier

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_ComputeBRIER to compute the Brier score
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
  REAL, ALLOCATABLE :: xth(:)        ! Array holding thresholds

  ! Output variables of rcrv routine
  REAL :: br     ! global Brier skill score
  REAL :: brc    ! reliability component of the Brier skill score
  REAL :: brv    ! resolution component of the Brier skill score
  REAL :: unc    ! uncertainty component of the Brier skill score
  REAL :: pc     ! climatological probability
  REAL :: s      ! entropy score
  REAL :: sunc   ! entropy for climatology
  REAL, ALLOCATABLE :: pp(:)    ! predicted probabilities related to thresholds xth
  REAL, ALLOCATABLE :: g(:)     ! distribution of predicted probabilities
  REAL, ALLOCATABLE :: pr(:)    ! predictable (observable) probabilities


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
  WRITE (*,'(10x,a)') '*        example_ComputeBRIER             *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*   Compute Brier score & decomposition   *'
  WRITE (*,'(10x,a/)') '*******************************************'


! ************************
! *** Read state files ***
! ************************

  WRITE (*,'(/1x,a)') '------- Read states -------------'
  WRITE (*,*) 'Read states from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  ALLOCATE(xens(m, nens))
  ALLOCATE(xobs(m))
  ALLOCATE(xth(m))

  ALLOCATE(pp(nens+1))
  ALLOCATE(g(nens+1))
  ALLOCATE(pr(nens+1))

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
  ! Same thresholds
  DO k = 1, m
     xth(k) = 1.
  ENDDO


! ****************************************************
! ***     Call routine to compute Brier score      ***
! ****************************************************

  call sangoma_computeBRIER(m,nens,xens,xobs,xth,br,brc,brv,unc,pc,s,sunc,pp,g,pr)

  WRITE (*,*) 'Brier skill score:         ', br
  WRITE (*,*) 'reliability & resolution:  ', brc, brv
  WRITE (*,*) 'climatology & uncertainty: ', pc, unc
  WRITE (*,*) 'entropy & uncertainty:     ', s, sunc
  WRITE (*,*) 'PP,g'                       , pp,g
  WRITE (*,*) 'pp,pr'                      , pp, pr
! (pp,g) enables to draw the sharpness diagram
! (pp,pr) enables to draw the reliability diagram 


! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(xens,xobs,xth,pp,g,pr)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_computebrier

