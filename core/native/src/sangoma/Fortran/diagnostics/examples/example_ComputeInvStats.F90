! $Id: example_ComStatF.F90  2015-04-15 Umer Altaf  $
!BOP
!
! !Program: example_compinvstats --- Compute innovation statistics
!
! !INTERFACE:
PROGRAM example_computeinvstats

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_ComStatF to compute the BIAS, RMS, MIN / MAX
! errors of the innovation y - Hx.
!
! The observations and predictions are read in from simple ASCII files.
!

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER :: i, iter              ! Counters
  INTEGER :: dim1               ! No. of timesteps
  INTEGER :: nfiles               ! Number of input files (=number of observation)
  CHARACTER(len=120) :: inpath, infile ! Path to and name stub of input files
  REAL, ALLOCATABLE :: obs(:, :)    ! Array holding observation
  REAL, ALLOCATABLE :: pred(:, :)    ! Array holding predictions
  REAL, ALLOCATABLE :: innovation(:, :)    ! Array holding predictions

  CHARACTER(len=2) :: obsstr           ! String for ensemble member

  ! Output variables of innovation routine
  INTEGER :: status = 0             ! Output of sangoma routine: status flag
  REAL, ALLOCATABLE :: rms_f(:)        ! rms of innovation
  REAL, ALLOCATABLE :: bias_f(:)       ! bias of innovation
  REAL, ALLOCATABLE :: min_f(:)        ! minimum of innovation
  REAL, ALLOCATABLE :: max_f(:)        ! maximum of innovation
  REAL :: p
!  CHARACTER(len=8) ::  dimstr

! ************************************************
! *** Configuration                            ***
! ************************************************

  ! Number of state files to be read
  nfiles = 5

  ! observation dimension
  dim1 = 4

  ! Path to and name of file holding model trajectory
  inpath = 'inputs/'
  infile = 'fieldA_'


! ************************************************
! *** Init                                     ***
! ************************************************

  WRITE (*,'(10x,a)') '***********************************'
  WRITE (*,'(10x,a)') '*         example_ComSta          *'
  WRITE (*,'(10x,a)') '*                                 *'
  WRITE (*,'(10x,a)') '*        Compute statics of       *'
  WRITE (*,'(10x,a)') '*            innovation           *'
  WRITE (*,'(10x,a/)') '**********************************'


! ************************
! *** Read state files ***
! ************************

  WRITE (*,'(/1x,a)') '------- Read observation -------------'
  WRITE (*,*) 'Read observation from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  ALLOCATE(obs(dim1, nfiles))

  read_obs: DO iter = 1, nfiles

     WRITE (obsstr, '(i1)') iter
     OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(obsstr)//'.txt', status='old')
 
     DO i = 1, dim1
        READ (11, *) obs(i, iter)
     END DO

     CLOSE(11)

  END DO read_obs

 
   infile = 'fieldC_'
    
   WRITE (*,'(/1x,a)') '------- Read predictions -------------'
   WRITE (*,*) 'Read prediction from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  ALLOCATE(pred(dim1, nfiles))
   
   read_pred: DO iter = 1, nfiles

     WRITE (obsstr, '(i1)') iter
     OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(obsstr)//'.txt', status='old')
 
     DO i = 1, dim1
        READ (11, *) pred(i, iter)
     END DO

     CLOSE(11)

  END DO read_pred


! *************************
! *** Compute Innovation ***
! *************************

  ALLOCATE(innovation(dim1, nfiles))

  WRITE (*,'(/1x,a)') '------- Compute innovations -------------'

  ! *** compute mean state ***
  
  DO i = 1, nfiles
     innovation(:,i) = obs(:,i) - pred(:, i)
  END DO

  

! *************************************************
! *** Call routine to perform statistics ***
! *************************************************

    ALLOCATE(rms_f(dim1))
    ALLOCATE(bias_f(dim1))
    ALLOCATE(min_f(dim1))
    ALLOCATE(max_f(dim1))
  

  call sangoma_computeinvstats(dim1,nfiles, innovation, &
       rms_f, bias_f,min_f, max_f,p, status)


WRITE (*,'(a,4e12.4)') 'RMS: ', rms_f
WRITE (*,'(a,4e12.4)') 'BAIS: ', bias_f
WRITE (*,'(a,4e12.4)') 'MINIMUM: ', min_f
WRITE (*,'(a,4e12.4)') 'MAXIMUM: ', max_f
WRITE (*,'(1x,a,1x,f12.5,1x,a)') 'p-value: ',p*100.0,'%'



! ****************
! *** Clean up ***
! ****************

   DEALLOCATE(obs, pred, innovation )

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_computeinvstats

