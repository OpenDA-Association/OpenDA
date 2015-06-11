! $Id: example_ComputeHistogram.F90 305 2014-03-14 08:56:44Z larsnerger $
!BOP
!
! !Program: example_CheckWhiteness --- check for whiteness of innovations
!
! !INTERFACE:
PROGRAM example_checkpvalue

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_CheckWhiteness to check for the whiteness
! of a sequence of innovations.
!
! The ensemble is read in from simple ASCII files.
!
! !REVISION HISTORY:
! 2015-04 - L. Nerger - Initial coding

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER :: i       ! Counter
  INTEGER :: dim     ! Size of innovation vector
  INTEGER :: ntimes  ! Number of times in innovation array
  REAL, ALLOCATABLE :: innovations(:,:) ! Array of innovations
  REAL, ALLOCATABLE :: rms_f(:) ! Array of innovations
  REAL, ALLOCATABLE :: bias_f(:) ! Array of innovations
  REAL, ALLOCATABLE :: max_f(:) ! Array of innovations
  REAL, ALLOCATABLE :: min_f(:) ! Array of innovations
  
  INTEGER :: iseed(4) ! Seed for random number generator
  
  ! Output variables of routine to check for whiteness
  INTEGER :: status = 0             ! Output of sangoma routine: status flag
  REAL :: Qvalue     ! Q-value of Ljung-Box test
  REAL :: pvalue     ! P-value of significance of Ljung-Box test result


! ************************************************
! *** Configuration                            ***
! ************************************************

  ! Number of times in innovation array
  ntimes = 10

  ! Dimension of innovation vector
  dim = 1000


! ************************************************
! *** Init                                     ***
! ************************************************

  WRITE (*,'(10x,a)') '********************************************'
  WRITE (*,'(10x,a)') '*        example_Ttest                     *'
  WRITE (*,'(10x,a)') '*                                          *'
  WRITE (*,'(10x,a)') '*  Based on innovation matrix gives PValue *'
  WRITE (*,'(10x,a/)') '********************************************'


! ************************************
! *** Initialize innovations array ***
! ************************************

  ALLOCATE(innovations(dim, ntimes))
 ALLOCATE(bias_f(dim))
 ALLOCATE(rms_f(dim))
 ALLOCATE(min_f(dim))
 ALLOCATE(max_f(dim))
  
  ! Set seed for random numbers
  iseed(1) = 1000
  iseed(2) = 2034
  iseed(3) = 0
  iseed(4) = 3

  DO i = 1, ntimes
     ! Generate uncorrelated innovations from random numbers
     CALL dlarnv(3, iseed, dim, innovations(1,i))

     ! Nor introduce some correlation
     innovations(1:dim-20, i) = 1.0
  END DO


! ******************************
! *** Perform whiteness test ***
! ******************************

  call sangoma_computeinvstats(dim, ntimes, innovations, rms_f, bias_f, min_f,max_f,Pvalue,status)

  WRITE (*,'(1x,a)') 'Significance of:'
  WRITE (*,'(1x,a,1x,f12.5,1x,a)') 'p-value: ',pvalue*100.0,'%'


! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(innovations)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_checkpvalue

