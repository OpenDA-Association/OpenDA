!BOP
!
! !Program: example_computeeffSample--- Compute effective sample size
!
! !INTERFACE:
PROGRAM example_ComputeEffSample

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_ComputeEffSample to compute a the effective sample
! size of an ensemble of particles given their weights
!
!
! !REVISION HISTORY:
! 2014-06 - P. Kirchgessner - Initial coding

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER              :: dim_sample
  REAL, ALLOCATABLE    :: weights(:)

  ! Output variables of histogram routine
  INTEGER :: status = 0                ! Output of sangoma routine: status flag
  REAL :: effSample                    ! Effective sample size

  EXTERNAL :: sangoma_computeESS

! ************************************************
! *** Configuration                            ***
! ************************************************

  ! Sample size
  dim_sample = 10

! ************************************************
! *** Init                                     ***
! ************************************************

  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*        example_ComputeEffSample         *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*       Compute effective sample size     *'
  WRITE (*,'(10x,a/)') '******************************************'


! *****************************************************
! *** Call routine to compute effective Sample size ***
! *****************************************************

  ALLOCATE(weights(dim_sample))
  weights = (/0.0239,0.1470,0.1450,0.0735,0.1212,0.0215, &
                     0.0639,0.1387,0.1200,0.1453/) 

  CALL sangoma_computeESS(dim_sample, weights, effSample, status)

  WRITE (*,*) 'Effective sample size is: ', effSample



! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(weights)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_ComputeEffSample

