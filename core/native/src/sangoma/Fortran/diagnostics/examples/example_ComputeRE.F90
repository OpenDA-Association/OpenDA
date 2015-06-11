!BOP
!
! !Program: example_ComputeRE --- Compute Relative Entropy
!
! !INTERFACE:
PROGRAM example_computeRE

! !DESCRIPTION:
! This is an example showing how to use the routines
! sangoma_ComputeRE 
!
! The ensemble is read in from simple ASCII files.
!
! !REVISION HISTORY:
! 2014-03 - P. Kirchgessner - Initial code

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER           :: i            ! Counters
  REAL, ALLOCATABLE :: prior_w(:)   ! Prior weights
  REAL, ALLOCATABLE :: post_w(:)    ! Posterior weights
  REAL              :: RE           ! Output: the relative entropy
  INTEGER           :: dim_ens      ! Size of ensemble
  INTEGER           :: status       ! Status

! ************************************************
! *** Configuration                            ***
! ************************************************
  
  ! Initialize dimension of weights
  dim_ens = 5

  ! Allocate prior and posterior weights
  ALLOCATE(post_w(dim_ens), prior_w(dim_ens)) 
 

  ! Initialize prior weights (equal weights)
  prior_w = 1./dim_ens

    
  ! Initialize prior weights (equal weights)
  post_w(1) = 0
  post_w(2) = 0.25
  post_w(3) = 0.5
  post_w(4) = 0.25
  post_w(5) = 0




  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*        example_ComputeRE                *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*       Compute relative entropy          *'
  WRITE (*,'(10x,a/)') '******************************************'


! ***********************************
! *** Call routine to compute RE  ***
! ***********************************

  WRITE (*,'(/1x,a)') '------- Compute Relative Entropy -------------'

  CALL sangoma_computeRE(dim_ens,post_w,prior_w,RE,status)


  ! If succesful, print matrix 
  IF ( status == 0) THEN
     WRITE (*,'(/1x,a)') '----------- Results: -----------'

     WRITE (*,*) 'Inputs: '
     WRITE (*,*) 'Prior weights:'
     DO i = 1, dim_ens
        WRITE (*,*)  prior_w(i)
     END DO
  
     WRITE (*,'(/1x,a)') 'Posterior weights:' 
     DO i = 1,dim_ens
        WRITE (*,'(5es12.4)')   post_w(i)
     ENDDO

     WRITE (*,'(/1x,a)') 'The relative entropy is:' 
     WRITE (*,'(5es12.4)')  RE
     WRITE (*,'(1x,a)') '--------------------------------'

  ELSE 
     WRITE(*,*) 'Problem with computing relative entropy'
  END IF


! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(prior_w, post_w)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_computeRE
