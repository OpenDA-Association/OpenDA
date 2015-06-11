! !Program: example_computehistogram --- Compute ensemble histogram
!
! !INTERFACE:
program example_checkNormality

! !DESCRIPTION:
! This is an example showing how to use the routine
! sangoma_checkNormality to make an Anderson-Darling Test
! and to test whether the distribution is Gaussian
!
! The random sample is read in from simple ASCII files.
!
! !REVISION HISTORY:
! 2015-04 - Alexander Barth - Initial coding

! !USES:
 use, intrinsic :: iso_c_binding
  implicit none
!EOP
  
 integer, parameter :: n = 100  ! sample size
 character(len=256) :: fname    ! file name for sample
 integer :: i
 real :: x(n), pvalue, adstat
 integer :: H
 real :: alpha = 0.05

 fname = 'inputs/checkNormality.txt'

 write (*,'(10x,a)') '********************************************'
 write (*,'(10x,a)') '*           example_checkNormality         *'
 write (*,'(10x,a)') '*                                          *'
 write (*,'(10x,a)') '*            Anderson-Darling Test         *'
 write (*,'(10x,a)') '********************************************'

! ****************************
! *** Load the sample data ***
! ****************************

 open(11, file = fname, status='old')
 
 do i = 1,n
   read (11,*) x(i)
 end do
 
 close(11)

! **************************************************************
! ***        Call routine to make the Anderson-Darling Test  ***
! **************************************************************


 call sangoma_checkNormality(n,x,alpha,H,pvalue,adstat)

 write(6,*) 'Null hypthesis: sample is normal distributed'

 if (H == 1) then   
   write(6,*) 'Null hypthesis is rejected (alpha=',alpha,')'
 else
   write(6,*) 'Null hypthesis could not be rejected (alpha=',alpha,')'
 end if

 write(6,*) 'p-value',  pvalue*100,'%'
 write(6,*) 'Anderson-Darling test statistic',  adstat

 call big_test()

contains

 subroutine big_test()
  implicit none
  INTEGER :: iseed(4) ! Seed for random number generator
  integer, parameter :: n = 100
  integer, parameter :: ntimes = 1000
  
  integer :: count = 0
  real :: x(n), pvalue, adstat
  logical :: H
  real :: alpha = 0.05

  ! Set seed for random numbers
  iseed(1) = 1000
  iseed(2) = 2034
  iseed(3) = 0
  iseed(4) = 3

  write (*,*) 'Making ',ntimes,'Tests'

  do i = 1,ntimes
    ! Generate uncorrelated innovations from random numbers
    CALL dlarnv(3, iseed, n, x)

    call sangoma_checkNormality(n,x,alpha,H,pvalue,adstat)
    if (H) then
      count = count+1
    end if
  end do

  write(6,*) 'Percentage of rejection: ',100.*count/ntimes
  write(6,*) 'Should be close to:      ',100*alpha

 end subroutine big_test
end program example_checkNormality

