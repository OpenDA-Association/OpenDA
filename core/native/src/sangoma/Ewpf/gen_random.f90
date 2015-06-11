! $Id: gen_random.f90 547 2015-02-19 12:32:23Z sp709689 $

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!    Subsection of subroutines needed for the equivalent weights 
!!!    particle filter code supplied for SANGOMA project by 
!!!    Sanita Vetra-Carvalho. Code given here was developed by 
!!!    Philip A. Browne for EMPIRE <http://www.met.reading.ac.uk/~darc/empire>.
!!!
!!!    Collection of subroutines to make multidimensional random arrays
!!!    Copyright (C) 2015  Sanita Vetra-Carvalho
!!!
!!!    This program is distributed under the Lesser General Public License (LGPL) version 3,
!!!    for more details see <https://www.gnu.org/licenses/lgpl.html>.
!!!
!!!    Email: s.vetra-carvalho @ reading.ac.uk
!!!    Mail:  School of Mathematical and Physical Sciences,
!!!    	      University of Reading,
!!!	      Reading, UK
!!!	      RG6 6BB
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> generate one dimension of uniform random numbers 
Subroutine UniformRandomNumbers1D(minv, maxv, n,phi)
use sangoma_base, only: REALPREC, INTPREC
implicit none

integer(INTPREC), intent(in) :: n                ! size of output vector
real(REALPREC), intent(in) :: minv               ! minimum value of uniform distribution
real(REALPREC), intent(in) :: maxv               ! maximum value of uniform distribution
real(REALPREC), dimension(n), intent(out) :: phi ! n dimensional uniform random numbers

call random_number(phi)

phi = minv + (maxv-minv)*phi
end Subroutine UniformRandomNumbers1D

! generate one or two dimension array of Normal random numbers 
Subroutine NormalRandomNumbers(mean,stdev,n,k,phi)
use sangoma_base, only: REALPREC, INTPREC
use random
IMPLICIT NONE

integer(INTPREC), INTENT(IN) :: n                  ! first dimension of output vector
integer(INTPREC), INTENT(IN) :: k                  ! second dimension of output vector
real(REALPREC), INTENT(IN) :: mean                ! mean of normal distribution
real(REALPREC), INTENT(IN) :: stdev               ! Standard Deviation of normal distribution
real(REALPREC), dimension(n,k), INTENT(OUT) :: phi  ! n,k dimensional normal random numbers
integer :: i,j

do i = 1,n
   do j = 1,k
      phi(i,j) = mean+stdev*random_normal()
   end do
end do

End Subroutine NormalRandomNumbers


subroutine MixtureRandomNumbers1D(mean,stdev,ufac,epsi,n,phi,uniform)
! Subroutine to generate one dimensional vector drawn from mixture density.
use random
use sangoma_base, only: REALPREC, INTPREC
implicit none

real(REALPREC), intent(in) :: mean                ! Mean of normal distribution
real(REALPREC), intent(in) :: stdev               ! Standard deviation of normal distribution
real(REALPREC), intent(in) :: ufac                ! half-width of uniform distribution
                                                  ! that is centered on the mean
real(REALPREC), intent(in) :: epsi                ! Proportion controlling mixture draw
                                                  ! if random_number > epsi then draw from 
                                                  ! uniform, else normal
integer(INTPREC),intent(in) :: n                  ! size of output vector
real(REALPREC), dimension(n), intent(out) :: phi  ! n dimensional mixture random numbers
logical, intent(out) :: uniform                   ! True if mixture drawn from uniform. 
                                                  ! False if drawn from normal
real(REALPREC) :: draw

call random_number(draw)

if(draw .gt. epsi) then
   call UniformRandomNumbers1D(mean-ufac,mean+ufac, n,phi)
   uniform = .true.
else
   call NormalRandomNumbers(mean,stdev,n,1,phi)
   uniform = .false.
end if

end subroutine MixtureRandomNumbers1D


subroutine MixtureRandomNumbers2D(mean,stdev,ufac,epsi,n,k,phi,uniform)
! Subroutine to generate two dimensional vector, each drawn from mixture density.
use random
use sangoma_base, only: REALPREC, INTPREC
implicit none

real(REALPREC), intent(in) :: mean                ! Mean of normal distribution
real(REALPREC), intent(in) :: stdev               ! Standard deviation of normal distribution
real(REALPREC), intent(in) :: ufac                ! half-width of uniform distribution
                                                  ! that is centered on the mean
real(REALPREC), intent(in) :: epsi                ! Proportion controlling mixture draw
                                                  ! if random_number > epsi then draw from 
                                                  ! uniform, else normal
integer(INTPREC),intent(in) :: n,k                ! first and second dimension of output vector
real(REALPREC), dimension(n,k), intent(out) :: phi  ! n,k dimensional mixture random numbers
logical, dimension(k), intent(out) :: uniform                   ! k dimensional logical with uniform(i) True if
                                                  ! phi(:,i) drawn from uniform. 
                                                  ! False if drawn from normal
real(REALPREC) :: draw
integer(INTPREC):: i

do  i = 1,k
   call random_number(draw)

   if (draw .gt. epsi) then
      call UniformRandomNumbers1D(mean-ufac,mean+ufac,n,phi(:,i))
      uniform(i) = .true.
   else
      call NormalRandomNumbers(mean,stdev,n,1,phi(:,i))
      uniform(i) = .false.
   end if
end do
end subroutine MixtureRandomNumbers2D


