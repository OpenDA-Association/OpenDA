! Copyright (c) 2015 Alexander Barth, a.barth@ulg.ac.be
!
! This routine is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, either version
! 3 of the License, or (at your option) any later version.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this software.  If not, see <http://www.gnu.org/licenses/>.


subroutine sangoma_checkNormality(n,x,alpha,H,pvalue,adstat)  &
   bind(C, name="sangoma_checknormality_")

 ! Performs an Anderson-Darling Test to test if a sample is from a Gaussian 
 ! distribution. The null hypothesis H0 is that the sample x follows a Gaussian 
 ! distribution. If H is 1, then this null hypothesis can be rejected at 
 ! the signficance level of alpha. It also returns the p-values (probability for
 ! a wrong H0 rejection) and the Anderson-Darling test statistic.

 use, intrinsic :: iso_c_binding
 use sangoma_base, only: realprec, intprec

 implicit none 

 integer(intprec), intent(in) :: n       ! number of samples
 real(realprec),   intent(in) :: x(n)    ! sample value to check
 real(realprec),   intent(in) :: alpha   ! significance level (fraction of 1)
 integer(intprec), intent(out) :: H      ! 1 if we reject H0 (otherwise 0)
 real(realprec),   intent(out) :: pvalue ! p-value for the test
 real(realprec),   intent(out) :: adstat ! Anderson-Darling test statistic

 interface
    function AndersonDarling(x,pvalue,adstat,alpha) result(H)
     implicit none  
     real, intent(in) :: x(:)
     real, intent(out), optional :: pvalue,adstat
     real, intent(in), optional :: alpha     
     logical :: H
    end function AndersonDarling
  end interface

  if (AndersonDarling(x,pvalue,adstat,alpha)) then
    H = 1
  else
    H = 0
  end if
  
  contains

 end subroutine sangoma_checkNormality


function AndersonDarling(x,pvalue,adstat,alpha) result(H)
  implicit none
  
  real, intent(in) :: x(:)
  real, intent(out), optional :: pvalue,adstat
  real, intent(in), optional :: alpha
  logical :: H

  interface
    function adcdf(AD) result(p)
     implicit none
     real :: AD, p
    end function adcdf
  end interface


  real :: stdx, meanx, adj, Asq, Asq_corrected, p, xn(size(x))
  integer :: n
  integer :: i, j(size(x))
  real :: alph = 0.05
  
  n = size(x)
  if (present(alpha)) alph = alpha

  xn = x
  call sisort(n,xn)

  adj = 1 + (.75 + 2.25/n)/n;
  
  meanx = sum(xn)/n
  stdx = sqrt(sum((xn - meanx)**2)/(n-1))

  xn = (xn - meanx)/stdx;
  xn = (1 - erf(xn / (-sqrt(2.)))) / 2;

  j = [(i,i=1,n)];
  Asq = -n - sum( (2*j-1) * (log(xn) + log(1-xn(n:1:-1))) )/n
  
  Asq_corrected = Asq*adj;
  p = adcdf(Asq_corrected);
  
  H = p < alph

  if (present(pvalue)) pvalue = p
  if (present(adstat)) adstat = Asq

end function AndersonDarling

function adcdf(AD) result(p)
 implicit none
  real :: AD, p

  if (AD >= 0.6) then
    p = exp(1.2937 - 5.709*AD+ 0.0186*AD**2)
  elseif (0.34 <= AD .and. AD < .6) then
    p = exp(0.9177 - 4.279*AD - 1.38*AD**2)
  elseif (0.2 <= AD .and. AD < 0.34) then
    p = 1 - exp(-8.318 + 42.796*AD- 59.938*AD**2)
  else
    p = 1 - exp(-13.436 + 101.14*AD- 223.73*AD**2)
  end if

 end function adcdf



SUBROUTINE sisort(n, veca)

! Sorts a vector veca(1:n) into ascending numerical order, by
! straight insertion.
!
! For large vectors, this routine will not be efficient.

  IMPLICIT NONE

  INTEGER, INTENT(in) :: n 
  REAL, INTENT(inout) :: veca(n)

  INTEGER :: i, j, k 
  REAL :: tmpa
  LOGICAL :: eflag

  DO j = 2, n

     eflag = .false.

     tmpa = veca(j) 

     sortloop: DO i = j-1, 1, -1 
        k = i

        IF(veca(i) <= tmpa) THEN
           eflag = .true.
           EXIT sortloop
        END IF

        veca(i+1) = veca(i) 
     ENDDO sortloop

     IF (.NOT.eflag) k=0

     veca(k+1) = tmpa 

  ENDDO 

END SUBROUTINE sisort

