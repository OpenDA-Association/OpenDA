! check with
! gfortran -g -fdefault-real-8  test_sangoma_utils.F90  sangoma_utils.F90 -llapack -lblas

program test_sangoma_utils
 use sangoma_utils
  real, parameter :: tolerance = 1e-6

 call test_symeig()
 call test_symsqrtm()
 call test_sort()
 call test_unique()
 call test_ind2sub()

contains

  subroutine test_sort
   implicit none
   integer, parameter :: n = 10
   integer :: ind(n)
   integer, dimension(1:n) :: A = &  
        (/0, 50, 20, 25, 90, 10, 5, 20, 99, 75/), sortedA
   
   sortedA = A
   call sort(sortedA,ind)
   call assert(all(sortedA(1:n-1) <= sortedA(2:n)),'quick sort (1)')
   call assert(all(sortedA == A(ind)),'quick sort (2)')
  end subroutine test_sort

 subroutine test_symeig()
  implicit none
  integer, parameter :: n = 2
  real :: A(n,n)
  real :: E(n), V(n,n), Atest(n,n)
  integer :: i
  

  A(1,:) = [2, 1]
  A(2,:) = [1, 2]

  call symeig(A,E,V)
  
  Atest = transpose(V)
  do i = 1,size(E)
    Atest(i,:) = E(i) * Atest(i,:)
  end do
  Atest = matmul(V,Atest)

  call assert(A,Atest,tolerance,'symeig')

 end subroutine test_symeig

 subroutine test_symsqrtm()
  implicit none
  real :: A(3,3)
  real :: S(size(A,1),size(A,1))

  A = reshape([2.,1.,1., 1.,2.,1., 1.,1.,2.],[3,3])
  S = symsqrtm(A)

  call assert(matmul(S,S), &
      A, &
      tolerance,'sqrtm factorization')
 end subroutine test_symsqrtm

 subroutine test_unique
   implicit none
   integer, parameter :: n = 10
   integer :: ind(n), ind2(n), i, nu
   integer, dimension(1:n) :: A = &  
        (/0, 20, 20, 25, 90, 10, 5, 20, 90, 75/), sortedA, uA, c
!        (/0, 20, 20, 25, 30, 40, 50, 50, 90, 175/), sortedA, uA, c
   
   uA = A
   call unique(uA,nu,ind,ind2)

   ! all elements in A must be one time in uA
   do i = 1,n
     if (count(A(i) == uA(1:nu)) /= 1) then
       write(6,*) 'unique (1): FAILED'
       stop
     end if
   end do

   write(6,*) 'unique (1): OK'

   call assert(all(uA(1:nu) == A(ind(1:nu))),'unique (2)')
   call assert(all(A == uA(ind2)),'unique (3)')

  end subroutine test_unique


 subroutine test_ind2sub
  implicit none
  integer :: sub(3)

  sub = ind2sub([10,20,30],5)
  call assert(all(sub == [5,1,1]),'ind2sub (1)')

  sub = ind2sub([10,20,30],510)
  call assert(all(sub == [10,11,3]),'ind2sub (2)')

 end subroutine test_ind2sub

end program test_sangoma_utils
