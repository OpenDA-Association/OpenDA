! some helper functions
! mainly to simplify porting from octave/matlab to Fortran

! if mask is a boolean vector
!
! mean(...) -> sum(...) / N
! repmat -> spread
! sum(mask) -> count(mask)
! find(mask) -> pack([(i,1:size(mask))],mask)
! array(mask) -> pack(array,mask)
! a' -> transpose(a)
! a*b -> matmul(a,b)
module sangoma_utils

interface randn
  module procedure &
    randn_vec, &
    randn_mat
end interface

interface assert
  module procedure assert_bool
  module procedure assert_scal
  module procedure assert_vec
  module procedure assert_mat
end interface 

interface load
  module procedure load_vec
  module procedure load_mat
end interface 

interface save
  module procedure save_vec
  module procedure save_mat
end interface 

contains

!_______________________________________________________
!
! create the identity matrix
!

 function eye(n) result(E)
  implicit none
  integer, intent(in) :: n
  real(4) :: E(n,n)
  integer :: i

  E = 0.

  do i=1,n
    E(i,i) = 1.
  end do

 end function 

!_______________________________________________________
!
! create a matrix filled with uniformly distributed 
! random number between 0 and 1
!

 function rand(n,m) result(E)
  implicit none
  integer, intent(in) :: n,m
  real :: E(n,m)

  call random_number(E)
 end function 

!_______________________________________________________
!
! create a matrix filled with gaussian distributed 
! random number with 0 mean and 1 standard deviation
!
! FIXME: implement the "Box-Muller transformation"
!

 function randn_mat(n,m) result(E)
  implicit none
  integer, intent(in) :: n,m
  real :: E(n,m)

  integer :: i

  E = rand(n,m);
  do i=1,11
    E = E + rand(n,m);
  end do

  E = E-6
 end function 

!_______________________________________________________
!
! create a vector filled with gaussian distributed 
! random number with 0 mean and 1 standard deviation

 function randn_vec(n) result(E)
  implicit none
  integer, intent(in) :: n
  real :: E(n), tmp(n)

  integer :: i

  call random_number(E)
  do i=1,11
    call random_number(tmp)
    E = E + tmp;
  end do

  E = E-6
 end function 

  subroutine assert_bool(cond,msg)
   logical, intent(in) :: cond
   character(len=*) :: msg
   
   if (cond) then
     write(6,*) msg, ': OK '
   else
     write(6,*) msg, ': FAIL '
     stop
   end if
  end subroutine assert_bool


  subroutine assert_scal(found,expected,tol,msg)
   real, intent(in) :: found, expected, tol
   character(len=*) :: msg
   
   real :: maxdiff
   maxdiff = abs(found - expected)
   
   if (maxdiff < tol) then
     write(6,*) msg, ': OK '
   else
     write(6,*) msg, ': FAIL ', maxdiff
     write(6,*) 'found ',found
     write(6,*) 'expected ',expected

     stop
   end if


  end subroutine assert_scal

  subroutine assert_vec(found,expected,tol,msg)
   real, intent(in) :: found(:), expected(:), tol
   character(len=*) :: msg
   
   real :: maxdiff
   maxdiff = maxval(abs(found - expected))
   
   if (maxdiff < tol) then
     write(6,*) msg, ': OK '
   else
     write(6,*) msg, ': FAIL ', maxdiff
     write(6,*) 'found ',found
     write(6,*) 'expected ',expected

     stop
   end if


  end subroutine assert_vec

  subroutine assert_mat(found,expected,tol,msg)
   real, intent(in) :: found(:,:), expected(:,:), tol
   character(len=*) :: msg
   
   real :: maxdiff
   maxdiff = maxval(abs(found - expected))
   
   if (maxdiff < tol) then
     write(6,*) msg, ': OK '
   else
     write(6,*) msg, ': FAIL ', maxdiff
!     write(6,*) 'found ',found
!     write(6,*) 'expected ',expected

     stop
   end if


  end subroutine assert_mat


function inv(A,det) result(B)
 implicit none
 real, intent(in) :: A(:,:)
 real, optional, intent(out) :: det

 real :: B(size(A,1),size(A,2))

 integer :: IPIV(min(size(A,1),size(A,2))), info, lwork,i
 real :: worksize
 real, allocatable :: work(:)


! write(6,*) 'shape ',shape(B),size(B,1),size(A,1)
 B = A
 call dgetrf(size(A,1),size(A,2), B, size(A,1), IPIV, INFO )

 if (present(det)) then
   det = 1.
   do i=1,size(B,1)
     if (ipiv(i).ne.i) then
       det = -det * b(i,i)
     else
       det = det * b(i,i)
     end if
   end do
 end if

 lwork = -1
 call dgetri(size(A,1), B, size(A,1), IPIV, worksize, -1, INFO )
 lwork = worksize+.5
 allocate(work(lwork))

 call dgetri(size(A,1), B, size(A,1), IPIV, work, lwork, INFO )
 deallocate(work)


end function 

!_______________________________________________________
!
! computes eigenvalue/-vector of a symetric matrix
!
! A = V diag(E) V'

 subroutine symeig(A,E,V,nbiggest,nsmallest,indices,info)
  implicit none
  real,    intent(in)  :: A(:,:)
  real,    intent(out) :: E(size(A,1))
  integer, optional, intent(in) :: nbiggest, nsmallest,indices(2)
  real,    optional, target, intent(out) :: V(:,:)
  integer, optional, intent(out) :: info

  character :: jobz
  real, pointer :: pV(:,:)
  real :: rlwork, tmp
  integer :: lwork, myinfo, N, iwork(5*size(A,1)), ifail(size(A,1)), i,j
  real, allocatable :: work(:)

  ! LAPACK Machine precision routine

  real :: slamch
  integer :: r,idummy,ind(2)


#ifndef ALLOCATE_LOCAL_VARS
  real :: copyA(size(A,1),size(A,2))
#else
  real, pointer :: copyA(:,:)
  allocate(copyA(size(A,1),size(A,2)))
#endif



  jobz='n'
  N = size(A,1)
  r = n

  if (present(V)) then
    jobz='v'
    pV => V
  else
    allocate(pV(1,1))
  end if

  ind = (/ 1,n /)

  if (present(nbiggest))  ind = (/ n-nbiggest+1,n /)
  if (present(nsmallest)) ind = (/ 1,nsmallest /)
  if (present(indices))   ind = indices

  ! protect content of A

  copyA = A

  ! determine the optimal size of work

  call dsyevx(JOBZ,'I','U',n,copyA,n,real(-1.,kind(A)),real(-1.,kind(A)),ind(1),ind(2),     &
       2*SLAMCH('S'),idummy,E,pV,n,rlWORK,-1, IWORK,   &
       IFAIL, myinfo )

  lwork = rlwork+0.5
  allocate(work(lwork))

  call dsyevx(JOBZ,'I','U',n,copyA,n,real(-1.,kind(A)),real(-1.,kind(A)),ind(1),ind(2),     &
       2*SLAMCH('S'),idummy,E,pV,n, WORK, LWORK, IWORK,   &
       IFAIL, myinfo )

  if (present(nbiggest)) then
    ! sort in descending order
    do i=1,nbiggest/2
      tmp = E(i)
      E(i) = E(nbiggest-i+1)
      E(nbiggest-i+1) = tmp

      if (present(V)) then
        do j=1,n
          tmp = V(j,i) 
          V(j,i) = V(j,nbiggest-i+1)
          V(j,nbiggest-i+1) = tmp
        end do
      end if
    end do
  end if

  deallocate(work)
#ifdef ALLOCATE_LOCAL_VARS
  deallocate(copyA)
#endif


  if (.not.present(V)) deallocate(pV)
  if (present(info)) info = myinfo
 end subroutine symeig



 ! computes the principal square root of the matrix A
 ! It is assumed that A is a real symmetric 
 ! positive definite matrix.

 function symsqrtm(A) result(S)
  real, intent(in) :: A(:,:)
  real :: S(size(A,1),size(A,1)), E(size(A,1))

  call symeig(A,E,S)
  S = matmul(S,matmul(diag(sqrt(E)),transpose(S)))
 end function symsqrtm

 ! returns all unique elements of A (inplace)
 ! n is the number of unique elements
 !
 ! ind is a vector of indices for all unique elements in A
 ! Anew(1:n) = Aold(ind)
 ! Aold = Anew(ind2)


  ! sort vector A in-place
  ! optional argument ind is the sort index
  ! such that
  ! sortedA = A
  ! call sort(sortedA,ind)
  ! all(sortedA,A(ind)) is true

#define DATA_TYPE integer 

  subroutine sort(A,ind)
   DATA_TYPE, intent(inout), dimension(:) :: A
   integer, intent(out), dimension(size(A)), optional :: ind
   
   integer :: sort_index(size(A)), i
   
   sort_index = [(i, i=1,size(A))]
   
   call sort_(A,sort_index)
   if (present(ind)) ind = sort_index

  contains
   recursive subroutine sort_(A,ind)
    DATA_TYPE, intent(inout), dimension(:) :: A
    integer, intent(inout), dimension(:) :: ind
    integer :: iq

    if (size(A) > 1) then
      call sort_partition(A, ind, iq)
      call sort_(A(:iq-1),ind(:iq-1))
      call sort_(A(iq:),ind(iq:))
    end if
   end subroutine sort_

   subroutine sort_partition(A, ind, marker)
    DATA_TYPE, intent(inout), dimension(:) :: A
    integer, intent(inout), dimension(:) :: ind
    integer, intent(out) :: marker
    integer :: i, j, tempind
    DATA_TYPE :: temp
    DATA_TYPE :: x      ! pivot point

    x = A(1)
    i = 0
    j = size(A) + 1
    
    do
      j = j-1
      do
        if (A(j) <= x) exit
        j = j-1
      end do
      
      i = i+1
      do
        if (A(i) >= x) exit
        i = i+1
      end do
      

      if (i < j) then
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp

        tempind = ind(i)
        ind(i) = ind(j)
        ind(j) = tempind        
      else if (i == j) then
        marker = i+1
        return
      else
        marker = i
        return
      end if
    end do
    
   end subroutine sort_partition

  end subroutine sort

 
 subroutine unique(A,n,ind,ind2)
  DATA_TYPE, intent(inout) :: A(:)
  integer, intent(out) :: n
  integer, intent(out), dimension(size(A)), optional :: ind, ind2

  integer :: unique_index(size(A)), unique_index2(size(A)), i

  unique_index = [(i, i=1,size(A))]

  call unique_(A,n,unique_index,unique_index2)
  if (present(ind)) ind = unique_index
  if (present(ind2)) ind2 = unique_index2

 contains
  subroutine unique_(A,n,ind,ind2)
   DATA_TYPE, intent(inout) :: A(:)
   integer, intent(out) :: n
   integer, intent(out), dimension(size(A)) :: ind
   integer, intent(out), dimension(size(A)) :: ind2

   integer :: i
   integer, dimension(size(A)) :: sort_ind

   ind2 = 1
   call sort(A,sort_ind)

   n = 1
   do i = 1,size(A)-1
     ind2(sort_ind(i)) = n

     A(n) = A(i)
     ind(n) = sort_ind(i)

     if (A(i) /= A(i+1)) then
       n = n+1
     end if
   end do

   A(n) = A(size(A))
   ind(n) = sort_ind(size(A))
   ind2(sort_ind(size(A))) = n

  end subroutine unique_
 end subroutine unique


 function diag(d) result(A)
  implicit none
  real, intent(in) :: d(:)
  real :: A(size(d),size(d))
  integer :: i

  A = 0.

  do i=1,size(d)
    A(i,i) = d(i)
  end do

 end function diag


 ! same as ind2sub in matlab/octave
 ! create subscripts from a linear index ind
 function ind2sub(dims,ind) result(sub)
  implicit none

  integer, intent(in) :: dims(:),ind
  real :: sub(size(dims))  
  integer :: i,j,tmp,offset(size(dims))

  offset(1) = 1
  do j = 2,size(dims)
    offset(j) = offset(j-1) * dims(j-1)
  end do

  ! tmp is here 0-based
  tmp = ind - 1

  do j = size(dims), 1, -1
    ! integer division
    sub(j) = tmp / offset(j)
    tmp = tmp - sub(j) * offset(j)
  end do

  ! make sub 1-based
  sub = sub+1
 end function ind2sub

 subroutine load_vec(fname,A)
  implicit none
  character(len=*), intent(in) :: fname
  real, intent(out) :: A(:)
  integer :: i

  open(11, file = fname, status='old')  
  do i = 1,size(A,1)
    read (11,*) A(i)
  end do  
 close(11)
  
end subroutine load_vec

 subroutine load_mat(fname,A)
  implicit none
  character(len=*), intent(in) :: fname
  real, intent(out) :: A(:,:)
  integer :: i,j

  open(11, file = fname, status='old')  
  do i = 1,size(A,1)
    read (11,*) (A(i,j), j=1,size(A,2))
  end do  
 close(11)
  
end subroutine load_mat


 subroutine save_vec(fname,A)
  implicit none
  character(len=*), intent(in) :: fname
  real, intent(out) :: A(:)
  integer :: i

  open(11, file = fname)  
  do i = 1,size(A,1)
    write(11,*) A(i)
  end do  
 close(11)
  
end subroutine save_vec

 subroutine save_mat(fname,A)
  implicit none
  character(len=*), intent(in) :: fname
  real, intent(out) :: A(:,:)
  integer :: i,j

  open(11, file = fname)  
  do i = 1,size(A,1)
    write(11,*) (A(i,j), j=1,size(A,2))
  end do  
 close(11)
  
end subroutine save_mat


end module sangoma_utils
