program test_slea
 use sangoma_utils
 use sangoma_ensemble_analysis
 implicit none

 real :: tol

! size of the domain (horizontal grid is 15x15 with 10 vertical layers)
 integer, parameter :: sz(3) =  [15, 15, 10]
 character(len=125) :: method = 'ETKF2'
 integer :: i,j,k,lind
 real, parameter :: pi = 3.141592654

! number of ensemble members
 integer, parameter :: dim_ens = 10;
! number of elements in the state vector
 integer, parameter :: n = product(sz);
 integer, parameter :: m = 9

 integer :: part(n)
 
 real, dimension(sz(1),sz(2),sz(3)) :: x,y,z,tmp
 logical, dimension(sz(1),sz(2),sz(3)) :: mask
 
 real :: xf(n), xa(n), Ea(n,dim_ens),Ef(n,dim_ens), Eag(n,dim_ens)

 real :: Ea2(n,dim_ens), Efp(n,dim_ens)
 integer :: sub(3)

 real, allocatable :: xobs(:),yobs(:),yo(:),HEf(:,:),diagR(:),R(:,:)
 real, allocatable :: Hxf(:), Ke(:,:), Re(:,:), S(:,:), Yp(:,:), sqrtR(:,:)
 real, allocatable :: Ye(:,:)
 real :: L, rho, dist


 real, allocatable :: PfH(:,:), HPfH(:,:)

 ! tolerance for checking
 tol = 1e-10;

 ! method to use for the analysis
 method = 'ETKF2';

 !sz = [15 1 1];
 
do k=1,sz(3)
  do j=1,sz(2)
    do i=1,sz(1)
      x(i,j,k) = -pi/2 + (i-1.)/(sz(1)-1) * pi
      y(i,j,k) = -pi/2 + (j-1.)/(sz(2)-1) * pi
      z(i,j,k) = -pi/2 + (k-1.)/(sz(3)-1) * pi
    end do
  end do
end do

! mask is .true. if point is observed
mask = .false.;
mask(::5,::5,sz(3)) = .true.;

! local of the observations

! number of observations
!m = sum(mask)
allocate(xobs(m),yobs(m),yo(m),diagR(m),R(m,m),HEf(m,dim_ens))

xobs = pack(x,mask);
yobs = pack(y,mask);


! ! H as sparse matrix
! Hi = find(mask);
! Hj = 1:m;
! H = sparse(Hj,Hi,ones(size(Hi)),m,n);

! initialize the ensemble

do i=1,dim_ens
  tmp = 0

  do k=1,dim_ens+1
    tmp = tmp + cos(1.*k*x)*cos(1.*k*y)*cos(1.*k*z) * cos(1.*i*k*x)
  end do
  tmp = cos(1.*i*x/3)*cos(1.*i*y/3)*cos(1.*i*z/3) / i
  
  Ef(:,i) = reshape(tmp,[n]);
end do

! some observations
yo = 1

! diagonal of observartion error covariance matrix R
diagR = 1
R = diag(diagR);

! observed part of the ensemble
do i=1,dim_ens
  HEf(:,i) = H(Ef(:,i))
end do

! partion vector
part = reshape(spread([(i,i=1,sz(1)*sz(2))],1,sz(3)),[product(sz)])

lind = 1
do k=1,sz(3)
  do j=1,sz(2)
    do i=1,sz(1)
      part(lind) = i + (j-1) * sz(2)
      lind = lind+1
    end do
  end do
end do


  ! make local analysis with a length-scale of L 
  ! L is used in selectObs
  L = 0.5


 ! make local analysis with a compact support correlation function

  call sangoma_local_ensemble_analysis(n,m,dim_ens,Ef,HEf,yo,diagR,part,selectObsCompactFunction, &
       method,Ea)

  call save('inputs/Ea-local-cf.txt2',Ea)


deallocate(xobs,yobs)

contains
 function H(x) result(Hx)
  implicit none
  real, intent(in) :: x(:)
  real :: Hx(m)
  
  Hx = pack(x,reshape(mask,[n]))
 end function H

 subroutine selectObsCompactFunction(m,i,w)
  implicit none
  integer, intent(in) :: m, i
  real, intent(out) :: w(m)

  integer :: sub(3), j

  ! unpack index i
  sub = ind2sub(sz,i)

  ! squared distance
  w = (x(sub(1),sub(2),sub(3)) - xobs)**2 + &
      (y(sub(1),sub(2),sub(3)) - yobs)**2

  do j = 1,size(w)
    w(j) = locfun(sqrt(w(j))/L)
  end do

 end subroutine selectObsCompactFunction




end program test_slea
 
