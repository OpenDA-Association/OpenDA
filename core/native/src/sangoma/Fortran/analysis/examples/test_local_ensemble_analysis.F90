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

! check if global analysis is the same as local analysis if 
! weights are always equal to 1
    
! global analysis
  call ensemble_analysis(Ef,HEf,yo,R,method,Eag)

 ! local analysis
  call local_ensemble_analysis(Ef,HEf,yo,diagR,part,selectAllObs,method,Ea,xa)

  call assert(Eag,Ea,tol,'check if global is the same as local analysis' // &
     ' if all weights are 1')


  ! make local analysis with a length-scale of L 
  ! L is used in selectObs
  L = 0.5
  call local_ensemble_analysis(Ef,HEf,yo,diagR,part,selectObs,method,Ea,xa)

  call save('inputs/Ea-local-exp.txt',Ea)
  call save('inputs/xa-local-exp.txt',xa)


 ! make local analysis with a compact support correlation function

  call local_ensemble_analysis(Ef,HEf,yo,diagR,part,selectObsCompactFunction, &
       method,Ea,xa)

  call save('inputs/Ea-local-cf.txt',Ea)
  call save('inputs/xa-local-cf.txt',xa)

! ! Test local EnKF


  allocate(Hxf(m), Ke(n,m), Re(m,m), S(m,dim_ens), Yp(m,dim_ens), sqrtR(m,m), &
       Ye(m,dim_ens))

! set seed
!randn('seed',1234)

! first "naive" implementation
! do not try this at home with your global ocean model!

! ensemble mean and perturbations
xf = sum(Ef,2)/dim_ens;
Efp = Ef - spread(xf,2,dim_ens);

! observed ensemble mean and perturbations
Hxf = sum(HEf,2)/dim_ens;
S = HEf - spread(Hxf,2,dim_ens)

! square root of observation error covariance matrix
sqrtR = symsqrtm(R);

! perturbation of observations
Yp = sqrtR * randn(m,dim_ens);
Ye = Yp + spread(yo,2,dim_ens)

! ensemble obs. error covariance matrix
Re = matmul(Yp, transpose(Yp)) / (dim_ens-1);

! ensemble error covariance
allocate(PfH(n,m),HPfH(m,m))

HPfH = matmul(S, transpose(S)) / (dim_ens-1);
PfH = matmul(Efp, transpose(S)) / (dim_ens-1);



! localize PfH 
do j = 1,m
  do i = 1,n
    ! unpack index i
    sub = ind2sub(sz,i)

    ! distance
    dist = sqrt((x(sub(1),sub(2),sub(3)) - xobs(j))**2 + &
         (y(sub(1),sub(2),sub(3)) - yobs(j))**2)

    rho = locfun(dist/L)
    PfH(i,j) = PfH(i,j) * rho
  end do
end do

! localize HPfH 

do j = 1,m
  do i = 1,m
    dist = sqrt((xobs(i) - xobs(j))**2 + (yobs(i) - yobs(j))**2);
    rho = locfun(dist/L)
    HPfH(i,j) = HPfH(i,j) * rho
  end do
end do

! Kalman gain with localized covariances
Ke = matmul(PfH, inv(HPfH + R))

! ! analysis ensemble
Ea2 = Ef + matmul(Ke,Ye-HEf);

! ! localization functions
! rho_PH = @(i,j) sangoma_compact_locfun(...
!     L,sqrt((x(i) - xobs(j))**2 + (y(i) - yobs(j))**2));

! rho_HPH = @(i,j) sangoma_compact_locfun(...
!     L,sqrt((xobs(i) - xobs(j))**2 + (yobs(i) - yobs(j))**2));


! ! use the sangoma_local_EnKF function
! randn('seed',1234)
! Ea = sangoma_local_EnKF(Ef,HEf,yo,R,rho_PH,rho_HPH);

! ! check results
! sangoma_check(Ea,Ea2,['compare naive and optimized version of local EnKF'],tol);

deallocate(Hxf, Ke, Re, S, Yp, sqrtR, Ye)

deallocate(PfH,HPfH)

deallocate(xobs,yobs)

contains
 function H(x) result(Hx)
  implicit none
  real, intent(in) :: x(:)
  real :: Hx(m)
  
  Hx = pack(x,reshape(mask,[n]))
 end function H

 subroutine selectAllObs(i,w)
  implicit none
  integer, intent(in) :: i
  real, intent(out) :: w(:)
  w = 1.
 end subroutine selectAllObs

 subroutine selectObs(i,w)
  implicit none
  integer, intent(in) :: i
  real, intent(out) :: w(:)

  integer :: sub(3)

  ! unpack index i
  sub = ind2sub(sz,i)

  ! squared distance
  w = (x(sub(1),sub(2),sub(3)) - xobs)**2 + &
      (y(sub(1),sub(2),sub(3)) - yobs)**2

  w = exp(-w/L**2)

 end subroutine selectObs

 subroutine selectObsCompactFunction(i,w)
  implicit none
  integer, intent(in) :: i
  real, intent(out) :: w(:)

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



 function rho_HPH(i,j) result(w)
  implicit none
  integer, intent(in) :: i,j
  real :: w

  ! squared distance
  w = (xobs(i) - xobs(j))**2 + &
      (yobs(i) - yobs(j))**2

  w = locfun(sqrt(w)/L)
 end function


 function rho_PH(i,j) result(w)
  implicit none
  integer, intent(in) :: i
  real :: w

  integer :: sub(3), j

  ! unpack index i
  sub = ind2sub(sz,i)

  ! squared distance
  w = (x(sub(1),sub(2),sub(3)) - xobs(j))**2 + &
      (y(sub(1),sub(2),sub(3)) - yobs(j))**2

  w = locfun(sqrt(w)/L)
 end function

end program test_slea
 
