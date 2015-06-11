program test_sangoma_ensemble
 use sangoma_ensemble_analysis
 use sangoma_utils
 implicit none

! test script for sangoma_ensemble_analysis
! Notations follows:
! Sangoma D3.1

! issues
! EAKF does not work in m < N-1

! number of elements in the state vector
 integer, parameter :: n = 10;
! ensemble size
 integer, parameter :: dim_ens = 3;
! number of observations
 integer, parameter :: m = 5;

! if debug is one, then internal checks are activated
 logical :: debug = .true. 
 real :: tol = 1e-10;

 real :: y(m), Ef(n,dim_ens), Ea(n,dim_ens), Eap(n,dim_ens), Efp(n,dim_ens)
 real :: H(m,n), xf(n), xa(n), HEf(m,dim_ens)
 real :: Pf(n,n), K(n,m), Pa_check(n,n), xa_check(n), R(m,m)

 real :: tmp(n,n), HPf(m,n), tmpm(m,m)
 character(len=125) :: method = 'ETKF2'
 integer :: i

 y = randn(m);
 Ef = randn(n,dim_ens);
 
 H = randn(m,n);

 y = [(i,i=1,m)]
 Ef = reshape([(sin(3.*i),i=1,n*dim_ens)],[n,dim_ens])
 H = reshape([(i,i=1,m*n)],[m,n])

 R = 2*eye(m);

 xf = sum(Ef,2)/dim_ens;
 Efp = Ef - spread(xf,2,dim_ens);

 HEf = matmul(H,Ef)

 Pf = matmul(Efp,transpose(Efp)) / (dim_ens-1);
! K = matmul(Pf, matmul(transpose(H)) * inv(matmul(H,matmul(Pf,transpose(H))) + R);

 K = matmul( &
      matmul(Pf, transpose(H)), &
      inv(matmul(H,matmul(Pf,transpose(H))) + R));


 Pa_check = Pf - matmul(K,matmul(H,Pf));
 xa_check = xf + matmul(K,(y - matmul(H,xf)));

! method = {'EnSRF','EAKF','ETKF','ETKF2','SEIK','ESTKF','serialEnSRF'};
!method = {'EAKF'};

 !disp('dim_enson-serial algorithms (all observations at once) with H')

 !for i = 1:length(method)

  call ensemble_analysis(Ef,HEf,y,R,method, &
      Ea,xa,debug,tol,cbR=cbR);
  Eap = Ea - spread(xa,2,dim_ens)
  
  ! check results
  call assert_vec(xa,xa_check,tol,trim(method) // '-analysis')
  
  call assert_vec(sum(Ea,2)/dim_ens,xa_check,tol, &
                 trim(method) // '-analysis ensemble mean');

  call assert_mat(matmul(Eap,transpose(Eap)) / (dim_ens-1.),Pa_check, tol, &
                 trim(method)//'-analysis ensemble variance')

!  write(*,*) 'xa ',xa

! method = {'EnSRF','EAKF','ETKF','ETKF2','SEIK','ESTKF'};
! !method = {'EAKF'};

! disp('Non-serial algorithms (all observations at once) with HEf')

! for i = 1:length(method)
!   [Ea,xa] = sangoma_ensemble_analysis(Ef,[],y,R,method{i},...
!       'debug',debug,'tolerance',tol,'HEf',H*Ef);
!   Eap = Ea - repmat(xa,[1 dim_ens]);
  
!   ! check results
!   sangoma_check(xa,xa_check,[method{i} '-analysis'],tol)
  
!   sangoma_check(mean(Ea,2),xa_check,...
!                 [method{i} '-analysis ensemble mean'],tol);

!   sangoma_check((Eap * Eap') / (dim_ens-1),Pa_check,...
!                 [method{i} '-analysis ensemble variance'],tol)
! end


! !{
!   method = {'EnSRF','ETKF','SEIK','ESTKF','serialEnSRF'};
!   disp('Serial algorithms (one observation after the other)')


! for i = 1:length(method)
!   [Ea,xa] = sangoma_ensemble_analysis(Ef,H,y,R,method{i},...
!       'debug',0,'tolerance',tol,'serial',1);
!   Eap = Ea - repmat(xa,[1 dim_ens]);
  
!   ! check results
!   sangoma_check(xa,xa_check,[method{i} '-analysis'],tol)
  
!   sangoma_check(mean(Ea,2),xa_check,...
!                 [method{i} '-analysis ensemble mean'],tol);

!   sangoma_check((Eap * Eap') / (dim_ens-1),Pa_check,...
!                 [method{i} '-analysis ensemble variance'],tol)
! end

! !}

contains
  subroutine cbR(x,mode,Rx)
   implicit none
   real, intent(in) :: x(:)
   integer, intent(in) :: mode
   real, intent(out) :: Rx(:)


   if (mode == 1) then
     Rx = matmul(R,x)
   else
     Rx = matmul(inv(R),x)
   end if
  end subroutine cbR

end program test_sangoma_ensemble
