program example_sangoma_ensemble_analysis
 use iso_c_binding, only: C_CHAR, C_NULL_CHAR
 use sangoma_ensemble_analysis
 use sangoma_utils
 implicit none

 ! tolerence for checking
 real, parameter :: tol = 1e-8

 ! number of elements in the state vector
 integer, parameter :: n = 10;
 ! ensemble size
 integer, parameter :: dim_ens = 3;
 ! number of observations
 integer, parameter :: m = 5;


 real :: y(m), Ef(n,dim_ens), Ea(n,dim_ens), Eap(n,dim_ens), Efp(n,dim_ens)
 real :: H(m,n), xf(n), xa(n), HEf(m,dim_ens)
 real :: Pf(n,n), K(n,m), Pa_check(n,n), xa_check(n), R(m,m)

 real :: tmp(n,n), HPf(m,n), tmpm(m,m)
 character(len=125) :: method = 'ETKF2'
 integer :: i

 write (*,'(10x,a)') '********************************************'
 write (*,'(10x,a)') '*         example_ensemble_analysis        *'
 write (*,'(10x,a)') '*                                          *'
 write (*,'(10x,a)') '*              ETKF Analysis               *'
 write (*,'(10x,a)') '********************************************'


 write (*,'(10x,a)') 'Generate random data'
 y = randn(m)
 Ef = randn(n,dim_ens)

 H = randn(m,n);

 y = [(i,i=1,m)]
 Ef = reshape([(sin(3.*i),i=1,n*dim_ens)],[n,dim_ens])
 H = reshape([(i,i=1,m*n)],[m,n])

 R = 2*eye(m);

 xf = sum(Ef,2)/dim_ens;
 Efp = Ef - spread(xf,2,dim_ens);

 HEf = matmul(H,Ef)

 Pf = matmul(Efp,transpose(Efp)) / (dim_ens-1);

 K = matmul( &
      matmul(Pf, transpose(H)), &
      inv(matmul(H,matmul(Pf,transpose(H))) + R));


 Pa_check = Pf - matmul(K,matmul(H,Pf));
 xa_check = xf + matmul(K,(y - matmul(H,xf)));

 write (*,'(10x,a)') 'Perform the analysis'

 call sangoma_ens_analysis(n,m,dim_ens,Ef,HEf,y,method // C_NULL_CHAR, &
      Ea,cbR);

 xa = sum(Ea,2)/dim_ens
 Eap = Ea - spread(xa,2,dim_ens)

 write (*,'(10x,a)') 'Check results'

 ! check results
 call assert_vec(xa,xa_check,tol,trim(method) // '-analysis')

 call assert_vec(xa,xa_check,tol, &
      trim(method) // '-analysis ensemble mean');

 call assert_mat(matmul(Eap,transpose(Eap)) / (dim_ens-1.),Pa_check, tol, &
      trim(method)//'-analysis ensemble variance')


contains
 subroutine cbR(x,mode,Rx) bind(c)
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

end program example_sangoma_ensemble_analysis
