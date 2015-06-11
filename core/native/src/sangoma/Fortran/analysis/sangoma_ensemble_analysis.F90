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

module sangoma_ensemble_analysis

contains


! sangoma interfaces


subroutine sangoma_ens_analysis(dim,dim_obs,dim_ens,Ef,HEf,y,method,Ea,cbR) &
   bind(C, name="sangoma_ensemble_analysis_")

! Computes the analysis ensemble Ea based on forecast ensemble Ef using the ETKF
! scheme. Currently, the method has to be ETKF2 (see Hunt et al., 2007)
! cbR is a callback-routine to compute the product of observation error covariance 
! matrix R (if mode is 1) with a given vector or theinverse of R time a vector
! if mode is -1.
! 
 use, intrinsic :: iso_c_binding
 use sangoma_base, only: realprec, intprec
 implicit none

! Inputs 
 integer(intprec), intent(in) :: dim                ! state dimension
 integer(intprec), intent(in) :: dim_obs            ! number of observations
 integer(intprec), intent(in) :: dim_ens            ! ensemble size
 
 real(realprec), intent(in) :: Ef(dim,dim_ens)      ! forecast ensemble
 real(realprec), intent(in) :: HEf(dim_obs,dim_ens) ! obs. forecast ensemble
 real(realprec), intent(in) :: y(dim_obs)           ! observation
 character(kind=c_char, len=1), intent(in) :: method(*) 
                                                    ! name of the method 
                                                    ! (null-terminated c-string)
! Output 
 real(realprec), intent(out) :: Ea(dim,dim_ens)     ! analysed ensemble

  interface
    subroutine cbR(x,mode,Rx) bind(C)
     use, intrinsic :: iso_c_binding
     use sangoma_base, only: realprec, intprec
     implicit none
     real(realprec), intent(in) :: x(:)
     integer(intprec), intent(in) :: mode
     real(realprec), intent(out) :: Rx(:)
    end subroutine cbR
  end interface

  real :: unusedParam(1,1)
  character(:), allocatable :: str


  str = c_to_f_string(method)
  call ensemble_analysis(Ef,HEf,y,unusedParam,str,Ea,cbR=cbR)
  deallocate(str)      
 end subroutine sangoma_ens_analysis


subroutine sangoma_local_ensemble_analysis(dim,dim_obs,dim_ens,Ef,HEf,y,diagR,part,selectObs,method,Ea) &
   bind(C, name="sangoma_local_ensemble_analysis_")

! Computes local ensemble analysis Ea based on forecast ensemble Ef using the ETKF
! scheme. Currently, method has to be ETKF2 (see Hunt et al., 2007)
! selectObs is a callback-routine which retuns the weight for each observation 
! relative to a state vector index i.
! 

 use, intrinsic :: iso_c_binding
 use sangoma_base, only: realprec, intprec
 implicit none

 integer(intprec), intent(in) :: dim                ! state dimension
 integer(intprec), intent(in) :: dim_obs            ! number of observations
 integer(intprec), intent(in) :: dim_ens            ! ensemble size

 real(realprec),   intent(in) :: Ef(dim,dim_ens)      ! forecast ensemble
 real(realprec),   intent(in) :: HEf(dim_obs,dim_ens) ! obs. forecast ensemble
 real(realprec),   intent(in) :: y(dim_obs)           ! observation

 real(realprec),   intent(in) :: diagR(dim_obs)       ! diagonal elements of R
 integer(intprec), intent(in) :: part(dim)            ! partition vector
 character(kind=c_char, len=1), intent(in) :: method(*) 
                                                    ! name of the method 
                                                    ! (null-terminated c-string)
! Output 
 real(realprec), intent(out) :: Ea(dim,dim_ens)     ! analysed ensemble


 interface
   subroutine selectObs(dim_obs,i,w) bind(C)
    use, intrinsic :: iso_c_binding
    use sangoma_base, only: realprec, intprec
    implicit none
    integer(intprec), intent(in) :: dim_obs, i
    real(realprec), intent(out) :: w(dim_obs)
   end subroutine selectObs
 end interface
 
 character(:), allocatable :: str

 str = c_to_f_string(method)
 call local_ensemble_analysis(Ef,HEf,y,diagR,part,selObs,str,Ea)
 deallocate(str)
 
 contains

   subroutine selObs(i,w) 
    implicit none
    integer, intent(in) :: i
    real, intent(out) :: w(:)

    call selectObs(size(w),i,w)
   end subroutine selObs
  
end subroutine sangoma_local_ensemble_analysis

! end of sangoma interfaces


  ! from http://stackoverflow.com/questions/20365293/converting-a-variable-length-c-string-to-fortran-string-in-visual-studio
  function c_to_f_string(s) result(str)
   use, intrinsic :: iso_c_binding
   character(kind=C_CHAR, len=1), intent(IN) :: s(*)
   character(:), allocatable  :: str
   integer i, nchars
   i = 1
   do
     if (s(i) == c_null_char) then
       exit
     end if
     i = i + 1
   end do
   nchars = i - 1  ! Exclude null character from Fortran string
   allocate(character(len=nchars) :: str)
   str = transfer(s(1:nchars), str)
  end function c_to_f_string


subroutine ensemble_analysis(Ef,HEf,y,R,method,Ea,xa_,debug_,tolerance_,cbR)
! [Xa] = sangoma_ensemble_analysis(Xf,H,y,R,method,...)
! Computes analysis ensemble Xa (n x N) based on forecast ensemble Xf 
! (n x N), observation y (m x 1), operator H (m x n) (see also below), and 
! observation error covariance R (m x m).
! Method can be 'EnSRF','EAKF','ETKF','ETKF2','SEIK', 'ESTKF' or 'EnKF'.
! 'ETKF': use SVD decomposition of invTTt (see Sangoma D3.1)
! 'ETKF2': use eigendecomposition decomposition of invTTt (see Hunt et al., 2007)
!
!
! Optional parameters:
! 'debug',debug: set to 1 to enable debugging. Default (0) is no debugging.
! 'tolerance', tolerance: expected rounding error (default 1e-10) for debugging
!    checks. This is not used if debug is 0.
! 'HXf': if non empty, then it is the product H Xf. In this case, H is not
! used (except for EnSRF).

! Notations follows:
! Sangoma D3.1
use sangoma_utils
implicit none
real, intent(in) :: Ef(:,:)
real, intent(in) :: HEf(:,:)
real, intent(in) :: y(:)
real, intent(in) :: R(:,:)
character(len=*), intent(in) :: method
real, intent(out) :: Ea(:,:)
real, intent(out), optional :: xa_(:)
logical, intent(in), optional :: debug_
real, intent(in), optional :: tolerance_
optional  :: cbR

interface
  subroutine cbR(x,mode,Rx)
   implicit none
   real, intent(in) :: x(:)
   integer, intent(in) :: mode
   real, intent(out) :: Rx(:)
  end subroutine cbR
end interface

! local variables


real :: xf(size(Ef,1))
real :: xa(size(Ef,1))
real :: Eap(size(Ef,1),size(Ef,2))
real :: Efp(size(Ef,1),size(Ef,2))
real :: Hxf(size(y))
real :: S(size(y),size(Ef,2))
real :: F(size(y),size(y))

! the product inv(R) * (y - Hxf)
real :: invR_innov(size(y))

real, dimension(size(Ef,2),size(Ef,2)) :: invTTt, T, U_T
real, dimension(size(Ef,2)) :: Sigma_T, invSigma_T
real :: y_tmp(size(y))

integer :: m,N, k
real :: tolerance
logical :: debug

tolerance = 1e-10;
debug = .false.


! ensemble size
N = size(Ef,2);

! number of observations
m = size(y,1);

xf = sum(Ef,2)/N;
Efp = Ef - spread(xf,2,N);


Hxf = sum(HEf,2)/N
S = HEf - spread(Hxf,2,N)

F = matmul(S,transpose(S)) + (N-1) * R;

if (debug) then
!  Pf = (Efp * Efp') / (N-1);  
!  HPfH = (S * S') / (N-1);
!  PfH = (Efp * S') / (N-1);
  
!  K = PfH * inv(HPfH + R);
end if

if (size(y) == 0) then
  ! nothing to do
  Ea = Ef
  if (present(xa_)) xa_ = xf
  return
end if

if (method == 'EnSRF') then
  ! ! EnSRF
  ! [Gamma_S,Lambda_S] = eig(F);
  ! E_S = S'*Gamma_S * sqrt(inv(Lambda_S));
  ! [U_S,Sigma_S,Z_S] = svd(E_S);
 
  ! Eap = Efp * (U_S * (sqrt(eye(N)-Sigma_S*Sigma_S') * U_S'));
  ! xa = xf + Efp * (S' * (Gamma_S * (Lambda_S \ (Gamma_S' * (y - Hxf)))));

! elseif (method == 'serialEnSRF') then
!   ! EnSRF with serial observation processing  
!   for iobs = 1:m           
!      Hloc = H(iobs,:);
!      yloc = y(iobs);
!      Sloc = Hloc*Efp;
!      Hxfloc = Hloc*xf;
     
!      Floc = Sloc*Sloc' + (N-1)*R(iobs, iobs);
!      R(iobs,iobs);
!      Kloc = Efp*Sloc' / Floc;
!      xa = xf + Kloc * (yloc - Hxfloc);
!      alpha = 1.0 / (1.0 + sqrt( (N-1)*R(iobs,iobs)/Floc) );
!      Eap = Efp - alpha * Kloc * Hloc * Efp;
     
!      Efp = Eap;
!      xf = xa;
!   end
  
! elseif strcmp(method,'ETKF')
!   ! ETKF with decomposition of Stilde
!   sqrtR = sqrtm(R);
!   Stilde = sqrt(1/(N-1)) * (sqrtR \ S);

!   ! "economy size" SVD decomposition
!   [U_T,Sigma_T,V_T] = svd(Stilde',0);
  
!   if size(Sigma_T,2) > N
!     Sigma_T = Sigma_T(:,1:N);
!     V_T = V_T(:,1:N);
!   end
!   Ndim = size(Sigma_T,1);
  
!   TTt = eye(N) - S'*(F\S);

!   if debug
!     sangoma_check(TTt,U_T * ((eye(Ndim)+Sigma_T*Sigma_T') \ U_T'),...
!         'ETKF-TTt',tolerance)

!     K2 = 1/sqrt(N-1.) * Efp * ...
!         (Stilde' * ((Stilde*Stilde'+ eye(m)) \ inv(sqrtR))) ;
!     sangoma_check(K,K2,'ETKF-Kalman gain',tolerance)
!   end

!   Eap = Efp * (U_T * (sqrt(eye(Ndim)+Sigma_T*Sigma_T') \ U_T'));
!   xa = xf + 1/sqrt(N-1.) *  Efp * (U_T * ((eye(Ndim)+Sigma_T'*Sigma_T) \ ...
!       (Sigma_T * V_T' * (sqrtR \ (y - Hxf)))));

elseif (method == 'ETKF2') then
  ! ETKF with square-root of invTTt (e.g. Hunt et al., 2007)

  if (present(cbR)) then
    call cbR(y - Hxf,-1,invR_innov)

    do k=1,size(Ef,2)
      ! y_tmp = inv(R) * S(:,k)
      call cbR(S(:,k),-1,y_tmp)
      invTTt(:,k) = matmul(transpose(S), y_tmp)
      invTTt(k,k) = invTTt(k,k) + N-1
    end do
  else
    invR_innov = matmul(inv(R) ,y - Hxf)
    invTTt = (N-1) * eye(N) + matmul(transpose(S), matmul(inv(R),S))
  end if

  call symeig(invTTt,Sigma_T,U_T)

  if (debug) then
    call assert(matmul(U_T,matmul(diag(Sigma_T),transpose(U_T))), &
         invTTt,tolerance,'ETKF2-eig')
  end if
    
  invSigma_T = 1/Sigma_T

  T = matmul(U_T, matmul(diag(sqrt(invSigma_T)) , transpose(U_T)))
  !T = matmul(U_T, (sqrt(invSigma_T) .dx. transpose(U_T)))

  if (debug) then
    call assert(matmul(T,T),inv(invTTt),tolerance,'ETKF2-sym. square root')
  end if

  Eap = sqrt(N-1.) * matmul(Efp, T);

  xa = xf + matmul(                       &
     Efp,                                 &
     matmul(                              &
       U_T ,                              &
       matmul(                            &
          diag(invSigma_T),                     &
          matmul(                        &
             transpose(U_T),                 &
             matmul(                       &
               transpose(S) ,             & 
               invR_innov)))));


! elseif strcmp(method,'ETKF3')
!   ! ETKF in style of ESTKF
!   A_T = zeros(N,N);
  
!   for j = 1:N
!     for i = 1:N
!       if i == j
!         A_T(i,j) = 1 - 1/N;
!       else
!         A_T(i,j) = - 1/N;
!       end         
!     end
!   end

!   L = Ef*A_T;
!   HL = HEf*(A_T*L);

!   if debug
!     Pf2 = 1/(N-1) * L * ((A_T'*A_T) \ L');
!     sangoma_check(Pf,Pf2','ETKF3-Pf',tolerance)
!     Pf2 = 1/(N-1) * L * L';
!     sangoma_check(Pf,Pf2','ETKF3-Pf2',tolerance)
!   end

!   invTTt = (N-1)*eye(N) + HL' * (R \ HL);

!   [U_T,Sigma_T] = eig(invTTt);

!   T = U_T * (sqrt(Sigma_T) \ U_T');
!   !T = sqrtm(inv(invTTt));

!   if debug
!     sangoma_check(T*T,inv(invTTt),'ETKF3-sym. square root',tolerance)
!   end
  
!   Eap = sqrt(N-1.) * L*T * A_T';
!   xa = xf + L * (U_T * (inv(Sigma_T) * U_T' * (HL' * (R \ (y - Hxf)))));

! elseif strcmp(method,'EAKF')
!   ! EAKF
!   sqrtR = sqrtm(R);

!   Stilde = sqrt(1/(N-1)) * (sqrtR \ S);
!   [U_A,Sigma_A,V_A] = svd(Stilde',0);
!   Sigma_A = Sigma_A(:,1:N);
!   V_A = V_A(:,1:N);

!   ! eigenvalue decomposition of Pf
!   ![Z_A,Gamma_A] = eig(Pf);
!   [Z_A,sqrtGamma_A,~] = svd(Efp,0);

!   if debug
!     ! last eigenvalue should be zero
!     sangoma_check(abs(sqrtGamma_A(end,end)),0,'EAKF-gamma',tolerance)
!   end

!   Gamma_A = sqrtGamma_A.^2 / (N-1);

!   if debug
!     sangoma_check(Z_A * Gamma_A * Z_A',Pf,'EAKF-decomposition',tolerance)
!   end

!   Eap = 1/sqrt(N-1.) * Efp * (U_A * ((sqrt(eye(N) + Sigma_A^2)) \ ...
!       (sqrt(pinv(Gamma_A)) * (Z_A' * Efp))));

!   xa = xf + 1/sqrt(N-1.) *  Efp * (U_A * ((eye(N)+Sigma_A'*Sigma_A) \ ...
!       (Sigma_A * V_A' * (sqrtR \ (y - Hxf)))));

! elseif strcmp(method,'SEIK')
!   ! SEIK

!   A = zeros(N,N-1);
!   A(1:N-1,1:N-1) = eye(N-1);
!   A = A - ones(N,N-1)/N;

!   L = Ef*A;
!   HL = HEf*A;
  
!   if debug
!     sangoma_check(L,Efp(:,1:N-1),'SEIK-L matrix',tolerance)
!     Pf2 = 1/(N-1) * L * ((A'*A) \ L');
!     sangoma_check(Pf,Pf2','SEIK-Pf',tolerance)
!   end

!   invTTt = (N-1)*(A'*A) + HL' * (R \ HL);
!   TTt = inv(invTTt);

!   T = chol(inv(invTTt))';

!   if debug
!     sangoma_check(T*T',inv(invTTt),'SEIK-Cholesky decomposition',tolerance)
!   end

!   ! add omega
!   w = ones(N,1)/sqrt(N);
!   Omega = householder(w);
!   Eap = sqrt(N-1.) * L*T*Omega';

!   xa = xf + L * (TTt * (HL' * (R \ (y - Hxf))));
! elseif strcmp(method,'ESTKF')
!   ! ESTKF
!   A = zeros(N,N-1);
  
!   for j = 1:N-1
!     for i = 1:N
!       if i == j && i < N
!         A(i,j) = 1 - 1/N * 1/(1/sqrt(N)+1);
!       elseif i < N
!         A(i,j) = - 1/N * 1/(1/sqrt(N)+1);
!       else
!         A(i,j) = - 1/sqrt(N);
!       end         
!     end
!   end

!   L = Ef*A;
!   HL = HEf*A;

!   if debug
!     Pf2 = 1/(N-1) * L * ((A'*A) \ L');
!     sangoma_check(Pf,Pf2','ESTKF-Pf',tolerance)
!     Pf2 = 1/(N-1) * L * L';
!     sangoma_check(Pf,Pf2','ESTKF-Pf2',tolerance)
!   end

!   invTTt = (N-1)*eye(N-1) + HL' * (R \ HL);

!   [U_E,Sigma_E] = eig(invTTt);

!   T = U_E * (sqrt(Sigma_E) \ U_E');
!   !T = sqrtm(inv(invTTt));

!   if debug
!     sangoma_check(T*T,inv(invTTt),'ESTKF-sym. square root',tolerance)
!   end
  
!   Eap = sqrt(N-1.) * L*T * A';
!   xa = xf + L * (U_E * (inv(Sigma_E) * U_E' * (HL' * (R \ (y - Hxf)))));
! elseif strcmp(method,'EnKF')
!   ! EnKF
!   sqrtR = sqrtm(R);

!   ! perturbation of observations
!   Yp = sqrtR * randn(m,N);
!   Y = Yp + repmat(y,[1 N]);

!   [U_F,Sigma_F,V_F] = svd(S + Yp,0);
!   [G_F,Gamma_F,Z_F] = svd(S'*(U_F*inv(Sigma_F)));

!   ! unclear in manuscript
!   !Eap = Efp * G_F * sqrt(eye(N)-Gamma_F*Gamma_F') * G_F';

!   Ea = Ef + Efp * (S' * (U_F * (inv(Sigma_F)^2 * (U_F' * (Y-HEf)))));

!   xa = mean(Ea,2);
!   Eap = Ea - repmat(xa,[1 N]);
else
  write(6,*) 'unknown method ',method
  stop 'sangoma:unknown_method'
end if

Ea = Eap + spread(xa,2,N)


if (present(xa_)) xa_ = xa

end subroutine ensemble_analysis


subroutine local_ensemble_analysis(Ef,HEf,y,diagR,part,selectObs,method,Ea,xa)

 use sangoma_utils

! [Ea,xa] = sangoma_local_ensemble_analysis(...
!    Ef,H,y,diagR,part,selectObs,method,...)
!
! Computes analysis ensemble Ea based on forecast ensemble Ef using the 
! observation y.
!
! Inputs:
! Ef: forecast ensemble (n x N)
! H: observation operator (m x n)
! y: observation (m x 1)
! diagR: diagonal of the observation error covariance R (m x 1)
! part: vector of integer "labels". Every element of the state vector with the
!   same number belong to the same subdomain
! selectObs: callback routine to select observations with a within a subdomain. 
!   As input is takes an integer representing the index of the state vector and
!   returns a vector of weights (m x 1). 
!   For example:
!      selectObs = @(i) exp(- ((x(i) - xobs(:)).^2 + (y(i) - yobs(:)).^2)/L^2 );
!   or
!      selectObs = @(i) sangoma_compact_locfun(L,...
!          sqrt((x(i) - xobs(:)).^2 + (y(i) - yobs(:)).^2));
!
!   where: 
!      x and y is the horizontal model grid
!      xobs and yobs are the localtion of the observations
!      L is a correlation length-scale
!
! method: method is one analysis schemes implemented sangoma_ensemble_analysis 
!   (except for EnSRF)
!
! Optional inputs:
! 'display', display: if true, then display progress (false is the default)
! 'minweight', minweight: analysis is performed using observations for which 
!    weights is larger than minweight. (default 1e-8)
! 'HEf', HEf: if non empty, then it is the product H Ef. In this case, H is not
!    used
!
! Output:
! Ea: analysis ensemble (n x N) 
! xa: analysis ensemble mean (n x 1) 

! See also:
! sangoma_ensemble_analysis, sangoma_compact_locfun
 implicit none
 real,    intent(in) :: Ef(:,:)
 real,    intent(in) :: HEf(:,:)
 real,    intent(in) :: y(:)
 real,    intent(in) :: diagR(:)
 integer, intent(in) :: part(:)
 character(len=*), intent(in) :: method
 real,    intent(out) :: Ea(:,:)
 real, optional, intent(out) :: xa(:)

 interface
   subroutine selectObs(i,w)
    implicit none
    integer, intent(in) :: i
    real, intent(out) :: w(:)
   end subroutine selectObs
 end interface

 integer :: unique_part(size(part))
 logical :: display
 real :: minweight = 1e-8
 logical :: mask(size(part))
 integer, allocatable :: sel(:)

 real :: weight(size(y))
 logical :: loc(size(y))
 integer, allocatable :: locindex(:)

 integer :: nzones, i, n, nloc, nz, k
 integer :: dim_ens
 real, allocatable :: HEfloc(:,:),Rloc(:,:),yloc(:), Ealoc(:,:), xaloc(:)

 display = .false.
! display = .true.

 ! unique element of partition vector

 unique_part = part
 call unique(unique_part,nzones);

 n = size(Ef,1)
 Ea = 0
 if (present(xa)) xa = 0

 dim_ens = size(Ef,2)

 ! loop over all zones
 do i=1,nzones
   if (display) then
     write(6,*) 'zone ',i,' out of ',nzones
   end if

    mask = part == unique_part(i)
    nz = count(mask)
    allocate(sel(nz))

    sel = pack([(i,i=1,n)],mask);
    call selectObs(sel(1),weight);

    ! restrict to local observations where weight exceeds minweight
    loc = weight > minweight;
    nloc = count(loc)
    allocate(locindex(nloc))

    allocate(HEfloc(nloc,dim_ens),Rloc(nloc,nloc),yloc(nloc),Ealoc(nz,dim_ens),xaloc(nz))

    do k=1,dim_ens
      HEfloc(:,k) = pack(HEf(:,k),loc)
    end do

    Rloc = diag(pack(diagR,loc) / pack(weight,loc));
    yloc = pack(y,loc);

    call ensemble_analysis(Ef(sel,:),HEfloc, &
        yloc,Rloc,method,Ealoc,xaloc,.false.,1e-8)


    Ea(sel,:) = Ealoc

    if (present(xa)) xa(sel) = xaloc
 
    deallocate(sel,locindex)
    deallocate(HEfloc,Rloc,yloc,Ealoc,xaloc)
  end do
 end subroutine local_ensemble_analysis

! Smooth compact localization function described in Gaspari et al. (1999), 
! equation 4.10.
!
! Input:
!   len: correlation length-scale (scalar)
!   r: matrix of distance
! Output:
!   fun: array of weights (same size as r). fun is zero if r > 2 len
!   
! Reference:
! @article {QJ:QJ49712555417,
! author = {Gaspari, Gregory and Cohn, Stephen E.},
! title = {Construction of correlation functions in two and three dimensions},
! journal = {Quarterly Journal of the Royal Meteorological Society},
! volume = {125},
! number = {554},
! publisher = {John Wiley & Sons, Ltd},
! issn = {1477-870X},
! url = {http://dx.doi.org/10.1002/qj.49712555417},
! doi = {10.1002/qj.49712555417},
! pages = {723--757},
! keywords = {Compactly supported, Convolution, Correlation functions, Data assimilation, Space-limited},
! year = {1999},
! }

 function locfun(r) result(fun)
  implicit none
  real :: r,fun

  ! optim
   if (r <= 1.) then
     fun = (((-r/4. + 1./2.) * r + 5./8.) * r - 5./3.) * r**2 + 1.
     
   elseif (r <= 2.) then
     fun = ((((r/12. - 1./2.) * r + 5./8.) * r + 5./3.) * r - 5.) * r + 4 - &
          2./(3*r)
   else
     fun = 0
   end if
  end function locfun

end module sangoma_ensemble_analysis
