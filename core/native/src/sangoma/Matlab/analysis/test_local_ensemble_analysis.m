% tolerance for checking
tol = 1e-10;

% method to use for the analysis
method = 'ETKF2';
%method = 'ETKF';

% size of the domain (horizontal grid is 15x15 with 10 vertical layers)
sz = [15 15 10];
%sz = [15 1 1];

[x,y,z] = ndgrid(linspace(-pi/2,pi/2,sz(1)),linspace(-pi/2,pi/2,sz(2)),...
                 linspace(-pi/2,pi/2,sz(3)));

% mask 1 if point is observed
mask = false(sz);
mask(1:5:end,1:5:end,end) = true;

% local of the observations

xobs = x(mask);
yobs = y(mask);

% number of elements in the state vector
n = numel(x);

% number of ensemble members
N = 10;

% number of observations
m = sum(mask(:));

% H as sparse matrix
Hi = find(mask);
Hj = 1:m;
H = sparse(Hj,Hi,ones(size(Hi)),m,n);

% initialize the ensemble
Xf = zeros(n,N);

for i=1:N
  tmp = 0;
  for k=1:N+1
    tmp = tmp + cos(k*x).*cos(k*y).*cos(k*z) * cos(i*k)/k ;
  end

  tmp = cos(i*x/3).*cos(i*y/3).*cos(i*z/3) /i ;
  
  Xf(:,i) = tmp(:);
end
%Xf = randn(n,N);

% some observations
yo = ones(m,1);

% diagonal of observartion error covariance matrix R
diagR = ones(m,1);
%diagR = [1:m]';
R = diag(diagR);

% observed part of the ensemble
HXf = zeros(m,N);
for i=1:N
    HXf(:,i) = reshape(H*Xf(:,i),[1 m]);
end

% partion vector
part = repmat([1:sz(1)*sz(2)]',1,sz(3));
part = part(:);

% check if global analysis is the same as local analysis if 
% weights are always equal to 1
selectObs = @(i) ones(m,1);
    
% % global analysis
% [Xag,xa] = sangoma_ensemble_analysis(Xf,H,yo,R,method);
% % local analysis
% [Xal,xal] = sangoma_local_ensemble_analysis(Xf,H,yo,diagR,part,selectObs,method);
    
% sangoma_check(Xag,Xal,['check of global is the same as local analysis' ...
%     'if all weights are 1'],tol);

% make local analysis with a length-scale of L

disp('Consistency checks for local ensemble analysis')
disp('Run the Fortran tools first.')

disp('make local analysis with Gaussian correlation function')

L = 0.5;
selectObs = @(i) exp(- ((x(i) - xobs(:)).^2 + (y(i) - yobs(:)).^2)/L^2 );

[Xa,xa] = sangoma_local_ensemble_analysis(Xf,H,yo,diagR,part,selectObs,method);

Xa_fortran = load('../../Fortran/analysis/examples/inputs/Ea-local-exp.txt');
sangoma_check(Xa_fortran,Xa,['compare Fortran and Matlab/Octave (ensemble)'],tol);

xa_fortran = load('../../Fortran/analysis/examples/inputs/xa-local-exp.txt');
sangoma_check(Xa_fortran,Xa,['compare Fortran and Matlab/Octave (mean state)'],tol);


% make local analysis with a compact support correlation function

disp('make local analysis with a compact support correlation function');

selectObs = @(i) sangoma_compact_locfun(L,...
    sqrt((x(i) - xobs(:)).^2 + (y(i) - yobs(:)).^2));

[Xa,xa] = sangoma_local_ensemble_analysis(Xf,H,yo,diagR,part,selectObs,method);

Xa_fortran = load('../../Fortran/analysis/examples/inputs/Ea-local-cf.txt');
sangoma_check(Xa_fortran,Xa,['compare Fortran and Matlab/Octave (ensemble)'],tol);

xa_fortran = load('../../Fortran/analysis/examples/inputs/xa-local-cf.txt');
sangoma_check(Xa_fortran,Xa,['compare Fortran and Matlab/Octave (mean state)'],tol);

% Test local EnKF


% set seed
randn('seed',1234)

% first "naive" implemented
% do not try this at home with your global ocean model

% ensemble mean and perturbations
xf = mean(Xf,2);
Xfp = Xf - repmat(xf,[1 N]);

% observed ensemble mean and perturbations
HXf = H*Xf;
Hxf = mean(HXf,2);
S = HXf - repmat(Hxf,[1 N]);

% square root of observation error covariance matrix
sqrtR = sqrtm(R);

% perturbation of observations
Yp = sqrtR * randn(m,N);
Y = Yp + repmat(yo,[1 N]);

% ensemble obs. error covariance matrix
Re = Yp * Yp'/(N-1);

% ensemble error covariance
HPfH = (S * S') / (N-1);
PfH = (Xfp * S') / (N-1);

% rho_* are the localization functions
rho_PH = zeros(n,m);
rho_HPH = zeros(m,m);

% localize PfH 
for j = 1:m
  for i = 1:n
    dist = sqrt((x(i) - xobs(j)).^2 + (y(i) - yobs(j)).^2);
    rho_PH(i,j) = sangoma_compact_locfun(L,dist);
    PfH(i,j) = PfH(i,j) * rho_PH(i,j);
  end
end

% localize HPfH 

for j = 1:m
  for i = 1:m
    dist = sqrt((xobs(i) - xobs(j)).^2 + (yobs(i) - yobs(j)).^2);
    rho_HPH(i,j) = sangoma_compact_locfun(L,dist);
    HPfH(i,j) = HPfH(i,j) * rho_HPH(i,j);
  end
end

% Kalman gain with localized covariances
Ke = PfH * inv(HPfH + R);

% analysis ensemble
Xa2 = Xf + Ke * (Y-HXf);

% localization functions
rho_PH = @(i,j) sangoma_compact_locfun(...
    L,sqrt((x(i) - xobs(j)).^2 + (y(i) - yobs(j)).^2));

rho_HPH = @(i,j) sangoma_compact_locfun(...
    L,sqrt((xobs(i) - xobs(j)).^2 + (yobs(i) - yobs(j)).^2));


% use the sangoma_local_EnKF function
randn('seed',1234)
Xa = sangoma_local_EnKF(Xf,HXf,yo,R,rho_PH,rho_HPH);

% check results
sangoma_check(Xa,Xa2,['compare naive and optimized version of local EnKF'],tol);
