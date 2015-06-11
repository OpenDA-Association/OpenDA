% This file is an example how to use
% sangoma_local_ensemble_analysis and sangoma_local_EnKF


disp('Example for sangoma_local_ensemble_analysis')
disp('-------------------------------------------')

% tolerance for checking
tol = 1e-10;

% method to use for the analysis
method = 'ETKF';

disp('Setup grid')

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
  
  Xf(:,i) = tmp(:);
end

% some observations
yo = ones(m,1);

% diagonal of observartion error covariance matrix R
diagR = ones(m,1);
R = diag(diagR);

% observed part of the ensemble
HXf = zeros(m,N);
for i=1:N
    HXf(:,i) = reshape(H*Xf(:,i),[1 m]);
end

% partion vector
part = repmat([1:sz(1)*sz(2)]',1,sz(3));
part = part(:);

disp('Make the analysis with a Gaussian localization functions')

% make local analysis with a length-scale of L
L = 0.5;
selectObs = @(i) exp(- ((x(i) - xobs(:)).^2 + (y(i) - yobs(:)).^2)/L^2 );

[Xa] = sangoma_local_ensemble_analysis(Xf,H,yo,diagR,part,selectObs,method);

disp('sangoma_local_ensemble_analysis done.')

% make local analysis with a compact support correlation function

disp('Make the analysis with a compact-support localization functions')

selectObs = @(i) sangoma_compact_locfun(L,...
    sqrt((x(i) - xobs(:)).^2 + (y(i) - yobs(:)).^2));

[Xa] = sangoma_local_ensemble_analysis(Xf,H,yo,diagR,part,selectObs,method);

disp('sangoma_local_ensemble_analysis done.')


% Test local EnKF

disp('Example for sangoma_local_EnKF')

% set seed
randn('seed',1234)

disp('Setup localization functions')

% localization functions
rho_PH = @(i,j) sangoma_compact_locfun(...
    L,sqrt((x(i) - xobs(j)).^2 + (y(i) - yobs(j)).^2));

rho_HPH = @(i,j) sangoma_compact_locfun(...
    L,sqrt((xobs(i) - xobs(j)).^2 + (yobs(i) - yobs(j)).^2));


Xa = sangoma_local_EnKF(Xf,HXf,yo,R,rho_PH,rho_HPH);

disp('local EnKF done.')
