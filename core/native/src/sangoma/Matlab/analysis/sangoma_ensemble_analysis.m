% [Xa] = sangoma_ensemble_analysis(Xf,H,y,R,method,...)
% Computes analysis ensemble Xa (n x N) based on forecast ensemble Xf 
% (n x N), observation y (m x 1), operator H (m x n) (see also below), and 
% observation error covariance R (m x m).
% Method can be 'EnSRF','EAKF','ETKF','ETKF2','SEIK', 'ESTKF' or 'EnKF'.
% 'ETKF': use SVD decomposition of invTTt (see Sangoma D3.1)
% 'ETKF2': use eigendecomposition decomposition of invTTt (see Hunt et al., 2007)
%
%
% Optional parameters:
% 'debug',debug: set to 1 to enable debugging. Default (0) is no debugging.
% 'tolerance', tolerance: expected rounding error (default 1e-10) for debugging
%    checks. This is not used if debug is 0.
% 'HXf': if non empty, then it is the product H Xf. In this case, H is not
% used (except for EnSRF).

% Notations follows:
% Sangoma D3.1

function [Xa,xa] = sangoma_ensemble_analysis(Xf,H,y,R,method,varargin)

tol = 1e-10;
debug = 0;
HXf = [];

for i=1:2:length(varargin)
  if strcmp(varargin{i},'debug')
    debug = varargin{i+1};
  elseif strcmp(varargin{i},'tolerance')
    tol = varargin{i+1};
  elseif strcmp(varargin{i},'HXf')
    HXf = varargin{i+1};
  else
    error(['unkown argument ' varargin{i}]);
  end 
end


% ensemble size
N = size(Xf,2);

% number of observations
m = size(y,1);

xf = mean(Xf,2);
Xfp = Xf - repmat(xf,[1 N]);

% do not use isempty here because m might be zero
if isequal(HXf,[])
    HXf = H*Xf;
end

Hxf = mean(HXf,2);
S = HXf - repmat(Hxf,[1 N]);

F = S*S' + (N-1) * R;

if debug
  Pf = (Xfp * Xfp') / (N-1);  
  HPfH = (S * S') / (N-1);
  PfH = (Xfp * S') / (N-1);
  
  K = PfH * inv(HPfH + R);
end

if strcmp(method,'EnSRF')
  % EnSRF
  [Gamma_S,Lambda_S] = eig(F);
  X_S = S'*Gamma_S * sqrt(inv(Lambda_S));
  [U_S,Sigma_S,Z_S] = svd(X_S);
 
  Xap = Xfp * (U_S * (sqrt(eye(N)-Sigma_S*Sigma_S') * U_S'));
  xa = xf + Xfp * (S' * (Gamma_S * (Lambda_S \ (Gamma_S' * (y - Hxf)))));

elseif strcmp(method,'serialEnSRF')
  % EnSRF with serial observation processing  
  for iobs = 1:m           
     Hloc = H(iobs,:);
     yloc = y(iobs);
     Sloc = Hloc*Xfp;
     Hxfloc = Hloc*xf;
     
     Floc = Sloc*Sloc' + (N-1)*R(iobs, iobs);
     R(iobs,iobs);
     Kloc = Xfp*Sloc' / Floc;
     xa = xf + Kloc * (yloc - Hxfloc);
     alpha = 1.0 / (1.0 + sqrt( (N-1)*R(iobs,iobs)/Floc) );
     Xap = Xfp - alpha * Kloc * Hloc * Xfp;
     
     Xfp = Xap;
     xf = xa;
  end
  
elseif strcmp(method,'ETKF')
  % ETKF with decomposition of Stilde
  sqrtR = sqrtm(R);
  Stilde = sqrt(1/(N-1)) * (sqrtR \ S);

  % "economy size" SVD decomposition
  [U_T,Sigma_T,V_T] = svd(Stilde',0);
  
  if size(Sigma_T,2) > N
    Sigma_T = Sigma_T(:,1:N);
    V_T = V_T(:,1:N);
  end
  Ndim = size(Sigma_T,1);
  
  TTt = eye(N) - S'*(F\S);

  if debug
    sangoma_check(TTt,U_T * ((eye(Ndim)+Sigma_T*Sigma_T') \ U_T'),...
        'ETKF-TTt',tol)

    K2 = 1/sqrt(N-1) * Xfp * ...
        (Stilde' * ((Stilde*Stilde'+ eye(m)) \ inv(sqrtR))) ;
    sangoma_check(K,K2,'ETKF-Kalman gain',tol)
  end

  Xap = Xfp * (U_T * (sqrt(eye(Ndim)+Sigma_T*Sigma_T') \ U_T'));
  xa = xf + 1/sqrt(N-1) *  Xfp * (U_T * ((eye(Ndim)+Sigma_T'*Sigma_T) \ ...
      (Sigma_T * V_T' * (sqrtR \ (y - Hxf)))));

elseif strcmp(method,'ETKF2')
  % ETKF with square-root of invTTt (e.g. Hunt et al., 2007)

  invTTt = (N-1) * eye(N) + S' * (R \ S);
  
  % eig is more precise if the matrix is exactly symmetric
  invTTt = (invTTt + invTTt')/2;
  
  [U_T,Sigma_T] = eig(invTTt);
  if debug
    sangoma_check(U_T*Sigma_T * U_T',invTTt,'ETKF2-eig',tol)
  end
  
  T = U_T * (sqrt(Sigma_T) \ U_T');

  if debug
    sangoma_check(T*T,inv(invTTt),'ETKF2-sym. square root',tol)
  end

  Xap = sqrt(N-1) * Xfp * T;
  xa = xf + Xfp * (U_T * (inv(Sigma_T) * U_T' * (S' * (R \ (y - Hxf)))));
%  keyboard
  
elseif strcmp(method,'ETKF3')
  % ETKF in style of ESTKF
  A_T = zeros(N,N);
  
  for j = 1:N
    for i = 1:N
      if i == j
        A_T(i,j) = 1 - 1/N;
      else
        A_T(i,j) = - 1/N;
      end         
    end
  end

  L = Xf*A_T;
  HL = HXf*(A_T*L);

  if debug
    Pf2 = 1/(N-1) * L * ((A_T'*A_T) \ L');
    sangoma_check(Pf,Pf2','ETKF3-Pf',tol)
    Pf2 = 1/(N-1) * L * L';
    sangoma_check(Pf,Pf2','ETKF3-Pf2',tol)
  end

  invTTt = (N-1)*eye(N) + HL' * (R \ HL);

  [U_T,Sigma_T] = eig(invTTt);

  T = U_T * (sqrt(Sigma_T) \ U_T');
  %T = sqrtm(inv(invTTt));

  if debug
    sangoma_check(T*T,inv(invTTt),'ETKF3-sym. square root',tol)
  end
  
  Xap = sqrt(N-1) * L*T * A_T';
  xa = xf + L * (U_T * (inv(Sigma_T) * U_T' * (HL' * (R \ (y - Hxf)))));

elseif strcmp(method,'EAKF')
  % EAKF
  sqrtR = sqrtm(R);

  Stilde = sqrt(1/(N-1)) * (sqrtR \ S);
  [U_A,Sigma_A,V_A] = svd(Stilde',0);
  Sigma_A = Sigma_A(:,1:N);
  V_A = V_A(:,1:N);

  % eigenvalue decomposition of Pf
  %[Z_A,Gamma_A] = eig(Pf);
  [Z_A,sqrtGamma_A,~] = svd(Xfp,0);

  if debug
    % last eigenvalue should be zero
    sangoma_check(abs(sqrtGamma_A(end,end)),0,'EAKF-gamma',tol)
  end

  Gamma_A = sqrtGamma_A.^2 / (N-1);

  if debug
    sangoma_check(Z_A * Gamma_A * Z_A',Pf,'EAKF-decomposition',tol)
  end

  Xap = 1/sqrt(N-1) * Xfp * (U_A * ((sqrt(eye(N) + Sigma_A^2)) \ ...
      (sqrt(pinv(Gamma_A)) * (Z_A' * Xfp))));

  xa = xf + 1/sqrt(N-1) *  Xfp * (U_A * ((eye(N)+Sigma_A'*Sigma_A) \ ...
      (Sigma_A * V_A' * (sqrtR \ (y - Hxf)))));

elseif strcmp(method,'SEIK')
  % SEIK

  A = zeros(N,N-1);
  A(1:N-1,1:N-1) = eye(N-1);
  A = A - ones(N,N-1)/N;

  L = Xf*A;
  HL = HXf*A;
  
  if debug
    sangoma_check(L,Xfp(:,1:N-1),'SEIK-L matrix',tol)
    Pf2 = 1/(N-1) * L * ((A'*A) \ L');
    sangoma_check(Pf,Pf2','SEIK-Pf',tol)
  end

  invTTt = (N-1)*(A'*A) + HL' * (R \ HL);
  TTt = inv(invTTt);

  T = chol(inv(invTTt))';

  if debug
    sangoma_check(T*T',inv(invTTt),'SEIK-Cholesky decomposition',tol)
  end

  % add omega
  w = ones(N,1)/sqrt(N);
  Omega = householder(w);
  Xap = sqrt(N-1) * L*T*Omega';

  xa = xf + L * (TTt * (HL' * (R \ (y - Hxf))));
elseif strcmp(method,'ESTKF')
  % ESTKF
  A = zeros(N,N-1);
  
  for j = 1:N-1
    for i = 1:N
      if i == j && i < N
        A(i,j) = 1 - 1/N * 1/(1/sqrt(N)+1);
      elseif i < N
        A(i,j) = - 1/N * 1/(1/sqrt(N)+1);
      else
        A(i,j) = - 1/sqrt(N);
      end         
    end
  end

  L = Xf*A;
  HL = HXf*A;

  if debug
    Pf2 = 1/(N-1) * L * ((A'*A) \ L');
    sangoma_check(Pf,Pf2','ESTKF-Pf',tol)
    Pf2 = 1/(N-1) * L * L';
    sangoma_check(Pf,Pf2','ESTKF-Pf2',tol)
  end

  invTTt = (N-1)*eye(N-1) + HL' * (R \ HL);

  [U_E,Sigma_E] = eig(invTTt);

  T = U_E * (sqrt(Sigma_E) \ U_E');
  %T = sqrtm(inv(invTTt));

  if debug
    sangoma_check(T*T,inv(invTTt),'ESTKF-sym. square root',tol)
  end
  
  Xap = sqrt(N-1) * L*T * A';
  xa = xf + L * (U_E * (inv(Sigma_E) * U_E' * (HL' * (R \ (y - Hxf)))));
elseif strcmp(method,'EnKF')
  % EnKF
  sqrtR = sqrtm(R);

  % perturbation of observations
  Yp = sqrtR * randn(m,N);
  Y = Yp + repmat(y,[1 N]);

  [U_F,Sigma_F,V_F] = svd(S + Yp,0);
  [G_F,Gamma_F,Z_F] = svd(S'*(U_F*inv(Sigma_F)));

  % unclear in manuscript
  %Xap = Xfp * G_F * sqrt(eye(N)-Gamma_F*Gamma_F') * G_F';

  Xa = Xf + Xfp * (S' * (U_F * (inv(Sigma_F)^2 * (U_F' * (Y-HXf)))));

  xa = mean(Xa,2);
  Xap = Xa - repmat(xa,[1 N]);
else
  error('sangoma:unknown_method','unknown method "%s"',method);
end

Xa = Xap + repmat(xa,[1 N]);
