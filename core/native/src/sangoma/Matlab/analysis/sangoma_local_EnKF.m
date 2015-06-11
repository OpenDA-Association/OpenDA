% [Xa,xa] = sangoma_local_EnKF(Xf,HXf,y,R,rho_PH,rho_HPH,...)
% Compute the local stochastic EnKF analysis obtained by element-wise 
% multiplication of Pf * H' and H * Pf * H' by a localization function (rho_PH 
% and rho_HPH respectively)
%
% Inputs:
% Xf: forecast ensemble (n x N)
% HXf: observed part of the forecast ensemble (m x N)
% y: observation (m x 1)
% R: observation error covariance R (m x m)
% rho_PH: localization function of Pf * H'
% rho_HPH: localization function of H * Pf * H'
%   For example:
%     rho_PH = @(i,j) sangoma_compact_locfun(...
%          L,sqrt((x(i) - xobs(j)).^2 + (y(i) - yobs(j)).^2));
% 
%     rho_HPH = @(i,j) sangoma_compact_locfun(...
%          L,sqrt((xobs(i) - xobs(j)).^2 + (yobs(i) - yobs(j)).^2));
% 
%   where: 
%      x and y is the horizontal model grid
%      xobs and yobs are the localtion of the observations
%      L is a correlation length-scale
%
% Optional inputs:
% Yp: ensemble of observation perturbations to be added to y (n x N)
%
% Output:
% Xa: analysis ensemble (n x N) 
% xa: analysis ensemble mean (n x 1) 
%
% Note:
% R can be empty, in which case an ensemble of observation perturbations Yp must 
% be provided. The analysis is done by the following formula:
%
% Xa = Xf + PfH * inv(HPfH + Re) * (Y-HXf);
% 
% where Re is derived from the observation perturbations (Re = Yp*Yp'/(N-1) 
% where N is the ensemble size) if R is not provided. Otherwise R is used 
% instead of Re in the previous equation.
% 
% See also:
% sangoma_local_ensemble_analysis, sangoma_ensemble_analysis, 
% sangoma_compact_locfun


function [Xa,xa] = sangoma_local_EnKF(Xf,HXf,yo,R,rho_PH,rho_HPH,varargin)

Yp = [];
for i=1:2:length(varargin)
  if strcmp(varargin{i},'Yp')
    Yp = varargin{i+1};
  else
    error(['unkown argument ' varargin{i}]);
  end 
end

% ensemble size, number of observation and state vector size
N = size(Xf,2);
m = size(yo,1);
n = size(Xf,1);

% ensemble mean and perturbations
xf = mean(Xf,2);
Xfp = Xf - repmat(xf,[1 N]);

% observed of of ensemble mean and perturbations
Hxf = mean(HXf,2);
S = HXf - repmat(Hxf,[1 N]);

HPfH = (S * S') / (N-1);

% localize HPfH 

for j = 1:m
  for i = 1:m
    HPfH(i,j) = HPfH(i,j) * rho_HPH(i,j);
  end
end


if ~isempty(R)
  % square root of observation error covariance matrix
  sqrtR = sqrtm(R);

  % perturbation of observations
  Yp = sqrtR * randn(m,N);
else
  % use ensemble perturbations to compute an ensemble approximation of R  
  R = Yp * Yp'/(N-1);
end

% add perturbation to observations
Y = Yp + repmat(yo,[1 N]);

% normalize innovation vector by its covariance matrix
tmp = ((HPfH + R) \ (Y-HXf))/(N-1);

% inc is the increment for each ensemble member
% inc = ((Xfp * S').*D) * tmp;

% avoid forming n x m matrices explicetly

inc = zeros(n,N);

for i = 1:n
    % PfH_loc(i,j) = Xfp(i,k) * S(j,k) * D(i,j) with sum over k
    
    % i-th row of Pf H'
    PfH_loc = Xfp(i,:) * S';
    
    % apply localization
    for j = 1:m
       PfH_loc(j) = PfH_loc(j) * rho_PH(i,j);           
    end
    
    inc(i,:) = PfH_loc * tmp;
end


Xa = Xf + inc;
xa = mean(Xa,2);
