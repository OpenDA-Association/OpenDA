% [Xa,xa] = sangoma_local_ensemble_analysis(...
%    Xf,H,y,diagR,part,selectObs,method,...)
%
% Computes analysis ensemble Xa based on forecast ensemble Xf using the 
% observation y.
%
% Inputs:
% Xf: forecast ensemble (n x N)
% H: observation operator (m x n)
% y: observation (m x 1)
% diagR: diagonal of the observation error covariance R (m x 1)
% part: vector of integer "labels". Every element of the state vector with the
%   same number belong to the same subdomain
% selectObs: callback routine to select observations with a within a subdomain. 
%   As input is takes an integer representing the index of the state vector and
%   returns a vector of weights (m x 1). 
%   For example:
%      selectObs = @(i) exp(- ((x(i) - xobs(:)).^2 + (y(i) - yobs(:)).^2)/L^2 );
%   or
%      selectObs = @(i) sangoma_compact_locfun(L,...
%          sqrt((x(i) - xobs(:)).^2 + (y(i) - yobs(:)).^2));
%
%   where: 
%      x and y is the horizontal model grid
%      xobs and yobs are the localtion of the observations
%      L is a correlation length-scale
%
% method: method is one analysis schemes implemented sangoma_ensemble_analysis 
%   (except for EnSRF)
%
% Optional inputs:
% 'display', display: if true, then display progress (false is the default)
% 'minweight', minweight: analysis is performed using observations for which 
%    weights is larger than minweight. (default 1e-8)
% 'HXf', HXf: if non empty, then it is the product H Xf. In this case, H is not
%    used
%
% Output:
% Xa: analysis ensemble (n x N) 
% xa: analysis ensemble mean (n x 1) 

% See also:
% sangoma_ensemble_analysis, sangoma_compact_locfun



function [Xa,xa] = sangoma_local_ensemble_analysis(...
    Xf,H,y,diagR,part,selectObs,method,varargin)

display = false;
minweight = 1e-8;
HXf = [];

for i=1:2:length(varargin)
  if strcmp(varargin{i},'display')
    display = varargin{i+1};
  elseif strcmp(varargin{i},'minweight')
    minweight = varargin{i+1};
  elseif strcmp(varargin{i},'HXf')
    HXf = varargin{i+1};
  else
    error(['unkown argument ' varargin{i}]);
  end 
end


% unique element of partition vector
p = unique(part);


Xa = zeros(size(Xf));
xa = zeros(size(Xf,1),1);

% do not use isempty here because m might be zero
if isequal(HXf,[])
    HXf = H*Xf;
end

% loop over all zones
for i=1:length(p)
    if display
        fprintf('zone %d out of %d\n',i,length(p));
    end
    
    sel = find(part == p(i));
    [weight] = selectObs(sel(1));
    
    % restrict to local observations where weight exceeds minweight
    loc = weight > minweight;
    HXfloc = HXf(loc,:);
    Rloc = diag(diagR(loc) ./ weight(loc));
    yloc = y(loc);
    
    [Xa(sel,:),xa(sel)] = sangoma_ensemble_analysis(Xf(sel,:),[],...
        yloc,Rloc,method,'HXf',HXfloc);        

    
end




% not necessary in octave but for compatability with
% matlab

function S = spdiag(d)
i=1:length(d);
S = sparse(i,i,d);

