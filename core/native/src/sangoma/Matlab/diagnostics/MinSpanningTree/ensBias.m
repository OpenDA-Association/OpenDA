function biasTerm = ensBias(n,N,p,xp,y)

%--------------------------------------------
% [biasTerm]=ensBias(n,N,p,xp,y)
%
% Function to compute bias term for ensemble correction over
% some time domain for a specific location in space.
%
% INPUTS:   n - number of state variables which are observed
%	        at a single spatial location
%           N  - scalar, number of particles
%           p - number of observation sets in time
%           xp - ensemble forecast matrix of n by N by p
%           y  - observation array of n by p elements
% 
% OUTPUT:   biasTerm - scalar used to correct ensemble for bias over the 
%                      verification window
%
% Date:     27/04/2015
% Author:   Sanita Vetra-Carvalho (Univeristy of Reading)

biasTerm = 0;

for i = 1:p
  for j = 1:N
    biasTerm = biasTerm + (xp(:,j,i)-y(:,i));
  end
end

biasTerm = biasTerm/(N*p);
