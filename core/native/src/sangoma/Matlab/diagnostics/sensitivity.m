function [S,Pa]=sensitivity(R,H,post_w,xp)

%--------------------------------------------
% [S,Pa]=sensitivity(R,H,post_w,xp)
%
% Calculates sensitivty of the posterior mean to the observations 
% (assuming Gaussian observation error and linear observation operator) 
% within a partical filter.
% To be used after weights have been updated by the observations.
%
% INPUTS:	n - scalar, size of state space
%           R - square matrix of size p, the observation error covariance
%           matrix. p is the size of observation space.
%           H - a matrix of size p by n, the (linear) observation operator.
%           N - scalar, number of particles
%           post_w - vector of size N, posterior weights
%           xp - matrix of n by N, the partical values
% 
% OUTPUT:	S - square matrix of size p, the sensitivity of the posterior
%           mean to the observations in observation space.
%           Pa - sqaure matrix of size n, the posterior covariance matrix.
%
% METHOD:   The sensitivity of the analysis to the observations can be
%           calculated exactly as the ratio of the posterior variance in 
%           observation space to the observation error covariance. See
%           Fowler and van Leeuwen, 2012.
%
% Date:     11/10/2012
% Author:   A. M. Fowler (Univeristy of Reading)

[n,N] = size(xp); % n - number of state space variables;
                  % N - number of particles.

posterior_mean=zeros(n,1);
for i=1:N
    posterior_mean(:)=posterior_mean(:)+(post_w(i)*xp(:,i));
end
Pa=zeros(n);

for i=1:N
        Pa(:,:)=Pa(:,:)+...
            post_w(i)*(xp(:,i)-posterior_mean)*(xp(:,i)-posterior_mean)';
end
S=H*Pa*H'*inv(R);

% Copyright (C) 2012 Alison Fowler <a.m.fowler@reading.ac.uk>
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; If not, see <http://www.gnu.org/licenses/>.

