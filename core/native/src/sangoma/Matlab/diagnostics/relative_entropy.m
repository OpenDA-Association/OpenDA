function RE=relative_entropy(post_w,prior_w)

%--------------------------------------------
% RE=relative_entropy(post_w,prior_w)
%
% Calculates relative entropy within a partical filter.
% To be used after weights have been updated by the observations.
%
% INPUTS:	N - scalar, number of particles
%           post_w - vector of size N, posterior weights
%           prior_w (optional) - vector of size N, prior weights. If not
%           explicitly given these are assumed to be 1/N
% 
% OUTPUT:	RE - scalar, relative entropy of posterior
%				given the prior
%
% METHOD:   RE is given by integral(p(x|y)ln[p(x|y)/p(x)])dx
%           If the particle positions are unchanged during the
%           assimilation, and only the weights are updated, RE can be
%           approximated in terms of the relative weights.
%           RE approx. Sum(post_w*ln(post_w/prior_w))
%
% Date:     12/10/2012
% Author:   A. M. Fowler (Univeristy of Reading)
%--------------------------------------------

N = size(post_w,2); % number of particles

% if prior_w not given set to 1/N
if nargin ~=3
    prior_w(1:N)=1/N;
end
w_ratio=zeros(N,1);


RE=0;
for i=1:N
    w_ratio(i)=post_w(i)/prior_w(i);
    if w_ratio(i)>0
        RE=RE+post_w(i)*log(w_ratio(i));
    end
end

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

