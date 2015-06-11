% f2 = diffusion2(f1,alpha,N)
%
% Two-dimensional diffusion of field f
%
% Input:
%   f1: initial condition of field.
%   alpha: vector of two elements. Diffusion coefficient (multiplied by grid spacing) for the two dimensions.
%   N: number of iterations.
% 
% Output:
%   f2: field after N iterations.

% Author: Alexander Barth <a.barth@ulg.ac.be>, 2010

function f = diffusion2(f,alpha,N)

[imax,jmax] = size(f);

f(isnan(f)) = 0;

F_u = zeros(imax+1,jmax);
F_v = zeros(imax,jmax+1);

for i=1:N

  F_u(2:end-1,:) = alpha(1) * (f(2:end,:)-f(1:end-1,:));
  F_v(:,2:end-1) = alpha(2) * (f(:,2:end)-f(:,1:end-1));

  f =  f + (F_u(2:end,:)-F_u(1:end-1,:) + F_v(:,2:end)-F_v(:,1:end-1));

end

% Copyright (C) 2010 Alexander Barth <a.barth@ulg.ac.be>
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
