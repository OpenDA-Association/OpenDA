% Perturbation aware of land-sea mask in 3d

% domain size
m = 50;
n = 50;
k = 10;

% grid resolution
dx = 200e3/(m-1);
dy = dx;
dz = 10;

[x,y] = ndgrid(dx*[0:m-1],dy*[0:n-1],dz*[0:k-1]);

% land-sea mask (true: sea, false: land)
mask = true(m,n,k);
m2 = floor(m/2);
mask(1:m2,m2,:) = false;


% metric
pm = ones(m,n,k)/dx;
pn = ones(m,n,k)/dy;
po = ones(m,n,k)/dz;

% number of ensemble perturbations
Nens = 1;

% number of EOFs retained
keof = 100;

% correlation length
len =  20e03 * ones(m,n,k);
lenz = 20 * ones(m,n,k); 
% create ensemble perturbations
[Ep] = wce_simple(mask,{pm,pn,po},{len,len,lenz},Nens,keof);

kk = 5;
contourf(x(:,:,kk)/1e3,y(:,:,kk)/1e3,Ep(:,:,kk),50)
shading flat
xlabel('x (km)');
ylabel('y (km)');
title('Perturbation aware of land-sea mask');

% Copyright (C) 2012 Alexander Barth <a.barth@ulg.ac.be>
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
