% Perturbation with variable correlation length

% domain size
m = 50;
n = 50;

% grid resolution
dx = 200e3/(m-1);
dy = dx;
[x,y] = ndgrid(dx*[0:m-1],dy*[0:n-1]);

% land-sea mask (true: sea, false: land)
mask = true(m,n);

% metric
pm = ones(m,n)/dx;
pn = ones(m,n)/dy;

% number of ensemble perturbations
Nens = 1;

% number of EOFs retained
k = 100;

% correlation length
len = repmat(linspace(3e3,10e3,m)',[1 n]);

% create ensemble perturbations
[Ep] = wce_simple(mask,{pm,pn},{len,len},Nens,k);


contourf(x/1e3,y/1e3,Ep,50)
shading flat
xlabel('x (km)');
ylabel('y (km)');
title('Perturbation with variable correlation length');


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
