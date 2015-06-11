% Perturbation constrained by harmonic shallow-water equations
% Color corresponds to the ampltiude of elevation and contourlines
% are represent the phase.

% domain size
m = 64;
n = 64;

% grid resolution
dx = 2000e3/(m-1);
dy = dx;
[x,y] = ndgrid(dx*[0:m-1],dy*[0:n-1]);


% land-sea mask (true: sea, false: land)
mask = true(m,n);
mask(1:m,[1 n]) = false;
mask(m,1:n) = false;

% bathymetry
h = ones(m,n) * 100;
h(mask == 0) = NaN;

% metric
pm = ones(m,n)/dx;
pn = ones(m,n)/dy;

% parameters
% tidal frequency (angular frequency in 1/s)
omega = 2*pi/(12*60*60); 
% acceleration due to gravity 
g = 9.81; % m/s2
% local Coriolis frequency
f =  6.8873485167246562E-005;

alpha = 0.0001;

% length-scale of perturbation
len = 20e3; % m

% number of ensemble members
Nens = 1;

% number of EOFs retained
k = 50;

% compute constrained perturbations

[Ezeta,Eu,Ev,info] = wce_tides(h,pm,pn,g,f,len,alpha,omega,k,Nens);

hold on
contourf(x/1e3,y/1e3,abs(Ezeta),50)
caxis([0 max(abs(Ezeta(:)))]);
shading flat
contour(x/1e3,y/1e3,angle(Ezeta),'k')
xlabel('x (km)');
ylabel('y (km)');
title('Perturbation constained by harmonic shallow-water equations');
hold off

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
