% ua = hfradar_vaverage(u,z,nu)
%
% Average current u over the vertical similar to a HF radar system

function ua = hfradar_vaverage(u,z,nu)

% speed of light
c = 299792458; % m / s

% wave length of electromagnetic waves
lambda = c/nu;

% Bragg wavelength
lambdab = c/nu/2;

kb = 2*pi/lambdab;


% check!!!
f = exp(kb*z);

f(isnan(u)) = 0;
u(isnan(u)) = 0;

sumf = sum(f,3);
sumf(sumf == 0) = NaN;

ua = sum(f.*u,3)./sumf;



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
