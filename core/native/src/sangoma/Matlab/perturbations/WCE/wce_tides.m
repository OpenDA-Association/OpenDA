% [Ezeta,Eu,Ev,info] = wce_tides(h,pm,pn,g,f,len,alpha,omega,k,Nens,cdragu,cdragv)
%
% Generate ensemble perturbations constrained by the harmonic shallow water equations as a weak constrain.
% It can be used to create perturbations for tidal parameters.
%
% Input parameters:
%   h: bathymetry (in m, two-dimensional array, positive in water and NaN on land).
%   pm: inverse of the local resolution in the first dimension (in m^-1, same size as h).
%   pn: inverse of the local resolution in the second dimension (in m^-1, same size as h).
%   g: acceleration due to gravity (scalar, m/s^2).
%   f: Coriolis frequency (scalar, 1/s).
%   len: correlation length (scalar, in m).
%   alpha: factor penalizing the total energy (adimensional).
%   omega: angular frequency (rad/s).
%   k: number of eigenvector and eigenvalues.
%   Nens: number of ensemble members to generate.
%
% Optional input parameters:
%   cdrag_u, cdrag_v: linear drag in the u- and v-momentum equation (no drag is assumed if they are omitted). 
%
% Output parameters:
%   Ezeta, Eu, Eu: perturbation for elevation (in m), u and v (depth averaged velocity in m/s). 
%     The shape of these arrays is the same as mask.
%   info: structure with some intermediate results:
%   info.sv: structure describing the concatenated state vector. 
%   info.WU: eigenvector. Use info.sv and statevector_unpack 
%       to extract the individual variables from WU.
%   info.lambda: eigenvalues.
%   info.WE: weighting matrix related to the total energy. x' * WE * x is the total barotropic energy
%       of the vector x.
%
% Note:
% see wce_example_tides.m

function [Ezeta,Eu,Ev,info] = wce_tides(h,pm,pn,g,f,len,alpha,omega,k,Nens,cdragu,cdragv)

if nargin == 10
  cdragu = zeros(size(h));
  cdragv = zeros(size(h));
end

  
domain.mask = ~isnan(h);
domain.h = h;
domain.h(isnan(h)) = 0;
domain.pm = pm;
domain.pn = pn;

[domain.mask_u,domain.mask_v] = stagger_mask(domain.mask);
[domain.h_u,domain.h_v] = stagger(domain.h);
[domain.pm_u,domain.pm_v] = stagger(domain.pm);
[domain.pn_u,domain.pn_v] = stagger(domain.pn);

[cdrag_u] = stagger_r2u(cdragu);
[cdrag_v] = stagger_r2v(cdragv);

[sv,WU,lambda,WE] = wce_modes(domain,omega,g,f,len,alpha,k,cdrag_u,cdrag_v);

dS = 1./(domain.pm.*domain.pn);
surf = sum(dS(domain.mask == 1));
norm_per_surf = 1;
norm = norm_per_surf * surf;

[Ezeta,Eu,Ev,E] = wce_pert(sv,WU,lambda,WE,norm,Nens);



if nargout == 4
  info.sv = sv;
  info.WU = WU;
  info.lambda = lambda;
  info.WE = WE;  
  info.norm = norm;  
  [info.Uzeta,info.Uu,info.Uv] = statevector_unpack(sv,WU,NaN);
end


% Copyright (C) 2009 Alexander Barth <a.barth@ulg.ac.be>
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


%  LocalWords:  Ezeta wce pn len Nens cdragu cdragv NaN cdrag sv statevector
