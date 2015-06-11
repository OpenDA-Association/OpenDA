% C = wce_tidal_constrain_sparse(domain,sv,g,f,omega,cdrag_u,cdrag_v)
% Creates a sparse matrix C such that the system C x = 0 represents the harmonic shallow water equations.
%
% Input:
%   domain: structure the metric describing the domain
%   sv: structure describing the concatenated state vector. 
%   g: acceleration due to gravity
%   f: Coriolis frequency
%   omega: angular frequency (rad/s)
%   cdrag_u, drag_v: linear drag in the u- and v-momentum equation.
%
% Output:
%   C: sparse representation of the harmonic shallow water equations


function C = wce_tidal_constrain_sparse(domain,sv,g,f,omega,cdrag_u,cdrag_v)

mask = domain.mask;
mask_u = domain.mask_u;
mask_v = domain.mask_v;

[m,n] = size(domain.mask);

Pr = sparse_pack(mask == 1);
Pu = sparse_pack(mask_u == 1);
Pv = sparse_pack(mask_v == 1);

UP = blockdiag(Pr',Pu',Pv');

Iu = sparse_diag(domain.h_u ./ domain.pn_u);
Iv = sparse_diag(domain.h_v ./ domain.pm_v);


SEx = sparse_stagger_ext([m-1 n-1],2) * sparse_stagger([m n-1],1);
SEy = sparse_stagger_ext([m-1 n-1],1) * sparse_stagger([m-1 n],2);


I = 2:m-1;
J = 2:n-1;

Txy = sparse_trim([m-2 n],2) * sparse_trim([m n],1);
DUx = sparse_diff([m-1 n-2],1);
TUy = sparse_trim([m-1 n],2);
DVy = sparse_diff([m-2 n-1],2);
TVx = sparse_trim([m n-1],1);
dS = sparse_diag(domain.pm(I,J).*domain.pn(I,J));
Hp = sparse_pack(mask(I,J) == 1);


I = 1:m-1;
J = 1:n;

Dx = sparse_diff([m n],1);
PMu = sparse_diag(domain.pm_u(I,J));
Drgu = sparse_diag(i*omega + cdrag_u(I,J));
Hu = sparse_pack(mask_u(I,J) == 1);


I = 1:m;
J = 1:n-1;

Dy = sparse_diff([m n],2);
PMv = sparse_diag(domain.pm_v(I,J));
Drgv = sparse_diag(i*omega + cdrag_v(I,J));
Hv = sparse_pack(mask_v(I,J) == 1);

M = [i*omega*Hp*Txy  Hp*dS*DUx*TUy*Iu   Hp*dS*DVy*TVx*Iv; ...
     g*Hu*PMu*Dx     Hu*Drgu            -f*Hu*SEx; ...
     g*Hv*PMv*Dy     f*Hv*SEy           Hv*Drgv];

Ex = sparse_diag(ones(sv.n,1));
Ex = Ex(:,1:size(M,1));

C = Ex * M* UP;





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

