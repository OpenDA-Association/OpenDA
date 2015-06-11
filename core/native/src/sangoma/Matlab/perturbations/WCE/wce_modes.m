% [sv,WU,lambda,WE] = wce_modes(domain,omega,g,f,len,alpha,k,cdrag_u,cdrag_v)
% Computes the eigenvector and eigenvalues of the Hessian matrix of a cost function 
% penalizing deviation from the harmonic shallow water equations as a weak contrain.
%
% Input:
%   domain: structure the metric describing the domain
%   omega: angular frequency (rad/s)
%   g: acceleration due to gravity
%   f: Coriolis frequency
%   len: correlation length
%   alpha: factor penalizing the total energy
%   k: number of eigenvector and eigenvalues
%   cdrag_u, drag_v: linear drag in the u- and v-momentum equation.
%
% Output:
%   sv: structure describing the concatenated state vector. Use statevector_unpack 
%     to extract the individual variables from WU
%   WU: eigenvector
%   lambda: eigenvalues
%   WE: weighing matrix related to the total energy. x' * WE * x is the total batrotopic energy
%     of the vector x.

function [sv,WU,lambda,WE] = wce_modes(domain,omega,g,f,len,alpha,k,cdrag_u,cdrag_v)

global PROFILING

[m,n] = size(domain.mask);

if nargin == 7
  cdrag_u = zeros(m-1,n);
  cdrag_v = zeros(m,n-1);
end

  
% time scale in s
Ts = sqrt(domain.h/g);

mask = domain.mask;
mask_u = domain.mask_u;
mask_v = domain.mask_v;

pm = domain.pm;
pm_u = domain.pm_u;
pm_v = domain.pm_v;

pn = domain.pn;
pn_u = domain.pn_u;
pn_v = domain.pn_v;

h_u = domain.h_u;
h_v = domain.h_v;

sv = statevector_init(mask,mask_u,mask_v);

N = sv.n;

Nz = sv.numels(1);
Nu = sv.numels(2);
Nv = sv.numels(3);

if isscalar(alpha)
  alpha = alpha * ones(m,n);
end

alpha_u = stagger_r2u(alpha);
alpha_v = stagger_r2v(alpha);

tic

disp('Creating sparse matrices');
Dz = sparse_laplacian(mask,{pm,pn});
Du = sparse_laplacian(mask_u,{pm_u,pn_u});
Dv = sparse_laplacian(mask_v,{pm_v,pn_v}); 

disp('Creating model constraint');
A = wce_tidal_constrain_sparse(domain,sv,g,f,omega,cdrag_u,cdrag_v);


Alpha = statevector_pack(sv,alpha,alpha_u,alpha_v);
Alpha = spdiag(Alpha);

% energy norm based on
% 
% 1/2 \int g \zeta^2 + h (u^2 + v^2) dS

WE = statevector_pack(sv,...
                             g./(2*pm.*pn),...
                             h_u ./(2*pm_u.*pn_u),...
                             h_v ./(2*pm_v.*pn_v));
WE = spdiag(sqrt(WE));


% the energy of a perturbation x is:  x' * WE^2 * x

%WE = speye(N);

D = WE * blockdiag(Dz,Du,Dv);

% xBx : units m^2

A = WE * A/omega;
B = A'*A +  ( len^4 * (D'*D) + Alpha * WE'*WE);

% normalize

B = inv(WE) * B * inv(WE);

PROFILING.time_B_matrix = toc();

tic
  
disp('Eigenvalue decomposition');

%isp = issparse(B)

if 0
  [U,Lambda] = eig(B);
  lambda = diag(Lambda);

  U=U(:,1:k);
  lambda=lambda(1:k);
else
  option.issym = 1;
  option.disp = 0;

  [U,Lambda] = eigs(B,k,'sm',option);  
  lambda = real(diag(Lambda));  
  lambda = lambda(end:-1:1);
  U = U(:,end:-1:1);
end



% spatial modes
WU = inv(WE) * U;


PROFILING.time_eigenvalue_decomp = toc();



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

