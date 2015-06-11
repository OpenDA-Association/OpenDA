% [Ep,info] = wce_simple(mask,pmn,len,Nens,k,...)
%
% Generate ensemble perturbations taking into account: the land-sea mask, 
% correlation length (possibly varying in space) and possibly a vector field 
% (advection constraint). This function works in an arbitrary high dimensional space
% on an orthogonal curvilinear grid characterized by the metric scale factors.
%
% Input parameters:
%   mask: land-sea mask (true: sea and false: land). This array has n dimensions.
%   pmn: cell array of n arrays (each n-dimensional). The arrays are the metric scale factors
%        for the different dimensions (units are (length-scale)^(-1)).
%   len: correlation length. It can be a scalar if the correlation length is 
%        constant and the same in all dimensions or a cell array of n arrays. In the later case
%        each array has to be n-dimensional (units are length-scale).
%   Nens: number of ensemble members to generate.
%   k: number of eigenvector and eigenvalues.
%
% Optional input parameters:
%   'velocity', velocity: vector field for the advection constraint (units: 
%        length-scale). This vector field can be scaled such that the alignment 
%        of the perturbation is satisfactory. The array velocity has the same size as mask.
%
% Output parameters:
%   Ep: perturbations (same size as mask plus the trailing ensemble dimension).
%   info: structure with some intermediate results:
%   info.sv: structure describing the concatenated state vector. 
%   info.WU: eigenvectors. Use info.sv and statevector_unpack 
%       to extract the individual variables from WU.
%   info.lambda: eigenvalues.
%   info.WE: weighting matrix related to the total energy. x' * WE * x is the total 
%       barotropic energy of the vector x.
%
% Note:
% The unit "length-scale" can be for example meters or arc degrees. The choice of
% the unit must be consistent for all parameters.
%
% see wce_example_simple.m, wce_example_simple3d.m

function [Ep,info] = wce_simple(mask,pmn,len,Nens,k,varargin)

% default values

velocity = [];

prop = varargin;
for i=1:length(prop)/2 
  if strcmp(prop{2*i-1},'velocity')
    velocity  = prop{2*i};
    velocity= cat_cell_array(velocity);
  end  
end


pmn = cat_cell_array(pmn);

n = size(pmn,ndims(pmn));

ivol = prod(pmn,n+1);

if isscalar(len)
  len = len*ones([size(mask) n]);  
else
  len = cat_cell_array(len);
end

L = pi/2 * len;
alpha = ones(size(mask));

if ~isempty(velocity)
  velocity = velocity .* len;
end

norm = sum(1./ivol(:));

sv = statevector_init(mask);

N = sv.n;

Nz = sv.numels(1);

disp('Creating sparse matrices');
Dz = sparse_laplacian(mask,pmn,len.^2);

WE = statevector_pack(sv,1./ivol);
WE = spdiag(sqrt(WE));

% the energy of a perturbation x is:  x' * WE^2 * x
% x' * WE^2 * x = \int \phi^2 dv
% (\phi continuous field represented by x, dv volume element)

D = WE * blockdiag(Dz);

% xBx : units m^2
% x' * B * x = \int (\nabla (L^2 \nabla \phi))^2 dv + \alpha \int \phi^2 dv
% (L = len)
B = D'*D + spdiag(alpha(mask)) * WE'*WE;

% add advection constraint
if ~isempty(velocity)
  A = wce_advec_constrain(mask,pmn,n,velocity);
  WA = WE * A;
  
  % x'*(WA'*WA)*x = \int (velocity \cdot \nabla \phi)^2 dv
  B = B + WA'*WA;
end
  
% normalize

B = inv(WE) * B * inv(WE);

disp('Eigenvalue decomposition');

%isp = issparse(B)

if 0
  [U,Lambda] = eig(B);
  lambda = diag(Lambda);

  U=U(:,1:k);
  lambda=lambda(1:k);
else
  option.issym = 1;
  
  % enable/disable screen output
  option.disp = 0;
  option.tol = 1e-6;

  [U,Lambda] = eigs(B,k,'sm',option);  
  lambda = real(diag(Lambda));  
  lambda = lambda(end:-1:1);
  U = U(:,end:-1:1);
end

% spatial modes
WU = inv(WE) * U;

% un-normalize, eigenvalues are unchanged

%U = WE * U;


a = sqrt(norm / sum(lambda.^(-1)));
k = size(WU,2);
U2 = a * WU * diag(lambda.^(-0.5));
E = U2 * randn(k,Nens);

[Ep] = statevector_unpack(sv,E,NaN);
[Up] = statevector_unpack(sv,WU,NaN);

if nargout == 2
  info.B = B;
  info.WE = WE;
  info.U = U;
  info.U2 = U2;
  info.WU = WU;
  info.lambda = lambda;
  info.sv = sv;  
  info.norm = norm;  
  info.Up = Up;  
end


% Copyright (C) 2009,2012 Alexander Barth <a.barth@ulg.ac.be>
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


%  LocalWords:  Ep wce pmn len Nens sv statevector
