% Lap = sparse_laplacian(mask,pmn,nu);
%
% Form a Laplacian using finite differences
%
% Input:
%   mask: binary mask delimiting the domain. 1 is inside and 0 outside.
%         For oceanographic application, this is the land-sea mask.
%   pmn: scale factor of the grid. 
%   nu: diffusion coefficient of the Laplacian (depends on location and direction)
%
% Output:
%   Lap: sparce matrix represeting a Laplaciant 
%
% Lap*x = \nabla \cdot (nu \nabla \phi)
% where x is a discrete representation of \phi


function Lap = sparse_laplacian(mask,pmn,nu)

pmn = cat_cell_array(pmn);

sz = size(mask);
n = size(pmn,ndims(pmn));

if (nargin == 2)
  nu = ones([sz n]);
else
  nu = cat_cell_array(nu);
end



% extraction operator of sea points
H = sparse_pack(mask==1);


sz = size(mask);

DD = sparse(0);

Pmn = reshape(pmn,[prod(sz) n]);    
Nu = reshape(nu,[prod(sz) n]);    

for i=1:n
  S = sparse_stagger(sz,i);

  d = S * mask(:) == 1;
  
  % metric
  for j = 1:n
    tmp = S * Pmn(:,j);
    
    if j==i
      d = d .* tmp;
    else
      d = d ./ tmp;
    end  
  end
  % "diffusion coefficient"

  
  d = d .* (S * Nu(:,i));

  szt = sz;
  szt(i) = szt(i)+1;
  extx = sparse_trim(szt,i)';
  
  % Flux operators D

  D = extx * sparse_diag(d) * sparse_diff(sz,i);
  
  % add laplacian along dimension i
  
  DD = DD + sparse_diff(szt,i) * D;
  
end

ivol = prod(pmn,n+1);

% Laplacian on regualar grid DD
DD = sparse_diag(ivol) * DD;

% Laplacian on grid with on sea points Lap
Lap = H * DD * H';




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

