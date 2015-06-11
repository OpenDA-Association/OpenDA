% advection constrain
%
% A*x = velocity \cdot \nabla \phi
% where x is a discrete representation of \phi

function A = wce_advec_constrain(mask,pmn,n,velocity)

velocity= cat_cell_array(velocity);
pmn     = cat_cell_array(pmn);

Dx = cell(n,1);
[Dx{:}] = sparse_gradient(mask,pmn);

sz = size(mask);

A = sparse(0);

vel = reshape(velocity,numel(mask),n);

for i=1:n
  S = sparse_stagger(sz,i);
  m = (S * mask(:) == 1);
  
  d = vel(:,i);
  
  A = A + sparse_diag(d(mask==1)) * sparse_pack(mask) * S' * sparse_pack(m)' * Dx{i};
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

