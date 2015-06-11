% [Dx1,Dx2,...,Dxn] = sparse_gradient(mask,pmn)
%
% Form the gradient using finite differences in all n-dimensions
%
% Input:
%   mask: binary mask delimiting the domain. 1 is inside and 0 outside.
%         For oceanographic application, this is the land-sea mask.
% 
%   pmn: scale factor of the grid. 
%
% Output:
%   Dx1,Dx2,...,Dxn: sparce matrix represeting a gradient along 
%     different dimensions 

function varargout = sparse_gradient(mask,pmn,iscyclic)

H = sparse_pack(mask);

sz = size(mask);
n = size(pmn,ndims(pmn));
Pmn = reshape(pmn,[prod(sz) n]);    

if nargin == 2
  iscyclic = zeros(n,1);
end

for i=1:n
  % staggering operator
  S = sparse_stagger(sz,i,iscyclic(i));

  % mask for staggered variable
  m = (S * mask(:) == 1);
  
  d = m .* (S * Pmn(:,i));

  varargout{i} = sparse_pack(m) * sparse_diag(d) * sparse_diff(sz,i,iscyclic(i)) * H';
%  varargout{i} = sparse_diag(d) * sparse_diff(sz,i) * H';
%  size(varargout{i})
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

