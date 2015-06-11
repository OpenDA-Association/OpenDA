% T = sparse_trim(sz1,m)
% operator which trim first and last row (or column) in dimension m

function T = sparse_trim(sz1,m)

n1 = prod(sz1);

sz2 = sz1;
sz2(m) = sz2(m)-2;
n2 = prod(sz2);

n = length(sz1);

% L1

for i=1:n
  vi{i} = 1:sz2(i);
end

IJ = cell(n,1);

[IJ{:}] = ndgrid(vi{:});

for i=1:n
  IJ{i} = IJ{i}(:);
end

L1 = sub2ind(sz2,IJ{:});

IJ{m}=IJ{m}+1;

L2 = sub2ind(sz1,IJ{:});

one = ones(size(L1));

T = sparse(L1, L2, one, n2, n1);


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

