% S = sparse_stagger(sz1,m)
% operator for staggering a field of size sz1 in dimension m

function S = sparse_stagger(sz1,m,cyclic)


if nargin == 2
    cyclic = false;
end

n1 = prod(sz1);

sz2 = sz1;

if ~cyclic
    sz2(m) = sz2(m)-1;
end
n2 = prod(sz2);

n = length(sz1);

for i=1:n
  vi{i} = 1:sz2(i);
end

IJ = cell(n,1);

[IJ{:}] = ndgrid(vi{:});

for i=1:n
  IJ{i} = IJ{i}(:);
end

L1 = [1:n2]';

L2 = sub2ind(sz1,IJ{:});
v = ones(size(L1))/2;

IJ{m} = IJ{m} + 1;

if cyclic    
    IJ{m} = mod(IJ{m}-1,sz1(m))+1;
end

L2o = sub2ind(sz1,IJ{:});

S = sparse( ...
       [L1;  L1;  ],  ...
       [L2;  L2o; ],  ...
       [v;   v    ], n2 , n1 );



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

