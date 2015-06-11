function S2 = sparse_stagger_ext(sz1,p)

n = numel(sz1);

n1 = prod(sz1);

sz2 = sz1;
sz2(p) = sz2(p)+1;
n2 = prod(sz2);

% % p == 2

% m = sz1(1);
% np = sz1(2);

% S = sparse(  n2,n1 );

% for j=2:np
%   for i=1:m
%     l = sub2ind(sz2,i,j);
    
%     S(l,sub2ind(sz1,i,j-1)) = .5;
%     S(l,sub2ind(sz1,i,j)) = .5;
%   end 
% end

% for i=1:m
%    S(sub2ind(sz2,i,1),sub2ind(sz1,i,1) ) = 1;
%    S(sub2ind(sz2,i,np+1),sub2ind(sz1,i,np) ) = 1;
% end

%v_u2(:,2:n-1) = (tmp(:,1:n-2)+tmp(:,2:n-1))/2;
%v_u2(:,1) = tmp(:,1);
%v_u2(:,n) = tmp(:,n-1);

for i=1:n
  vi{i} = 1:sz1(i);
end
vi{p} = 2:sz1(p);

IJ = cell(n,1);

[IJ{:}] = ndgrid(vi{:});
L1 = sub2ind(sz2,IJ{:});
L2 = sub2ind(sz1,IJ{:});


v = ones(size(L1))/2;

cs = [1 cumprod(sz1)];
offset = cs(p);

S2 = sparse( ...
       [L1;     L1;        ],  ...
       [L2;     L2-offset; ],  ...
       [v;   v        ], n2 , n1 );


% BC for dimension p

for i=1:n
  vi{i} = 1:sz1(i);
end
vi{p} = 1;
IJ = cell(n,1);

[IJ{:}] = ndgrid(vi{:});

L1 = sub2ind(sz2,IJ{:});
L2 = sub2ind(sz1,IJ{:});

v = ones(size(L1));


S2 = S2 + sparse( ...
       [L1;          ],  ...
       [L2;   ],  ...
       [v;          ], n2 , n1 );


for i=1:n
  vi{i} = 1:sz1(i);
end
vi{p} = sz1(p);
IJ = cell(n,1);
[IJ{:}] = ndgrid(vi{:});


L1 = sub2ind(sz2,IJ{:});
L2 = sub2ind(sz1,IJ{:});

S2 = S2 + sparse( ...
       [L1+offset;          ],  ...
       [L2;   ],  ...
       [v;          ], n2 , n1 );





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

