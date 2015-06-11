function D = blockdiag(varargin);

for i=1:nargin
  [m(i),n(i)] = size(varargin{i});
end


j = [0 cumsum(m)];
k = [0 cumsum(n)];

if 0
D = sparse([],[],[],sum(m),sum(n));

for i=1:nargin
  D(j(i)+1:j(i+1),k(i)+1:k(i+1)) = varargin{i};
end
else

Di = [];
Dj = [];
Ds = [];

for i=1:nargin
  [Vi,Vj,Vs] = find(varargin{i});
  Di = [Di; Vi+j(i)];
  Dj = [Dj; Vj+k(i)];
  Ds = [Ds; Vs];

%  D(j(i)+1:j(i+1),k(i)+1:k(i+1)) = varargin{i};
end
D = sparse(Di,Dj,Ds,sum(m),sum(n));

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

