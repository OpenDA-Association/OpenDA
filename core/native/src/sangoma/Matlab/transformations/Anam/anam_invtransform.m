% x = anam_invtransform(anam,y)
%
% Apply inverse transformation to y according to the transformation anam.
%
% Input:
%   anam: transform (created by anam_setup).
%   y: transformed data.
%
% Output:
%   x: data in original scale.

% Alexander Barth, 2010

function x = anam_invtransform(a,y)


if strcmp(a.method,'by_uniform')
  y2 = (erf( y / sqrt(2)) + 1)/2;
  x2 = interp1(a.yp,a.xp,y2);
  x = a.stdx * sqrt(2)*erfinv(2*x2 -1) + a.meanx;

%  y2,x2,x
else
  x = interp1(a.y,a.x,y);
end

m = y < a.y(1);
count = sum(m(:));
if count > 0
  warning('minimum reached %d time(s) ',count);
  x(m) = a.x(1);
end


m = y > a.y(end);
count = sum(m(:));
if count > 0
  warning('maximum reached %d time(s) ',count);
  x(m) = a.x(end);
end


% Copyright (C) 2012 Alexander Barth <a.barth@ulg.ac.be>
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
