% y = anam_transform(anam,x)
%
% Transform the data x according to the transformation anam.
%
% Input:
%   anam: transform (created by anam_setup).
%   x: original data.
%
% Output:
%   y: transformed data.

% Alexander Barth, 2010
% Marie-Eve, 2010

function y = anam_transform(a,x)



if strcmp(a.method,'by_uniform')
    xnorm = (x-a.meanx)/a.stdx;
    method = 'pchip';
    
    % avoid number 1 - "a very small number"
    i = xnorm <= 0;
    
    x2 = cdf('Normal',xnorm(i),0,1);
    y2 = interp1(a.xp,a.yp,x2,method);
    y(i) = icdf('Normal',y2,0,1);
    
    x2 = cdf('Normal',-xnorm(~i),0,1);
    y2 = interp1(1-a.xp,1-a.yp,x2,method);
    y(~i) = -icdf('Normal',y2,0,1);
    
    %  x2-1,y2,y
    %  toto = 2*y2 -1
else
    y = interp1(a.x,a.y,x,'linear','extrap');
end

if any(isnan(y))
    warning('suspect value');
    % x(isnan(y))
    % a.x(1)
    % a.x(end)
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
