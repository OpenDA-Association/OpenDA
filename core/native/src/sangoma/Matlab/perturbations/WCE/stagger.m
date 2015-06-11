function [x_u,x_v,x_psi] = stagger(x_r)

x_u =  (x_r(1:end-1,:,:) + x_r(2:end,:,:))/2;
x_v =  (x_r(:,1:end-1,:) + x_r(:,2:end,:))/2;
x_psi =  (x_r(1:end-1,1:end-1,:) +x_r(1:end-1,2:end,:)  + x_r(2:end,1:end-1,:)  +x_r(2:end,2:end,:))/4;


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

