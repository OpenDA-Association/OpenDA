function [mask_u,mask_v,mask_psi] = stagger_mask(mask_r)

mask_u = mask_r(1:end-1,:) .* mask_r(2:end,:);
mask_v = mask_r(:,1:end-1) .* mask_r(:,2:end);
mask_psi = mask_r(1:end-1,1:end-1) .* mask_r(1:end-1,2:end) .* mask_r(2:end,1:end-1) .* mask_r(2:end,2:end);


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
