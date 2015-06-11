function [Ezeta,Eu,Ev,E] = wce_pert(sv,WU,lambda,WE,norm,Nens)

alpha = sqrt(norm / sum(lambda.^(-1)));

k = size(WU,2);

U2 = alpha * WU * diag(lambda.^(-0.5));
E = U2 * randn(k,Nens);

[Ezeta,Eu,Ev] = statevector_unpack(sv,E,NaN);

% check:
% this should be ~ norm
%err_norm_ens = trace(WE^2 * E*E') / Nens / norm
%err_norm_ens = sum(sum(abs(WE * E).^2)) / Nens / norm

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

