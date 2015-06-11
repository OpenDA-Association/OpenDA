% example Gaussian anamorphosis
  
disp('Example of Gaussian anamorphosis');
disp('Generate log-normal distributed data');
  
% initial distribution
x = exp(randn(100000,1));

% initalize transformation
anam = anam_setup(x);

% apply transformation
y = anam_transform(anam,x);

% test if y is gaussian distributed
if isempty(which('kstest'))
  % for octave
  kstest = @(x) kolmogorov_smirnov_test(x,'norm') < 0.05;
end

assert(kstest(y) == 0)

disp('Transormed data pass Kolmogorov-Smirnov test');


% Copyright (C) 2012-2014 Alexander Barth <a.barth@ulg.ac.be>
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
