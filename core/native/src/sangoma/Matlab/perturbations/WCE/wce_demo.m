% show all examples

examples={'wce_example_simple','wce_example_simple_varlen','wce_example_simple_advection','wce_example_tides'};

more off

disp('-----------------------------------------------------------');
disp('This script will show you some examples for generating ');
disp('constrained ensemble perturbations');

for iindex=1:length(examples);
  disp('-----------------------------------------------------------');
  disp(['Running ' examples{iindex} ':']);
  help(examples{iindex});
  clf
  eval(examples{iindex});
  disp('-----------------------------------------------------------');
  drawnow;
  
  if iindex ~= length(examples)
    input('Press Enter to continue (or Ctrl-C to abort)...');
  end

end

disp('Bug reports and improvements are welcome!');
disp('Send them to a.barth@ulg.ac.be.');
disp('Have fun!');

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
