function x = cat_cell_array(x)

do_grid = 0;

if iscell(x)
  n = length(x);
  
  if do_grid
    all_vectors = 1;
  
    for i=1:n
      all_vectors = all_vectors & isvector(x{i});
    end

    if all_vectors
      [x{:}] = ndgrid(x{:});
    end
  end

  sz = size(x{1});
  for i=2:n
    if ~isequal(sz,size(x{i}))
      %sz
      %size(x{i})
      error('all arguments in cell array should have the same size');
    end
  end
    
  x = cat(n+1,x{:});
  end
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

