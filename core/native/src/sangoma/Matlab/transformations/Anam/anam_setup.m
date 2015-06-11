% anam = anam_setup(x)
%
% Determine the empirical transformation function (empirical Gaussian anamorphosis).
% The transformed data (anam_transform(anam,x)) should follow approximately a Gaussian 
% distribution. The transformation function is a piece-wise linear function.
%
% Input parameters:
%   x: a data sample (vector).
%
% Optional input parameters:
%   'addnoise', addnoise: add Gaussian noise level to the data sample.
%   'method', method: method can be either 'direct' (i.e. the data sample is mapped
%      directly to Gaussian distributed variable) or 'by_uniform' (i.e. an analytical transformation is 
%      first applied to bring the data sample to a bounded interval).
%   'N', N: number of segments of the piece-wise linear function.
%
% Output:
%   anam: a structure describing the transformation used in anam_transform 
%      and anam_invtransform.

% Alexander Barth, 2010

function a = anam_setup(x,varargin)

addnoise = 0;
lim = 1e-4;
a.method = 'direct';
N = 100;

for i=1:2:length(varargin)
  if strcmp(varargin{i},'addnoise')
    addnoise = varargin{i+1};
  elseif strcmp(varargin{i},'method')
    a.method = varargin{i+1};
  elseif strcmp(varargin{i},'N')
    N = varargin{i+1};
  elseif strcmp(varargin{i},'lim')
    lim = varargin{i+1};
  else
    warning(['unknown key ' varargin{i}]);
  end
end

% remove NaNs and Infs
x = x(isfinite(x));
x = x(:);

if addnoise ~= 0
  addnoise
  x = x + addnoise * randnunique(length(x));
end

% detect identical values
if length(unique(x)) ~= length(x)
  error('Different values are present multiple time. Consider to use the addnoise parameter.');
end


if strcmp(a.method,'direct')
  % determine the empirical transormation function (anamorphisis)

  a.x = sort(x);

  % erfinv is the inverse of the error function which is undefined for
  % -1 and 1


  a.y = sqrt(2)*erfinv(linspace(-1+lim,1-lim,length(x)));
elseif strcmp(a.method,'by_uniform')
  a.meanx = mean(x);
  a.stdx = std(x);

  x2 = (x-a.meanx)/a.stdx;
  x2 = (erf( x2 / sqrt(2)) + 1)/2;

  %lim = 1e-15;

  % transformation for uniform distribution
  x2(x2 == min(x2)) = 0;
  x2(x2 == max(x2)) = 1;

  a.yp = linspace(0+lim,1-lim,N);
  a.xp = prctile(x2,100*a.yp);

  a.ypr = linspace(1-lim,0-lim,N);
  a.xpr = prctile(x2,100*a.ypr);

  %a.yp = linspace(0+lim,1-lim,length(x));
  %a.xp = sort(x2);


  % total transformation
  a.x = erfinv(2*a.xp -1)*sqrt(2)*a.stdx + a.meanx;
  a.y = sqrt(2)*erfinv(2*a.yp -1);    
else
  error(['unknown method ' method]);
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


%  LocalWords:  anam anamorphosis addnoise invtransform
