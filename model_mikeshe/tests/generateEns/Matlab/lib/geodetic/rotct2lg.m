function R=rotct2lg(lat,lon)
% ROTCT2LG  Forms rotation matrix to convert from CT
%   coordinate system to LG (NEU) coordinate system.
%   If astronomic lat,lon input, then output is in
%   local astronomic system. Non-vectorized. See also
%   ROTLG2CT.
% Version: 2011-02-19
% Useage:  R=rotct2lg(lat,lon)
% Input:   lat - lat of local system origin (rad)
%          lon - lon of local system origin (rad)
% Output:  R - Rotation matrix to convert from CT to LG (NEU)

% Copyright (c) 2011, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com

if nargin ~= 2
  warning('Incorrect number of input arguments');
  return
end

R=zeros(3,3);
R(1,1)=-sin(lat).*cos(lon);
R(1,2)=-sin(lat).*sin(lon);
R(1,3)=cos(lat);
R(2,1)=-sin(lon);
R(2,2)=cos(lon);
R(2,3)=0;
R(3,1)=cos(lat).*cos(lon);
R(3,2)=cos(lat).*sin(lon);
R(3,3)=sin(lat);
