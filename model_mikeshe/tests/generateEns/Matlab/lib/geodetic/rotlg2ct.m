function R=rotlg2ct(lat,lon)
% ROTLG2CT  Forms rotation matrix to convert from LG
%   (NEU) coordinate system to CT coordinate system.
%   If astronomic lat,lon input, then output is in
%   local astronomic system. Non-vectorized. See also
%   ROTCT2LG.
% Version: 9 Jun 2000
% Useage:  R=rotlg2ct(lat,lon)
% Input:   lat - lat of local system origin (rad)
%          lon - lon of local system origin (rad)
% Output:  R - Rotation matrix to convert from LT (NEU) to CT

% Copyright (c) 2011, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com

if nargin ~= 2
  warning('Incorrect number of input arguments');
  return
end

R=zeros(3,3);
R(1,1)=-sin(lat).*cos(lon);
R(2,1)=-sin(lat).*sin(lon);
R(3,1)=cos(lat);
R(1,2)=-sin(lon);
R(2,2)=cos(lon);
R(3,2)=0;
R(1,3)=cos(lat).*cos(lon);
R(2,3)=cos(lat).*sin(lon);
R(3,3)=sin(lat);
