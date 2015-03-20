function Cct=clg2cct(Clg,lat,lon)
% CLG2CCT  Convert local geodetic covariance matrix to CT.
%   Vectorized. See also CCT2CLG.
% Version: 1996-02-29
% Useage:  Cct=clg2cct(Clg,lat,lon)
% Input:   Clg - LG covariance matrix
%          lat - vector of station latitudes (rad)
%          lon - vector of station longitudes (rad)
% Output:  Cct - CT covariance matrix

% Copyright (c) 2011, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com

n=length(lat);
if (n*3 ~= max(size(Clg)) )
  error('Size of lat,lon does not match size of Clg');
end

for i=1:n
  sinlat=sin(lat(i));
  coslat=cos(lat(i));
  sinlon=sin(lon(i));
  coslon=cos(lon(i));
  Ji=[-sinlat*coslon  -sinlat*sinlon  coslat
      -sinlon          coslon         0
       coslat*coslon   coslat*sinlon  sinlat];
  indi=(i-1)*3+[1:3];
  for j=1:n
    sinlat=sin(lat(j));
    coslat=cos(lat(j));
    sinlon=sin(lon(j));
    coslon=cos(lon(j));
    Jj=[-sinlat*coslon  -sinlat*sinlon  coslat
        -sinlon          coslon         0
         coslat*coslon   coslat*sinlon  sinlat];
    indj=(j-1)*3+[1:3];
    Cct(indi,indj)=Ji'*Clg(indi,indj)*Jj;
%    Cct(indj,indi)=Cct(indi,indj)';
  end
end
