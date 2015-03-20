function Clg=cct2clg(Cct,lat,lon)
% CCT2CLG  Convert CT covariance matrix to local geodetic.
%   Vectorized. See also CLG2CCT.
% Version: 1996-02-29
% Useage:  Clg=cct2clg(Cct,lat,lon)
% Input:   Cct - CT covariance matrix
%          lat - vector of station latitudes (rad)
%          lon - vector of station longitudes (rad)
% Output:  Clg - LG covariance matrix

% Copyright (c) 2011, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com

n=length(lat);
if (n*3 ~= max(size(Cct)) )
  error('Size of lat,lon does not match size of Cct');
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
    Clg(indi,indj)=Ji*Cct(indi,indj)*Jj';
%    Clg(indj,indi)=Clg(indi,indj)';
  end
end
