function [N,E,Zone]=ell2utm(lat,lon,a,e2,lcm)
% ELL2UTM  Converts ellipsoidal coordinates to UTM.
%   UTM northing and easting coordinates in a 6 degree
%   system.  Zones begin with zone 1 at longitude 180E
%   to 186E and increase eastward.  Formulae from E.J.
%   Krakiwsky, "Conformal Map Projections in Geodesy",
%   Dept. Surveying Engineering Lecture Notes No. 37,
%   University of New Brunswick, Fredericton, N.B.
%   Vectorized.
% Version: 2011-02-19
% Useage:  [N,E,Zone]=ell2utm(lat,lon,a,e2,lcm)
%          [N,E,Zone]=ell2utm(lat,lon,a,e2)
%          [N,E,Zone]=ell2utm(lat,lon,lcm)
%          [N,E,Zone]=ell2utm(lat,lon)
% Input:   lat - vector of latitudes (rad)
%          lon - vector of longitudes (rad)
%          a   - ref. ellipsoid major semi-axis (m); default GRS80
%          e2  - ref. ellipsoid eccentricity squared; default GRS80
%          lcm - central meridian; default = standard UTM def'n
% Output:  N   - vector of UTM northings (m)
%          E   - vector of UTM eastings (m)
%          Zone- vector of UTM zones

% Copyright (c) 2011, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com

if nargin ~= 2 & nargin ~= 3 & nargin ~= 4 & nargin ~= 5
  warning('Incorrect number of input arguments');
  return
end

if nargin == 2 | nargin == 3
  [a,b,e2]=refell('grs80');
end
if nargin == 3 | nargin==5
  Zone=zeros(size(lat));
else
  Zone=floor((rad2deg(lon)-180)/6)+1;
  Zone=Zone+(Zone<0)*60-(Zone>60)*60;
  lcm=deg2rad(Zone*6-183);
end

ko=0.9996;           % Scale factor
No=zeros(size(lat)); % False northing (north)
No(lat<0)=1e7;       % False northing (south)
Eo=500000;           % False easting

lam=lon-lcm;
lam=lam-(lam>=pi)*(2*pi);
  
%fprintf('\nZones\n');
%fprintf('%3d\n',Zone');
%fprintf('\nCentral Meridians\n');
%fprintf('%3d %2d %9.6f\n',rad2dms(lcm)');
%fprintf('\nLongitudes wrt Central Meridian\n');
%fprintf('%3d %2d %9.6f\n',rad2dms(lam)');

f=1-sqrt(1-e2);
RN=a./(1-e2*sin(lat).^2).^0.5;
RM=a*(1-e2)./(1-e2*sin(lat).^2).^1.5;
t=tan(lat);
h=sqrt(e2*cos(lat).^2/(1-e2));
n=f/(2-f);

a0=1+n^2/4+n^4/64;
a2=1.5*(n-n^3/8);
a4=15/16*(n^2-n^4/4);
a6=35/48*n^3;
a8=315/512*n^4;

s=a/(1+n)*(a0*lat-a2*sin(2*lat)+a4*sin(4*lat)- ...
  a6*sin(6*lat)+a8*sin(8*lat));

E1=lam .* cos(lat);
E2=lam.^3 .* cos(lat).^3/6 .* (1-t.^2+h.^2);
E3=lam.^5 .* cos(lat).^5/120 .* ...
    (5-18*t.^2+t.^4+14*h.^2-58*t.^2 .*h.^2+13*h.^4+...
     4*h.^6-64*t.^2 .*h.^4-24*t.^2 .*h.^6);
E4=lam.^7 .*cos(lat).^7/5040 .* ...
    (61-479*t.^2+179*t.^4-t.^6);
E=Eo + ko*RN.*(E1 + E2 + E3 + E4);

N1=lam.^2/2 .* sin(lat) .* cos(lat);
N2=lam.^4/24 .* sin(lat) .* cos(lat).^3 .* ...
    (5-t.^2+9*h.^2+4*h.^4);
N3=lam.^6/720 .* sin(lat) .* cos(lat).^5 .* ...
    (61-58*t.^2+t.^4+270*h.^2-...
     330*t.^2 .*h.^2+445*h.^4+...
     324*h.^6-680*t.^2 .*h.^4+...
     88*h.^8-600*t.^2 .*h.^6-...
     192*t.^2 .*h.^8);
N4=lam.^8/40320 .* sin(lat) .* cos(lat).^7 .* ...
   (1385-311*t.^2+543*t.^4-t.^6);
N=No + ko*RN.*(s./RN + N1 + N2 + N3 + N4);
