% [velg,velf] = hfradar_extractf(domain,site,u,v)
%
% Extract the model equivalent of HF radar surface currents. The currents u and v are specified
% on an Arakawa-C grid.
%
% Input:
%   domain: structure describing the model domain with the following fields:
%       domain.lon_u, domain.lon_v: longitude of model grid at u/v points (degrees east).
%       domain.lat_u, domain.lat_v: latitude of model grid at u/v points (degrees north).
%       domain.z_u, domain.z_v: depth of model grid at u/v points (negative in water).
%   site: structure describing the HF radar site with the following fields:
%       site.nu: the frequency of the HF radar system in Hz.
%       site.res: the effective azimuthal resolution in degrees.
%       site.lon0, site.lat0: longitude and latitude of the HF radar system.
%       site.lon, site.lat: radial grid of the HF radar data. The first dimension is the radial 
%         dimension and the second is the azimuth.
%       site.grid.lon, site.grid.lat: Cartesian grid of the HF radar data. The first dimension is longitude,
%         and the second is the latitude (might be empty return if result is not required
%         on a Cartesian grid).
%   u,v: u- and v-velocity of the model currents on Arakawa-C grid. The order of the dimensions is
%     longitude, latitude and depth. 
%
% Output:
%   velg: radial velocity on a Cartesian grid (empty if site.grid.lon or 
%     site.grid.lat is empty).
%   velf: radial velocity on a radial grid.
%
% Note:
% The sizes of the variable on u- and v-grid are related by
% size(u,1) + 1 == size(v,1) and size(u,2) == size(v,2) + 1

% Author: Alexander Barth <a.barth@ulg.ac.be>, 2010

function [velg,velf] = hfradar_extractf(domain,site,u,v)

% vertical average
ua = hfradar_vaverage(u,domain.z_u,site.nu);
va = hfradar_vaverage(v,domain.z_v,site.nu);

x = site.lon;
y = site.lat;

% horizontal interpolation
ui = interpn(domain.lon_u,domain.lat_u,ua,x,y);
vi = interpn(domain.lon_v,domain.lat_v,va,x,y);

% rotation for radian currents
az = azimuth(y,x,site.lat0,site.lon0);

az = mod(90 - az,360);

az = az*pi/180;

vel = cos(az) .* ui + sin(az) .* vi;

vel = -vel;


% filtering in azimuthal direction
alpha = 0.4;
res = site.res/2;

velf = diffusion2(vel,[0 alpha],round(res^2/alpha));

velg = [];
if isfield(site.grid,'lon') && isfield(site.grid,'lat')
  if ~isempty(site.grid.lon) && ~isempty(site.grid.lat)    
    [gx,gy] = ndgrid(site.grid.lon,site.grid.lat);
  end
end

velg = griddata(x(:),y(:),velf(:),gx,gy);



% Copyright (C) 2012,2014 Alexander Barth <a.barth@ulg.ac.be>
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

%  LocalWords:  velg velf hfradar extractf lon
