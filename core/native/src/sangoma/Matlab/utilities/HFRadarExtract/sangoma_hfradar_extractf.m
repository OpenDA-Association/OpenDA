% [velf,velg] = sangoma_hfradar_extractf( ...
%                  lon_u,lat_u,z_u,u,...
%                  lon_v,lat_v,z_v,v,...
%                  nu,res,lon0,lat0, ...
%                  lon,lat, ...
%                  long,latg)
%
% Extract the model equivalent of HF radar surface currents. The currents u and v are specified
% on an Arakawa-C grid.
%
% Input:
%     lon_u, lon_v: longitude of model grid at u/v points (degrees east).
%     lat_u, lat_v: latitude of model grid at u/v points (degrees north).
%     z_u, z_v: depth of model grid at u/v points (negative in water).
%     nu: the frequency of the HF radar system in Hz.
%     res: the effective azimuthal resolution in degrees.
%     lon0, lat0: longitude and latitude of the HF radar system.
%     lon, lat: radial grid of the HF radar data. The first dimension is the radial 
%         dimension and the second is the azimuth.
%     u,v: u- and v-velocity of the model currents on Arakawa-C grid. The order of the dimensions is
%        longitude, latitude and depth. 
%
% Optional input:
%     long, latg: Cartesian grid of the HF radar data. The first dimension is longitude,
%         and the second is the latitude.
%
% Output:
%   velf: radial velocity on a radial grid.
%
% Optional output:
%   velg: radial velocity on a Cartesian grid.
%
% Note:
% The sizes of the variable on u- and v-grid are related by
% size(u,1) + 1 == size(v,1) and size(u,2) == size(v,2) + 1

% Author: Alexander Barth <a.barth@ulg.ac.be>, 2014

function [velf,velg] = sangoma_hfradar_extractf( ...
                  lon_u,lat_u,z_u,u,...
                  lon_v,lat_v,z_v,v,...
                  nu,res,lon0,lat0, ...
                  lon,lat, ...
                  long,latg)

site.nu = nu;
site.res = res;
site.lon0 = lon0;
site.lat0 = lat0;
site.lon = lon;
site.lat = lat;

if nargin == 16
  site.grid.lon = long;
  site.grid.lat = latg;
end

domain = struct( ...
    'lon_u',lon_u ,...
    'lon_v',lon_v,...
    'lat_u',lat_u ,...
    'lat_v',lat_v,...
    'z_u',z_u ,...
    'z_v',z_v);

[velg,velf] = hfradar_extractf(domain,site,u,v);