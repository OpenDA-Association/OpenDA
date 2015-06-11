% This is a example script for sangoma_hfradar_extractf


% load the data from file test_data_hfradar_extractf which containts:
%
% domain: structure describing the model domain with the following fields:
%     domain.lon_u, domain.lon_v: longitude of model grid at u/v points
%        (degrees east).
%     domain.lat_u, domain.lat_v: latitude of model grid at u/v points
%        (degrees north).
%     domain.z_u, domain.z_v: depth of model grid at u/v points (negative
%        in water).
%
% site: structure describing the HF radar site with the following fields:
%     site.nu: the frequency of the HF radar system in Hz.
%     site.res: the effective azimuthal resolution in degrees.
%     site.lon0, site.lat0: longitude and latitude of the HF radar system.
%     site.lon, site.lat: radial grid of the HF radar data. The first dimension
%       is the radial dimension and the second is the azimuth.
%     site.grid.lon, site.grid.lat: Cartesian grid of the HF radar data. The
%       first dimension is longitude, and the second is the latitude
%
% u,v: u- and v-velocity of the model currents on Arakawa-C grid. The order of
%   the dimensions is longitude, latitude and depth.
%

disp('Example for sangoma_hfradar_extractf');

disp('Load sample data');
load test_data_hfradar_extractf domain site u v

disp('Apply observation operator');

% call observation operator
[velf_sangoma,velg_sangoma] = sangoma_hfradar_extractf(...
                 domain.lon_u,domain.lat_u,domain.z_u,u,...
                 domain.lon_v,domain.lat_v,domain.z_v,v,...
                 site.nu,site.res,site.lon0,site.lat0, ...
                 site.lon,site.lat, ...
                 site.grid.lon,site.grid.lat);

disp('Radial currents extracted');
