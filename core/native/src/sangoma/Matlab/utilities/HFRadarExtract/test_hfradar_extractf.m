% This test script only tests if all functions are present

load test_data_hfradar_extractf domain site u v

[velg,velf] = hfradar_extractf(domain,site,u,v);

[velf_sangoma,velg_sangoma] = sangoma_hfradar_extractf(...
                 domain.lon_u,domain.lat_u,domain.z_u,u,...
                 domain.lon_v,domain.lat_v,domain.z_v,v,...
                 site.nu,site.res,site.lon0,site.lat0, ...
                 site.lon,site.lat, ...
                 site.grid.lon,site.grid.lat);
                 
assert(max(max(abs(velg_sangoma - velg))) < 1e10)
assert(max(max(abs(velf_sangoma - velf))) < 1e10)

disp('OK')