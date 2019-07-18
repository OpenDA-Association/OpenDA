 fprintf('Attention: results only useful after running Simulation.oda \n')

 file = '.\stochModel\work0\DFM_OUTPUT_dcsmv5\dcsmv5_his.nc';
 time_work = ncread(file,'time');
 time_work = time_work/(24*60*60);
 wl_work = ncread(file,'waterlevel');
 stations = ncread(file,'station_name');
 cell_stations = cellstr(stations')
 
 selected_stations = {'WICK', 'NORTHSS', 'LOWST', 'SHEERNS', 'DOVR', 'VLISSGN', 'HOEKVHLD', 'DENHDR' };
 for j = 1:numel(selected_stations) 
     figure(j)
     selected_stations{j}
     st_index = find( strcmp(selected_stations{j}, cell_stations ))
     plot(time_work,wl_work(st_index,:))
     title(selected_stations{j})
     xlabel('time (days)');
     ylabel('waterlevel');
 end

 
 