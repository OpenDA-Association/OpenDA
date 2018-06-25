 file = '.\stochModel\input_dflowfm\DFM_OUTPUT_simplewaal_salt\simplewaal_salt_his.nc';
 time = ncread(file,'time');
 time_days = time/(24*60*60);
 waterlevel = ncread(file,'waterlevel');
 salinity = ncread(file,'salinity');

 figure(1)
 plot(time_days,salinity(1,:),'b')
 title('station01')
 xlabel('time (days)');
 ylabel('salinity');
 
 figure(2)
 plot(time_days,salinity(2,:))
 title('station02')
 xlabel('time (days)');
 ylabel('salinity');
 
 figure(3)
 plot(time_days,salinity(3,:))
 title('station03')
 xlabel('time (days)');
 ylabel('salinity');
 
 