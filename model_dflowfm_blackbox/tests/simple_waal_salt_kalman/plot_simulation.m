 fprintf('Attention: results only useful after running Simulation.oda \n')

 file = '.\stochModel\work0\DFM_OUTPUT_simplewaal_salt\simplewaal_salt_his.nc';
 time_work = ncread(file,'time');
 time_work = time_work/(24*60*60);
 sal_work = ncread(file,'salinity');
 
 figure(1)
 plot(time_work,sal_work(1,:),'b')
 title('station01')
 xlabel('time (days)');
 ylabel('salinity');
 
 figure(2)
 plot(time_work,sal_work(2,:))
 title('station02')
 xlabel('time (days)');
 ylabel('salinity');
 
 figure(3)
 plot(time_work,sal_work(3,:))
 title('station03')
 xlabel('time (days)');
 ylabel('salinity');
 
 
