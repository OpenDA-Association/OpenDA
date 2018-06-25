 fprintf('Attention: results only useful after running Simulation.oda \n')

 file = '.\stochModel\work0\DFM_OUTPUT_lake2d\lake2d_his.nc';
 time_work = ncread(file,'time');
 time_work = time_work/(24*60*60);
 wl_work = ncread(file,'waterlevel');
 figure(1)
 plot(time_work,wl_work(1,:))
 title('S4')
 xlabel('time (days)');
 ylabel('waterlevel');
 
 figure(2)
 plot(time_work,wl_work(2,:))
 title('S2')
 xlabel('time (days)');
 ylabel('waterlevel');
 
 figure(3)
 plot(time_work,wl_work(3,:))
 title('S3')
 xlabel('time (days)');
 ylabel('waterlevel');
 
 figure(4)
 plot(time_work,wl_work(4,:))
 title('S4')
 xlabel('time (days)');
 ylabel('waterlevel'); 
