 fprintf('Attention: results only useful after running Simulation.oda \n')

 file = ['stochModel',filesep,'work0',filesep,'DFM_OUTPUT_estuary',filesep,'estuary_his.nc']
 time_work = ncread(file,'time');
 time_work = time_work/(24*60*60);
 wl_work = ncread(file,'waterlevel');
 figure(1)
 plot(time_work,wl_work(1,:),'b')
 title('station01')
 xlabel('time (days)');
 ylabel('waterlevel');
 
 figure(2)
 plot(time_work,wl_work(2,:))
 title('station02')
 xlabel('time (days)');
 ylabel('waterlevel');
 
 figure(3)
 plot(time_work,wl_work(3,:))
 title('station03')
 xlabel('time (days)');
 ylabel('waterlevel');
 
 
