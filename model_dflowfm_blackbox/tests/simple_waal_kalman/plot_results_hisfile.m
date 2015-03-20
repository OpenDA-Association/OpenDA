 file = '.\stochModel\input_dflowfm\DFM_OUTPUT_simple_waal\simple_waal_his.nc';
 time = ncread(file,'time');
 time_days = time/(24*60*60);
 waterlevel = ncread(file,'waterlevel');

 figure(1)
 plot(time_days,waterlevel(1,:),'b')
 title('station01')
 xlabel('time (days)');
 ylabel('waterlevel');
 
 figure(2)
 plot(time_days,waterlevel(2,:))
 title('station02')
 xlabel('time (days)');
 ylabel('waterlevel');
 
 figure(3)
 plot(time_days,waterlevel(3,:))
 title('station03')
 xlabel('time (days)');
 ylabel('waterlevel');
 
 