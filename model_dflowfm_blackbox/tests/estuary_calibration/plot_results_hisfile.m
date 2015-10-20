 filename = '.\stochModel\input_dflowfm\DFM_OUTPUT_estuary\estuary_his.nc';
 time = ncread(filename,'time');
 time_days = time./(24*60*60);
 waterlevel = ncread(filename,'waterlevel');

 figure(1)
 plot(time_days,waterlevel(1,:))
 hold on
 title('station01')

 figure(2)
 plot(time_days,waterlevel(2,:))
 hold on
 title('station02')
 
 figure(3)
 plot(time_days,waterlevel(3,:))
 hold on
 title('station03')
 
 