
%load data
dud_results;

ntimes=250;

figure(1);clf
istart=1;istop=ntimes;
t=0:60:((ntimes-1)*60);
y_obs=observed{1}(istart:istop);
y_firstguess=predicted{1}(istart:istop);
last=length(predicted);
y_final=predicted{last}(istart:istop);

plot(t,y_obs,'k-');
hold on
plot(t,y_firstguess,'g-');
plot(t,y_final,'b-');
hold off
legend('observed','before calibration','after calibration');
title('Concentration 2 in location C');
xlabel('time [seconds]');


figure(2);clf
istart=ntimes+1;istop=2*ntimes;
t=0:60:((ntimes-1)*60);
y_obs=observed{1}(istart:istop);
y_firstguess=predicted{1}(istart:istop);
last=length(predicted);
y_final=predicted{last}(istart:istop);

plot(t,y_obs,'k-');
hold on
plot(t,y_firstguess,'g-');
plot(t,y_final,'b-');
hold off
legend('observed','before calibration','after calibration');
title('Concentration 1 in location C');
xlabel('time [seconds]');
