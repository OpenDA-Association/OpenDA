%generate surge for reference case and for truth-case that is used for generation of observations


dt=60; %1h in minutes
days_to_minutes=60*24;
tstop=4*days_to_minutes; %4days in minutes
h_true = 1.2;
h_init = 0.0;
t_true = 1.2*days_to_minutes;
t_init = 1.6*days_to_minutes;
width  = 0.3*days_to_minutes;

t=0:dt:tstop;

values_true = h_true*exp(-(0.5/width/width)*(t-t_true).^2);
values_init = h_init*exp(-(0.5/width/width)*(t-t_init).^2);

figure(1);clf
plot(t/3600,values_true,'k-');
hold on
plot(t/3600,values_init,'b-');
hold off


%write tim file
data_true = [t',values_true'];
save estuary_west_noise_true.tim -ascii data_true 
data_init = [t',values_init'];
save estuary_west_noise_init.tim -ascii data_init
