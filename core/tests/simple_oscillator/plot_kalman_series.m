%
% simple script to plot the output of several kalman filtering algorithms
%

figi=1;

%load simulation results and obs
sequentialSimulation_results
t_obs = 0:1:10;
val_obs = [obs{:}];
n=length(x_f_central{1}); %number of state variables
t_sim = [model_time{:}];
val_sim = [x{:}];



if(1==1),
   %
   % ENKF synchronous
   %
   figure(figi);clf;figi=figi+1;
   clear x
   enkf_results; %load results
   t_model = [model_time{:}];
   val_model = [x{:}];
   plot(t_obs,val_obs,'k+');
   hold on;
   plot(t_sim,val_sim(1:n:end),'g-')
   plot(t_model,val_model(1:n:end),'b-')
   hold off
   title('Results of synchronous ENKF');
   xlabel('time');
   ylabel('position');
end;


if(1==1),
   %
   % ENKF asynchronous
   %
   figure(figi);clf;figi=figi+1;
   enkf_fixed_results; %load results
   t_model = [model_time{:}];
   val_model = [x{:}];
   plot(t_obs,val_obs,'k+');
   hold on;
   plot(t_sim,val_sim(1:n:end),'g-')
   plot(t_model,val_model(1:n:end),'b-')
   hold off
   title('Results of asynchronous ENKF');
   xlabel('time');
   ylabel('position');
end;



if(1==1),
   %
   % ENSR synchronous
   %
   figure(figi);clf;figi=figi+1;
   clear x
   ensr_results; %load results
   t_model = [model_time{:}];
   val_model = [x{:}];
   plot(t_obs,val_obs,'k+');
   hold on;
   plot(t_sim,val_sim(1:n:end),'g-')
   plot(t_model,val_model(1:n:end),'b-')
   hold off
   title('Results of synchronous ENSR');
   xlabel('time');
   ylabel('position');
end;


if(1==1),
   %
   % ENSR asynchronous
   %
   figure(figi);clf;figi=figi+1;
   clear x
   ensr_fixed_results; %load results
   t_model = [model_time{:}];
   val_model = [x{:}];
   plot(t_obs,val_obs,'k+');
   hold on;
   plot(t_sim,val_sim(1:n:end),'g-')
   plot(t_model,val_model(1:n:end),'b-')
   hold off
   title('Results of asynchronous ENSR');
   xlabel('time');
   ylabel('position');
end;


if(1==1),
   %
   % Particle filter synchronous
   %
   figure(figi);clf;figi=figi+1;
   clear x
   particle_filter_results; %load results
   t_model = [model_time{:}];
   val_model = [x{:}];
   plot(t_obs,val_obs,'k+');
   hold on;
   plot(t_sim,val_sim(1:n:end),'g-')
   plot(t_model,val_model(1:n:end),'b-')
   hold off
   title('Results of synchronous Particle filter');
   xlabel('time');
   ylabel('position');
end;


if(1==1),
   %
   % Particle filter asynchronous
   %
   figure(figi);clf;figi=figi+1;
   clear x
   particle_filter_fixed_results; %load results
   t_model = [model_time{:}];
   val_model = [x{:}];
   plot(t_obs,val_obs,'k+');
   hold on;
   plot(t_sim,val_sim(1:n:end),'g-')
   plot(t_model,val_model(1:n:end),'b-')
   hold off
   title('Results of asynchronous Particle filter');
   xlabel('time');
   ylabel('position');
end;

