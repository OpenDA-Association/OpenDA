%
% simple script to plot the output of several algorithms
%

figi=1;

if(1==1),
   %
   %  SIMULATION
   %
   figure(figi);clf;figi=figi+1;
   simulation_results; %load simulation results
   plot(obs{1},'k+');
   hold on;
   for i=1:length(pred),
      plot(pred{i},'b-');  
   end;
   hold off
   title('Results of simulation');
   xlabel('analysis step');
   ylabel('position');
end;


if(1==1),
   %
   %  ENSEMBLE KALMAN FILTER
   %
   figure(figi);clf;figi=figi+1;
   enkf_results; %load enkf results
   plot([obs{:}],'k+');
   hold on;
   plot([pred_f{:}],'b-');  
   plot([pred_a{:}],'g-');  
   hold off
   title('Results of enkf');
   xlabel('analysis step');
   ylabel('position');
end;


if(1==1),
   %
   %  PARTICLE FILTER
   %
   figure(figi);clf;figi=figi+1;
   particle_filter_results; %load particle filter results
   plot([obs{:}],'k+');
   hold on;
   plot([pred_f{:}],'b-');  
   plot([pred_a{:}],'g-');  
   hold off
   title('Results of residual resampling particle filter');
   xlabel('analysis step');
   ylabel('position');
end;

if(1==1),
   %
   %  ENSEMBLE SQUARE-ROOT FILTER
   %
   figure(figi);clf;figi=figi+1;
   ensr_results; %load ensr filter results
   plot([obs{:}],'k+');
   hold on;
   plot([pred_f{:}],'b-');  
   plot([pred_a{:}],'g-');  
   hold off
   title('Results of ensemblesquare-root filter');
   xlabel('analysis step');
   ylabel('position');
end;
