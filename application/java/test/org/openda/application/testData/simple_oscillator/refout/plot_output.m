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
   clear pred_f;
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
   clear pred_f;
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
   clear pred_f;
   figure(figi);clf;figi=figi+1;
   ensr_results; %load enkf results
   plot([obs{:}],'k+');
   hold on;
   plot([pred_f{:}],'b-');  
   plot([pred_a{:}],'g-');  
   hold off
   title('Results of ensr');
   xlabel('analysis step');
   ylabel('position');
end;


clear costs;

if(1==1),
   %
   %  DUD Calibration
   %
   clear cost;
   figure(figi);clf;figi=figi+1;
   dud_results; %load simulation results
   plot([cost{:}],'b-');  
   title('Results of dud calibration');
   xlabel('modelevaluation number');
   ylabel('cost');
end;

if(1==1),
   %
   %  DUD Calibration with weak-constraint
   %
   clear cost;
   figure(figi);clf;figi=figi+1;
   dud_constraint_results; %load simulation results
   plot([cost{:}],'b-');  
   title('Results of dud calibration with weak constraints');
   xlabel('modelevaluation number');
   ylabel('cost');
end;

if(1==1),
   %
   %  Simplex Calibration
   %
   clear cost;
   figure(figi);clf;figi=figi+1;
   simplex_results; %load simulation results
   plot([cost{:}],'b-');  
   title('Results of simplex calibration');
   xlabel('modelevaluation number');
   ylabel('cost');
end;

if(1==1),
   %
   %  Simplex Calibration with weak constraints
   %
   clear cost;
   figure(figi);clf;figi=figi+1;
   simplex_constraint_results; %load simulation results
   plot([cost{:}],'b-');  
   title('Results of simplex calibration with weak constraints');
   xlabel('modelevaluation number');
   ylabel('cost');
end;

if(1==1),
   %
   %  Powell Calibration
   %
   clear cost;
   figure(figi);clf;figi=figi+1;
   powell_results; %load simulation results
   plot([cost{:}],'b-');  
   title('Results of powell calibration');
   xlabel('modelevaluation number');
   ylabel('cost');
end;

if(1==1),
   %
   %  Powell Calibration with weak constraints
   %
   clear cost;
   figure(figi);clf;figi=figi+1;
   powell_constraint_results; %load simulation results
   plot([cost{:}],'b-');  
   title('Results of powell calibration with weak constraints');
   xlabel('modelevaluation number');
   ylabel('cost');
end;

