% after results, the following variables are available:
% bg
%ens_mean

timesteps = size(bg,2);
names=fieldnames(bg);
%time = names(1)
%state_lor96=names(2)

%figure(1);clf;
for ts=1:timesteps,
  
  current_time(ts) = bg(ts).time;
   bgstate_x(ts) = bg(ts).oscill_model(1);
 %   bgstate = bg(ts).lorenz_model;

  ens_mst_x(ts)=ens_mean(ts).oscill_model(1);
 %   ens_mst=ens_mean(ts).lorenz_model;

    
end;

    figure(1),
    plot(current_time,bgstate_x,'r',current_time,ens_mst_x,'k');
    legend('background' ,'ensemble mean');
    xlabel('time(s)')
    ylabel('x(m)')
    title('oscillation model: position in time') 
  %  title(['time is ',num2str(current_time)]);


figure(2);

timesteps = size(observation,2);
for ts = 1:timesteps-1,
    current_time(ts) = observation(ts).time;
    obs(ts) = observation(ts).after;
    pred_after(ts) = prediction(ts).after;
%    res_after(ts) = residu(ts).after(1);
end; 


    plot(current_time,obs,"*",current_time,pred_after,'k');
    legend('observation' ,'prediction');
    xlabel('time(s)')
    ylabel('x(m)')
    title('oscill model: position in time') 
  %  title(['time is ',num2str(current_time)]);
