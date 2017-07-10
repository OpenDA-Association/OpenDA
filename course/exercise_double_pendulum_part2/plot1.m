[tt,truth,tobs1,obs1]=load_results('simulation_truth_results');
[ti,initial,tobs2,obs2]=load_results('simulation_initial_results');
[te,enkf,tobs2,obs2]=load_results('simulation_enkf_results');
figure(1);clf;subplot(2,1,1);
plot(tt,truth(1,:),'k');
hold on;
plot(ti,initial(1,:),'g');
plot(te,enkf(1,1:2:end),'b');
hold off;
legend('truth','initial','enkf')
subplot(2,1,2);
plot(tt,truth(2,:),'k');
hold on;
plot(ti,initial(2,:),'g');
plot(te,enkf(2,1:2:end),'b');
hold off;
