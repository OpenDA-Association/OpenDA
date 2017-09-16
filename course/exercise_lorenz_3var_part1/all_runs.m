% Script that will produce the figures of exercise 1
% Note your environment must consists of a configured OpenDA distro 
% since we are starting OpenDA from this script.
%
% Author Nils van Velzen

if (~ exist('simulation_unperturbed_results')) 
   system('oda_run.sh simulation_unperturbed.oda');
end
if (~ exist('simulation_perturbed_results')) 
   system('oda_run.sh simulation_perturbed.oda');
end
if (~ exist('simulation_ensemble_results')) 
   system('oda_run.sh simulation_ensemble.oda');
end


[t,xyz,tobs,obs]=load_results('simulation_unperturbed_results');
figure(1)
plot3(xyz(1,:),xyz(2,:),xyz(3,:))
title('Lorenz model (simulation unperturbed)');
disp('Push a button')
pause

figure(1)
title('Lorenz model first variable (simulation unperturbed)');
plot(t,xyz(1,:),'b')
disp('Push a button')
pause

figure(1)
[t,xyz,tobs,obs]=load_results('simulation_unperturbed_results');
plot(t,xyz(1,:),'b');
hold on;
plot(tobs,obs,'r*');
title('Lorenz model first variable and observations (simulation unperturbed)')
hold off;
disp('Push a button')
pause

[t1,xyz1,tobs1,obs1]=load_results('simulation_unperturbed_results');
[t2,xyz2,tobs2,obs2]=load_results('simulation_perturbed_results');
figure(1)
plot3(xyz1(1,:),xyz1(2,:),xyz1(3,:),'b');
hold on
plot3(xyz2(1,:),xyz2(2,:),xyz2(3,:),'r');
hold off
legend('unperturbed','perturbed')
figure(2)
plot(t1,xyz1(1,:),'b')
hold on
plot(t2,xyz2(1,:),'r')
hold off
legend('unperturbed','perturbed')
disp('Push a button')
pause

figure(1);
[t,ens]=load_ensemble('simulation_ensemble_results');
ens1=reshape(ens(1,:,:),size(ens,2),size(ens,3));
plot(t,ens1)
title('Lorenz model first variable (ensemble)')

disp('Push a button')
pause
plot(t,mean(ens1,2))
title('Lorenz model mean of first variable (ensemble)')


