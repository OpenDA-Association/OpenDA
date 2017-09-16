% Script that will produce the figures of exercise 1
% Note your environment must consists of a configured OpenDA distro 
% since we are starting OpenDA from this script.
%
% Author Nils van Velzen

if (~ exist('enkf_results_std1_ens10'))     system('oda_run.sh EnkF_std1_ens10.oda');      end
if (~ exist('enkf_results_std10_ens10'))   system('oda_run.sh EnkF_std10_ens10.oda');    end
if (~ exist('enkf_results_std5_ens5'))   system('oda_run.sh EnkF_std5_ens5.oda');    end
if (~ exist('enkf_results_std5_ens10'))   system('oda_run.sh EnkF_std5_ens10.oda');    end
if (~ exist('enkf_results_std5_ens100'))   system('oda_run.sh EnkF_std5_ens100.oda');    end


figure(1);
[t,ens]=load_ensemble('enkf_results_std1_ens10');
[tobs,obs]=load_obs ('enkf_results_std1_ens10');
ens1=reshape(ens(1,:,:),size(ens,2),size(ens,3));
plot(t,ens1)
title('Lorenz model first variable (EnKF)')

disp('push a button to see mean and obs')
pause
plot(t,mean(ens1,2));
hold on
plot(tobs,obs,'*r');
hold off
title('Lorenz model mean and observations of first variable (EnKF)')

disp('Push a button to see the stdev=10')
pause

clear

figure(1);
[t,ens]=load_ensemble('enkf_results_std10_ens10');
[tobs,obs]=load_obs ('enkf_results_std10_ens10');
ens1=reshape(ens(1,:,:),size(ens,2),size(ens,3));
plot(t,ens1)
title('Lorenz model first variable, obs stdvar=10 (EnKF)')
disp('push a button to see mean and obs')
pause
plot(t,mean(ens1,2));
hold on
plot(tobs,obs,'*r');
hold off
title('Lorenz model mean and observations of first variable (EnKF)')

disp('Push a button to see various ensemble sizes with stdev=5')
pause


figure(1);
[tobs,obs]=load_obs ('enkf_results_std5_ens5');
[t,ens5]=load_ensemble('enkf_results_std5_ens5');
[t,ens10]=load_ensemble('enkf_results_std5_ens10');
[t,ens100]=load_ensemble('enkf_results_std5_ens100');

ens5=reshape(ens5(1,:,:),size(ens5,2),size(ens5,3));
ens10=reshape(ens10(1,:,:),size(ens10,2),size(ens10,3));
ens100=reshape(ens100(1,:,:),size(ens100,2),size(ens100,3));
m5=mean(ens5,2);
m10=mean(ens10,2);
m100=mean(ens100,2);


plot(t,m5, t, m10, t, m100);
title('Lorenz EnKF various ensemble sizes')
legend('n=5','n=10','n=100');

disp('Push a button to zoomin')
pause
axis([20 30 -20 20])



disp('Push a button to see the obs')
pause
%axis([0 30 -20 20])
hold on
plot(tobs,obs,'*r');
hold off




