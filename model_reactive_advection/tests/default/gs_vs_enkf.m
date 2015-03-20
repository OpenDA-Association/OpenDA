% Sript to make some plots on a sinle timeseries for three different methods.
%
% author: Nils van Velzen
%
% NOTE first run the three models enkf.oda, enkf5.oda and enkf_gs.oda
% then run oda_matlabconv.pl on the three matlab result files (enkf_results.m, enkf5_results.m and enkf_gs_results.m)



cd enkf_gs_results
f_obs
f_analysis_time
f_pred_f_std
f_pred_f
cd ../

gs_obs=obs;
gs_time=analysis_time;
gs_pred_f_std=pred_f_std;
gs_pred_f=pred_f;

clear obs analysis_time pred_f_std pred_f

cd enkf_results
f_obs
f_analysis_time
f_pred_f_std
f_pred_f
cd ../

en_obs=obs;
en_time=analysis_time;
en_pred_f_std=pred_f_std;
en_pred_f=pred_f;

clear obs analysis_time pred_f_std pred_f

cd enkf5_results
f_obs
f_analysis_time
f_pred_f_std
f_pred_f
cd ../

en5_obs=obs;
en5_time=analysis_time;
en5_pred_f_std=pred_f_std;
en5_pred_f=pred_f;

clear obs analysis_time pred_f_std pred_f

subplot(3,1,1)
plot(gs_time,gs_pred_f(:,1),'b')
hold on; plot(gs_time,gs_obs(:,1),'r*')
title('EnKF with gain regularisation nEns=5')
legend('predictions', 'observations');
hold off;

subplot(3,1,2)

plot(en_time,en_pred_f(:,1),'b')
hold on; plot(en_time,en_obs(:,1),'r*')
title('EnKF nEns=30')
legend('predictions', 'observations');
hold off;

subplot(3,1,3)
plot(en5_time,en5_pred_f(:,1),'b')
hold on; plot(en5_time,en5_obs(:,1),'r*')
title('EnKF nEns=5')
legend('predictions', 'observations');
hold off;
