% Ensemble Kalman filter:
% Make plot to show difference between observations and predicted values
%
% author: Nils van Velzen
%
[t, ens, obs, pred_f, pred_a]=load_ensemble('enkf_results');

diff=pred_a-obs;
figure(1);
plot(diff')
title('Difference between model and observations');
ylabel('prediction-observation');
xlabel('time');

[nobs,ntimes]=size(diff)
for iobs=1:nobs
   rms=norm(diff(iobs,:))/sqrt(ntimes);
   disp(['RMS of location ',num2str(iobs),' ',num2str(rms)]);
end

