% Simulation of model:
% Make plot to show difference between observations and predicted values of EnKF run
%
[t, obs, pred_f pred_a]=load_enkf('Enkf_results');

gcf=1; figure(gcf);
plot(pred_f(1,1:length(t)));
hold on
plot(obs(1,1:length(t)),'k+');
title('Obs01');
legend('prediction','observation');
xlabel('time');

gcf = gcf + 1; figure(gcf);
plot(pred_f(2,1:length(t)));
hold on
plot(obs(2,1:length(t)),'k+');
title('Obs02');
legend('prediction','observation');
xlabel('time');

gcf = gcf + 1; figure(gcf);
plot(pred_f(3,1:length(t)));
hold on
plot(obs(3,1:length(t)),'k+');
title('Obs03');
legend('prediction','observation');
xlabel('time');

diff=pred_f-obs;
gcf = gcf + 1; figure(gcf);
plot(diff')
title('Difference between model and observations');
legend('Obs01','Obs02','Obs03');
ylabel('prediction-observation');
xlabel('time');

[nobs,ntimes]=size(diff);
for iobs=1:nobs
   rms=norm(diff(iobs,:))/sqrt(ntimes);
   disp(['RMS of location ',num2str(iobs),' ',num2str(rms)]);
end

