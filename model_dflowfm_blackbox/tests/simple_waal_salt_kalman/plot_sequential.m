% Sequential simulation:
% Make plot to show difference between observations and predicted values
%
[t, obs, pred_f pred_a]=load_sequential('SequentialSimulation_results');

gcf=1; figure(gcf);
plot(pred_f(1,1:length(t)));
hold on
plot(obs(1,1:length(t)),'k');
title('Obs01');
legend('prediction','observation');
xlabel('time');

gcf = gcf + 1; figure(gcf);
plot(pred_f(2,1:length(t)));
hold on
plot(obs(2,1:length(t)),'k');
title('Obs02');
legend('prediction','observation');
xlabel('time');

gcf = gcf + 1; figure(gcf);
plot(pred_f(3,1:length(t)));
hold on
plot(obs(3,1:length(t)),'k');
title('Obs03');
legend('prediction','observation');
xlabel('time');

diff=pred_f-obs;
gcf = gcf + 1; figure(gcf);
plot(diff')
title('Difference between model and observations');
ylabel('prediction-observation');
xlabel('time');

[nobs,ntimes]=size(diff)
for iobs=1:nobs
   rms=norm(diff(iobs,:))/sqrt(ntimes);
   disp(['RMS of location ',num2str(iobs),' ',num2str(rms)]);
end

