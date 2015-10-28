% Simulation of model:
% Make plot to show difference between observations and predicted values of EnKF run
%
[t, obs, pred_f, pred_a, pred_f_std]=load_enkf('Enkf_results');

nobs=size(obs,1);

for j = 1:nobs,
    gcf=j; figure(gcf);
    std = [ pred_f(j,1:length(t)) - 2* pred_f_std(j,1:length(t)) ; 4* pred_f_std(j,1:length(t)) ];
    h = area(std','LineStyle','none');
    set(h(1),'FaceColor' , [1 1 1]);
    set(h(2),'FaceColor', [ 135 206 250 ]/255);
    hold on
    h1 = plot(pred_f(j,1:length(t)));
    h0 = plot(obs(j,1:length(t)),'k+');
    title(strcat('S',num2str(j)));
    legend([h0, h1, h(2)], {'observation','prediction', '95% interval'});
    xlabel('time');
    set(gca,'Layer','top')
    grid on
end

diff=pred_f-obs;
gcf = gcf + 1; figure(gcf);
plot(diff')
title('Difference between model and observations');
legend('S1','S2','S3');
ylabel('prediction-observation');
xlabel('time');

[nobs,ntimes]=size(diff);
for iobs=1:nobs
   rms=norm(diff(iobs,:))/sqrt(ntimes);
   disp(['RMS of location ',num2str(iobs),' ',num2str(rms)]);
end

