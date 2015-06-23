% Sequential simulation:
% Make plot to show difference between observations and predicted values
%
[t, obs, pred_f pred_a]=load_sequential('SequentialSimulation_results');


for j = 1:4,
    gcf=j; figure(gcf);
    plot(pred_f(j,1:length(t)),'DisplayName', 'Simulation');
    hold on
    plot(obs(j,1:length(t)),'k', 'DisplayName', 'Observation');
    title(strcat('S',num2str(j)));
    %legend('prediction','observation');
    xlabel('time');
end


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

