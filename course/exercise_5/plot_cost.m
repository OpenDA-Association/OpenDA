
% load results
dud_results;

% create plot of cost and parameter
figure(1);clf
subplot(2,1,1);
plot([costs{:}]);
xlabel('model run');
ylabel('cost function');
subplot(2,1,2);
plot([evaluatedParameters{:}]);
xlabel('model run');
ylabel('change of reaction\_time [seconds]');

