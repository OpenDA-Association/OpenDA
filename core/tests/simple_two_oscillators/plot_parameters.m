%
% plot parameters versus iterations 
%
figure
p = reshape([evaluatedParameters{:}],length([evaluatedParameters{2}]),length(evaluatedParameters)-1)';
plot(p);  % total cost 
title('Modification of paremeters per iteration');
xlabel('modelevaluation number');
ylabel('parameters');

