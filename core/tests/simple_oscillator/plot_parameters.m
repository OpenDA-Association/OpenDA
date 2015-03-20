%
% plot parameters versus iterations 
%
figure(2)
p = reshape([evaluatedParameters{:}],length([evaluatedParameters{2}]),length(evaluatedParameters))';
plot(p);  % total cost 
title('Modification of parameters per iteration');
xlabel('modelevaluation number');
ylabel('parameters');

