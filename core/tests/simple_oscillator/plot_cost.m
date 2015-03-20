%
% plot cost versus iterations 
%
figure
plot([costTotal{:}],'k-');  % total cost 
hold on;
plot([costObserved{:}],'g-');  % just for observations
if (exist('costWeakConstraintPenalty')),
   plot([costWeakConstraintPenalty{:}],'m-');  % penalty term
end;
hold off
title('Results of dud calibration');
xlabel('modelevaluation number');
ylabel('cost');

