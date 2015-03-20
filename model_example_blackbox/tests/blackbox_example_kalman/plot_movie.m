


sequentialSimulation_results
x_fg=x_a;
enkf_results;
x_enkf=x_a;

xobs = [10,20,40]; %this is not a very good idea. The observation locations may change.

for i=1:length(x_enkf),
	plot(x_fg{i},'b-');
	hold on;
	plot(x_enkf{i},'g-');
	plot(xobs,obs{i},'k+');
	hold off;
	axis([0 65 0 300]);
	xlabel('position x');
	ylabel('concentration c');
	pause(0.1);
end;
