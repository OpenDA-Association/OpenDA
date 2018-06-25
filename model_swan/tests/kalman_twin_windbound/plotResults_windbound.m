%% This script is for plotting Hsig at each of the six observation locations. Three Hsig's values
%% are available for each location: observed values, output of SWAN with deterministic run, and
%% output of SWAN with EnKF. If everything is okay, the EnKF results should be closer to the observation.
%% Before using this script, make the EnKF_results.m available first, by 
%% running OpenDA with SWANEnKF.oda as input configuration file. 

enkf_windbound_results;
nObsTimes = length(obs);
firstObs = obs{1};
nStat = length(firstObs);

stations = {'anasuria', 'd151', 'europlatform', 'k13', 'north_cormorant'};

Obs   = [];
EnkfPredF = [];
for iObsTime = 1:nObsTimes
	Obs = [Obs;obs{iObsTime}];
	EnkfPredF = [EnkfPredF;pred_f_central{iObsTime}];
end

simulation_results;
SimPredF = [];
nObsTimes = length(obs);
for iObsTime = 1:nObsTimes
	SimPredF = [SimPredF;pred_f_central{iObsTime}];
end

iStat=1;
for iStat=1:nStat
	figure(iStat);clf;
	set(iStat,'renderer','zbuffer');
	plot(Obs(:,iStat),'k');
	hold on;
	plot(EnkfPredF(:,iStat),'b');
	plot(SimPredF(:,iStat),'g');
	hold off;
	title(['Hsig' stations{iStat}]);
	xlabel('Time [h]');
	ylabel('Hsig [m]');
	legend('Obs','With EnKF','Without EnKF');
	drawnow;
	print('-dpng',['Hsig_kalman_windbound_' stations{iStat} '.png']);
end
