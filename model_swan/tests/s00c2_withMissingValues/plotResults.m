%% This script is for plotting Hsig at each of the six observation locations. Three Hsig's values
%% are available for each location: observed values, output of SWAN with deterministic run, and
%% output of SWAN with EnKF. If everything is okay, the EnKF results should be closer to the observation.
%% Before using this script, make the EnKF_results.m available first, by 
%% running OpenDA with SWANEnKF.oda as input configuration file. 

EnKF_results;
nObsTimes = length(obs);
firstObs = obs{1};
nStat = length(firstObs);

thisObs   = [];
thisPredA = [];
for iObsTime = 1:nObsTimes
	thisObs = [thisObs;obs{iObsTime}];
	thisPredA = [thisPredA;pred_a{iObsTime}];
end

detRun=load('stochModel/deterministicRun/timeseries.out');

iStat=1;
for iStat=1:nStat
	plot(thisObs(:,iStat),'b');
	hold on;
	plot(thisPredA(:,iStat),'g');
	plot(detRun(iStat+nStat:nStat:size(detRun,1),6),'r');
	hold off;
	title(['Hsig' num2str(iStat)]);
	xlabel('Time [h]');
	ylabel('Hsig [m]');
	legend('Obs','With EnKF','Without EnKF','location','east');
	print('-djpeg',['Hsig' num2str(iStat) '.jpg']);
end