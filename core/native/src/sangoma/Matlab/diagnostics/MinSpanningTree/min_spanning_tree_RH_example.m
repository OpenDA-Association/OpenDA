% Example setups to verify ensemble using minimum spanning
% tree rank histogram (MST RH). 
%
% A number of basic tests have been set up here to test 
% an ensemble in cases where :
% * obs are from the same distribution as ensemble;
% * obs are from different distribution than ensemble;
% * obs are from the same distribution as ensemble but are biased;
% * some variables have much smaller variance than others and 
%   ensemble scaling should be used. 
%
% Us exampleLog to choose the scenario you would like to test. 
%
%


N = 10; % number of ensembles
n = 8; % number of variables at a single spatial location
p = 100; % number of observation/forecast pairs
avg = 10; % for plotting observation/forecast pair data

sigma_ens = 1; % standard deviation for ensemble
sigma_obs = 5; % standard deviation for observations

ens_mean = 10*ones(n,1); % mean vector for ensemble

exampleLog = 3; % 1 - Random Uniform/Uniform distribution
		% 2 - Random Normal/Uniform distribution
		% 3 - Random Normal/Normal distribution
		% 4 - Random Normal/Normal distribution with bias
		% 5 - Random Uniform/Uniform distribution with bias

if (exampleLog == 1)
	%-----------------------------------------%
	% 1 - Random Uniform/Uniform distribution %
	%-----------------------------------------%
        rand('seed', 1);
	xp = ens_mean + rand(n,N,p);
	y = ens_mean + rand(n,p);
	mstRank = min_spanning_tree_RH(N,n,p,xp,y);
elseif (exampleLog == 2)
	%----------------------------------------%
	% 2 - Random Normal/Uniform distribution %
	%----------------------------------------%
        rand('seed', 1);
	xp = ens_mean+sigma_ens*rand(n,N,p);
        randn('seed', 1);
	y = ens_mean+ens_mean*0.1 + sigma_obs*randn(n,p);
	mstRank = min_spanning_tree_RH(N,n,p,xp,y);
elseif (exampleLog == 3)
	%----------------------------------------%
	% 3 - Random Normal/Normal distribution  %
	%----------------------------------------%
        randn('seed',1);% sum(100*clock));
	xp = ens_mean+sigma_ens*randn(n,N,p);
	y = ens_mean+sigma_obs*randn(n,p);
	mstRank = min_spanning_tree_RH(N,n,p,xp,y);
elseif (exampleLog == 4)
	%-------------------------------------------------%
	% 4 - Random Normal/Normal distribution with bias %
	%-------------------------------------------------%
        randn('seed', 1);
	xp = ens_mean+sigma_ens*randn(n,N,p);
	y = ens_mean+sigma_obs*randn(n,p)+0.2*ens_mean;
	mstRank = min_spanning_tree_RH(N,n,p,xp,y);
elseif (exampleLog == 5)
	%---------------------------------------------------%
	% 3 - Random Uniform/Uniform distribution with bias %
	%---------------------------------------------------%
        randn('seed', 1);
	xp = ens_mean+rand(n,N,p);
	y = ens_mean+rand(n,p)+0.1*ens_mean;
	mstRank = min_spanning_tree_RH(N,n,p,xp,y);
else 
	disp('No such example')
end


figure(1)
% plot the rank histogram
bar(mstRank/p);

figure(2)
% plot data
j = 1;
for i = 1:p/avg:p
subplot(avg/2,2,j)
hold on
	plot(squeeze(xp(:,:,i)),'.k');
	plot(y(:,i),'.r');
hold off
j = j +1;
end






