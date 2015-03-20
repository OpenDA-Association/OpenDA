% make a plot of the mean ensemble at t=50
[t,ens]=load_ensemble('enkf_results');

% compute ensemble mean
mens=mean(ens,3);
det_mens=mens(1:80,:);
%plot at t=50
pcolor(reshape(det_mens(:,50),8,10))
%contour(reshape(det_mens(:,50),8,10))
colorbar
