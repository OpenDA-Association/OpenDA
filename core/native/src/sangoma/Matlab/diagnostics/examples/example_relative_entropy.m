% adapted from example_computeRE.F90
% REVISION HISTORY
% 2014-04 - Alexander Barth - Initial code

disp('Compute Relative Entropy');

% Size of ensemble
dim_ens = 5;

disp('prior weights (equal weights)');
prior_w = ones(1,dim_ens)/dim_ens

disp('posterior weights');
post_w = [0 0.25 .5 0.25 0]

disp('The relative entropy is:');
re = relative_entropy(post_w,prior_w)