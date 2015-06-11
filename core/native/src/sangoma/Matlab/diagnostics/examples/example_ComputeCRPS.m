function example_computeCRPS

% DESCRIPTION:
%-----------------------------------------------------------
% This is an example showing how to use the routine
% sangoma_ComputeCRPS to compute the CRPS and its partition
%
% The ensemble is read in from simple ASCII files.
%
% USES: computeCRPS.m
%-----------------------------------------------------------

% ************************************************
% *** Configuration                            ***
% ************************************************

  % Number of files to be read = ensemble size
  nens = 5;
  
  % Number of realizations of the ensemble process
  m = 4;

  % Path to and name of file holding model trajectory
  inpath = 'inputs/';
  infile = 'fieldA_';

%% MAIN FUNCTION
  
% ************************************************
% *** Initialization                           ***
% ************************************************

  disp('******************************************');
  disp('*          example_ComputeCRPS           *');
  disp('*                                        *');
  disp('*      Compute CRPS & decomposition      *');
  disp('******************************************');


% ************************
% *** Read state files ***
% ************************

  % Read ensemble from input files
  for i=1:nens
    xens(:,i) = load([inpath infile num2str(i) '.txt']);
  end

  % Observations and error (just some prescribed values)
  xobs = [ 0.78, 2.47, 3.21, 1.13 ];
  sig0 = 0.26;
  missing = zeros(m,1);
  

% ***************************************************
% ***        Call function to compute CRPS        ***
% ***************************************************

[crps,reli,resol,unc,bb,aa]=computeCRPS(xens,xobs,missing,m,nens);

disp(['CRPS = Reli + Resol:  ' num2str(crps) ' = ' num2str(reli) ' + ' num2str(resol)]);
disp(['uncertainty: ', num2str(unc)]);
