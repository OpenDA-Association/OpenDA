function example_computeBRIER

% DESCRIPTION:
%---------------------------------------------------
% example_ComputeBRIER
%
% This is an example showing how to use the routine
% sangoma_ComputeBRIER to compute the Brier score
%
% The ensemble is read in from simple ASCII files.
%
% REVISION HISTORY:
% 2015-06 - L. Nerger - Initial code based on Fortran version by G. Candille
%
% USES:   CcomputeBRIER.m
%---------------------------------------------------

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

% ************************************************
% *** Init                                     ***
% ************************************************

  disp('*******************************************');
  disp('*        example_ComputeBRIER             *');
  disp('*                                         *');
  disp('*   Compute Brier score & decomposition   *');
  disp('*******************************************');


% ************************
% *** Read state files ***
% ************************

  for i=1:nens
      xens(:,i) = load([inpath infile num2str(i) '.txt']);
  end

  % Observations
  xobs = [ 0.78, 2.47, 3.21, 1.13 ];
  
  % Same thresholds
  xth = ones(1,m);


% ****************************************************
% ***     Call routine to compute Brier score      ***
% ****************************************************
 
  [br,brc,brv,unc,pc,s,sunc,pp,g,pr]=computeBRIER(m,nens,xens,xobs,xth);

  disp(['Brier skill score:         ', num2str(br)])
  disp(['reliability & resolution:  ', num2str(brc) '    ' num2str(brv)])
  disp(['climatology & uncertainty: ', num2str(pc) '    ' num2str(unc)])
  disp(['entropy & uncertainty:     ', num2str(s) '    ' num2str(sunc)])
  disp(['pp  '                   , num2str(pp)])
  disp(['g   '                   , num2str(g)])
  disp(['pr  '                   , num2str(pr)])
% (pp,g) enables to draw the sharpness diagram
% (pp,pr) enables to draw the reliability diagram 

