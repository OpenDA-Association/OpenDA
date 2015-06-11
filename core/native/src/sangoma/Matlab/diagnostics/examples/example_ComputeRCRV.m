function example_ComputeRCRV
% DESCRIPTION:
%--------------------------------------------
% example_ComputeRCRV
%
% This is an example showing how to use the routine
% computeRCRV.m
% to compute the bias and the dispersion of the RCRV.
%
% INPUTS: None
%         The ensemble is read in from somple ASCII files.
% 
% OUTPUT: None
%
% USES:   computeRCRV.m
%
%--------------------------------------------
  
% ************************************************
% *** Configuration                            ***
% ************************************************

  % Number of files to be read = ensemble size
  nfiles = 5;
  
  % Number of realizations of the ensemble process
  nstate = 4;

  % Path to and name of file holding model trajectory
  inpath = 'inputs/';
  infile = 'fieldA_';

%% MAIN FUNCTION

% ***********************************************
% *** Initialize                              ***
% ***********************************************

  disp('*******************************************');
  disp('*           example_ComputeRCRV           *');
  disp('*                                         *');
  disp('*      Compute RCRV- bias/dispersion      *');
  disp('*******************************************');

  % Read ensemble from input files
  for i=1:nfiles
    xens(:,i) = load([inpath infile num2str(i) '.txt']);
  end

  % Observations and error (just some prescribed values)
  xobs = [ 0.78, 2.47, 3.21, 1.13 ];
  sig0 = 0.26;
  for k = 1:nstate
     missing(k) = 0.0;
  end

% ****************************************************
% *** Call routine to compute RCRV bias/dispersion ***
% ****************************************************

[bias,dispersion]=computeRCRV(xens,xobs,sig0,missing,nstate,nfiles)

