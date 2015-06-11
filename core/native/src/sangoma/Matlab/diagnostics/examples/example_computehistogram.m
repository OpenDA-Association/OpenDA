function example_ComputeHistogram
% DESCRIPTION:
%--------------------------------------------
% example_ComputeHistogram
%
% This is an example showing how to use the routine
% histogram.m
% to compute a histogram of the ensemble distribution.
%
% INPUTS: None
% 
% OUTPUT: None
%
% USES:   computehistogram.m
%
%--------------------------------------------

% ************************************************
% *** DEFINE VARIABLES AND FIELDS              ***
% ************************************************

  % Number of state files to be read
  nfiles = 5;

  % State dimension
  nstate = 4;

  % Path to and name of file holding model trajectory
  inpath = 'inputs/';
  infile = 'fieldA_';

%% MAIN FUNCTION
  disp('*******************************************');
  disp('*        example_ComputeHistogram         *');
  disp('*                                         *');
  disp('*       Compute ensemble histogram        *');
  disp('*******************************************');

% ***********************************************
% *** Initialize                              ***
% ***********************************************

    % Read ensemble from input files
    for i=1:nfiles
        states(:,i) = load([inpath infile num2str(i) '.txt']);
    end

    % Define the central state as a vector of ones
    centralstate = ones(nfiles+1, 1);

    % Initialize the histogram array
    histogram = zeros(nfiles+1, 1);

    % Initialize number of calls to histogram routine
    ncall = 1; % Initial call

    % Call routine for histogram increment
    [histogram,delta]=computehistogram(ncall,4,5,0,centralstate,states,histogram)


    
