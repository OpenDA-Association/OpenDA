function example_ComputeSensitivity


% DESCRIPTION:
%--------------------------------------------
% function example_Sensitivity
%
% Based on P. Kirchgessner's example for Fortran
% code for Sensitivity. 
%
% This is an example showing how to use the routine
% sensitivity.m
%
% INPUTS: Read an ensemble from input files.
% 
% OUTPUT: None
%
% USES:   sensitivity.m
%
%--------------------------------------------

% ************************************************
% *** DEFINE VARIABLES AND FIELDS              ***
% ************************************************
  dim_ens = 5; % Number of state files to be read
  dim_state = 4; % State dimension
  dim_obs = 2; % Observation dimension
  dim_sample = 4; % Sample dimension
  
% ALLOCATE FIELDS
  obs = zeros(dim_obs,1);
  weights = zeros(dim_ens,1);
  R = zeros(dim_obs,dim_obs);


% MAIN FUNCTION
 
% ************************************************
% *** Iinitialise                              ***
% ************************************************

  %Initialize H
  %       | 0 1 0 0 |
  %  H =  | 0 0 0 1 |
  %
  H = zeros(dim_obs,dim_state);
  H(1,2) =1; 
  H(2,4) =1;


  %Initialize R
  %       | 0.5 0.25 |
  %  R =  | 0.25 0.5 |
  % 
  R(1,1) = 0.5;
  R(1,2) = 0.25;
  R(2,2) = 0.5;
  R(2,1) = 0.25;
 

% GENERATE RANDOM STATES
% **************************************************
 % true state
 state_true = [10; 12; 9; 15]; 

 % Generate particles
 randn('state',cputime*1000); % random seed
 for i = 1:dim_ens
%states(:,i) = state_true + sqrt(0.5)*randn(dim_state, 1);
    states(:,i) = load(['examples/inputs/fieldA_' num2str(i) '.txt']);
 end
 
 % Generate observations
% randn('state',cputime*1000); % random seed
% obs = H*state_true+ sqrt(0.5)*randn(dim_obs,1);

 % Set all the weights equal
 weights = ones(dim_ens,1)*1./real(dim_ens)



  disp('*******************************************');
  disp('*        example_Sensitivity              *');
  disp('*                                         *');
  disp('*       Compute ensemble sensitivity      *');
  disp('*******************************************');


% ********************************************
% *** Call routine to perform Sensetivity  ***
% ********************************************

 disp( '------- Compute sensitivity -------------');

 [S,PA]=sensitivity(R,H,weights,states);


 disp('------- Results: --------------');
 disp('-------H as matrix ------------');
 disp('-------------------------------');

 disp('Sensitivity matrix: ');
  for i = 1:dim_obs
          disp(i)
	  disp(S(:,i));
  end
  
 disp('--------------------------------');
 
  disp('Posterior Covariance matrix:');
  for i = 1:dim_state
	 disp(i)
  	 disp(PA(:,i));
  end


% ****************
% *** Clean up ***
% ****************

 close all;
 clear all;


 disp('------- END -------------')



