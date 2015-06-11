% include to all sangoma Matlab subdirectories in search path 


function sangoma_addpath()

sangoma_matlab_dir = fileparts(mfilename('fullpath'));

% list obtained from 
% find . -name "*.m" -exec dirname {} \; | sort | uniq

addpath(fullfile(sangoma_matlab_dir,'utilities','examples'))
addpath(fullfile(sangoma_matlab_dir,'utilities','HFRadarExtract'))
addpath(fullfile(sangoma_matlab_dir,'diagnostics'))
addpath(fullfile(sangoma_matlab_dir,'diagnostics','examples'))
addpath(fullfile(sangoma_matlab_dir,'diagnostics'))
addpath(fullfile(sangoma_matlab_dir,'perturbations','examples'))
addpath(fullfile(sangoma_matlab_dir,'perturbations','WCE'))
addpath(fullfile(sangoma_matlab_dir,'transformations','examples'))
addpath(fullfile(sangoma_matlab_dir,'transformations','Anam'))
