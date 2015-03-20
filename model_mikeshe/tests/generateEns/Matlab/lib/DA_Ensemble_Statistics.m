function [EnsAvg, EnsStd, EnsMin, EnsMax, CI_Lower, CI_Uppler ] = DA_Ensemble_Statistics(EnsData)
%DA_Ensemble_Statistics Reads of Matrix of Data and calculates some statistics.
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com) 2012
%
%   Inputs: 
%       - The Data Cells. Each cell is a matrix of dimensions ( nt, nvar )
%   Outputs:
%       - Statistics (the average, st. devation and the min/max at every
%       time step.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
numEns = size(EnsData,2);
numVars = size(EnsData{1},2);
numTime = size( EnsData{1},1 );

% Run Statistics on the Enembles
DataCube = zeros(numEns, numTime, numVars);
for i=1:numEns
    DataCube(i,1:end,1:end) =  EnsData{i};
end

EnsAvg = reshape( mean(DataCube,1) , numTime,numVars);
EnsMin = reshape( min(DataCube,[],1) , numTime,numVars);
EnsMax = reshape( max(DataCube,[],1) , numTime,numVars);
EnsStd = reshape( std(DataCube,0,1) , numTime,numVars);

%QUpper = squeeze( quantile(DataCube,0.95) );
%QLower = squeeze( quantile(DataCube,0.05) );

[CI_Lower, CI_Uppler] = MR_ConfidanceInterval(DataCube,0.95);


disp( ['Stats on Ensembles Done ']);

end