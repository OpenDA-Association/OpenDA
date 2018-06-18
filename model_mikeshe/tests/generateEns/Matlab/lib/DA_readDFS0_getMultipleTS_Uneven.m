function [datenumVec , dataVec ] = DA_readDFS0_getMultipleTS_Uneven(filename)
%DA_readDFS0_getMultipleTS_Uneven Gets multiple Time Series from a dfs0 file.
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com) 2012
%
%   Inputs: 
%       - dfs0 path and filename 

%   Outputs:
%       - DateNum time vector (tnew)
%       - A matrix of data (n x tnew)
%

NET.addAssembly('DHI.Generic.MikeZero.DFS');
import DHI.Generic.MikeZero.DFS.*;
import DHI.Generic.MikeZero.DFS.dfs0.*;

% OpenFIle
try
   dfs0  = DfsFileFactory.DfsGenericOpen(filename);
   % dfs0  = dfsTSO(filename)
catch err
    rethrow(err)
    strcat( 'File ',' ' , filename )  
end  % end try/catch


% Read times and data for all items
% Use the Dfs0Util for bulk-reading all data and timesteps
dd = double(Dfs0Util.ReadDfs0DataDouble(dfs0));
t = dd(:,1);
dataVec = dd(:,2:end);

%numSteps = dfs0.FileInfo.TimeAxis.NumberOfTimeSteps;

startTime = datenum( ...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Year ),...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Month ),...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Day ),...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Hour ),...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Minute ),...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Second )...
    );
% Date and Time Vector 
datenumVec = startTime + t./(24*60*60);

dfs0.Close();


end

