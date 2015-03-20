function [datenumVec , dataVec ] = MR_readDFS0_varIndex(variableNumber, filename)
%MR_readDFS0_varIndex Gets time series from a dfs0
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com) 2013
%
%   Inputs: 
%       - the variable number to get from the dfs0. Starts at 1
%         and must be less than or equal to the number of variables in the
%         dfs file.
%       - dfs0 path and filename 
%   Outputs:
%       - DateNum time vector (tnew)
%       - A vector of data (1 x tnew)
% 
% Note: if the variable is not found within the dfs0, throw exception. 

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

% Find the variable Index based on the Variable Name
% Variable Index stats at zero
if variableNumber < 1 | variableNumber > dfs0.ItemInfo.Count
         ME = MException('VerifyItem:ItemNotFoundinDFS0', ...
         strcat( 'Item number: ', variableNumber, ' Not valid for File ', filename ));
     throw(ME)
end
itemIdx = int32(variableNumber);


% Read times and data for all items
% Use the Dfs0Util for bulk-reading all data and timesteps
dd = double(Dfs0Util.ReadDfs0DataDouble(dfs0));
t = dd(:,1);
dataVec = dd(:,itemIdx+2);


numSteps = dfs0.FileInfo.TimeAxis.NumberOfTimeSteps;
datenumVec = NaN(numSteps,1);

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

