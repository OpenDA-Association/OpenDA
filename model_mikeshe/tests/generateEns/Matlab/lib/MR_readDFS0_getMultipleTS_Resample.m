function [datenumVec , dataVec, itemname ] = MR_readDFS0_getMultipleTS_Resample(filename, dtSec)
%MR_readDFS0_getMultipleTS_Resample  Gets multiple Time Series from a dfs0 file and resamples time
%serires by given dt in seconds.
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com) 2012
%
%   Inputs: 
%       - dfs0 path and filename 
%       - desired dt output in seconds 
%   Outputs:
%       - DateNum time vector (tnew)
%       - A matrix of data (n x tnew)
%       - Cells of item names read.
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
% Remove Delete values
dd(dd==dfs0.FileInfo.DeleteValueFloat)=nan;
t = dd(:,1);
dataUneven = dd(:,2:end);

% Find the variable Index based on the Variable Name
% Variable Index stats at zero
for item = 0 : dfs0.ItemInfo.Count-1
    itemname{item+1} = char(dfs0.ItemInfo.Item(item).Name);
end

startTime = datenum( ...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Year ),...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Month ),...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Day ),...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Hour ),...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Minute ),...
    double( dfs0.FileInfo.TimeAxis.StartDateTime.Second )...
    );
% Date and Time Vector 
timeUneven = startTime + t./(24*60*60);

% MR_EvenTimeSteps(tVec, DataMatrix, dtSec)
[datenumVec , dataVec ] = MR_EvenTimeSteps(timeUneven, dataUneven, dtSec);

dfs0.Close();


end

