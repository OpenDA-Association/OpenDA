function [dateVec] = MR_DateVec_FromDFS0(filename)
%MR_DateVec_FromDFS0 Extracts the Date information from a dfs0 file even
%though the time steps are uneven (UnEqual Time Step Axis)
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com) 2013
%
%   Inputs: 
%       - dfs0 path and filename 
%   Outputs:
%       - DateNum time vector (tnew)

NET.addAssembly('DHI.Generic.MikeZero.DFS');
import DHI.Generic.MikeZero.DFS.*;
import DHI.Generic.MikeZero.DFS.dfs0.*;

try
   dfs0File  = DfsFileFactory.DfsGenericOpen(filename);
   % dfs0  = dfsTSO(filename)
catch err
    rethrow(err)
    strcat( 'File ',' ' , filename )  
end  % end try/catch

% Read times for all items
% Use the Dfs0Util for bulk-reading all data and timesteps
dd = double(Dfs0Util.ReadDfs0DataDouble(dfs0File));
t = dd(:,1);

% Convert the times read from seconds from start date to a date num vector
% dateVec = nan(dfs0File.FileInfo.TimeAxis.NumberOfTimeSteps,1);

startTime = datenum( ...
    double( dfs0File.FileInfo.TimeAxis.StartDateTime.Year ),...
    double( dfs0File.FileInfo.TimeAxis.StartDateTime.Month ),...
    double( dfs0File.FileInfo.TimeAxis.StartDateTime.Day ),...
    double( dfs0File.FileInfo.TimeAxis.StartDateTime.Hour ),...
    double( dfs0File.FileInfo.TimeAxis.StartDateTime.Minute ),...
    double( dfs0File.FileInfo.TimeAxis.StartDateTime.Second )...
    );

dateVec = startTime + t./(24*60*60);

dfs0File.Close();

end