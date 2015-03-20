function [datenumVec ] = MR_createDateTimeVec(dfs)
%MR_createDateTimeVec Creates a Date/Time Vector in the matlab datenum format. ASSUMES
%EVENLY SPACED TIME AXIS!
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com) 2012
%
%   Inputs: 
%       -  a dfs2   or a   dfs3   object
%   Outputs:
%       -  a date/time vector in matlab datenum format
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

NET.addAssembly('DHI.Generic.MikeZero.DFS');
import DHI.Generic.MikeZero.DFS.*;

numSteps = dfs.FileInfo.TimeAxis.NumberOfTimeSteps;
datenumVec = NaN(numSteps,1);

startTime = datenum( ...
    double( dfs.FileInfo.TimeAxis.StartDateTime.Year ),...
    double( dfs.FileInfo.TimeAxis.StartDateTime.Month ),...
    double( dfs.FileInfo.TimeAxis.StartDateTime.Day ),...
    double( dfs.FileInfo.TimeAxis.StartDateTime.Hour ),...
    double( dfs.FileInfo.TimeAxis.StartDateTime.Minute ),...
    double( dfs.FileInfo.TimeAxis.StartDateTime.Second )...
    );

% dt in seconds. Again, assume evenly spaced time axis.
dtSec = dfs.FileInfo.TimeAxis.TimeStep;

datenumVec(1) =  startTime;
% This is a slow approach, but avoids Round off errors. 
% If this function is run more often, look into a quicker approach.
for i = 2:numSteps
    datenumVec(i) = addtodate(datenumVec(i-1), double(dtSec), 'second');
end

end

