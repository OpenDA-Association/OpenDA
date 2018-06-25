function [datenumVec , dataVec ] = MR_readDFS0(variableName, filename)
%MR_readDFS0 Gets time series from a dfs0
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com) 2012
%
%   Inputs: 
%       - string of the variable to read 
%       - dfs0 path and filename 
%   Outputs:
%       - DateNum time vector (tnew)
%       - A matrix of data (n x tnew)
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
for item = 0 : dfs0.ItemInfo.Count-1
    itemname = char(dfs0.ItemInfo.Item(item).Name)
    if ( strcmp(itemname , variableName) == 1 )
        itemIdx = item;
        break; 
    end
end
% Check to Make Sure Item was found. If not, throw exception.
if ~exist( 'itemIdx' , 'var')
     ME = MException('VerifyItem:ItemNotFoundinDFS0', ...
         strcat( 'Item: ', variableName, ' Not Found in File ', filename ));
     throw(ME)
end


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

