function [ filename ] = DA_writeDFS0_setsMultipleTS(filename, data, timeVector)
%DA_writeDFS0_setsMultipleTS Sets multiple time series and Creates the dfs0 
%
% Inputs, 
%   filename
%   data size ( timeTS  ,  itemNumber )
%
% Outputs,
%   Filename of the output file
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com)
%           06/10/2012 
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

NET.addAssembly('DHI.Generic.MikeZero.DFS');
NETaddAssembly('DHI.Matlab.Toolbox.dll');

import DHI.Generic.MikeZero.DFS.*;
import DHI.Generic.MikeZero.DFS.dfs0.*;
import DHI.Generic.MikeZero.*
import DHI.Matlab.Toolbox.*

% OpenFile - For Edit
try
   dfs0  = DfsFileFactory.DfsGenericOpenEdit(filename);
catch err
    rethrow(err)
    strcat( 'File ',' ' , filename )  
end  % end try/catch

dd = double(Dfs0Util.ReadDfs0DataDouble(dfs0));
t = dd(:,1);

% Clears the data in the dfs0 file - clearing way for the new data.
dfs0.Reset();

% Slow version
numItems = size(data,2);

for i=1:length(t)
   for ni = 1: numItems
        dfs0.WriteItemTimeStepNext(t(i), NET.convertArray(single(data(i,ni)))); 
    
    end
end
dfs0.Close();


end

