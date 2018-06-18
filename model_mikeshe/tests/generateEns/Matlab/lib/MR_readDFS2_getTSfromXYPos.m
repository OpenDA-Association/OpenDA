function [datenumVec , dataVec ] = MR_readDFS2_getTSfromXYPos(variableName, filename,x,y)
%MR_readDFS2_getTSfromXYPos Gets a time series from a dfs2 at given indices
% Gets a Time Series from a dfs2 file
% From 2D position idex (starting at 1)
%
% Inputs, 
%   X, Y Position index in Grid (starting at 1)
%   variable name  ( string )
%   filename
%
% Outputs,
%   data array containing the data (in double)
%   a date/time vector in matlab datenum format
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com)
%           10/04/2012 
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

NET.addAssembly('DHI.Generic.MikeZero.DFS');
import DHI.Generic.MikeZero.DFS.*;

% OpenFIle
try
   dfs2  = DfsFileFactory.Dfs2FileOpen(filename);
catch err
    rethrow(err)
    strcat( 'File ',' ' , filename )  
end  % end try/catch


% Find the variable Index based on the Variable Name
% Variable Index stats at zero
for item = 0 : dfs2.ItemInfo.Count-1
    itemname = char(dfs2.ItemInfo.Item(item).Name)
    if ( strcmp(itemname , variableName) == 1 )
        itemIdx = item;
        break; 
    end
end
% Check to Make Sure Item was found. If not, throw exception.
if ~exist( 'itemIdx' , 'var')
     ME = MException('VerifyItem:ItemNotFoundindfs2', ...
         strcat( 'Item: ', variableName, ' Not Found in File ', filename ));
     throw(ME)
end


% Check to Make Positions Indices are within range and are valid
if x < 1 || x > dfs2.SpatialAxis.XCount || ...
            y < 1 || y > dfs2.SpatialAxis.YCount 

     ME = MException('VerifyItemGridSize:XYZPositionNotInRange', ...
          ['Coordinate Indices provided: ','(',num2str( x ),')'...
          ,' are out of the valid range: ' ,'(',num2str( dfs2.SpatialAxis.XCount ),',',num2str( dfs2.SpatialAxis.YCount ),')' ]  );
     throw(ME)

end


% Read first timestep
% Now for dfs2.ReadItemTimeStep, ItemIdx starts at 1 ! 
% !!! NOTE - it started at zero for the dfs2.ItemInfo !!!!!! 
% Time starts at Zero 
data = double(dfs2.ReadItemTimeStep(itemIdx+1 , 0).To2DArray()); 

% Creates teh Date Time Vec by calling a special function
datenumVec = MR_createDateTimeVec(dfs2);

% delete values for Float seems to be the one that works for double.
deletevalue = dfs2.FileInfo.DeleteValueFloat;

% Empty Data Vector
dataVec = NaN(dfs2.FileInfo.TimeAxis.NumberOfTimeSteps,1);

%  TODO: Check is there is no dedicated function for extracting a time
%  series
% This is SLOW! Is there no dedicated function for extracting a time series
% from a dfs2 file instead of getting ALL the values for all the time steps
for i = 0 : 1 : dfs2.FileInfo.TimeAxis.NumberOfTimeSteps-1;
   data = double( dfs2.ReadItemTimeStep(itemIdx+1 , i) .To2DArray()) ;
   
   if data(x,y) ~= deletevalue
       dataVec(i+1) = data(x,y);
   end
end


dfs2.Close();


end

