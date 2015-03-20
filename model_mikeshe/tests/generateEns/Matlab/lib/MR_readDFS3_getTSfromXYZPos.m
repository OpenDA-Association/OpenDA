function [datenumVec , dataVec ] = MR_readDFS3_getTSfromXYZPos(x,y,z,variableName, filename)
%MR_readDFS3_getTSfromXYZPos Gets a time series from a dfs3 at given indices
% Gets a Time Series from a dfs3 file
% From 3D position idex (starting at 1)
%
% Inputs, 
%   X, Y, Z Position index in Grid (starting at 1)
%   NOTE: the Z position is from the "top"
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
   dfs3  = DfsFileFactory.Dfs3FileOpen(filename);
catch err
    rethrow(err)
    strcat( 'File ',' ' , filename )  
end  % end try/catch


% Find the variable Index based on the Variable Name
% Variable Index stats at zero
for item = 0 : dfs3.ItemInfo.Count-1
    itemname = char(dfs3.ItemInfo.Item(item).Name);
    if ( strcmp(itemname , variableName) == 1 )
        itemIdx = item;
        itemname
        break; 
    end
end
% Check to Make Sure Item was found. If not, throw exception.
if ~exist( 'itemIdx' , 'var')
     ME = MException('VerifyItem:ItemNotFoundinDFS3', ...
         strcat( 'Item: ', variableName, ' Not Found in File ', filename ));
     throw(ME)
end


% Check to Make Positions Indices are within range and are valid
if x < 1 || x > dfs3.SpatialAxis.XCount || ...
        y < 1 || y > dfs3.SpatialAxis.YCount || ...
            z < 1 || z > dfs3.SpatialAxis.ZCount 

     ME = MException('VerifyItemGridSize:XYZPositionNotInRange', ...
          ['Coordinate Indices provided: ','(',num2str( x ),',',num2str( y ),',',num2str( z ),')'...
          ,' are out of the valid range: ' ,'(',num2str( dfs3.SpatialAxis.XCount ),',',num2str( dfs3.SpatialAxis.YCount ),',',num2str( dfs3.SpatialAxis.ZCount ),')' ]  );
     throw(ME)

end

% Get number of z layers
zNum = size( double(dfs3.ReadItemTimeStep(itemIdx+1 , 0).To3DArray()) , 3 );

% Read first timestep
% Now for dfs3.ReadItemTimeStep, ItemIdx starts at 1 ! 
% !!! NOTE - it started at zero for the dfs3.ItemInfo !!!!!! 
% Time starts at Zero 
%data = rot90( double(dfs3.ReadItemTimeStep(itemIdx+1 , 0).To3DArray()) ); 
data = double(dfs3.ReadItemTimeStep(itemIdx+1 , 0).To3DArray()); 
data = rot90(data(:,:,zNum-z+1));

% Creates teh Date Time Vec by calling a special function
datenumVec = MR_createDateTimeVec(dfs3);

% delete values for Float seems to be the one that works for double.
deletevalue = dfs3.FileInfo.DeleteValueFloat;

% Empty Data Vector
dataVec = NaN(dfs3.FileInfo.TimeAxis.NumberOfTimeSteps,1);

%  Extracting time series
% This is SLOW! Is there no dedicated function for extracting a time series
% from a dfs3 file instead of getting ALL the values for all the time steps?
for i = 0 : 1 : dfs3.FileInfo.TimeAxis.NumberOfTimeSteps-1;
%   data = rot90( double( dfs3.ReadItemTimeStep(itemIdx+1 , i).To3DArray()) ) ;
data = double(dfs3.ReadItemTimeStep(itemIdx+1 , i).To3DArray()); 
data = rot90(data(:,:,zNum-z+1));
   
   if data(y,x,1) ~= deletevalue
       dataVec(i+1) = data(y,x,1);
   end
end


dfs3.Close();


end

