function [datenumVec , dataVec ] = MR_readDFS3_fromXYZRange(x0,nx,y0,ny,z0,nz,variableName, filename)
%MR_readDFS3_fromXYZRange Gets a time series data cube from a dfs3 from the
%given range of indices. Returns DATA(y,x,z,time)
% Gets a Time Series from a dfs3 file
% From 3D position index (starting at 1)
%
% Inputs, 
%   X, Y, Z Position index in Grid (starting at 1) --> x0, y0, z0
%   nx, ny, nz are the number of x,y,z nodes to extract data.
%   NOTE: the Z position is from the "top"
%   variable name  ( string )
%   filename
%
% Outputs, DATA(y,x,z,time)
%   A 4D data cube 
%   a date/time vector in matlab datenum format
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com)
%           20/08/2013 
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
if x0 < 1 || x0 + nx - 1 > dfs3.SpatialAxis.XCount || ...
        y0 < 1 || y0 + ny - 1 > dfs3.SpatialAxis.YCount || ...
            z0 < 1 || z0 + nz - 1 > dfs3.SpatialAxis.ZCount 

     ME = MException('VerifyItemGridSize:XYZPositionNotInRange', ...
          ['Coordinate Indices provided: ','(',num2str( x0 ),',',num2str( y0 ),',',num2str( z0 ),')'...
          ,' are out of the valid range: ' ,'(',num2str( dfs3.SpatialAxis.XCount ),',',num2str( dfs3.SpatialAxis.YCount ),',',num2str( dfs3.SpatialAxis.ZCount ),')' ]  );
     throw(ME)

end

% Get number of z layers
zNum = size( double(dfs3.ReadItemTimeStep(itemIdx+1 , 0).To3DArray()) , 3 );

% Read first timestep
% Now for dfs3.ReadItemTimeStep, ItemIdx starts at 1 ! 
% Time starts at Zero 
data = double(dfs3.ReadItemTimeStep(itemIdx+1 , 0).To3DArray()); 
data = rot90(data(:,:,zNum-z0+1));

% Creates the Date Time Vec by calling a special function
datenumVec = MR_createDateTimeVec(dfs3);

% delete values for Float seems to be the one that works for double.
deletevalue = dfs3.FileInfo.DeleteValueFloat;

% Empty Data Vector
dataVec = NaN(ny,nx,nz,dfs3.FileInfo.TimeAxis.NumberOfTimeSteps);

%  TODO: Check is there is no dedicated function for extracting a time
%  series
% This is SLOW! Is there no dedicated function for extracting a time series
% from a dfs3 file instead of getting ALL the values for all the time steps
for i = 0 : 1 : dfs3.FileInfo.TimeAxis.NumberOfTimeSteps-1;
    data = double(dfs3.ReadItemTimeStep(itemIdx+1 , i).To3DArray()); 
    % gets the data of interest 
    data = data(x0 : x0+nx-1, y0 : y0+ny-1, zNum-z0-nz+2:zNum-z0+1 );
    
    % Remove delete values --> sets them to NaN
    data(data==deletevalue) = NaN;

    
    % ROTATE 90 for every z and invert the z (so top 3d layer is lower in z
    % index). Need to create a new matrix to perform this.
    dat = NaN(ny,nx,nz);
    for iz = 1 : nz
        dat(:,:,iz) = rot90(data(:,:,nz-iz+1));
    end

    dataVec(:,:,:,i+1) = dat;
   
end

dfs3.Close();


end

