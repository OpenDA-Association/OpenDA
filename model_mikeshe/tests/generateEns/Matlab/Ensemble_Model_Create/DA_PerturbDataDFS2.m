% Perturbs the data according to the specified case
%
%%%%%%%%%%%%%%%%% 
% Techniques for adding noise
% Options:
% 1. Percent noise -->   NewValue = OldValue * ( 1 + Rand(mu,stdev) )
%    Over the whole map, same percent noise
%    AND rounds to nearest 0.1 for every pixel ( needed for precipitation so the SVAT is stable )
%
% 2. ?? 
%...
% AND MORE
%
% Inputs, 
%
% Outputs, 
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com)
%           02/07/2013 
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [done] = DA_PerturbDataDFS2(dfs2FileNameIn, perturbationOption, forceing, i)
%DA_PerturbDataDFS2 Perturbs a dfs2 file time series with given statistical amount and method.
%

NET.addAssembly('DHI.Generic.MikeZero.DFS');
import DHI.Generic.MikeZero.DFS.*;

% OpenFIle
try
   dfs2  = DfsFileFactory.Dfs2FileOpenEdit(dfs2FileNameIn);
catch err
    rethrow(err)
    strcat( 'File ',' ' , filename )  
end  % end try/catch

% Check to make sure there is only one variable in the dfs file.
if dfs2.ItemInfo.Count > 1
    error('There is more than one variable in the dfs file:' +  dfs2FileNameIn)
end


nx = dfs2.SpatialAxis.XCount;
ny = dfs2.SpatialAxis.YCount;

% delete values for Float seems to be the one that works for double.
deletevalue = dfs2.FileInfo.DeleteValueFloat;

% Read first timestep
% Now for dfs2.ReadItemTimeStep, ItemIdx starts at 1 ! 
% !!! NOTE - it started at zero for the dfs2.ItemInfo !!!!!! 
% Time starts at Zero 
data = double(dfs2.ReadItemTimeStep(1 , 0).To2DArray()); 
data ( data == deletevalue ) = nan;

% Mean
meanN = forceing{i,2};

% Standard Deviation
sdDevN = forceing{i,3};

% Creates the Date Time Vec by calling a special function
datenumVec = DA_createDateTimeVec(dfs2);

% Number of time steps
nt = length(datenumVec);

switch perturbationOption
    
   % Percent     NewValue = OldValue * ( 1 + Rand(mu,stdev) )
   case 1     
        
       for tstep = 0:nt-1
            % Time starts at Zero 
            itemData = dfs2.ReadItemTimeStep(1,tstep);
            data = double(itemData.Data);
            %data = double(dfs2.ReadItemTimeStep(1 , i).To2DArray()); 
            data ( data == deletevalue ) = nan;
            data = data .* ( 1 + ones(1,nx*ny).* sdDevN.*randn( 1 ) );
            
            %round to nearest 0.1
            data = round(data.*1000)/1000;
            
            %delete all values below 0.001 in case
            data ( data<0.001 ) = 0;
            
            data ( isnan(data) ) = deletevalue;
            dfs2.WriteItemTimeStep(1,tstep,itemData.Time,NET.convertArray(single(data(:))));
            tstep
       end
       dfs2.Close();
        
   otherwise
      throwAsCaller( 'Perturbation Option Not Valid' )
end


end

