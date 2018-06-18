function [ filename ] = DA_writeDFS0_setsMultipleTS_UnevenTime(filename, data, dt)
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
NET.addAssembly('DHI.Generic.MikeZero.EUM');
import DHI.Generic.MikeZero.DFS.*;
import DHI.Generic.MikeZero.DFS.dfs123.*;
import DHI.Generic.MikeZero.*

dfs0 = dfsTSO(filename,1);
% Set a file title
set(dfs0,'filetitle','variable');

% Start date (from the provided dt vector)
set(dfs0,'startdate',double(datevec(dt(1))));

% Non even time steps
set(dfs0,'timeaxistype','non_equidistant_calendar')

% Add Items, note the two ways of setting EUM unit and type
addItem(dfs0,'Water depth','Water Depth','m');

nt = length(dt);

%First Data Point
addTimesteps(dfs0,1);

if ~isnan(data(1))
    dfs0(1,0)  = single(data(1));
end

% Loop over timesteps and calculate average
for i = 1:nt-1
  addTimesteps(dfs0,1);
  writeTimes(dfs0,i,datevec(dt(i+1)));
  if ~isnan(data(i+1))
    dfs0(1,i)  = single(data(i+1));
  end
end

save(dfs0);
close(dfs0);


