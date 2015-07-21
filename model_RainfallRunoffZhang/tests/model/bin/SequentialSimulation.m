function SequentialSimulation(modelName,modelConfiguration,SSresultFileName)
% function SequentialSimulation(modelName,modelConfiguration,SSresultFileName)
% 
% Calls modelName and simulates it for a given number of time steps
% specified in the configuration of the model run. 
% 
% @arg1 modelName (string) Name of the matlab model to be called.
% @arg2 modelConfiguration (string) Name of the model configuration file.
% @arg3 SSresultFileName (string) Name of file for results of
%           SequentialSimulation.
% 
% Details The simulation time span is broken up in pieces of time step size
%         given in the model configuration. The time configuration is
%         overwritten in every time step. The initial states are written by
%         the model code. 
% Bugs    The function restore overwrites the time configuration. Modify
%         time configuration in restore.
% 
% author Beatrice Marti, hydrosolutions ltd.

%% Set up the simulation.
% Restore to original values and remove unused files from previous simulations.
restore; 

% Read configuration of the current model run.
run(strcat(modelConfiguration,'.m'));

% Get the time configuration.
[currentTime,simulationTimeStep,finalTime] = setupSequentialSimulation(timeFileName);
numberOfTimeSteps = (finalTime-currentTime)/simulationTimeStep;

% Print header of SSresultFile. Overwrites existing file!
fid = fopen(SSresultFileName,'w+');
fprintf(fid,'time [mjd] totalRunoff [mm]\n');
fclose(fid);

%% Sequential simulation.
for i = 1:1:numberOfTimeSteps
  
  % Overwrite the time configuration file.
  selectionTimeSpan = [currentTime+i-1,simulationTimeStep,currentTime+i*simulationTimeStep];
  overwriteTimeConfiguration(timeFileName,selectionTimeSpan);
  % Load new time configuration. Needed because of memory flush in
  % overwriteTimeConfiguration.
  run(strcat(modelConfiguration,'.m'));
   
  % Run forecast for the selected time span.
  run(strcat(modelName,'.m'));
  
  % Reads result file and stores data.
  storeResults(SSresultFileName,resultFileName);  
  
end

%% Wrapping up.
% No wrapping up needed. Can be done by calling restore.


end

%% Local functions.
% - setupSequentialSimulation()
% - rainfallRunoffModelWrapper()

function [currentTime,simulationTimeStep,finalTime] = setupSequentialSimulation(timeFileName)
% function [currentTime,simulationTimeStep,finalTime] = setupSequentialSimulation(timeFileName)
% Reads configuration and splits the simulation time span to smaller
% sequences.
% @arg1 timeFileName (string) Contains the name of the file holding the
%            time configuration.
% @output1 currentTime (int) Start time of the simulation.
% @output2 simulationTimeStep (int) Time step size.
% @output3 finalTime (int) End time of the simulation.

% Test if the time file name is in the current directory.
directoryContent = dir;
directoryContentCell = struct2cell(directoryContent);
booleanValue = any(ismember(directoryContentCell(1,:),timeFileName));

if booleanValue == 0
  sprintf('Error, could not find file %s in current directory: %s\n',timeFileName,pwd)
end

% Read the time configuration file.
run(timeFileName);

end

function overwriteTimeConfiguration(timeFileName,selectionTimeSpan)
% function overwriteTimeConfiguration(startTime,endTime)
% Rewrite time configuration file.
% @arg1 timeFileName (string) Contains name of the time configuration file.
% @arg2 selectionTimeSpan (int[3]) Array of 3 integers specifying start time,
%             time step size and end time of the model run in this order.

fid = fopen(timeFileName,'w+');
% fprintf('write:[%d, %d, %d]\n',selectionTimeSpan(1),selectionTimeSpan(2),selectionTimeSpan(3))
fprintf(fid,'currentTime = %d;\n',selectionTimeSpan(1));
fprintf(fid,'simulationTimeStep = %d;\n',selectionTimeSpan(2));
fprintf(fid,'finalTime = %d;\n', selectionTimeSpan(3));
fclose(fid);
clear all; % Flush memory. Needed to ashure correctly written files.

end

function storeResults(SSresultFileName,resultFileName)
% function storeResults(SSresultFileName,resultFileName)
% Reads the result file and appends it to sequential simulation results file.
% @arg1 SSresultFileName (string) File name of the SequentialSimulation
%           results. Written during runtime.
% @arg2 resultFileName (string) File name of results written by the
%           model.

% Read model results.
result = dlmread(resultFileName,' ',0,0); % Read space separated data from 1st row.

% Test if result contains 2 space separated numbers.
if length(result) ~= 2
  fprintf('Error parsing model result file %s.\n',resultFileName)
end

% Append model results to SSresultFileName.
fid = fopen(SSresultFileName,'a+');
fprintf('time = %d, totalRunoff = %d\n',result(1),result(2))
fprintf(fid,'%d %d\n',result(1),result(2));
fclose(fid);

clear result;

end

