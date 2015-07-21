% Restores the default initial values and times in timeConfig.m and
% initialValues.m

function restore

configuration;

% Writes soilMoisture and gwStorage to the configuration file for the next
% run of the model. Overwrites the initial values used for starting the
% simulation.
fid = fopen(initialStatesFileName,'w+');
[~,~,initialStatesFileExtension] = fileparts(initialStatesFileName);
if strcmp(initialStatesFileExtension,'.m') == true
  fprintf(fid,'soilMoisture = 3;\n');
  fprintf(fid,'gwStorage = 4;\n');
elseif strcmp(initialStatesFileExtension,'.txt') == true
  fprintf(fid,'3\n');
  fprintf(fid,'4\n');
end
fclose(fid);

% Writes times to time configuration file. Used for the next simulation.
% Overwrites previous values!
fid = fopen(timeFileName,'w+');
[~,~,timeFileExtension] = fileparts(timeFileName);
if strcmp(timeFileExtension,'.m') == true
  fprintf(fid,'currentTime = 26000;\n');
  fprintf(fid,'simulationTimeStep = 1;\n');
  fprintf(fid,'finalTime = 26150;\n');
elseif strcmp(timeFileExtension,'.txt') == true
  fprintf(fid,'26000\n');
  fprintf(fid,'1\n');
  fprintf(fid,'26000\n');
end
fclose(fid);

% Deletes the result file.
if exist(resultFileName,'file')
  delete(resultFileName);
end

% Flush memory.
clear all

end