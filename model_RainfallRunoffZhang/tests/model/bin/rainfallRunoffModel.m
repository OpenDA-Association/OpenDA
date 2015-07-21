function rainfallRunoffModel
% function rainfallRunoffModel
% Test model for one rainfall runoff cell to test i/o with openDA.
%
% Details : The rainfall runoff model reads configuration files (specifying initial
%           parameter values, initial conditions, and the data source for the
%           boundary conditions and forecasts the total runoff of a
%           one-cell catchment.
%           After computations it writes the computed direct discharge of the
%           one-cell catchment in ASCII format.
%           The implementation follows Lu et al., 2008.
%           doi:10.1016/j.jhydrol.2008.07.021
%
% Input files needed:
%           configuration.m : Specifies the four parameters needed
%             to run the model in the matlab format and names of the input
%             and ouptut files.
%             Example for detBudykoModellConfigFile.m:
%               % Configuration file for deterministic Budyko Modell
%
%               % File containing parameters. Example:
%               % parameter_d = 0.2; % Fraction of groundwater storage going to base flow.
%               % parameter_Smax = 20; % Maximum soil water storage capacity [mm].
%               % parameter_alpha1 = 0.64; % Retention efficiency.
%               % parameter_alpha2 = 0.7; % Evapotranspiration efficiency.
%               parameterFileName = 'parameters.m';
%
%               % File name with values for the initial states. Contains the follwing int
%               % variables with respective example use:
%               % gwStorage = 4; % Initial groundwater storage [mm].
%               % soilMoisture = 2; % Initial soil moisture [mm].
%               initialValuesfileName = 'initialStates.m';
%
%               % Files containing boundary conditions over time. Format: .mat
%               potETfileName = 'potET';
%               precipFileName = 'precipitation';
%
%               % File to write results (total runoff) to.
%               resultFileName = 'totalRunoff.txt';
%
%               % Result file to interact with openDA. Writes one result value for the
%               % current time step.
%               odaResultFileName = 'odaTotalRunoff.txt';
%
%               % File name for time specifications. Contains the int variables currentTime,
%               % simulationTimeStep, and finalTime according to the following format
%               % example:
%               % currentTime = 735210; % Start time of the simulation.
%               % simulationTimeStep = 1; % Time step size.
%               % finalTime = 735211; % Stop simulation after computation of finalTime.
%               timeFileName = 'timeConfig.m';
%
%           timeConfig.m : Defines values for the start time (currentTime) of the
%             simulation, the time step size the simulation takes, and
%             the last time step to be computed (finalTime). The contents of the 
%             file is overwritten during simulation.
%             The first forecast is for currentTime + simulationTimeStep.
%             Example for timeConfig.m:
%               currentTime = 735210; % Start time.
%               simulationTimeStep = 1; % Time step size to simulate.
%               finalTime = 735211; % End time of simulation.
%
%           initialStates.m : Defines the initial values for soil moisture
%             and groundwater storage. The contents of this file are
%             overwritten during simulation.
%             Example for initialValues.m:
%               gwStorage = 4; % Initial groundwater storage [mm].
%               soilMoisture = 2; % Initial soil moisture [mm].
%
% Output file written : The total runoff [mm] of each simulation time step is
%           written to the output file specified by
%           detBudykoModellConfigFile.m. The output has following format:
%             <currentTime> <totalRunoff>
%
% Author  : Robert Naudascher, Tobias Siegfried, Beatrice Marti.
% Copy right : hydrosolutions ltd.
%

%% Setup of the model run.
% Loading of parameters, initial values, boundary conditions and setting up
% of the model run.
fprintf('########\n')

% Read the configuration file.
fprintf('# Reads configuration\n')
configuration;

% Read the parameter file with method depending on file extension.
fprintf('# Reads parameter file\n')
isInDir(parameterFileName); % 1 for true, 0 and exit for false.
[~,~,parameterFileExtension] = fileparts(parameterFileName);
if strcmp(parameterFileExtension,'.m') == true
  run(parameterFileName); % Reading from matlab file source.
else 
  sprintf('InputFileError: The input file %s must have extionsion ".m"',parameterFileName)
  return
end

% Impose hard constraints on parameters. This is necessary in case
% parameters are updated with a kalman filter method in order to avoid
% unphysical resuslts. Wang et al., 2009, WRR showed, that the "native"
% approach to impose constraints has a reasonable performance.
if parameter_d < 0
  parameter_d = 0;
elseif parameter_d > 1
  parameter_d = 1;
end

if parameter_Smax < 0 
  parameter_Smax = 0;
end

if parameter_alpha1 < 0 
  parameter_alpha1 = 0;
elseif parameter_alpha1 > 1
  parameter_alpha1 = 1;
end

if parameter_alpha2 < 0
  parameter_alpha2 = 0;
elseif parameter_alpha2 > 1
  parameter_alpha2 = 1;
end

% Load initial values for soil moisture and groundwater storage.
fprintf('# Reads initial states file.\n')
isInDir(initialStatesFileName);
[~,~,initialStatesFileExtension] = fileparts(initialStatesFileName);
if strcmp(initialStatesFileExtension,'.m') == true
  run(initialStatesFileName);
else
  sprintf('InputFileError: The input file %s must have extionsion ".m"',initialStatesFileName)
  return
end

% Impose hard constraints on updated states. This is necessary because the
% analyzing algorithm does not know about constraints on the states. 
if soilMoisture < 0 % Soil moisture must be larger than or equal to zero.
  soilMoisture = 0;
elseif soilMoisture > parameter_Smax % Soil moisture cannot be higher than the specified maximum soil moisture.
  soilMoisture = parameter_Smax;
end

if gwStorage < 0 % Groundwater storage cannot be smaller than zero.
  gwStorage = 0;
end

% Store initial States in variables.
fprintf('#     initial soil moisture = %d, initial groundwater storage = %d\n', soilMoisture, gwStorage)
initialSoilMoisture = soilMoisture;
initialGwStorage = gwStorage;

% Load the values for currentTime, simulationTimeStep, and finalTime.
fprintf('# Reads time configuration\n')
isInDir(timeFileName);
[~,~,timeFileExtension] = fileparts(timeFileName);
if strcmp(timeFileExtension,'.m') == true
  run(timeFileName);
else
  sprintf('InputFileError: The input file %s must have extionsion ".m"',timeFileName)
  return
end

% Test whether or not the time variables are read.
if exist('currentTime','var') == 0 || exist('simulationTimeStep','var') == 0 || exist('finalTime','var') == 0
  fprintf('Error: Time configuration file is empty.\n')
end

fprintf('#     currentTime = %d, simulationTimeStep = %d, finalTime = %d\n',currentTime,simulationTimeStep,finalTime)
% Currently only timeStepSize = 1 is implemented.
if simulationTimeStep > 1 || simulationTimeStep < 1
  fprintf('Error: Currently only simulationTimeStep = 1 is implemented.\n')
  fprintf('       The configuration read simulationTimeStep = %d\n',simulationTimeStep)
end
% Check that finalTime is larger than currentTime.
if finalTime < currentTime
  fprintf('Error: finalTime < currentTime.\n')
  fprintf('       The configuration read finalTime = %d, currentTime = %d.\n', finalTime, currentTime)
end

% Load the time dependent boundary conditions. 
fprintf('# Load time dependent boundary conditions.\n')
isInDir(potETfileName);
potET = dlmread(potETfileName,' ',1,0);

isInDir(precipFileName);
precip = dlmread(precipFileName,' ',1,0);
clear potETfileName precipFileName

% To simplify the coding, define w1 and w2 from the parameters alpha1 and
% alpha2.
w1 = 1/(1-parameter_alpha1);
w2 = 1/(1-parameter_alpha2);

numberOfTimeSteps = (finalTime - currentTime)/simulationTimeStep;
% Test if integer.
if (mod(numberOfTimeSteps,1) ~= 0)
  fprintf('Error in time configuration: finalTime-currentTime is not a multiple of simulationTimeStep.\n')
end

% Preallocation of variable lists.
a = NaN(numberOfTimeSteps,1);
b = NaN(numberOfTimeSteps,1);
c = NaN(numberOfTimeSteps,1);
catchmentRainfallRetention = NaN(numberOfTimeSteps,1);
directRunoff = NaN(numberOfTimeSteps,1);
waterAvailability = NaN(numberOfTimeSteps,1);
evapotranspirationOpportunity = NaN(numberOfTimeSteps,1);
gwRecharge = NaN(numberOfTimeSteps,1);
actualET = NaN(numberOfTimeSteps,1);
soilMoisture = NaN(numberOfTimeSteps,1);
baseFlow = NaN(numberOfTimeSteps,1);
gwStorage = NaN(numberOfTimeSteps,1);
totalRunoff = NaN(numberOfTimeSteps,1);

% Initialize timeStepCounter to allocate the time dependent values within
% the for loop.
timeStepCounter = 0;

%% Forecast.
fprintf('# FORECASTING - start time %d , step size : %d , last forecast : %d.\n', currentTime, simulationTimeStep, finalTime)

for time = 1:simulationTimeStep:numberOfTimeSteps
  
  % Increment timeStepCounter, used to avoid sparsely filled vectors in case simulationTimeStep > 1.
  timeStepCounter = timeStepCounter + 1;
  
  % Compute the index to access time series of forecasted boundary conditions.
  bcTimeIndex = currentTime + (time-1) * simulationTimeStep;
  
  % Compute index of dryness a. "a" is a fraction between potential
  % catchment rainfall retention and precipitation.
  if time == 1
    a(timeStepCounter) = (parameter_Smax - initialSoilMoisture + potET(potET==bcTimeIndex,2)) / precip(precip==bcTimeIndex,2);
  elseif time > 1
    a(timeStepCounter) = (parameter_Smax - soilMoisture(timeStepCounter-1) + potET(potET==bcTimeIndex,2)) / precip(precip==bcTimeIndex,2);
  end
  
  % If the potential rainfall retention of a catchment is above tolerance,
  % it is set equal to precipitation.
  if ~isinf(a(timeStepCounter))
    catchmentRainfallRetention(timeStepCounter) = precip(precip==bcTimeIndex,2) * (1+a(timeStepCounter)-(1+a(timeStepCounter)^w1)^(1/w1)); % Fu's curve
  else
    catchmentRainfallRetention(timeStepCounter) = precip(precip==bcTimeIndex,2);
  end
  
  % Rainfall minus what is retained in the catchment equals direct runoff.
  directRunoff(timeStepCounter) = precip(precip==bcTimeIndex,2) - catchmentRainfallRetention(timeStepCounter); %
  
  % Water availability is the retention plus the present soil water storage.
  if time == 1
    waterAvailability(timeStepCounter) = catchmentRainfallRetention(timeStepCounter) + initialSoilMoisture;
  elseif time > 1
    waterAvailability(timeStepCounter) = catchmentRainfallRetention(timeStepCounter) + soilMoisture(timeStepCounter-1);
  end
  
  % Compute the upper (or demand) limit of the evapotranspiration
  % opportunity divided by the water availability: "b". Again a dryness
  % index.
  b(timeStepCounter) = (potET(potET==bcTimeIndex,2) + parameter_Smax) / waterAvailability(timeStepCounter);
  if ~isinf(b(timeStepCounter))
    evapotranspirationOpportunity(timeStepCounter) = waterAvailability(timeStepCounter)*(1+b(timeStepCounter)-(1+b(timeStepCounter)^w2)^(1/w2)); % Fu's curve
  else
    evapotranspirationOpportunity(timeStepCounter) = waterAvailability(timeStepCounter);
  end
  
  % Groundwater recharge.
  gwRecharge(timeStepCounter) = waterAvailability(timeStepCounter) - evapotranspirationOpportunity(timeStepCounter);
  
  % Another dryness index c to compute actual evapotranspiration.
  c(timeStepCounter) = potET(potET==bcTimeIndex,2) / waterAvailability(timeStepCounter);
  if ~isinf(c(timeStepCounter))
    actualET(timeStepCounter) = waterAvailability(timeStepCounter)*(1+c(timeStepCounter)-(1+c(timeStepCounter)^w2)^(1/w2));
  else
    actualET(timeStepCounter) = waterAvailability(timeStepCounter);
  end
  
  % Soil water storage. 
  soilMoisture(timeStepCounter) = evapotranspirationOpportunity(timeStepCounter) - actualET(timeStepCounter);
  
  if soilMoisture(timeStepCounter)<0
    soilMoisture(timeStepCounter) = 0;
  end
  
  % Computes base flow, new groundwater storage (overwrites initial
  % value!) and total runoff.
  if time == 1
    baseFlow(timeStepCounter) = parameter_d * initialGwStorage;
    gwStorage(timeStepCounter) = (1-parameter_d) * initialGwStorage + gwRecharge(timeStepCounter);
  elseif time > 1
    baseFlow(timeStepCounter) = parameter_d * gwStorage(timeStepCounter-1);
    gwStorage(timeStepCounter) = (1-parameter_d) * gwStorage(timeStepCounter-1) + gwRecharge(timeStepCounter);
  end
  
  totalRunoff(timeStepCounter) = baseFlow(timeStepCounter) + directRunoff(timeStepCounter);
  
  % Correct numerical fuckup (slightly negative totalRunoff that may
  % happen for extremly fast reacting models (parameter combinations).
  if totalRunoff(timeStepCounter) < 0
    totalRunoff(timeStepCounter) = 0;
  end
  
end

%% Wrapping up.
% Writes the results to files.
fprintf('#######\n')

% Writes soilMoisture and gwStorage to the configuration file for the next
% run of the model. 
isInDir(initialStatesFileName);
fid = openFile(initialStatesFileName,'w+');
fprintf(fid,'%% currentTime = %d, finalTime = %d;\n', currentTime, finalTime);
fprintf(fid,'soilMoisture = %d;\n', soilMoisture(timeStepCounter)); %format for longEng precision: 20.15g
fprintf(fid,'gwStorage = %d;\n', gwStorage(timeStepCounter)); %format for longEng precision: 20.15g
fclose(fid);

% Print total runoff to file <currentTime> <totalRunoff>.
fid = openFile(resultFileName,'w+');
%fprintf(fid,'%% time [mjd] totalRunoff [mm]\n');
for time = 1:simulationTimeStep:numberOfTimeSteps
  fprintf(fid,'%d %d\n',currentTime + (time) * simulationTimeStep, totalRunoff(time)); %format for longEng precision: 20.15g
end
fclose(fid);

% Returns command to keyboard before exiting the function. Usefull for debugging.
% keyboard 

% Clear all variables to force matlab to load new file versions and not the
% cached ones.
clear all

end


%% Local functions
% isInDir
% openFile

function isInDir(fileName)
% function isInDir(fileName)
% Test if given file name exists in current directory. Exits matlab in case
% it does not find the file.
% @arg1 fileName string specifying the fileName.
%

% Parse path of current directory.
directoryContent = dir;

% Pass struct to cell.
directoryContentCell = struct2cell(directoryContent);
booleanValue = any(ismember(directoryContentCell(1,:),fileName));

if booleanValue == 0
  sprintf('Error, could not find file %s in current directory: %s\n',fileName,pwd)
end

end


function fileId = openFile(fileName,fopenArgs)
% function fileId = openFile(fileName, fopenArgs)
% Opens file specified by fileName using fopen with given arguments and
% throws an error message if fopen fails.
% @arg fileName string specifying the file name.
% @arg fopenArgs string specifyng the arguments to fopen, e.g. w+ for
%               opening or creating fileName to write.
% @return fileId int > 0
%%

% Assigning default values.
[fileId,errorMessage] = fopen(fileName,fopenArgs);
if fileId < 0
  sprintf('Error opening file %s using args %s\n',fileName, fopenArgs)
  sprintf('%s\n',errorMessage)
end

end