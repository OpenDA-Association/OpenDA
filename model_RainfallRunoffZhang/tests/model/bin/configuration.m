% Configuration file for deterministic Budyko Modell

% File containing parameters. Example:
% parameter_d = 0.2; % Fraction of groundwater storage going to base flow.
% parameter_Smax = 20; % Maximum soil water storage capacity [mm].
% parameter_alpha1 = 0.64; % Retention efficiency.
% parameter_alpha2 = 0.7; % Evapotranspiration efficiency.
parameterFileName = 'parameters.m';

% File name with values for the initial states. Contains the follwing int
% variables with respective example use:
% gwStorage = 4; % Initial groundwater storage [mm].
% soilMoisture = 2; % Initial soil moisture [mm].
initialStatesFileName = 'initialStates.m';

% Files containing boundary conditions over time. Format: .mat
potETfileName = 'potETNewTimes.txt';
precipFileName = 'precipitationNewTimes.txt';

% File to write results (total runoff) to. Values are appended. 
resultFileName = 'totalRunoff.txt';

% Result file to interact with openDA. Writes one result value for the
% current time step. 
odaResultFileName = 'odaTotalRunoff.txt';

% File name for time specifications. Contains the int variables currentTime,
% simulationTimeStep, and finalTime according to the following format
% example:
% currentTime = 735210; % Current time step.
% simulationTimeStep = 1; % Number of time step size to simulate (obsolete?)
% finalTime = 735211; % Stop simulation after computation of finalTime time step
timeFileName = 'timeConfig.m';
