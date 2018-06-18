% A simple routine to copy MIKE models And Perturb forcing to create an ensemble
% of models. Used for the DA framework
% 
%
clear all
historicalTSdfs = '';

% The directory where ther is the BaseModel directory.
baseModelDir = 'c:\devel_nils\public\model_mikeshe\tests\generateEns\BaseModel\';
% Number of Ensembles to Create
numEns = 50;


destinationDirectory = 'Ensembles'


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Techniques for adding noise to forecing
% Options:
% 1. Random Gausian ( negative values OK )
% 2. Random Gausian ( negative values NOT OK - Precipitation)
% 3. Slow Change - MA ( negative values NOT OK - Precipitation) 
% 4. Shuffle
% 5. LogNormal (takes mean and st.dev)
% 6. Exponential (takes mean)
% 7. Second Order Markov Chain (WeaGETS - Matlab implementation) 
% 8. Percent - NewValue = OldValue * ( 1 + Rand(mu,stdev) )  and mu = 0

PerturbationOption{1} = 8;
PerturbationOption{2} = 8;

% FORCING INFORMATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FORCING 1
% FileName
forceing{1,1} = 'Time\Precipitation.dfs0';
% Perturbation Mean - used if not precipitation
forceing{1,2} = 0;
% Perturbation St. Dev  - used if not precipitation
forceing{1,3} = 0.25;
% Slower Change type process - Note in this algorithm Negatives NOT OK
% How much is the value based on the historical vs how much the value
% before influences the next. 
forceing{1,4} = 0.70;

% dfs0 file with precipitation for historical statistics gathering   
% Used in Second Order Markov Chain
historicalTSdfs = ['';]
forceing{1,5} = historicalTSdfs;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FORCING 2
% FileName
forceing{2,1} = 'Time\PotentialEvap.dfs0';
% Perturbation Mean - used if not precipitation
forceing{2,2} = 0;
% Perturbation St. Dev  - used if not precipitation
forceing{2,3} = 0.25;
% Slower Change type process - Note in this algorithm Negatives NOT OK
% How much is the value based on the historical vs how much the value
% before influences the next. 
forceing{2,4} = 0.70;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% COPY FILES
% retuns the new directories into dirList (ending with the '\' char)
dirList = DA_CopyDirectory(baseModelDir,destinationDirectory, numEns);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purturb the forcing data
%

numToPerturb = size(forceing,1);

% Perturb data according to user selection then saves the perturbed data
% to the dfs0 file.
for i = 1:numToPerturb
    disp(num2str(i));
    % Gets Data
    [datenumVec , dataVec ] = DA_readDFS0_getMultipleTS_Uneven( strcat( baseModelDir , forceing{i,1} ) );
    
    % For each ensemble Member
    for ens = 1:numEns
        
        % Calls function that gets the data and perturbs it according to the
        % option
        [datenumVec , dataPert] = DA_PerturbData(datenumVec , dataVec, PerturbationOption{i} , forceing, i);

        % Stores it to File
        filename = strcat( dirList{ens} , forceing{i,1} );
        DA_writeDFS0_setsMultipleTS( filename, dataPert );
    end
    
    figure
    plot(dataVec); hold on; plot(dataPert,'k');
    sum(dataVec)
    sum(dataPert)

end
disp('DONE');






