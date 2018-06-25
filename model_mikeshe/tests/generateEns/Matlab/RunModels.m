%RunModels Runs a number of MikeSHE models by first running the preprocessor, then the Water Movement.
% FOR 2013.
% Created by Marc Ridler (MER)

clear all
baseModelDir = 'c:\devel_nils\public\model_mikeshe\tests\generateEns\Ensembles\';
numEns = 50;

modelName = 'Karup.SHE';

PPexe = '"C:\Program Files (x86)\DHI\2016\bin\x64\MSHE_PreProcessor.exe"';
WWexe = '"C:\Program Files (x86)\DHI\2016\bin\x64\MSHE_WaterMovement.exe"';

for i=0:numEns-1
    
    filename = strcat(baseModelDir , num2str(i),'\',modelName);
    cmd = [PPexe , ' ' , filename];
    dos(cmd);

    cmd = [WWexe , ' ' , filename];
    dos(cmd);
    
    disp( ['DONE ' ,'.... ' , num2str(i)  ]);
    
end


%%

