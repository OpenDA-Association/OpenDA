% A simple to prepare the restart DA input file from the template input
%
% Author: Nils van Velzen
% 
%
clear all
historicalTSdfs = '';
EnsembleDir = 'c:\devel_nils\public\model_mikeshe\tests\generateEns\Ensembles\';
infile = 'Karup.SHE';
outfile = 'Karup_DA.SHE'

tijdsblokje= ['[SimulationPeriod]',char(10),...                                               '
      '   Touched = 1',char(10),...
      '   IsDataUsedInSetup = 1',char(10),...
      '   HotStart = 1',char(10),...
      '   HotStartResultFile = |.\Karup.she - Result Files\Karup.Sheres|',char(10),...
      '   HotStartDate = 1972, 12, 28, 0, 0',char(10),...
      '   HotStartDateIndex = 0',char(10),...
      '   SimStartFromHotStartDate = 1',char(10),...
      '   SIMSTART = 1972, 12, 28, 0, 0',char(10),...
      '   SIMEND = 1976, 1, 1, 0, 0',char(10)];
  %    'EndSect  // SimulationPeriod\n'];

numEns = 50;

   
    % For each ensemble Member
for ens = 1:numEns
   filenameIn = strcat( EnsembleDir , num2str(ens-1),'\', infile ); 
   filenameOut = strcat( EnsembleDir , num2str(ens-1),'\', outfile ); 
   
   wholeFile = fileread(filenameIn);
   
   %Find start of time block
   p1=findstr(wholeFile,'[SimulationPeriod]');
   p2=min(findstr(wholeFile(p1:end),'EndSec'));
   
   newFileData= [wholeFile(1:p1-1),tijdsblokje,wholeFile(p1+p2-1:end)];
   
   disp(['Write :',filenameOut]);
   fileID = fopen(filenameOut,'w');

   fprintf(fileID,'%s',newFileData);

   fclose(fileID);
end
    
disp('DONE');






