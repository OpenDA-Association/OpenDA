
function [directoryNames] = DA_CopyDirectory(baseModelDir, destinationName ,numEns)
%DA_CopyDirectory Copies the basedirectory multiple times (numEns times)
%
% Copies the basedirectory multiple times (numEns times)
% The files will be copied to the parent of the baseDirectory + the number 
% of the ensemble (zero based).
%
% Therefore is BaseDirectory is C:\work\Models\BaseModel\
% And numEns is set to 3
% Then C:\work\Models\BaseModel\ will be copied to
% 
% C:\work\Models\Ensembles\0
% C:\work\Models\Ensembles\1
% C:\work\Models\Ensembles\2
%
%
% Inputs, 
%   baseModelDir  ( string ) MUST END WITH '\' CHARACTER !!!!!!!!!!!!!!
%   number of times to copy
%
% Outputs, 
%   The newly created model directories
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com)
%           10/04/2012 
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find Parent Directory
% Check. Ensure the last character in baseModelDir is '\'
% if not - adds it
finalChar = baseModelDir( length( baseModelDir ) );
if ( finalChar ~= '\' )
    baseModelDir = strcat(baseModelDir ,'\');
    %throwAsCaller( 'Final character in baseModelDir NOT a \\ ' )
end

% Returns the postition before --> ie the parent directory
pos = strfind(baseModelDir, '\');
parentDir = baseModelDir(1:pos(end-1));

parentDir = strcat( parentDir ,destinationName ,'\');
mkdir( parentDir );      


for ens=0:numEns-1
    directoryNames{ens+1} = strcat( parentDir , num2str(ens) ,'\');    
    copyfile( strcat( baseModelDir , '*' ) , directoryNames{ens+1} );
end

end

