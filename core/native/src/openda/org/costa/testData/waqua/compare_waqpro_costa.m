% COMPARE THE RESULTS OF THE COSTA AND THE WAQPRO CALCULATIONS
%
%  Compare: 
%              costa and waqpro       Kalman caluclations
%              costa and waqpro       0-Kalman tests
%              waqpro                 Kalman calculation and 0-Kalman test
%

%
% $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/org/costa/testData/waqua/compare_waqpro_costa.m $
% $Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (Tue, 07 Oct 2008) $
% 

addpath('/v3/bo_omgeving/test-simona/matlab/SimonaTools');
request.simpar='NO';
request.slib3d='NO';
request.waqwnd='NO';
request.verbosity=1;
request.donotcompare={'powner'};
request.runids   = { ...
   'cta'; ...
   'pro'};
request.sdsfiles = { ...
   'SDS-cta'; ...
   'SDS-pro'};
request.dldsfiles = { ...
   '/v3/bo_omgeving/test-simona/matlab/SimonaTools/../../etc/WAQUA_lds-testbank-short.dlds', ...
   '/v3/bo_omgeving/test-simona/matlab/SimonaTools/../../etc/SIMPAR_lds.dlds', ...
   '/v3/bo_omgeving/test-simona/matlab/SimonaTools/../../etc/SLIB3D_lds.dlds'};
[ costa, waqpro] = CompareSds( request );



request.runids   = { ...
   '0cta'; ...
   '0pro'};
request.sdsfiles = { ...
   'SDS-0cta'; ...
   'SDS-0pro'};
[ costa, waqpro] = CompareSds( request );


request.runids   = { ...
   'waqpro'; ...
   '0pro'};
request.sdsfiles = { ...
   'SDS-waqpro'; ...
   'SDS-0pro'};
[ costa, waqpro] = CompareSds( request );


