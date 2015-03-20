function editTekFile(tekfile)

%% editTekFile.m
%
% ALGEMEEN
%   Programmeur : Vincent Vuik, HKV lijn in water
%   Datum       : 18-Dec-2012
%   Versie      : 1
%   Project     : pr2516.10 - Elbe
%
%   Gewijzigd:  : 
%   Datum       : 
%   Versie      :
%   Project     : 
%
% BESCHRIJVING
%
% INPUT
%
% VOORBEELDINPUT
%   editTekFile('tekfile.tek')
%
% BENODIGD
%   
%
% OUTPUT
%   
%

%% INHOUD FUNCTIE

headerlines = 5;
tekfile2    = strrep(tekfile,'.tek','_edit.tek');

fid1        = fopen(tekfile);
fid2        = fopen(tekfile2,'w');

for i=1:headerlines
    fgetl(fid1);
end

while ~feof(fid1)
    regel1 = fgetl(fid1);
    regel2 = [regel1(1:8),regel1(10:13),regel1(16:end)];
    fprintf(fid2,'%s\r\n',regel2);
end

fclose all;

