function A=read_gain(dirname)
% function A=read_gain(dirname)
%
% Read gain matrix 'enkf_wave_gain.xml' from the directory 'dirname'
%
% author: Nils van Velzen
%
file_base='enkf_wave_gain.xml';
file=[dirname,'/',file_base];

fid=fopen(file,'r');
line=' ';
i=0;
while (line ~= -1)
   line=fgets(fid);
   idx=find(line=='<');
   if (length(idx)==2)
     if (line(idx(1):idx(1)+7)=='<vector>') 
        i=i+1;
        A(:,i)=str2num(line(idx(1)+8:idx(2)-1))';
     end
   end
end
