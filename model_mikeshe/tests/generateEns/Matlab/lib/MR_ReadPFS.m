function [ curr ] = MR_ReadPFS( filename )
%MR_ReadPFS Reads a pfs file (such as a .SHE file)
%   Reads a pfs file line by line and stores it to a cell matrix. 
%   Stips the blank spaces at the start and end 

fid = fopen(filename);

k = 0;
%curr = cell(65193,1);


while ~feof(fid)
    k = k + 1;
    curr{k,1} = cellstr ( strtrim( char( fgetl(fid) ) ) );
end
    
fclose(fid);
%curr = curr';

end

