function [ success ] = MR_WritePFS( pfsCell, filenameOut )
%MR_WritePFS writes a pfs cell matrix to a file.
%   writes a pfs cell matrix line by line to a file, such as .SHE file
%   returns 0 if there's a fail and 1 if successfull. (Not a full check)

fid = fopen(filenameOut,'w');

nlines = length(pfsCell);
indent = 0;
success = 0;

for i = 1:nlines
   
   ln = '';
   if strcmp ( pfsCell{i} , '' ) ~= 1
       C = textscan( char( pfsCell{i} ) ,'%s'); 

       % Calculates the indentation based on
       % the beginnin square bracket is a start of a section
       % and the sections finishes with 'EndSec'
       if size(C{1},1) ~= 0
           if ( strcmp(C{1}{1}(1),'[' ) == 1 )
               indent = indent + 1;
           elseif ( strcmp(C{1,1}(1), 'EndSect')  == 1) 
                indent = indent - 1;
           end
       end

       % Check if there's a problem
       if indent < 0
           'PROBLEM Writing in MR_WritePFS !! '
           success = 0;
           break;
       end

       % For each line, adds the correct amount of indentation. 
       ln = char( pfsCell{i} );
       ind = indent;
       if ( strcmp(C{1}{1}(1),'[' ) == 1 )
           ind = ind - 1;
       end
       for ind = 1 : ind
          ln = ['  ' ln];
       end

   end
   
   fprintf(fid,'%s\n',ln);
    
end


fclose(fid);
success = 1;



end
