function [ paramvalue ] = DA_PerturbParameter( perturbationOption, parameter, i,ens, filename, paramValues )
%DA_PerturbParameter Perturb parameter values and write new one to file
%   Takes statistical details about a parameter's mean value and st. dev
%   and sets a new value for the paremeter based on that.

switch perturbationOption
    
   case 1
      
        meanN = parameter{i,2};
        sdDevN = parameter{i,3};
        paramvalue = meanN + sdDevN.*randn( 1,1 ) ;
        
   case 2
        frac = parameter{i,2};
        valueOrig = paramValues( parameter{i,3} ,ens );
        paramvalue = frac * valueOrig ;     
        
        % CASE 3 = For exponential distributions (hydraulic conductivity)
    case 3
        meanN = parameter{i,2};
        sdDevN = parameter{i,3};
        b = log10(meanN) + randn(1,1).*sdDevN;
        paramvalue = 10.^b;
        
        
   otherwise
      throwAsCaller( 'Perturbation Option Not Valid' )
      
end


%Now Replace new param value with that in file
wholeFile = fileread(filename);

newStr = num2str(paramvalue);
oldStr = parameter{i,4};
newFileData = strrep(wholeFile,oldStr,newStr);

fileID = fopen(filename,'w');

fprintf(fileID,'%s',newFileData);

fclose(fileID);


end

