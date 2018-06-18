% Perturbs the data according to the specified case
%
%%%%%%%%%%%%%%%%% 
% Techniques for adding noise
% Options:
% 1. Random Gausian ( negative values OK )
% 2. Random Gausian ( negative values NOT OK - Precipitation)
% 3. AR(1)
% 4. Shuffle
%...
% AND MORE
%
% Inputs, 
%
% Outputs, 
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com)
%           10/04/2012 
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [datenumVec , dataPerturbed] = DA_PerturbData(datenumVec , dataVec, perturbationOption, forceing, i)
%UNTITLED Perturbs a time series with given statistical amount and method.
%
% Create Empty matrix for data
dataPerturbed = zeros( size(dataVec,1)  , size(dataVec,2) );

switch perturbationOption
    
   case 1
        % throwAsCaller( 'Not Implemented yet!' )
        meanN = forceing{i,2};
        sdDevN = forceing{i,3};
        dataPerturbed = dataVec + ( meanN + sdDevN.*randn( size(dataVec) ) );
   case 2
        %throwAsCaller( 'Not Implemented yet!' )
        meanN = forceing{i,2};
        sdDevN = forceing{i,3};
        dataPerturbed = dataVec + ( meanN + sdDevN.*randn( size(dataVec) ) );

        % Carryover negatives
        % probably a way to do it more elegantly. Not row by row.
        for i = 1:size(dataVec,2)
            Ineg = find( dataPerturbed(:,i) <  0 );
            Ipos = find( dataPerturbed(:,i) >= 0 );
            sumOfNegative = abs( sum(dataPerturbed(Ineg,i)) ) ;
            dataPerturbed(Ineg,i) = 0;
            numOfPositiveDataIndices = length(Ipos);
            quantityToAdd = sumOfNegative / numOfPositiveDataIndices;
            
            dataPerturbed(Ipos,i) = dataPerturbed(Ipos,i) + quantityToAdd;
        end
        
        
    case 3
        
        meanN = forceing{i,2};
        sdDevN = forceing{i,3};
        historicFactor = forceing{i,4}; 
        nSteps = size(dataVec,1);
        
        if ( historicFactor > 1 || historicFactor < 0 )
            throwAsCaller( 'forceing{i,3} term Must be between 0 and 1' )
        end
        if ( meanN ~= 0 )
            throwAsCaller( 'forceing{i,3} cannot have a mean' )
        end
        
        % Perturbed data for the First entry
        dataPerturbed(1,:) = dataVec(1,:) + ( meanN + sdDevN.*randn( 1,size(dataVec,2) ) );
        % For each time entry thereafter, it's a combination of historical
        % and the pervious time series value.
        for i = 2:size(dataVec,1)
            dataPerturbed(i,:) = historicFactor .* dataVec(i,:) +  (1- historicFactor).* dataPerturbed(i,:) +  ( meanN + sdDevN.*randn(1, size(dataVec,2) ) );  
            %dataPerturbed(i,:) = ( meanN + sdDevN.*randn(1, size(dataVec,2) ) );
        end
        
        % Carryover negatives
        % probably a way to do it more elegantly. Not row by row.
        for i = 1:size(dataVec,2)
            Ineg = find( dataPerturbed(:,i) <  0 );
            Ipos = find( dataPerturbed(:,i) >= 0 );

            % sumOfNegative = abs(sum( dataPerturbed(Ineg,i)));
            dataPerturbed(Ineg,i) = 0;
            
            diffFromOrig = sum(dataVec(:,i)) - sum( dataPerturbed(:,i));
            
            if( diffFromOrig > 0 )
                quantityToAdd = diffFromOrig / nSteps;
                dataPerturbed(:,i) = dataPerturbed(:,i) + quantityToAdd;
            elseif( diffFromOrig < 0  )
                quantityToSub = diffFromOrig / length(Ipos);
                dataPerturbed(Ipos,i) = dataPerturbed(Ipos,i) + quantityToSub;
            end
            
            Ineg = find( dataPerturbed(:,i) <  0 );
            Ipos = find( dataPerturbed(:,i) >  mean( dataPerturbed(:,i)) );
            quantityToAdd = abs(sum( dataPerturbed(Ineg,i)))/length(Ipos);
            dataPerturbed(Ineg,i) = 0;
            dataPerturbed(Ipos,i) = dataPerturbed(Ipos,i) - quantityToAdd;
        end
        
        % Test
        %sum(dataVec)
        %sum(dataPerturbed)
        %plot(dataPerturbed)
        
        
   case 4
       for i = 1:size(dataVec,2)
            ix = randperm(size(dataVec,1));
            dataPerturbed(:,i) = dataVec(ix,i);
       end
       
   % LOGNORMAL
   case 5
        ['NOTE: NEED STATISTICAL TOOLBOX FOR THIS!!! ']
        ['*** Otherwise you will get a nondescript nonsensical error about not taking doubles etc. ']
        meanN = forceing{i,2};
        sdDevN = forceing{i,3};
        variance = sdDevN * sdDevN;
        
        mu = log((meanN^2)/sqrt(variance+meanN^2));
        sigma = sqrt(log(variance/(meanN^2)+1));

        dataPerturbed = lognrnd(mu,sigma,size(dataVec),1);

   % Exponential Distribution
   case 6
        ['NOTE: NEED STATISTICAL TOOLBOX FOR THIS!!! ']
        ['*** Otherwise you will get a nondescript nonsensical error about not taking doubles etc. ']
        meanN = forceing{i,2};

        dataPerturbed = exprnd(meanN,size(dataVec),1);        
    
    case 7
        % Gets data from dfs0
        ['USES WeaGETS - modified without the temperature and other data']
        [dataO] = DA_FormatDataForWeaGETS(forceing{i,5});
        nDays = length(dataVec);
        numYearsToGenerate = ceil(nDays / 365 ) + 1;
        dataPerturbed = DA_GeneratePrecipitation(dataO,numYearsToGenerate);
        clear dataO;
        dataPerturbed = reshape( dataPerturbed , numYearsToGenerate * 365 , 1);
        dataPerturbed =  dataPerturbed(1:length(dataVec));

   % Percent     NewValue = OldValue * ( 1 + Rand(mu,stdev) )
   case 8     
        sdDevN = forceing{i,3};
        dataPerturbed = dataVec .* ( 1 + sdDevN.*randn( size(dataVec) ) );
        dataPerturbed(dataPerturbed < 0.001 & dataPerturbed > -0.001) = 0;
        
   otherwise
      throwAsCaller( 'Perturbation Option Not Valid' )
end


end

