function [hist,delta]=computehistogram(ncall,dim,dim_ens,element,state,ens,hist)

%--------------------------------------------
% [hist,delta]=computehistogram([b,d]=computeRCRV(ens,ana,sig0,missing,m,nens)
%
% Increment information on an ensemble rank histogram.
%
% INPUTS:   ncall               number of calls to tool
%           dim                 state dimension
%           dim_ens             ensemble size
%           element             element of vector used for histogram 
%                               (If element=0, all elements are used)
%           state(dim)          state vector
%           ens(dim, dim_ens)   state ensemble
% IN/OUT:   hist(dim_ens+1)     histogram about the state
% OUTPUTS:  delta               deviation measure from flat histogram
%
% !REVISION HISTORY:
% 2015-04 - Lars Nerger - Initial code
% Author:   L. Nerger (Alfred Wegener Institute, Bremerhaven, Germany)


  if element>0 && element<dim

    % Increment histogram for single element
    
    % find rank to increment
    rank = 0;
    for i = 1:dim_ens
      if ens(element, i) < state(element)
        rank = rank + 1;
      end
    end

    % increment histogram
    hist(rank+1) = hist(rank+1) + 1;

  elseif element == 0

    % Increment histogram over all elements
    for elem = 1:dim
  	  rank = 0;
      for i = 1: dim_ens
        if ens(elem,i) < state(elem)
           rank = rank + 1;
        end
      end
      
      hist(rank+1) = hist(rank+1) + 1;
    end

  end

% *************************************************************
% *** Compute delta (deviation from ideally flat histogram) ***
% *************************************************************

  if element>0 && element<dim

    % delta for histogram for single element
    delta = 0.0
	for elem = 1:dim_ens+1
      delta = delta + (hist(elem) - ncall/(dim_ens+1))^2;
    end
    delta = delta * (dim_ens+1) / (ncall * dim_ens);

  elseif element == 0 

    % delta for histogram over all elements
	delta = 0.0;
	for elem = 1:dim_ens+1
        delta = delta + (hist(elem) - (ncall*dim)/(dim_ens+1))^2;
    end
	delta = delta * (dim_ens+1) / (ncall*dim_ens);

  end
