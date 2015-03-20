function [ lower , upper ] = MR_ConfidanceInterval( data , fraction )
%MR_ConfidanceInterval Calculates the confiance interval of data with
%unknown distribution. 
%Sorts data then discards the (1-CI)/2 upper and lower points.
%Data is an m x t matrix where m number of ensembles with time vector t.
%EXTENDED: Note Now can be a m x t x n  matrix where n is another variable.
% The routine sorts the data. If we are looking for the .95 confidance of
% 200 members then return the lower=6 and upper=200-5 th sorted at each t. 

% Written by Marc Ridler (MER)  @ mer@dhigroup.com


s = sort(data);
nm = size(data,1);
nt = size(data,2);

% Remove nan
s = s(isfinite(s(:,1)),:);
% if 3D -- then reshape
if size(size(data),2) == 3
    nvar = floor( size(s,2) / nt );
    s = reshape(s, nm, nt, nvar ); 
end

nm = size(s,1);
numDiscard = floor( (1-fraction)/2 * nm );

lower = squeeze( s(numDiscard+1,:,:) );
upper = squeeze( s(nm-numDiscard,:,:));



end

