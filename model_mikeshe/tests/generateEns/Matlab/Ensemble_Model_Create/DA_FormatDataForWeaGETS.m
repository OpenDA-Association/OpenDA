function [dataO] = DA_FormatDataForWeaGETS(filename)
%UNTITLED Formats a dfs0 file so as to use in the WeaGets weather generator for
%the extraction of statistics.
%
% Modified,  Marc-Etienne Ridler  (mer@dhigroup.com) 2012
% CAREFUL - MUST START JANUARY 1ST

dtSec = 24*60*60;
[TimeVec , DataVec, itemname ] = DA_readDFS0_getMultipleTS(filename, dtSec);

%plot(TimeVec,DataVec)

% Find FIRST January 1st Index
jan1 = -99;
for i = 1:length(DataVec)
    
    [y, m, d] = datevec( TimeVec(i) );
    if m==1 && d ==1
        jan1 = i;
        break
    end
end

count = 1;
for i = 1:length(DataVec)
    
    [y, m, d] = datevec( TimeVec(i) );
    if m==2 && d ==29
        %remove leap
        rm(count) = i;
        count = count+1;
    end
end


% Remove Feb 29 indices
DataVec(rm) = [];

% Remove all dates before January 1st
DataVec(1:jan1) = [];

% formats it into a Matrix (number of years   x   365 days)
nYears = floor( length(DataVec)/365 );
numIdx = nYears * 365;

dataO = reshape( DataVec(1:numIdx) , nYears, 365 );

end

