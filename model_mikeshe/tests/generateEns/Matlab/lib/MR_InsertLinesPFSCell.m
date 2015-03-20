function [ Cnew ] = MR_InsertLinesPFSCell( C, insertAt, numToInsert )
%MR_InsertLinesPFSCell Insert blank lines into a cell array
%   Insert blank lines into a cell array. Slow because the celles are
%   copied over each other. Only works for 1D cell array.

n = size(C,1);
nnew = n + numToInsert;
Cnew = cell(nnew,1);
Cnew(1:insertAt-1 , 1) = C(1:insertAt-1 , 1);
Cnew(insertAt+numToInsert:nnew , 1) = C(insertAt:n , 1);

end

