function[arrayOut] = MR_cells2array(cellIn)
%% MR_cells2array puts all the double arrays within cells to a large array
% goes down the cells then accross.

totalSize = 0;
for y = 1:size(cellIn,1)
   for x = 1:size(cellIn,2)
        totalSize = totalSize + size(cellIn{y,x},1);
   end
end

% create array
arrayOut = nan(1,totalSize);

% fills the array with data
pos = 1;
for y = 1:size(cellIn,1)
   for x = 1:size(cellIn,2)
        arrayOut(pos:pos+size(cellIn{y,x},1)-1) = cellIn{y,x};
        pos = pos+size(cellIn{y,x},1);
   end
end


end