function [ MatOut ] = DA_Cell2Mat( MatIn )
%DA_Cell2Mat A Matlab Cell structure to Matrix. Specific use.
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com) 2012
%
%   Inputs: 
%       -  A Cell data structure
%   Outputs:
%       -  The data in a 3 x n matrix
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
num = size(MatIn,1);
n = size(MatIn{1},1);
m = size(MatIn{1},2);

MatOut = zeros(n,m,num);
for i=1:num
    MatOut(:,:,i) = MatIn{i};
end



end

