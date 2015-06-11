function [distMST,tree] = min_span_tree(A,m)

%---------------------------------------------------------------%
% [distMST,tree] = min_span_tree(A,m)
%
% This a routing that can build and return the minimum spanning 
% tree and/or the total distance between all nodes of the final 
% minimal spanning tree. 
% 
% 
% Prim's minimal spanning tree algorithm works as follows: 
%  * start at any node, find closest neighbor and mark edges
%  * for all remaining nodes, find the closest node to 
%    previous cluster and mark edge
%  * continue until no nodes remain
%
% INPUTS: 
%		A - symmetric matrix of distances between 
%                   nodes (adjacency matrix) 
%		m - type of output required:
%			* m = 1 will output only distMST
% 			* m = 2 will output only tree
%			* m = 3 will output distMST and tree.
%
% OUTPUTS: 
%		MST - Euclidian distance between all points in 
%		      the minimum spanning tree
%		tree - matrix specifying minimum spanning tree
%       
% Date:     27/04/2015
% Author:   Sanita Vetra-Carvalho (Univeristy of Reading)
%---------------------------------------------------------------%



n = length(A);     % number of nodes

if (m == 1) 
	distMST = 0;  % initialize Euclidian distance between all nodes
elseif (m == 2) 
	tree = zeros(n,n);   % initialize tree
elseif (m == 3)
	distMST = 0;  % initialize Euclidian distance between all nodes
        tree = zeros(n,n);   % initialize tree
else 
	disp('incorrect input in min_spanning_tree(A,m) funtion.');
	disp('type of output, m, has to be either 1,2 or 3, but ');
	disp('m = ',num2str(m));
	return;
end


A(find(A==0))=inf; % set all zeros in the matrix to inf

conn_nodes = 1;        % nodes part of the min-span-tree
notconn_nodes = [2:n];     % remaining nodes

while length(notconn_nodes)>0
  % find the absolute minimum in submatrix A(conn_nodes,notconn_nodes)
  [minlink]=min(min(A(conn_nodes,notconn_nodes)));

  % get the index of this minimum
  ind=find(A(conn_nodes,notconn_nodes)==minlink);

  % map from index ind(1) to subsript in the adjacency matrix
  [ind_i,ind_j] = ind2sub([length(conn_nodes),length(notconn_nodes)],ind(1));

  % get corresponding indecies of A matrix
  i=conn_nodes(ind_i); j=notconn_nodes(ind_j); 

  % Compute tree and/or total distance 
  if (m == 1) 
	distMST = distMST + A(i,j)^2;
  elseif (m ==2) 
	tree(i,j)=1; tree(j,i)=1;
  elseif (m == 3)
	distMST = distMST + A(i,j)^2;
	tree(i,j)=1; tree(j,i)=1;
  end

  % put added node into connected set and remove it from remaining set of nodes
  conn_nodes = [conn_nodes j];
  notconn_nodes = setdiff(notconn_nodes,j);
end

if (m == 1 || m == 3)
	distMST = sqrt(distMST); % square the distance to get Euclidian norm
end

