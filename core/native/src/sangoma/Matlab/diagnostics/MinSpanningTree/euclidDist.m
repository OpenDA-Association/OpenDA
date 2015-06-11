function distA = euclidDist(A)

%---------------------------------------------------------------%
% distA = euclidDist(A)
%
% A routine to compute Euclidian distance between all column
% vectors in A. 
% 
%       
% Date:     27/04/2015
% Author:   Sanita Vetra-Carvalho (Univeristy of Reading)
%---------------------------------------------------------------%

[n,N] = size(A);
distA = zeros(N,N);
counter = 0;

for j1 = 1:N
  for j2 = 1:N
    if (j2 > j1)
      for i = 1:n
        distA(j1,j2) = distA(j1,j2) + (A(i,j1) - A(i,j2))^2;
      end
      distA(j2,j1) = sqrt(distA(j2,j1));
      distA(j2,j1) = distA(j1,j2);
      counter = counter +1;
    end
  end
end

% to check if correct number of calculations have been done!
%disp(['counter in euclidDist: ',num2str(counter)]);
