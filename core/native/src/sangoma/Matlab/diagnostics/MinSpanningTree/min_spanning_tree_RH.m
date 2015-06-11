function mstRankTotal=min_spanning_tree_RH(N,n,p,xp,y,scaleLog,biasLog)

%--------------------------------------------
% [mstRank]=min_spanning_tree_RH(N,n,p,xp,y,scaleLog,biasLog)
%
% Calculates minimum spanning tree rank histogram for given forecast (xp)
% and observation (y) pairs to verify if observations are statisitically
% from the same (or not) distribution as ensembles. 
% 
%
% INPUTS:   N  - scalar, number of particles
%           n - a number of state variables which are observed
%	        at a single spatial location and specific time 
%               (e.g. if T and SSH is observed then n = 2)
%           p - number of observation/forecast sets in time
%           xp - ensemble element matrix of n by N by p
%           y  - observation array of n by p elements        
%           biasLog [optional] -  if given and true ensemble bias will 
%                                 be calculated and corrected. 
% 				  Note that care needs
%				  to be taken, e.g. nonhomogeneous bias
%				  characteristics of variables such as 
%				  winter and summer seasons. 
%	    scaleLog [optiona] -  if 'true' will scale ensemble if 'false'
%				  will not scale ensemble
% 
% OUTPUT:   mstRank - array with N+1 elements (bins) for MST rank histogram
%
% METHOD:   Minimum spaning tree (MST) rank histogram computes initial MST
%            (i.e. the minimum distance between all ensemble members in 
%            given multi-dimensional space) and then computes N ensemble MSTs 
%            by replacing each ensemble member in turn with the given observation.
%            The value for MSTRH is between 0 and 1 given as a fraction of 
%            ensemble MSTs which were smaller than than the initial MST, e.g. if
%            5 ensemble MSTs were smaller than initial MST and N=10 then 
%            its rank is 5 out of 11. 
%
%            NOTE: 
%                 1) rank is computed for one space and time location only.
%                 2) nr of ensemble variables have to be the same as nr of
%                    observation variables! 
            
% Date:     27/04/2015
% Author:   Sanita Vetra-Carvalho (Univeristy of Reading)

[nx,Nx,pX] = size(xp); % nx - number of variables
                       % Nx - number of particles
                       % px - number of forecasts 

[ny,py] = size(y); % ny - number of state space variables;
                   % py - number of particles.

% Checking that y and xp have the same number of variables
if (nx ~= ny) && (nx == n)
     	disp(['Number of space variable: ', num2str(nx)]);
        disp(['Number of observation variables: ', num2str(ny)]);
	return;
endif 

% Checking that ensemble number is larger than 1
if (Nx ~= N)
     	disp(['Nr of ensemble members in state vector xp (',num2str(Nx),') is not the same as N (', num2str(N)],')');
	return;
endif 

% Checking that there is only one overvation vector given
if (py ~= p)
     	disp(['Observatio array y has wrong dimentions (', num2str(ny),' by ', num2str(py),')']);
	return;
endif 

% If bias correction is used calculate biasTerm
if (nargin == 7 && biasLog == 'true')
  biasTerm = ensBias(n,N,p,xp,y);
end

 
mstRankTotal = zeros(N+1,1);


for k = 1:p % go over all obs/forecast sets
disp(['main loop, k = ', num2str(k)]);
fflush(stdout)

mstRank = 1; % count MSTs
        ensMatr = squeeze(xp(:,:,k));

	mst = zeros(Nx+1); % array to store all the MSTs, note mst(1) is the 'initial MST'
	yx = zeros(ny);


	%--------------------------%
	% CORRECT & SCALE ENSEMBLE %
	%--------------------------%
	% Correct ensemble bias
	if (nargin == 7 && biasLog == 'true')
	  ensMatr = ensMatr - biasTerm;
	end

	% Scale ensemble:
	% Scale ensemble so that variables with small variances are 
	% taken into an account in the skill score
	if (nargin == 6 && scaleLog == 'true')
	  S = zeros(nx,nx);
	  ensPert = zeros(nx,Nx);

	  ensMean = mean(ensMatr,2); % ensemble mean
	  yx = y - ensMean;

	  for j = 1:N
	    ensPert(:,j) = ensMatr(:,j) - ensMean;
	  end
	  S = 1/N*( yx*yx' + ensPert*ensPert');

	  [V,L] = eig(S);
	  S_half = V*diag(diag(L).^(-1/2))*V';

	  % transform observation
	  y = S_half*(y-ensMean);

	  for j = 1:N
	    ensMatr(:,j) = S_half*(ensMatr(:,j) - ensMean);
	  end
	end


	%-----------------%
	% GET INITIAL MST %
	%-----------------%
	% Minimum distance between all ensemble members such that the 
	% network does not contain any closed loops to get the initial MST
	A = euclidDist(ensMatr); % compute Euclidian distances between ensemble variables. 
	[mst(1),AA] = min_span_tree(A,3); % supply square distances and return only total 
				      % minimum spanning tree Euclidian distance (dim2 =1)


	%------------------%
	% RANK OBSERVATION %
	%------------------%
	for j = 1:N
	  xp_obs = ensMatr;
	  xp_obs(:,j) = y(:,k);
	  A = euclidDist(xp_obs); % compute square sums of distances between ensemble variables. 
	  [mst(j+1),AA1] = min_span_tree(A,3); % supply square distances and return only total 
				      % minimum spanning tree Euclidian distance (dim2 =1)

	  % check if the current mst is lower than the initial MST, if it is
	  % then inclrease the rank for this obervation.
	  if (mst(j+1) < mst(1))
	    mstRank = mstRank+1;
	  end
	end


	mstRankTotal(mstRank) = mstRankTotal(mstRank) + 1; % normalise the rank 
end % for k = 1:p over all obs/forecast sets



