function MI=mutual_information(dim_sample,dim_ens,dim_obs,obs,R,H,states,method,prior_w)

%--------------------------------------------
% MI=mutual_information(M,N,p,n,y,R,H,xp,method,prior_w)
%
% Calculates mutual information within a partical filter, uses the 
% assumption that the likelihood is Gaussian.
% To be used after weights have been updated by the observations.
%
% INPUTS:   dim-sample - scalar, sample size for observation space. 
%	    dim_ens - number of ensemble members
%	    dim_obs - number of spatial observations at a current time
%           obs - vector size dim_obs, observations at current time
%           R - square matrix of size dim_obs, the observation error covariance
%           matrix.
%           H - a matrix of size dim_obs by n (size of state space), the (linear) observation operator.
%           states - matrix of size n by dim_ens, the particle values.
%           method - string, you have a choice of method either 'quad'
%           which solves the integral using a quardature method or 'rndm'
%           which solve the integral using a random sampling method.
%           prior_w (optional) - vector of size dim_ens, prior weights. If not
%           explicitly given these are assumed to be 1/dim_ens
% 
% OUTPUT:	MI - scalar, mutual information.
%
% METHOD:   Mutual information is the relative entropy averaged over
% observation space, MI=int(RE*p(y))dy. Where p(y)=int(p(y|x)*p(x))dy, 
% given approximately by the sum of the posterior weights. MI can then be 
% approximated in two ways: 
%   1) Quadrature: Discretise ob space into M points. 
%       MI=sum_{i=1}^{M}(RE_i*p(y)_i)*Dy.
%   2) random sampling: Sample M random points from p(y|x). 
%       MI=sum_i(RE_i*p(y)_i)
%
% Date:     12/10/2012
% Author:   A. M. Fowler (Univeristy of Reading)
%--------------------------------------------

% if prior_w not given set to 1/dim_ens
if nargin ~=10
    prior_w(1:dim_ens)=1/dim_ens;
end
%---
%initialise variables
MI=0;
re_y=zeros(dim_sample,1);
p_y=zeros(dim_sample,1);
wpp=zeros(dim_sample,dim_ens);

if strcmp(method,'quad')
    % use quadrature method. only suitable for small observation space
    if p>1
        display('ob space too large for quadrature method, use random sampling method instead');
            return
    end
    yi=zeros(dim_obs,dim_sample);
    Dy=sqrt(diag(R))/(dim_sample/20); % the value of MI may be sensitive 
                                    %to the choise of sampling
    for i=1:dim_sample
        % generate observations to sample over
        yi(1,i)=obs(1)-(dim_sample/2-i)*Dy(1);
                
        % calculate weights given obs yi(i,:)
        for j=1:N
            p_y_x=(((2*pi)^(dim_obs))*det(R))^(-0.5)*...
                    exp(-0.5*(H*states(:,j)-yi(:,i))'*...
                    inv(R)*(H*states(:,j)-yi(:,i))); % likelihood
            wpp(i,j)=p_y_x*prior_w(j);
        end
        p_y(i)=sum(wpp(i,:)); % marginal distributiom
        wpp(i,:)=wpp(i,:)/p_y(i);
        re_y(i)=0; % relative entropy as a function of ob
        for j=1:dim_ens
            if wpp(i,j)/prior_w(j)>0
                re_y(i)=re_y(i)+wpp(i,j)*...
                    log(wpp(i,j)/prior_w(j));
            end
        end
        MI=MI+p_y(i)*re_y(i)*Dy;
        
    end
   
elseif strcmp(method,'rndm')
    yi=zeros(dim_obs,dim_sample);
    randn('state',0);
    for i=1:dim_sample
        % generate observations to sample over
        % sample dim_sample particles from likelihood
        yi(:,i)=obs(:)+sqrt(R)*randn(dim_obs,1);
        %calculate weights
        for j=1:dim_ens
            p_y_x=(((2*pi)^(dim_obs))*det(R))^(-0.5)*...
                exp(-0.5*(H*states(:,j)-yi(:,i))'*...
                inv(R)*(H*states(:,j)-yi(:,i))); % likelihood
            wpp(i,j)=p_y_x*prior_w(j);
        end
        p_y(i)=sum(wpp(i,:)); % marginal distributiom
        wpp(i,:)=wpp(i,:)/p_y(i);
        re_y(i)=0; % relative entropy as a function of ob
        for j=1:dim_ens
            if wpp(i,j)/prior_w(j)>0
                re_y(i)=re_y(i)+wpp(i,j)*...
                    log(wpp(i,j)/prior_w(j));
            end
        end
    end
        p_y=p_y/sum(p_y);
        MI=sum(p_y.*re_y);
end

% Copyright (C) 2012 Alison Fowler <a.m.fowler@reading.ac.uk>
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; If not, see <http://www.gnu.org/licenses/>.
