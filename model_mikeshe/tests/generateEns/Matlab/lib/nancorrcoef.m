function [R,sig,ci1,ci2,nan_sig] = nancorrcoef(X,Y,Mode);
% CORRCOEF calculates the correlation coefficient.
%   The input data can contain missing values encoded with NaN.
%   Missing data (NaN's) are handled by pairwise deletion [15]. 
%   In order to avoid possible pitfalls, use case-wise deletion or 
%   or check the correlation of NaN's with your data (see below). 
%   A significance test for testing the Hypothesis  
%   "correlation coefficient R is significantly different to zero" 
%   is included. 
%
% [...] = CORRCOEF(X [,Mode]);
%      calculates the (auto-)correlation matrix of X
% [...] = CORRCOEF(X,Y [,Mode]);
%      calculates the crosscorrelation between X and Y
%
%       Mode='Pearson' or 'parametric' [default]
%       	gives the correlation coefficient  
%       	also known as the "product-moment coefficient of correlation" 
%               or "Pearson's correlation" [1]
%       Mode='Spearman' 	gives "Spearman's Rank Correlation Coefficient"
%	        This replaces SPEARMAN.M
%       Mode='Rank' 		gives a nonparametric Rank Correlation Coefficient
%	        This replaces RANKCORR.M
%
% [R,p,ci1,ci2,nansig] = CORRCOEF(...);
% 	R is the correlation matrix
%	R(i,j) is the correlation coefficient r between X(:,i) and Y(:,j)
%  p    gives the significance of R
%	It tests the null hypothesis that the product moment correlation coefficient is zero 
%       using Student's t-test on the statistic t = r sqrt(N-2)/sqrt(1-r^2) 
%       where N is the number of samples (Statistics, M. Spiegel, Schaum series).
%  p > alpha: do not reject the Null hypothesis: "R is zero".
%  p < alpha: The alternative hypothesis "R2 is larger than zero" is true with probability (1-alpha).
%  ci1	lower 0.95 confidence interval 
%  ci2	upper 0.95 confidence interval 
%  nan_sig 	p-value whether H0: "NaN's are not correlated" could be correct

%       if nan_sig < alpha, H1 ("NaNs are correlated") is very likely. 
% 

% The result is only valid if the occurence of NaN's is uncorrelated. In
% order to avoid this pitfall, the correlation of NaN's should be checked 
% or case-wise deletion should be applied. 
%   Case-Wise deletion can be implemented 
%    ix = ~any(isnan([X,Y]))
%    [...] = CORRCOEF(X(ix,:),Y(ix,:),...) 
%
%  Correlation (non-random distribution) of NaN's can be checked with 

%       [nan_R,nan_sig]=corrcoef(X,isnan(X))

%   or  [nan_R,nan_sig]=corrcoef([X,Y],isnan([X,Y]))

%   or  [R,p,ci1,ci2] = CORRCOEF(...);
%
% Further recommandation related to the correlation coefficient 
% + LOOK AT THE SCATTERPLOTS!
% + Correlation is not causation. The observed correlation between two variables 
%	might be due to the action of other, unobserved variables.
%
% see also: SUMSKIPNAN, COVM, COV, COR, SPEARMAN, RANKCORR, RANKS,
%       PARTCORRCOEF
%
% REFERENCES:
% on the correlation coefficient 
% [ 1] http://mathworld.wolfram.com/CorrelationCoefficient.html
% [ 2] http://www.geography.btinternet.co.uk/spearman.htm
% [ 3] Hogg, R. V. and Craig, A. T. Introduction to Mathematical Statistics, 5th ed.  New York: Macmillan, pp. 338 and 400, 1995.
% [ 4] Lehmann, E. L. and D'Abrera, H. J. M. Nonparametrics: Statistical Methods Based on Ranks, rev. ed. Englewood Cliffs, NJ: Prentice-Hall, pp. 292, 300, and 323, 1998.
% [ 5] Press, W. H.; Flannery, B. P.; Teukolsky, S. A.; and Vetterling, W. T. Numerical Recipes in FORTRAN: The Art of Scientific Computing, 2nd ed. Cambridge, England: Cambridge University Press, pp. 634-637, 1992
% [ 6] http://mathworld.wolfram.com/SpearmanRankCorrelationCoefficient.html
% on the significance test of the correlation coefficient
% [11] http://www.met.rdg.ac.uk/cag/STATS/corr.html
% [12] http://www.janda.org/c10/Lectures/topic06/L24-significanceR.htm
% [13] http://faculty.vassar.edu/lowry/ch4apx.html
% [14] http://davidmlane.com/hyperstat/B134689.html
% [15] http://www.statsoft.com/textbook/stbasic.html#Correlationsk
% others
% [20] http://www.tufts.edu/~gdallal/corr.htm

%       $Revision: 1.31 $
%       $Id: corrcoef.m,v 1.31 2005/05/25 02:35:36 pkienzle Exp $
%       Copyright (C) 2000-2004 by Alois Schloegl <a.schloegl@ieee.org>	
%       This function is part of the NaN-toolbox
%       http://www.dpmi.tu-graz.ac.at/~schloegl/matlab/NaN/

%    This program is free software; you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation; either version 2 of the License, or
%    (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with this program; if not, write to the Free Software
%    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

% Features:
% + handles missing values (encoded as NaN's)
%       + pairwise deletion of missing data
%       + checks independence of missing values (NaNs) 
% + parametric and non-parametric (rank) correlation
%       + Pearson's correlation
%       + Spearman's rank correlation
%       + Rank correlation (non-parametric, non-Spearman)
% + is fast, using an efficient algorithm O(n.log(n)) for calculating the ranks
% + significance test for null-hypthesis: r=0 
% + confidence interval included
% - rank correlation works for cell arrays, too (no check for missing values).
% + compatible with Octave and Matlab


NARG = nargout;	% needed because nargout is not reentrant in Octave

if nargin==1
        Y = [];
        Mode='Pearson';
elseif nargin==0
        fprintf(2,'Error CORRCOEF: Missing argument(s)\n');
elseif nargin==2
        if ~isnumeric(Y)
                Mode=Y;
                Y=[];
        else
                Mode='Pearson';
        end;
end;        
Mode=[Mode,'        '];


FLAG_WARNING = warning;		% save warning status

warning('off');


[r1,c1]=size(X);
if ~isempty(Y)
        [r2,c2]=size(Y);
        if r1~=r2,
                fprintf(2,'Error CORRCOEF: X and Y must have the same number of observations (rows).\n');
                return;
        end;
        NN = real(~isnan(X)')*real(~isnan(Y));
else
        [r2,c2]=size(X);
        NN = real(~isnan(X)')*real(~isnan(X));  
end;

%%%%% generate combinations using indices for pairwise calculation of the correlation
YESNAN = any(isnan(X(:))) | any(isnan(Y(:)));
if isempty(Y),
        IX = ones(c1)-diag(ones(c1,1));
        [jx, jy ] = find(IX);
        [jxo,jyo] = find(IX);
	R = eye(c1);        
else
        IX = zeros(c1+c2);
        IX(1:c1,c1+(1:c2)) = 1;
        [jx,jy] = find(IX);
        
        IX = ones(c1,c2);
        [jxo,jyo] = find(IX);
	R = repmat(nan,c1,c2);
end;  

if strcmp(lower(Mode(1:7)),'pearson');
        % see http://mathworld.wolfram.com/CorrelationCoefficient.html
	if ~YESNAN,
                [S,N,SSQ] = sumskipnan(X,1);
                if ~isempty(Y),
	                [S2,N2,SSQ2] = sumskipnan(Y,1);
                        CC = X'*Y;
                        M1 = S./N;
                        M2 = S2./N2;
                        cc = CC./NN - M1'*M2;
                        R  = cc./sqrt((SSQ./N-M1.*M1)'*(SSQ2./N2-M2.*M2));
                else        
                        CC = X'*X;
                        M  = S./N;
                        cc = CC./NN - M'*M;
                        v  = SSQ./N - M.*M; %max(N-1,0);
                        R  = cc./sqrt(v'*v);
                end;
        else
                if ~isempty(Y),
                        X  = [X,Y];
                end;  
                for k = 1:length(jx),
                        %ik = ~any(isnan(X(:,[jx(k),jy(k)])),2);
                        ik = ~isnan(X(:,[jx(k)])) & ~isnan(X(:,[jy(k)]));
                        [s,n,s2] = sumskipnan(X(ik,[jx(k),jy(k)]),1);
                        v  = (s2-s.*s./n)./n;
                        cc = X(ik,jx(k))'*X(ik,jy(k));
                        cc = cc/n(1) - prod(s./n);
                        %r(k) = cc./sqrt(prod(v));
                        R(jxo(k),jyo(k)) = cc./sqrt(prod(v));
                end;
        end
        
elseif strcmp(lower(Mode(1:4)),'rank');
        % see [ 6] http://mathworld.wolfram.com/SpearmanRankCorrelationCoefficient.html
	if ~YESNAN,
                if isempty(Y)
	                R = corrcoef(ranks(X));
                else
                        R = corrcoef(ranks(X),ranks(Y));
                end;
        else
                if ~isempty(Y),
                        X = [X,Y];
                end;  
                for k = 1:length(jx),
                        %ik = ~any(isnan(X(:,[jx(k),jy(k)])),2);
                        ik = ~isnan(X(:,[jx(k)])) & ~isnan(X(:,[jy(k)]));
                        il = ranks(X(ik,[jx(k),jy(k)]));
                        R(jxo(k),jyo(k)) = corrcoef(il(:,1),il(:,2));
                end;
		X = ranks(X);
        end;
        
elseif strcmp(lower(Mode(1:8)),'spearman');
        % see [ 6] http://mathworld.wolfram.com/SpearmanRankCorrelationCoefficient.html
        if ~isempty(Y),
                X = [X,Y];
        end;  
        
        n = repmat(nan,c1,c2);
        
        if ~YESNAN,
                iy = ranks(X);	%  calculates ranks;
		                
                for k = 1:length(jx),
                        [R(jxo(k),jyo(k)),n(jxo(k),jyo(k))] = sumskipnan((iy(:,jx(k)) - iy(:,jy(k))).^2);	% NN is the number of non-missing values
                end;
        else
                for k = 1:length(jx),
                        %ik = ~any(isnan(X(:,[jx(k),jy(k)])),2);
                        ik = ~isnan(X(:,[jx(k)])) & ~isnan(X(:,[jy(k)]));
                        il = ranks(X(ik,[jx(k),jy(k)]));
                        % NN is the number of non-missing values
                        [R(jxo(k),jyo(k)),n(jxo(k),jyo(k))] = sumskipnan((il(:,1) - il(:,2)).^2);
                end;
		X = ranks(X);
        end;
        R = 1 - 6 * R ./ (n.*(n.*n-1));

        
elseif strcmp(lower(Mode(1:7)),'partial');
        fprintf(2,'Error CORRCOEF: use PARTCORRCOEF \n',Mode);
        
        return;
        
elseif strcmp(lower(Mode(1:7)),'kendall');
        fprintf(2,'Error CORRCOEF: mode ''%s'' not implemented yet.\n',Mode);
        
        return;
else
        fprintf(2,'Error CORRCOEF: unknown mode ''%s''\n',Mode);
end;

if (NARG<2), 
	warning(FLAG_WARNING); 	% restore warning status

        return;
end;


% CONFIDENCE INTERVAL
if exist('flag_implicit_significance')==2,
        alpha = flag_implicit_significance;
else
	alpha = 0.01;        
end;
% fprintf(1,'CORRCOEF: confidence interval is based on alpha=%f\n',alpha);


% SIGNIFICANCE TEST
tmp = 1 - R.*R;
tmp(tmp<0) = 0;		% prevent tmp<0 i.e. imag(t)~=0 
t   = R.*sqrt(max(NN-2,0)./tmp);

if exist('t_cdf')>1;
        sig = t_cdf(t,NN-2);
elseif exist('tcdf')>1;
        sig = tcdf(t,NN-2);
else
        fprintf('CORRCOEF: significance test not completed because of missing TCDF-function\n')
        sig = repmat(nan,size(R));
end;
sig  = 2 * min(sig,1 - sig);


if NARG<3, 
	warning(FLAG_WARNING); 	% restore warning status

        return;
end;


tmp = R;
%tmp(ix1 | ix2) = nan;		% avoid division-by-zero warning
z   = log((1+tmp)./(1-tmp))/2; 	% Fisher's z-transform; 
%sz = 1./sqrt(NN-3);		% standard error of z
sz  = sqrt(2)*erfinv(1-2*alpha)./sqrt(NN-3);		% confidence interval for alpha of z

ci1 = tanh(z-sz);
ci2 = tanh(z+sz);

%ci1(isnan(ci1))=R(isnan(ci1));	% in case of isnan(ci), the interval limits are exactly the R value 
%ci2(isnan(ci2))=R(isnan(ci2));

if (NARG<5) | ~YESNAN, 
        sig_nan = [];

	warning(FLAG_WARNING); 	% restore warning status

        return;
end;



%%%%% ----- check independence of NaNs (missing values) -----
%[nan_R,nan_sig,nan_ci1,nan_ci2] = corrcoef(X,isnan(X))
[nan_R, nan_sig] = corrcoef(X,isnan(X));

% remove diagonal elements, because these have not any meaning %
nan_sig(isnan(nan_R)) = nan;

if any(nan_sig(:) < alpha),

        tmp = nan_sig(:);			% Hack to skip NaN's in MIN(X)

        min_sig = min(tmp(~isnan(tmp))); 	% Necessary, because Octave returns NaN rather than min(X) for min(NaN,X) 

        fprintf(1,'CORRCOFF Warning: Missing Values (i.e. NaNs) are not independent of data (p-value=%f)\n', min_sig);
        fprintf(1,'   Its recommended to remove all samples with any missing value (NaN).\n');
        fprintf(1,'   In the following combinations the null-hypotheses (NaNs are uncorrelated) must be rejected.\n');
        [ix,iy] = find(nan_sig < alpha);
        disp([ix,iy])
end;


%%%%% ----- end of independence check ------


warning(FLAG_WARNING); 	% restore warning status

return;