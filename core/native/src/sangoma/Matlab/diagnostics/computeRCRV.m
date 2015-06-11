function [b,d]=computeRCRV(ens,ana,sig0,missing,m,nens)

%-----------------------------------------------------------------------------
% [b,d]=computeRCRV(ens,ana,sig0,missing,m,nens)
%
% Check the indistinguishability between the observations and 
% the ensemble members : reliability.
%
% INPUTS:  m                     Size of verification set
%          nens                  Size of each ensemble
%          ens(m,nens)           Ensemble of anomalies
%          ana(m)                Analysis (or observation) of anomaly
%          sig0                  obs. error
%          missing(m)            0 : the observation is OK
%                                1 : the observation should not be used
%
% OUTPUTS: b & d                 mean & std of y
%
% Method:
%   Computed are the Mean and Standard Deviation of the variable:
%            y=(o-xm)/SQRT(xs^2+so^2)
% WITH     o: obs, xm: ens. mean, xs: ens. spread, so: obs. error
%
%    Mean of y : b = weighted bias of the ensembles
%    S-D  of y : d = agreement between the mean error and the ensemble SPREAD 
%                     (+ obs. error)
%    
%    A perfectly reliable system gives : b = 0 and d = 1
% 
% Code based on Fortran implementation by G. Candille
%-----------------------------------------------------------------------------

  y = zeros(m);
  d=0.0;

% initialize missing
  ngood=0;
  for k = 1:m
    if (missing(k)==0) 
       ngood=ngood+1;
    end
  end

  if ngood>1
    ww=zeros(m);
    for k = 1:m
       ww(k) = 1.0/ngood;
    end
% define y (RCRV) and compute b (bias)
    b=0.0;
    for k = 1:m
      if missing(k)==0
%       xmk & xsk : ensemble mean & ensemble spread
        xmk=0.0;
        xsk=0.0;
        for i=1:nens
          xmk=xmk+ens(k,i);
        end
        xmk=xmk/nens;
        for i = 1:nens
            xsk=xsk+(xmk-ens(k,i))^2;
        end
        xsk=sqrt(xsk/(nens-1));
        if xsk^2 + sig0^2 == 0.0
            y(k)=ana(k)-xmk;
        else
            y(k)=(ana(k)-xmk)/sqrt(xsk^2+sig0^2);
        end
        b=b+y(k)*ww(k);
      end
    end
    
% compute d (dispersion)
    d=0.0;
    for k = 1:m
        if missing(k)==0
            d=d+ww(k)*(b-y(k))^2;
        end
    end
    d = sqrt(ngood * d / (ngood-1));
  else
      b=0.0;
      d=-2.0;
  end