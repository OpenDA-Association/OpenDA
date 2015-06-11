function [br, brc, brv, unc, pc, s, sunc, pp, g, pr]=computeBRIER(m, nens, ens, ana, xth) 

%---------------------------------------------------------------------------------
% [br, brc, brv, unc, pc, s, sunc, pp, g, pr]=computeBRIER(m, nens, ens, ana, xth) 
%
% This function computes scores related to 1 binary event associated 
% with 1 threshold vector xth (externally defined by the user).
% First, 'predicted' probabilities of occurrence of the event are computed
% with their distribution and their associated 'predictable' probabilities
% (these can be used to draw reliability and sharpness diagrams).
% Then, the Brier (skill) score and partition, and entropy, are computed:
%
%  B = E[(p-p'])^2] - E[(p'-pc])^2] + pc(1-pc) and BS = 1 - B/pc(1-pc)
%
%  S = -E[p'log(p')]
%
%    (see details in deliverable 4.1).
%
%    Input:  m                   size of verification set
%            nens                size of each ensemble
%            ens(m,nens)         ensemble
%            ana(m)              analysis or observation (verification)
%            xth(m)              threshold
%
%    Output: br                  Brier global skill score
%            brc                 reliability component
%            brv                 resolution component
%            unc                 uncertainty
%            pc                  'climatological' probability
%            s                   entropy
%            sunc                'climatological' entropy
%            pp(nens+1)          predicted probabilities
%            g(nens+1)           distribution of pp
%            pr(nens+1)          predictable probabilities
%
% Code based on Fortran implementation by G. Candille
%---------------------------------------------------------------------------------

  br=0.0; 
  brc=0.0; 
  brv=0.0; 
  pc=0.0;
  unc=0.0;
  s=0.0; 
  sunc=0.0;
  o = zeros(m,1);
  pr = zeros(1,nens+1);

%     define probabilities and observed occurrences
  for k=1:m
     xk=0.0;
     for i=1:nens
        if ens(k,i) < xth(k)
            xk=xk+1.0;
        end
     end
     p(k)=xk/nens;
     if ana(k) < xth(k) 
         o(k)=1.0;
     end
  end

%     uncertainty
  for k=1:m
     pc=pc+o(k);
  end
  pc=pc/m;
  unc=pc*(1.0-pc);

%     predictable probabilities
  for i=1:nens+1
     g(i)=0.0;
     dc=i-1;
     pp(i)=dc/nens;
     for k=1:m
        if p(k) == pp(i)
           g(i)=g(i)+1.0;
           pr(i)=pr(i)+o(k);
        end
     end
     if g(i) ~= 0.0 
        pr(i)=pr(i)/g(i);
     else
        pr(i)=0.0;
     end
     g(i)=g(i)/m;
  end

%     Brier skill score and decomposition
  for i=1:nens+1
     brc=brc+g(i)*(pp(i)-pr(i))^2;
     brv=brv+g(i)*(pr(i)-pc)^2;
  end
  brc=brc/unc;
  brv=1.0-brv/unc;
  br=1.0-(brc+brv);

%     Entropy
  for i=1:nens+1
     if pr(i) > 0.0
         s=s-g(i)*pr(i)*log(pr(i));
     end
  end
  sunc=-pc*log(pc);
