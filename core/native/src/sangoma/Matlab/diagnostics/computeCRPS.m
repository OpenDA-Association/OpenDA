function [CRPS,RELI,RESOL,UNCERT,BB,AA]=computeCRPS(ENS,ANA,missing,NCASES,NENS)

%-----------------------------------------------------------------------------
% [CRPS,RELI,RESOL,UNCERT,BB,AA]=computeCRPS(ENS,ANA,MISSING,NCASES,NENS)
%
% Exact evaluation of the Continuous Ranked Probability Score,
% and its decomposition:
%                        CRPS = RELI + RESOL
%
% See H. Hersbach (2000) Wea. Forecasting, Vol 15, pp 559-570
% (note: RESOL here is equivalent to CRPS_pot = UNCERT - RESOL in Hersbach 2000)
%
% Code based on Fortran implementation by G. Candille
%
%  CRPS: global score evaluating both reliability (RELI) and resolution (RESOL) of the system
%        A perfectly reliable system gives RELI = 0
%        An informative system gives RESOL << UNCERT
%        (UNCERT is the value of the score based on the verification sample only)
%
%
%     Input:   NCASES                 Size of verification set
%              NENS                   Size of each ensemble
%              ENS(NCASES,NENS)       Ensemble of anomalies
%              ANA(NCASES)            Analysis of anomaly
%              missing(ncases)        0 : the observation is OK
%                                     1 : the observation should
%                                         not be used
%
%     Output:  CRPS                  CRPS & decomposition based on sample set of ANA
%              RELI   
%              RESOL
%              UNCERT            
%              BB(1:NENS), AA(1:NENS)  Detailed info on CRPS
%
%-----------------------------------------------------------------------------


%%  1. Initialize
      for i=1:NENS
         BB(i)=0.0;
         AA(i)=0.0;
      end
      BB0 = 0.0;
      AA0 = 0.0;
      OI = 0;

      CRPS=0.0;
      RELI=0.0;
      RESOL=0.0;
      UNCERT=0.0;

%     re-initialize weight depending on the missing values
      ngood=0;
      for icase=1:NCASES
         if missing(icase)==0 
             ngood=ngood+1;
         end
      end
      
      if ngood ~= 0
         ww = zeros(NCASES,1);
         for icase=1:NCASES
            ww(icase)=1.0/ngood;
         end

         ngood=0;
%%     2. Loop over cases, determine relevant quantities
         for ICASE=1:NCASES
            if missing(icase)==0
                ngood=ngood+1;
                W=ww(ICASE);
                A=ANA(ICASE);
                for i=1:NENS
                    E(i)=ENS(ICASE,i);
                end
                Esort=sort(E);
                E=Esort;
               if A<E(1) 
                   AA0=AA0+W*(E(1)-A);
               end
               if A<E(1)
                   BB0=BB0+W;
               end
               for I=1:NENS-1
                  BB(I)=BB(I)+W*max(min(A,E(I+1))-E(I),0.);
                  AA(I)=AA(I)+W*max(E(I+1)-max(A,E(I)),0.);
               end
               if A>E(NENS)
                   BB(NENS)=BB(NENS)+W*(A-E(NENS));
               end
               if A>E(NENS)
                   AA(NENS)=AA(NENS)+W;
               end

%     Update climate set
               ACLI(ngood)=A;
               WCLI(ngood)=W;
            end
         end

%%     3. Reinterpretation of BB(0) and AA(NENS)
         if BB0~=0.0 
             BB0=AA(0)*(1.0/BB0-1.0);
         end
         if AA(NENS)~=0.0
             AA(NENS)=BB(NENS)*(1.0/AA(NENS)-1.0);
         end
%     (Note that these do not contribute to the CRPS)

%%     4. Calculate uncertainty
         [acli_sort,isort]=sort(ACLI);
         wcli_sort = WCLI(isort);
         ACLI = acli_sort;
         WCLI = wcli_sort;
         
         UNCERT=0.0;
         P=0.0;
         for ICASE=1:ngood-1
             P=P+WCLI(ICASE);
             UNCERT=UNCERT+(ACLI(ICASE+1)-ACLI(ICASE))*P*(1.0-P);
         end

%%     5. Compute CRPS, reliability and resolution
         CRPS=0.0; 
         RELI=0.0;
         RESOL=0.0;

         for I=0:NENS
            if I~=0
                GI=BB(I)+AA(I);
            else
                GI=BB0+AA0;
            end
            if GI~=0.0 
                if I~=0
                    OI=AA(I)/GI;
                else
                    OI=AA0/GI;
                end
            end
            P=(I+0.0)/NENS;

            if I~=0
                CRPS = CRPS+BB(I)*P*P+AA(I)*(1.-P)*(1.-P);
            else
                CRPS = CRPS+BB0*P*P+AA0*(1.-P)*(1.-P);
            end
            RELI = RELI + GI*(OI-P)^2;
            RESOL= RESOL+ GI*OI*(1.-OI);
         end
         
      else
         CRPS=0.0; 
         RELI=0.0; 
         RESOL=0.0; 
         UNCERT=0.0;
      end
