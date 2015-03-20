function [gP] = DA_GeneratePrecipitation(P, GeneratedYears)
%UNTITLED Part of th WeaGETS Weather Generator.
%
% Modified,  Marc-Etienne Ridler  (mer@dhigroup.com) 2012
%
% Markov-Chain 2nd Order
% Code from http://www.mathworks.com/matlabcentral/fileexchange/29136-stochastic-weather-generator-weagets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

MarkovChainOrder = 2;
PrecipThreshold = 0.1;

%P = load(filenamein);

idistr = 2
%
% we start the analysis of the precipitation time series
%
% generate matrix of precip - no precip (PnP)
%
[Y,D]=size(P);      % years, days

PnP=zeros(Y,D);
[k]=find(P>PrecipThreshold);        % finds days with significant precipitation
PnP(k)=1;           % a value of 1 means that significant precipitation occured
%
% adjust precipitation to threshold: substract threshold to precipitation
% values so to be able to use the 1-parameter exponential funtion of
% 2-parameter gamma funtion.  The threshold will be added back by the
% generator

[k2]=find(P<=PrecipThreshold & P>0); 
P(k2)=0;
P(k)=P(k)-PrecipThreshold;
%
% put NaN for missing values instead of -999
%
[kn]=find(P<0);
PnP(kn)=NaN;
%
%  calculate transition state a00, a01, a10, a11 (Woolhiser and Pegram, 1979)
%
% matrix 'a' is a [4 365] matrix that contains the number of years over Y
% where a00 (line 1) a01 (line 2) a10 (line 3) and a11 (line 4) was
% observed.  As such, the sum of each column of matrix 'a' is equal to Y
% (in fact it is equal to Y-Days_without_data)

[a] = WEAGETS_transition2(PnP,Y,D);
%
% calculate average p00 and p10 using maximum likelihood estimator
% on 14-days periods (Woolhiser and Pegram, 1979)
%
n14=round(D/14);
for i=1:n14
   A000(i)=sum(a(1,14*(i-1)+1:14*i));
   A001(i)=sum(a(2,14*(i-1)+1:14*i));
 
   A010(i)=sum(a(3,14*(i-1)+1:14*i));
   A011(i)=sum(a(4,14*(i-1)+1:14*i));
  
   A100(i)=sum(a(5,14*(i-1)+1:14*i));
   A101(i)=sum(a(6,14*(i-1)+1:14*i));
 
   A110(i)=sum(a(7,14*(i-1)+1:14*i));
   A111(i)=sum(a(8,14*(i-1)+1:14*i));
end

ap000=A000./(A001+A000);
ap001=1-ap000;

ap010=A010./(A010+A011);
ap011=1-ap010;

ap100=A100./(A101+A100);
ap101=1-ap100;

ap110=A110./(A110+A111);
ap111=1-ap110;

%
t=1:n14;
t=t';
T14=n14/(2*pi);
%
n=1:D;
T=D/(2*pi);
%
%  we now process the precipitation amounts
%  an exponential distribution (Richardson, 1981), or a 2-parameter Gamma
%  distribution is usede
%
%  we first create a precipitation matrice containing positive
% precipitation amounts when precipitation has been recorded and
% zero values (when there is no precipitation or when data is missing)
% otherwise.  This requires to replace NaN values in PnP matrice
% by 0 values.  Position of NaN values in vector kn
%
PnP(kn)=0;
PP=P.*PnP;
sPtot=sum(PP); %total precipitation amounts on given calendar day for all years analysed
stot=sum(PnP); % # of days with precip. on a given calendar day for all years analysed
sPtot2=sum(PP.^2);
%
%  select exponential or gamma distribution
%
if idistr == 1   % exponential distribution
    %
    %  mle of parameter lambda of the exponential distribution taken over 14-day periods
    %
    for i=1:n14
        alambda(i)=sum(stot(14*(i-1)+1:14*i))/sum(sPtot(14*(i-1)+1:14*i));
    end
    %
    nu=-999;       % just so that the variable is defined to avoid errors in the SAVE portion

elseif idistr == 2   %  2-parameter gamma distribution
   
    kzero=find(PP==0);
    PP(kzero)=1e-20;
    logPP=log(PP);
    logPP(kzero)=0;
    %
    %  estimates of the lambda and nu parameters of the Gamma
    %  distribution taken over 14-day periods
    %
    slogPtot=sum(logPP);
    for i=1:n14
        aa(i)=sum(stot(14*(i-1)+1:14*i));
        xmean(i)=sum(sPtot(14*(i-1)+1:14*i))/aa(i);
        xvar(i)=(sum(sPtot2(14*(i-1)+1:14*i))-aa(i)*xmean(i)^2)/(aa(i)-1);
        meanlogx(i)=sum(slogPtot(14*(i-1)+1:14*i))/aa(i);
    end
    logxmean=log(xmean);
    y=logxmean-meanlogx;
    
    % MLM
    
        nu=(1+sqrt(1+4*y/3))./(4*y);
        nu=(aa-3).*nu./aa;
   
    alambda=nu./xmean;
end
% 
n=n';
%
% we start the analysis of the climatic (temperature, radiation) time series
%
%
%  PnP:  matrix with elements = 1 for wet days only (=0 for dry and missing
%  days)
%
%  PnPm1:  matrix with elements = 1 for dry days only (=0 when wet and
%  missing days).
%
PnPm1=1-PnP;  % here elements = 1 include dry and missing precipitation days
PnPm1(kn)=0;   % missing precipitation days are removed from the matrix
%
%  make sure that all matrices P and Tmin, Tmax and Rad are the same size

% take the years that are common only

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% GENERATION !!!!!!!!!!!!!!!!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Y=GeneratedYears;

%
% generation of time series

% time series for precipitation
%
%  create vectors of daily p00, p10, lbd values from Fourier series
%
day=1:365;
tot=365*Y;
t=1:tot;
T=365/(2*pi);
%%waitbar(0.2,h);

p000=zeros(Y,356);
p010=zeros(Y,365);
p100=zeros(Y,365);
p110=zeros(Y,365);
mm=1;
nn=1;
for i=1:14:364
    p000(1,mm:mm+13)=ap000(nn);
    p010(1,mm:mm+13)=ap010(nn);
    p100(1,mm:mm+13)=ap100(nn);
    p110(1,mm:mm+13)=ap110(nn);
    mm=mm+14;
    nn=nn+1;
end
p000(1,365)=ap000(26);
p010(1,365)=ap010(26);
p100(1,365)=ap100(26);
p110(1,365)=ap110(26);

for i=2:Y
    p000(i,:)=p000(1,:);
    p010(i,:)=p010(1,:);
    p100(i,:)=p100(1,:);
    p110(i,:)=p110(1,:);
end
p000=p000';
p000=reshape(p000,1,[]);
 
p010=p010';
p010=reshape(p010,1,[]);  

p100=p100';
p100=reshape(p100,1,[]);
 
p110=p110';
p110=reshape(p110,1,[]); 

p001=1-p000;

%%waitbar(0.35,h);

p011=1-p010;

%%waitbar(0.5,h);

p101=1-p100;

%%waitbar(0.65,h);

p111=1-p110;

%%waitbar(0.8,h);
%
% add one element to verctors p00, p01, p10 and p11 to avoid put an if
% statement in the loop to generate time series of wet dry days
%
p000(tot+1)=0;

p010(tot+1)=p000(tot+1);

p100(tot+1)=p000(tot+1);

p110(tot+1)=p100(tot+1);

lbd=zeros(Y,356);
mm=1;
nn=1;
for i=1:14:364
    lbd(1,mm:mm+13)=alambda(nn);
    mm=mm+14;
    nn=nn+1;
end
lbd(1,365)=alambda(26);

for i=2:Y
    lbd(i,:)=lbd(1,:);
end
lbd=lbd';
lbd=reshape(lbd,1,[]);

if idistr == 2
    anu=nu;
    nu=zeros(Y,356);
    mm=1;
    nn=1;
    for i=1:14:364
        nu(1,mm:mm+13)=anu(nn);
        mm=mm+14;
        nn=nn+1;
    end
    nu(1,365)=anu(26);
    for i=2:Y
        nu(i,:)=nu(1,:);
    end
    nu=nu';
    nu=reshape(nu,1,[]);
end

%
% generate time series of dry (X=0) and wet (X=1) days
% pn is the marginal distribution of X on day n
%
% assume that day 0 is dry.  state=0 when day is dry,  state=1 when day is wet
%
% loop to generate time series
%
n=Y*365;

markov=zeros(1,n);
markov(1)=0;    % start by assuming two consecutive dry days
markov(2)=0;

%h = %waitbar(0,'Generation of precipitation occurrence...');
wbt=round(n/100);

for i=1:n-2;
    Ru=rand(1);
            if markov(i)==0&markov(i+1)==0;
                Pr=p000(i);
            elseif markov(i)==0&markov(i+1)>0;
                Pr=p010(i);;
            elseif markov(i)>0&markov(i+1)==0;
                Pr=p100(i);
            elseif markov(i)>0&markov(i+1)>0;
                Pr=p110(i);
            end

    % establish if day i is dry or wet
   if Ru <= Pr;
      % dry day
      markov(i+2)=0;
   else
      % wet day
      markov(i+2)=1;
   end
end

%close(h);
clear p000 p010 p100 p110 ;
clear p001 p011 p101 p111 ;

X=markov;
XX=find(X==1);
mat=XX;
nw=length(XX);

%
% generate time series of precipitation amounts
%

tol=0.00001;    % tolerance criteria for iteration
r=zeros(1,n);
rr=zeros(1,n);

%h = %waitbar(0,'Generation of precipitation quantity...');
wbt=round(nw/100);

if idistr == 1
    
%     assuming exponential distribution
    
    for i=1:nw
       P=rand(1);
       r(XX(i))=-log(1-P)/lbd(XX(i));  
       if i/wbt - round(i/wbt) == 0   % update %waitbar every 1/100th
            %waitbar(i/(nw*1.1),h);  % %waitbar at 90% after this step
       end
    end

else
    
    %
    % assuming 2-parameter gamma distribution
    %
    
    for i=1:nw  % for each wet day, draw a random number and get precip using
                % inverse of gamma CDF
       Px=rand(1);   
       r(XX(i))=gaminv(Px,nu(XX(i)),1/lbd(XX(i)));
%        end

       if i/wbt - round(i/wbt) == 0   % update %waitbar every 1/100th
           %waitbar(i/(nw*1.1),h);      % %waitbar at 90% after this step
       end     
    end
end

clear lbd;

gP=reshape(r,365,Y);
gP=gP';
clear r;

jj=find(gP>0);   % add precipitation threshold
gP(jj)=gP(jj)+PrecipThreshold;
clear jj;




end

