function [t, ens]=load_ensemble(fname)
%[t,ens]=function load_ensemble(fname)
%
% Load the ensemble states at observation times for the advection model model as stored
% in fname.m
%
% Note we assume a state vector of 52 elements (1-st element is noise 2nd..52th model)
%
% t     model times
% ens   ensemble states at time(s) t
%
% author: Nils van Velzen


  eval(fname)

  %Pick all ensemble members from the result file
  for i=0:1000;
     varnam=['xi_f_',num2str(i)];
     if ~exist(varnam)
        disp(['Number of ensembles is ',num2str(i)]);
        break;
     else
        eval(['dum=[',varnam,'{:}];']);
        dim1=length(dum);
        dum=reshape(dum,52,dim1/52);
        ens(:,:,i+1)=dum;
     end
  end

  t=[analysis_time{:}];

