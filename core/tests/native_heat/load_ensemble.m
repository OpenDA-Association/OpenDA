function [t, ens]=load_ensemble(fname)
%[t,ens]=function load_ensemble(fname)
%
% Load the ensemble states at observation times for the heat model as stored in fname.m
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
        dum=reshape(dum,100,dim1/100);
        ens(:,:,i+1)=dum;
     end
  end

  t=[analysis_time{:}];

