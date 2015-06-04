function [t, ens, obs, pred_f, pred_a]=load_ensemble(fname)
%[t,ens, obs, pred_f, pred_a]=function load_ensemble(fname)
%
% Load the ensemble states at observation times and observations at analysis times as stored in fname.m
%
% t       model times
% ens     ensemble states at time(s) t
% obs     observations at time(s) t
% pred_f  mean prediction at forecast
% pred_a  mean prediction after assimilation
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
        dum=reshape(dum,203,dim1/203);
        ens(:,:,i+1)=dum;
     end
  end

  nobs=length(obs{1});
  obs=[obs{:}];
  obs=reshape(obs,nobs,length(obs)/nobs);
  pred_f=[pred_f_central{:}];
  pred_f=reshape(pred_f,nobs,length(pred_f)/nobs);
  pred_a=[pred_a_central{:}];
  pred_a=reshape(pred_a,nobs,length(pred_a)/nobs);

  t=[analysis_time{:}];

