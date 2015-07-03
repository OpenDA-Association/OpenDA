function [t, obs, pred_f, pred_a]=load_obs(fname)
% function [t, obs, pred_f, pred_a]=load_obs(fname)
%
% read observations from file 'fname'
%
% t        time of observations
% obs      Observed valued
% pred_f   Model prediction (before assimilation)
% pred_a   Model prediction (after assimilation)
%
% author: Nils van Velzen

  eval(fname)
  nobs=length(obs{1});
  obs=[obs{:}];
  obs=reshape(obs,nobs,length(obs)/nobs);
  pred_f=[pred_f_central{:}];
  pred_f=reshape(pred_f,nobs,length(pred_f)/nobs);
  pred_a=[pred_a_central{:}];
  pred_a=reshape(pred_a,nobs,length(pred_a)/nobs);

  t=[analysis_time{:}];


