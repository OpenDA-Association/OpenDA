function [t, obs, pred_f, pred_a]=load_sequential(fname)
% function [t, obs, pred_f, pred_a]=load_sequential(fname)
%
% read from file 'fname', produced by a SequentialSimulation run
%
% t        time of observations
% obs      Observed values
% pred_f   Model prediction (before assimilation)
% pred_a   Model prediction (after assimilation)

  eval(fname)
  nobs=length(obs{1});
  obs=[obs{:}];
  obs=reshape(obs,nobs,length(obs)/nobs);
  pred_f=[pred_f_central{:}];
  pred_f=reshape(pred_f,nobs,length(pred_f)/nobs);
  pred_a=[pred_a_central{:}];
  pred_a=reshape(pred_a,nobs,length(pred_a)/nobs);

  t=[analysis_time{:}];
