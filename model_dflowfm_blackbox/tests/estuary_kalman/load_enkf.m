function [t, obs, pred_f, pred_a, pred_f_std]=load_enkf(fname)
%[t, pred_f, pred_a ]=function load_enkf(fname)
%
% Load the ensemble states at observation times and observations at analysis times as stored in fname.m
%
% t       model times
% obs     observations at time(s) t
% pred_f  mean prediction at forecast
% pred_a  mean prediction after assimilation
%
  eval(fname)

  nobs=length(obs{1});
  obs=[obs{:}];
  obs=reshape(obs,nobs,length(obs)/nobs);
  pred_f=[pred_f_central{:}];
  pred_f=reshape(pred_f,nobs,length(pred_f)/nobs);
  pred_a=[pred_a_linear{:}];
  pred_a=reshape(pred_a,nobs,length(pred_a)/nobs);
  pred_f_std=[pred_f_std{:}];
  pred_f_std=reshape(pred_f_std,nobs,length(pred_f_std)/nobs);
  
  t=[analysis_time{:}];

