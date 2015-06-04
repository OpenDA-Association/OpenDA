function [tobs,obs]=load_obs (fname)
%[tobs,obs]=function load_obs (fname)
%
% Load the observations of the lorenz model as stored in fname.m
%
% t     model times
% xyz   model states at t
% tobs  observation times
% obs   observed values at tobs
%
% autor: Nils van Velzen

  eval(fname)

  nobs = length(obs{1});
  obs =reshape([obs{:}],nobs,length(obs));
  tobs=[analysis_time{:}];

