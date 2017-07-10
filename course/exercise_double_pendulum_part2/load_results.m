function [t,xyz,tobs,obs]=load_results (fname)
%[t,xyz,tobs,obs]=function load_results (fname)
%
% Load the results of the lorenz model as stored in fname.m
%
% t     model times
% xyz   model states at t
% tobs  observation times
% obs   observed values at tobs
%
% autor: Nils van Velzen


  eval(fname)

  if exist('x')==1, %simulation
     nstate=length(x{1});
     xyz =reshape([x{:}],nstate,length(x));
     t   =[model_time{:}];
  else %enkf
     nstate=length(x_f_central{1});
     xyz   =reshape([x_f_central{:}],nstate,length(x_f_central));
     t     =[analysis_time{:}];
  end
  nobs = length(obs{1});
  obs =reshape([obs{:}],nobs,length(obs));
  tobs=[analysis_time{:}];

