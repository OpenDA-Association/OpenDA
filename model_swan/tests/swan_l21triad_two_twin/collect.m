
clear all
results_dud
save dud.mat
clear all
results_dudWithConstr
save dud_constraint.mat
clear all
results_simplex
save simplex.mat
clear all
results_simplexWithConstr
save simplex_constraint.mat
clear all
results_powell
save powell.mat
clear all
results_powellWithConstr
save powell_constraint.mat
clear all

%load all data at once
dud  = load('dud.mat');
dudc = load('dud_constraint.mat');
simplex  = load('simplex.mat');
simplexc = load('simplex_constraint.mat');
powell  = load('powell.mat');
powellc = load('powell_constraint.mat');

%save all data
save data.mat
