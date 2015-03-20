% OpenDA version 2.0
% Starting Algorithm: 
%	className: org.openda.algorithms.kalmanFilter.EnKF
%	dir.: d:\src\ODA\openda2.1\public\model_swan\tests\s00c2_withMissingValues\.\algorithm
%	config.: EnkfAlgorithm.xml
% configstring = EnkfAlgorithm.xml
% opening :d:\src\ODA\openda2.1\public\model_swan\tests\s00c2_withMissingValues\.\algorithm\EnkfAlgorithm.xml
% analysisTimes@type=fromObservationTimes
% mainModel@stochParameter=false
% mainModel@stochForcing=false
% mainModel@stochInit=false
% this.ensembleSize=2
% ensembleModel@stochParameter=false
% ensembleModel@stochForcing=true
% ensembleModel@stochInit=true
% saveGain/times@type=none
% Creating mainModel
% configstring = BoundaryNoise.xml
% opening :d:\src\ODA\openda2.1\public\model_swan\tests\s00c2_withMissingValues\.\stochModel\.\BoundaryNoise.xml
% analysisTimes=201001020000,201001020100,...,201001100000
%    Do not add noise to forcing
% model time 55198.0 55199.0
% analysisTimes acquired from OBSERVER:55198.041666666664 -- 55198.125
% Creating ensemble model 0
% configstring = BoundaryNoise.xml
% opening :d:\src\ODA\openda2.1\public\model_swan\tests\s00c2_withMissingValues\.\stochModel\.\BoundaryNoise.xml
% analysisTimes=201001020000,201001020100,...,201001100000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 1
% configstring = BoundaryNoise.xml
% opening :d:\src\ODA\openda2.1\public\model_swan\tests\s00c2_withMissingValues\.\stochModel\.\BoundaryNoise.xml
% analysisTimes=201001020000,201001020100,...,201001100000
%    Add noise to initial state
%    Add noise to forcing
% Application initializing finished
% Initializing Algorithm
% Algorithm initialized
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201001020000UTC  to 201001020100UTC  (55198.0-->55198.041666666664) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (2):
% - member 0
% - member 1
% ========================================================================
%
%  analysis at 201001020100UTC (55198.041666666664) 
%
% ========================================================================
%
%  resultItem id: pred_f_central, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_central{1}	=[1.28949, 1.40544, 1.28949, 1.13182];
%  resultItem id: obs, outputLevel: essential, context: analysis step
%  ObsValues: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
obs{1}	=[4.29859, 4.68213, 4.29859, 4.89074];
%  resultItem id: pred_f_0, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_0{1}	=[1.05043, 1.14516, 1.05043, 0.72692];
%  resultItem id: pred_f_1, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_1{1}	=[1.3115, 1.4294, 1.3115, 1.16534];
%  resultItem id: pred_f, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f{1}	=[1.180965, 1.28728, 1.180965, 0.94613];
% length of state vector: 850952.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_linear{1}	=[2.545891557038196, 2.7733439850328905, 2.545891557038196, 3.238278087243596];
%  resultItem id: pred_a_0, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_0{1}	=[1.05043, 1.14516, 1.05043, 0.72692];
%  resultItem id: pred_a_1, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_1{1}	=[1.3115, 1.4294, 1.3115, 1.16534];
%  resultItem id: pred_a, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a{1}	=[1.180965, 1.28728, 1.180965, 0.94613];
%  resultItem id: pred_a_central, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_central{1}	=[1.28949, 1.40544, 1.28949, 1.13182];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201001020100UTC  to 201001020200UTC  (55198.041666666664-->55198.083333333336) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (2):
% - member 0
% - member 1
% ========================================================================
%
%  analysis at 201001020200UTC (55198.083333333336) 
%
% ========================================================================
%
%  resultItem id: pred_f_central, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_central{2}	=[2.84488, 3.3122, 2.84488, 3.2654];
%  resultItem id: obs, outputLevel: essential, context: analysis step
%  ObsValues: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
obs{2}	=[4.29859, 4.96128, 4.29859, 4.89074];
%  resultItem id: pred_f_0, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_0{2}	=[3.25859, 3.80331, 3.25858, 3.74967];
%  resultItem id: pred_f_1, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_1{2}	=[2.88611, 3.36126, 2.88612, 3.31378];
%  resultItem id: pred_f, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f{2}	=[3.07235, 3.582285, 3.07235, 3.531725];
% length of state vector: 850952.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_linear{2}	=[3.7966043151558218, 4.441812008200792, 3.796565426930136, 4.379274434689838];
%  resultItem id: pred_a_0, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_0{2}	=[3.25859, 3.80331, 3.25858, 3.74967];
%  resultItem id: pred_a_1, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_1{2}	=[2.88611, 3.36126, 2.88612, 3.31378];
%  resultItem id: pred_a, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a{2}	=[3.07235, 3.582285, 3.07235, 3.531725];
%  resultItem id: pred_a_central, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_central{2}	=[2.84488, 3.3122, 2.84488, 3.2654];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201001020200UTC  to 201001020300UTC  (55198.083333333336-->55198.125) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (2):
% - member 0
% - member 1
% ========================================================================
%
%  analysis at 201001020300UTC (55198.125) 
%
% ========================================================================
%
%  resultItem id: pred_f_central, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_central{3}	=[3.43376, 3.8904, 3.74092, 3.94677, 3.43376, 3.8904];
%  resultItem id: obs, outputLevel: essential, context: analysis step
%  ObsValues: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
obs{3}	=[4.29859, 4.89074, 4.68213, 4.96128, 4.29859, 4.89074];
%  resultItem id: pred_f_0, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_0{3}	=[3.1559, 3.55527, 3.43828, 3.60702, 3.1559, 3.55527];
%  resultItem id: pred_f_1, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_1{3}	=[2.91253, 3.27343, 3.17316, 3.32116, 2.91253, 3.27343];
%  resultItem id: pred_f, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f{3}	=[3.0342149999999997, 3.4143499999999998, 3.30572, 3.4640899999999997, 3.0342149999999997, 3.4143499999999998];
% length of state vector: 850952.
% number of observations: 6.
%  resultItem id: pred_a_linear, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_linear{3}	=[3.6622245202990253, 4.141630285988731, 3.9898547907370565, 4.201743784249, 3.6622245202990253, 4.141630285988731];
%  resultItem id: pred_a_0, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_0{3}	=[3.1559, 3.55527, 3.43828, 3.60702, 3.1559, 3.55527];
%  resultItem id: pred_a_1, outputLevel: normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_1{3}	=[2.91253, 3.27343, 3.17316, 3.32116, 2.91253, 3.27343];
%  resultItem id: pred_a, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a{3}	=[3.0342149999999997, 3.4143499999999998, 3.30572, 3.4640899999999997, 3.0342149999999997, 3.4143499999999998];
%  resultItem id: pred_a_central, outputLevel: essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_central{3}	=[3.43376, 3.8904, 3.74092, 3.94677, 3.43376, 3.8904];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201001020300UTC  to 201001030000UTC  (55198.125-->55199.0) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (2):
% - member 0
% - member 1
% Algorithm Done
% Application Done
