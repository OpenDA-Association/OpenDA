% OpenDA version 2.0
% Starting Algorithm: 
%	className: org.openda.algorithms.kalmanFilter.EnKF
%	dir.: D:\src\ODA\openda2.1\public\model_swan\tests\s00c2_withMissingValues\.\algorithm
%	config.: EnkfAlgorithm.xml
% configstring = EnkfAlgorithm.xml
% opening :D:\src\ODA\openda2.1\public\model_swan\tests\s00c2_withMissingValues\.\algorithm\EnkfAlgorithm.xml
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
% opening :D:\src\ODA\openda2.1\public\model_swan\tests\s00c2_withMissingValues\.\stochModel\.\BoundaryNoise.xml
% analysisTimes=201001020000,201001020100,...,201001100000
%    Do not add noise to forcing
% model time 55198.0 55199.0
% analysisTimes acquired from OBSERVER:55198.041666666664 -- 55198.125
% Creating ensemble model 0
% configstring = BoundaryNoise.xml
% opening :D:\src\ODA\openda2.1\public\model_swan\tests\s00c2_withMissingValues\.\stochModel\.\BoundaryNoise.xml
% analysisTimes=201001020000,201001020100,...,201001100000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 1
% configstring = BoundaryNoise.xml
% opening :D:\src\ODA\openda2.1\public\model_swan\tests\s00c2_withMissingValues\.\stochModel\.\BoundaryNoise.xml
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
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_central{1}	=[1.28949, 1.40544, 1.28949, 1.13182];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
%  ObsValues: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
obs{1}	=[4.29859, 4.68213, 4.29859, 4.89074];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_0{1}	=[1.05043, 1.14516, 1.05043, 0.72692];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_1{1}	=[1.3115, 1.4294, 1.3115, 1.16534];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f{1}	=[1.180965, 1.28728, 1.180965, 0.94613];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_std{1}	=[0.18460436736437205, 0.2009880314844643, 0.18460436736437205, 0.3100097550078062];
% length of state vector: 850952.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_linear{1}	=[2.545891557038196, 2.7733439850328905, 2.545891557038196, 3.238278087243596];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_0{1}	=[1.05043, 1.14516, 1.05043, 0.72692];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_1{1}	=[1.3115, 1.4294, 1.3115, 1.16534];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,1000., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a{1}	=[1.180965, 1.28728, 1.180965, 0.94613];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
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
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_central{2}	=[3.03752, 3.53887, 3.03752, 3.4889];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
%  ObsValues: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
obs{2}	=[4.29859, 4.96128, 4.29859, 4.89074];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_0{2}	=[3.35088, 3.91177, 3.35088, 3.8566];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_1{2}	=[3.17927, 3.70621, 3.17927, 3.65389];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f{2}	=[3.265075, 3.80899, 3.265075, 3.755245];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_std{2}	=[0.1213465947194236, 0.14535286994070684, 0.1213465947194236, 0.14333761561432484];
% length of state vector: 850952.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_linear{2}	=[3.5128850178477324, 4.105824842193229, 3.5128850178477324, 4.047964356202516];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_0{2}	=[3.35088, 3.91177, 3.35088, 3.8566];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_1{2}	=[3.17927, 3.70621, 3.17927, 3.65389];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a{2}	=[3.265075, 3.80899, 3.265075, 3.755245];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_central{2}	=[3.03752, 3.53887, 3.03752, 3.4889];
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
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_central{3}	=[3.13363, 3.54821, 3.41396, 3.59965, 3.13363, 3.54821];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
%  ObsValues: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
obs{3}	=[4.29859, 4.89074, 4.68213, 4.96128, 4.29859, 4.89074];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_0{3}	=[2.76196, 3.10569, 3.00911, 3.15096, 2.76196, 3.10569];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_1{3}	=[2.70732, 3.03897, 2.9496, 3.08333, 2.70732, 3.03897];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f{3}	=[2.73464, 3.07233, 2.979355, 3.117145, 2.73464, 3.07233];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_f_std{3}	=[0.038636314524032975, 0.04717816444076653, 0.04207992454841141, 0.047821631611646105, 0.038636314524032975, 0.04717816444076653];
% length of state vector: 850952.
% number of observations: 6.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_linear{3}	=[2.8101577083073197, 3.164543424199568, 3.0616035143003044, 3.2106161312742314, 2.8101577083073197, 3.164543424199568];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_0{3}	=[2.76196, 3.10569, 3.00911, 3.15096, 2.76196, 3.10569];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_1{3}	=[2.70732, 3.03897, 2.9496, 3.08333, 2.70732, 3.03897];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a{3}	=[2.73464, 3.07233, 2.979355, 3.117145, 2.73464, 3.07233];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: Hsig @ 1000.,1000., Hsig @ 1000.,2500., Hsig @ 3000.,1000., Hsig @ 3000.,2500., Hsig @ 5000.,1000., Hsig @ 5000.,2500.
pred_a_central{3}	=[3.13363, 3.54821, 3.41396, 3.59965, 3.13363, 3.54821];
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
