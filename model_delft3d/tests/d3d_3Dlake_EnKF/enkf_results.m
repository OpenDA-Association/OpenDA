% OpenDA version (Development)
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\stochObserver\noosObservations.xml
% NoosStochObserver[i]=TimeSeries(
%   Location = station1
%   Position = (5.0,11.0)
%   Height   = 20.0
%   Quantity = temperature
%   Unit     = m
%   Source   = observed
%   Id       = station1.temperature
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.5
%   status  = assimilation
%   Values   = 
%   (57175.875=201506022100,9.99)
%
%   Values.length()=1
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = station2
%   Position = (17.0,13.0)
%   Height   = 20.0
%   Quantity = temperature
%   Unit     = m
%   Source   = observed
%   Id       = station2.temperature
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.5
%   status  = assimilation
%   Values   = 
%   (57175.875=201506022100,10.18)
%
%   Values.length()=1
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = station3
%   Position = (19.0,7.0)
%   Height   = 20.0
%   Quantity = temperature
%   Unit     = m
%   Source   = observed
%   Id       = station3.temperature
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.5
%   status  = assimilation
%   Values   = 
%   (57175.875=201506022100,10.11)
%
%   Values.length()=1
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = station4
%   Position = (27.0,10.0)
%   Height   = 20.0
%   Quantity = temperature
%   Unit     = m
%   Source   = observed
%   Id       = station4.temperature
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.5
%   status  = assimilation
%   Values   = 
%   (57175.875=201506022100,10.17)
%
%   Values.length()=1
%)
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\stochModel\ThreadStochModel.xml
% Starting Algorithm: 
%	className: org.openda.algorithms.kalmanFilter.EnKF
%	dir.: C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\algorithm
%	config.: EnkfAlgorithm.xml
% configstring = EnkfAlgorithm.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\algorithm\EnkfAlgorithm.xml
% analysisTimes@type=fromObservationTimes
% mainModel@stochParameter=false
% mainModel@stochForcing=false
% mainModel@stochInit=false
% this.ensembleSize=3
% ensembleModel@stochParameter=false
% ensembleModel@stochForcing=true
% ensembleModel@stochInit=true
% saveGain/times@type=none
% Creating mainModel
% Create new BBModelInstance with number: 0
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\stochModel\.\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Do not add noise to forcing
% model time 57174.0 57176.0
% analysisTimes acquired from OBSERVER:57175.875 -- 57175.875
% Creating ensemble model 0
% Create new BBModelInstance with number: 1
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\stochModel\.\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 1
% Create new BBModelInstance with number: 2
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\stochModel\.\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 2
% Create new BBModelInstance with number: 3
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\stochModel\.\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Add noise to initial state
%    Add noise to forcing
% Application initializing finished
% Initializing Algorithm
% Algorithm initialized
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506010000UTC  to 201506022100UTC  (57174.0-->57175.875) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (3):
% - member 0
% - member 1
% - member 2
% ========================================================================
%
%  analysis at 201506022100UTC (57175.875) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{1}	=57175.875;
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station1.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station2.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station3.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station4.temperature'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{1}	=[5.486257076263428, 5.5687479972839355, 5.502621650695801, 5.5181756019592285];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{1}	=[9.99,10.18,10.11,10.17];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station1.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station2.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station3.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station4.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station1.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station2.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station3.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station4.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station1.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station2.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station3.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station4.temperature'.
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{1}	=[4.019341945648193, 3.9999265670776367, 3.9457240104675293, 3.9710958003997803];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{1}	=[4.251110076904297, 4.281209468841553, 4.282816410064697, 4.244258880615234];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{1}	=[4.186851978302002, 4.219995975494385, 4.198075771331787, 4.2521586418151855];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{1}	=[4.152434666951497, 4.167044003804524, 4.142205397288004, 4.155837774276733];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{1}	=[0.1196558822496194, 0.14792887204857952, 0.17535377238140137, 0.16003999262908844];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{1}	=[5.398266925270843, 5.71414585819913, 5.972442707606783, 5.799257968724473];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station1.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station2.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station3.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station4.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station1.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station2.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station3.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station4.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station1.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station2.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station3.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station4.temperature'.
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{1}	=[5.440797805786133, 5.764656066894531, 6.033725738525391, 5.843865871429443];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{1}	=[5.360177516937256, 5.657468795776367, 5.911611557006836, 5.702256202697754];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{1}	=[5.338547706604004, 5.650392532348633, 5.890120029449463, 5.772374153137207];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{1}	=[5.379841009775797, 5.690839131673177, 5.945152441660563, 5.772832075754801];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station1.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station2.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station3.temperature'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station4.temperature'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{1}	=[5.379840850830078, 5.6908392906188965, 5.945152282714844, 5.772831916809082];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506022100UTC  to 201506030000UTC  (57175.875-->57176.0) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (3):
% - member 0
% - member 1
% - member 2
% Algorithm Done
% Application Done
