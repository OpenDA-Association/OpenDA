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
%   standarddeviation  = 0.1
%   status  = assimilation
%   Values   = 
%   (57174.125=201506010300,6.95)
%   (57174.25=201506010600,6.93)
%   (57174.375=201506010900,7.2)
%   (57174.5=201506011200,7.55)
%   (57174.625=201506011500,8.3)
%   ...
%   (57175.375=201506020900,8.18)
%   (57175.5=201506021200,8.9)
%   (57175.625=201506021500,9.42)
%   (57175.75=201506021800,9.86)
%   (57175.875=201506022100,9.99)
%
%   Values.length()=15
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
%   standarddeviation  = 0.1
%   status  = assimilation
%   Values   = 
%   (57174.125=201506010300,6.96)
%   (57174.25=201506010600,6.99)
%   (57174.375=201506010900,7.19)
%   (57174.5=201506011200,7.33)
%   (57174.625=201506011500,8.05)
%   ...
%   (57175.375=201506020900,8.08)
%   (57175.5=201506021200,8.76)
%   (57175.625=201506021500,9.8)
%   (57175.75=201506021800,10.25)
%   (57175.875=201506022100,10.18)
%
%   Values.length()=15
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
%   standarddeviation  = 0.1
%   status  = assimilation
%   Values   = 
%   (57174.125=201506010300,6.95)
%   (57174.25=201506010600,6.96)
%   (57174.375=201506010900,7.2)
%   (57174.5=201506011200,7.44)
%   (57174.625=201506011500,8.21)
%   ...
%   (57175.375=201506020900,8.2)
%   (57175.5=201506021200,8.82)
%   (57175.625=201506021500,9.44)
%   (57175.75=201506021800,9.89)
%   (57175.875=201506022100,10.11)
%
%   Values.length()=15
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
%   standarddeviation  = 0.1
%   status  = assimilation
%   Values   = 
%   (57174.125=201506010300,6.96)
%   (57174.25=201506010600,6.97)
%   (57174.375=201506010900,7.2)
%   (57174.5=201506011200,7.36)
%   (57174.625=201506011500,8.02)
%   ...
%   (57175.375=201506020900,7.99)
%   (57175.5=201506021200,8.64)
%   (57175.625=201506021500,9.75)
%   (57175.75=201506021800,10.26)
%   (57175.875=201506022100,10.17)
%
%   Values.length()=15
%)
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
% this.ensembleSize=4
% ensembleModel@stochParameter=false
% ensembleModel@stochForcing=true
% ensembleModel@stochInit=true
% saveGain/times@type=none
% Creating mainModel
% Create new BBModelInstance with number: 0
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\stochModel\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Do not add noise to forcing
% model time 57174.0 57176.0
% analysisTimes acquired from OBSERVER:57174.125 -- 57175.875
% Creating ensemble model 0
% Create new BBModelInstance with number: 1
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\stochModel\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 1
% Create new BBModelInstance with number: 2
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\stochModel\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 2
% Create new BBModelInstance with number: 3
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\stochModel\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 3
% Create new BBModelInstance with number: 4
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF\.\stochModel\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Add noise to initial state
%    Add noise to forcing
% Application initializing finished
% Initializing Algorithm
% Algorithm initialized
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506010000UTC  to 201506010300UTC  (57174.0-->57174.125) 
%
% ========================================================================
%
