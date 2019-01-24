% OpenDA version 2.4.4.-1 July 26 2018
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochObserver\noosObservations.xml
% NoosStochObserver[i]=TimeSeries(
%   Location = station01
%   Position = (30000.0,250.0)
%   Height   = NaN
%   Quantity = waterlevel
%   Unit     = m
%   Source   = observed
%   Id       = station01.waterlevel
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.005
%   status  = assimilation
%   Values   = 
%   (48257.0=199101010000,1.0)
%   (48257.041666666664=199101010100,1.067964431681159)
%   (48257.083333333336=199101010200,1.103553671560076)
%   (48257.125=199101010300,0.959473263982275)
%   (48257.166666666664=199101010400,0.879899644186308)
%   ...
%   (48258.833333333336=199101022000,0.327040594118595)
%   (48258.875=199101022100,0.196536161615293)
%   (48258.916666666664=199101022200,0.08396046048499)
%   (48258.958333333336=199101022300,-0.007319618606549)
%   (48259.0=199101030000,-0.045794542637673)
%
%   Values.length()=49
%)
% Starting Algorithm: 
%	className: org.openda.algorithms.kalmanFilter.SequentialSimulation
%	dir.: C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\algorithm
%	config.: SequentialSimulation.xml
% configstring = SequentialSimulation.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\algorithm\SequentialSimulation.xml
% analysisTimes@type=fixed
% analysisTimes@type=fixed
% mainModel@stochParameter=false
% mainModel@stochForcing=false
% mainModel@stochInit=false
% Creating mainModel
% Create new BBModelInstance with number: 0
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Do not add noise to forcing
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% model time 48257.0 48259.0
% analysisTimes acquired from MODEL:48257.0 -- 48259.0
% Application initializing finished
% Initializing application
% Application initialized
% Application starting next step
% Skip Analysis at initial time
%
% Application starting next step
% ========================================================================
%
%  Forecast from 199101010000UTC  to 199101010100UTC  (48257.0-->48257.041666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101010100UTC (48257.041666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{1}	=48257.041666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{1}	=[1.0679644316811592];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{1}	=[1.067964431681159];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{1}	=[1.0679644316811592];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101010100UTC  to 199101010200UTC  (48257.041666666664-->48257.083333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101010200UTC (48257.083333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{2}	=48257.083333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{2}	=[1.1035536718841266];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{2}	=[1.103553671560076];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{2}	=[1.1035536718841266];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101010200UTC  to 199101010300UTC  (48257.083333333336-->48257.125) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101010300UTC (48257.125) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{3}	=48257.125;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{3}	=[0.9594608890981019];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{3}	=[0.959473263982275];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{3}	=[0.9594608890981019];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101010300UTC  to 199101010400UTC  (48257.125-->48257.166666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101010400UTC (48257.166666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{4}	=48257.166666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{4}	=[0.8793175713769861];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{4}	=[0.879899644186308];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{4}	=[0.8793175713769861];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101010400UTC  to 199101010500UTC  (48257.166666666664-->48257.208333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101010500UTC (48257.208333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{5}	=48257.208333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{5}	=[0.7020594577926271];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{5}	=[0.704063874508183];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{5}	=[0.7020594577926271];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101010500UTC  to 199101010600UTC  (48257.208333333336-->48257.25) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101010600UTC (48257.25) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{6}	=48257.25;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{6}	=[0.5629259466967395];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{6}	=[0.563027454075032];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{6}	=[0.5629259466967395];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101010600UTC  to 199101010700UTC  (48257.25-->48257.291666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101010700UTC (48257.291666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{7}	=48257.291666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{7}	=[0.41564945336838255];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{7}	=[0.415996571707539];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{7}	=[0.41564945336838255];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101010700UTC  to 199101010800UTC  (48257.291666666664-->48257.333333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101010800UTC (48257.333333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{8}	=48257.333333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{8}	=[0.2726246301535747];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{8}	=[0.272819351291811];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{8}	=[0.2726246301535747];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101010800UTC  to 199101010900UTC  (48257.333333333336-->48257.375) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101010900UTC (48257.375) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{9}	=48257.375;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{9}	=[0.14362682832567877];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{9}	=[0.143731047197675];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{9}	=[0.14362682832567877];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101010900UTC  to 199101011000UTC  (48257.375-->48257.416666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101011000UTC (48257.416666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{10}	=48257.416666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{10}	=[0.04051483481230789];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{10}	=[0.010563030173172];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{10}	=[0.04051483481230789];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101011000UTC  to 199101011100UTC  (48257.416666666664-->48257.458333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101011100UTC (48257.458333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{11}	=48257.458333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{11}	=[-0.011518733416506432];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{11}	=[-0.03];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{11}	=[-0.011518733416506432];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101011100UTC  to 199101011200UTC  (48257.458333333336-->48257.5) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101011200UTC (48257.5) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{12}	=48257.5;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{12}	=[0.0711727518722663];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{12}	=[0.011216754498642];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{12}	=[0.0711727518722663];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101011200UTC  to 199101011300UTC  (48257.5-->48257.541666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101011300UTC (48257.541666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{13}	=48257.541666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{13}	=[0.3163426841283466];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{13}	=[0.316440636275286];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{13}	=[0.3163426841283466];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101011300UTC  to 199101011400UTC  (48257.541666666664-->48257.583333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101011400UTC (48257.583333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{14}	=48257.583333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{14}	=[0.522613613690725];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{14}	=[0.522622799609719];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{14}	=[0.522613613690725];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101011400UTC  to 199101011500UTC  (48257.583333333336-->48257.625) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101011500UTC (48257.625) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{15}	=48257.625;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{15}	=[0.7751135243650628];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{15}	=[0.85143743355238];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{15}	=[0.7751135243650628];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101011500UTC  to 199101011600UTC  (48257.625-->48257.666666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101011600UTC (48257.666666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{16}	=48257.666666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{16}	=[0.8142431213171347];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{16}	=[0.898249109261345];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{16}	=[0.8142431213171347];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101011600UTC  to 199101011700UTC  (48257.666666666664-->48257.708333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101011700UTC (48257.708333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{17}	=48257.708333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{17}	=[0.7093416994325886];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{17}	=[0.809461902913392];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{17}	=[0.7093416994325886];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101011700UTC  to 199101011800UTC  (48257.708333333336-->48257.75) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101011800UTC (48257.75) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{18}	=48257.75;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{18}	=[0.5562770359119711];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{18}	=[0.55660624202];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{18}	=[0.5562770359119711];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101011800UTC  to 199101011900UTC  (48257.75-->48257.791666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101011900UTC (48257.791666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{19}	=48257.791666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{19}	=[0.3928169536169888];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{19}	=[0.393212624260681];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{19}	=[0.3928169536169888];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101011900UTC  to 199101012000UTC  (48257.791666666664-->48257.833333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101012000UTC (48257.833333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{20}	=48257.833333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{20}	=[0.24746505464711788];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{20}	=[0.247660891049175];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{20}	=[0.24746505464711788];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101012000UTC  to 199101012100UTC  (48257.833333333336-->48257.875) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101012100UTC (48257.875) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{21}	=48257.875;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{21}	=[0.1281232537656632];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{21}	=[0.128409322409243];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{21}	=[0.1281232537656632];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101012100UTC  to 199101012200UTC  (48257.875-->48257.916666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101012200UTC (48257.916666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{22}	=48257.916666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{22}	=[0.024040744648406717];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{22}	=[-0.024109480980681];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{22}	=[0.024040744648406717];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101012200UTC  to 199101012300UTC  (48257.916666666664-->48257.958333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101012300UTC (48257.958333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{23}	=48257.958333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{23}	=[-0.04182938878274063];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{23}	=[-0.066653768355225];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{23}	=[-0.04182938878274063];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101012300UTC  to 199101020000UTC  (48257.958333333336-->48258.0) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101020000UTC (48258.0) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{24}	=48258.0;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{24}	=[-0.00508883604642399];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{24}	=[-0.034987799551524];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{24}	=[-0.00508883604642399];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101020000UTC  to 199101020100UTC  (48258.0-->48258.041666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101020100UTC (48258.041666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{25}	=48258.041666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{25}	=[0.24286189151628224];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{25}	=[0.242932268595065];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{25}	=[0.24286189151628224];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101020100UTC  to 199101020200UTC  (48258.041666666664-->48258.083333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101020200UTC (48258.083333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{26}	=48258.083333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{26}	=[0.4289643589239772];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{26}	=[0.429001883357458];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{26}	=[0.4289643589239772];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101020200UTC  to 199101020300UTC  (48258.083333333336-->48258.125) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101020300UTC (48258.125) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{27}	=48258.125;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{27}	=[0.7179810520368647];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{27}	=[0.797991277594757];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{27}	=[0.7179810520368647];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101020300UTC  to 199101020400UTC  (48258.125-->48258.166666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101020400UTC (48258.166666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{28}	=48258.166666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{28}	=[0.8053545732393419];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{28}	=[0.885361083558286];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{28}	=[0.8053545732393419];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101020400UTC  to 199101020500UTC  (48258.166666666664-->48258.208333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101020500UTC (48258.208333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{29}	=48258.208333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{29}	=[0.7476503986005665];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{29}	=[0.827656933976138];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{29}	=[0.7476503986005665];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101020500UTC  to 199101020600UTC  (48258.208333333336-->48258.25) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101020600UTC (48258.25) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{30}	=48258.25;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{30}	=[0.5984479025401295];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{30}	=[0.598548157166975];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{30}	=[0.5984479025401295];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101020600UTC  to 199101020700UTC  (48258.25-->48258.291666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101020700UTC (48258.291666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{31}	=48258.291666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{31}	=[0.4430923540938818];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{31}	=[0.443264999340227];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{31}	=[0.4430923540938818];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101020700UTC  to 199101020800UTC  (48258.291666666664-->48258.333333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101020800UTC (48258.333333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{32}	=48258.333333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{32}	=[0.2862233372307033];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{32}	=[0.286272651083282];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{32}	=[0.2862233372307033];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101020800UTC  to 199101020900UTC  (48258.333333333336-->48258.375) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101020900UTC (48258.375) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{33}	=48258.375;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{33}	=[0.16255595959024424];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{33}	=[0.162622389655705];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{33}	=[0.16255595959024424];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101020900UTC  to 199101021000UTC  (48258.375-->48258.416666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101021000UTC (48258.416666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{34}	=48258.416666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{34}	=[0.05325631611321724];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{34}	=[0.053192168856411];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{34}	=[0.05325631611321724];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101021000UTC  to 199101021100UTC  (48258.416666666664-->48258.458333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101021100UTC (48258.458333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{35}	=48258.458333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{35}	=[-0.027349915232363364];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{35}	=[-0.04375335753163];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{35}	=[-0.027349915232363364];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101021100UTC  to 199101021200UTC  (48258.458333333336-->48258.5) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101021200UTC (48258.5) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{36}	=48258.5;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{36}	=[-0.03627885883959763];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{36}	=[-0.146022089254433];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{36}	=[-0.03627885883959763];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101021200UTC  to 199101021300UTC  (48258.5-->48258.541666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101021300UTC (48258.541666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{37}	=48258.541666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{37}	=[0.16145669366023518];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{37}	=[0.021576347921177];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{37}	=[0.16145669366023518];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101021300UTC  to 199101021400UTC  (48258.541666666664-->48258.583333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101021400UTC (48258.583333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{38}	=48258.583333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{38}	=[0.35503947279645026];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{38}	=[0.355127993074255];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{38}	=[0.35503947279645026];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101021400UTC  to 199101021500UTC  (48258.583333333336-->48258.625) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101021500UTC (48258.625) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{39}	=48258.625;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{39}	=[0.6289370954250927];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{39}	=[0.628976646438423];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{39}	=[0.6289370954250927];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101021500UTC  to 199101021600UTC  (48258.625-->48258.666666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101021600UTC (48258.666666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{40}	=48258.666666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{40}	=[0.7809978271968868];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{40}	=[0.781033116731799];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{40}	=[0.7809978271968868];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101021600UTC  to 199101021700UTC  (48258.666666666664-->48258.708333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101021700UTC (48258.708333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{41}	=48258.708333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{41}	=[0.7700712917772554];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{41}	=[0.770074634039716];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{41}	=[0.7700712917772554];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101021700UTC  to 199101021800UTC  (48258.708333333336-->48258.75) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101021800UTC (48258.75) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{42}	=48258.75;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{42}	=[0.6372178773395163];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{42}	=[0.637268510660378];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{42}	=[0.6372178773395163];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101021800UTC  to 199101021900UTC  (48258.75-->48258.791666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101021900UTC (48258.791666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{43}	=48258.791666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{43}	=[0.48816919807479164];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{43}	=[0.48842429511432];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{43}	=[0.48816919807479164];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101021900UTC  to 199101022000UTC  (48258.791666666664-->48258.833333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101022000UTC (48258.833333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{44}	=48258.833333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{44}	=[0.3270625165259268];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{44}	=[0.327040594118595];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{44}	=[0.3270625165259268];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101022000UTC  to 199101022100UTC  (48258.833333333336-->48258.875) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101022100UTC (48258.875) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{45}	=48258.875;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{45}	=[0.1964673692286788];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{45}	=[0.196536161615293];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{45}	=[0.1964673692286788];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101022100UTC  to 199101022200UTC  (48258.875-->48258.916666666664) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101022200UTC (48258.916666666664) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{46}	=48258.916666666664;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{46}	=[0.08400723803532656];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{46}	=[0.08396046048499];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{46}	=[0.08400723803532656];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101022200UTC  to 199101022300UTC  (48258.916666666664-->48258.958333333336) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101022300UTC (48258.958333333336) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{47}	=48258.958333333336;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{47}	=[-0.00726874933759793];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{47}	=[-0.007319618606549];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{47}	=[-0.00726874933759793];
% Application starting next step
% ========================================================================
%
%  Forecast from 199101022300UTC  to 199101030000UTC  (48258.958333333336-->48259.0) 
%
% ========================================================================
%
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\estuary_map.nc
% - mainModel 
%
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% ========================================================================
%
%  analysis at 199101030000UTC (48259.0) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{48}	=48259.0;
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work0\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_f_central{48}	=[-0.04594582587580048];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{48}	=[-0.045794542637673];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: 
station01.waterlevel
pred_a_central{48}	=[-0.04594582587580048];
% Application Done
