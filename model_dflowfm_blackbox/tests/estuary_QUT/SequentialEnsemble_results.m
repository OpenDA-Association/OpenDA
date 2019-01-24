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
%   standarddeviation  = 5.0E-4
%   status  = assimilation
%   Values   = 
%   (48257.0=199101010000,1.0)
%   (48257.041666666664=199101010100,1.1167964431681159)
%   (48257.083333333336=199101010200,1.203553671560076)
%   (48257.125=199101010300,1.059473263982275)
%   (48257.166666666664=199101010400,0.979899644186308)
%   ...
%   (48258.833333333336=199101022000,0.427040594118595)
%   (48258.875=199101022100,0.296536161615293)
%   (48258.916666666664=199101022200,0.18396046048499)
%   (48258.958333333336=199101022300,-0.007319618606549)
%   (48259.0=199101030000,-0.045794542637673)
%
%   Values.length()=49
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = station02
%   Position = (60000.0,250.0)
%   Height   = NaN
%   Quantity = waterlevel
%   Unit     = m
%   Source   = observed
%   Id       = station02.waterlevel
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 5.0E-4
%   status  = assimilation
%   Values   = 
%   (48257.0=199101010000,1.0)
%   (48257.041666666664=199101010100,1.00664)
%   (48257.083333333336=199101010200,0.964747)
%   (48257.125=199101010300,0.830432)
%   (48257.166666666664=199101010400,0.661804)
%   ...
%   (48258.833333333336=199101022000,0.357047)
%   (48258.875=199101022100,0.104238)
%   (48258.916666666664=199101022200,-0.0870496)
%   (48258.958333333336=199101022300,-0.174015)
%   (48259.0=199101030000,-0.0735941)
%
%   Values.length()=49
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = station03
%   Position = (90000.0,250.0)
%   Height   = NaN
%   Quantity = waterlevel
%   Unit     = m
%   Source   = observed
%   Id       = station03.waterlevel
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 5.0E-4
%   status  = assimilation
%   Values   = 
%   (48257.0=199101010000,1.0)
%   (48257.041666666664=199101010100,1.10973)
%   (48257.083333333336=199101010200,1.1245)
%   (48257.125=199101010300,1.00434)
%   (48257.166666666664=199101010400,0.767126)
%   ...
%   (48258.833333333336=199101022000,0.54925)
%   (48258.875=199101022100,0.283848)
%   (48258.916666666664=199101022200,0.0492151)
%   (48258.958333333336=199101022300,-0.136759)
%   (48259.0=199101030000,-0.232202)
%
%   Values.length()=49
%)
% Starting Algorithm: 
%	className: org.openda.algorithms.kalmanFilter.SequentialEnsembleSimulation
%	dir.: C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\algorithm
%	config.: SequentialEnsembleSimulation.xml
% configstring = SequentialEnsembleSimulation.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\algorithm\SequentialEnsembleSimulation.xml
% analysisTimes@type=fixed
% analysisTimes@type=fixed
% mainModel@stochParameter=false
% mainModel@stochForcing=false
% mainModel@stochInit=false
% this.ensembleSize=20
% ensembleModel@stochParameter=false
% ensembleModel@stochForcing=true
% ensembleModel@stochInit=false
% saveGain/times@type=none
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
% Creating ensemble model 0
% Create new BBModelInstance with number: 1
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 1
% Create new BBModelInstance with number: 2
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 2
% Create new BBModelInstance with number: 3
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 3
% Create new BBModelInstance with number: 4
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 4
% Create new BBModelInstance with number: 5
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 5
% Create new BBModelInstance with number: 6
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 6
% Create new BBModelInstance with number: 7
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 7
% Create new BBModelInstance with number: 8
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 8
% Create new BBModelInstance with number: 9
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 9
% Create new BBModelInstance with number: 10
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 10
% Create new BBModelInstance with number: 11
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 11
% Create new BBModelInstance with number: 12
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 12
% Create new BBModelInstance with number: 13
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 13
% Create new BBModelInstance with number: 14
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 14
% Create new BBModelInstance with number: 15
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 15
% Create new BBModelInstance with number: 16
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 16
% Create new BBModelInstance with number: 17
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 17
% Create new BBModelInstance with number: 18
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 18
% Create new BBModelInstance with number: 19
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
% Creating ensemble model 19
% Create new BBModelInstance with number: 20
% Instance initialization done
% configstring = BoundaryNoiseSurge.xml
% opening :C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\BoundaryNoiseSurge.xml
% analysisTimes=199101010000,199101010100,...,199101050000
%    Add noise to forcing
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{1}	=[1.0679644316811592, 0.9960290728626799, 0.7631600594390275];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{1}	=[1.1167964431681159,1.00664,1.10973];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{1}	=[1.069093534444124, 0.996327756181244, 0.7619397872566025];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{1}	=[0.0022373068178305833, 5.01223823562381E-4, 0.16380210506732992];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{2}	=[1.1035536718841266, 0.8688377570118682, 0.5879183141389265];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{2}	=[1.203553671560076,0.964747,1.1245];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{2}	=[1.102875458903874, 0.8873358754426687, 0.5924704066089541];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{2}	=[0.0030134130921953923, 0.11262958129978284, 0.23628913277460917];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{3}	=[0.9594608890981019, 0.8224669287230606, 0.3820575653741908];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{3}	=[1.059473263982275,0.830432,1.00434];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{3}	=[0.9745799801489634, 0.8362603994042003, 0.3767748162036937];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{3}	=[0.09149197781803498, 0.1441488274818881, 0.1841616169387897];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{4}	=[0.8793175713769861, 0.7074450868804701, 0.14479860834932012];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{4}	=[0.979899644186308,0.661804,0.767126];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{4}	=[0.8958640911552452, 0.712123664436526, 0.15971970958560894];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{4}	=[0.10771485285854156, 0.09439155844591975, 0.1337692919639266];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{5}	=[0.7020594577926271, 0.5705813616259865, -0.06601785633417576];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{5}	=[0.804063874508183,0.39194,0.576391];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{5}	=[0.7278918965261002, 0.5770701031719017, -0.03743592489444088];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{5}	=[0.1366519588436382, 0.06484321986357837, 0.15753373507718207];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{6}	=[0.5629259466967395, 0.3915715468932952, -0.2228190360573638];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{6}	=[0.663027454075032,0.15716,0.346304];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{6}	=[0.5843847891902219, 0.4196046302375474, -0.20241758556300937];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{6}	=[0.11569959804466318, 0.11094408651171964, 0.14340921703851275];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{7}	=[0.41564945336838255, 0.2515640121481246, -0.3246060692779129];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{7}	=[0.515996571707539,-0.0712184,0.11151];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{7}	=[0.4353732470002239, 0.27690149632927297, -0.26141786529642486];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{7}	=[0.09583528613113508, 0.11035090551097104, 0.1592145195406845];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{8}	=[0.2726246301535747, 0.12677329122844616, -0.29253942347770995];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{8}	=[0.372819351291811,-0.27036,-0.105376];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{8}	=[0.2929394634022157, 0.15616338745910055, -0.2077245490390469];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{8}	=[0.09018794684545176, 0.09272803040020376, 0.20579095518338572];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{9}	=[0.14362682832567877, 0.03748094015084701, -0.11944596792120704];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{9}	=[0.243731047197675,-0.40268,-0.295931];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{9}	=[0.17084408486046979, 0.09069200289106068, -0.10318097703172234];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{9}	=[0.08373469731361405, 0.09344146919577243, 0.2069144969578311];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{10}	=[0.04051483481230789, 0.0176971002675798, 0.1894093911119724];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{10}	=[0.110563030173172,-0.411997,-0.433929];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{10}	=[0.08545344687626498, 0.060471356526434074, 0.13802190808995723];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{10}	=[0.08529249733545165, 0.10293918146506761, 0.18553160135681113];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{11}	=[-0.011518733416506432, 0.14212945441937483, 0.5019327463986522];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{11}	=[-0.03,-0.140139,-0.459482];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{11}	=[0.03652818371266338, 0.12971950111110747, 0.4784869180433667];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{11}	=[0.09152158095142375, 0.11477082863972715, 0.2008626460870921];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{12}	=[0.0711727518722663, 0.3849018952662871, 0.6724417746210584];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{12}	=[0.111216754498642,0.140953,-0.119895];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{12}	=[0.09045548149783243, 0.3563962455369159, 0.6723752915078424];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{12}	=[0.13327722279433593, 0.15593702764531078, 0.17908092904891726];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{13}	=[0.3163426841283466, 0.4820398910337862, 0.6899039989195169];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{13}	=[0.316440636275286,0.397734,0.423836];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{13}	=[0.30581009271087384, 0.5059226567154517, 0.6151421708942952];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{13}	=[0.1625410484337461, 0.13591253516154866, 0.26774407351025753];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{14}	=[0.522613613690725, 0.5120658200221135, 0.5483084044280248];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{14}	=[0.622622799609719,0.757555,0.79817];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{14}	=[0.5150589428282553, 0.48250476382416746, 0.4881501298238378];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{14}	=[0.14439207079337538, 0.18924629708121085, 0.22023443795598116];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{15}	=[0.7751135243650628, 0.5334303233252042, 0.270294689354934];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{15}	=[0.85143743355238,0.956983,1.0795];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{15}	=[0.6914742391387343, 0.47082686088788633, 0.26047133116248883];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{15}	=[0.16529809671657197, 0.18504926613692077, 0.18535998922710276];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{16}	=[0.8142431213171347, 0.560982373893668, -7.788914551022617E-4];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{16}	=[0.958249109261345,0.988025,1.18727];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{16}	=[0.7526540853097924, 0.4953612214803831, -0.0012563673047015647];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{16}	=[0.1941877145485843, 0.16825074214472896, 0.14865376075663364];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{17}	=[0.7093416994325886, 0.4487090963671657, -0.11393094741100823];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{17}	=[0.909461902913392,0.803495,1.06131];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{17}	=[0.6418988457842294, 0.42698623738449837, -0.08942266188179473];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{17}	=[0.19468524140245794, 0.14893406116745084, 0.16321861104936583];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{18}	=[0.5562770359119711, 0.3693786322508937, -0.2581530612447442];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{18}	=[0.65660624202,0.559506,0.768153];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{18}	=[0.5043848511088722, 0.334332774292987, -0.20704237097751263];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{18}	=[0.17276090355600762, 0.15254568273358501, 0.16064219240373293];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{19}	=[0.3928169536169888, 0.2571711347173343, -0.3312875707058703];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{19}	=[0.493212624260681,0.316366,0.475723];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{19}	=[0.3650331431408228, 0.23491815112007092, -0.3203456463887941];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{19}	=[0.14687981532626074, 0.15042776030338326, 0.15977071909885776];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{20}	=[0.24746505464711788, 0.11858310162212718, -0.32351838672393196];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{20}	=[0.347660891049175,0.0983913,0.23709];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{20}	=[0.2298997808729571, 0.10183371498436994, -0.3178584608946449];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{20}	=[0.12961137036262935, 0.13808971075937743, 0.17146545809142175];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{21}	=[0.1281232537656632, 0.011084366471660848, -0.20350967891651442];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{21}	=[0.228409322409243,-0.0385162,0.0394848];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{21}	=[0.11435188561727927, 5.1739998570236435E-5, -0.17038182839204996];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{21}	=[0.1241477448711263, 0.1240169812386467, 0.2085228074339946];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{22}	=[0.024040744648406717, -0.028111118312337032, 0.07366204247417574];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{22}	=[0.124109480980681,-0.0117263,-0.094982];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{22}	=[0.01808674921598946, -0.011377354586368085, 0.05860966540230875];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{22}	=[0.11604281024765475, 0.1277530500409269, 0.28849248077208534];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{23}	=[-0.04182938878274063, 0.05468363390434472, 0.409425939457514];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{23}	=[-0.066653768355225,0.354549,-0.0706808];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{23}	=[-0.02903397930103034, 0.08864160466457538, 0.3261248925222116];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{23}	=[0.11004122403813052, 0.1987351136608842, 0.26130774919441646];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{24}	=[-0.00508883604642399, 0.31625736250381475, 0.620328122047231];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{24}	=[-0.034987799551524,0.685381,0.445142];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{24}	=[0.03504077526186669, 0.23262178544516368, 0.6006803385339143];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{24}	=[0.17370362922454025, 0.20934199781760401, 0.22579386642979693];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{25}	=[0.24286189151628224, 0.43824570815254216, 0.6872983356046756];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{25}	=[0.142932268595065,1.06396,1.04925];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{25}	=[0.18453339156455953, 0.41992420034869316, 0.7101045459884543];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{25}	=[0.19849576022749527, 0.1651906552247577, 0.19875234669453834];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{26}	=[0.4289643589239772, 0.48733084615323663, 0.5967203104868948];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{26}	=[0.329001883357458,1.53689,1.53314];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{26}	=[0.44781719430705186, 0.5107640579273619, 0.6174610016204213];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{26}	=[0.23129225818402938, 0.13638004668531456, 0.223314755417877];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{27}	=[0.7179810520368647, 0.5080992341573772, 0.362065676540808];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{27}	=[0.697991277594757,1.8517,1.94685];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{27}	=[0.6486799901954785, 0.5550784492029202, 0.346501012060097];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{27}	=[0.20912813420938003, 0.19622172276254224, 0.2384286503973949];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{28}	=[0.8053545732393419, 0.584329849305224, 0.06391708508140184];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{28}	=[0.785361083558286,2.0082,2.185];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{28}	=[0.7922909693222828, 0.49418365645947326, 0.03213823784572694];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{28}	=[0.18448170803275868, 0.22952390697467967, 0.21685575835688095];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{29}	=[0.7476503986005665, 0.47077694833215655, -0.06601137658777897];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{29}	=[0.727656933976138,1.87543,2.15514];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{29}	=[0.7303612598347698, 0.41495218603727296, -0.1873553977412713];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{29}	=[0.1915073557537799, 0.18402344079485503, 0.17476642395340983];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{30}	=[0.5984479025401295, 0.39135116744582343, -0.2193145937972338];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{30}	=[0.498548157166975,1.58743,1.82862];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{30}	=[0.5775199499946955, 0.34169055265017656, -0.3028796910134841];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{30}	=[0.1660120562029738, 0.13677558260244468, 0.1414058812691259];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{31}	=[0.4430923540938818, 0.29251277688260124, -0.3126976478838087];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{31}	=[0.343264999340227,1.28194,1.42898];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{31}	=[0.41169297445767405, 0.2567931706672722, -0.384127837149923];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{31}	=[0.16837041502183728, 0.13332277289034933, 0.14973534844014447];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{32}	=[0.2862233372307033, 0.161232663263135, -0.33103096993418035];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{32}	=[0.186272651083282,0.972264,1.11373];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{32}	=[0.2523343598427326, 0.12754615325994262, -0.3684511783559958];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{32}	=[0.13993247868637745, 0.1398665944275545, 0.1626262228660511];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{33}	=[0.16255595959024424, 0.038366239410342395, -0.2511951196205169];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{33}	=[0.062622389655705,0.733404,0.845989];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{33}	=[0.1268336209468933, 0.005929189021436672, -0.29054883036742035];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{33}	=[0.12139609941603759, 0.1297326440888013, 0.16418381979711785];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{34}	=[0.05325631611321724, -0.024079375018726944, -0.038001217425695955];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{34}	=[-0.053192168856411,0.590532,0.628656];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{34}	=[0.02153083781579613, -0.05113211551496653, -0.056941566622967225];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{34}	=[0.11736137980813766, 0.12678564884704002, 0.1740005755519814];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{35}	=[-0.027349915232363364, 0.007432777424402635, 0.30670462210575644];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{35}	=[-0.14375335753163,0.603104,0.498969];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{35}	=[-0.052884612932101, -0.014232265251619592, 0.32814010404852584];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{35}	=[0.11551856717960826, 0.11044180497983369, 0.23545907430259994];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{36}	=[-0.03627885883959763, 0.23354152985521034, 0.5565517081679439];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{36}	=[-0.246022089254433,0.841517,0.553519];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{36}	=[-0.05467051753467816, 0.23068578114430088, 0.559624245373811];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{36}	=[0.10095242082862221, 0.17608404128649124, 0.22228776810939185];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{37}	=[0.16145669366023518, 0.4029498457000514, 0.6685944104719821];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{37}	=[0.121576347921177,1.07246,0.978757];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{37}	=[0.15349951608080142, 0.3895788980254149, 0.6602299982661741];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{37}	=[0.15054093854939937, 0.14588510292484655, 0.23450980285469644];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{38}	=[0.35503947279645026, 0.46919498904309553, 0.6281968634562486];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{38}	=[0.455127993074255,1.31407,1.42847];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{38}	=[0.3477542493116097, 0.4554651667249924, 0.5630474962324133];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{38}	=[0.14006651094081718, 0.15488084347244258, 0.2201684164616327];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{39}	=[0.6289370954250927, 0.4892698937007607, 0.4398991318819666];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{39}	=[0.728976646438423,1.53828,1.67664];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{39}	=[0.6123769094996215, 0.4494594664767948, 0.32874414550996767];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{39}	=[0.20160215381891108, 0.16890234255827452, 0.2004641918925531];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{40}	=[0.7809978271968868, 0.559903009426744, 0.14557245865513702];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{40}	=[0.881033116731799,1.50633,1.72157];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{40}	=[0.7253523838415878, 0.47667208946114087, 0.09017103376485425];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{40}	=[0.1828566045752882, 0.18990970099214474, 0.18364339648609973];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{41}	=[0.7700712917772554, 0.4987106259335852, -0.036202946570673356];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{41}	=[0.870074634039716,1.26277,1.51791];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{41}	=[0.6880585113307647, 0.4341414419650167, -0.022198496813301046];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{41}	=[0.1678155921867011, 0.15604388365700228, 0.17226096851067044];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{42}	=[0.6372178773395163, 0.4103916491810966, -0.17021998273075467];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{42}	=[0.737268510660378,0.965657,1.17052];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{42}	=[0.5669048682017607, 0.3797871201820533, -0.1812177667196672];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{42}	=[0.16227556865658938, 0.14795177003917342, 0.15363040862715813];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{43}	=[0.48816919807479164, 0.32309092747523904, -0.2841459927471666];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{43}	=[0.58842429511432,0.649957,0.84286];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{43}	=[0.42450705226497853, 0.2807236870837866, -0.3140970899316484];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{43}	=[0.14503237964670768, 0.13078174792405753, 0.1406416709503346];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{44}	=[0.3270625165259268, 0.20167673683858345, -0.32687832130000494];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{44}	=[0.427040594118595,0.357047,0.54925];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{44}	=[0.28549457791788874, 0.14358112216923308, -0.3844422431599499];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{44}	=[0.12441306929388092, 0.12141058038082639, 0.17134899019476074];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{45}	=[0.1964673692286788, 0.0703680167467542, -0.28236214157453665];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{45}	=[0.296536161615293,0.104238,0.283848];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{45}	=[0.15939688275177583, 0.023117485544462087, -0.3841817154815934];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{45}	=[0.11445169866912591, 0.12405896158540582, 0.16650144004478987];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{46}	=[0.08400723803532656, -0.01173809070970699, -0.1212479470586702];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{46}	=[0.18396046048499,-0.0870496,0.0492151];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{46}	=[0.04720627608190325, -0.06404852721038728, -0.1634067956399337];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{46}	=[0.10807077863685363, 0.12392948882567491, 0.18641860763930268];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{47}	=[-0.00726874933759793, -0.016858739641917246, 0.18987040066854521];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{47}	=[-0.007319618606549,-0.174015,-0.136759];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{47}	=[-0.049604347044086505, -0.053873661567873966, 0.13207792299532203];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{47}	=[0.10968052768920823, 0.1100270307794692, 0.2684031288294471];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
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
% computation for member (20):
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\estuary_map.nc
% - member 0
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\estuary_map.nc
% - member 1
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\estuary_map.nc
% - member 2
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\estuary_map.nc
% - member 3
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\estuary_map.nc
% - member 4
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\estuary_map.nc
% - member 5
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\estuary_map.nc
% - member 6
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\estuary_map.nc
% - member 7
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\estuary_map.nc
% - member 8
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\estuary_map.nc
% - member 9
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\estuary_map.nc
% - member 10
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\estuary_map.nc
% - member 11
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\estuary_map.nc
% - member 12
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\estuary_map.nc
% - member 13
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\estuary_map.nc
% - member 14
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\estuary_map.nc
% - member 15
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\estuary_map.nc
% - member 16
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\estuary_map.nc
% - member 17
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\estuary_map.nc
% - member 18
% FileCopier: copying file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_map.nc to C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\estuary_map.nc
% - member 19
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
% DFlowFMRestartFileWrapper: no time specified, reading values for the last time index
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
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f_central{48}	=[-0.04594582587580048, 0.1350300637023537, 0.47737383546280976];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{48}	=[-0.045794542637673,-0.0735941,-0.232202];
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work1\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work2\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work3\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work4\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work5\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work6\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work7\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work8\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work9\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work10\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work11\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work12\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work13\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work14\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work15\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work16\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work17\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work18\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work19\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% NetcdfDataObject: station_id variable found in netcdf file C:\selfTraining\New folder (2)\Developer\Etuary of mine\.\stochModel\.\.\work20\dflowfm\DFM_OUTPUT_estuary\estuary_his.nc
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: 
 
pred_f{48}	=[-0.0803417866414463, 0.11029644135512828, 0.3799847484760927];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: 
 
pred_f_std{48}	=[0.10124189499322767, 0.193953513652674, 0.19709803283595942];
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station01.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station02.waterlevel'.
% Getting model values at observed coordinates for scalar observation exchangeItem with id 'station03.waterlevel'.
% Application Done
