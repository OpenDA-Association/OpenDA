% OpenDA version (Development)
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochObserver\noosObservations.xml
% NoosStochObserver[i]=TimeSeries(
%   Location = STATION01
%   Position = (10.0,3.0)
%   Height   = NaN
%   Quantity = waterlevel
%   Unit     = m
%   Source   = observed
%   Id       = S1.obs
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.01
%   status  = assimilation
%   Values   = 
%   (55371.0=201006240000,0.0)
%   (55371.006944444445=201006240010,9.67100146226585E-4)
%   (55371.01388888889=201006240020,0.0130689451470971)
%   (55371.020833333336=201006240030,0.0680139064788818)
%   (55371.02777777778=201006240040,0.195170596241951)
%   ...
%   (55371.97222222222=201006242320,-1.16998505592346)
%   (55371.979166666664=201006242330,-1.09515416622162)
%   (55371.98611111111=201006242340,-1.0075706243515)
%   (55371.993055555555=201006242350,-0.911334872245789)
%   (55372.0=201006250000,-0.815616965293884)
%
%   Values.length()=145
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = STATION02
%   Position = (30.0,3.0)
%   Height   = NaN
%   Quantity = waterlevel
%   Unit     = m
%   Source   = observed
%   Id       = S2.obs
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.01
%   status  = assimilation
%   Values   = 
%   (55371.0=201006240000,0.0)
%   (55371.006944444445=201006240010,4.80017342852079E-6)
%   (55371.01388888889=201006240020,1.15550581540447E-4)
%   (55371.020833333336=201006240030,0.00124155497178435)
%   (55371.02777777778=201006240040,0.00797272194176912)
%   ...
%   (55371.97222222222=201006242320,-1.35375535488129)
%   (55371.979166666664=201006242330,-1.34409284591675)
%   (55371.98611111111=201006242340,-1.31861007213593)
%   (55371.993055555555=201006242350,-1.2746741771698)
%   (55372.0=201006250000,-1.20938920974731)
%
%   Values.length()=145
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = STATION03
%   Position = (50.0,3.0)
%   Height   = NaN
%   Quantity = waterlevel
%   Unit     = m
%   Source   = observed
%   Id       = S3.obs
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.01
%   status  = assimilation
%   Values   = 
%   (55371.0=201006240000,0.0)
%   (55371.006944444445=201006240010,2.38246808947906E-8)
%   (55371.01388888889=201006240020,8.24864343940135E-7)
%   (55371.020833333336=201006240030,1.32920376927359E-5)
%   (55371.02777777778=201006240040,1.32875517010689E-4)
%   ...
%   (55371.97222222222=201006242320,-1.41139578819275)
%   (55371.979166666664=201006242330,-1.44611239433289)
%   (55371.98611111111=201006242340,-1.4709233045578)
%   (55371.993055555555=201006242350,-1.48361349105835)
%   (55372.0=201006250000,-1.48111832141876)
%
%   Values.length()=145
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = STATION04
%   Position = (70.0,3.0)
%   Height   = NaN
%   Quantity = waterlevel
%   Unit     = m
%   Source   = observed
%   Id       = S4.obs
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.01
%   status  = assimilation
%   Values   = 
%   (55371.0=201006240000,0.0)
%   (55371.006944444445=201006240010,1.18513726610203E-10)
%   (55371.01388888889=201006240020,5.35674304913414E-9)
%   (55371.020833333336=201006240030,1.14936909767493E-7)
%   (55371.02777777778=201006240040,1.55966711190558E-6)
%   ...
%   (55371.97222222222=201006242320,-1.43370485305786)
%   (55371.979166666664=201006242330,-1.48428237438202)
%   (55371.98611111111=201006242340,-1.5282176733017)
%   (55371.993055555555=201006242350,-1.56422936916351)
%   (55372.0=201006250000,-1.59064984321594)
%
%   Values.length()=145
%)
% Starting Algorithm: 
%	className: org.openda.algorithms.kalmanFilter.EnKF
%	dir.: d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\algorithm
%	config.: EnkfAlgorithm.xml
% configstring = EnkfAlgorithm.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\algorithm\EnkfAlgorithm.xml
% analysisTimes@type=fixed
% analysisTimes@type=fixed
% mainModel@stochParameter=false
% mainModel@stochForcing=false
% mainModel@stochInit=false
% this.ensembleSize=32
% ensembleModel@stochParameter=false
% ensembleModel@stochForcing=true
% ensembleModel@stochInit=true
% saveGain/times@type=none
% Creating mainModel
% Create new BBModelInstance with number: 0
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Do not add noise to forcing
% model time 55371.0 55372.0
% analysisTimes acquired from MODEL:55371.0 -- 55372.0
% Creating ensemble model 0
% Create new BBModelInstance with number: 1
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 1
% Create new BBModelInstance with number: 2
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 2
% Create new BBModelInstance with number: 3
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 3
% Create new BBModelInstance with number: 4
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 4
% Create new BBModelInstance with number: 5
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 5
% Create new BBModelInstance with number: 6
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 6
% Create new BBModelInstance with number: 7
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 7
% Create new BBModelInstance with number: 8
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 8
% Create new BBModelInstance with number: 9
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 9
% Create new BBModelInstance with number: 10
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 10
% Create new BBModelInstance with number: 11
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 11
% Create new BBModelInstance with number: 12
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 12
% Create new BBModelInstance with number: 13
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 13
% Create new BBModelInstance with number: 14
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 14
% Create new BBModelInstance with number: 15
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 15
% Create new BBModelInstance with number: 16
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 16
% Create new BBModelInstance with number: 17
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 17
% Create new BBModelInstance with number: 18
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 18
% Create new BBModelInstance with number: 19
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 19
% Create new BBModelInstance with number: 20
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 20
% Create new BBModelInstance with number: 21
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 21
% Create new BBModelInstance with number: 22
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 22
% Create new BBModelInstance with number: 23
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 23
% Create new BBModelInstance with number: 24
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 24
% Create new BBModelInstance with number: 25
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 25
% Create new BBModelInstance with number: 26
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 26
% Create new BBModelInstance with number: 27
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 27
% Create new BBModelInstance with number: 28
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 28
% Create new BBModelInstance with number: 29
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 29
% Create new BBModelInstance with number: 30
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 30
% Create new BBModelInstance with number: 31
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 31
% Create new BBModelInstance with number: 32
% Instance initialization done
% configstring = boundaryNoise.xml
% opening :d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\boundaryNoise.xml
% analysisTimes=201006240000,201006240010,...,201006250000
%    Add noise to initial state
%    Add noise to forcing
% Application initializing finished
% Initializing Algorithm
% Algorithm initialized
% Algorithm starting next step
% Skip Analysis at initial time
%
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201006240000UTC  to 201006240100UTC  (55371.0-->55371.041666666664) 
%
% ========================================================================
%
% D3dNetcdfHisDataObject: writing data to file d:\Data\OpenDA_Issues\2020\DELFT3DSUP9908\Marcela\openda_2.4\examples\model_delft3d\d3d_estuary_EnKF_marcela\.\stochModel\.\.\work\work0\trih-Est1D.nc
