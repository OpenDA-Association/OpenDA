% OpenDA version (Development)
% opening :c:\OpenDA\public_new_master\public\model_dflowfm_blackbox\tests\simple_waal_calibration_roughness\.\stochObserver\noosObservations.xml
% NoosStochObserver[i]=TimeSeries(
%   Location = Obs03
%   Position = (200.0,2.0)
%   Height   = NaN
%   Quantity = waterlevel
%   Unit     = m
%   Source   = model
%   Id       = Obs03.waterlevel
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = most recent
%   standarddeviation  = 0.05
%   status  = assimilation
%   Values   = 
%   (48865.0=199208310000,0.0)
%   (48865.00347222222=199208310005,1.7287E-18)
%   (48865.006944444445=199208310010,1.81577E-13)
%   (48865.010416666664=199208310015,1.0739E-10)
%   (48865.01388888889=199208310020,1.06504E-8)
%   ...
%   (48874.98611111111=199209092340,3.57331)
%   (48874.989583333336=199209092345,3.57331)
%   (48874.993055555555=199209092350,3.57331)
%   (48874.99652777778=199209092355,3.57331)
%   (48875.0=199209100000,3.57331)
%
%   Values.length()=2881
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = Obs02
%   Position = (134.0,2.0)
%   Height   = NaN
%   Quantity = waterlevel
%   Unit     = m
%   Source   = model
%   Id       = Obs02.waterlevel
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = most recent
%   standarddeviation  = 0.05
%   status  = assimilation
%   Values   = 
%   (48865.0=199208310000,0.0)
%   (48865.00347222222=199208310005,0.0)
%   (48865.006944444445=199208310010,0.0)
%   (48865.010416666664=199208310015,0.0)
%   (48865.01388888889=199208310020,0.0)
%   ...
%   (48874.98611111111=199209092340,0.0924516340949433)
%   (48874.989583333336=199209092345,0.092525069456263)
%   (48874.993055555555=199209092350,0.0926037941007544)
%   (48874.99652777778=199209092355,0.0926985586426059)
%   (48875.0=199209100000,0.0927541080946812)
%
%   Values.length()=2880
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = Obs01
%   Position = (67.0,2.0)
%   Height   = NaN
%   Quantity = waterlevel
%   Unit     = m
%   Source   = model
%   Id       = Obs01.waterlevel
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = most recent
%   standarddeviation  = 0.05
%   status  = assimilation
%   Values   = 
%   (48865.0=199208310000,0.0)
%   (48865.00347222222=199208310005,0.0)
%   (48865.006944444445=199208310010,0.0)
%   (48865.010416666664=199208310015,9.90606E-11)
%   (48865.01388888889=199208310020,3.74446E-6)
%   ...
%   (48874.98611111111=199209092340,5.32529)
%   (48874.989583333336=199209092345,5.3252)
%   (48874.993055555555=199209092350,5.32528)
%   (48874.99652777778=199209092355,5.32538)
%   (48875.0=199209100000,5.32522)
%
%   Values.length()=2881
%)
% Starting Algorithm: 
%	className: org.openda.algorithms.Dud
%	dir.: c:\OpenDA\public_new_master\public\model_dflowfm_blackbox\tests\simple_waal_calibration_roughness\.\algorithm
%	config.: dudAlgorithm.xml
%  Algorithm  Dud initialisation (stoch. obs. and stoch. model have been set)
%  Algorithm  configString = dudAlgorithm.xml
% opening :c:\OpenDA\public_new_master\public\model_dflowfm_blackbox\tests\simple_waal_calibration_roughness\.\algorithm\dudAlgorithm.xml
%  Algorithm  Retrieving initial parameters from model
% Create new BBModelInstance with number: 0
% Instance initialization done
%  Algorithm  Starting optimizer
%  Algorithm  costFunction@class=org.openda.algorithms.SimulationKwadraticCostFunction
% Create new BBModelInstance with number: 1
% Instance initialization done
% CostFunction = SimulationKwadraticCostFunction
%  Algorithm  costFunction@weakParameterConstraint=false
%  Algorithm  costFunction@factor=0.5
%  Algorithm  costFunction@biasRemoval=false
%  Algorithm  costFunction@stdRemoval=false
%  Algorithm  costFunction@tryParallel=false
%  Algorithm  outerLoop@maxIterations=10
%  Algorithm  outerLoop@absTolerance=0.01
%  Algorithm  outerLoop@relTolerance=0.01
%  Algorithm  outerLoop@relToleranceLinearCost=0.01
%  Algorithm  lineSearch@maxIterations=5
%  Algorithm  lineSearch@maxRelStepSize=10.0
%  Algorithm  lineSearch/backtracking@startIterationNegativeLook=3
%  Algorithm  lineSearch/backtracking@shorteningFactor=0.5
%  Algorithm  constraints@parameterConstraint=false
%  Algorithm  constraints/lowerbounds@Lbounds=null
%  Algorithm  constraints/upperbounds@Ubounds=null
% Application initializing finished
% Initializing Algorithm
% Evaluating with parameters 
% Params - containing:
%    friction_1= 0
%    friction_2= 0
%    friction_3= 0
% ========================================================================
% prepare no 1
% Create new BBModelInstance with number: 2
% Instance initialization done
%  resultItem id: evaluatedParameters, outputLevel: Essential, context: any
%  Params: friction_1, friction_2, friction_3
evaluatedParameters{1}	=[0.0, 0.0, 0.0];
% NOTE:
%    Model failed: OutputCheck failed file does not exist: c:\OpenDA\public_new_master\public\model_dflowfm_blackbox\tests\simple_waal_calibration_roughness\.\stochModel\.\.\work2\DFM_OUTPUT_simple_waal\simple_waal_his.nc
% Error running algorithm step.
% Error message: OutputCheck failed file does not exist: c:\OpenDA\public_new_master\public\model_dflowfm_blackbox\tests\simple_waal_calibration_roughness\.\stochModel\.\.\work2\DFM_OUTPUT_simple_waal\simple_waal_his.nc
% Error type :RuntimeException
% Stacktrace :java.lang.RuntimeException: OutputCheck failed file does not exist: c:\OpenDA\public_new_master\public\model_dflowfm_blackbox\tests\simple_waal_calibration_roughness\.\stochModel\.\.\work2\DFM_OUTPUT_simple_waal\simple_waal_his.nc
%	at org.openda.blackbox.config.BBCheckOutput.performCheckOnFile(BBCheckOutput.java:60)
%	at org.openda.blackbox.config.BBCheckOutput.performCheck(BBCheckOutput.java:51)
%	at org.openda.blackbox.config.BBAction.run(BBAction.java:118)
%	at org.openda.blackbox.wrapper.BBModelInstance.compute(BBModelInstance.java:252)
%	at org.openda.blackbox.wrapper.BBStochModelInstance.compute(BBStochModelInstance.java:358)
%	at org.openda.algorithms.SimulationKwadraticCostFunction.prepare(SimulationKwadraticCostFunction.java:159)
%	at org.openda.algorithms.SimulationKwadraticCostFunction.evaluate(SimulationKwadraticCostFunction.java:318)
%	at org.openda.algorithms.BaseDudCoreOptimizer.initialize(BaseDudCoreOptimizer.java:310)
%	at org.openda.algorithms.BaseDud.prepare(BaseDud.java:377)
%	at org.openda.application.ApplicationRunnerSingleThreaded.runSingleThreaded(ApplicationRunnerSingleThreaded.java:76)
%	at org.openda.application.OpenDaApplication.runApplicationBatch(OpenDaApplication.java:114)
%	at org.openda.application.OpenDaApplication.main(OpenDaApplication.java:102)
%
% Application Done
