% Starting Algorithm: 
%	className: org.openda.algorithms.Dud
%	dir.: /home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./algorithm
%	config.: dudAlgorithm.xml
%  Algorithm  Dud initialisation (stoch. obs. and stoch. model have been set)
%  Algorithm  configString = dudAlgorithm.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./algorithm/dudAlgorithm.xml
%  Algorithm  Retrieving initial parameters from model
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
%  Algorithm  Starting optimizer
%  Algorithm  costFunction@class=org.openda.algorithms.SimulationKwadraticCostFunction
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
%  Algorithm  costFunction@weakParameterConstraint=false
%  Algorithm  costFunction@factor=0.5
%  Algorithm  costFunction@tryParallel=false
%  Algorithm  outerLoop@maxIterations=10
%  Algorithm  outerLoop@absTolerance=0.01
%  Algorithm  outerLoop@relTolerance=0.01
%  Algorithm  outerLoop@relToleranceLinearCost=0.01
%  Algorithm  lineSearch@maxIterations=5
%  Algorithm  lineSearch@maxRelStepSize=10.0
%  Algorithm  lineSearch/backtracking@startIterationNegativeLook=3
%  Algorithm  lineSearch/backtracking@shorteningFactor=0.5
% Application initializing finished
% Initializing application
% Evaluating with parameters  [8,1.571]
% ========================================================================
% prepare no 1
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{1}	=[8.0,1.5708];
% ========================================================================
% evaluate no. 1
predicted{1}	=[0.059874050823311174,-0.622514741258897,-0.05209975910799278,0.4843573470865686,0.04483119930100093,-0.3768240910010292,-0.03822558598991682,0.29313512040223755,0.03234578622602441,-0.2280097266602124];
observed{1}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{1}	=[-0.10300849000705582,-0.008407429022448842,0.22805168831317807,-0.018591390128484264,-0.2901175989177405,0.05884531247445479,0.30590623973760833,-0.09851447303582975,-0.29030932920303837,0.12998862887590798];
obs_stdev{1}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{1}	=17.756295868749987;
costTotal{1}	=17.756295868749987;
% SimulationKwadraticCostFunction: evaluation 1 : cost = 17.756
% Evaluating with parameters  [9,1.571]
% ========================================================================
% prepare no 2
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{2}	=[9.0,1.5708];
% ========================================================================
% evaluate no. 2
predicted{2}	=[0.0535786279459891,-0.6402131222275624,-0.04740700352235454,0.5123091801872715,0.041565211398558684,-0.4099327838549162,-0.03616714183080783,0.32799413193914145,0.03126817887015455,-0.26241723932575955];
observed{2}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{2}	=[-0.09671306712973375,0.009290951946216541,0.22335893272753982,-0.046543223229187136,-0.28685161101529827,0.0919540053283418,0.30384779557849934,-0.13337348457273365,-0.2892317218471685,0.16439614154145513];
obs_stdev{2}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{2}	=18.651382948941716;
costTotal{2}	=18.651382948941716;
% SimulationKwadraticCostFunction: evaluation 2 : cost = 18.651
% Evaluating with parameters  [8,1.696]
% ========================================================================
% prepare no 3
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{3}	=[8.0,1.6965];
% ========================================================================
% evaluate no. 3
predicted{3}	=[-0.03350311927911278,-0.6158978936059812,0.15740421970211982,0.4461027450327285,-0.21769751450176725,-0.30101131585066443,0.23371991318597032,0.1845979073251928,-0.22137835983632012,-0.09656628411831196];
observed{3}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{3}	=[-0.009631319904631869,-0.015024276675364656,0.018547709503065474,0.019663211925355806,-0.027588885114972345,-0.016967462675909983,0.03396074056172119,0.010022740041215006,-0.03658518314069384,-0.0014548136659924688];
obs_stdev{3}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{3}	=0.23462856704877713;
costTotal{3}	=0.23462856704877713;
% SimulationKwadraticCostFunction: evaluation 3 : cost = 0.235
% Application initialized
% Application starting next step
% ======================================================
% DUD outer iteration no.1
% ======================================================
% -----------------------------------------------------
costs{1}	=[0.23462856704877713,17.756295868749987,18.651382948941716];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [8.663,1.71]
%   delta_p=    [0.663,0.014]
% ========================================================================
% prepare no 4
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{4}	=[8.663227729470336,1.710474267579495];
% ========================================================================
% evaluate no. 4
predicted{4}	=[-0.04871533545298321,-0.623329637530464,0.18902524808841653,0.4492358459183649,-0.25841736087328293,-0.29429346004168633,0.2761308470105584,0.16701975973254954,-0.2594931715055899,-0.06999665346208907];
observed{4}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{4}	=[0.005580896269238564,-0.007592532750881897,-0.013073318883231239,0.01653011103971941,0.013130961256543339,-0.023685318484888085,-0.00845019326286689,0.027600887633858262,0.0015296285285759659,-0.02802444432221536];
obs_stdev{4}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{4}	=0.1443644828480898;
costTotal{4}	=0.1443644828480898;
% SimulationKwadraticCostFunction: evaluation 4 : cost = 0.144
linearCost{1}	=0.013400084664938843;
% linearized cost rel error0.5919870568741853 < 0.01
% Error estimate for this outer iteration
parameterErrorEstimateStd{1}	=[1.5059320861162961,0.023763314363807318];
parameterErrorCorrelations{1}	=[0.9999999999999998,0.23509589433394898;0.235095894333949,0.9999999999999998];
% stop criterium1: imain > maxit 1<10.0
% stop criterium2: diff < abstol 0.09026408420068732>0.01
% stop criterium3: diff < reltol 0.09026408420068732>0.001443644828480898
% Application starting next step
% ======================================================
% DUD outer iteration no.2
% ======================================================
% -----------------------------------------------------
costs{2}	=[0.1443644828480898,0.23462856704877713,17.756295868749987];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [9.057,1.699]
%   delta_p=    [0.393,-0.011]
% ========================================================================
% prepare no 5
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{5}	=[9.056627185141869,1.6993903683204152];
% ========================================================================
% evaluate no. 5
predicted{5}	=[-0.04303478974611255,-0.6318717397479763,0.17559631802055448,0.467458631810538,-0.2451795903906122,-0.320086756598632,0.2680687689166381,0.19680560214609127,-0.25889538627262,-0.10000077505517663];
observed{5}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{5}	=[-9.964943763209749E-5,9.495694666303844E-4,3.556111846308163E-4,-0.0016926748524536683,-1.0680922612738009E-4,0.0021079780720575925,-3.8811516894660114E-4,-0.0021849547796834734,9.318432956060274E-4,0.001979677270872199];
obs_stdev{5}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{5}	=9.03515710710213E-4;
costTotal{5}	=9.03515710710213E-4;
% SimulationKwadraticCostFunction: evaluation 5 : cost = 9.035E-4
linearCost{2}	=0.08238681481185198;
% linearized cost rel error1.3147203127019744 < 0.01
% Error estimate for this outer iteration
parameterErrorEstimateStd{2}	=[4.465558725976933,0.043147579493820955];
parameterErrorCorrelations{2}	=[1.0,-0.84465641048109;-0.8446564104810932,1.0];
% stop criterium1: imain > maxit 2<10.0
% stop criterium2: diff < abstol 0.1434609671373796>0.01
% stop criterium3: diff < reltol 0.1434609671373796>9.035157107102131E-6
% Application starting next step
% ======================================================
% DUD outer iteration no.3
% ======================================================
% -----------------------------------------------------
costs{3}	=[9.03515710710213E-4,0.1443644828480898,0.23462856704877713];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [9.004,1.7]
%   delta_p=    [-0.052,6.186E-4]
% ========================================================================
% prepare no 6
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{6}	=[9.004484712448104,1.7000089455533827];
% ========================================================================
% evaluate no. 6
predicted{6}	=[-0.04316935722993649,-0.630983746773088,0.17601944806912898,0.4658499358078916,-0.24539107920893427,-0.3180553199030434,0.26781443378625003,0.1946708325308591,-0.258112416349844,-0.09803619732105781];
observed{6}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{6}	=[3.4918046191843854E-5,6.157649174209912E-5,-6.7518863943683E-5,-8.397884980726467E-5,1.046795921946786E-4,7.654137646900816E-5,-1.337800385585175E-4,-5.018516445129406E-5,1.4887337283003843E-4,1.5099536753382381E-5];
obs_stdev{6}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{6}	=3.812275220890606E-6;
costTotal{6}	=3.812275220890606E-6;
% SimulationKwadraticCostFunction: evaluation 6 : cost = 3.812E-6
linearCost{3}	=2.344137399272678E-8;
% linearized cost rel error0.004193543182922271 < 0.01
% Error estimate for this outer iteration
parameterErrorEstimateStd{3}	=[1.574360951603447,0.020367356266850184];
parameterErrorCorrelations{3}	=[1.0,-0.11806660474049042;-0.11806660474049056,0.9999999999999998];
% stop criterium1: imain > maxit 3<10.0
% stop criterium2: diff < abstol 8.997034354893224E-4>0.01
% stop criterium3: diff < reltol 8.997034354893224E-4>3.812275220890606E-8
% ===================================================================
% SimulationKwadraticCostfunction: optimal results
%     number of evaluations: 6
%     all cost values:
%         [17.756,18.651,0.235,0.144,9.035E-4,3.812E-6]
%     all parameter values
%         [8,9,8,8.663,9.057,9.004;1.571,1.571,1.696,1.71,1.699,1.7]
%     number of observations: 10
%     best cost:
%         cost = 3.812E-6
%     best parameters:
%                 [9.004,1.7]
% ===================================================================
% Application Done
