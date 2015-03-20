% Starting Algorithm: 
%	className: org.openda.algorithms.Dud
%	dir.: /home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./algorithm
%	config.: dudAlgorithm_withConstraint.xml
%  Algorithm  Dud initialisation (stoch. obs. and stoch. model have been set)
%  Algorithm  configString = dudAlgorithm_withConstraint.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./algorithm/dudAlgorithm_withConstraint.xml
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
%  Algorithm  costFunction@weakParameterConstraint=true
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
costWeakConstraintPenalty{1}	=0.0;
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
costWeakConstraintPenalty{2}	=0.5;
% SimulationKwadraticCostFunction: evaluation 2 : cost = 19.151
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
costWeakConstraintPenalty{3}	=0.49999999999999933;
% SimulationKwadraticCostFunction: evaluation 3 : cost = 0.735
% Application initialized
% Application starting next step
% ======================================================
% DUD outer iteration no.1
% ======================================================
% -----------------------------------------------------
costs{1}	=[0.7346285670487764,17.756295868749987,19.151382948941716];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [8.181,1.704]
%   delta_p=    [0.181,7.683E-3]
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
evaluatedParameters{4}	=[8.181223185630179,1.7041826399382376];
% ========================================================================
% evaluate no. 4
predicted{4}	=[-0.040585015190801446,-0.617260465069167,0.17231898046685054,0.4441596511798952,-0.23607224568967303,-0.2940847884695359,0.2518305352496267,0.17296948062693956,-0.23659613078999128,-0.08158946365726215];
observed{4}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{4}	=[-0.002549423992943202,-0.01366170521217891,0.0036329487383347514,0.021606305778189117,-0.009214153927066565,-0.023893990057038517,0.015850118498064836,0.021651166739468236,-0.021367412187022677,-0.01643163412704228];
obs_stdev{4}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{4}	=0.13877800462959186;
costTotal{4}	=0.13877800462959186;
costWeakConstraintPenalty{4}	=0.5794075325813892;
% SimulationKwadraticCostFunction: evaluation 4 : cost = 0.718
linearCost{1}	=0.6630387622607283;
% linearized cost rel error0.7703160403010277 < 0.01
% Error estimate for this outer iteration
parameterErrorEstimateStd{1}	=[0.8328151714485218,0.022912708345870118];
parameterErrorCorrelations{1}	=[1.0000000000000002,0.1304360258558542;0.13043602585585348,1.0000000000000002];
% stop criterium1: imain > maxit 1<10.0
% stop criterium2: diff < abstol 0.01644302983779533>0.01
% stop criterium3: diff < reltol 0.01644302983779533>0.007181855372109811
% Application starting next step
% ======================================================
% DUD outer iteration no.2
% ======================================================
% -----------------------------------------------------
costs{2}	=[0.7181855372109811,0.7346285670487764,17.756295868749987];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [7.962,1.704]
%   delta_p=    [-0.219,-3.818E-4]
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
evaluatedParameters{5}	=[7.961949948333727,1.7038008458733018];
% ========================================================================
% evaluate no. 5
predicted{5}	=[-0.03859998220633975,-0.6135806114218177,0.16857998886036066,0.4392192248935886,-0.230314713056045,-0.28964804911735265,0.24456133618499878,0.1700838716211395,-0.228644287956656,-0.08067260840244983];
observed{5}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{5}	=[-0.004534456977404899,-0.01734155885952815,0.0073719403448246346,0.026546732064495704,-0.014971686560694586,-0.028330729409221767,0.023119317562692726,0.024536775745268302,-0.029319255020357954,-0.017348489381854598];
obs_stdev{5}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{5}	=0.22021460430798723;
costTotal{5}	=0.22021460430798723;
costWeakConstraintPenalty{5}	=0.5604921446759399;
% SimulationKwadraticCostFunction: evaluation 5 : cost = 0.781
linearCost{2}	=0.6858492121132808;
% linearized cost rel error2.9334668235813943 < 0.01
% Cost is worse! Reducing stepsize.
% Next try p=   [7.962,1.704]
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
evaluatedParameters{6}	=[8.071586566981953,1.7039917429057698];
% ========================================================================
% evaluate no. 6
predicted{6}	=[-0.03960323697611687,-0.615443710143527,0.17046518113922332,0.441718690180447,-0.23321417181968676,-0.2918926121855984,0.24821812623777348,0.171546698764899,-0.2326403699359549,-0.08114542602248047];
observed{6}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{6}	=[-0.00353120220762778,-0.01547846013781895,0.005486748065961972,0.0240472667776373,-0.012072227797052831,-0.02608616634097599,0.01946252750991803,0.023073948601508798,-0.02532317304105905,-0.01687567176182396];
obs_stdev{6}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{6}	=0.1761951395428017;
costTotal{6}	=0.1761951395428017;
costWeakConstraintPenalty{6}	=0.5639385913724938;
% SimulationKwadraticCostFunction: evaluation 6 : cost = 0.74
% Cost is worse! Reducing stepsize.
% Next try p=   [8.072,1.704]
% ========================================================================
% prepare no 7
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{7}	=[8.126404876306067,1.7040871914220037];
% ========================================================================
% evaluate no. 7
predicted{7}	=[-0.040096759625573734,-0.6163577769315697,0.17139594372272027,0.44294637244758434,-0.23464832005987013,-0.29299516681970605,0.25002983579671795,0.17226304465289932,-0.23462327919672163,-0.0813710093155019];
observed{7}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{7}	=[-0.0030376795581709137,-0.014564393349776217,0.004555985482465025,0.02281958451049998,-0.010638079556869462,-0.02498361170686836,0.017650817950973563,0.02235760271350848,-0.02334026378029233,-0.016650088468802532];
obs_stdev{7}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{7}	=0.15667981075286708;
costTotal{7}	=0.15667981075286708;
costWeakConstraintPenalty{7}	=0.5701702501628986;
% SimulationKwadraticCostFunction: evaluation 7 : cost = 0.727
% Cost is worse! Reducing stepsize.
% Next try p=   [8.126,1.704]
% ========================================================================
% prepare no 8
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{8}	=[8.153814030968123,1.7041349156801207];
% ========================================================================
% evaluate no. 8
predicted{8}	=[-0.04034153955622652,-0.616810530645544,0.17185842030798168,0.44355479858856656,-0.23536155306054646,-0.2935415841709101,0.2509315564558574,0.17261749509651286,-0.2356109606791507,-0.08148112350523785];
observed{8}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{8}	=[-0.002792899627518129,-0.014111639635801865,0.004093508897203613,0.02221115836951776,-0.009924846556193134,-0.024437194355664327,0.016749097291834103,0.022003152269894938,-0.02235258229786327,-0.016539974279066574];
obs_stdev{8}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{8}	=0.14752948671631708;
costTotal{8}	=0.14752948671631708;
costWeakConstraintPenalty{8}	=0.5744131884186331;
% SimulationKwadraticCostFunction: evaluation 8 : cost = 0.722
% Cost is worse! Reducing stepsize.
% Next try p=   [8.154,1.704]
% ========================================================================
% prepare no 9
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{9}	=[8.16751860829915,1.7041587778091791];
% ========================================================================
% evaluate no. 9
predicted{9}	=[-0.04046343963943663,-0.6170358486993843,0.17208893900930328,0.44385766988438496,-0.23571721597180115,-0.2938135866980617,0.25138138791921216,0.17279379519400312,-0.23610385945913687,-0.08153551482521328];
observed{9}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{9}	=[-0.0026709995443080156,-0.01388632158196157,0.0038629901958820156,0.02190828707369935,-0.009569183644938445,-0.024165191828512733,0.016299265828479348,0.02182685217240468,-0.021859683517877093,-0.016485582959091152];
obs_stdev{9}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{9}	=0.1431041712223045;
costTotal{9}	=0.1431041712223045;
costWeakConstraintPenalty{9}	=0.5768164347616335;
% SimulationKwadraticCostFunction: evaluation 9 : cost = 0.72
% Cost is worse! Reducing stepsize.
% Start looking in negative direction
% Next try p=   [8.168,1.704]
% ========================================================================
% prepare no 10
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{10}	=[8.188075474295694,1.7041945710027668];
% ========================================================================
% evaluate no. 10
predicted{10}	=[-0.04064568174556219,-0.6173725110967817,0.17243382280808633,0.444310309130109,-0.2362495236925543,-0.29422008985214104,0.2520548527817894,0.17305709333148023,-0.23684203128761683,-0.08161627245882472];
observed{10}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{10}	=[-0.0024887574381824568,-0.013549659184564189,0.003518106397098958,0.02145564782797532,-0.009036875924185284,-0.02375868867443337,0.015625800965902104,0.021563554034927573,-0.021121511689397132,-0.01640482532547971];
obs_stdev{10}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{10}	=0.13665192781946003;
costTotal{10}	=0.13665192781946003;
costWeakConstraintPenalty{10}	=0.5807735257950504;
% SimulationKwadraticCostFunction: evaluation 10 : cost = 0.717
% Error estimate for this outer iteration
parameterErrorEstimateStd{2}	=[0.9147542422068329,0.023616526887482676];
parameterErrorCorrelations{2}	=[0.9999999999999999,-0.27336849857847256;-0.2733684985784741,1.0000000000000002];
% stop criterium1: imain > maxit 2<10.0
% stop criterium2: diff < abstol 7.600835964707287E-4>0.01
% stop criterium3: diff < reltol 7.600835964707287E-4>0.007174254536145104
% Application starting next step
% ======================================================
% DUD outer iteration no.3
% ======================================================
% -----------------------------------------------------
costs{3}	=[0.7174254536145104,0.7181855372109811,0.7346285670487764];
% -----------------------------------------------------
% Start search until improvement,
% % Reducing stepsize! Relative step was:[-42.298170141638295,0.9197755546265522]
% Next try p=   [8.216,1.703]
%   delta_p=    [0.028,-1.554E-3]
% ========================================================================
% prepare no 11
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{11}	=[8.215701267033372,1.7026406938274077];
% ========================================================================
% evaluate no. 11
predicted{11}	=[-0.039703186398383,-0.618204888135759,0.17030909949120038,0.4462852685305665,-0.23398843500033947,-0.2971754009374379,0.25033749549897233,0.17660286304318265,-0.23600793011515964,-0.08530866869924443];
observed{11}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{11}	=[-0.0034312527853616465,-0.012717282145586872,0.005642829713984915,0.019480688427517823,-0.011297964616400119,-0.02080337758913653,0.01734315824871918,0.01801778432322515,-0.021955612861854323,-0.012712429085059998];
obs_stdev{11}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{11}	=0.12071732562844331;
costTotal{11}	=0.12071732562844331;
costWeakConstraintPenalty{11}	=0.5733087557859622;
% SimulationKwadraticCostFunction: evaluation 11 : cost = 0.694
linearCost{3}	=0.6951921607915978;
% linearized cost rel error0.052447443861786916 < 0.01
% Error estimate for this outer iteration
parameterErrorEstimateStd{3}	=[0.836758898774683,0.02133288200013846];
parameterErrorCorrelations{3}	=[0.9999999999999999,-0.1036962352801259;-0.10369623528012756,1.0000000000000002];
% stop criterium1: imain > maxit 3<10.0
% stop criterium2: diff < abstol 0.023399372200104906>0.01
% stop criterium3: diff < reltol 0.023399372200104906>0.0069402608141440544
% Application starting next step
% ======================================================
% DUD outer iteration no.4
% ======================================================
% -----------------------------------------------------
costs{4}	=[0.6940260814144055,0.7174254536145104,0.7181855372109811];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [8.302,1.698]
%   delta_p=    [0.086,-5.034E-3]
% ========================================================================
% prepare no 12
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{12}	=[8.301889759008672,1.6976064748747468];
% ========================================================================
% evaluate no. 12
predicted{12}	=[-0.03661158821002161,-0.6207967522786081,0.16330506229456168,0.4525030383801186,-0.22640460231397605,-0.30656457796937187,0.2443693720433963,0.1879764707461662,-0.23278915406790643,-0.09728600472992852];
observed{12}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{12}	=[-0.006522850973723036,-0.010125418002737807,0.012646866910623611,0.013262918577965688,-0.018881797302763542,-0.011414200557202547,0.023311281704295223,0.00664417662024161,-0.02517438890910753,-7.350930543759127E-4];
obs_stdev{12}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{12}	=0.1094788663281297;
costTotal{12}	=0.1094788663281297;
costWeakConstraintPenalty{12}	=0.5544099603141736;
% SimulationKwadraticCostFunction: evaluation 12 : cost = 0.664
linearCost{4}	=0.6613771345578974;
% linearized cost rel error0.07693026349195294 < 0.01
% Error estimate for this outer iteration
parameterErrorEstimateStd{4}	=[0.8346164839567266,0.020819687060756];
parameterErrorCorrelations{4}	=[1.0,-0.08656827312629964;-0.08656827312629264,1.0];
% stop criterium1: imain > maxit 4<10.0
% stop criterium2: diff < abstol 0.030137254772102162>0.01
% stop criterium3: diff < reltol 0.030137254772102162>0.006638888266423033
% Application starting next step
% ======================================================
% DUD outer iteration no.5
% ======================================================
% -----------------------------------------------------
costs{5}	=[0.6638888266423033,0.6940260814144055,0.7174254536145104];
% -----------------------------------------------------
% Start search until improvement,
% % Reducing stepsize! Relative step was:[150.90796509308427,-115.27742847122788]
% Next try p=   [8.309,1.698]
%   delta_p=    [7.534E-3,1.623E-5]
% ========================================================================
% prepare no 13
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{13}	=[8.309423365766943,1.6976227071405214];
% ========================================================================
% evaluate no. 13
predicted{13}	=[-0.03667876872468904,-0.6209171137129531,0.16343120412993073,0.4526661098788737,-0.22659699843993963,-0.306713806385064,0.24461205348270026,0.1880776776482617,-0.23305621473797794,-0.09732458325430075];
observed{13}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{13}	=[-0.00645567045905561,-0.01000505656839279,0.012520725075254563,0.013099847079210636,-0.018689401176799958,-0.011264972141510399,0.023068600264991246,0.006542969718146108,-0.02490732823903602,-6.965145300036751E-4];
obs_stdev{13}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{13}	=0.10710878025169852;
costTotal{13}	=0.10710878025169852;
costWeakConstraintPenalty{13}	=0.55684293667779;
% SimulationKwadraticCostFunction: evaluation 13 : cost = 0.664
linearCost{5}	=0.6580908373204173;
% linearized cost rel error1.0108469132476334 < 0.01
% Cost is worse! Reducing stepsize.
% Next try p=   [8.309,1.698]
% ========================================================================
% prepare no 14
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{14}	=[8.298122955629536,1.6975983587418595];
% ========================================================================
% evaluate no. 14
predicted{14}	=[-0.03657796314178147,-0.6207364944776522,0.16324194214490675,0.4524214023754667,-0.2263083396924198,-0.3064898697709588,0.24424796120651832,0.187925790627354,-0.2326555583148111,-0.09726665589534843];
observed{14}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{14}	=[-0.0065564760419631815,-0.010185675803693672,0.012709987060278538,0.01334455458261763,-0.01897805992431978,-0.01148890875561559,0.023432692541173195,0.006694856739053789,-0.025307984662202854,-7.544418889560006E-4];
obs_stdev{14}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{14}	=0.11067467586726722;
costTotal{14}	=0.11067467586726722;
costWeakConstraintPenalty{14}	=0.5532147615973507;
% SimulationKwadraticCostFunction: evaluation 14 : cost = 0.664
% Cost is worse! Reducing stepsize.
% Next try p=   [8.298,1.698]
% ========================================================================
% prepare no 15
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{15}	=[8.30377316069824,1.6976105329411904];
% ========================================================================
% evaluate no. 15
predicted{15}	=[-0.03662839203584624,-0.6208268618965379,0.16333661005492786,0.45254383130578385,-0.22645271747862897,-0.30660190856105757,0.24443005992803915,0.18800179163218517,-0.2328559355887536,-0.09729566424987028];
observed{15}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{15}	=[-0.00650604714789841,-0.010095308384807988,0.012615319150257431,0.013222125652300465,-0.018833682138110625,-0.011376869965516845,0.02325059381965236,0.006618855734222634,-0.025107607388260372,-7.254335344341473E-4];
obs_stdev{15}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{15}	=0.10888365523838044;
costTotal{15}	=0.10888365523838044;
costWeakConstraintPenalty{15}	=0.5550128820388303;
% SimulationKwadraticCostFunction: evaluation 15 : cost = 0.664
% Cost is worse! Reducing stepsize.
% Next try p=   [8.304,1.698]
% ========================================================================
% prepare no 16
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{16}	=[8.300948058163888,1.697604445841525];
% ========================================================================
% evaluate no. 16
predicted{16}	=[-0.03660318412073239,-0.6207816926504578,0.16328928533657258,0.4524826356497269,-0.22638054069592908,-0.3065459067978842,0.24433902371818703,0.18796380551061578,-0.2327557592187054,-0.09728117124615168];
observed{16}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{16}	=[-0.006531255063012255,-0.0101404776308881,0.01266264386861271,0.01328332130835741,-0.01890585892081051,-0.01143287172869023,0.023341630029504484,0.006656841855792017,-0.025207783758308566,-7.399265381527464E-4];
obs_stdev{16}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{16}	=0.10977714503590573;
costTotal{16}	=0.10977714503590573;
costWeakConstraintPenalty{16}	=0.5541098300434059;
% SimulationKwadraticCostFunction: evaluation 16 : cost = 0.664
% Error estimate for this outer iteration
parameterErrorEstimateStd{5}	=[0.514923271634258,0.019614033479651963];
parameterErrorCorrelations{5}	=[1.0000000000000002,-0.6481573850379302;-0.6481573850366199,1.0];
% stop criterium1: imain > maxit 5<10.0
% stop criterium2: diff < abstol 1.8515629917059684E-6>0.01
% stop criterium3: diff < reltol 1.8515629917059684E-6>0.006638869750793116
% Application starting next step
% ======================================================
% DUD outer iteration no.6
% ======================================================
% -----------------------------------------------------
costs{6}	=[0.6638869750793116,0.6638888266423033,0.6940260814144055];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [8.299,1.698]
%   delta_p=    [-2.272E-3,2.203E-4]
% ========================================================================
% prepare no 17
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{17}	=[8.298675968730981,1.6978246999748794];
% ========================================================================
% evaluate no. 17
predicted{17}	=[-0.03674984351945504,-0.620694574990531,0.1636179295435025,0.4522490486053666,-0.22675033921634952,-0.30617320679438587,0.24465263659255668,0.18749477173681767,-0.23295863892668833,-0.09677098945062153];
observed{17}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{17}	=[-0.006384595664289609,-0.01022759529081485,0.012333999661682804,0.013516908352717738,-0.018536060400390075,-0.011805571732188547,0.023028017155134833,0.007125875629590134,-0.025004904050325627,-0.0012501083336829005];
obs_stdev{17}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{17}	=0.10855169301937947;
costTotal{17}	=0.10855169301937947;
costWeakConstraintPenalty{17}	=0.5551977817523599;
% SimulationKwadraticCostFunction: evaluation 17 : cost = 0.664
linearCost{6}	=0.6638290689652802;
% linearized cost rel error1.3745386799316124 < 0.01
% Error estimate for this outer iteration
parameterErrorEstimateStd{6}	=[0.8368061980730954,0.020817630901336356];
parameterErrorCorrelations{6}	=[1.0,-0.07111327501193611;-0.07111327501193777,1.0000000000000002];
% stop criterium1: imain > maxit 6<10.0
% stop criterium2: diff < abstol 1.3750030757220255E-4>0.01
% stop criterium3: diff < reltol 1.3750030757220255E-4>0.006637494747717394
% ===================================================================
% SimulationKwadraticCostfunction: optimal results
%     number of evaluations: 17
%     all cost values:
%         [17.756,19.151,0.735,0.718,0.781,0.74,0.727,0.722,0.72,0.717,0.694,0.664,0.664,0.664,0.664,0.664,0.664]
%     all parameter values
%         [8,9,8,8.181,7.962,8.072,8.126,8.154,8.168,8.188,8.216,8.302,8.309,8.298,8.304,8.301,8.299;1.571,1.571,1.696,1.704,1.704,1.704,1.704,1.704,1.704,1.704,1.703,1.698,1.698,1.698,1.698,1.698,1.698]
%     number of observations: 10
%     best cost:
%         cost = 0.664
%     best parameters:
%                 [8.299,1.698]
% ===================================================================
% Application Done
