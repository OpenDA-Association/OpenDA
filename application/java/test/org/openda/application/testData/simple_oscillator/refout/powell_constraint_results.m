% Starting Algorithm: 
%	className: org.openda.algorithms.Powell
%	dir.: /home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./algorithm
%	config.: powellAlgorithm_withConstraint.xml
% configstring = powellAlgorithm_withConstraint.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./algorithm/powellAlgorithm_withConstraint.xml
% costFunction@class=org.openda.algorithms.SimulationKwadraticCostFunction
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
% costFunction@weakParameterConstraint=true
% costFunction@factor=0.5
% outerLoop@maxIterations=10
% outerLoop@absTolerance=0.01
% outerLoop@relTolerance=0.01
% lineSearch@maxIterations=10
% lineSearch@maxRelStepSize=100.0
% lineSearch/brent@startBracketValue=1.0
% lineSearch@relTolerance=0.01
% lineSearch@absTolerance=0.0010
% Application initializing finished
% Initializing application
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
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
% Application initialized
% Application starting next step
% =================================================================
% Outer loop iteration no. 1
% =================================================================
p_start{1}	=[8.0,1.5708];
f_p_start{1}	=17.756295868749987;
% direction 0
% direction 0
searchDir{1}	=[1.0,0.0];
% Start line optimization in point [8.0,1.5708] in direction:[1.0,0.0]
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
evaluatedParameters{2}	=[8.0,1.5708];
% ========================================================================
% evaluate no. 2
predicted{2}	=[0.059874050823311174,-0.622514741258897,-0.05209975910799278,0.4843573470865686,0.04483119930100093,-0.3768240910010292,-0.03822558598991682,0.29313512040223755,0.03234578622602441,-0.2280097266602124];
observed{2}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{2}	=[-0.10300849000705582,-0.008407429022448842,0.22805168831317807,-0.018591390128484264,-0.2901175989177405,0.05884531247445479,0.30590623973760833,-0.09851447303582975,-0.29030932920303837,0.12998862887590798];
obs_stdev{2}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{2}	=17.756295868749987;
costTotal{2}	=17.756295868749987;
costWeakConstraintPenalty{2}	=0.0;
% SimulationKwadraticCostFunction: evaluation 2 : cost = 17.756
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
evaluatedParameters{3}	=[9.0,1.5708];
% ========================================================================
% evaluate no. 3
predicted{3}	=[0.0535786279459891,-0.6402131222275624,-0.04740700352235454,0.5123091801872715,0.041565211398558684,-0.4099327838549162,-0.03616714183080783,0.32799413193914145,0.03126817887015455,-0.26241723932575955];
observed{3}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{3}	=[-0.09671306712973375,0.009290951946216541,0.22335893272753982,-0.046543223229187136,-0.28685161101529827,0.0919540053283418,0.30384779557849934,-0.13337348457273365,-0.2892317218471685,0.16439614154145513];
obs_stdev{3}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{3}	=18.651382948941716;
costTotal{3}	=18.651382948941716;
costWeakConstraintPenalty{3}	=0.5;
% SimulationKwadraticCostFunction: evaluation 3 : cost = 19.151
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
evaluatedParameters{4}	=[6.381966,1.5708];
% ========================================================================
% evaluate no. 4
predicted{4}	=[0.07392469442771454,-0.5837861878453423,-0.06185437993020622,0.4259019784281821,0.05091707132981887,-0.3106397960275624,-0.041378718967410084,0.2265143100007068,0.03327992153638219,-0.1651295426334458];
observed{4}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{4}	=[-0.1170591336114592,-0.04713598243600359,0.23780630913539152,0.0398639785299022,-0.29620347094655847,-0.007338982499011992,0.3090593727151016,-0.03189366263429899,-0.29124346451339617,0.06710844484914139];
obs_stdev{4}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{4}	=17.385858824346535;
costTotal{4}	=17.385858824346535;
costWeakConstraintPenalty{4}	=1.3090170125779996;
% SimulationKwadraticCostFunction: evaluation 4 : cost = 18.695
% ---bracket 1.0,0.0,-1.618034
% --- f values 19.151382948941716,17.756295868749987,18.694875836924535
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
evaluatedParameters{5}	=[8.0,1.5708];
% ========================================================================
% evaluate no. 5
predicted{5}	=[0.059874050823311174,-0.622514741258897,-0.05209975910799278,0.4843573470865686,0.04483119930100093,-0.3768240910010292,-0.03822558598991682,0.29313512040223755,0.03234578622602441,-0.2280097266602124];
observed{5}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{5}	=[-0.10300849000705582,-0.008407429022448842,0.22805168831317807,-0.018591390128484264,-0.2901175989177405,0.05884531247445479,0.30590623973760833,-0.09851447303582975,-0.29030932920303837,0.12998862887590798];
obs_stdev{5}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{5}	=17.756295868749987;
costTotal{5}	=17.756295868749987;
costWeakConstraintPenalty{5}	=0.0;
% SimulationKwadraticCostFunction: evaluation 5 : cost = 17.756
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
evaluatedParameters{6}	=[7.381966025156,1.5708];
% ========================================================================
% evaluate no. 6
predicted{6}	=[0.0645617435829447,-0.6094762479324758,-0.05546557932849853,0.46426247066173093,0.04704473436649417,-0.35359834150549496,-0.03949188747476485,0.26927531645936365,0.032870132041405764,-0.2050323427890559];
observed{6}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{6}	=[-0.10769618276668935,-0.021445922348870106,0.2314175085336838,0.0015034862963533846,-0.29233113398323374,0.03561956297892055,0.30717254122245635,-0.07465466909295584,-0.2908336750184197,0.10701124500475148];
obs_stdev{6}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{6}	=17.41524435191253;
costTotal{6}	=17.41524435191253;
costWeakConstraintPenalty{6}	=0.1909829970307369;
% SimulationKwadraticCostFunction: evaluation 6 : cost = 17.606
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
evaluatedParameters{7}	=[7.000000015547263,1.5708];
% ========================================================================
% evaluate no. 7
predicted{7}	=[0.06784418758853712,-0.6004167542268303,-0.057756328530377425,0.45054821480298163,0.04848495117867335,-0.33802995235263095,-0.04024846767980133,0.2535679116339529,0.033106169181408185,-0.19017714967781263];
observed{7}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{7}	=[-0.11097862677228176,-0.03050541605451562,0.23370825773556272,0.015217742155102687,-0.29377135079541294,0.020051173826056534,0.30792912142749285,-0.058947264267545124,-0.29106971215842214,0.0921560518935082];
obs_stdev{7}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{7}	=17.31555302816486;
costTotal{7}	=17.31555302816486;
costWeakConstraintPenalty{7}	=0.4999999844527371;
% SimulationKwadraticCostFunction: evaluation 7 : cost = 17.816
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
evaluatedParameters{8}	=[7.5374648034495335,1.5708];
% ========================================================================
% evaluate no. 8
predicted{8}	=[0.06331459512623203,-0.6129335186923043,-0.0545809105199468,0.46954972119000055,0.046473899651087607,-0.3596619601387016,-0.03917661452998368,0.2754558439916278,0.032752668738720944,-0.21093754321263397];
observed{8}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{8}	=[-0.10644903430997668,-0.017988651589041615,0.23053283972513208,-0.0037837642319162312,-0.2917602992678272,0.041683181612127196,0.3068572682776752,-0.08083519662522001,-0.2907162117157349,0.11291644542832954];
obs_stdev{8}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{8}	=17.481900346198422;
costTotal{8}	=17.481900346198422;
costWeakConstraintPenalty{8}	=0.10696940402398933;
% SimulationKwadraticCostFunction: evaluation 8 : cost = 17.589
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
evaluatedParameters{9}	=[7.5325492463107375,1.5708];
% ========================================================================
% evaluate no. 9
predicted{9}	=[0.06335328211737029,-0.6128261470120387,-0.054608471263487106,0.46938507027083076,0.04649180348461528,-0.3594726189058187,-0.03918662767294355,0.27526232702282055,0.032756548160703496,-0.21075214332504805];
observed{9}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{9}	=[-0.10648772130111495,-0.01809602326930715,0.2305604004686724,-0.003619113312746447,-0.2917782031013549,0.04149384037924431,0.3068672814206351,-0.08064167965641275,-0.29072009113771746,0.11273104554074362];
obs_stdev{9}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{9}	=17.479581230505833;
costTotal{9}	=17.479581230505833;
costWeakConstraintPenalty{9}	=0.10925510356232977;
% SimulationKwadraticCostFunction: evaluation 9 : cost = 17.589
% ---linesearch -0.618033974844,-0.46745075368926275,-0.4625351965504667
% --- f values 17.606227348943268,17.588836334068162,17.58886975022241
p_dir{1}	=[7.5325492463107375,1.5708];
f_p_dir{1}	=17.588836334068162;
% direction 1
% direction 1
searchDir{2}	=[0.0,0.1257];
% Start line optimization in point [7.5325492463107375,1.5708] in direction:[0.0,0.1257]
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
evaluatedParameters{10}	=[7.5325492463107375,1.5708];
% ========================================================================
% evaluate no. 10
predicted{10}	=[0.06335328211737029,-0.6128261470120387,-0.054608471263487106,0.46938507027083076,0.04649180348461528,-0.3594726189058187,-0.03918662767294355,0.27526232702282055,0.032756548160703496,-0.21075214332504805];
observed{10}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{10}	=[-0.10648772130111495,-0.01809602326930715,0.2305604004686724,-0.003619113312746447,-0.2917782031013549,0.04149384037924431,0.3068672814206351,-0.08064167965641275,-0.29072009113771746,0.11273104554074362];
obs_stdev{10}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{10}	=17.479581230505833;
costTotal{10}	=17.479581230505833;
costWeakConstraintPenalty{10}	=0.10925510356232977;
% SimulationKwadraticCostFunction: evaluation 10 : cost = 17.589
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
evaluatedParameters{11}	=[7.5325492463107375,1.6965];
% ========================================================================
% evaluate no. 11
predicted{11}	=[-0.029621837086496565,-0.6072131699318484,0.15055043442944593,0.43392296384060175,-0.20679727186110375,-0.28920039348340015,0.21945969290147674,0.17555961221096114,-0.20524469175281335,-0.09141119348660742];
observed{11}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{11}	=[-0.013512602097248083,-0.023709000349497522,0.02540149477573936,0.031842993117482565,-0.038489127755635844,-0.02877838504317426,0.04822096084621477,0.01906103515544666,-0.052718851224200614,-0.006609904297697006];
obs_stdev{11}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{11}	=0.5112539948484649;
costTotal{11}	=0.5112539948484649;
costWeakConstraintPenalty{11}	=0.6092551035623291;
% SimulationKwadraticCostFunction: evaluation 11 : cost = 1.121
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
evaluatedParameters{12}	=[7.5325492463107375,1.8998868737999999];
% ========================================================================
% evaluate no. 12
predicted{12}	=[-0.1768099417600231,-0.5147361022132367,0.42295853306150505,0.15856691034706477,-0.412857996541515,0.10890872974806222,0.2557804127668101,-0.22631541133188876,-0.06978297543075343,0.2125013881032259];
observed{12}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{12}	=[0.13367550257627844,-0.11618606806810916,-0.24700660385631976,0.30719904661101954,0.1675715969247754,-0.4268875082746366,0.011900240980881438,0.42093605869829653,-0.18818056754626053,-0.31052248588753034];
obs_stdev{12}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{12}	=35.311498264900514;
costTotal{12}	=35.311498264900514;
costWeakConstraintPenalty{12}	=3.5363061161403286;
% SimulationKwadraticCostFunction: evaluation 12 : cost = 38.848
% ---bracket 0.0,1.0,2.6180339999999998
% --- f values 17.588836334068162,1.120509098410794,38.847804381040845
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
evaluatedParameters{13}	=[7.5325492463107375,1.6965];
% ========================================================================
% evaluate no. 13
predicted{13}	=[-0.029621837086496565,-0.6072131699318484,0.15055043442944593,0.43392296384060175,-0.20679727186110375,-0.28920039348340015,0.21945969290147674,0.17555961221096114,-0.20524469175281335,-0.09141119348660742];
observed{13}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{13}	=[-0.013512602097248083,-0.023709000349497522,0.02540149477573936,0.031842993117482565,-0.038489127755635844,-0.02877838504317426,0.04822096084621477,0.01906103515544666,-0.052718851224200614,-0.006609904297697006];
obs_stdev{13}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{13}	=0.5112539948484649;
costTotal{13}	=0.5112539948484649;
costWeakConstraintPenalty{13}	=0.6092551035623291;
% SimulationKwadraticCostFunction: evaluation 13 : cost = 1.121
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
evaluatedParameters{14}	=[7.5325492463107375,1.7741868706378907];
% ========================================================================
% evaluate no. 14
predicted{14}	=[-0.08655287975087152,-0.583569604494827,0.26783027565792955,0.3550274428256599,-0.3279383695469762,-0.15902802700787136,0.3063668472828759,0.0161802957976178,-0.24051150702863558,0.07062238846041649];
observed{14}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{14}	=[0.04341844056712687,-0.04735256578651892,-0.09187834645274426,0.11073851413242441,0.0826519699302366,-0.15895075151870305,-0.03868619353518438,0.17844035156879,-0.01745203594837838,-0.16864348624472092];
obs_stdev{14}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{14}	=5.950577235922828;
costTotal{14}	=5.950577235922828;
costWeakConstraintPenalty{14}	=1.4182720754370657;
% SimulationKwadraticCostFunction: evaluation 14 : cost = 7.369
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
evaluatedParameters{15}	=[7.5325492463107375,1.6484868738];
% ========================================================================
% evaluate no. 15
predicted{15}	=[0.005822456493840682,-0.6141863783225242,0.07327586238378363,0.46169044636954043,-0.11462722276392291,-0.3395207286121701,0.1308723098505959,0.2437833327672788,-0.13121128051166034,-0.17032786737150976];
observed{15}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{15}	=[-0.04895689567758533,-0.01673579195882169,0.10267606682140167,0.004075510588543885,-0.1306591768528167,0.021541950085595674,0.13680834389709562,-0.04916268540087099,-0.12675226246535362,0.07230676958720533];
obs_stdev{15}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{15}	=3.6599811262302597;
costTotal{15}	=3.6599811262302597;
costWeakConstraintPenalty{15}	=0.3002381161403301;
% SimulationKwadraticCostFunction: evaluation 15 : cost = 3.96
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
evaluatedParameters{16}	=[7.5325492463107375,1.6991260606543832];
% ========================================================================
% evaluate no. 16
predicted{16}	=[-0.0315559373876011,-0.6066614894889447,0.15469534018505535,0.43191894217670795,-0.21153152697366706,-0.2856983656921585,0.22366093656504057,0.1709655681110837,-0.20828194254536228,-0.08629525443712746];
observed{16}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{16}	=[-0.011578501796143545,-0.02426068079240118,0.021256589020129946,0.03384701478137636,-0.03375487264307253,-0.032280412834415906,0.044019717182650936,0.02365507925532409,-0.04968160043165168,-0.011725843347176967];
obs_stdev{16}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{16}	=0.48022884508574676;
costTotal{16}	=0.48022884508574676;
costWeakConstraintPenalty{16}	=0.6303648236745681;
% SimulationKwadraticCostFunction: evaluation 16 : cost = 1.111
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
evaluatedParameters{17}	=[7.5325492463107375,1.7007714950075727];
% ========================================================================
% evaluate no. 17
predicted{17}	=[-0.03276752139159761,-0.6063068790133701,0.15728723771210945,0.4306383249638738,-0.21447878901674483,-0.2834671862852179,0.22625407947334394,0.1680478841208552,-0.21012480321961005,-0.08305880731182977];
observed{17}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{17}	=[-0.010366917792147039,-0.024615291267975792,0.01866469149307584,0.03512763199421054,-0.03080761059999476,-0.034511592241356526,0.041426574274347566,0.026572763245552605,-0.04783873975740391,-0.01496229047247466];
obs_stdev{17}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{17}	=0.4685276808527461;
costTotal{17}	=0.4685276808527461;
costWeakConstraintPenalty{17}	=0.6438141430131162;
% SimulationKwadraticCostFunction: evaluation 17 : cost = 1.112
% ========================================================================
% prepare no 18
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{18}	=[7.5325492463107375,1.6978477031793453];
% ========================================================================
% evaluate no. 18
predicted{18}	=[-0.03061449253724917,-0.606932237800891,0.1526788797166797,0.4329006173548156,-0.2092315565593374,-0.28741222357004975,0.22162534453289437,0.17321162697227394,-0.20681810116928187,-0.08879341930471525];
observed{18}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{18}	=[-0.01251994664649548,-0.02398993248045489,0.023273049488505587,0.032865339603268706,-0.0360548430574022,-0.03056655495652466,0.04605530921479714,0.021409020394133865,-0.05114544180773209,-0.009227678479589177];
obs_stdev{18}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{18}	=0.49343706114612185;
costTotal{18}	=0.49343706114612185;
costWeakConstraintPenalty{18}	=0.6200341643113014;
% SimulationKwadraticCostFunction: evaluation 18 : cost = 1.113
% ========================================================================
% prepare no 19
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{19}	=[7.5325492463107375,1.700411156645318];
% ========================================================================
% evaluate no. 19
predicted{19}	=[-0.03250221198737714,-0.6063851247860599,0.1567199785354128,0.430920410154471,-0.21383463336243672,-0.28395821353298256,0.22568881060831292,0.168689380815834,-0.20972522523247444,-0.08376954226767075];
observed{19}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{19}	=[-0.010632227196367507,-0.024537045495286014,0.019231950669772485,0.03484554680361329,-0.031451766254302876,-0.03402056499359185,0.04199184313937859,0.02593126655057379,-0.048238317744539516,-0.01425155551663368];
obs_stdev{19}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{19}	=0.47057954835123617;
costTotal{19}	=0.47057954835123617;
costWeakConstraintPenalty{19}	=0.6408541845698539;
% SimulationKwadraticCostFunction: evaluation 19 : cost = 1.111
% ========================================================================
% prepare no 20
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{20}	=[7.5325492463107375,1.7004093549535066];
% ========================================================================
% evaluate no. 20
predicted{20}	=[-0.0325008854137206,-0.606385515185726,0.15671714174737622,0.43092181827267123,-0.21383141078321877,-0.28396066527364827,0.2256859805743395,0.16869258472993753,-0.20972322170039082,-0.08377309318595862];
observed{20}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{20}	=[-0.010633553770024048,-0.024536655095619864,0.01923478745780907,0.034844138685413084,-0.031454988833520825,-0.03401811325292614,0.04199467317335201,0.025928062636470267,-0.048240321276623144,-0.014248004598345812];
obs_stdev{20}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{20}	=0.470590527453315;
costTotal{20}	=0.470590527453315;
costWeakConstraintPenalty{20}	=0.6408394054246165;
% SimulationKwadraticCostFunction: evaluation 20 : cost = 1.111
% ---linesearch 1.0311150091115195,1.0208914928749666,1.0311006758433312
% --- f values 1.11143373292109,1.1105936687603148,1.1114299328779316
p_dir{2}	=[7.5325492463107375,1.6991260606543832];
f_p_dir{2}	=1.1105936687603148;
% End point of this outer loop has cost 1.1105936687603148 at [7.5325492463107375,1.6991260606543832]
% Relative convergence check for outer loop: 1.7645412262466935<0.01
% Absolute convergence check for outer loop: 16.64570219998967<0.01
% ========================================================================
% prepare no 21
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{21}	=[7.065098492621475,1.8274521213087664];
% ========================================================================
% evaluate no. 21
predicted{21}	=[-0.12033141864821917,-0.5508484540201478,0.3283353744671908,0.2733770771070535,-0.36533919381666985,-0.048348074032235705,0.29612840079343045,-0.09134011051792829,-0.18371066924271623,0.14808532354761658];
observed{21}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{21}	=[0.07719697946447451,-0.08007371626119808,-0.1523834452620055,0.19238887985103081,0.12005279419993026,-0.2696307044943387,-0.028447747045738936,0.28596075788433606,-0.07425287373429773,-0.246106421331921];
obs_stdev{21}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{21}	=15.419172429813168;
costTotal{21}	=15.419172429813168;
costWeakConstraintPenalty{21}	=2.5214592946982726;
% SimulationKwadraticCostFunction: evaluation 21 : cost = 17.941
% Candidate direction :[-0.4674507536892625,0.12832606065438323]
% Is new direction a good one,i.e 0.18433585576145362<0 and 1.317574128892443<0
% Application starting next step
% =================================================================
% Outer loop iteration no. 2
% =================================================================
p_start{2}	=[7.5325492463107375,1.6991260606543832];
f_p_start{2}	=1.1105936687603148;
% direction 0
% direction 0
searchDir{3}	=[0.46745075368926275,0.0];
% Start line optimization in point [7.5325492463107375,1.6991260606543832] in direction:[0.46745075368926275,0.0]
% ========================================================================
% prepare no 22
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{22}	=[7.5325492463107375,1.6991260606543832];
% ========================================================================
% evaluate no. 22
predicted{22}	=[-0.0315559373876011,-0.6066614894889447,0.15469534018505535,0.43191894217670795,-0.21153152697366706,-0.2856983656921585,0.22366093656504057,0.1709655681110837,-0.20828194254536228,-0.08629525443712746];
observed{22}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{22}	=[-0.011578501796143545,-0.02426068079240118,0.021256589020129946,0.03384701478137636,-0.03375487264307253,-0.032280412834415906,0.044019717182650936,0.02365507925532409,-0.04968160043165168,-0.011725843347176967];
obs_stdev{22}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{22}	=0.48022884508574676;
costTotal{22}	=0.48022884508574676;
costWeakConstraintPenalty{22}	=0.6303648236745681;
% SimulationKwadraticCostFunction: evaluation 22 : cost = 1.111
% ========================================================================
% prepare no 23
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{23}	=[8.0,1.6991260606543832];
% ========================================================================
% evaluate no. 23
predicted{23}	=[-0.03544544857629823,-0.61532027947412,0.16163211284506815,0.4440059175505932,-0.22259126473019866,-0.2973081666470502,0.23811307078801425,0.1796788819735912,-0.22457982424534306,-0.09101683298363962];
observed{23}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{23}	=[-0.007688990607446419,-0.015601890807225849,0.014319816360117144,0.021760039407491105,-0.02269513488654093,-0.020670611879524237,0.029567582959677263,0.014941765392816608,-0.0333837187316709,-0.007004264800664808];
obs_stdev{23}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{23}	=0.20922350444008866;
costTotal{23}	=0.20922350444008866;
costWeakConstraintPenalty{23}	=0.5211097201122383;
% SimulationKwadraticCostFunction: evaluation 23 : cost = 0.73
% ========================================================================
% prepare no 24
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{24}	=[8.756351212794852,1.6991260606543832];
% ========================================================================
% evaluate no. 24
predicted{24}	=[-0.04090833500183839,-0.6275789189002163,0.17158269478534816,0.46141759464235893,-0.23876379688615526,-0.3143034785710625,0.25963639141585493,0.19260590257989188,-0.24928713192030735,-0.09805144027124883];
observed{24}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{24}	=[-0.0022261041819062605,-0.003343251381129564,0.004369234419837137,0.004348362315725385,-0.0065226027305843315,-0.0036752999555119126,0.008044262331836582,0.0020147447865159207,-0.008676411056706607,3.0342486944404112E-5];
obs_stdev{24}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{24}	=0.012711694626205007;
costTotal{24}	=0.012711694626205007;
costWeakConstraintPenalty{24}	=0.8071432986603604;
% SimulationKwadraticCostFunction: evaluation 24 : cost = 0.82
% ---bracket 0.0,1.0,2.6180339999999998
% --- f values 1.1105936687603148,0.730333224552327,0.8198549932865654
% ========================================================================
% prepare no 25
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{25}	=[8.0,1.6991260606543832];
% ========================================================================
% evaluate no. 25
predicted{25}	=[-0.03544544857629823,-0.61532027947412,0.16163211284506815,0.4440059175505932,-0.22259126473019866,-0.2973081666470502,0.23811307078801425,0.1796788819735912,-0.22457982424534306,-0.09101683298363962];
observed{25}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{25}	=[-0.007688990607446419,-0.015601890807225849,0.014319816360117144,0.021760039407491105,-0.02269513488654093,-0.020670611879524237,0.029567582959677263,0.014941765392816608,-0.0333837187316709,-0.007004264800664808];
obs_stdev{25}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{25}	=0.20922350444008866;
costTotal{25}	=0.20922350444008866;
costWeakConstraintPenalty{25}	=0.5211097201122383;
% SimulationKwadraticCostFunction: evaluation 25 : cost = 0.73
% ========================================================================
% prepare no 26
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{26}	=[8.288900447346398,1.6991260606543832];
% ========================================================================
% evaluate no. 26
predicted{26}	=[-0.03764285117404126,-0.6202374993172651,0.16560544486573683,0.4509478657950699,-0.22900583548524509,-0.30404589379321445,0.24659521107064739,0.18477947214896157,-0.23425580510301117,-0.09378808540115958];
observed{26}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{26}	=[-0.0054915880097033895,-0.010684670964080767,0.010346484339448458,0.014818091163014413,-0.016280564131494507,-0.013932884733359963,0.021085442677044125,0.009841175217446235,-0.023707737874002793,-0.0042330123831448435];
obs_stdev{26}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{26}	=0.10257735966496616;
costTotal{26}	=0.10257735966496616;
costWeakConstraintPenalty{26}	=0.5628414543507128;
% SimulationKwadraticCostFunction: evaluation 26 : cost = 0.665
% ========================================================================
% prepare no 27
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{27}	=[8.467450746421683,1.6991260606543832];
% ========================================================================
% evaluate no. 27
predicted{27}	=[-0.03893026393051391,-0.6231269616564045,0.16795159514914199,0.4550534966046009,-0.23282051262289266,-0.3080546211970382,0.25167371699410673,0.18782931949028653,-0.2400872902409209,-0.09544782469935269];
observed{27}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{27}	=[-0.004204175253230739,-0.00779520862494143,0.008000334056043307,0.010712460353483388,-0.01246588699384693,-0.00992415732953622,0.016006936753584777,0.0067913278761212725,-0.017876252736093057,-0.002573273084951741];
obs_stdev{27}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{27}	=0.05698080304764425;
costTotal{27}	=0.05698080304764425;
costWeakConstraintPenalty{27}	=0.6303648202773326;
% SimulationKwadraticCostFunction: evaluation 27 : cost = 0.687
% ========================================================================
% prepare no 28
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{28}	=[8.2959122086154,1.6991260606543832];
% ========================================================================
% evaluate no. 28
predicted{28}	=[-0.03769439259545765,-0.6203530567858211,0.16569911340468063,0.45111168575717236,-0.22915775009731165,-0.3042055076726437,0.24679696973651477,0.1849006909220334,-0.23448693645961238,-0.09385401416414524];
observed{28}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{28}	=[-0.0054400465882869994,-0.010569113495524807,0.010252815800504661,0.01465427120091195,-0.01612864951942794,-0.013773270853930708,0.02088368401117674,0.0097199564443744,-0.023476606517401577,-0.00416708362015919];
obs_stdev{28}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{28}	=0.10050629695993843;
costTotal{28}	=0.10050629695993843;
costWeakConstraintPenalty{28}	=0.5648917377160605;
% SimulationKwadraticCostFunction: evaluation 28 : cost = 0.665
% ========================================================================
% prepare no 29
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{29}	=[8.304368472121133,1.6991260606543832];
% ========================================================================
% evaluate no. 29
predicted{29}	=[-0.037756443140376975,-0.6204921895034169,0.16581190933415998,0.4513089686355884,-0.22934072854389295,-0.3043977627871884,0.24704003814895215,0.18504672286916526,-0.2347654513239773,-0.09393344263478746];
observed{29}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{29}	=[-0.0053779960433676735,-0.010429980777929004,0.010140019871025308,0.014456988322495923,-0.01594567107284664,-0.013581015739386026,0.020640615598739365,0.009573924497242536,-0.023198091653036657,-0.004087655149516964];
obs_stdev{29}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{29}	=0.09803978464133864;
costTotal{29}	=0.09803978464133864;
costWeakConstraintPenalty{29}	=0.5674298035229149;
% SimulationKwadraticCostFunction: evaluation 29 : cost = 0.665
% ---linesearch 1.6511241445627491,1.6330339747662646,1.6180339748439998
% --- f values 0.6654695881642536,0.6653980346759989,0.6654188140156789
p_dir{3}	=[8.2959122086154,1.6991260606543832];
f_p_dir{3}	=0.6653980346759989;
% direction 1
% direction 1
searchDir{4}	=[0.0,0.12832606065438332];
% Start line optimization in point [8.2959122086154,1.6991260606543832] in direction:[0.0,0.12832606065438332]
% ========================================================================
% prepare no 30
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{30}	=[8.2959122086154,1.6991260606543832];
% ========================================================================
% evaluate no. 30
predicted{30}	=[-0.03769439259545765,-0.6203530567858211,0.16569911340468063,0.45111168575717236,-0.22915775009731165,-0.3042055076726437,0.24679696973651477,0.1849006909220334,-0.23448693645961238,-0.09385401416414524];
observed{30}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{30}	=[-0.0054400465882869994,-0.010569113495524807,0.010252815800504661,0.01465427120091195,-0.01612864951942794,-0.013773270853930708,0.02088368401117674,0.0097199564443744,-0.023476606517401577,-0.00416708362015919];
obs_stdev{30}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{30}	=0.10050629695993843;
costTotal{30}	=0.10050629695993843;
costWeakConstraintPenalty{30}	=0.5648917377160605;
% SimulationKwadraticCostFunction: evaluation 30 : cost = 0.665
% ========================================================================
% prepare no 31
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{31}	=[8.2959122086154,1.8274521213087664];
% ========================================================================
% evaluate no. 31
predicted{31}	=[-0.13189420687742318,-0.5701647347913239,0.35634282341209556,0.29008730540155775,-0.4085759766627735,-0.046858545642000544,0.34181789571139926,-0.11467712944192261,-0.21776650839995873,0.18662721305328217];
observed{31}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{31}	=[0.08875976769367852,-0.060757435490021994,-0.18039089420691026,0.17567865155652657,0.16328957704603392,-0.2711202328845739,-0.07413724196370775,0.3092977768083304,-0.04019703457705523,-0.28464831083758657];
obs_stdev{31}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{31}	=17.947260007057388;
costTotal{31}	=17.947260007057388;
costWeakConstraintPenalty{31}	=2.1282208980527755;
% SimulationKwadraticCostFunction: evaluation 31 : cost = 20.075
% ========================================================================
% prepare no 32
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{32}	=[8.2959122086154,1.4914901314295288];
% ========================================================================
% evaluate no. 32
predicted{32}	=[0.11692763210591173,-0.6111905822162237,-0.18299214353028728,0.45297994429133137,0.2113188949124157,-0.32443887033416813,-0.21441516906115307,0.22297268337605172,0.201722099347005,-0.14513511113846622];
observed{32}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{32}	=[-0.16006207128965638,-0.019731588065122208,0.3589440727354726,0.012786012666752944,-0.4566052945291553,0.006460091807593715,0.48209582280884455,-0.02835203600964392,-0.459685642324019,0.04711401335416179];
obs_stdev{32}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{32}	=40.514724990598964;
costTotal{32}	=40.514724990598964;
costWeakConstraintPenalty{32}	=0.24282822606524984;
% SimulationKwadraticCostFunction: evaluation 32 : cost = 40.758
% ---bracket 1.0,0.0,-1.618034
% --- f values 20.07548090511016,0.6653980346759989,40.75755321666421
% ========================================================================
% prepare no 33
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{33}	=[8.2959122086154,1.6991260606543832];
% ========================================================================
% evaluate no. 33
predicted{33}	=[-0.03769439259545765,-0.6203530567858211,0.16569911340468063,0.45111168575717236,-0.22915775009731165,-0.3042055076726437,0.24679696973651477,0.1849006909220334,-0.23448693645961238,-0.09385401416414524];
observed{33}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{33}	=[-0.0054400465882869994,-0.010569113495524807,0.010252815800504661,0.01465427120091195,-0.01612864951942794,-0.013773270853930708,0.02088368401117674,0.0097199564443744,-0.023476606517401577,-0.00416708362015919];
obs_stdev{33}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{33}	=0.10050629695993843;
costTotal{33}	=0.10050629695993843;
costWeakConstraintPenalty{33}	=0.5648917377160605;
% SimulationKwadraticCostFunction: evaluation 33 : cost = 0.665
% ========================================================================
% prepare no 34
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{34}	=[8.2959122086154,1.6198161953120824];
% ========================================================================
% evaluate no. 34
predicted{34}	=[0.02130036285051265,-0.6303025555300954,0.033004047238498854,0.49267268281489174,-0.06481391173896517,-0.38201596526252257,0.08107671138714825,0.293781055390176,-0.08689243955401532,-0.2239891344931632];
observed{34}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{34}	=[-0.0644348020342573,-6.196147512504968E-4,0.14294788196668645,-0.026906725856807423,-0.18047248787777442,0.06403718673594816,0.18660394236054326,-0.09916040802376822,-0.17107110342299864,0.12596803670885878];
obs_stdev{34}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{34}	=7.588423158958048;
costTotal{34}	=7.588423158958048;
costWeakConstraintPenalty{34}	=0.11981090667220574;
% SimulationKwadraticCostFunction: evaluation 34 : cost = 7.708
% ========================================================================
% prepare no 35
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{35}	=[8.2959122086154,1.7481422527382955];
% ========================================================================
% evaluate no. 35
predicted{35}	=[-0.07391874575838904,-0.6060392991437958,0.24323025564603168,0.401904313163021,-0.3139075860351557,-0.2199076081437602,0.3138437411212049,0.07691827206696392,-0.27010995523059095,0.022078685482914712];
observed{35}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{35}	=[0.030784306574644392,-0.024882871137550056,-0.06727832644084639,0.06386164379506332,0.06862118641841608,-0.09807117038281421,-0.04616308737351338,0.11770237529944388,0.012146412253576988,-0.12009978326721914];
obs_stdev{35}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{35}	=2.7527354154198425;
costTotal{35}	=2.7527354154198425;
costWeakConstraintPenalty{35}	=1.0390130074748156;
% SimulationKwadraticCostFunction: evaluation 35 : cost = 3.792
% ========================================================================
% prepare no 36
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{36}	=[8.2959122086154,1.6968131330255856];
% ========================================================================
% evaluate no. 36
predicted{36}	=[-0.03597950663367408,-0.6208760234169997,0.16193319584606408,0.4530093784732275,-0.22476661700440453,-0.3075769297518338,0.2428311825230218,0.18941053445994974,-0.23158720663280796,-0.09897868020337358];
observed{36}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{36}	=[-0.007154932550070568,-0.01004614686434624,0.01401873335912121,0.01275657848485684,-0.020519782612335058,-0.010401848774740596,0.02484947122466971,0.005210112906458064,-0.026376336344205997,9.575824190691529E-4];
obs_stdev{36}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{36}	=0.11909514111581528;
costTotal{36}	=0.11909514111581528;
costWeakConstraintPenalty{36}	=0.5462762343946915;
% SimulationKwadraticCostFunction: evaluation 36 : cost = 0.665
% ========================================================================
% prepare no 37
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{37}	=[8.2959122086154,1.6979649581138243];
% ========================================================================
% evaluate no. 37
predicted{37}	=[-0.036833564918773244,-0.6206173206814672,0.16380964731117437,0.4520692882112255,-0.22695727726926013,-0.305905467265237,0.24481429881043723,0.18717277207453586,-0.23304407196164373,-0.09643313900109787];
observed{37}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{37}	=[-0.006300874264971404,-0.010304849599878652,0.012142281894010926,0.01369666874685882,-0.018329122347479465,-0.012073311261337427,0.02286635493725428,0.007447875291871936,-0.02491947101537023,-0.0015879587832065556];
obs_stdev{37}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{37}	=0.10822444701193605;
costTotal{37}	=0.10822444701193605;
costWeakConstraintPenalty{37}	=0.5555043303957258;
% SimulationKwadraticCostFunction: evaluation 37 : cost = 0.664
% ---linesearch 0.0,-0.00904806501998047,-0.018023834106674363
% --- f values 0.6653980346759989,0.6637287774076618,0.6653713755105068
p_dir{4}	=[8.2959122086154,1.6979649581138243];
f_p_dir{4}	=0.6637287774076618;
% End point of this outer loop has cost 0.6637287774076618 at [8.2959122086154,1.6979649581138243]
% Relative convergence check for outer loop: 0.5037020101027883<0.01
% Absolute convergence check for outer loop: 0.44686489135265306<0.01
% ========================================================================
% prepare no 38
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{38}	=[9.059275170920063,1.6968038555732654];
% ========================================================================
% evaluate no. 38
predicted{38}	=[-0.04112281454736218,-0.6325290573744475,0.17130513021839666,0.4697638897434919,-0.24010336397357823,-0.3241964636900018,0.26343852616162144,0.202362335386526,-0.2555069895325199,-0.10639776197648788];
observed{38}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{38}	=[-0.002011624636382471,0.0016068870931016388,0.004646798986788631,-0.00399793278540761,-0.00518303564316136,0.006217685163427411,0.004242127586070066,-0.007741688020118209,-0.0024565534444940806,0.008376664192183456];
obs_stdev{38}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{38}	=0.013193046606466176;
costTotal{38}	=0.013193046606466176;
costWeakConstraintPenalty{38}	=1.0634521732152438;
% SimulationKwadraticCostFunction: evaluation 38 : cost = 1.077
% Candidate direction :[0.7633629623046625,-0.0011611025405589004]
% Is new direction a good one,i.e -0.033948448938604825<0 and -5.08295291694396E-4<0
% Application starting next step
% =================================================================
% Outer loop iteration no. 3
% =================================================================
p_start{3}	=[8.2959122086154,1.6979649581138243];
f_p_start{3}	=0.6637287774076618;
% direction 0
% direction 0
searchDir{5}	=[0.7633629623046628,0.0];
% Start line optimization in point [8.2959122086154,1.6979649581138243] in direction:[0.7633629623046628,0.0]
% ========================================================================
% prepare no 39
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{39}	=[8.2959122086154,1.6979649581138243];
% ========================================================================
% evaluate no. 39
predicted{39}	=[-0.036833564918773244,-0.6206173206814672,0.16380964731117437,0.4520692882112255,-0.22695727726926013,-0.305905467265237,0.24481429881043723,0.18717277207453586,-0.23304407196164373,-0.09643313900109787];
observed{39}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{39}	=[-0.006300874264971404,-0.010304849599878652,0.012142281894010926,0.01369666874685882,-0.018329122347479465,-0.012073311261337427,0.02286635493725428,0.007447875291871936,-0.02491947101537023,-0.0015879587832065556];
obs_stdev{39}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{39}	=0.10822444701193605;
costTotal{39}	=0.10822444701193605;
costWeakConstraintPenalty{39}	=0.5555043303957258;
% SimulationKwadraticCostFunction: evaluation 39 : cost = 0.664
% ========================================================================
% prepare no 40
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{40}	=[9.059275170920063,1.6979649581138243];
% ========================================================================
% evaluate no. 40
predicted{40}	=[-0.041988579613225364,-0.6322528828283739,0.1732466031901854,0.468759537180392,-0.2424100709458951,-0.3223852672375858,0.2655589377803929,0.1998969936769775,-0.2570816271396019,-0.10354474787483017];
observed{40}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{40}	=[-0.0011458595705192837,0.0013307125470279901,0.002705326014999898,-0.002993580222307668,-0.0028763286708445024,0.004406488711011403,0.0021217159672985986,-0.005276346310569696,-8.819158374120395E-4,0.005523650090525745];
obs_stdev{40}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{40}	=0.0055342252858891884;
costTotal{40}	=0.0055342252858891884;
costWeakConstraintPenalty{40}	=1.0727542566557686;
% SimulationKwadraticCostFunction: evaluation 40 : cost = 1.078
% ========================================================================
% prepare no 41
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{41}	=[7.060764981265737,1.6979649581138243];
% ========================================================================
% evaluate no. 41
predicted{41}	=[-0.02629297480797945,-0.5971416264686195,0.14518485705129144,0.41935989718097755,-0.19742182772346573,-0.27446416246776906,0.20635064684082782,0.1634354673183805,-0.18976505906319005,-0.08327683261886931];
observed{41}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{41}	=[-0.016841464375765197,-0.03378054381272644,0.03076707215389385,0.04640605977710677,-0.047864571893273866,-0.043514616058805355,0.06133000690686369,0.03118518004802731,-0.06819848391382391,-0.014744265165435116];
obs_stdev{41}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{41}	=0.9155872997035612;
costTotal{41}	=0.9155872997035612;
costWeakConstraintPenalty{41}	=0.9528035230002795;
% SimulationKwadraticCostFunction: evaluation 41 : cost = 1.868
% ---bracket 1.0,0.0,-1.618034
% --- f values 1.0782884819416578,0.6637287774076618,1.8683908227038408
% ========================================================================
% prepare no 42
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{42}	=[8.2959122086154,1.6979649581138243];
% ========================================================================
% evaluate no. 42
predicted{42}	=[-0.036833564918773244,-0.6206173206814672,0.16380964731117437,0.4520692882112255,-0.22695727726926013,-0.305905467265237,0.24481429881043723,0.18717277207453586,-0.23304407196164373,-0.09643313900109787];
observed{42}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{42}	=[-0.006300874264971404,-0.010304849599878652,0.012142281894010926,0.01369666874685882,-0.018329122347479465,-0.012073311261337427,0.02286635493725428,0.007447875291871936,-0.02491947101537023,-0.0015879587832065556];
obs_stdev{42}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{42}	=0.10822444701193605;
costTotal{42}	=0.10822444701193605;
costWeakConstraintPenalty{42}	=0.5555043303957258;
% SimulationKwadraticCostFunction: evaluation 42 : cost = 0.664
% ========================================================================
% prepare no 43
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{43}	=[7.8241279627735585,1.6979649581138243];
% ========================================================================
% evaluate no. 43
predicted{43}	=[-0.033175815454520065,-0.612423164447149,0.15724457028723368,0.4405060642361855,-0.2163992167047909,-0.2946603622593407,0.23088226759713626,0.17860102702043348,-0.21716826833256567,-0.09166624073897678];
observed{43}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{43}	=[-0.009958623729224583,-0.018499005834196947,0.01870735891795161,0.025259892721898825,-0.028887182911948694,-0.023318416267233688,0.03679838615055525,0.016019620345974317,-0.04079527464444829,-0.00635485704532765];
obs_stdev{43}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{43}	=0.3061510417969123;
costTotal{43}	=0.3061510417969123;
costWeakConstraintPenalty{43}	=0.5271877995309932;
% SimulationKwadraticCostFunction: evaluation 43 : cost = 0.833
% ========================================================================
% prepare no 44
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{44}	=[8.587490905875063,1.6979649581138243];
% ========================================================================
% evaluate no. 44
predicted{44}	=[-0.038904644539621794,-0.6252797100831949,0.16757507334143618,0.4587188806909034,-0.23308424244403758,-0.3124361142013219,0.25298978194429617,0.1921922111176979,-0.24246140174267874,-0.09923335150023535];
observed{44}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{44}	=[-0.004229794644122854,-0.005642460198150956,0.008376855863749116,0.0070470762671809295,-0.01220215717270201,-0.005542664325252522,0.014690871803395344,0.002428436248709892,-0.015502141234335215,0.0012122537159309221];
obs_stdev{44}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{44}	=0.04063401221326599;
costTotal{44}	=0.04063401221326599;
costWeakConstraintPenalty{44}	=0.6842950950348546;
% SimulationKwadraticCostFunction: evaluation 44 : cost = 0.725
% ========================================================================
% prepare no 45
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{45}	=[8.301005782565566,1.6979649581138243];
% ========================================================================
% evaluate no. 45
predicted{45}	=[-0.03687091993825342,-0.6207012679502956,0.16387725379543025,0.4521885645035333,-0.22706682722954527,-0.30602219928856017,0.24495989680263508,0.18726222628949388,-0.23321113927837547,-0.09648298537619733];
observed{45}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{45}	=[-0.006263519245491227,-0.01022090233105033,0.012074675409755042,0.01357739245455103,-0.01821957238719432,-0.011956579238014242,0.02272075694505643,0.0073584210769139236,-0.024752403698638487,-0.001538112408107098];
obs_stdev{45}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{45}	=0.10670904652521625;
costTotal{45}	=0.10670904652521625;
costWeakConstraintPenalty{45}	=0.5570245533608581;
% SimulationKwadraticCostFunction: evaluation 45 : cost = 0.664
% ========================================================================
% prepare no 46
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{46}	=[8.293578755255941,1.6979649581138243];
% ========================================================================
% evaluate no. 46
predicted{46}	=[-0.03681643751992709,-0.6205788323727306,0.16377865330594632,0.452014607707387,-0.22690705999019428,-0.3058519581664904,0.2447475643362165,0.1871317701620902,-0.23296750497087637,-0.0964102922908108];
observed{46}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{46}	=[-0.00631800166381756,-0.010343337908615302,0.012173275899238972,0.01375134925069732,-0.01837933962654531,-0.012126820360084,0.02293308941147501,0.0074888772043175955,-0.02499603800613759,-0.0016108054934936278];
obs_stdev{46}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{46}	=0.10892282078406458;
costTotal{46}	=0.10892282078406458;
costWeakConstraintPenalty{46}	=0.5548165555607176;
% SimulationKwadraticCostFunction: evaluation 46 : cost = 0.664
% ---linesearch -0.003056807147695806,0.0,0.006672545305038828
% --- f values 0.6637393763447822,0.6637287774076618,0.6637335998860744
p_dir{5}	=[8.2959122086154,1.6979649581138243];
f_p_dir{5}	=0.6637287774076618;
% direction 1
% direction 1
searchDir{6}	=[0.0,0.012832606065438332];
% Start line optimization in point [8.2959122086154,1.6979649581138243] in direction:[0.0,0.012832606065438332]
% ========================================================================
% prepare no 47
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{47}	=[8.2959122086154,1.6979649581138243];
% ========================================================================
% evaluate no. 47
predicted{47}	=[-0.036833564918773244,-0.6206173206814672,0.16380964731117437,0.4520692882112255,-0.22695727726926013,-0.305905467265237,0.24481429881043723,0.18717277207453586,-0.23304407196164373,-0.09643313900109787];
observed{47}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{47}	=[-0.006300874264971404,-0.010304849599878652,0.012142281894010926,0.01369666874685882,-0.018329122347479465,-0.012073311261337427,0.02286635493725428,0.007447875291871936,-0.02491947101537023,-0.0015879587832065556];
obs_stdev{47}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{47}	=0.10822444701193605;
costTotal{47}	=0.10822444701193605;
costWeakConstraintPenalty{47}	=0.5555043303957258;
% SimulationKwadraticCostFunction: evaluation 47 : cost = 0.664
% ========================================================================
% prepare no 48
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{48}	=[8.2959122086154,1.7107975641792625];
% ========================================================================
% evaluate no. 48
predicted{48}	=[-0.04634113734546301,-0.6175031272338315,0.1845698998392439,0.44093636355322113,-0.25082363540134717,-0.2862981453066335,0.2657823109935608,0.16119798015220557,-0.24752150185800362,-0.06727841369500843];
observed{48}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{48}	=[0.003206698161718362,-0.0134190430475144,-0.008617970634058614,0.02482959340486318,0.005537235784607575,-0.03168063321994091,0.0018983427541306996,0.03342266721420223,-0.010442041119010342,-0.030742684089295993];
obs_stdev{48}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{48}	=0.20451412469656607;
costTotal{48}	=0.20451412469656607;
costWeakConstraintPenalty{48}	=0.6639943646932713;
% SimulationKwadraticCostFunction: evaluation 48 : cost = 0.869
% ========================================================================
% prepare no 49
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{49}	=[8.2959122086154,1.6772013651913389];
% ========================================================================
% evaluate no. 49
predicted{49}	=[-0.021422170204469355,-0.6247519557644353,0.12968749502630272,0.4674891478294913,-0.1863521359506672,-0.33367981139641456,0.20670675130977736,0.22485867390784534,-0.2030436206559409,-0.14001122628242776];
observed{49}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{49}	=[-0.021712268979275293,-0.006170214516910577,0.04626443417888257,-0.0017231908714069877,-0.05893426366607238,0.015701032869840148,0.060973902437914146,-0.030238026541437535,-0.05491992232107307,0.041990128498123336];
obs_stdev{49}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{49}	=0.7892077540898528;
costTotal{49}	=0.7892077540898528;
costWeakConstraintPenalty{49}	=0.4020383284707847;
% SimulationKwadraticCostFunction: evaluation 49 : cost = 1.191
% ---bracket 1.0,0.0,-1.618034
% --- f values 0.8685084893898374,0.6637287774076618,1.1912460825606375
% ========================================================================
% prepare no 50
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{50}	=[8.2959122086154,1.6979649581138243];
% ========================================================================
% evaluate no. 50
predicted{50}	=[-0.036833564918773244,-0.6206173206814672,0.16380964731117437,0.4520692882112255,-0.22695727726926013,-0.305905467265237,0.24481429881043723,0.18717277207453586,-0.23304407196164373,-0.09643313900109787];
observed{50}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{50}	=[-0.006300874264971404,-0.010304849599878652,0.012142281894010926,0.01369666874685882,-0.018329122347479465,-0.012073311261337427,0.02286635493725428,0.007447875291871936,-0.02491947101537023,-0.0015879587832065556];
obs_stdev{50}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{50}	=0.10822444701193605;
costTotal{50}	=0.10822444701193605;
costWeakConstraintPenalty{50}	=0.5555043303957258;
% SimulationKwadraticCostFunction: evaluation 50 : cost = 0.664
% ========================================================================
% prepare no 51
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{51}	=[8.2959122086154,1.6900339715795942];
% ========================================================================
% evaluate no. 51
predicted{51}	=[-0.030950739514650195,-0.6223289284349476,0.15084837779144328,0.4583422459730223,-0.21172045080008992,-0.31710914844597715,0.23083770496211758,0.20224462241797134,-0.22250607269851425,-0.11368064930415304];
observed{51}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{51}	=[-0.012183699669094453,-0.008593241846398314,0.025103551413742015,0.007423710985062004,-0.03356594881664968,-8.696300805972612E-4,0.036842948785573926,-0.007623975051563536,-0.035457470278499714,0.015659551519848616];
obs_stdev{51}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{51}	=0.24764984605126494;
costTotal{51}	=0.24764984605126494;
costWeakConstraintPenalty{51}	=0.49366490030693716;
% SimulationKwadraticCostFunction: evaluation 51 : cost = 0.741
% ========================================================================
% prepare no 52
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{52}	=[8.2959122086154,1.7028665773222156];
% ========================================================================
% evaluate no. 52
predicted{52}	=[-0.040466805174991134,-0.6194780057082805,0.17177145964576776,0.4479591442683845,-0.23619226076883368,-0.29862741649873187,0.253070537525908,0.17747225587222512,-0.238957599293709,-0.08546018618092685];
observed{52}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{52}	=[-0.002667634008753514,-0.011444164573065407,0.004180469559417532,0.017806812689699802,-0.009094138847905914,-0.019351362027842545,0.014610116221783487,0.01714839149418268,-0.019005943683304966,-0.012560911603377578];
obs_stdev{52}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{52}	=0.09781739370267775;
costTotal{52}	=0.09781739370267775;
costWeakConstraintPenalty{52}	=0.5957136616732386;
% SimulationKwadraticCostFunction: evaluation 52 : cost = 0.694
% ========================================================================
% prepare no 53
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{53}	=[8.2959122086154,1.6979498112771951];
% ========================================================================
% evaluate no. 53
predicted{53}	=[-0.03682233450335447,-0.6206207449957621,0.16378498483447504,0.4520817144462141,-0.22692851949647927,-0.3059275440518573,0.24478832522404154,0.18720230419767148,-0.23302507840018608,-0.09646669822939949];
observed{53}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{53}	=[-0.006312104680390181,-0.010301425285583754,0.012166944370710253,0.013684242511870226,-0.018357880120260323,-0.01205123447471712,0.022892328523649974,0.007418343168736324,-0.024938464576827885,-0.0015543995549049394];
obs_stdev{53}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{53}	=0.10834663828793024;
costTotal{53}	=0.10834663828793024;
costWeakConstraintPenalty{53}	=0.5553824334082161;
% SimulationKwadraticCostFunction: evaluation 53 : cost = 0.664
% ========================================================================
% prepare no 54
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{54}	=[8.2959122086154,1.697989390475683];
% ========================================================================
% evaluate no. 54
predicted{54}	=[-0.03685167992072759,-0.6206117958940686,0.1638494279676653,0.45204924069293023,-0.22700366171309566,-0.30586985124515836,0.24485618917531907,0.18712512988962943,-0.23307469995443172,-0.09637900211614639];
observed{54}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{54}	=[-0.006282759263017058,-0.010310374387277332,0.012102501237519991,0.013716716265154083,-0.01828273790364393,-0.012108927281416049,0.022824464572372444,0.007495517476778374,-0.024888843022582235,-0.001642095668158039];
obs_stdev{54}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{54}	=0.10802851504360131;
costTotal{54}	=0.10802851504360131;
costWeakConstraintPenalty{54}	=0.5557009849768526;
% SimulationKwadraticCostFunction: evaluation 54 : cost = 0.664
% ---linesearch 0.0019039283006298952,0.0,-0.0011803398742210001
% --- f values 0.6637295000204539,0.6637287774076618,0.6637290716961464
p_dir{6}	=[8.2959122086154,1.6979649581138243];
f_p_dir{6}	=0.6637287774076618;
% End point of this outer loop has cost 0.6637287774076618 at [8.2959122086154,1.6979649581138243]
% Relative convergence check for outer loop: 0.0<0.01
% Absolute convergence check for outer loop: 0.0<0.01
% ===================================================================
% SimulationKwadraticCostfunction: optimal results
%     number of evaluations: 54
%     all cost values:
%         [17.756,17.756,19.151,18.695,17.756,17.606,17.816,17.589,17.589,17.589,...,0.664,0.664,0.664,0.869,1.191,0.664,0.741,0.694,0.664,0.664]
%     all parameter values
%         [8,8,9,6.382,8,7.382,7,7.537,7.533,7.533,7.533,7.533,7.533,7.533,7.533,7.533,7.533,7.533,7.533,7.533,7.065,7.533,8,8.756,8,8.289,8.467,8.296,8.304,8.296,8.296,8.296,8.296,8.296,8.296,8.296,8.296,9.059,8.296,9.059,7.061,8.296,7.824,8.587,8.301,8.294,8.296,8.296,8.296,8.296,8.296,8.296,8.296,8.296;1.571,1.571,1.571,1.571,1.571,1.571,1.571,1.571,1.571,1.571,1.696,1.9,1.696,1.774,1.648,1.699,1.701,1.698,1.7,1.7,1.827,1.699,1.699,1.699,1.699,1.699,1.699,1.699,1.699,1.699,1.827,1.491,1.699,1.62,1.748,1.697,1.698,1.697,1.698,1.698,1.698,1.698,1.698,1.698,1.698,1.698,1.698,1.711,1.677,1.698,1.69,1.703,1.698,1.698]
%     number of observations: 10
%     best cost:
%         cost = 0.664
%     best parameters:
%                 [8.296,1.698]
% ===================================================================
% Application Done
