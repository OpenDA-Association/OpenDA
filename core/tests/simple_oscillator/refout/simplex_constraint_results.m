% Starting Algorithm: 
%	className: org.openda.algorithms.Simplex
%	dir.: /home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./algorithm
%	config.: simplexAlgorithm_withConstraint.xml
% configstring = simplexAlgorithm_withConstraint.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./algorithm/simplexAlgorithm_withConstraint.xml
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
% outerLoop@maxIterations=30
% outerLoop@absTolerance=0.01
% outerLoop@relTolerance=0.01
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
% outerLoop@maxIterations=30
% outerLoop@absTolerance=0.01
% outerLoop@relTolerance=0.01
% Application initialized
% Application starting next step
costs{1}	=[17.756295868749987,19.151382948941716,0.7346285670487764];
% Iteration step no.1
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
evaluatedParameters{4}	=[9.0,1.4451];
% ========================================================================
% evaluate no. 4
predicted{4}	=[0.14752315336770708,-0.606370838278394,-0.2587800762467689,0.42551772762718837,0.3059172956115961,-0.2697679780698142,-0.30753439209669897,0.14467820291852235,0.27981365126873675,-0.05094417588228275];
observed{4}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{4}	=[-0.19065759255145173,-0.024551332002951898,0.43473200545195423,0.040248229330895946,-0.5512036952283357,-0.04821080045676024,0.5752150458443905,0.04994244444788545,-0.5377771942457508,-0.04707692190202168];
obs_stdev{4}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{4}	=57.92509304465168;
costTotal{4}	=57.92509304465168;
costWeakConstraintPenalty{4}	=0.9999999999999992;
% SimulationKwadraticCostFunction: evaluation 4 : cost = 58.925
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
evaluatedParameters{5}	=[8.25,1.6336499999999998];
% ========================================================================
% evaluate no. 5
predicted{5}	=[0.011309485588505721,-0.6289472770665655,0.05610786237392339,0.4877526449568503,-0.09442364285791426,-0.372995124720875,0.11263421608080974,0.281060692081189,-0.1174258370184203,-0.20842251507666973];
observed{5}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{5}	=[-0.054443924772250366,-0.0019748932147803933,0.1198440668312619,-0.021986687998765964,-0.15086275675882532,0.05501634619430057,0.15504643766688175,-0.0864400447147812,-0.14053770595859366,0.1104014172923653];
obs_stdev{5}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{5}	=5.352551228841082;
costTotal{5}	=5.352551228841082;
costWeakConstraintPenalty{5}	=0.1562499999999994;
% SimulationKwadraticCostFunction: evaluation 5 : cost = 5.509
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
evaluatedParameters{6}	=[8.5,1.5708];
% ========================================================================
% evaluate no. 6
predicted{6}	=[0.05655179594166106,-0.6318275927143532,-0.04964775110460242,0.4989685655923278,0.04314978079060869,-0.3940158829695735,-0.037191495281632916,0.3111144312934395,0.03183259567874311,-0.24563622031570642];
observed{6}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{6}	=[-0.0996862351254057,9.054224330072858E-4,0.22559968030978772,-0.03320260863424346,-0.2884361804073483,0.0760371044429991,0.3048721490293244,-0.11649378392703172,-0.28979613865575704,0.147615122531402];
obs_stdev{6}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{6}	=18.160136049322855;
costTotal{6}	=18.160136049322855;
costWeakConstraintPenalty{6}	=0.125;
% SimulationKwadraticCostFunction: evaluation 6 : cost = 18.285
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
evaluatedParameters{7}	=[8.0,1.6336499999999998];
% ========================================================================
% evaluate no. 7
predicted{7}	=[0.013117584678946087,-0.6243844271153112,0.05373643480252343,0.4807673230272821,-0.09109247004451315,-0.3650919848023582,0.10833737199668383,0.27323760188902185,-0.11235955726546693,-0.2012893381869515];
observed{7}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{7}	=[-0.056252023862690737,-0.006537743166034726,0.12221549440266186,-0.015001366069197775,-0.15419392957222644,0.0471132062757838,0.15934328175100768,-0.07861695452261405,-0.14560398571154703,0.10326824040264707];
obs_stdev{7}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{7}	=5.389993978071564;
costTotal{7}	=5.389993978071564;
costWeakConstraintPenalty{7}	=0.12499999999999939;
% SimulationKwadraticCostFunction: evaluation 7 : cost = 5.515
% SHRINK
maximumCost{1}	=18.285136049322855;
% WARNING: SHRINK produces worse parameter set
% costs[5.514993978071563,17.756295868749987,18.285136049322855]costs in simplex
% Application starting next step
costs{2}	=[5.514993978071563,17.756295868749987,18.285136049322855];
% Iteration step no.1
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
evaluatedParameters{8}	=[7.5,1.6336499999999998];
% ========================================================================
% evaluate no. 8
predicted{8}	=[0.017072034614144187,-0.6144572904782736,0.04862771650292286,0.46574725767797337,-0.08401560675827444,-0.34829233818649696,0.09932540649880252,0.25679284323740004,-0.10186325995853383,-0.1864559078395128];
observed{8}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{8}	=[-0.060206473797888835,-0.01646487980307232,0.12732421270226243,1.869928011094535E-5,-0.16127079285846516,0.03031355965992255,0.168355247248889,-0.062172195870992236,-0.1561002830184801,0.08843481005520837];
obs_stdev{8}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{8}	=5.571571648803955;
costTotal{8}	=5.571571648803955;
costWeakConstraintPenalty{8}	=0.24999999999999942;
% SimulationKwadraticCostFunction: evaluation 8 : cost = 5.822
% REFLECT!
% costs[5.514993978071563,5.821571648803954,17.756295868749987]costs in simplex
% Application starting next step
costs{3}	=[5.514993978071563,5.821571648803954,17.756295868749987];
% Iteration step no.1
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
evaluatedParameters{9}	=[7.5,1.6965];
% ========================================================================
% evaluate no. 9
predicted{9}	=[-0.029334725954557904,-0.6065730099965794,0.15004827128811185,0.4330321548918995,-0.20600554562124396,-0.288342790171887,0.21843244211152327,0.17490731560302897,-0.20409177603937728,-0.09104009004424007];
observed{9}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{9}	=[-0.013799713229186744,-0.024349160284766524,0.02590365791707344,0.032733802066184814,-0.039280853995495635,-0.029635988354687393,0.04924821163616824,0.019713331763378833,-0.05387176693763668,-0.006981007740064354];
obs_stdev{9}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{9}	=0.5355997909986161;
costTotal{9}	=0.5355997909986161;
costWeakConstraintPenalty{9}	=0.6249999999999993;
% SimulationKwadraticCostFunction: evaluation 9 : cost = 1.161
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
evaluatedParameters{10}	=[7.25,1.7593499999999995];
% ========================================================================
% evaluate no. 10
predicted{10}	=[-0.07303091088777083,-0.5839614597725805,0.24071744635082404,0.36679840582970635,-0.299071199213995,-0.18347360466218668,0.2851873264489946,0.048750509478945256,-0.23190250551320518,0.036585819263257255];
observed{10}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{10}	=[0.029896471704026185,-0.04696071050876538,-0.06476551714563875,0.09896755112837796,0.053784799597255406,-0.13450517386438773,-0.017506672701303072,0.14587013788746256,-0.02606103746380878,-0.13460691704756167];
obs_stdev{10}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{10}	=3.92277413552255;
costTotal{10}	=3.92277413552255;
costWeakConstraintPenalty{10}	=1.4062499999999944;
% SimulationKwadraticCostFunction: evaluation 10 : cost = 5.329
% REFLECT ALSO!
% costs[1.1605997909986154,5.514993978071563,5.821571648803954]costs in simplex
% Application starting next step
costs{4}	=[1.1605997909986154,5.514993978071563,5.821571648803954];
% Iteration step no.1
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
evaluatedParameters{11}	=[8.0,1.6964999999999995];
% ========================================================================
% evaluate no. 11
predicted{11}	=[-0.03350311927911247,-0.6158978936059812,0.15740421970211924,0.4461027450327289,-0.21769751450176625,-0.30101131585066493,0.23371991318596938,0.1845979073251936,-0.22137835983631918,-0.09656628411831293];
observed{11}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{11}	=[-0.009631319904632181,-0.015024276675364656,0.018547709503066057,0.019663211925355417,-0.027588885114973344,-0.016967462675909484,0.03396074056172213,0.010022740041214201,-0.03658518314069478,-0.0014548136659914973];
obs_stdev{11}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{11}	=0.23462856704878535;
costTotal{11}	=0.23462856704878535;
costWeakConstraintPenalty{11}	=0.4999999999999958;
% SimulationKwadraticCostFunction: evaluation 11 : cost = 0.735
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
evaluatedParameters{12}	=[8.25,1.727924999999999];
% ========================================================================
% evaluate no. 12
predicted{12}	=[-0.05866030695266441,-0.6119530410840802,0.21112915025221593,0.4232540406733302,-0.2798664880994931,-0.2566330466264982,0.2888541032977467,0.12345688738089494,-0.25997768890653955,-0.026740903511241764];
observed{12}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{12}	=[0.01552586776891976,-0.018969129197265677,-0.03517722104703064,0.04251191628475409,0.03458008848275351,-0.061345731900076195,-0.02117344955005518,0.07116375998551286,0.0020141459295255904,-0.07128019427306266];
obs_stdev{12}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{12}	=0.9601090130830304;
costTotal{12}	=0.9601090130830304;
costWeakConstraintPenalty{12}	=0.8124999999999906;
% SimulationKwadraticCostFunction: evaluation 12 : cost = 1.773
% REFLECT ALSO!
% costs[0.7346285670487811,1.1605997909986154,5.514993978071563]costs in simplex
% Application starting next step
costs{5}	=[0.7346285670487811,1.1605997909986154,5.514993978071563];
% Iteration step no.1
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
evaluatedParameters{13}	=[7.5,1.7593499999999995];
% ========================================================================
% evaluate no. 13
predicted{13}	=[-0.07543086838029056,-0.5886520766142722,0.24577688334574282,0.37236994683582103,-0.307174152825535,-0.18710434160218045,0.29503042554282144,0.049082521852079807,-0.2416481356727888,0.03958325660366384];
observed{13}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{13}	=[0.03229642919654591,-0.04227009366707368,-0.06982495414055753,0.09339601012226328,0.061887753208795415,-0.13087443692439396,-0.02734977179512993,0.145538125514328,-0.01631540730422515,-0.13760435438796825];
obs_stdev{13}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{13}	=3.9258439089985893;
costTotal{13}	=3.9258439089985893;
costWeakConstraintPenalty{13}	=1.2499999999999944;
% SimulationKwadraticCostFunction: evaluation 13 : cost = 5.176
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
evaluatedParameters{14}	=[7.625,1.7279249999999995];
% ========================================================================
% evaluate no. 14
predicted{14}	=[-0.053550340500053015,-0.6011906716973814,0.20102716664555886,0.4090417772637783,-0.26339434175837967,-0.24464378549688748,0.26766580995014844,0.11703937417550755,-0.23702704995923113,-0.027020874369191816];
observed{14}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{14}	=[0.010415901316308367,-0.02973149858396451,-0.02507523744037357,0.05672417969430604,0.018107942141640077,-0.07333499302968693,1.4843797543073745E-5,0.07758127319090025,-0.02093649301778283,-0.07100022341511261];
obs_stdev{14}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{14}	=1.10214972546853;
costTotal{14}	=1.10214972546853;
costWeakConstraintPenalty{14}	=0.8515624999999949;
% SimulationKwadraticCostFunction: evaluation 14 : cost = 1.954
% CONTRACT OUTSIDE!
% costs[0.7346285670487811,1.1605997909986154,1.9537122254685249]costs in simplex
% Application starting next step
costs{6}	=[0.7346285670487811,1.1605997909986154,1.9537122254685249];
% Iteration step no.1
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
evaluatedParameters{15}	=[7.875,1.6650749999999999];
% ========================================================================
% evaluate no. 15
predicted{15}	=[-0.009250878143760813,-0.61911806199883,0.10457488495466531,0.4638086684165481,-0.1540862477741257,-0.33554350956884604,0.1723145381836801,0.2331784275497121,-0.17035059080009263,-0.15408108467060438];
observed{15}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{15}	=[-0.033883561039983835,-0.01180410828251588,0.07137704425051998,0.0019572885415362062,-0.0912001518426139,0.01756473104227163,0.09536611556401142,-0.0385577801833043,-0.08761295217692133,0.056059986886299956];
obs_stdev{15}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{15}	=1.820604175372719;
costTotal{15}	=1.820604175372719;
costWeakConstraintPenalty{15}	=0.2890624999999994;
% SimulationKwadraticCostFunction: evaluation 15 : cost = 2.11
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
evaluatedParameters{16}	=[7.6875,1.7122124999999997];
% ========================================================================
% evaluate no. 16
predicted{16}	=[-0.04253804965897175,-0.6065789475149521,0.17770512361802068,0.42517416934321317,-0.2385715023799356,-0.27078345577820456,0.2486111467315932,0.14936108443197071,-0.2273432310300532,-0.06083424510607064];
observed{16}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{16}	=[-5.963895247729015E-4,-0.02434322276639378,-0.0017531944128353905,0.040591787614871144,-0.0067148972368039905,-0.04719532274836985,0.019069507016098303,0.045259562934437086,-0.03062031194696077,-0.03718685267823379];
obs_stdev{16}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{16}	=0.4624371536229158;
costTotal{16}	=0.4624371536229158;
costWeakConstraintPenalty{16}	=0.6816406249999974;
% SimulationKwadraticCostFunction: evaluation 16 : cost = 1.144
% CONTRACT INSIDE!
% costs[0.7346285670487811,1.1440777786229133,1.1605997909986154]costs in simplex
% Application starting next step
costs{7}	=[0.7346285670487811,1.1440777786229133,1.1605997909986154];
% Iteration step no.1
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
evaluatedParameters{17}	=[8.1875,1.7122124999999988];
% ========================================================================
% evaluate no. 17
predicted{17}	=[-0.04657389898732157,-0.6153550213557517,0.1852935908648893,0.4371805672623526,-0.2508603658877398,-0.2817651324841591,0.26463235446541744,0.156701936380332,-0.24517509261741763,-0.06344876970261157];
observed{17}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{17}	=[0.003439459803576919,-0.015567148925594188,-0.009341661659704015,0.02858538969573171,0.005573966271000208,-0.03621364604241534,0.0030482992822740695,0.0379187109860758,-0.012788450359596332,-0.03457232808169286];
obs_stdev{17}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{17}	=0.2653482759328398;
costTotal{17}	=0.2653482759328398;
costWeakConstraintPenalty{17}	=0.6503906249999896;
% SimulationKwadraticCostFunction: evaluation 17 : cost = 0.916
% REFLECT!
% costs[0.7346285670487811,0.9157389009328294,1.1440777786229133]costs in simplex
% Application starting next step
costs{8}	=[0.7346285670487811,0.9157389009328294,1.1440777786229133];
% Iteration step no.1
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
evaluatedParameters{18}	=[8.5,1.6964999999999986];
% ========================================================================
% evaluate no. 18
predicted{18}	=[-0.037209274661666555,-0.6242446061113114,0.1640624834014042,0.45797506832261337,-0.2284538118347452,-0.31267609669025925,0.2480026190625168,0.1936238651229987,-0.23777227901669779,-0.10173904770810226];
observed{18}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{18}	=[-0.005925164522078093,-0.006677564170034511,0.011889445803781096,0.0077908886354709495,-0.016832587781994396,-0.0053026818363151595,0.019678034685174706,9.967822434091E-4,-0.020191263960316175,0.003717949923797828];
obs_stdev{18}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{18}	=0.07014688335342296;
costTotal{18}	=0.07014688335342296;
costWeakConstraintPenalty{18}	=0.6249999999999889;
% SimulationKwadraticCostFunction: evaluation 18 : cost = 0.695
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
evaluatedParameters{19}	=[8.90625,1.688643749999998];
% ========================================================================
% evaluate no. 19
predicted{19}	=[-0.03407606934377016,-0.6321576575631528,0.15589077887814182,0.4732799735497261,-0.22086675376597525,-0.33313273895468154,0.2442485204889721,0.2164146693144602,-0.23917079581457212,-0.12420598960087309];
observed{19}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{19}	=[-0.00905836983997449,0.0012354872818068907,0.020061150327043475,-0.007514016591641803,-0.024419645850764338,0.015153960428107127,0.0234321332587194,-0.02179402194805241,-0.018792747162441836,0.02618489181656866];
obs_stdev{19}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{19}	=0.17156562389909022;
costTotal{19}	=0.17156562389909022;
costWeakConstraintPenalty{19}	=0.8500976562499853;
% SimulationKwadraticCostFunction: evaluation 19 : cost = 1.022
% REFLECT ALSO!
% costs[0.6951468833534119,0.7346285670487811,0.9157389009328294]costs in simplex
% Application starting next step
costs{9}	=[0.6951468833534119,0.7346285670487811,0.9157389009328294];
% Iteration step no.1
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
evaluatedParameters{20}	=[8.3125,1.6807874999999992];
% ========================================================================
% evaluate no. 20
predicted{20}	=[-0.024205950210139406,-0.624396303231143,0.13582553742427217,0.46546564698123377,-0.19384721659332624,-0.32966829737694126,0.21405980437295524,0.21912966265213724,-0.2092733267893928,-0.13312447522866472];
observed{20}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{20}	=[-0.018928488973605242,-0.00652586705020286,0.04012639178091312,3.003099768505435E-4,-0.051439183023413354,0.011689518850366842,0.05362084937473627,-0.024509015285729435,-0.04869021618762115,0.03510337744436029];
obs_stdev{20}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{20}	=0.5936299059215213;
costTotal{20}	=0.5936299059215213;
costWeakConstraintPenalty{20}	=0.43164062499999467;
% SimulationKwadraticCostFunction: evaluation 20 : cost = 1.025
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
evaluatedParameters{21}	=[8.21875,1.704356249999999];
% ========================================================================
% evaluate no. 21
predicted{21}	=[-0.04099644027835315,-0.6178458668420362,0.17312114452468677,0.4448887482626723,-0.23724167114865133,-0.2946592700375288,0.25323287368314173,0.17323041089790356,-0.23806237858764878,-0.08149172067600047];
observed{21}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{21}	=[-0.0021379989053915,-0.013076303439309656,0.0028307846804985215,0.02087720869541204,-0.008044728468088264,-0.023319508489045626,0.014447780064549776,0.021390236468504242,-0.019901164389365178,-0.01652937710830396];
obs_stdev{21}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{21}	=0.12817531423423104;
costTotal{21}	=0.12817531423423104;
costWeakConstraintPenalty{21}	=0.5883789062499909;
% SimulationKwadraticCostFunction: evaluation 21 : cost = 0.717
% CONTRACT INSIDE!
% costs[0.6951468833534119,0.7165542204842219,0.7346285670487811]costs in simplex
% Application starting next step
costs{10}	=[0.6951468833534119,0.7165542204842219,0.7346285670487811];
% Iteration step no.1
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
evaluatedParameters{22}	=[8.71875,1.7043562499999982];
% ========================================================================
% evaluate no. 22
predicted{22}	=[-0.04454656181061647,-0.6257405482551408,0.17973309010974112,0.45602234973651934,-0.24806585736102443,-0.30533335712783055,0.2676374722852983,0.18101896486305744,-0.25452599402309384,-0.08521072959098362];
observed{22}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{22}	=[0.00141212262687182,-0.0051816220262050905,-0.0037811609045558248,0.009743607221564976,0.002779457744284841,-0.012645421398743861,4.318146239323806E-5,0.013601682503350365,-0.003437548953920122,-0.012810368193320806];
obs_stdev{22}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{22}	=0.03333201659669052;
costTotal{22}	=0.03333201659669052;
costWeakConstraintPenalty{22}	=0.8227539062499852;
% SimulationKwadraticCostFunction: evaluation 22 : cost = 0.856
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
evaluatedParameters{23}	=[8.1796875,1.6984640624999991];
% ========================================================================
% evaluate no. 23
predicted{23}	=[-0.03633900723934205,-0.6185645308992951,0.16305755494104818,0.4489100342693079,-0.2253724296920858,-0.3024954632247201,0.2423104875904715,0.18415392808614342,-0.22982316284271903,-0.09419912596958946];
observed{23}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{23}	=[-0.006795431944402595,-0.012357639382050811,0.012894374264137115,0.016855922688776392,-0.0199139699246538,-0.015483315301854317,0.025370166157220014,0.01046671928026438,-0.02814038013429493,-0.0038219718147149695];
obs_stdev{23}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{23}	=0.14226307103250133;
costTotal{23}	=0.14226307103250133;
costWeakConstraintPenalty{23}	=0.5318908691406181;
% SimulationKwadraticCostFunction: evaluation 23 : cost = 0.674
% CONTRACT INSIDE!
% costs[0.6741539401731195,0.6951468833534119,0.7165542204842219]costs in simplex
% Application starting next step
costs{11}	=[0.6741539401731195,0.6951468833534119,0.7165542204842219];
% Iteration step no.1
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
evaluatedParameters{24}	=[8.4609375,1.6906078124999993];
% ========================================================================
% evaluate no. 24
predicted{24}	=[-0.03255818609522998,-0.6249067394167289,0.15387342093513848,0.46179206070818085,-0.21619436489200347,-0.3202340626970874,0.23636177049848672,0.20434040343630994,-0.2285135722565124,-0.11445428642215523];
observed{24}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{24}	=[-0.010576253088514669,-0.006015430864616977,0.022078508270046815,0.003973896249903464,-0.02909203472473612,0.002255284170512961,0.03131888324920479,-0.009719756069902136,-0.029449970720501567,0.0164331886378508];
obs_stdev{24}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{24}	=0.18577121390555762;
costTotal{24}	=0.18577121390555762;
costWeakConstraintPenalty{24}	=0.5604553222656196;
% SimulationKwadraticCostFunction: evaluation 24 : cost = 0.746
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
evaluatedParameters{25}	=[8.279296875,1.700919140624999];
% ========================================================================
% evaluate no. 25
predicted{25}	=[-0.03890109758789488,-0.6196645548102475,0.16838883564311016,0.4492268390074284,-0.2321765398665684,-0.30117685725106685,0.2493434860757678,0.18108029357153313,-0.23611249120352634,-0.0896998723174215];
observed{25}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{25}	=[-0.004233341595849767,-0.011257615471098381,0.007563093562075129,0.016539117950655935,-0.013109859750171204,-0.01680192127550756,0.0183371676719237,0.013540353794874671,-0.021851051773487618,-0.008321225466882923];
obs_stdev{25}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{25}	=0.09979375155955739;
costTotal{25}	=0.09979375155955739;
costWeakConstraintPenalty{25}	=0.5747776031494064;
% SimulationKwadraticCostFunction: evaluation 25 : cost = 0.675
% CONTRACT INSIDE!
% costs[0.6741539401731195,0.6745713547089638,0.6951468833534119]costs in simplex
% Application starting next step
costs{12}	=[0.6741539401731195,0.6745713547089638,0.6951468833534119];
% Iteration step no.1
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
evaluatedParameters{26}	=[7.958984375,1.7028832031249994];
% ========================================================================
% evaluate no. 26
predicted{26}	=[-0.037898291907395396,-0.6137424557943423,0.16706977850750515,0.43991317825310355,-0.22856904188379507,-0.2909217607357172,0.24300126146852696,0.1718009697336784,-0.22752138949740067,-0.08261938062697326];
observed{26}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{26}	=[-0.0052361472763492525,-0.017179714487003617,0.008882150697680141,0.02585277870498076,-0.016717357732944527,-0.02705701779085723,0.024679392279164547,0.022819677632729407,-0.030442153479613293,-0.015401717157331171];
obs_stdev{26}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{26}	=0.21875592738234662;
costTotal{26}	=0.21875592738234662;
costWeakConstraintPenalty{26}	=0.5529117584228468;
% SimulationKwadraticCostFunction: evaluation 26 : cost = 0.772
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
evaluatedParameters{27}	=[8.36474609375,1.6980958007812488];
% ========================================================================
% evaluate no. 27
predicted{27}	=[-0.037431816564864516,-0.6217143043180191,0.16493122797592424,0.45356358960207055,-0.22867944212237104,-0.3072827021021312,0.24699896958521333,0.1881187985828188,-0.2354590656898338,-0.09681091679508058];
observed{27}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{27}	=[-0.005702622618880132,-0.009207865963326789,0.011020701229261048,0.012202367356013766,-0.01660695749436855,-0.010696076424443202,0.020681684162478176,0.006501848783589004,-0.022504477287180163,-0.0012101809892238469];
obs_stdev{27}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{27}	=0.08778887732510199;
costTotal{27}	=0.08778887732510199;
costWeakConstraintPenalty{27}	=0.5792957544326686;
% SimulationKwadraticCostFunction: evaluation 27 : cost = 0.667
% CONTRACT INSIDE!
% costs[0.6670846317577706,0.6741539401731195,0.6745713547089638]costs in simplex
% costs[0.6670846317577706,0.6741539401731195,0.6745713547089638]costs in simplex
% Convergence on absolute error max(cost)-min(cost)=0.0074867229511932365<0.01
% ===================================================================
% SimulationKwadraticCostfunction: optimal results
%     number of evaluations: 27
%     all cost values:
%         [17.756,19.151,0.735,58.925,5.509,18.285,5.515,5.822,1.161,5.329,...,0.695,1.022,1.025,0.717,0.856,0.674,0.746,0.675,0.772,0.667]
%     all parameter values
%         [8,9,8,9,8.25,8.5,8,7.5,7.5,7.25,8,8.25,7.5,7.625,7.875,7.688,8.188,8.5,8.906,8.312,8.219,8.719,8.18,8.461,8.279,7.959,8.365;1.571,1.571,1.696,1.445,1.634,1.571,1.634,1.634,1.696,1.759,1.696,1.728,1.759,1.728,1.665,1.712,1.712,1.696,1.689,1.681,1.704,1.704,1.698,1.691,1.701,1.703,1.698]
%     number of observations: 10
%     best cost:
%         cost = 0.667
%     best parameters:
%                 [8.365,1.698]
% ===================================================================
% Error estimate for this outer iteration
parameterErrorEstimateStd{1}	=[0.8368907087262158,0.02068464863005931];
parameterErrorCorrelations{1}	=[1.0,-0.07689572583985142;-0.07689572583985117,1.0];
% Application Done
