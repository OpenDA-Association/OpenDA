% Starting Algorithm: 
%	className: org.openda.algorithms.Powell
%	dir.: /home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./algorithm
%	config.: powellAlgorithm.xml
% configstring = powellAlgorithm.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./algorithm/powellAlgorithm.xml
% costFunction@class=org.openda.algorithms.SimulationKwadraticCostFunction
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/deltares/src/openda_20101025/public/tests/simple_oscillator/./model/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
% costFunction@weakParameterConstraint=false
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
% SimulationKwadraticCostFunction: evaluation 3 : cost = 18.651
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
% SimulationKwadraticCostFunction: evaluation 4 : cost = 17.386
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
evaluatedParameters{5}	=[6.741095761542893,1.5708];
% ========================================================================
% evaluate no. 5
predicted{5}	=[0.07026545166603368,-0.5937709675169015,-0.059410903459682654,0.4406171240041819,0.04948990986655807,-0.32690152143766094,-0.040739982978436944,0.24248526418759142,0.03321498575808285,-0.17983169851660746];
observed{5}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{5}	=[-0.11339989084977833,-0.03715120276444439,0.23536283266486796,0.02514883295390241,-0.29477630948329764,0.008922742911086523,0.30842063672612846,-0.04786461682118362,-0.2911785287350968,0.08181060073230303];
obs_stdev{5}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{5}	=17.306639128735785;
costTotal{5}	=17.306639128735785;
% SimulationKwadraticCostFunction: evaluation 5 : cost = 17.307
% ---bracket 0.0,-1.2589042384571068,-1.618034
% --- f values 17.756295868749987,17.306639128735785,17.385858824346535
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
evaluatedParameters{6}	=[6.741095761542893,1.5708];
% ========================================================================
% evaluate no. 6
predicted{6}	=[0.07026545166603368,-0.5937709675169015,-0.059410903459682654,0.4406171240041819,0.04948990986655807,-0.32690152143766094,-0.040739982978436944,0.24248526418759142,0.03321498575808285,-0.17983169851660746];
observed{6}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{6}	=[-0.11339989084977833,-0.03715120276444439,0.23536283266486796,0.02514883295390241,-0.29477630948329764,0.008922742911086523,0.30842063672612846,-0.04786461682118362,-0.2911785287350968,0.08181060073230303];
obs_stdev{6}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{6}	=17.306639128735785;
costTotal{6}	=17.306639128735785;
% SimulationKwadraticCostFunction: evaluation 6 : cost = 17.307
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
evaluatedParameters{7}	=[7.221954377889401,1.5708];
% ========================================================================
% evaluate no. 7
predicted{7}	=[0.06589739010714536,-0.6057829201374564,-0.05640429952830434,0.45864695342435347,0.04764161989178271,-0.34719565480346143,-0.03981244827228571,0.26278715733638197,0.03297878123195544,-0.19886935605135678];
observed{7}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{7}	=[-0.10903182929089,-0.025139250143889513,0.23235622873348963,0.00711900353373085,-0.2929280195085223,0.029216876276887016,0.3074931020199772,-0.06816650996997417,-0.29094232420896937,0.10084825826705235];
obs_stdev{7}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{7}	=17.361847874720592;
costTotal{7}	=17.361847874720592;
% SimulationKwadraticCostFunction: evaluation 7 : cost = 17.362
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
evaluatedParameters{8}	=[6.924767403794303,1.5708];
% ========================================================================
% evaluate no. 8
predicted{8}	=[0.06853040477251666,-0.5985300849835807,-0.05822829801445151,0.44771776274158737,0.04877469949418561,-0.33484573685767555,-0.04039343748891947,0.25038428694188336,0.03314250931850793,-0.18719356922236044];
observed{8}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{8}	=[-0.1116648439562613,-0.03239208529776516,0.2341802272196368,0.01804819421649695,-0.2940610991109252,0.016866958331101134,0.308074091236611,-0.05576363957547556,-0.2911060522955219,0.08917247143805601];
obs_stdev{8}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{8}	=17.307725876496036;
costTotal{8}	=17.307725876496036;
% SimulationKwadraticCostFunction: evaluation 8 : cost = 17.308
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
evaluatedParameters{9}	=[6.824857851264803,1.5708];
% ========================================================================
% evaluate no. 9
predicted{9}	=[0.06946343493264029,-0.5959688569206382,-0.05886616413771962,0.443889420996183,0.04916240110319847,-0.3305547602328911,-0.04058333807732208,0.24610993037772652,0.033184907674552754,-0.1832026016409999];
observed{9}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{9}	=[-0.11259787411638494,-0.03495331336070773,0.2348180933429049,0.02187653596190131,-0.29444880071993806,0.012575981706316686,0.3082639918250136,-0.05148928301131872,-0.2911484506515667,0.08518150385669548];
obs_stdev{9}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{9}	=17.303876527780282;
costTotal{9}	=17.303876527780282;
% SimulationKwadraticCostFunction: evaluation 9 : cost = 17.304
% ---linesearch -1.0752325962056968,-1.1751421487351967,-1.2589042384571068
% --- f values 17.307725876496036,17.303876527780282,17.306639128735785
p_dir{1}	=[6.824857851264803,1.5708];
f_p_dir{1}	=17.303876527780282;
% direction 1
% direction 1
searchDir{2}	=[0.0,0.1257];
% Start line optimization in point [6.824857851264803,1.5708] in direction:[0.0,0.1257]
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
evaluatedParameters{10}	=[6.824857851264803,1.5708];
% ========================================================================
% evaluate no. 10
predicted{10}	=[0.06946343493264029,-0.5959688569206382,-0.05886616413771962,0.443889420996183,0.04916240110319847,-0.3305547602328911,-0.04058333807732208,0.24610993037772652,0.033184907674552754,-0.1832026016409999];
observed{10}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{10}	=[-0.11259787411638494,-0.03495331336070773,0.2348180933429049,0.02187653596190131,-0.29444880071993806,0.012575981706316686,0.3082639918250136,-0.05148928301131872,-0.2911484506515667,0.08518150385669548];
obs_stdev{10}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{10}	=17.303876527780282;
costTotal{10}	=17.303876527780282;
% SimulationKwadraticCostFunction: evaluation 10 : cost = 17.304
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
evaluatedParameters{11}	=[6.824857851264803,1.6965];
% ========================================================================
% evaluate no. 11
predicted{11}	=[-0.02280558103339831,-0.5920992039272197,0.13880807073266196,0.4131458968856403,-0.1885336569498002,-0.2694202904946572,0.19606384627531154,0.16065375738698603,-0.1793078947544898,-0.08296195338396917];
observed{11}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{11}	=[-0.020328858150346338,-0.03882296635412619,0.03714385847252333,0.05262006007244402,-0.05675274266693939,-0.0485584880319172,0.07161680747237997,0.033966889979421766,-0.07865564822252416,-0.015059144400335261];
obs_stdev{11}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{11}	=1.217201412471818;
costTotal{11}	=1.217201412471818;
% SimulationKwadraticCostFunction: evaluation 11 : cost = 1.217
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
evaluatedParameters{12}	=[6.824857851264803,1.8998868737999999];
% ========================================================================
% evaluate no. 12
predicted{12}	=[-0.16892678255758503,-0.5040482656600312,0.40274411143589045,0.15490410844466512,-0.3854859790499136,0.09607842284924539,0.23482009564448714,-0.20059157001567224,-0.0650465401950247,0.18535002487067032];
observed{12}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{12}	=[0.12579234337384038,-0.12687390462131465,-0.22679218223070516,0.3108618485134192,0.14019957943317402,-0.4140572013758198,0.032860558103204374,0.39521221738208,-0.19291700278198926,-0.28337112265497477];
obs_stdev{12}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{12}	=32.29392241779712;
costTotal{12}	=32.29392241779712;
% SimulationKwadraticCostFunction: evaluation 12 : cost = 32.294
% ---bracket 0.0,1.0,2.6180339999999998
% --- f values 17.303876527780282,1.217201412471818,32.29392241779712
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
evaluatedParameters{13}	=[6.824857851264803,1.6965];
% ========================================================================
% evaluate no. 13
predicted{13}	=[-0.02280558103339831,-0.5920992039272197,0.13880807073266196,0.4131458968856403,-0.1885336569498002,-0.2694202904946572,0.19606384627531154,0.16065375738698603,-0.1793078947544898,-0.08296195338396917];
observed{13}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{13}	=[-0.020328858150346338,-0.03882296635412619,0.03714385847252333,0.05262006007244402,-0.05675274266693939,-0.0485584880319172,0.07161680747237997,0.033966889979421766,-0.07865564822252416,-0.015059144400335261];
obs_stdev{13}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{13}	=1.217201412471818;
costTotal{13}	=1.217201412471818;
% SimulationKwadraticCostFunction: evaluation 13 : cost = 1.217
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
evaluatedParameters{14}	=[6.824857851264803,1.7741868706378907];
% ========================================================================
% evaluate no. 14
predicted{14}	=[-0.07931569537411423,-0.569929296910234,0.2521810917259664,0.33975663623695046,-0.3031862143062007,-0.15077717664425846,0.27723554633278547,0.018589236042574914,-0.21310982684197186,0.058304531939313654];
observed{14}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{14}	=[0.03618125619036958,-0.06099287337111192,-0.07622916252078113,0.12600932072113386,0.05789981468946109,-0.16720160188231595,-0.009554892585093955,0.17603141132383288,-0.044853716135042104,-0.15632562972361808];
obs_stdev{14}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{14}	=5.777756217323354;
costTotal{14}	=5.777756217323354;
% SimulationKwadraticCostFunction: evaluation 14 : cost = 5.778
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
evaluatedParameters{15}	=[6.824857851264803,1.6484868738];
% ========================================================================
% evaluate no. 15
predicted{15}	=[0.01237208691785124,-0.5983099420799927,0.0642598088069854,0.43843905830406577,-0.1017896473148839,-0.31456760046453264,0.1145715639277261,0.2205910166776876,-0.11256375947991235,-0.1507322389734366];
observed{15}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{15}	=[-0.055506526101595885,-0.03261222820135323,0.1116921203981999,0.02732689865401855,-0.1434967523018557,-0.0034111780620417753,0.1531090898199654,-0.0259703693112798,-0.14539978349710161,0.05271114118913217];
obs_stdev{15}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{15}	=4.300289499414775;
costTotal{15}	=4.300289499414775;
% SimulationKwadraticCostFunction: evaluation 15 : cost = 4.3
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
evaluatedParameters{16}	=[6.824857851264803,1.7053268915920554];
% ========================================================================
% evaluate no. 16
predicted{16}	=[-0.029255858225237182,-0.5903280621266669,0.15222110355675136,0.40675373912532037,-0.20340226403991366,-0.2585041891417985,0.2088361945330104,0.1467115230490847,-0.18819614446262026,-0.06787469045073427];
observed{16}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{16}	=[-0.013878580958507466,-0.04059410815467901,0.02373082564843393,0.05901221783276395,-0.04188413557682594,-0.0594745893847759,0.05884445921468112,0.04790912431732311,-0.0697673985143937,-0.030146407333570158];
obs_stdev{16}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{16}	=1.1355924252503982;
costTotal{16}	=1.1355924252503982;
% SimulationKwadraticCostFunction: evaluation 16 : cost = 1.136
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
evaluatedParameters{17}	=[6.824857851264803,1.706972325945245];
% ========================================================================
% evaluate no. 17
predicted{17}	=[-0.030457564244074246,-0.5899764512627479,0.1547090292132327,0.4055041930694344,-0.2061294503664516,-0.256386467839045,0.2111282197190289,0.14402813302092943,-0.18972071605221377,-0.06499940490065598];
observed{17}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{17}	=[-0.012676874939670402,-0.040945719018598004,0.021242899991952585,0.0602617638886499,-0.03915694925028798,-0.061592310687529406,0.05655243402866261,0.05059251434547837,-0.06824282692480019,-0.03302169288364845];
obs_stdev{17}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{17}	=1.1376085695799099;
costTotal{17}	=1.1376085695799099;
% SimulationKwadraticCostFunction: evaluation 17 : cost = 1.138
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
evaluatedParameters{18}	=[6.824857851264803,1.704017529962329];
% ========================================================================
% evaluate no. 18
predicted{18}	=[-0.028299434756914897,-0.5906030474804929,0.15023846966069065,0.40773511546450253,-0.20122189710509586,-0.2601710040443692,0.206992004994935,0.14882840222113514,-0.18695286963333346,-0.07014945394003098];
observed{18}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{18}	=[-0.014835004426829751,-0.040319122800853036,0.025713459544494638,0.058030841493581786,-0.04406450251164373,-0.0578077744822052,0.06068864875275651,0.045792245145272664,-0.0710106733436805,-0.027871643844273444];
obs_stdev{18}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{18}	=1.137863741017159;
costTotal{18}	=1.137863741017159;
% SimulationKwadraticCostFunction: evaluation 18 : cost = 1.138
% ---linesearch 1.0598053298514638,1.0702218901515939,1.0833120600258148
% --- f values 1.137863741017159,1.1355924252503982,1.1376085695799099
p_dir{2}	=[6.824857851264803,1.7053268915920554];
f_p_dir{2}	=1.1355924252503982;
% End point of this outer loop has cost 1.1355924252503982 at [6.824857851264803,1.7053268915920554]
% Relative convergence check for outer loop: 1.759559784055883<0.01
% Absolute convergence check for outer loop: 16.62070344349959<0.01
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
evaluatedParameters{19}	=[5.6497157025296065,1.8398537831841109];
% ========================================================================
% evaluate no. 19
predicted{19}	=[-0.11004773650532598,-0.5140037767270895,0.299083934780578,0.23168197650901512,-0.309913670016271,-0.028853106074152472,0.22997381802057462,-0.0790054539518975,-0.12731422005531465,0.110400833322324];
observed{19}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{19}	=[0.06691329732158133,-0.11691839355425637,-0.1231320055753927,0.2340839804490692,0.06462727039953142,-0.28912567245242193,0.03770683572711689,0.2736261013183053,-0.1306493229216993,-0.20842193110662843];
obs_stdev{19}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{19}	=15.633821754466714;
costTotal{19}	=15.633821754466714;
% SimulationKwadraticCostFunction: evaluation 19 : cost = 15.634
% Candidate direction :[-1.1751421487351967,0.13452689159205544]
% Is new direction a good one,i.e -2.122474114283273<0 and -60.097395074275106<0
% Application starting next step
% =================================================================
% Outer loop iteration no. 2
% =================================================================
p_start{2}	=[6.824857851264803,1.7053268915920554];
f_p_start{2}	=1.1355924252503982;
% direction 0
% direction 0
searchDir{3}	=[1.1751421487351967,0.0];
% Start line optimization in point [6.824857851264803,1.7053268915920554] in direction:[1.1751421487351967,0.0]
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
evaluatedParameters{20}	=[6.824857851264803,1.7053268915920554];
% ========================================================================
% evaluate no. 20
predicted{20}	=[-0.029255858225237182,-0.5903280621266669,0.15222110355675136,0.40675373912532037,-0.20340226403991366,-0.2585041891417985,0.2088361945330104,0.1467115230490847,-0.18819614446262026,-0.06787469045073427];
observed{20}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{20}	=[-0.013878580958507466,-0.04059410815467901,0.02373082564843393,0.05901221783276395,-0.04188413557682594,-0.0594745893847759,0.05884445921468112,0.04790912431732311,-0.0697673985143937,-0.030146407333570158];
obs_stdev{20}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{20}	=1.1355924252503982;
costTotal{20}	=1.1355924252503982;
% SimulationKwadraticCostFunction: evaluation 20 : cost = 1.136
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
evaluatedParameters{21}	=[8.0,1.7053268915920554];
% ========================================================================
% evaluate no. 21
predicted{21}	=[-0.04002954409142044,-0.6138860331704719,0.17157249129785102,0.43885602960543985,-0.23398843976536227,-0.28826776532179127,0.2481580972917939,0.1677488155751591,-0.23163042468199033,-0.07766844633543266];
observed{21}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{21}	=[-0.003104895092324207,-0.01703613711087404,0.004379437907334277,0.02690992735264447,-0.011297959851377326,-0.029711013204783143,0.01952255645589762,0.02687183179124869,-0.026333118295023633,-0.020352651448871772];
obs_stdev{21}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{21}	=0.21322356523458078;
costTotal{21}	=0.21322356523458078;
% SimulationKwadraticCostFunction: evaluation 21 : cost = 0.213
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
evaluatedParameters{22}	=[9.901419951486606,1.7053268915920554];
% ========================================================================
% evaluate no. 22
predicted{22}	=[-0.05231432723898796,-0.6412773107935604,0.19480352046754443,0.47779810259297767,-0.27246817840687126,-0.32580329112741,0.29988769367283397,0.19510635419337888,-0.2913018589194845,-0.09034941954307156];
observed{22}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{22}	=[0.009179888055243314,0.010355140512214533,-0.018851591262359135,-0.012032145634893354,0.027181778790131672,0.00782451260083561,-0.032207039925142456,-4.8570682697107737E-4,0.03333831594247055,-0.007671678241232868];
obs_stdev{22}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{22}	=0.18497768429258055;
costTotal{22}	=0.18497768429258055;
% SimulationKwadraticCostFunction: evaluation 22 : cost = 0.185
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
evaluatedParameters{23}	=[8.980385394059525,1.7053268915920554];
% ========================================================================
% evaluate no. 23
predicted{23}	=[-0.04697799835684052,-0.6293086991098437,0.1845592344811463,0.4605671586738546,-0.25526659141930635,-0.3090029489589266,0.2764628627284314,0.18274876562764028,-0.2639435476899906,-0.08462493266970637];
observed{23}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{23}	=[0.0038435591730958743,-0.0016134711715022343,-0.008607305275961014,0.0051987982842297,0.009980191802566762,-0.008975829567647808,-0.008782208980739892,0.01187188173876752,0.005980004712976661,-0.013396165114598055];
obs_stdev{23}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{23}	=0.036597282782050135;
costTotal{23}	=0.036597282782050135;
% SimulationKwadraticCostFunction: evaluation 23 : cost = 0.037
% ---bracket 1.0,1.8342696201601745,2.6180339999999998
% --- f values 0.21322356523458078,0.036597282782050135,0.18497768429258055
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
evaluatedParameters{24}	=[8.980385394059525,1.7053268915920554];
% ========================================================================
% evaluate no. 24
predicted{24}	=[-0.04697799835684052,-0.6293086991098437,0.1845592344811463,0.4605671586738546,-0.25526659141930635,-0.3090029489589266,0.2764628627284314,0.18274876562764028,-0.2639435476899906,-0.08462493266970637];
observed{24}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{24}	=[0.0038435591730958743,-0.0016134711715022343,-0.008607305275961014,0.0051987982842297,0.009980191802566762,-0.008975829567647808,-0.008782208980739892,0.01187188173876752,0.005980004712976661,-0.013396165114598055];
obs_stdev{24}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{24}	=0.036597282782050135;
costTotal{24}	=0.036597282782050135;
% SimulationKwadraticCostFunction: evaluation 24 : cost = 0.037
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
evaluatedParameters{25}	=[8.605911506632184,1.7053268915920554];
% ========================================================================
% evaluate no. 25
predicted{25}	=[-0.0445002048023138,-0.6237881767017066,0.17988254401841683,0.4527321465631677,-0.24753549319749046,-0.30146418965726485,0.2660914451320959,0.17726282578874808,-0.2520063546880387,-0.08208231424258115];
observed{25}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{25}	=[0.0013657656185691522,-0.007133993579639286,-0.003930614813231537,0.013033810394916634,0.0022490935807508716,-0.016514588869309565,0.0015892086155955831,0.01735782157765972,-0.005957188288975235,-0.015938783541723275];
obs_stdev{25}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{25}	=0.055461583042154775;
costTotal{25}	=0.055461583042154775;
% SimulationKwadraticCostFunction: evaluation 25 : cost = 0.055
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
evaluatedParameters{26}	=[9.332189279821716,1.7053268915920554];
% ========================================================================
% evaluate no. 26
predicted{26}	=[-0.04913411959909847,-0.6341314829498814,0.1886700432203152,0.4674700920671149,-0.26212541885561613,-0.31569699040872085,0.28574598034505183,0.1876508230771187,-0.27472059425580975,-0.08689601296528857];
observed{26}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{26}	=[0.005999680415353821,0.0032093126685355244,-0.0127181140151299,-0.0017041351090305579,0.01683901923887654,-0.002281788117853567,-0.01806532659736032,0.006969824289289106,0.016757051278795787,-0.011125084819015862];
obs_stdev{26}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{26}	=0.06396051174439281;
costTotal{26}	=0.06396051174439281;
% SimulationKwadraticCostFunction: evaluation 26 : cost = 0.064
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
evaluatedParameters{27}	=[8.935891672483965,1.7053268915920554];
% ========================================================================
% evaluate no. 27
predicted{27}	=[-0.04669387327506441,-0.6286744889308291,0.18402039248522428,0.45966344319127844,-0.2543719152408173,-0.30813019151757837,0.27525760819692235,0.18211177726089767,-0.2625506932253093,-0.08432976814090862];
observed{27}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{27}	=[0.0035594340913197645,-0.0022476813505167836,-0.00806846328003899,0.006102513766805873,0.009085515624077722,-0.009848587008996046,-0.007576954449230844,0.012508870105510134,0.004587150248295313,-0.013691329643395811];
obs_stdev{27}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{27}	=0.036099010077761895;
costTotal{27}	=0.036099010077761895;
% SimulationKwadraticCostFunction: evaluation 27 : cost = 0.036
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
evaluatedParameters{28}	=[8.916208766493154,1.7053268915920554];
% ========================================================================
% evaluate no. 28
predicted{28}	=[-0.04656732826649873,-0.6283921198435809,0.18378061499421947,0.45926138424620233,-0.2539741230150638,-0.3077421746653038,0.27472214712856735,0.1818287386090231,-0.2619323596452944,-0.08419861022562142];
observed{28}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{28}	=[0.003432889082754083,-0.002530050437764997,-0.007828685789034173,0.006504572711881984,0.008687723398324187,-0.010236603861270621,-0.007041493380875841,0.012791908757384701,0.003968816668280417,-0.013822487558683011];
obs_stdev{28}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{28}	=0.036103824661231174;
costTotal{28}	=0.036103824661231174;
% SimulationKwadraticCostFunction: evaluation 28 : cost = 0.036
% ---linesearch 1.8342696201601745,1.7964072035806598,1.7796578205277276
% --- f values 0.036597282782050135,0.036099010077761895,0.036103824661231174
p_dir{3}	=[8.935891672483965,1.7053268915920554];
f_p_dir{3}	=0.036099010077761895;
% direction 1
% direction 1
searchDir{4}	=[0.0,0.13452689159205536];
% Start line optimization in point [8.935891672483965,1.7053268915920554] in direction:[0.0,0.13452689159205536]
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
evaluatedParameters{29}	=[8.935891672483965,1.7053268915920554];
% ========================================================================
% evaluate no. 29
predicted{29}	=[-0.04669387327506441,-0.6286744889308291,0.18402039248522428,0.45966344319127844,-0.2543719152408173,-0.30813019151757837,0.27525760819692235,0.18211177726089767,-0.2625506932253093,-0.08432976814090862];
observed{29}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{29}	=[0.0035594340913197645,-0.0022476813505167836,-0.00806846328003899,0.006102513766805873,0.009085515624077722,-0.009848587008996046,-0.007576954449230844,0.012508870105510134,0.004587150248295313,-0.013691329643395811];
obs_stdev{29}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{29}	=0.036099010077761895;
costTotal{29}	=0.036099010077761895;
% SimulationKwadraticCostFunction: evaluation 29 : cost = 0.036
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
evaluatedParameters{30}	=[8.935891672483965,1.8398537831841109];
% ========================================================================
% evaluate no. 30
predicted{30}	=[-0.14573301222654858,-0.571150828453314,0.3846527640629186,0.27602616702224814,-0.4371045215232545,-0.015460022099362696,0.35670674507401595,-0.1551075009465677,-0.2123534699463797,0.22369860483612422];
observed{30}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{30}	=[0.10259857304280393,-0.0597713418280319,-0.2087008348577333,0.18973978993583618,0.19181812190651493,-0.3025187564272117,-0.08902609132632444,0.3497281483129755,-0.045610073030634246,-0.32171970262042865];
obs_stdev{30}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{30}	=22.889368260343243;
costTotal{30}	=22.889368260343243;
% SimulationKwadraticCostFunction: evaluation 30 : cost = 22.889
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
evaluatedParameters{31}	=[8.935891672483965,1.4876578070817956];
% ========================================================================
% evaluate no. 31
predicted{31}	=[0.11616023718082613,-0.6214471718991954,-0.18981915760146,0.4672092644423072,0.22464364109015383,-0.3384688657954133,-0.2323993949271949,0.2343358141156494,0.22235377883219332,-0.15265263585349112];
observed{31}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{31}	=[-0.1592946763645708,-0.00947499838215049,0.3657710868066453,-0.0014433074842228888,-0.4699300407068934,0.020490087268838886,0.5000800486748864,-0.03971516674924161,-0.4803173218092073,0.05463153806918669];
obs_stdev{31}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{31}	=43.2927955781041;
costTotal{31}	=43.2927955781041;
% SimulationKwadraticCostFunction: evaluation 31 : cost = 43.293
% ---bracket 1.0,0.0,-1.618034
% --- f values 22.889368260343243,0.036099010077761895,43.2927955781041
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
evaluatedParameters{32}	=[8.935891672483965,1.7053268915920554];
% ========================================================================
% evaluate no. 32
predicted{32}	=[-0.04669387327506441,-0.6286744889308291,0.18402039248522428,0.45966344319127844,-0.2543719152408173,-0.30813019151757837,0.27525760819692235,0.18211177726089767,-0.2625506932253093,-0.08432976814090862];
observed{32}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{32}	=[0.0035594340913197645,-0.0022476813505167836,-0.00806846328003899,0.006102513766805873,0.009085515624077722,-0.009848587008996046,-0.007576954449230844,0.012508870105510134,0.004587150248295313,-0.013691329643395811];
obs_stdev{32}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{32}	=0.036099010077761895;
costTotal{32}	=0.036099010077761895;
% SimulationKwadraticCostFunction: evaluation 32 : cost = 0.036
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
evaluatedParameters{33}	=[8.935891672483965,1.6221847020580096];
% ========================================================================
% evaluate no. 33
predicted{33}	=[0.015427288376061015,-0.6408802338334574,0.04211015413112961,0.5087821271819561,-0.07688723578516687,-0.4002203376273791,0.09546771558519977,0.3118508654213715,-0.10281499242294938,-0.24057866178460205];
observed{33}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{33}	=[-0.05856172755980566,0.009958063552111485,0.13384177507405567,-0.04301617022387183,-0.16839916383157272,0.08224155910080466,0.17221293816249172,-0.11723021805496373,-0.15514855055406457,0.14255756400029762];
obs_stdev{33}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{33}	=7.310427746560132;
costTotal{33}	=7.310427746560132;
% SimulationKwadraticCostFunction: evaluation 33 : cost = 7.31
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
evaluatedParameters{34}	=[8.935891672483965,1.7567115902659065];
% ========================================================================
% evaluate no. 34
predicted{34}	=[-0.08480817052760622,-0.6120683634059122,0.2662808140182951,0.4029766677254139,-0.34355763058759703,-0.21075664931285923,0.34300492839540037,0.05726324651743149,-0.2927886340892786,0.049164915692444966];
observed{34}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{34}	=[0.04167373134386158,-0.0188538068754337,-0.09032888481310983,0.06278928923267041,0.09827123097085744,-0.10722212921371518,-0.07532427464770886,0.1373574008489763,0.03482509111226462,-0.1471860134767494];
obs_stdev{34}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{34}	=4.138254973736798;
costTotal{34}	=4.138254973736798;
% SimulationKwadraticCostFunction: evaluation 34 : cost = 4.138
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
evaluatedParameters{35}	=[8.935891672483965,1.69892722915175];
% ========================================================================
% evaluate no. 35
predicted{35}	=[-0.04192860209297581,-0.6302633462167149,0.17341179954920882,0.46538168152492926,-0.24193096793869445,-0.3183567145866054,0.2640762019799411,0.1959005285261552,-0.2546004490457223,-0.10010465709905667];
observed{35}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{35}	=[-0.0012058370907688382,-6.58824064631025E-4,0.0025401296559764697,3.8427543315505686E-4,-0.00335543167804514,3.7793606003100333E-4,0.0036044517677504273,-0.0012798811597473925,-0.0033630939312916386,0.002083559314752237];
obs_stdev{35}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{35}	=0.0025085782232353005;
costTotal{35}	=0.0025085782232353005;
% SimulationKwadraticCostFunction: evaluation 35 : cost = 2.509E-3
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
evaluatedParameters{36}	=[8.935891672483965,1.7000937846140358];
% ========================================================================
% evaluate no. 36
predicted{36}	=[-0.042797495487928716,-0.6299817143748582,0.17535071452874837,0.46436244004606375,-0.24421806116632697,-0.31652764908768183,0.2661552490449871,0.1934247120827004,-0.25611426635206125,-0.09725807259819884];
observed{36}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{36}	=[-3.369436958159319E-4,-9.404559064877249E-4,6.012146764369231E-4,0.0014035169120205637,-0.0010683384504126259,-0.0014511294388925844,0.0015254047027044026,0.0011959352837074,-0.0018492766249527115,-7.630251861055898E-4];
obs_stdev{36}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{36}	=7.167791524408185E-4;
costTotal{36}	=7.167791524408185E-4;
% SimulationKwadraticCostFunction: evaluation 36 : cost = 7.168E-4
% ---linesearch 0.0,-0.038900080988184205,-0.047571622034587945
% --- f values 0.036099010077761895,7.167791524408185E-4,0.0025085782232353005
p_dir{4}	=[8.935891672483965,1.7000937846140358];
f_p_dir{4}	=7.167791524408185E-4;
% End point of this outer loop has cost 7.167791524408185E-4 at [8.935891672483965,1.7000937846140358]
% Relative convergence check for outer loop: 1.9974768165226033<0.01
% Absolute convergence check for outer loop: 1.1348756460979574<0.01
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
evaluatedParameters{37}	=[11.046925493703128,1.6948606776360162];
% ========================================================================
% evaluate no. 37
predicted{37}	=[-0.04987701347172808,-0.6564608621661265,0.187145093942157,0.5062618459281659,-0.26838427298742173,-0.3629266392157521,0.30439469266786,0.23534513650560673,-0.30615872338845823,-0.12850040996236164];
observed{37}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{37}	=[0.00674257428798343,0.02553869188478064,-0.011193164736971711,-0.040495888970081606,0.023097873370682143,0.044947860689177666,-0.0367140389201685,-0.04072448913919893,0.04819518041144427,0.030479312178057216];
obs_stdev{37}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{37}	=0.5637440770761101;
costTotal{37}	=0.5637440770761101;
% SimulationKwadraticCostFunction: evaluation 37 : cost = 0.564
% Candidate direction :[2.111033821219162,-0.005233106978019597]
% Is new direction a good one,i.e -0.5718483481742881<0 and -0.35529471098246523<0
% Application starting next step
% =================================================================
% Outer loop iteration no. 3
% =================================================================
p_start{3}	=[8.935891672483965,1.7000937846140358];
f_p_start{3}	=7.167791524408185E-4;
% direction 0
% direction 0
searchDir{5}	=[2.1110338212191624,0.0];
% Start line optimization in point [8.935891672483965,1.7000937846140358] in direction:[2.1110338212191624,0.0]
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
evaluatedParameters{38}	=[8.935891672483965,1.7000937846140358];
% ========================================================================
% evaluate no. 38
predicted{38}	=[-0.042797495487928716,-0.6299817143748582,0.17535071452874837,0.46436244004606375,-0.24421806116632697,-0.31652764908768183,0.2661552490449871,0.1934247120827004,-0.25611426635206125,-0.09725807259819884];
observed{38}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{38}	=[-3.369436958159319E-4,-9.404559064877249E-4,6.012146764369231E-4,0.0014035169120205637,-0.0010683384504126259,-0.0014511294388925844,0.0015254047027044026,0.0011959352837074,-0.0018492766249527115,-7.630251861055898E-4];
obs_stdev{38}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{38}	=7.167791524408185E-4;
costTotal{38}	=7.167791524408185E-4;
% SimulationKwadraticCostFunction: evaluation 38 : cost = 7.168E-4
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
evaluatedParameters{39}	=[11.046925493703128,1.7000937846140358];
% ========================================================================
% evaluate no. 39
predicted{39}	=[-0.05382215284901394,-0.6550749644897974,0.19635299649326934,0.5011988134242308,-0.27970639481192777,-0.3535348518230171,0.31511845909568087,0.2221376733457682,-0.3142890485757751,-0.11269196459673798];
observed{39}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{39}	=[0.010687713665269293,0.02415279420845151,-0.02040106728808405,-0.03543285646614647,0.03441999519518818,0.03555607329644267,-0.04743780534798936,-0.0275170259793604,0.05632550559876115,0.014670866812433556];
obs_stdev{39}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{39}	=0.5606787490887801;
costTotal{39}	=0.5606787490887801;
% SimulationKwadraticCostFunction: evaluation 39 : cost = 0.561
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
evaluatedParameters{40}	=[5.520167174601439,1.7000937846140358];
% ========================================================================
% evaluate no. 40
predicted{40}	=[-0.008632161604231234,-0.5551359624269314,0.11654742495125134,0.36320569721276463,-0.15344662522679858,-0.2222617046370287,0.15106618370073405,0.12462902520438739,-0.12996858319643223,-0.06087095324919463];
observed{40}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{40}	=[-0.034502277579513414,-0.07578620785441448,0.059404504253933954,0.10256025974531968,-0.09183977438994101,-0.0957170738895457,0.11661447004695746,0.06999162216202041,-0.12799495978058173,-0.0371501445351098];
obs_stdev{40}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{40}	=3.741918281159873;
costTotal{40}	=3.741918281159873;
% SimulationKwadraticCostFunction: evaluation 40 : cost = 3.742
% ---bracket 1.0,0.0,-1.618034
% --- f values 0.5606787490887801,7.167791524408185E-4,3.741918281159873
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
evaluatedParameters{41}	=[8.935891672483965,1.7000937846140358];
% ========================================================================
% evaluate no. 41
predicted{41}	=[-0.042797495487928716,-0.6299817143748582,0.17535071452874837,0.46436244004606375,-0.24421806116632697,-0.31652764908768183,0.2661552490449871,0.1934247120827004,-0.25611426635206125,-0.09725807259819884];
observed{41}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{41}	=[-3.369436958159319E-4,-9.404559064877249E-4,6.012146764369231E-4,0.0014035169120205637,-0.0010683384504126259,-0.0014511294388925844,0.0015254047027044026,0.0011959352837074,-0.0018492766249527115,-7.630251861055898E-4];
obs_stdev{41}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{41}	=7.167791524408185E-4;
costTotal{41}	=7.167791524408185E-4;
% SimulationKwadraticCostFunction: evaluation 41 : cost = 7.168E-4
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
evaluatedParameters{42}	=[7.631201048925768,1.7000937846140358];
% ========================================================================
% evaluate no. 42
predicted{42}	=[-0.03312719438096081,-0.6083568029144849,0.1577465227544056,0.4338050056169519,-0.2156870697760546,-0.2869001341391168,0.22833218593008417,0.17111537675971888,-0.2128903260686574,-0.08538205176284547];
observed{42}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{42}	=[-0.010007244802783838,-0.02256536736686099,0.0182054064507797,0.031960951341132415,-0.029599329840685,-0.03107864438745761,0.03934846781760734,0.023505270606688916,-0.045073216908356556,-0.01263904602145896];
obs_stdev{42}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{42}	=0.4048211242139776;
costTotal{42}	=0.4048211242139776;
% SimulationKwadraticCostFunction: evaluation 42 : cost = 0.405
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
evaluatedParameters{43}	=[9.742234817039764,1.7000937846140358];
% ========================================================================
% evaluate no. 43
predicted{43}	=[-0.04754420151026304,-0.64072762335172,0.1842712459518979,0.4799550344353028,-0.25910124133532186,-0.3320214894708423,0.28643824059073025,0.20533292794860708,-0.27992258204690906,-0.10363662601906219];
observed{43}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{43}	=[0.004409762326518393,0.00980545307037406,-0.008319316746712602,-0.014189077477218481,0.013814841718582271,0.0140427109442679,-0.018757586843038743,-0.01071228058219928,0.021959039069895103,0.005615528234757763];
obs_stdev{43}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{43}	=0.08772575102632589;
costTotal{43}	=0.08772575102632589;
% SimulationKwadraticCostFunction: evaluation 43 : cost = 0.088
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
evaluatedParameters{44}	=[9.066347780718242,1.7000937846140358];
% ========================================================================
% evaluate no. 44
predicted{44}	=[-0.04361962336598001,-0.6318366634077252,0.176882549808999,0.46703460519170187,-0.24675364038672065,-0.31916496314353526,0.26958456374490575,0.19544008902234752,-0.2601097769979312,-0.09833552107072925];
observed{44}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{44}	=[4.851841822353653E-4,9.144931263792566E-4,-9.306206038137144E-4,-0.001268648233617553,0.0014672407699810597,0.0011861846169608503,-0.001903909997214237,-8.194416559397233E-4,0.002146234020917237,3.1442328642482253E-4];
obs_stdev{44}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{44}	=8.054297110484662E-4;
costTotal{44}	=8.054297110484662E-4;
% SimulationKwadraticCostFunction: evaluation 44 : cost = 8.054E-4
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
evaluatedParameters{45}	=[8.998978016486658,1.7000937846140358];
% ========================================================================
% evaluate no. 45
predicted{45}	=[-0.04319787746560459,-0.6308847620217319,0.17609603869443852,0.4656623239622699,-0.24545071468165203,-0.3178096511633733,0.2678210290434503,0.1944037927778542,-0.25805353595237707,-0.09778140048838414];
observed{45}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{45}	=[6.34382818599441E-5,-3.7408259613957995E-5,-1.4410948925322464E-4,1.0363299581439556E-4,1.6431506491243475E-4,-1.6912736320112698E-4,-1.4037529575877672E-4,2.1685458855361106E-4,8.99929753631068E-5,-2.3969729592028532E-4];
obs_stdev{45}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{45}	=1.1240965363492196E-5;
costTotal{45}	=1.1240965363492196E-5;
% SimulationKwadraticCostFunction: evaluation 45 : cost = 1.124E-5
% ---linesearch 0.06179726109690492,0.02988409914070454,0.0
% --- f values 8.054297110484662E-4,1.1240965363492196E-5,7.167791524408185E-4
p_dir{5}	=[8.998978016486658,1.7000937846140358];
f_p_dir{5}	=1.1240965363492196E-5;
% direction 1
% direction 1
searchDir{6}	=[0.0,0.013452689159205536];
% Start line optimization in point [8.998978016486658,1.7000937846140358] in direction:[0.0,0.013452689159205536]
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
evaluatedParameters{46}	=[8.998978016486658,1.7000937846140358];
% ========================================================================
% evaluate no. 46
predicted{46}	=[-0.04319787746560459,-0.6308847620217319,0.17609603869443852,0.4656623239622699,-0.24545071468165203,-0.3178096511633733,0.2678210290434503,0.1944037927778542,-0.25805353595237707,-0.09778140048838414];
observed{46}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{46}	=[6.34382818599441E-5,-3.7408259613957995E-5,-1.4410948925322464E-4,1.0363299581439556E-4,1.6431506491243475E-4,-1.6912736320112698E-4,-1.4037529575877672E-4,2.1685458855361106E-4,8.99929753631068E-5,-2.3969729592028532E-4];
obs_stdev{46}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{46}	=1.1240965363492196E-5;
costTotal{46}	=1.1240965363492196E-5;
% SimulationKwadraticCostFunction: evaluation 46 : cost = 1.124E-5
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
evaluatedParameters{47}	=[8.998978016486658,1.7135464737732413];
% ========================================================================
% evaluate no. 47
predicted{47}	=[-0.053213645927415745,-0.6273656131397054,0.1983302499927554,0.45311421305697663,-0.271275955935243,-0.2954818952438181,0.2905582552129303,0.1644767954948188,-0.2734805832006282,-0.0638176422503408];
observed{47}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{47}	=[0.010079206743671097,-0.003556557141640493,-0.0223783207875701,0.012651743901107682,0.025989556318503415,-0.022496883282756286,-0.022877601465238817,0.030143851871588995,0.01551704022361422,-0.03420345553396363];
obs_stdev{47}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{47}	=0.2399676764794166;
costTotal{47}	=0.2399676764794166;
% SimulationKwadraticCostFunction: evaluation 47 : cost = 0.24
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
evaluatedParameters{48}	=[8.998978016486658,1.67832687616301];
% ========================================================================
% evaluate no. 48
predicted{48}	=[-0.026960335949417435,-0.6355723959538025,0.13949686385900115,0.4830349407886325,-0.2013085940505339,-0.3494284294414494,0.2260160968406855,0.23786370715825333,-0.2251027145546983,-0.14870133110144534];
observed{48}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{48}	=[-0.016174103234327213,0.004650225672456654,0.036455065346184146,-0.0172689838305482,-0.0439778055662057,0.031449650914875005,0.04166455690700602,-0.043243059791845534,-0.03286082842231566,0.050680233317140916];
obs_stdev{48}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{48}	=0.6043880673679607;
costTotal{48}	=0.6043880673679607;
% SimulationKwadraticCostFunction: evaluation 48 : cost = 0.604
% ---bracket 1.0,0.0,-1.618034
% --- f values 0.2399676764794166,1.1240965363492196E-5,0.6043880673679607
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
evaluatedParameters{49}	=[8.998978016486658,1.7000937846140358];
% ========================================================================
% evaluate no. 49
predicted{49}	=[-0.04319787746560459,-0.6308847620217319,0.17609603869443852,0.4656623239622699,-0.24545071468165203,-0.3178096511633733,0.2678210290434503,0.1944037927778542,-0.25805353595237707,-0.09778140048838414];
observed{49}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{49}	=[6.34382818599441E-5,-3.7408259613957995E-5,-1.4410948925322464E-4,1.0363299581439556E-4,1.6431506491243475E-4,-1.6912736320112698E-4,-1.4037529575877672E-4,2.1685458855361106E-4,8.99929753631068E-5,-2.3969729592028532E-4];
obs_stdev{49}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{49}	=1.1240965363492196E-5;
costTotal{49}	=1.1240965363492196E-5;
% SimulationKwadraticCostFunction: evaluation 49 : cost = 1.124E-5
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
evaluatedParameters{50}	=[8.998978016486658,1.6917795656606311];
% ========================================================================
% evaluate no. 50
predicted{50}	=[-0.03700001562007528,-0.6328225925002411,0.16220113940917175,0.4727314295918498,-0.22891321413970947,-0.33056764039452013,0.25254781934352255,0.2117832234129701,-0.24658745490485365,-0.11791914284527383];
observed{50}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{50}	=[-0.006134423563669368,0.0019004222188951925,0.013750789796013546,-0.006965472633765513,-0.016373185477030122,0.012588861867945722,0.01513283440416896,-0.017162576046562295,-0.011376088072160312,0.0198980450609694];
obs_stdev{50}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{50}	=0.08771548513109287;
costTotal{50}	=0.08771548513109287;
% SimulationKwadraticCostFunction: evaluation 50 : cost = 0.088
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
evaluatedParameters{51}	=[8.998978016486658,1.705232254481421];
% ========================================================================
% evaluate no. 51
predicted{51}	=[-0.04702546617226,-0.6295964258964308,0.18462642279502806,0.4610298835005231,-0.25545592531718864,-0.3095213406086221,0.27680235701515715,0.1832227109960744,-0.26441180286243854,-0.08498562562483421];
observed{51}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{51}	=[0.0038910269885153517,-0.0013257443849150619,-0.008674493589842763,0.0047360734575612184,0.010169525700449045,-0.008457437917952337,-0.009121703267465642,0.0113979363703334,0.006448259885424579,-0.013035472159470221];
obs_stdev{51}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{51}	=0.03570722215577004;
costTotal{51}	=0.03570722215577004;
% SimulationKwadraticCostFunction: evaluation 51 : cost = 0.036
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
evaluatedParameters{52}	=[8.998978016486658,1.699992241234571];
% ========================================================================
% evaluate no. 52
predicted{52}	=[-0.043122216080487885,-0.6309095241443722,0.17592701540392205,0.4657518499705281,-0.24525130236100845,-0.3179703766360383,0.2676399551056682,0.1946214608651157,-0.25792220497324503,-0.09803175041877499];
observed{52}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{52}	=[-1.2223103256762957E-5,-1.2646136973715194E-5,2.4913801263237723E-5,1.4106987556217199E-5,-3.509725573114242E-5,-8.401890536136047E-6,4.069864202332907E-5,-8.134987078967093E-7,-4.1338003768931486E-5,1.0652634470559308E-5];
obs_stdev{52}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{52}	=2.955396584385814E-7;
costTotal{52}	=2.955396584385814E-7;
% SimulationKwadraticCostFunction: evaluation 52 : cost = 2.955E-7
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
evaluatedParameters{53}	=[8.998978016486658,1.700033812329338];
% ========================================================================
% evaluate no. 53
predicted{53}	=[-0.04315319138722774,-0.63089938998728,0.17599621434591922,0.46571520818237166,-0.24533294809958126,-0.31790459131779997,0.26771410204565194,0.19453236488860476,-0.25797599729039644,-0.09792927135747641];
observed{53}	=[-0.04313443918374465,-0.6309221702813459,0.1759519292051853,0.4657659569580843,-0.2452863996167396,-0.3179787785265744,0.2676806537476915,0.1946206473664078,-0.25796354297701396,-0.09802109778430443];
residuals{53}	=[1.875220348309531E-5,-2.2780294065882778E-5,-4.428514073392953E-5,5.074877571265013E-5,4.654848284166291E-5,-7.418720877444551E-5,-3.344829796042559E-5,8.828247780304221E-5,1.2454313382481708E-5,-9.182642682802056E-5];
obs_stdev{53}	=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1];
costObserved{53}	=1.5288744584642498E-6;
costTotal{53}	=1.5288744584642498E-6;
% SimulationKwadraticCostFunction: evaluation 53 : cost = 1.529E-6
% ---linesearch 0.0,-0.007548184475475821,-0.004458014601254822
% --- f values 1.1240965363492196E-5,2.955396584385814E-7,1.5288744584642498E-6
p_dir{6}	=[8.998978016486658,1.699992241234571];
f_p_dir{6}	=2.955396584385814E-7;
% End point of this outer loop has cost 2.955396584385814E-7 at [8.998978016486658,1.699992241234571]
% Relative convergence check for outer loop: 1.998351414926814<0.01
% Absolute convergence check for outer loop: 7.164836127823799E-4<0.01
% ===================================================================
% SimulationKwadraticCostfunction: optimal results
%     number of evaluations: 53
%     all cost values:
%         [17.756,17.756,18.651,17.386,17.307,17.307,17.362,17.308,17.304,17.304,...,8.054E-4,1.124E-5,1.124E-5,0.24,0.604,1.124E-5,0.088,0.036,2.955E-7,1.529E-6]
%     all parameter values
%         [8,8,9,6.382,6.741,6.741,7.222,6.925,6.825,6.825,6.825,6.825,6.825,6.825,6.825,6.825,6.825,6.825,5.65,6.825,8,9.901,8.98,8.98,8.606,9.332,8.936,8.916,8.936,8.936,8.936,8.936,8.936,8.936,8.936,8.936,11.047,8.936,11.047,5.52,8.936,7.631,9.742,9.066,8.999,8.999,8.999,8.999,8.999,8.999,8.999,8.999,8.999;1.571,1.571,1.571,1.571,1.571,1.571,1.571,1.571,1.571,1.571,1.696,1.9,1.696,1.774,1.648,1.705,1.707,1.704,1.84,1.705,1.705,1.705,1.705,1.705,1.705,1.705,1.705,1.705,1.705,1.84,1.488,1.705,1.622,1.757,1.699,1.7,1.695,1.7,1.7,1.7,1.7,1.7,1.7,1.7,1.7,1.7,1.714,1.678,1.7,1.692,1.705,1.7,1.7]
%     number of observations: 10
%     best cost:
%         cost = 2.955E-7
%     best parameters:
%                 [8.999,1.7]
% ===================================================================
% Application Done
