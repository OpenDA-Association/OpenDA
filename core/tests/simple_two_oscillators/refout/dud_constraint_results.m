% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./groupStochObserverForCalibration.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./groupStochModel.xml
% Starting Algorithm: 
%	className: org.openda.algorithms.Dud
%	dir.: /home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./algorithm
%	config.: dudAlgorithm_withConstraint.xml
%  Algorithm  configstring = dudAlgorithm_withConstraint.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./algorithm/dudAlgorithm_withConstraint.xml
%  Algorithm  Dud initialized
% Application initializing finished
% Initializing application
%  Algorithm  Dud preparation
%  Algorithm  Retrieving initial parameters from model
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model1/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model2/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
%  Algorithm  Starting optimizer
%  Algorithm  costFunction@class=org.openda.algorithms.SimulationKwadraticCostFunction
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model1/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model2/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
%  Algorithm  costFunction@weakParameterConstraint=true
%  Algorithm  costFunction@factor=0.5
% ========================================================================
% no1
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model1/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model2/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{1}	=[8.0,1.5708];
costWeakConstraintPenalty{1}	=0.0;
%  combined: oscillator1, oscillator2
predicted{1}	=[0.80.059874050823311174-0.622514741258897-0.052099759107992780.48435734708656860.04483119930100093-0.3768240910010292-0.038225585989916820.293135120402237550.03234578622602441-0.2280097266602124,0.80.059874050823311174-0.622514741258897-0.052099759107992780.48435734708656860.04483119930100093-0.3768240910010292-0.038225585989916820.293135120402237550.03234578622602441-0.2280097266602124];
%  combined: oscillator1, oscillator2
observed{1}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{1}	=[0.0-0.10300849000705582-0.0084074290224488420.22805168831317807-0.018591390128484264-0.29011759891774050.058845312474454790.30590623973760833-0.09851447303582975-0.290309329203038370.12998862887590798,0.0-0.17338851662001990.035343145939906620.37509848337793017-0.14862058630102226-0.433498148158251440.265896072968163530.3894759465761312-0.34501651706073294-0.28934002911712360.37111766634875387];
%  combined: oscillator1, oscillator2
obs_stdev{1}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{1}	=65.00104173743813;
costTotal{1}	=65.00104173743813;
% SimulationKwadraticCostFunction: evaluation 1 : cost = 65.001
% Evaluating with parameters  [9,1.571]
% ========================================================================
% no2
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model1/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model2/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{2}	=[9.0,1.5708];
costWeakConstraintPenalty{2}	=0.5;
%  combined: oscillator1, oscillator2
predicted{2}	=[0.80.0535786279459891-0.6402131222275624-0.047407003522354540.51230918018727150.041565211398558684-0.4099327838549162-0.036167141830807830.327994131939141450.03126817887015455-0.26241723932575955,0.80.0535786279459891-0.6402131222275624-0.047407003522354540.51230918018727150.041565211398558684-0.4099327838549162-0.036167141830807830.327994131939141450.03126817887015455-0.26241723932575955];
%  combined: oscillator1, oscillator2
observed{2}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{2}	=[0.0-0.096713067129733750.0092909519462165410.22335893272753982-0.046543223229187136-0.286851611015298270.09195400532834180.30384779557849934-0.13337348457273365-0.28923172184716850.16439614154145513,0.0-0.167093093742697820.0530415269085720.3704057277922919-0.17657241940172513-0.43023216025580920.299004765822050540.3874175024170222-0.37987552859763685-0.28826242176125370.405525179014301];
%  combined: oscillator1, oscillator2
obs_stdev{2}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{2}	=69.42932908650751;
costTotal{2}	=69.92932908650751;
% SimulationKwadraticCostFunction: evaluation 2 : cost = 69.929
% Evaluating with parameters  [8,1.696]
% ========================================================================
% no3
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model1/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model2/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{3}	=[8.0,1.6965];
costWeakConstraintPenalty{3}	=0.49999999999999933;
%  combined: oscillator1, oscillator2
predicted{3}	=[0.8-0.03350311927911278-0.61589789360598120.157404219702119820.4461027450327285-0.21769751450176725-0.301011315850664430.233719913185970320.1845979073251928-0.22137835983632012-0.09656628411831196,0.8-0.03350311927911278-0.61589789360598120.157404219702119820.4461027450327285-0.21769751450176725-0.301011315850664430.233719913185970320.1845979073251928-0.22137835983632012-0.09656628411831196];
%  combined: oscillator1, oscillator2
observed{3}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{3}	=[0.0-0.009631319904631869-0.0150242766753646560.0185477095030654740.019663211925355806-0.027588885114972345-0.0169674626759099830.033960740561721190.010022740041215006-0.03658518314069384-0.0014548136659924688,0.0-0.080011346517595950.0287262982869908030.16559450456781755-0.11036598424718219-0.170969434355483260.190083297817798760.11753044740024407-0.23647930398368816-0.035615883054779060.23967422380685344];
%  combined: oscillator1, oscillator2
obs_stdev{3}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{3}	=12.266603855137104;
costTotal{3}	=12.766603855137104;
% SimulationKwadraticCostFunction: evaluation 3 : cost = 12.767
%  Algorithm  outerLoop@maxIterations=10
%  Algorithm  outerLoop@absTolerance=0.01
%  Algorithm  outerLoop@relTolerance=0.01
%  Algorithm  lineSearch@maxIterations=5
%  Algorithm  lineSearch@maxRelStepSize=10.0
%  Algorithm  lineSearch/backtracking@startIterationNegativeLook=3
%  Algorithm  lineSearch/backtracking@shorteningFactor=0.5
% Application initialized
% Application starting next step
% ======================================================
% DUD outer iteration no.1
% ======================================================
% -----------------------------------------------------
costs{1}	=[12.766603855137104,65.00104173743813,69.92932908650751];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [7.165,1.74]
%   delta_p=    [-0.835,0.043]
% ========================================================================
% no4
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model1/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model2/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{4}	=[7.165347664393082,1.739609139020931];
costWeakConstraintPenalty{4}	=1.2500830736857522;
%  combined: oscillator1, oscillator2
predicted{4}	=[0.8-0.05779679261672458-0.58882364710476940.21014138822494350.3860219429602016-0.26806377090508365-0.216242078056723440.26389366252276750.08899093579750332-0.22477379859633603-0.0037885486977344606,0.8-0.05779679261672458-0.58882364710476940.21014138822494350.3860219429602016-0.26806377090508365-0.216242078056723440.26389366252276750.08899093579750332-0.22477379859633603-0.0037885486977344606];
%  combined: oscillator1, oscillator2
observed{4}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{4}	=[0.00.014662353432979933-0.042098523176576474-0.03418945901975820.079744013997882720.02277737128834406-0.101736700469850980.00378699122492398570.10562971156890448-0.03318974438067793-0.09423254908656997,0.0-0.055717673179984140.00165205178577898510.11285733604499387-0.050285182174655274-0.120603177952166850.105314060023857750.08735669806344687-0.1408723324559987-0.0322204442947631450.14689648838627592];
%  combined: oscillator1, oscillator2
obs_stdev{4}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{4}	=6.781972889338199;
costTotal{4}	=8.032055963023952;
% SimulationKwadraticCostFunction: evaluation 4 : cost = 8.032
% Error estimate for this outer iteration
parameterErrorEstimateStd{1}	=[0.7287909058495994,0.016412886174165135];
parameterErrorCorrelations{1}	=[1.0,0.16199221517758197;0.1619922151775808,0.9999999999999998];
% Application starting next step
% ======================================================
% DUD outer iteration no.2
% ======================================================
% -----------------------------------------------------
costs{2}	=[8.032055963023952,12.766603855137104,65.00104173743813];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [7.369,1.754]
%   delta_p=    [0.203,0.014]
% ========================================================================
% no5
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model1/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model2/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{5}	=[7.368760808732959,1.7535412132601498];
costWeakConstraintPenalty{5}	=1.2559819459005983;
%  combined: oscillator1, oscillator2
predicted{5}	=[0.8-0.06995279672134919-0.58827338504521780.234688939328845020.3760863085595846-0.2948502393677531-0.195788514837909430.285125596111375330.06134565959789892-0.236263940481513830.026076099001424588,0.8-0.06995279672134919-0.58827338504521780.234688939328845020.3760863085595846-0.2948502393677531-0.195788514837909430.285125596111375330.06134565959789892-0.236263940481513830.026076099001424588];
%  combined: oscillator1, oscillator2
observed{5}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{5}	=[0.00.02681835753760454-0.04264878523612814-0.058737010123659730.089679648398499730.04956383975101353-0.12219026368866498-0.0174449423636838240.13327498776850888-0.021699602495500125-0.12409719678572902,0.0-0.0435616690753595350.00110178972622732020.08830978494109235-0.040349547774038264-0.093816709489497390.084860496805043750.06612476447483906-0.11322705625639429-0.0207303024095853440.11703184068711688];
%  combined: oscillator1, oscillator2
obs_stdev{5}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{5}	=6.20013264486412;
costTotal{5}	=7.456114590764718;
% SimulationKwadraticCostFunction: evaluation 5 : cost = 7.456
% Error estimate for this outer iteration
parameterErrorEstimateStd{2}	=[0.3773432827687703,0.017454782295489944];
parameterErrorCorrelations{2}	=[1.0,-0.37285585076152117;-0.3728558507615216,0.9999999999999998];
% Application starting next step
% ======================================================
% DUD outer iteration no.3
% ======================================================
% -----------------------------------------------------
costs{3}	=[7.456114590764718,8.032055963023952,12.766603855137104];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [8.179,1.75]
%   delta_p=    [0.81,-3.879E-3]
% ========================================================================
% no6
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model1/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model2/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{6}	=[8.179174501945141,1.7496624990157967];
costWeakConstraintPenalty{6}	=1.0284188850731564;
%  combined: oscillator1, oscillator2
predicted{6}	=[0.8-0.07413422795770415-0.6036716602657050.243636774386450370.3978219013110443-0.31308708046357836-0.215274710413994740.31135247641473050.07285928422654785-0.266212052614264670.02478579670909281,0.8-0.07413422795770415-0.6036716602657050.243636774386450370.3978219013110443-0.31308708046357836-0.215274710413994740.31135247641473050.07285928422654785-0.266212052614264670.02478579670909281];
%  combined: oscillator1, oscillator2
observed{6}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{6}	=[0.00.0309997887739595-0.02725051001564094-0.067684845181265080.067944055647040040.06780068084683877-0.10270406811257968-0.0436718226670390060.121761363139859950.008248509637250712-0.12280689449339724,0.0-0.039380237839004570.016500064946714520.07936194988348699-0.06208514052549796-0.075579868393672140.104346692381129050.03989788417148388-0.124740680885043230.0092178097231654930.11832214297944865];
%  combined: oscillator1, oscillator2
obs_stdev{6}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{6}	=5.8871280455456585;
costTotal{6}	=6.915546930618815;
% SimulationKwadraticCostFunction: evaluation 6 : cost = 6.916
% Error estimate for this outer iteration
parameterErrorEstimateStd{3}	=[0.8016914591777942,0.015791264834660364];
parameterErrorCorrelations{3}	=[1.0,-0.10313754639979236;-0.10313754639979329,0.9999999999999998];
% Application starting next step
% ======================================================
% DUD outer iteration no.4
% ======================================================
% -----------------------------------------------------
costs{4}	=[6.915546930618815,7.456114590764718,8.032055963023952];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [8.106,1.747]
%   delta_p=    [-0.073,-2.257E-3]
% ========================================================================
% no7
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model1/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
% configstring = OscillatorStochModel.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./model2/OscillatorStochModel.xml
% simulationTimespan =[0.0,0.05,10.0]
% parameters@names =t_damp,omega parameters=[8.0,1.5708]
% parameterUncertainty@names =t_damp,omega parameterUncertainty=[1.0,0.1257]
% systemNoise = {[0.0,0.0],[0.3,0.3]}
% initialState = [0.8,0.0]
% initialStateUncertainty = [0.8,0.8]
evaluatedParameters{7}	=[8.1062852207876,1.7474058475922765];
costWeakConstraintPenalty{7}	=0.9926311272621706;
%  combined: oscillator1, oscillator2
predicted{7}	=[0.8-0.07189819526970391-0.60330040870848560.238986264658489270.3989776803138949-0.3076293922286494-0.218528674514047140.30658581729517530.07784959127215389-0.26314278374740920.018907340865417663,0.8-0.07189819526970391-0.60330040870848560.238986264658489270.3989776803138949-0.3076293922286494-0.218528674514047140.30658581729517530.07784959127215389-0.26314278374740920.018907340865417663];
%  combined: oscillator1, oscillator2
observed{7}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{7}	=[0.00.028763756085959262-0.027621761572860337-0.063034335453303970.06678827664418940.06234299261190979-0.09945010401252727-0.03890516354748380.116771056094253910.0051792407703952215-0.11692843864972209,0.0-0.041616270527004810.0161288133894951220.0840124596114481-0.0632409195283486-0.081037556628601130.107600656481181460.04466454329103908-0.129730987930649270.0061485408563100030.1242005988231238];
%  combined: oscillator1, oscillator2
obs_stdev{7}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{7}	=5.906632745670016;
costTotal{7}	=6.8992638729321865;
% SimulationKwadraticCostFunction: evaluation 7 : cost = 6.899
% Error estimate for this outer iteration
parameterErrorEstimateStd{4}	=[0.7096083432611837,0.016606154755428898];
parameterErrorCorrelations{4}	=[1.0,-0.17596761875198227;-0.1759676187519823,1.0000000000000002];
% ===================================================================
% SimulationKwadraticCostfunction: optimal results
%     number of evaluations: 7
%     all cost values:
%         [65.001,69.929,12.767,8.032,7.456,6.916,6.899]
%     all parameter values
%         [8,9,8,7.165,7.369,8.179,8.106;1.571,1.571,1.696,1.74,1.754,1.75,1.747]
%     number of observations: 22
%     best cost:
%         cost = 6.899
%     best parameters:
%                 [8.106,1.747]
% ===================================================================
% Application Done
