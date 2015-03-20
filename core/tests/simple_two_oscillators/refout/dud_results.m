% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./groupStochObserverForCalibration.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./groupStochModel.xml
% Starting Algorithm: 
%	className: org.openda.algorithms.Dud
%	dir.: /home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./algorithm
%	config.: dudAlgorithm.xml
%  Algorithm  configstring = dudAlgorithm.xml
% opening :/home/verlaanm/tudelft/svn_costa/trunk/openda/tests/two_oscillators/./algorithm/dudAlgorithm.xml
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
%  Algorithm  costFunction@weakParameterConstraint=false
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
%  combined: oscillator1, oscillator2
predicted{2}	=[0.80.0535786279459891-0.6402131222275624-0.047407003522354540.51230918018727150.041565211398558684-0.4099327838549162-0.036167141830807830.327994131939141450.03126817887015455-0.26241723932575955,0.80.0535786279459891-0.6402131222275624-0.047407003522354540.51230918018727150.041565211398558684-0.4099327838549162-0.036167141830807830.327994131939141450.03126817887015455-0.26241723932575955];
%  combined: oscillator1, oscillator2
observed{2}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{2}	=[0.0-0.096713067129733750.0092909519462165410.22335893272753982-0.046543223229187136-0.286851611015298270.09195400532834180.30384779557849934-0.13337348457273365-0.28923172184716850.16439614154145513,0.0-0.167093093742697820.0530415269085720.3704057277922919-0.17657241940172513-0.43023216025580920.299004765822050540.3874175024170222-0.37987552859763685-0.28826242176125370.405525179014301];
%  combined: oscillator1, oscillator2
obs_stdev{2}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{2}	=69.42932908650751;
costTotal{2}	=69.42932908650751;
% SimulationKwadraticCostFunction: evaluation 2 : cost = 69.429
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
%  combined: oscillator1, oscillator2
predicted{3}	=[0.8-0.03350311927911278-0.61589789360598120.157404219702119820.4461027450327285-0.21769751450176725-0.301011315850664430.233719913185970320.1845979073251928-0.22137835983632012-0.09656628411831196,0.8-0.03350311927911278-0.61589789360598120.157404219702119820.4461027450327285-0.21769751450176725-0.301011315850664430.233719913185970320.1845979073251928-0.22137835983632012-0.09656628411831196];
%  combined: oscillator1, oscillator2
observed{3}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{3}	=[0.0-0.009631319904631869-0.0150242766753646560.0185477095030654740.019663211925355806-0.027588885114972345-0.0169674626759099830.033960740561721190.010022740041215006-0.03658518314069384-0.0014548136659924688,0.0-0.080011346517595950.0287262982869908030.16559450456781755-0.11036598424718219-0.170969434355483260.190083297817798760.11753044740024407-0.23647930398368816-0.035615883054779060.23967422380685344];
%  combined: oscillator1, oscillator2
obs_stdev{3}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{3}	=12.266603855137104;
costTotal{3}	=12.266603855137104;
% SimulationKwadraticCostFunction: evaluation 3 : cost = 12.267
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
costs{1}	=[12.266603855137104,65.00104173743813,69.42932908650751];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [6.264,1.739]
%   delta_p=    [-1.736,0.043]
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
evaluatedParameters{4}	=[6.263864303871257,1.7391146633462222];
%  combined: oscillator1, oscillator2
predicted{4}	=[0.8-0.04724978889072543-0.5684196980501890.18966133494393120.3612231894982103-0.23652806963841946-0.197854122489748730.22594213580330450.082032420500798-0.1866002300348009-0.008619546158332275,0.8-0.04724978889072543-0.5684196980501890.18966133494393120.3612231894982103-0.23652806963841946-0.197854122489748730.22594213580330450.082032420500798-0.1866002300348009-0.008619546158332275];
%  combined: oscillator1, oscillator2
observed{4}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{4}	=[0.00.004115349706980784-0.0625024722311569-0.0137094057387459060.10454276745987401-0.008758329978320128-0.120124656036825680.0417385179443870160.1125882268656098-0.07136331294221307-0.08940155162597216,0.0-0.06626467690598328-0.0187518972688014430.13333738932600617-0.02548642871266399-0.152138879218831040.086926104456883050.1253082247829099-0.13391381715929337-0.070394012856298290.15172748584687373];
%  combined: oscillator1, oscillator2
obs_stdev{4}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{4}	=8.626795020775754;
costTotal{4}	=8.626795020775754;
% SimulationKwadraticCostFunction: evaluation 4 : cost = 8.627
% Error estimate for this outer iteration
parameterErrorEstimateStd{1}	=[1.0648547900992305,0.016803200730115844];
parameterErrorCorrelations{1}	=[1.0000000000000002,0.23509589433394887;0.23509589433395198,0.9999999999999999];
% Application starting next step
% ======================================================
% DUD outer iteration no.2
% ======================================================
% -----------------------------------------------------
costs{2}	=[8.626795020775754,12.266603855137104,65.00104173743813];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [6.834,1.764]
%   delta_p=    [0.571,0.025]
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
evaluatedParameters{5}	=[6.834484291334119,1.763872454094438];
%  combined: oscillator1, oscillator2
predicted{5}	=[0.8-0.0719584103872133-0.57392325693492210.238050121510391680.35185336987481236-0.29067341360737475-0.16922057112465710.2712831308697830.0391507363915111-0.215032971667650.03985194100946289,0.8-0.0719584103872133-0.57392325693492210.238050121510391680.35185336987481236-0.29067341360737475-0.16922057112465710.2712831308697830.0391507363915111-0.215032971667650.03985194100946289];
%  combined: oscillator1, oscillator2
observed{5}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{5}	=[0.00.028823971203468655-0.05699891334642382-0.0620981923052063840.113912587083271950.04538701399063516-0.1487582074019173-0.00360247712209149640.1554699109748967-0.04293057130936395-0.13787303879376733,0.0-0.04155605540949542-0.0132483383840683590.08494860275954569-0.016116609089266043-0.097993535249875750.058292553091791420.07996722971643139-0.09103213305000647-0.041961271223449170.10325599867907857];
%  combined: oscillator1, oscillator2
obs_stdev{5}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{5}	=6.9810085649498435;
costTotal{5}	=6.9810085649498435;
% SimulationKwadraticCostFunction: evaluation 5 : cost = 6.981
% Error estimate for this outer iteration
parameterErrorEstimateStd{2}	=[0.6399175392063647,0.019180277682362346];
parameterErrorCorrelations{2}	=[1.0,-0.5243345190576945;-0.5243345190576946,0.9999999999999998];
% Application starting next step
% ======================================================
% DUD outer iteration no.3
% ======================================================
% -----------------------------------------------------
costs{3}	=[6.9810085649498435,8.626795020775754,12.266603855137104];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [8.338,1.755]
%   delta_p=    [1.503,-9.114E-3]
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
evaluatedParameters{6}	=[8.337598561707939,1.7547586842948522];
%  combined: oscillator1, oscillator2
predicted{6}	=[0.8-0.07910688493370645-0.60427596317829870.25399709282317760.394795114588529-0.325110625237011-0.207424070115424690.32159682721269170.06112969411757658-0.272407181836256840.038353813078841545,0.8-0.07910688493370645-0.60427596317829870.25399709282317760.394795114588529-0.325110625237011-0.207424070115424690.32159682721269170.06112969411757658-0.272407181836256840.038353813078841545];
%  combined: oscillator1, oscillator2
observed{6}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{6}	=[0.00.0359724457499618-0.026646207103047215-0.078045163617992320.070970842369555310.07982422562027142-0.11055470841114973-0.053916173465000220.133490953248831230.01444363885924288-0.13637491086314596,0.0-0.0344075808630022740.0171043678593082450.06900163144675975-0.05905835380298269-0.06355632362023950.0964960520825590.029653533373522667-0.113011090776071950.0154129389451576610.10475412660969992];
%  combined: oscillator1, oscillator2
obs_stdev{6}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{6}	=5.959899661339367;
costTotal{6}	=5.959899661339367;
% SimulationKwadraticCostFunction: evaluation 6 : cost = 5.96
% Error estimate for this outer iteration
parameterErrorEstimateStd{3}	=[1.0369990931755295,0.016742476821840324];
parameterErrorCorrelations{3}	=[0.9999999999999998,-0.14131261994579886;-0.14131261994579747,0.9999999999999998];
% Application starting next step
% ======================================================
% DUD outer iteration no.4
% ======================================================
% -----------------------------------------------------
costs{4}	=[5.959899661339367,6.9810085649498435,8.626795020775754];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [8.219,1.749]
%   delta_p=    [-0.119,-5.622E-3]
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
evaluatedParameters{7}	=[8.219085124319166,1.7491365716077936];
%  combined: oscillator1, oscillator2
predicted{7}	=[0.8-0.07405899153891332-0.60448828671044610.243496102097981030.39922883024748473-0.3133706806265097-0.216868931380443830.312212087322263470.07425266043246186-0.26755467693509760.02386054821800919,0.8-0.07405899153891332-0.60448828671044610.243496102097981030.39922883024748473-0.3133706806265097-0.216868931380443830.312212087322263470.07425266043246186-0.26755467693509760.02386054821800919];
%  combined: oscillator1, oscillator2
observed{7}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{7}	=[0.00.030924552355168673-0.02643388357089982-0.067544172892795740.066537126710599580.0680842810097701-0.10110984714613058-0.0445314335745719550.120367986933945940.009591133958083642-0.12188164600231362,0.0-0.03945547425779540.017316691391455640.07950262217195633-0.06349206946193842-0.075296268230740810.105940913347578140.03903827326395093-0.126134057090957240.0105604340439984230.11924739147053227];
%  combined: oscillator1, oscillator2
obs_stdev{7}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{7}	=5.889427349199533;
costTotal{7}	=5.889427349199533;
% SimulationKwadraticCostFunction: evaluation 7 : cost = 5.889
% Error estimate for this outer iteration
parameterErrorEstimateStd{4}	=[0.9693697588089613,0.018857311951084057];
parameterErrorCorrelations{4}	=[1.0,-0.2972478593101752;-0.29724785931017506,1.0000000000000002];
% Application starting next step
% ======================================================
% DUD outer iteration no.5
% ======================================================
% -----------------------------------------------------
costs{5}	=[5.889427349199533,5.959899661339367,6.9810085649498435];
% -----------------------------------------------------
% Start search until improvement,
% Next try p=   [8.174,1.75]
%   delta_p=    [-0.045,1.035E-3]
% ========================================================================
% no8
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
evaluatedParameters{8}	=[8.173925893399222,1.7501715882114377];
%  combined: oscillator1, oscillator2
predicted{8}	=[0.8-0.0744674514236295-0.60340714671158850.24432354681248650.39712039528543325-0.31371897480314537-0.214213859294418160.311665889400924770.07163919081158439-0.266105373043089340.02594492981281916,0.8-0.0744674514236295-0.60340714671158850.24432354681248650.39712039528543325-0.31371897480314537-0.214213859294418160.311665889400924770.07163919081158439-0.266105373043089340.02594492981281916];
%  combined: oscillator1, oscillator2
observed{8}	=[0.8-0.04313443918374465-0.63092217028134590.17595192920518530.4657659569580843-0.2452863996167396-0.31797877852657440.26768065374769150.1946206473664078-0.25796354297701396-0.09802109778430443,0.8-0.11351446579670872-0.58717159531899040.322998724269937370.3357367607855463-0.3886669488572505-0.110928018032865690.3512503605862144-0.05188139665849537-0.25699424289109920.14310793968854146];
%  combined: oscillator1, oscillator2
residuals{8}	=[0.00.03133301223988485-0.027515023569757435-0.06837161760730120.068645561672651070.06843257518640578-0.10376491923215625-0.043985235653233260.122981456554823410.008141830066075384-0.1239660275971236,0.0-0.0390470143730792250.0162355513925980240.07867517745745087-0.06138363449988693-0.074947974054105140.103285841261552480.03958447118528963-0.123520587470079770.0091111301519901660.1171630098757223];
%  combined: oscillator1, oscillator2
obs_stdev{8}	=[0.10.10.10.10.10.10.10.10.10.10.1,0.10.10.10.10.10.10.10.10.10.10.1];
costObserved{8}	=5.886725805147115;
costTotal{8}	=5.886725805147115;
% SimulationKwadraticCostFunction: evaluation 8 : cost = 5.887
% Error estimate for this outer iteration
parameterErrorEstimateStd{5}	=[0.9790588006497586,0.015630348113236412];
parameterErrorCorrelations{5}	=[1.0000000000000002,-0.2414496295166824;-0.24144962951668242,1.0];
% ===================================================================
% SimulationKwadraticCostfunction: optimal results
%     number of evaluations: 8
%     all cost values:
%         [65.001,69.429,12.267,8.627,6.981,5.96,5.889,5.887]
%     all parameter values
%         [8,9,8,6.264,6.834,8.338,8.219,8.174;1.571,1.571,1.696,1.739,1.764,1.755,1.749,1.75]
%     number of observations: 22
%     best cost:
%         cost = 5.887
%     best parameters:
%                 [8.174,1.75]
% ===================================================================
% Application Done
