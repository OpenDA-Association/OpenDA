% OpenDA version (Development)
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF_states\.\stochObserver\noosObservations.xml
% NoosStochObserver[i]=TimeSeries(
%   Location = station1
%   Position = (5.0,11.0)
%   Height   = 20.0
%   Quantity = temperature
%   Unit     = m
%   Source   = observed
%   Id       = station1.temperature
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.1
%   status  = assimilation
%   Values   = 
%   (57174.125=201506010300,6.95)
%   (57174.25=201506010600,6.93)
%   (57174.375=201506010900,7.2)
%   (57174.5=201506011200,7.55)
%   (57174.625=201506011500,8.3)
%   ...
%   (57175.375=201506020900,8.18)
%   (57175.5=201506021200,8.9)
%   (57175.625=201506021500,9.42)
%   (57175.75=201506021800,9.86)
%   (57175.875=201506022100,9.99)
%
%   Values.length()=15
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = station2
%   Position = (17.0,13.0)
%   Height   = 20.0
%   Quantity = temperature
%   Unit     = m
%   Source   = observed
%   Id       = station2.temperature
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.1
%   status  = assimilation
%   Values   = 
%   (57174.125=201506010300,6.96)
%   (57174.25=201506010600,6.99)
%   (57174.375=201506010900,7.19)
%   (57174.5=201506011200,7.33)
%   (57174.625=201506011500,8.05)
%   ...
%   (57175.375=201506020900,8.08)
%   (57175.5=201506021200,8.76)
%   (57175.625=201506021500,9.8)
%   (57175.75=201506021800,10.25)
%   (57175.875=201506022100,10.18)
%
%   Values.length()=15
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = station3
%   Position = (19.0,7.0)
%   Height   = 20.0
%   Quantity = temperature
%   Unit     = m
%   Source   = observed
%   Id       = station3.temperature
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.1
%   status  = assimilation
%   Values   = 
%   (57174.125=201506010300,6.95)
%   (57174.25=201506010600,6.96)
%   (57174.375=201506010900,7.2)
%   (57174.5=201506011200,7.44)
%   (57174.625=201506011500,8.21)
%   ...
%   (57175.375=201506020900,8.2)
%   (57175.5=201506021200,8.82)
%   (57175.625=201506021500,9.44)
%   (57175.75=201506021800,9.89)
%   (57175.875=201506022100,10.11)
%
%   Values.length()=15
%)
% NoosStochObserver[i]=TimeSeries(
%   Location = station4
%   Position = (27.0,10.0)
%   Height   = 20.0
%   Quantity = temperature
%   Unit     = m
%   Source   = observed
%   Id       = station4.temperature
%   relativestandarddeviation  = 0.0
%   timezone  = GMT
%   analtime  = 
%   standarddeviation  = 0.1
%   status  = assimilation
%   Values   = 
%   (57174.125=201506010300,6.96)
%   (57174.25=201506010600,6.97)
%   (57174.375=201506010900,7.2)
%   (57174.5=201506011200,7.36)
%   (57174.625=201506011500,8.02)
%   ...
%   (57175.375=201506020900,7.99)
%   (57175.5=201506021200,8.64)
%   (57175.625=201506021500,9.75)
%   (57175.75=201506021800,10.26)
%   (57175.875=201506022100,10.17)
%
%   Values.length()=15
%)
% Starting Algorithm: 
%	className: org.openda.algorithms.kalmanFilter.EnKF
%	dir.: C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF_states\.\algorithm
%	config.: EnkfAlgorithm.xml
% configstring = EnkfAlgorithm.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF_states\.\algorithm\EnkfAlgorithm.xml
% analysisTimes@type=fromObservationTimes
% mainModel@stochParameter=false
% mainModel@stochForcing=false
% mainModel@stochInit=false
% this.ensembleSize=4
% ensembleModel@stochParameter=false
% ensembleModel@stochForcing=true
% ensembleModel@stochInit=true
% saveGain/times@type=none
% Creating mainModel
% Create new BBModelInstance with number: 0
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF_states\.\stochModel\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Do not add noise to forcing
% model time 57174.0 57176.0
% analysisTimes acquired from OBSERVER:57174.125 -- 57175.875
% Creating ensemble model 0
% Create new BBModelInstance with number: 1
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF_states\.\stochModel\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 1
% Create new BBModelInstance with number: 2
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF_states\.\stochModel\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 2
% Create new BBModelInstance with number: 3
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF_states\.\stochModel\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Add noise to initial state
%    Add noise to forcing
% Creating ensemble model 3
% Create new BBModelInstance with number: 4
% Instance initialization done
% configstring = noiseModelConfig.xml
% opening :C:\Users\Theo\Documents\OpenDA\public\model_delft3d\tests\d3d_3Dlake_EnKF_states\.\stochModel\.\noiseModelConfig.xml
% analysisTimes=201506010000,201506010100,...,201506030000
%    Add noise to initial state
%    Add noise to forcing
% Application initializing finished
% Initializing Algorithm
% Algorithm initialized
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506010000UTC  to 201506010300UTC  (57174.0-->57174.125) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506010300UTC (57174.125) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{1}	=57174.125;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{1}	=[6.62406063079834, 6.744590759277344, 6.656722545623779, 6.750398635864258];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{1}	=[6.95,6.96,6.95,6.96];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{1}	=[6.644106388092041, 6.731283664703369, 6.648671627044678, 6.742994785308838];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{1}	=[6.576149940490723, 6.706127166748047, 6.682270050048828, 6.729118347167969];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{1}	=[6.669703960418701, 6.746575355529785, 6.688745021820068, 6.749942779541016];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{1}	=[6.600829124450684, 6.733613967895508, 6.68953800201416, 6.7563042640686035];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{1}	=[6.622697353363037, 6.729400038719177, 6.677306175231934, 6.7445900440216064];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{1}	=[0.04208265262465482, 0.016910778843495344, 0.01936527993414464, 0.011658944817059836];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{1}	=[6.682144696890719, 6.755706934820853, 6.683863204781241, 6.758347513434569];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{1}	=[6.644106388092041, 6.731283664703369, 6.648671627044678, 6.742994785308838];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{1}	=[6.576149940490723, 6.706127166748047, 6.682270050048828, 6.729118347167969];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{1}	=[6.669703960418701, 6.746575355529785, 6.688745021820068, 6.749942779541016];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{1}	=[6.600829124450684, 6.733613967895508, 6.68953800201416, 6.7563042640686035];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{1}	=[6.622697353363037, 6.729400038719177, 6.677306175231934, 6.7445900440216064];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{1}	=[6.62406063079834, 6.744590759277344, 6.656722545623779, 6.750398635864258];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506010300UTC  to 201506010600UTC  (57174.125-->57174.25) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506010600UTC (57174.25) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{2}	=57174.25;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{2}	=[6.560694694519043, 6.670781135559082, 6.574470520019531, 6.61823844909668];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{2}	=[6.93,6.99,6.96,6.97];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{2}	=[6.545876979827881, 6.615065097808838, 6.589465141296387, 6.651435852050781];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{2}	=[6.590059280395508, 6.668237686157227, 6.60324764251709, 6.554215431213379];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{2}	=[6.566421985626221, 6.574831962585449, 6.46993350982666, 6.508565902709961];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{2}	=[6.425731182098389, 6.659104824066162, 6.6045756340026855, 6.624804973602295];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{2}	=[6.5320223569869995, 6.629309892654419, 6.566805481910706, 6.584755539894104];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{2}	=[0.07312405751530685, 0.043104021813979106, 0.06494165226734935, 0.0652885396327731];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{2}	=[6.586259385779293, 6.725663390827153, 6.748313163094897, 6.703589548270319];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{2}	=[6.545876979827881, 6.615065097808838, 6.589465141296387, 6.651435852050781];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{2}	=[6.590059280395508, 6.668237686157227, 6.60324764251709, 6.554215431213379];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{2}	=[6.566421985626221, 6.574831962585449, 6.46993350982666, 6.508565902709961];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{2}	=[6.425731182098389, 6.659104824066162, 6.6045756340026855, 6.624804973602295];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{2}	=[6.5320223569869995, 6.629309892654419, 6.566805481910706, 6.584755539894104];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{2}	=[6.560694694519043, 6.670781135559082, 6.574470520019531, 6.61823844909668];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506010600UTC  to 201506010900UTC  (57174.25-->57174.375) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506010900UTC (57174.375) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{3}	=57174.375;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{3}	=[6.713649272918701, 6.704550266265869, 6.626068592071533, 6.538699626922607];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{3}	=[7.2,7.19,7.2,7.2];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{3}	=[6.5539231300354, 6.677882194519043, 6.607168197631836, 6.532244682312012];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{3}	=[6.57700777053833, 6.5890889167785645, 6.56602668762207, 6.465734481811523];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{3}	=[6.704379558563232, 6.669339656829834, 6.630151748657227, 6.451220512390137];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{3}	=[6.681463718414307, 6.722188472747803, 6.631892681121826, 6.554755687713623];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{3}	=[6.629193544387817, 6.664624810218811, 6.60880982875824, 6.500988841056824];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{3}	=[0.07477543734275215, 0.05542933777527757, 0.030666962334334472, 0.050291012437801624];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{3}	=[6.896210402239459, 6.966475783956383, 6.764738545839379, 6.684674252626855];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{3}	=[6.5539231300354, 6.677882194519043, 6.607168197631836, 6.532244682312012];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{3}	=[6.57700777053833, 6.5890889167785645, 6.56602668762207, 6.465734481811523];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{3}	=[6.704379558563232, 6.669339656829834, 6.630151748657227, 6.451220512390137];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{3}	=[6.681463718414307, 6.722188472747803, 6.631892681121826, 6.554755687713623];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{3}	=[6.629193544387817, 6.664624810218811, 6.60880982875824, 6.500988841056824];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{3}	=[6.713649272918701, 6.704550266265869, 6.626068592071533, 6.538699626922607];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506010900UTC  to 201506011200UTC  (57174.375-->57174.5) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506011200UTC (57174.5) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{4}	=57174.5;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{4}	=[6.868166923522949, 6.871669769287109, 6.812815189361572, 6.686974048614502];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{4}	=[7.55,7.33,7.44,7.36];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{4}	=[6.739607334136963, 6.896777629852295, 6.747331619262695, 6.613293647766113];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{4}	=[6.764366149902344, 6.656352519989014, 6.808712959289551, 6.719269275665283];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{4}	=[6.876726150512695, 6.9547438621521, 6.78443717956543, 6.656306266784668];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{4}	=[6.668955326080322, 6.718297004699707, 6.7265238761901855, 6.702944278717041];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{4}	=[6.762413740158081, 6.806542754173279, 6.766751408576965, 6.672953367233276];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{4}	=[0.08626653731782123, 0.1419510713114844, 0.036828163193410104, 0.047892331417307664];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{4}	=[7.219059186048828, 7.098991883871443, 6.927374313591194, 6.6848672288402735];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{4}	=[6.739607334136963, 6.896777629852295, 6.747331619262695, 6.613293647766113];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{4}	=[6.764366149902344, 6.656352519989014, 6.808712959289551, 6.719269275665283];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{4}	=[6.876726150512695, 6.9547438621521, 6.78443717956543, 6.656306266784668];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{4}	=[6.668955326080322, 6.718297004699707, 6.7265238761901855, 6.702944278717041];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{4}	=[6.762413740158081, 6.806542754173279, 6.766751408576965, 6.672953367233276];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{4}	=[6.868166923522949, 6.871669769287109, 6.812815189361572, 6.686974048614502];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506011200UTC  to 201506011500UTC  (57174.5-->57174.625) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506011500UTC (57174.625) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{5}	=57174.625;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{5}	=[6.706190586090088, 6.737608432769775, 6.766206741333008, 6.772286415100098];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{5}	=[8.3,8.05,8.21,8.02];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{5}	=[6.7128071784973145, 6.723069190979004, 6.765020847320557, 6.763858795166016];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{5}	=[6.648968696594238, 6.729798316955566, 6.728664875030518, 6.759241104125977];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{5}	=[6.618277549743652, 6.580410003662109, 6.638424396514893, 6.769397735595703];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{5}	=[6.600451946258545, 6.630083084106445, 6.650326251983643, 6.662591457366943];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{5}	=[6.6451263427734375, 6.665840148925781, 6.695609092712402, 6.73877227306366];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{5}	=[0.0493697565934609, 0.07289876065778263, 0.06118633171472466, 0.05095665567181687];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{5}	=[7.320115435009285, 7.5500965233292145, 7.539216030994185, 7.226668959765161];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{5}	=[6.7128071784973145, 6.723069190979004, 6.765020847320557, 6.763858795166016];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{5}	=[6.648968696594238, 6.729798316955566, 6.728664875030518, 6.759241104125977];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{5}	=[6.618277549743652, 6.580410003662109, 6.638424396514893, 6.769397735595703];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{5}	=[6.600451946258545, 6.630083084106445, 6.650326251983643, 6.662591457366943];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{5}	=[6.6451263427734375, 6.665840148925781, 6.695609092712402, 6.73877227306366];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{5}	=[6.706190586090088, 6.737608432769775, 6.766206741333008, 6.772286415100098];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506011500UTC  to 201506011800UTC  (57174.625-->57174.75) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506011800UTC (57174.75) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{6}	=57174.75;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{6}	=[6.440403938293457, 6.610820770263672, 6.597354888916016, 6.791781425476074];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{6}	=[8.53,8.05,8.33,7.87];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{6}	=[6.360527038574219, 6.567378997802734, 6.557651042938232, 6.739180088043213];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{6}	=[6.442577838897705, 6.599783420562744, 6.543205738067627, 6.7166218757629395];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{6}	=[6.32371711730957, 6.641506671905518, 6.646412372589111, 6.795774936676025];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{6}	=[6.388969421386719, 6.456817626953125, 6.512619972229004, 6.68716287612915];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{6}	=[6.378947854042053, 6.56637167930603, 6.564972281455994, 6.734684944152832];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{6}	=[0.05012972340932738, 0.07908796082891403, 0.05744747070057558, 0.0459594563863427];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{6}	=[6.430638985083752, 7.456338506527826, 6.99382542441617, 7.076975929555493];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{6}	=[6.360527038574219, 6.567378997802734, 6.557651042938232, 6.739180088043213];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{6}	=[6.442577838897705, 6.599783420562744, 6.543205738067627, 6.7166218757629395];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{6}	=[6.32371711730957, 6.641506671905518, 6.646412372589111, 6.795774936676025];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{6}	=[6.388969421386719, 6.456817626953125, 6.512619972229004, 6.68716287612915];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{6}	=[6.378947854042053, 6.56637167930603, 6.564972281455994, 6.734684944152832];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{6}	=[6.440403938293457, 6.610820770263672, 6.597354888916016, 6.791781425476074];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506011800UTC  to 201506012100UTC  (57174.75-->57174.875) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506012100UTC (57174.875) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{7}	=57174.875;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{7}	=[6.316381454467773, 6.454875469207764, 6.423218250274658, 6.394774436950684];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{7}	=[8.5,8.05,8.17,7.63];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{7}	=[6.219316482543945, 6.395519256591797, 6.387300491333008, 6.368045806884766];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{7}	=[6.303474426269531, 6.3767852783203125, 6.284534454345703, 6.2774858474731445];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{7}	=[6.193729877471924, 6.411294937133789, 6.343556880950928, 6.380300998687744];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{7}	=[6.280747413635254, 6.420949935913086, 6.370912551879883, 6.349973678588867];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{7}	=[6.249317049980164, 6.401137351989746, 6.34657609462738, 6.34395158290863];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{7}	=[0.051351264054533496, 0.01932443621928676, 0.045125825638634445, 0.04602814819889014];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{7}	=[6.3475029139873005, 6.5401446969061166, 6.671177594156833, 6.498299842247012];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{7}	=[6.219316482543945, 6.395519256591797, 6.387300491333008, 6.368045806884766];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{7}	=[6.303474426269531, 6.3767852783203125, 6.284534454345703, 6.2774858474731445];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{7}	=[6.193729877471924, 6.411294937133789, 6.343556880950928, 6.380300998687744];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{7}	=[6.280747413635254, 6.420949935913086, 6.370912551879883, 6.349973678588867];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{7}	=[6.249317049980164, 6.401137351989746, 6.34657609462738, 6.34395158290863];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{7}	=[6.316381454467773, 6.454875469207764, 6.423218250274658, 6.394774436950684];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506012100UTC  to 201506020000UTC  (57174.875-->57175.0) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506020000UTC (57175.0) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{8}	=57175.0;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{8}	=[6.368857383728027, 6.197770118713379, 5.9927778244018555, 6.312125205993652];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{8}	=[8.43,8.29,8.3,7.98];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{8}	=[6.295572757720947, 6.15514612197876, 5.990790367126465, 6.311618328094482];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{8}	=[6.355118274688721, 6.194265842437744, 5.920114040374756, 6.209247589111328];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{8}	=[6.275307655334473, 6.245678901672363, 6.074643611907959, 6.304698944091797];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{8}	=[6.271071434020996, 6.142599105834961, 5.850856304168701, 6.243230819702148];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{8}	=[6.299267530441284, 6.184422492980957, 5.95910108089447, 6.267198920249939];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{8}	=[0.03873866478048212, 0.04638800400146281, 0.09590135270473463, 0.049369903807906165];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{8}	=[6.410607024431871, 6.852956127896326, 7.5046768526251935, 6.687599320212396];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{8}	=[6.295572757720947, 6.15514612197876, 5.990790367126465, 6.311618328094482];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{8}	=[6.355118274688721, 6.194265842437744, 5.920114040374756, 6.209247589111328];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{8}	=[6.275307655334473, 6.245678901672363, 6.074643611907959, 6.304698944091797];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{8}	=[6.271071434020996, 6.142599105834961, 5.850856304168701, 6.243230819702148];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{8}	=[6.299267530441284, 6.184422492980957, 5.95910108089447, 6.267198920249939];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{8}	=[6.368857383728027, 6.197770118713379, 5.9927778244018555, 6.312125205993652];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506020000UTC  to 201506020300UTC  (57175.0-->57175.125) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506020300UTC (57175.125) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{9}	=57175.125;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{9}	=[6.2198805809021, 6.128903865814209, 5.85472297668457, 5.9341721534729];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{9}	=[8.21,8.06,8.18,7.98];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{9}	=[6.121194362640381, 6.088000297546387, 5.8597259521484375, 5.9250807762146];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{9}	=[6.216258525848389, 6.040276050567627, 5.7730021476745605, 5.853420734405518];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{9}	=[6.048851013183594, 6.119084358215332, 5.858932971954346, 5.8095879554748535];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{9}	=[6.116516590118408, 6.051171779632568, 5.79219388961792, 5.838963985443115];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{9}	=[6.125705122947693, 6.0746331214904785, 5.820963740348816, 5.8567633628845215];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{9}	=[0.0688264703405911, 0.035988131035339, 0.044989592482566856, 0.049060435623936256];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{9}	=[6.302112073054502, 6.187729905528365, 6.13728874457145, 6.482751446193561];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{9}	=[6.121194362640381, 6.088000297546387, 5.8597259521484375, 5.9250807762146];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{9}	=[6.216258525848389, 6.040276050567627, 5.7730021476745605, 5.853420734405518];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{9}	=[6.048851013183594, 6.119084358215332, 5.858932971954346, 5.8095879554748535];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{9}	=[6.116516590118408, 6.051171779632568, 5.79219388961792, 5.838963985443115];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{9}	=[6.125705122947693, 6.0746331214904785, 5.820963740348816, 5.8567633628845215];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{9}	=[6.2198805809021, 6.128903865814209, 5.85472297668457, 5.9341721534729];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506020300UTC  to 201506020600UTC  (57175.125-->57175.25) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506020600UTC (57175.25) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{10}	=57175.25;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{10}	=[5.7988152503967285, 5.8447041511535645, 5.714424133300781, 5.582211017608643];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{10}	=[7.94,7.75,8.0,7.79];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{10}	=[5.470909595489502, 5.776553153991699, 5.678986549377441, 5.606827735900879];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{10}	=[5.816405296325684, 5.789503574371338, 5.640009880065918, 5.507821083068848];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{10}	=[5.6347832679748535, 5.839200973510742, 5.674549579620361, 5.466554164886475];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{10}	=[5.813076496124268, 5.863156795501709, 5.671061038970947, 5.482316493988037];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{10}	=[5.683793663978577, 5.817103624343872, 5.666151762008667, 5.51587986946106];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{10}	=[0.16534967115608662, 0.04088720589689163, 0.017727136441455495, 0.06297084974545188];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{10}	=[6.882971885816376, 6.014418157674714, 5.629450483246109, 5.39012327828297];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{10}	=[5.470909595489502, 5.776553153991699, 5.678986549377441, 5.606827735900879];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{10}	=[5.816405296325684, 5.789503574371338, 5.640009880065918, 5.507821083068848];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{10}	=[5.6347832679748535, 5.839200973510742, 5.674549579620361, 5.466554164886475];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{10}	=[5.813076496124268, 5.863156795501709, 5.671061038970947, 5.482316493988037];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{10}	=[5.683793663978577, 5.817103624343872, 5.666151762008667, 5.51587986946106];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{10}	=[5.7988152503967285, 5.8447041511535645, 5.714424133300781, 5.582211017608643];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506020600UTC  to 201506020900UTC  (57175.25-->57175.375) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506020900UTC (57175.375) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{11}	=57175.375;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{11}	=[5.462090015411377, 5.748250961303711, 5.752294540405273, 5.721193790435791];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{11}	=[8.18,8.08,8.2,7.99];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{11}	=[5.461653232574463, 5.713937759399414, 5.655261516571045, 5.63296365737915];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{11}	=[5.367989540100098, 5.681256294250488, 5.717684268951416, 5.602348804473877];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{11}	=[5.490013122558594, 5.681478023529053, 5.754071235656738, 5.693285942077637];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{11}	=[5.500175952911377, 5.691065788269043, 5.706366539001465, 5.697528839111328];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{11}	=[5.454957962036133, 5.6919344663619995, 5.708345890045166, 5.656531810760498];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{11}	=[0.06022697081229427, 0.015365112674294088, 0.04082434359377828, 0.04662931312146537];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{11}	=[6.5116332640210155, 5.64467164185899, 6.056650882754174, 6.538456309634184];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{11}	=[5.461653232574463, 5.713937759399414, 5.655261516571045, 5.63296365737915];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{11}	=[5.367989540100098, 5.681256294250488, 5.717684268951416, 5.602348804473877];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{11}	=[5.490013122558594, 5.681478023529053, 5.754071235656738, 5.693285942077637];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{11}	=[5.500175952911377, 5.691065788269043, 5.706366539001465, 5.697528839111328];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{11}	=[5.454957962036133, 5.6919344663619995, 5.708345890045166, 5.656531810760498];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{11}	=[5.462090015411377, 5.748250961303711, 5.752294540405273, 5.721193790435791];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506020900UTC  to 201506021200UTC  (57175.375-->57175.5) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506021200UTC (57175.5) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{12}	=57175.5;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{12}	=[5.682034492492676, 5.4959001541137695, 5.735622882843018, 5.846793174743652];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{12}	=[8.9,8.76,8.82,8.64];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{12}	=[5.630070209503174, 5.490847110748291, 5.675378322601318, 5.807642459869385];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{12}	=[5.633796691894531, 5.446336269378662, 5.694647312164307, 5.775539875030518];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{12}	=[5.608705997467041, 5.423897743225098, 5.695301055908203, 5.763045787811279];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{12}	=[5.650200843811035, 5.481822967529297, 5.697793483734131, 5.798167705535889];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{12}	=[5.630693435668945, 5.460726022720337, 5.69078004360199, 5.786098957061768];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{12}	=[0.017068538515447786, 0.031175568404728748, 0.010356908655187078, 0.02043461139658019];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{12}	=[5.89184243451983, 5.953971018739149, 5.647173691890733, 6.098671294354175];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{12}	=[5.630070209503174, 5.490847110748291, 5.675378322601318, 5.807642459869385];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{12}	=[5.633796691894531, 5.446336269378662, 5.694647312164307, 5.775539875030518];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{12}	=[5.608705997467041, 5.423897743225098, 5.695301055908203, 5.763045787811279];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{12}	=[5.650200843811035, 5.481822967529297, 5.697793483734131, 5.798167705535889];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{12}	=[5.630693435668945, 5.460726022720337, 5.69078004360199, 5.786098957061768];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{12}	=[5.682034492492676, 5.4959001541137695, 5.735622882843018, 5.846793174743652];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506021200UTC  to 201506021500UTC  (57175.5-->57175.625) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506021500UTC (57175.625) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{13}	=57175.625;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{13}	=[5.780898571014404, 5.6050591468811035, 5.910792827606201, 5.7124834060668945];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{13}	=[9.42,9.8,9.44,9.75];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{13}	=[5.711303234100342, 5.539144039154053, 5.805695533752441, 5.625061988830566];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{13}	=[5.729236602783203, 5.540657043457031, 5.895505905151367, 5.666794300079346];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{13}	=[5.73685359954834, 5.589374542236328, 5.82614803314209, 5.774744987487793];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{13}	=[5.702651023864746, 5.581483364105225, 5.792392730712891, 5.656681060791016];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{13}	=[5.720011115074158, 5.562664747238159, 5.829935550689697, 5.68082058429718];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{13}	=[0.015768944845127822, 0.026489722687168277, 0.04586523166238174, 0.06509036725247554];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{13}	=[6.135745721392668, 5.958464600590193, 6.468189980673048, 7.471897528728899];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{13}	=[5.711303234100342, 5.539144039154053, 5.805695533752441, 5.625061988830566];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{13}	=[5.729236602783203, 5.540657043457031, 5.895505905151367, 5.666794300079346];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{13}	=[5.73685359954834, 5.589374542236328, 5.82614803314209, 5.774744987487793];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{13}	=[5.702651023864746, 5.581483364105225, 5.792392730712891, 5.656681060791016];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{13}	=[5.720011115074158, 5.562664747238159, 5.829935550689697, 5.68082058429718];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{13}	=[5.780898571014404, 5.6050591468811035, 5.910792827606201, 5.7124834060668945];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506021500UTC  to 201506021800UTC  (57175.625-->57175.75) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506021800UTC (57175.75) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{14}	=57175.75;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{14}	=[5.763506889343262, 5.673346042633057, 5.6378607749938965, 5.693177223205566];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{14}	=[9.86,10.25,9.89,10.26];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{14}	=[5.718655109405518, 5.625396728515625, 5.601602554321289, 5.6595940589904785];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{14}	=[5.6973981857299805, 5.618008613586426, 5.596559524536133, 5.653909206390381];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{14}	=[5.729131698608398, 5.641080379486084, 5.595271587371826, 5.653496742248535];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{14}	=[5.74797248840332, 5.656146049499512, 5.608092784881592, 5.658304214477539];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{14}	=[5.723289370536804, 5.635157942771912, 5.60038161277771, 5.656326055526733];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{14}	=[0.021096752766263334, 0.01697995559389363, 0.005821614656997456, 0.0030789139236522753];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{14}	=[6.089207794618407, 5.922126805976503, 5.6798204919023405, 5.681662970873055];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{14}	=[5.718655109405518, 5.625396728515625, 5.601602554321289, 5.6595940589904785];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{14}	=[5.6973981857299805, 5.618008613586426, 5.596559524536133, 5.653909206390381];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{14}	=[5.729131698608398, 5.641080379486084, 5.595271587371826, 5.653496742248535];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{14}	=[5.74797248840332, 5.656146049499512, 5.608092784881592, 5.658304214477539];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{14}	=[5.723289370536804, 5.635157942771912, 5.60038161277771, 5.656326055526733];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{14}	=[5.763506889343262, 5.673346042633057, 5.6378607749938965, 5.693177223205566];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506021800UTC  to 201506022100UTC  (57175.75-->57175.875) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% ========================================================================
%
%  analysis at 201506022100UTC (57175.875) 
%
% ========================================================================
%
%  resultItem id: analysis_time, outputLevel: Normal, context: analysis step
analysis_time{15}	=57175.875;
%  resultItem id: pred_f_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_central{15}	=[5.59448766708374, 5.647350311279297, 5.612604141235352, 5.5743327140808105];
%  resultItem id: obs, outputLevel: Essential, context: analysis step
obs{15}	=[9.99,10.18,10.11,10.17];
%  resultItem id: pred_f_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_0{15}	=[5.556032180786133, 5.61489200592041, 5.5790114402771, 5.5460968017578125];
%  resultItem id: pred_f_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_1{15}	=[5.548765182495117, 5.616324424743652, 5.574092388153076, 5.543638706207275];
%  resultItem id: pred_f_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_2{15}	=[5.568611145019531, 5.609765529632568, 5.575465679168701, 5.532639503479004];
%  resultItem id: pred_f_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_3{15}	=[5.573999404907227, 5.61137580871582, 5.5837931632995605, 5.55454683303833];
%  resultItem id: pred_f, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f{15}	=[5.561851978302002, 5.613089442253113, 5.578090667724609, 5.5442304611206055];
%  resultItem id: pred_f_std, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_f_std{15}	=[0.011523607297796522, 0.003038600692089814, 0.004329860749360031, 0.0090297882929365];
% length of state vector: 46024.
% number of observations: 4.
%  resultItem id: pred_a_linear, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_linear{15}	=[5.626574957514823, 5.604880902073169, 5.613170316666982, 5.605220612783981];
%  resultItem id: pred_a_0, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_0{15}	=[5.556032180786133, 5.61489200592041, 5.5790114402771, 5.5460968017578125];
%  resultItem id: pred_a_1, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_1{15}	=[5.548765182495117, 5.616324424743652, 5.574092388153076, 5.543638706207275];
%  resultItem id: pred_a_2, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_2{15}	=[5.568611145019531, 5.609765529632568, 5.575465679168701, 5.532639503479004];
%  resultItem id: pred_a_3, outputLevel: Normal, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_3{15}	=[5.573999404907227, 5.61137580871582, 5.5837931632995605, 5.55454683303833];
%  resultItem id: pred_a, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a{15}	=[5.561851978302002, 5.613089442253113, 5.578090667724609, 5.5442304611206055];
%  resultItem id: pred_a_central, outputLevel: Essential, context: analysis step
%  predictions: station1.temperature, station2.temperature, station3.temperature, station4.temperature
pred_a_central{15}	=[5.59448766708374, 5.647350311279297, 5.612604141235352, 5.5743327140808105];
% Algorithm starting next step
% ========================================================================
%
%  Forecast from 201506022100UTC  to 201506030000UTC  (57175.875-->57176.0) 
%
% ========================================================================
%
% - mainModel 
%
% computation for member (4):
% - member 0
% - member 1
% - member 2
% - member 3
% Algorithm Done
% Application Done
