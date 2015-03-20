% good iter
good_iter = 1:9;
good_eval = [ 50.962389808028, 3.0416714472266175, 3.042498568098905, 3.0418931112370653, 3.0417343051228003, 3.041690882647114, 3.041678166554914, 3.0416703363489375, 3.0417571293846244];
good_outer_iter = [ 2 3 ];
good_outer_lineval = [3.0525517144219383 , 3.040901258454022];

figure(1);clf
semilogy(good_iter,good_eval,'k-');
hold on
semilogy(good_outer_iter, good_eval(good_outer_iter),'b+');
semilogy(good_outer_iter, good_outer_lineval,'gd');
hold off;
xlabel('Evaluation');
ylabel('Cost');
legend('Evaluation','Outer iteration','Linearized cost')

%AnalysisLeastSquaresCost: evaluation 1 : cost = 50.962389808028
%AnalysisLeastSquaresCost: evaluation 2 : cost = 3.0416714472266175
%non-linear cost for new estimate 3.0416714472266175
%linearized cost for new estimate 3.0525517144219383
%AnalysisLeastSquaresCost: evaluation 3 : cost = 3.042498568098905
%non-linear cost for new estimate 3.042498568098905
%linearized cost for new estimate 3.040901258454022
%AnalysisLeastSquaresCost: evaluation 4 : cost = 3.0418931112370653
%AnalysisLeastSquaresCost: evaluation 5 : cost = 3.0417343051228003
%AnalysisLeastSquaresCost: evaluation 6 : cost = 3.041690882647114
%AnalysisLeastSquaresCost: evaluation 7 : cost = 3.041678166554914
%AnalysisLeastSquaresCost: evaluation 8 : cost = 3.0416703363489375
%-
%AnalysisLeastSquaresCost: evaluation 9 : cost = 3.0417571293846244



% bad iter
bad_iter = 1:25;
bad_eval = [ 146.75631769260866 ,188.9583500817153 ,704.0536222346798 ,440.81690104302834 ,224.28948894319342 ,142.19396219313424 ,108.8506504851723 ,108.23299700583074 ,103.56113738814136 ,193.355378883954 ,5754.818748048964 ,448.8085735228593 ,300.61505339080674 ,137.54084547449543 ,113.88748512581044 ,105.90334971060253 ,104.17871253481498 ,122.95615237280039 ,109.36035388189555 ,104.81823726020814 ,103.93548579318225 ,103.62916243207505 ,103.59036158420886 ,103.56240462125555 ,103.56446482072535 ];
bad_outer_iter = [ 2 10 18 ];
bad_outer_lineval = [4.9055369931910775 5.404129358946746 94.53099459792186 ];

figure(2);clf
semilogy(bad_iter,bad_eval,'k-');
hold on
semilogy(bad_outer_iter, bad_eval(bad_outer_iter),'b+');
semilogy(bad_outer_iter, bad_outer_lineval,'gd');
hold off;
xlabel('Evaluation');
ylabel('Cost');
legend('Evaluation','Outer iteration','Linearized cost')

%AnalysisLeastSquaresCost: evaluation 1 : cost = 146.75631769260866
%AnalysisLeastSquaresCost: evaluation 2 : cost = 188.9583500817153
%non-linear cost for new estimate 188.9583500817153
%linearized cost for new estimate 4.9055369931910775
%AnalysisLeastSquaresCost: evaluation 3 : cost = 704.0536222346798
%AnalysisLeastSquaresCost: evaluation 4 : cost = 440.81690104302834
%AnalysisLeastSquaresCost: evaluation 5 : cost = 224.28948894319342
%AnalysisLeastSquaresCost: evaluation 6 : cost = 142.19396219313424
%AnalysisLeastSquaresCost: evaluation 7 : cost = 108.8506504851723
%AnalysisLeastSquaresCost: evaluation 8 : cost = 108.23299700583074
%AnalysisLeastSquaresCost: evaluation 9 : cost = 103.56113738814136
%AnalysisLeastSquaresCost: evaluation 10 : cost = 193.355378883954
%non-linear cost for new estimate 193.355378883954
%linearized cost for new estimate 5.404129358946746
%AnalysisLeastSquaresCost: evaluation 11 : cost = 5754.818748048964
%AnalysisLeastSquaresCost: evaluation 12 : cost = 448.8085735228593
%AnalysisLeastSquaresCost: evaluation 13 : cost = 300.61505339080674
%AnalysisLeastSquaresCost: evaluation 14 : cost = 137.54084547449543
%AnalysisLeastSquaresCost: evaluation 15 : cost = 113.88748512581044
%AnalysisLeastSquaresCost: evaluation 16 : cost = 105.90334971060253
%AnalysisLeastSquaresCost: evaluation 17 : cost = 104.17871253481498
%AnalysisLeastSquaresCost: evaluation 18 : cost = 122.95615237280039
%non-linear cost for new estimate 122.95615237280039
%linearized cost for new estimate 94.53099459792186
%AnalysisLeastSquaresCost: evaluation 19 : cost = 109.36035388189555
%AnalysisLeastSquaresCost: evaluation 20 : cost = 104.81823726020814
%AnalysisLeastSquaresCost: evaluation 21 : cost = 103.93548579318225
%AnalysisLeastSquaresCost: evaluation 22 : cost = 103.62916243207505
%AnalysisLeastSquaresCost: evaluation 23 : cost = 103.59036158420886
%AnalysisLeastSquaresCost: evaluation 24 : cost = 103.56240462125555
%AnalysisLeastSquaresCost: evaluation 25 : cost = 103.56446482072535
