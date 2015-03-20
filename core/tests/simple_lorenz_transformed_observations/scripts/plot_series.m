function plot_series(simfile,kalfile)
% plot timeseries for kalman filter run
% simfile filename for reference solution
% kalfile filename for kalman run

%
%load results
%

%simulation
eval(simfile);
%simulation_results
%xall=reshape([x{1:(length(x)*0.5)}],length(x{1}),0.5*length(x));
xall=reshape([x{1:(length(x))}],length(x{1}),length(x));
%tall=[t{1:(0.5*length(x))}];
tall=[t{1:(length(x))}];

%enkf
eval(kalfile);
%enkf_results
%dudenkf_results_copy
taall = 0.2*(1:1:length(x_a));
xfall=reshape([x_f{:}],length(x_f{1}),length(x_f));
xaall=reshape([x_a{:}],length(x_a{1}),length(x_a));
obsall=reshape([obs{1:(length(obs))}],length(obs{1}),length(obs));
predfall=reshape([pred_f{1:(length(pred_f))}],length(pred_f{1}),length(pred_f));
predaall=reshape([pred_a{1:(length(pred_a))}],length(pred_a{1}),length(pred_a));

% timeseries enkf
% y(1) vs t
figure(1);clf
plot(tall,xall(1,:).^2,'k-'); %truth
hold on
plot(taall,obsall(1,:),'b+'); %observed
plot(taall,predfall(1,:),'g-'); % forecast
plot(taall,predaall(1,:),'gx-'); % analysis
hold off
xlabel('time');
ylabel('x^2');

% y(1) vs t
figure(2);clf
plot(tall,xall(1,:).^2,'k-'); %truth
hold on
plot(taall,obsall(1,:),'b+'); %observed
plot(taall,predfall(1,:),'g-'); % forecast
plot(taall,predaall(1,:),'gx-'); % analysis
hold off
xlabel('time');
ylabel('x^2');

% y(1) vs t
figure(3);clf
plot(tall,xall(2,:).^2,'k-'); %truth
hold on
plot(taall,obsall(2,:),'b+'); %observed
plot(taall,predfall(2,:),'g-'); % forecast
plot(taall,predaall(2,:),'gx-'); % analysis
hold off
xlabel('time');
ylabel('y^2');

% x vs t
figure(4);clf
plot(tall,xall(1,:),'k-'); %truth
hold on
plot(taall,xfall(1,:),'g-');
plot(taall,xaall(1,:),'gx-');
hold off
xlabel('time');
ylabel('x');

% y vs t
figure(5);clf
plot(tall,xall(2,:),'k-'); %truth
hold on
plot(taall,xfall(2,:),'g-');
plot(taall,xaall(2,:),'gx-');
hold off
xlabel('time');
ylabel('y');

% z vs t
figure(6);clf
plot(tall,xall(3,:),'k-'); %truth
hold on
plot(taall,xfall(3,:),'g-');
plot(taall,xaall(3,:),'gx-');
hold off
xlabel('time');
ylabel('z');
