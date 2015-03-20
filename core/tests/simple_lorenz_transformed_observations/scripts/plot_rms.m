%plot_rms

%simulation
simulation_results
xall=reshape([x{1:(length(x))}],length(x{1}),length(x));
tall=[t{1:(length(x))}];

%enkf
enkf_results
taall = 0.2*(1:1:length(x_a));
xfall=reshape([x_f{:}],length(x_f{1}),length(x_f));
xaall=reshape([x_a{:}],length(x_a{1}),length(x_a));
stdxfall=reshape([std_x_f{:}],length(std_x_f{1}),length(std_x_f));
stdxaall=reshape([std_x_a{:}],length(std_x_a{1}),length(std_x_a));
trpf = sqrt(mean(stdxfall.^2,1)); % average over variables corresponding to rms
trpa = sqrt(mean(stdxaall.^2,1));

%compute rms
rms_enkf_xf = NaN*ones(1,length(xfall));
rms_enkf_xa = NaN*ones(1,length(xaall));
for i=1:length(xaall),
   [imin,isim] = min(abs(tall-taall(i)));
   rms_enkf_xf(i) = sqrt(mean((xfall(:,i)-xall(:,isim)).^2));
   rms_enkf_xa(i) = sqrt(mean((xaall(:,i)-xall(:,isim)).^2));
end;

% timeseries enkf
% rms vs t
figure(1);clf
plot(taall,rms_enkf_xf,'k-'); %forecast
hold on
plot(taall,rms_enkf_xa,'b-'); %analysis
hold off
xlabel('time');
ylabel('rms');
legend('EnKF rms(x^f-x^t)','EnKF rms(x^a-x^t)')
print rms_lorenz_enkf.png -dpng
% std vs t
figure(2);clf
plot(taall,trpf,'k-'); %forecast
hold on
plot(taall,trpa,'b-'); %analysis
hold off
xlabel('time');
ylabel('sqrt(tr(P))');
legend('EnKF sqrt(tr(Pf))','EnKF sqrt(tr(P^a))')
print std_lorenz_enkf.png -dpng

%dudenkf
clear x_f
clear x_a
clear trpf
clear trpa
dudenkf_results
taall = 0.2*(1:1:length(x_a));
xfall=reshape([x_f{:}],length(x_f{1}),length(x_f));
xaall=reshape([x_a{:}],length(x_a{1}),length(x_a));
stdxfall=reshape([std_x_f{:}],length(std_x_f{1}),length(std_x_f));
stdxaall=reshape([std_x_a{:}],length(std_x_a{1}),length(std_x_a));
trpf = sqrt(mean(stdxfall.^2,1)); % average over variables corresponding to rms
trpa = sqrt(mean(stdxaall.^2,1));

%compute rms
rms_dudenkf_xf = NaN*ones(1,length(xfall));
rms_dudenkf_xa = NaN*ones(1,length(xaall));
for i=1:length(xaall),
   [imin,isim] = min(abs(tall-taall(i)));
   rms_dudenkf_xf(i) = sqrt(mean((xfall(:,i)-xall(:,isim)).^2));
   rms_dudenkf_xa(i) = sqrt(mean((xaall(:,i)-xall(:,isim)).^2));
end;

% timeseries dudenkf
% rms vs t
figure(3);clf
plot(taall,rms_dudenkf_xf,'k-'); %forecast
hold on
plot(taall,rms_dudenkf_xa,'b-'); %analysis
hold off
xlabel('time');
ylabel('rms');
legend('DUDEnKF rms(x^f-x^t)','DUDEnKF rms(x^a-x^t)')
print rms_lorenz_dudenkf.png -dpng
% std vs t
figure(4);clf
plot(taall,trpf,'k-'); %forecast
hold on
plot(taall,trpa,'b-'); %analysis
hold off
xlabel('time');
ylabel('sqrt(tr(P))');
legend('DUDEnKF sqrt(tr(Pf))','DUDEnKF sqrt(tr(P^a))')
print std_lorenz_dudenkf.png -dpng

%particle filter
clear x_f
clear x_a
clear trpf
clear trpa
particle_filter_results
taall = 0.2*(1:1:length(x_a));
xfall=reshape([x_f{:}],length(x_f{1}),length(x_f));
xaall=reshape([x_a{:}],length(x_a{1}),length(x_a));
stdxfall=reshape([std_x_f{:}],length(std_x_f{1}),length(std_x_f));
stdxaall=reshape([std_x_a{:}],length(std_x_a{1}),length(std_x_a));
trpf = sqrt(mean(stdxfall.^2,1)); % average over variables corresponding to rms
trpa = sqrt(mean(stdxaall.^2,1));

%compute rms
rms_dudenkf_xf = NaN*ones(1,length(xfall));
rms_dudenkf_xa = NaN*ones(1,length(xaall));
for i=1:length(xaall),
   [imin,isim] = min(abs(tall-taall(i)));
   rms_dudenkf_xf(i) = sqrt(mean((xfall(:,i)-xall(:,isim)).^2));
   rms_dudenkf_xa(i) = sqrt(mean((xaall(:,i)-xall(:,isim)).^2));
end;

% timeseries dudenkf
% rms vs t
figure(5);clf
plot(taall,rms_dudenkf_xf,'k-'); %forecast
hold on
plot(taall,rms_dudenkf_xa,'b-'); %analysis
hold off
xlabel('time');
ylabel('rms');
legend('SIR rms(x^f-x^t)','SIR rms(x^a-x^t)')
print rms_lorenz_sir.png -dpng
% std vs t
figure(6);clf
plot(taall,trpf,'k-'); %forecast
hold on
plot(taall,trpa,'b-'); %analysis
hold off
xlabel('time');
ylabel('sqrt(tr(P))');
legend('SIR sqrt(tr(Pf))','SIR sqrt(tr(P^a))')
print std_lorenz_sir.png -dpng
