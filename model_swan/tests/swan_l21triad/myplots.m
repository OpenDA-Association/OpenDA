
load data

figure(1);clf
plot([dud.costTotal{:}],'k-');
hold on
plot([simplex.cost{:}],'g-');
plot([powell.cost{:}],'r-');
hold off
xlabel('evaluation number');
ylabel('cost');
legend('dud','simplex','powell');
title('calibration swan laboratory test without weak-constraint');
figure(2);clf
plot([dudc.costTotal{:}],'k-');
hold on
plot([simplexc.cost{:}],'g-');
plot([powellc.cost{:}],'r-');
hold off
xlabel('evaluation number');
ylabel('cost');
legend('dud','simplex','powell');
title('calibration swan laboratory test with weak-constraint');

figure(3);clf
pardata=dud.evaluatedParameters;
m = length(pardata{2});
n = length(pardata);
dud_pars=reshape([pardata{:}],m,n);
pardata=simplex.evaluatedParameters;
m = length(pardata{2});
n = length(pardata);
simplex_pars=reshape([pardata{:}],m,n);
pardata=powell.evaluatedParameters;
m = length(pardata{2});
n = length(pardata);
powell_pars=reshape([pardata{:}],m,n);
for i=1:m,
   subplot(m,1,i);
   plot(dud_pars(i,:),'k-');
   hold on;
   plot(simplex_pars(i,:),'g-');
   plot(powell_pars(i,:),'r-');
   hold off;
end;

figure(4);clf
pardata=dudc.evaluatedParameters;
m = length(pardata{2});
n = length(pardata);
dudc_pars=reshape([pardata{:}],m,n);
pardata=simplexc.evaluatedParameters;
m = length(pardata{2});
n = length(pardata);
simplexc_pars=reshape([pardata{:}],m,n);
pardata=powellc.evaluatedParameters;
m = length(pardata{2});
n = length(pardata);
powellc_pars=reshape([pardata{:}],m,n);
for i=1:m,
   subplot(m,1,i);
   plot(dudc_pars(i,:),'k-');
   hold on;
   plot(simplexc_pars(i,:),'g-');
   plot(powellc_pars(i,:),'r-');
   hold off;
end;
