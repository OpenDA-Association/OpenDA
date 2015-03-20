% Script to plot some of the results
clear all
results
%index of interest where the obs are.
idx = 21+11;

n = size(x_a{1,1},2);
nt = size(x_a,2);


xa = reshape ( cell2mat(x_a) , n, nt) ;
xf = reshape ( cell2mat(x_f) , n, nt) ;
obst = cell2mat(obs);


n = size(xi_f_0{1,1},2);
nt = size(xi_f_0,2);
x0 = reshape ( cell2mat(xi_f_0) , n, nt) ;
x1 = reshape ( cell2mat(xi_f_1) , n, nt) ;
x2 = reshape ( cell2mat(xi_f_2) , n, nt) ;
x3 = reshape ( cell2mat(xi_f_3) , n, nt) ;

figure(1);
hold on;
plot(xa(idx,:),'b*-'); 
plot(xf(idx,:),'g*-');
plot(obst,'ro');
hold off
legend('xa','xf','obs');
title('Forecasted and analyzed state and observerd value at centerpoint');


figure(2);
plot(x0(idx,:),'b');
hold on
plot(x1(idx,:),'g');
plot(x2(idx,:),'y');
plot(x3(idx,:),'c');
plot((x0(idx,:)+x1(idx,:)+x2(idx,:)+x3(idx,:))*0.25,'k');
hold off

n = size(x_f_central{1,1},2);
nt = size(x_f_central,2);
x_c=reshape ( cell2mat(x_f_central) , n, nt) ;
figure(3);
plot(x_c(idx,:),'b');
hold off