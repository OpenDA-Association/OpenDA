
load 'gen_obs100noref.mat'

h1 = figure;
hold on
plot(var_a{5})
plot(var_a{10},'g')
plot(var_a{15},'r')
plot(var_a{20},'k')
hold off
legend('a(x,5)','a(x,10)','a(x,15)','a(x,20)','Location','Northwest')
grid

print(h1,'general_behaviour','-dpng') 

h2 = figure;
hold on
plot(var_a{11})
plot([20 40 60 80],var_a{11}([20 40 60 80]),'r*')
% plot(40,var_a{10}(40),'r*')
% plot(60,var_a{10}(60),'r*')
% plot(80,var_a{10}(80),'r*')
legend('a(x,10)','obs','Location','Northwest')

print(h2,'observations','-dpng') 