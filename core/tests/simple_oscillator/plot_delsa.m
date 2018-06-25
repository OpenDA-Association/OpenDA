%% Monday, 01-12-2014
%% Plot the output of DELSA algorithm. First load the respective output m file of the algorithm before running this script.
close all;
figure;
plot(Param0{1},Param1{1},'o')
minx=min(Param0{1});maxx=max(Param0{1});
miny=min(Param1{1});maxy=max(Param1{1});
minx=minx-0.10*(maxx-minx);
miny=miny-0.10*(maxy-miny);
maxx=maxx+0.10*(maxx-minx);
maxy=maxy+0.10*(maxy-miny);
set(gca,'xlim',[minx,maxx],'ylim',[miny,maxy]);
xlabel('t_{damp}');ylabel('\Omega')

figure;
par0=reshape(Param0{1},10,10);
par1=reshape(Param1{1},10,10);
SL1_par0=reshape(SL1_param0{1},10,10);
SL1_par1=reshape(SL1_param1{1},10,10);
scatter(Param0{1},Param1{1},1500,SL1_param0{1},'.')
xlabel('t_{damp}');ylabel('\Omega');title('SL1 of parameter t_{damp}')
set(gca,'xlim',[minx,maxx],'ylim',[miny,maxy]);
box on;
colorbar;
print('-dpng','SL1_t_damp.png')

figure;
scatter(Param0{1},Param1{1},1500,SL1_param1{1},'.')
xlabel('t_{damp}');ylabel('\Omega');title('SL1 of parameter \Omega')
set(gca,'xlim',[minx,maxx],'ylim',[miny,maxy]);
box on;
colorbar;
print('-dpng','SL1_omega.png')