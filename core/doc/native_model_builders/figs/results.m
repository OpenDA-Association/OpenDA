%Aster 30x30x4  24 UUR

timestep(1) = 701.748992919922;
assim    = 92.2109680175781;


timestep(2) = 336.705993652344;

timestep(3) = 237.515014648438;

timestep(4) = 138.054016113281;

timestep(5) =91.3769836425781;

nproc=[1,2,3,6,12];
assim_seq=assim*ones(size(nproc));

plot(nproc,timestep,'r'); hold on
plot(nproc,assim_seq,'b'); hold off
title('Lotos-Euros; Ensemble of 24 model on ASTER  30x30x4 mesh 24 hour simulation')
xlabel ("Number of processors");
ylabel ("Wall-time (s) of timesteps");
y=assim*1.3;
text(2,y,'Assimilation time on 1 CPU')

print aster_30_24.eps -depsc 


speedup=ones(size(timestep))./timestep*timestep(1);

plot(nproc,speedup,'r'); hold on;
plot(nproc,nproc,'b'); hold off;
xlabel ("Number of processors");
ylabel ("Speedup");

print aster_30_24_su.eps -depsc 



%Aster 60x60x4  24 UUR

timestep(1) = 3059.37399291992;
assim    = 539.819000244141;


timestep(2) =  1759.47000122070;

timestep(3) = 1348.66299438477;

timestep(4) = 941.031005859375;

timestep(5) =740.764007568359;

nproc=[1,2,3,6,12];
assim_seq=assim*ones(size(nproc));

plot(nproc,timestep,'r'); hold on
plot(nproc,assim_seq,'b'); hold off
title('Lotos-Euros; Ensemble of 24 model on ASTER  60x60x4 mesh 24 hour simulation')
xlabel ("Number of processors");
ylabel ("Wall-time (s) of timesteps");
y=assim*1.3;
text(2,y,'Assimilation time on 1 CPU')

print aster_60_24.eps -depsc 


speedup=ones(size(timestep))./timestep*timestep(1);

plot(nproc,speedup,'r'); hold on;
plot(nproc,nproc,'b'); hold off;
xlabel ("Number of processors");
ylabel ("Speedup");

print aster_60_24_su.eps -depsc 









if 0

%Laptop 30x30
nl=[1 2];
t30_as=1.67199707031250;
t30l=[43.9119873046875, 29.8649902343750];
t30lp=(t30l-t30_as);

t30l=t30l/t30l(1);
t30lp=t30lp/t30lp(1);


%Laptop 60x60
t60_as=44.6970214843750;
t60l=[234.566986083984, 164.756011962891];
t60lp=(t60l-t60_as);

t60l=t60l/t60l(1);
t60lp=t60lp/t60lp(1);

%Aster 30x30
na=[1 2 4 6 12];
t30a_as=3.07702636718750;
t30a=[51.1059875488281, 39.2750244140625, 27.8359985351562, 22.1260070800781, 18.4869995117188];
t30ap=(t30a-t30a_as);

t30a=t30a/t30a(1);
t30ap=t30ap/t30ap(1);


t30l=t30l.^-1;
t60l=t60l.^-1;
t30a=t30a.^-1;

figure(1);
title('Relative time');
hold off;
plot(nl,[t30l' t60l'])
hold on;
plot(na,t30a')
legend('Centrino duo: 30x30x4','Centrino duo: 60x60x4','Aster 30x30x4');
hold off;

t30lp=t30lp.^-1;
t60lp=t60lp.^-1;
t30ap=t30ap.^-1;

figure(2);
title('Relative time -no sequential assim');
hold off;
plot(nl,[t30lp' t60lp'])
hold on;
plot(na,t30ap')
legend('Centrino duo: 30x30x4','Centrino duo: 60x60x4','Aster 30x30x4');
hold off;

end




