% generate_dummyobservations

%generates something like:
%
%time,index,value,std,transform
%00.0,2.0,0.0,10.0,2.0
%00.1,2.0,0.0,10.0,2.0
%00.2,2.0,0.0,10.0,2.0
%00.3,2.0,0.0,10.0,2.0
%00.4,2.0,0.0,10.0,2.0
%00.5,2.0,0.0,10.0,2.0

%settings
time       = 0.0:0.2:1000;
index      = [0 1 2];
dummyvalue = 0.0;
stdev      = 10.0;
transform  = 2.0;

fid = fopen('stochobserver/observations_lorenz_dummy.csv','w');
fprintf(fid,'time,index,value,std,transform\n');
for it=1:1:length(time),
   for ii=1:length(index),
       fprintf(fid,'%f,%f,%f,%f,%f\n',time(it),index(ii),dummyvalue,stdev,transform);
   end;
end;
fclose(fid);

