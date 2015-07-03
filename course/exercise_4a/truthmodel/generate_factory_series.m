% generate a timeseries with output from a factory
%
% uses an AR(1) proces and log transform
%
type=1; %1=additive 2=multiplicative
dt=60;
time=0:dt:18000; %5hours with 1min step
alpha=exp(-dt/(60*dt)); %1hour time correlation
mean_pollution = 0.0;
if(type==1)
	sigma=40.0;
else
	sigma=1.0;
end;
sigma=sigma*sqrt(1.0-alpha*alpha);
n=length(time);

white=sigma*randn(1,n);
noise=zeros(1,n);
noise(1)=white(1);
for i=2:n,
	noise(i) = alpha*noise(i-1)+white(i);
end;
noise=noise-mean(noise);

if(type==1),
	factory = mean_pollution + noise;
else
	factory = mean_pollution * exp(log(10.0)* noise);
end;
