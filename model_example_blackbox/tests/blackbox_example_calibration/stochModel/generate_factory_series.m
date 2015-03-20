% generate a timeseries with output from a factory
%
% uses an AR(1) proces and log transform
%
dt=60;
time=0:dt:18000; %5hours with 1min step
alpha=exp(-dt/(60*dt)); %1hour time correlation
sigma=sqrt(1.0-alpha*alpha);
n=length(time);

white=sigma*randn(1,n);
noise=zeros(1,n);
noise(1)=white(1);
for i=2:n,
	noise(i) = alpha*noise(i-1)+white(i);
end;
noise=noise-mean(noise);

mean_pollution = 100.0;
factory = mean_pollution * exp(log(10.0)* noise);
