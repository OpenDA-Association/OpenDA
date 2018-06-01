
%rng default
cd stat_run

fid = fopen('gen_obs100_twoVar_assim_a_nrm_seed10.csv','w');
fprintf(fid,'time, location, value, std, type\n');
fclose(fid);

refid = fopen('referenceField_nrm_seed10.csv','w');
fprintf(refid,'location, a, b\n');
fclose(refid);

%%% Generate initial sample

% variable a

N_sines=5;
N_var=100;

amplitude = rand(N_sines+1,1)             % sine amplitudes as in paper; 0 < A_k < 1
phase = rand(N_sines+1,1) * 2 * pi   % sine phases as in paper; 0 < p_k < 2*pi

a = zeros(N_var,1);        % two variables of each 1000 grid values

% initialisation for variable a
for i=1:N_var
  
   for k=1:(N_sines+1)
      a(i) = a(i) + amplitude(k) * sin(2*pi*(k-1)*i/N_var + phase(k));
   end
   
end


% zero mean
%a = a - mean(a);

% variance 1
a = a./std(a);

% Now variable a indeed has mean 6 and variance 1.0


%%% variable b (calculated from reference a)

b=zeros(length(a),1);
b(2:end-1) =0.5*(a(3:end)-a(1:end-2));
b(1) = 0.5*(a(2)-a(end));
b(end) = 0.5*(a(1)-a(end-1));

% zero mean
%b = b - mean(b);

% variance 1
%b = b./std(b);

%%% offset a with mean 6 and b with mean 0.5

% offset as mean 0
a = a - mean(a) + 6.0;

% offset as mean 1
b = b - mean(b) + 0.5;

% a = a*0.0;
% b = b*0.0;

refid = fopen('referenceField_nrm_seed10.csv','a');
for i=1:N_var
    fprintf(refid,'%2.1f, %12.8f, %12.8f\n',i,a(i), b(i));
end

fclose(refid);

%%% Calculate truth as sum of reference field with the next sample

aNew = a*0.0;
bNew = b*0.0;
   
amplitude = rand(N_sines+1,1);             % sine amplitudes as in paper; 0 < A_k < 1
phase = rand(N_sines+1,1) * 2 * pi;   % sine phases as in paper; 0 < p_k < 2*pi

% initialisation for variable a
for i=1:N_var
  
   for k=1:(N_sines+1)
      aNew(i) = aNew(i) + amplitude(k) * sin(2*pi*(k-1)*i/N_var + phase(k));
   end
   
end

% zero mean
aNew = aNew - mean(aNew);

% variance 1
aNew = aNew./std(aNew);

% Now variable a indeed has mean 0 and variance 1.0

% Calculate b

bNew=zeros(length(aNew),1);
bNew(2:end-1) = 0.5*(aNew(3:end)-aNew(1:end-2));
bNew(1) = 0.5*(aNew(2)-aNew(end));
bNew(end) = 0.5*(aNew(1)-aNew(end-1));

% Add reference field

aAlt = aNew + a;
bAlt = bNew + b;

% This is the initial true state

var_a{1} = aAlt;
var_b{1} = bAlt;


%%% Propagate truth

for t = 1:200
   
   % Calculate next a 
   
   aNew(2:end) = aAlt(1:end-1);
   aNew(1) = aAlt(end);
   
   % Calculate next b
   bNew(2:end) = bAlt(1:end-1);
   bNew(1) = bAlt(end);
      
   var_a{t+1} = aNew;
   var_b{t+1} = bNew;
     
   % Add noise
   std_a = 0.01;
   std_b = 0.001;
   o_a = aNew + sqrt(std_a)*randn(length(aNew),1);
%    o_b = bNew + sqrt(std_b)*randn(length(bNew),1);
   
   fid = fopen('gen_obs100_twoVar_assim_a_nrm_seed10.csv','a');

   if (mod(t,5) == 0)
      for i = [20 40 60 80]  
         fprintf(fid,'%2.1f, %2.1f, %12.8f, %1.2f, %1.2f\n',t, i, o_a(i),sqrt(std_a),1.0);
      end

%       for i =[20 40 60 80] 
%          fprintf(fid,'%2.1f, %2.1f, %12.8f, %1.2f, %1.2f\n',t, i, o_b(i),0.1,2.0);
%       end
      fclose(fid);
   end

   aAlt = aNew;
   bAlt = bNew;
end


save gen_obs100_twoVar_assim_a_nrm_seed10.mat var_a var_b;

cd ..

for j = 1:200
    hold off
plot([var_a{j}])
hold on
plot([var_b{j}])
% axis([0 100 -7 7])
% axis([0 1000 -7 7])
drawnow
pause(0.01)
F(j)=getframe;
end
