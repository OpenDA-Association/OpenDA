function [a,amplitude,phase] = generate_sample(N_sines, N_var)

%N_sines = 5;                        % Number of sines to combine

amplitude = rand(N_sines+1,1);             % sine amplitudes as in paper; 0 < A_k < 1
phase = rand(N_sines+1,1) * 2 * pi;   % sine phases as in paper; 0 < p_k < 2*pi

a = zeros(N_var,1);        % two variables of each 1000 grid values

% initialisation for variable a
for i=1:N_var
  
	for k=1:(N_sines+1)
      a(i) = a(i) + amplitude(k) * sin(2*pi*(k-1)*i/N_var + phase(k));
   end
   
end

end