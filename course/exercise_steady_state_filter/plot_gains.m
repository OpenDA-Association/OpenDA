%
% Read gain matrices and make some plots of the (waterlevel) part of the columns.
%
% author: Nils van Velzen
%
dirname{1}='enkf_wave_185811190000';
dirname{2}='enkf_wave_185811210000';
dirname{3}='enkf_wave_185811230000';
dirname{4}='enkf_wave_185811250000';
dirname{5}='enkf_wave_185811270000';

for i=1:length(dirname)
  A{i}=read_gain(dirname{i});
end


for i=1:length(A)
   figure(i)
   Ai=A{i};
   plot(Ai(3:102,:))
   legend('station 1','station 2', 'station 3')
   title(['Waterlevel gain "',dirname{i},'"']);
   xlabel('location in state vector')
   ylabel('Gain')
end

