
%load the data
reactive_pollution_model_output;
sequentialSimulation_results;
x_fg=x_a; %rename to avoid overwritten arrays
%enkf_results;
enkf_results2;
x_enkf=x_a;

%offsets for chunks in state vector
ngrid=length(c1);
no_sources=length(x_f{1})-2*ngrid;

figure(1); clf;
for i=1:length(x_fg),
	subplot(2,1,1);
	h1=plot(c1_map{i},'k-');
	outlocs1 = output_locations(output_substance==1);
	hold on;plot(outlocs1,0*outlocs1,'*');hold off
	sourcelocs1 = source_locations(source_substance==1);
	hold on;plot(sourcelocs1,0*sourcelocs1,'d');hold off
	ylabel('c_1');
	axis([0 ngrid 0 200]);
	% add first guess
	c_fg=x_fg{i};
	c1_fg=c_fg((no_sources+1):(no_sources+ngrid));
	hold on;h2=plot(c1_fg,'b-');hold off
	% add enkf
	c_enkf=x_enkf{i};
	c1_enkf=c_enkf((no_sources+1):(no_sources+ngrid));
	hold on;h3=plot(c1_enkf,'g-');hold off
	legend([h1,h2,h3],'truth','first guess','EnKF');

	subplot(2,1,2);
	plot(c2_map{i},'k-');
	outlocs2 = output_locations(output_substance==2);
	hold on;plot(outlocs2,0*outlocs2,'*');hold off
	sourcelocs2 = source_locations(source_substance==2);
	hold on;plot(sourcelocs2,0*sourcelocs2,'d');hold off
	ylabel('c_2');
	axis([0 ngrid 0 300]);
	% add first guess
	c2_fg=c_fg((no_sources+ngrid+1):end);
	hold on;plot(c2_fg,'b-');hold off
	% add enkf
	c2_enkf=c_enkf((no_sources+ngrid+1):end);
	hold on;plot(c2_enkf,'g-');hold off

	pause(0.1);
end;
