
reactive_pollution_model_truth;
c1_map_truth = c1_map;
c2_map_truth = c2_map;

reactive_pollution_model_output;

figure(1); clf;
for i=1:length(c1_map),
	subplot(2,1,1);
	plot(c1_map{i},'b-');
	hold on;plot(c1_map_truth{i},'k-');hold off
	outlocs1 = output_locations(output_substance==1);
	hold on;plot(outlocs1,0*outlocs1,'*');hold off
	sourcelocs1 = source_locations(source_substance==1);
	hold on;plot(sourcelocs1,0*sourcelocs1,'d');hold off
	ylabel('c_1');
	subplot(2,1,2);
	plot(c2_map{i},'b-');
	hold on;plot(c2_map_truth{i},'k-');hold off
	outlocs2 = output_locations(output_substance==2);
	hold on;plot(outlocs2,0*outlocs2,'*');hold off
	sourcelocs2 = source_locations(source_substance==2);
	hold on;plot(sourcelocs2,0*sourcelocs2,'d');hold off
	ylabel('c_2');
	legend('uncalibrated model','truth')
	pause(0.1);
end;
