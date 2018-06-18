%Create some figures
cd results_denkfbias_poster
f_x_a
f_analysis_time
cd ..

t=analysis_time-analysis_time(1);

n=size(x_a,2)
p15=n-5;
p25=n-4;
p20=n-3;
p10=n-2;
p30=n-1;
p5=n;

plot(t', x_a(:,[p5 p10 p15 p20 p25 p30]),'LineWidth',4)
h_leg=legend('H5: (+25cm bias)', 'H10: (+50cm bias)', 'H15: (0 cm bias)', 'H20: (0 cm bias)','H25: (-50 cm bias)', 'H30: (-30 cm bias)');
set(h_leg,'FontSize', 20);
title('Detected bias in Hydrailic head (DEnKF, n=30)','FontSize', 20);
xlabel('Simulation time (Days)','FontSize', 20);
ylabel('Bias (m)','FontSize', 20);
print -djpeg100 'plaatje.jpg'
