function[OK,stats] = MR_ScatterPlot(XData, YData, axislabels)
%MR_ScatterPlot Creates a scatter plot with statistics box
%   Nice scatter plot function.

OK = 0;

% Create figure
figure1 = figure;

axismax = nanmax( nanmax(XData), nanmax( YData) ) *1.02;
axismin = min( min(XData), min( YData) )*0.98 ;

x = [axismin axismax];
y = [axismin axismax];


stats = regstats(YData,XData  ,'linear',{'rsquare','mse','beta','r','yhat'});
fprintf(1, ' y = %f x + %f  \n', stats.beta(2),stats.beta(1))
fprintf(1, ' r^2 = %f,  rmse = %f  \n', stats.rsquare, rmse(YData,XData))

%figure
plot(XData,YData, '.k','MarkerFaceColor','k');
hold on;
plot(x, y,'k--' ,x,x.*stats.beta(2)+ stats.beta(1),'k','MarkerSize',4);


xlabel(axislabels{1},'FontSize',14)
ylabel(axislabels{2},'FontSize',14)

axis square;
axis( [axismin, axismax, axismin, axismax] );



% Create textbox
annotation(figure1,'textbox',...
    [0.232809315323376 0.708415841584158 0.254409936013522 0.170418416566939],...
    'String',{['RMSE = ', num2str(rmse(YData,XData),6) ,'  ', 'R^2 = ', num2str(stats.rsquare,6) ]},'FontWeight','bold',...
    'FitBoxToText','off','LineStyle','none');



OK = 1;

end


