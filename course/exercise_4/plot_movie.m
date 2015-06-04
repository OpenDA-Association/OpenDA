function plot_movie
% funciton plot_movie
%
% plot results of advection model as stored in the file simulation_results.
%
% author: Martin Verlaan and Nils van Velzen
%
   simulation_results

   nState=length(x{1});
   nGrid=(nState-1)/2;

   for i=1:1:length(x)
      yy(i)=x{i}(3);
   end
   figure(2);
   plot(yy)

   if 1
	figure(1);
   	for i=1:length(x),
      		plot(x{i}(2:nGrid+1),'b'), 
      		axis([1 nGrid+1 -1 2]);
      		pause(0.1);
   	end
   end
 
