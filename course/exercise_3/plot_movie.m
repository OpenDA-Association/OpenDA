function plot_movie
% funciton plot_movie
%
% plot results of advection model as stored in the file simulation_results.
%
% author: Martin Verlaan and Nils van Velzen
%
   simulation_results

   for i=1:length(x), 
	   plot(x{i},'b'), 
      axis([1 52 -1 2]);
	   pause(0.1);
   end
end
