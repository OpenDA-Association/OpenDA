function generate_obs(indx, step, stdev)
% function generate_obs(indx, step, stdev)
%
% Generate observations file (obs.cvs) for the locations in array indx
%
% The model results are taken from the file simulation_results.m
%
% indx: indices in state vector to create observations for
% step: interval of available times to create observations for 
%
% author: Nils van Velzen
%
   simulation_results
  
   n=length(x{1});
   nt = length(model_time);
   X=[x{:}];
   X=reshape(X,n,nt);
   Obs=X(indx,:);
   indx2=step:step:nt;
   Obs=Obs(:,indx2);
   model_time=[model_time{:}];
   tObs=model_time(indx2);

   FID=fopen('obs.cvs','w');

   plot(tObs, Obs);

   fprintf(FID,'time,index,value,std\n');
   for i=1:length(tObs)
      for j=1:size(Obs,1)
         fprintf(FID,'%f,%f,%f,%f\n',tObs(i),indx(j),Obs(j,i),stdev);
      end
   end

   fclose(FID);

