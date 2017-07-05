function plot_movie(times,states,morestates);
% function plot_movie(times,states,morestates);
% 
%constants fixed in code for now
m=1.; %probably does not matter
g=9.81; %acceleration of gravity
l=0.3; %length of one limb
w=0.02; %width of the limb
p.m=m;p.g=g;p.l=l;p.w=w;

%colors
%call=0.25+0.5*rand(n,1)*[1 1 1];
n=1;call=0.25+0.5*rand(n,3);

%plotting
figure(1);clf
set(gca,'position',[0 0 1 1])

i=0;
for t = times,
   i=i+1;
   clf
   %for j=1:n,
   %   plot_pendulum(states(:,i),p,call(j,:));
   %end;
   plot_pendulum(states(:,i),p,'k');
   if(nargin>2),
      hold on
      plot_pendulum(morestates(:,i),p,'g');
      hold off
   end;
   lplot=0.6;
   axis([-lplot,lplot,-lplot,lplot]);
   %axis off
   set(gca,'xtick',[]);
   set(gca,'ytick',[]);
   title(sprintf('t=%.2f',t));
   pause(0.2); %x times slower (plus plotting)
   %figname=sprintf('frames/fig_double_pendulum_%4.4d.png',i);
   %figname=sprintf('frames/fig_ensemble_double_pendulum_%4.4d.png',i);
   %print(figname,'-dpng');
end
