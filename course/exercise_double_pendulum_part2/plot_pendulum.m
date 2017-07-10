function plot_pendulum(x,p,c);
% create a drawing of the current state of the double pendulum
% c is color as used for patch

%unpack p
g=p.g;
m=p.m;
l=p.l;
w=p.w;

x1=l*sin(x(1));
y1=-l*cos(x(1));
x2=x1+l*sin(x(2));
y2=y1-l*cos(x(2));

%plot([0,x1,x2],[0,y1,y2],c);

%top limb
dx1=0.5*w*cos(x(1));
dy1=0.5*w*sin(x(1));
px1=[dx1,-dx1,x1-dx1,x1+dx1];
py1=[dy1,-dy1,y1-dy1,y1+dy1];
patch(px1,py1,c);
dx2=0.5*w*cos(x(2));
dy2=0.5*w*sin(x(2));
px2=[x1+dx2,x1-dx2,x2-dx2,x2+dx2];
py2=[y1+dy2,y1-dy2,y2-dy2,y2+dy2];
patch(px2,py2,c);
