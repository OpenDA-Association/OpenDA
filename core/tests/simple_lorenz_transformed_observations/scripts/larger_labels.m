function larger_labels
%function larger_labels
% purpose : enlarge all labels and text for producing figures for slides etc.
% author  : Martin Verlaan
% log     : 20030321 start

%new settings
text_fontsize     = 20;
axes_linewidth    = 2;
axes_fontsize     = 20;
lines_linewidth   = 2;
lines_markersize  = 20;
label_fontsize    = 20;


%for current figure
fig=gcf;

%look for all text
h_text = findobj(fig,'type','text');
for h=h_text,
   %change fontsize for texts
   set(h,'fontsize',text_fontsize);
end;

%for all axes
h_axes = findobj(fig,'type','axes');
for h=h_axes,
   %change line thicknes
   set(h,'linewidth',axes_linewidth);
   %change fontsize
   set(h,'fontsize',axes_fontsize);
   %change fontsize of the labels
   h_xlabel = get(h,'xlabel');
   set(h_xlabel,'fontsize',label_fontsize);
   h_ylabel = get(h,'ylabel');
   set(h_ylabel,'fontsize',label_fontsize);
   h_zlabel = get(h,'zlabel');
   set(h_zlabel,'fontsize',label_fontsize);
end;

%for all lines
h_lines = findobj(fig,'type','line');
for h=h_lines,
   %change thickness of the lines
   set(h,'linewidth',lines_linewidth);
   %change size of the markers
   set(h,'markersize',lines_markersize);
end;
