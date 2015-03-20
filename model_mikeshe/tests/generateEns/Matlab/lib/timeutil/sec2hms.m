function [hour, minute, second] = sec2hms(sec)
%SEC2HMS  Convert seconds to hours, minutes and seconds.
%
%   [HOUR, MINUTE, SECOND] = SEC2HMS(SEC) converts the number of seconds in
%   SEC into hours, minutes and seconds.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:50:09 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   hour   = fix(sec/3600);      % get number of hours
   sec    = sec - 3600*hour;    % remove the hours
   minute = fix(sec/60);        % get number of minutes
   sec    = sec - 60*minute;    % remove the minutes
   second = sec;
