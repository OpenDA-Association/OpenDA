function [hour, minute, second] = days2hms(days)
%DAYS2HMS Convert days into hours, minutes, and seconds.
%
%   [HOUR, MINUTE, SECOND] = DAYS2HMS(DAYS) converts the number of days to
%   hours, minutes, and seconds.
%
%   The following holds (to within rounding precision):
%
%     DAYS = HOUR / 24 + MINUTE / (24 * 60) + SECOND / (24 * 60 * 60)
%          = (HOUR + (MINUTE + SECOND / 60) / 60) / 24

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:52:02 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   error(nargchk(1, 1, nargin));

   second = 86400 * days;
   hour   = fix(second/3600);           % get number of hours
   second = second - 3600*hour;         % remove the hours
   minute = fix(second/60);             % get number of minutes
   second = second - 60*minute;         % remove the minutes
