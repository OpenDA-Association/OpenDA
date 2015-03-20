function sec = hms2sec(hour, minute, second)
%HMS2SEC  Convert from hours, minutes and seconds to seconds.
%
%   HMS2SEC(HOUR, MINUTE, SECOND) converts the number of hours, minutes and
%   seconds to seconds.

%   Author:      Peter J. Acklam
%   Time-stamp:  2003-06-22 19:22:49 +0200
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   sec = second + 60 * minute + 3600 * hour;
