function days = hms2days(HOUR, MINUTE, SECOND)
%HMS2DAYS Convert hours, minutes, and seconds to days.
%
%   DAYS = HMS2DAYS(HOUR, MINUTE, SECOND) converts the number of hours,
%   minutes, and seconds to a number of days.
%
%   The following holds (to within rounding precision):
%
%     DAYS = HOUR / 24 + MINUTE / (24 * 60) + SECOND / (24 * 60 * 60)
%          = (HOUR + (MINUTE + SECOND / 60) / 60) / 24

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-05-24 15:23:31 +0200
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   error(nargchk(1, 3, nargin));
   argv = {0 0 0};
   argv(1:nargsin) = varargin;
   [hour, minute, second] = deal(argv{:});

   days = (hour + (minute + second / 60) / 60) / 24;
