function y = yearnum(varargin)
%YEARNUM Ordinal year number.
%
%   YEARNUM(YEAR, MONTH, DAY, HOUR, MIN, SEC) returns the ordinal year
%   number plus a fractional part depending on the month, day, and time of
%   day.
%
%   Any missing MONTH or DAY will be replaced by ones.  Any missing HOUR,
%   MINUTE or SECOND will be replaced by zeros.
%
%   If no date is specified, the current date and time is used.  Gregorian
%   calendar is assumed.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:49:10 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   nargsin = nargin;
   error(nargchk(0, 6, nargsin));
   if nargsin
      argv = {1 1 1 0 0 0};
      argv(1:nargsin) = varargin;
   else
      argv = num2cell(clock);
   end
   [year, month, day, hour, minute, second] = deal(argv{:});

   year + (dayofyear(year, month, day, hour, minute, second) - 1) ...
          ./ daysinyear(year);
