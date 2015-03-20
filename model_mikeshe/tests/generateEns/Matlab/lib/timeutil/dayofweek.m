function dow = dayofweek(varargin)
%DAYOFWEEK Day of week.
%
%   NUM = DAYOFWEEK(YEAR, MONTH, DAY, HOUR, MINUTE, SECOND) returns the
%   ordinal day number in the given week plus a fractional part depending on
%   the time of day.  This function is ISO 8601 compliant, so Monday is day
%   1, Tuesday is day 2, ..., Sunday is day 7.
%
%   If Monday is day 0, Tuesday is day 1, ..., Sunday is day 6, use
%
%      num = dayofweek(...) - 1
%
%   If Sunday is day 1, Monday is day 2, ..., Saturday is day 7, use
%
%      num = rem(dayofweek(...), 7) + 1
%
%   If Sunday is day 0, Monday is day 1, ..., Saturday is day 6, use
%
%      num = rem(dayofweek(...), 7)
%
%   Any missing MONTH or DAY will be replaced by ones.  Any missing HOUR,
%   MINUTE or SECOND will be replaced by zeros.
%
%   If no date is specified, the current date is used.  Gregorian calendar
%   is assumed.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-05-24 14:26:48 +0200
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   error(nargchk(0, 6, nargin));
   dow = 1 + mod(date2mjd(varargin{:}) + 2, 7);
