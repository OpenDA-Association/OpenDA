function [year, month, day, hour, minute, second] = unixsecs2date(secs)
%UNIXSECS2DATE Number of seconds since 00:00:00 1 January 1970 to date.
%
%   [YEAR, MONTH, DAY, HOUR, MINUTE, SECOND] = UNIXSECS2DATE(SECS) returns
%   the Gregorian calendar date (year, month, day, hour, minute, and second)
%   corresponding to given number of seconds since 00:00:00 1 January 1970.
%
%   Any missing MONTH or DAY will be replaced by ones.  Any missing HOUR,
%   MINUTE or SECOND will be replaced by zeros.  If no date is specified,
%   the current date and time is used.
%
%   In UNIX, the smallest time unit is a signed 32-bit integer counting the
%   number of seconds since 00:00:00 1 January 1970.  The range is from
%   1901-12-13 20:45:52, when the number of seconds is 2^31-1, to 2038-01-19
%   03:14:07, when the number of seconds is 2^31.
%
%   This function is compatible but the number of seconds is not limited to
%   a 32-bit integer, any MATLAB double precision number may be used.  Also,
%   fractional seconds are allowed.
%
%   See also DATE2UNIXSECS.

%   Author:      Peter J. Acklam
%   Time-stamp:  2003-01-14 21:32:11 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   error(nargchk(1, 1, nargin));

   [year, month, day, hour, minute, second] ...
      = jd2date(secs / 86400 + date2jd(1970, 1, 1));
