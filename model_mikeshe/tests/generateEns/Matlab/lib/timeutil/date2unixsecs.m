function secs = date2unixsecs(varargin)
%DATE2UNIXSECS Number of seconds since 00:00:00 1 January 1970.
%
%   DATE2UNIXSECS(YEAR, MONTH, DAY, HOUR, MINUTE, SECOND) returns the number
%   of seconds since 00:00:00 1 January 1970.
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

%   Author:      Peter J. Acklam
%   Time-stamp:  2003-06-22 19:38:22 +0200
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   nargsin = nargin;
   error(nargchk(0, 6, nargsin));
   if nargsin
      argv = { 1 1 1 0 0 0 };
      argv(1:nargsin) = varargin;
   else
      argv = num2cell(clock);
   end
   [year, month, day, hour, minute, second] = deal(argv{:});

   secs = 86400 * (date2jd(year,month,day,hour,minute,second) - date2jd(1970, 1, 1));
