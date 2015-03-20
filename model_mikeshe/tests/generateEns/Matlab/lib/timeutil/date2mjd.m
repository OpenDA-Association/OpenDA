function mjd = date2mjd(varargin)
%DATE2MJD Modified Julian day number.
%
%   MJD = DATE2MJD(YEAR, MONTH, DAY, HOUR, MINUTE, SECOND) returns the
%   modified Julian day number of the given date plus a fractional part
%   depending on the day and time of day.
%
%   Any missing MONTH or DAY will be replaced by ones.  Any missing HOUR,
%   MINUTE or SECOND will be replaced by zeros.
%
%   If no date is specified, the current date and time is used.  Gregorian
%   calendar is assumed.
%
%   Start of the MJD (modified Julian day) count is from 0 at 00:00 UTC 17
%   November 1858 (Gregorian calendar).

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:52:13 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   mjd = date2jd(varargin{:}) - 2400000.5;
