function jd = jdate2jd(varargin)
%JDATE2JD Julian day number from Julian date.
%
%   JDATE2JD(YEAR, MONTH, DAY, HOUR, MINUTE, SECOND) returns the Julian day
%   number of the given date (Julian calendar) plus a fractional part
%   depending on the time of day.
%
%   Any missing MONTH or DAY will be replaced by ones.  Any missing HOUR,
%   MINUTE or SECOND will be replaced by zeros.
%
%   If no date is specified, the current date and time is used.
%
%   Start of the JD (Julian day) count is from 0 at 12 noon 1 JAN -4712
%   (4713 BC), Julian proleptic calendar.  Note that this day count conforms
%   with the astronomical convention starting the day at noon, in contrast
%   with the civil practice where the day starts with midnight.
%
%   Astronomers have used the Julian period to assign a unique number to
%   every day since 1 January 4713 BC.  This is the so-called Julian Day
%   (JD).  JD 0 designates the 24 hours from noon UTC on 1 January 4713 BC
%   (Julian calendar) to noon UTC on 2 January 4713 BC.

%   Sources:  - http://tycho.usno.navy.mil/mjd.html
%             - The Calendar FAQ (http://www.faqs.org)

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-05-24 13:31:03 +0200
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

   % The following algorithm is from the Calendar FAQ.  The one in the
   % Calendar FAQ is correct back to, and including, -4800-03-01 Julian
   % proleptic calendar.  The algorithm below is correct for all dates in
   % the Julian proleptic calendar.

   a = floor((14 - month)/12);
   y = year + 4800 - a;
   m = month + 12*a - 3;

   jd = day + floor((153*m + 2)/5) + y*365 + floor(y/4) - 32083 ...
        + ( second + 60*minute + 3600*(hour - 12) )/86400;
