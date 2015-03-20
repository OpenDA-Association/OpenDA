function [year, month, day, hour, minute, second] = jd2date(jd)
%JD2DATE Gregorian calendar date from modified Julian day number.
%
%   [YEAR, MONTH, DAY, HOUR, MINUTE, SECOND] = JD2DATE(JD) returns the
%   Gregorian calendar date (year, month, day, hour, minute, and second)
%   corresponding to the Julian day number JD.
%
%   Start of the JD (Julian day) count is from 0 at 12 noon 1 JAN -4712
%   (4713 BC), Julian proleptic calendar.  Note that this day count conforms
%   with the astronomical convention starting the day at noon, in contrast
%   with the civil practice where the day starts with midnight.
%
%   Astronomers have used the Julian period to assign a unique number to
%   every day since 1 January 4713 BC.  This is the so-called Julian Day
%   (JD). JD 0 designates the 24 hours from noon UTC on 1 January 4713 BC
%   (Julian calendar) to noon UTC on 2 January 4713 BC.

%   Sources:  - http://tycho.usno.navy.mil/mjd.html
%             - The Calendar FAQ (http://www.faqs.org)

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-05-24 15:24:45 +0200
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   nargsin = nargin;
   error(nargchk(1, 1, nargsin));

   % Adding 0.5 to JD and taking FLOOR ensures that the date is correct.
   % Here are some sample values:
   %
   %  MJD     Date       Time
   %  -1.00 = 1858-11-16 00:00 (not 1858-11-15 24:00!)
   %  -0.75 = 1858-11-16 06:00
   %  -0.50 = 1858-11-16 12:00
   %  -0.25 = 1858-11-16 18:00
   %   0.00 = 1858-11-17 00:00 (not 1858-11-16 24:00!)
   %  +0.25 = 1858-11-17 06:00
   %  +0.50 = 1858-11-17 12:00
   %  +0.75 = 1858-11-17 18:00
   %  +1.00 = 1858-11-18 00:00 (not 1858-11-17 24:00!)

   ijd = floor(jd + 0.5);               % integer part

   if nargout > 3
      fjd = jd - ijd + 0.5;             % fraction part
      [hour, minute, second] = days2hms(fjd);
   end

   % The following algorithm is from the Calendar FAQ.

   a = ijd + 32044;
   b = floor((4 * a + 3) / 146097);
   c = a - floor((b * 146097) / 4);

   d = floor((4 * c + 3) / 1461);
   e = c - floor((1461 * d) / 4);
   m = floor((5 * e + 2) / 153);

   day   = e - floor((153 * m + 2) / 5) + 1;
   month = m + 3 - 12 * floor(m / 10);
   year  = b * 100 + d - 4800 + floor(m / 10);
