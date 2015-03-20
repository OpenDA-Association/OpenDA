function [year, month, day, hour, minute, second] = jd2jdate(jday)
%JD2JDATE Julian day number to Julian calendar date.
%
%   [YEAR, MONTH, DAY, HOUR, MINUTE, SECOND] = JD2JDATE(JDAY) returns the
%   Julian calendar date (year, month, day, hour, minute, and second)
%   corresponding to the Julian day number JDAY.
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

   ijday = floor(jday);                 % integer part
   fjday = jday - ijday;                % fraction part

   second = 86400 * fjday;
   hour   = floor(second / 3600);         % get number of hours
   second = second - 3600 * hour;         % remove the hours
   minute = floor(second / 60);           % get number of minutes
   second = second - 60 * minute;         % remove the minutes
   hour   = hour + 12;                  % Julian days start at noon

   % The following algorithm is from the Calendar FAQ.

   b = 0;
   c = ijday + 32082;

   d = floor((4 * c + 3) / 1461);
   e = c - floor((1461 * d) / 4);
   m = floor((5 * e + 2) / 153);

   day   = e - floor((153 * m + 2) / 5) + 1;
   month = m + 3 - 12 * floor(m / 10);
   year  = b * 100 + d - 4800 + floor(m / 10);
