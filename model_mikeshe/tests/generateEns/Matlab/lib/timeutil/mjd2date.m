function [year, month, day, hour, minute, second] = mjd2date(mjd)
%MJD2DATE Gregorian calendar date from Julian day number.
%
%   [YEAR, MONTH, DAY, HOUR, MINUTE, SECOND] = MJD2DATE(MJD) returns the
%   Gregorian calendar date (year, month, day, hour, minute, and second)
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
%   Time-stamp:  2002-03-03 12:50:30 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   nargsin = nargin;
   error(nargchk(1, 1, nargsin));

   % We could have got everything by just using
   %
   %   jd = mjd2jd(mjd);
   %   [year, month, day, hour, minute, second] = jd2date(jd);
   %
   % but we lose precision in the fraction part when MJD is converted to JD
   % because of the large offset (2400000.5) between JD and MJD.

   jd = mjd2jd(mjd);
   [year, month, day] = jd2date(jd);

   if nargout > 3
      fmjd = mjd - floor(mjd);
      [hour, minute, second] = days2hms(fmjd);
   end
