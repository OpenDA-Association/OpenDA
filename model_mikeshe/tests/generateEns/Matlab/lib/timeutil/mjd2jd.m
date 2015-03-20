function jd = mjd2jd(mjd)
%MJD2JD Modified Julian day number from Julian day number.
%
%   JD = MJD2JD(MJD) returns the Julian day number corresponding to the
%   given modified Julian day number.
%
%   See also JD2MJD, DATE2JD, DATE2MJD.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:50:25 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   error(nargchk(1, 1, nargin));

   jd = mjd + 2400000.5;
