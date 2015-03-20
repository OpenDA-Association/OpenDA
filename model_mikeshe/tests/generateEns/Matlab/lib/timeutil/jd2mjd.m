function mjd = jd2mjd(jd)
%JD2MJD Modified Julian day number from Julian day number.
%
%   [MJD] = JD2MJD(JD) returns the modified Julian day number corresponding
%   to the given Julian day number.
%
%   See also MJD2JD, DATE2MJD, DATE2JD.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:51:32 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   error(nargchk(1, 1, nargin));

   mjd = jd - 2400000.5;
