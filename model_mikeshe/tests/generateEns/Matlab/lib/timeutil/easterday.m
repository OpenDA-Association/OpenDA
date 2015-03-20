function [month, day] = easterday(year)
%EASTERDAY Easter day for a given year.
%
%   [MONTH, DAY] = EASTERDAY(YEAR) returns the month and day for easter day.
%
%   If no year is specified, the current year is used.  Gregorian calendar
%   is assumed.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-05-24 15:26:50 +0200
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   nargsin = nargin;
   error(nargchk(0, 1, nargsin));
   if ~nargsin
      clk = clock;
      year = clk(1);
   end

   % The following algorithm is from the Calendar FAQ,
   % http://www.tondering.dk/claus/calendar.html.

   G = rem(year, 19);

   % For the Julian calendar:
   % I = rem(19*G + 15), 30);
   % J = rem(year + year/4 + I), 7);

   % For the Gregorian calendar:
   C = fix(year/100);
   H = rem((C - fix(C/4) - fix((8*C + 13)/25) + 19*G + 15), 30);
   I = H - fix(H/28)*(1 - fix(H/28)*fix(29/(H + 1))*fix((21 - G)/11));
   J = rem((year + fix(year/4) + I + 2 - C + fix(C/4)), 7);

   L = I - J;
   month = 3 + fix((L + 40)/44);
   day = L + 28 - 31*fix(month/4);
