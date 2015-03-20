function t = isleapyear(year)
%ISLEAPYEAR True for leap years.
%
%   ISLEAPYEAR(YEAR) returns 1's for the elements of YEAR that are leap
%   years and 0's for those that are not.  If YEAR is omitted, the current
%   year is used.  Gregorian calendar is assumed.
%
%   A year is a leap year if the following returns true
%
%       ( ~rem(year, 4) & rem(year, 100) ) | ~rem(year, 400)
%
%   A year is not a leap year if the following returns true
%
%      rem(year, 4) | ( ~rem(year, 100) & rem(year, 400) )

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:51:45 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   error(nargchk(0, 1, nargin));

   if nargin == 0               % If no input argument...
      clk = clock;              % ...get current date and time...
      year = clk(1);            % ...and extract year.
   end

   t = ( ~rem(year, 4) & rem(year, 100) ) | ~rem(year, 400);
