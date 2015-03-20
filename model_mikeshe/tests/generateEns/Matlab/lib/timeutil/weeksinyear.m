function n = weeksinyear(year)
%WEEKSINYEAR Number of weeks in a year.
%
%   WEEKSINYEAR(YEAR) returns the number of weeks in the given year.
%
%   The number of weeks is always either 52 or 53.
%
%   If no year is specified, the current year is used.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:49:04 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   if nargin < 1
      clk = clock;
      year = clk(1);
   end

   n = ( 365 + isleapyear(year) ...
         + dayofweek(year, 1, 4) - dayofweek(year+1, 1, 4) )/7;
