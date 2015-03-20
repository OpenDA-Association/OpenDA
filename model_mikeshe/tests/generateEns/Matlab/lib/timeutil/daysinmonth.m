function dim = daysinmonth(varargin)
%DAYSINMONTH Number of days in a month.
%
%   DAYSINMONTH(YEAR, MONTH) returns the number of days in the given month.
%
%   If no date is specified, the current date is used.  Gregorian calendar
%   is assumed.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:52:00 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   nargsin = nargin;
   error(nargchk(0, 2, nargsin));
   if nargsin
      argv = {1 1};
      argv(1:nargsin) = varargin;
   else
      argv = clock;
      argv = num2cell(argv(1:2));
   end
   [year, month] = deal(argv{:});

   % Now get the number of days in the month.
   days = [31 28 31 30 31 30 31 31 30 31 30 31];
   dim  = days(month);

   % Add leap day as necessary.
   dim = dim + ( month == 2 & isleapyear(year) );
