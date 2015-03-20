function t = isdate(varargin)
%ISDATE True for valid dates (Gregorian calendar).
%
%   ISDATE(YEAR, MONTH, DAY) returns 1 if input is a valid year-month-date
%   triple and 0 otherwise.  Gregorian calendar is assumed.
%
%   See also ISJDATE.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:51:49 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   nargsin = nargin;
   error(nargchk(1, 3, nargsin));
   argv = {1 1 1};
   argv(1:nargsin) = varargin;
   [year, month, day] = deal(argv{:});

   t =   ~imag(year) & ~imag(month) & ~imag(day) ...
       & (year == round(year)) & (month == round(month)) ...
       & (day == round(day)) & (1 <= month) & (month <= 12) & (1 <= day);

   % Since this function might be called at the beginning of other
   % time-related m-files, we make this function independent of all other
   % m-files to avoid infinite recursion.

   days = [ 31 28 31 30 31 30 31 31 30 31 30 31 ];
   is_february = month == 2;
   is_leapyear = ( ~rem(year, 4) & rem(year, 100) ) | ~rem(year, 400);
   days_in_month = days(month) + ( is_february & is_leapyear );

   t = t & ( day <= days_in_month );
