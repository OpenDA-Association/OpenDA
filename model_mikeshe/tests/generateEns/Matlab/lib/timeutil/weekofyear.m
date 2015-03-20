function [w, y] = weekofyear(varargin)
%WEEKOFYEAR Ordinal week number.
%
%   WEEKOFYEAR(YEAR, MONTH, DAY, HOUR, MINUTE, SECOND) returns the
%   ordinal week number in the given year plus a fractional part
%   depending on the day and time of day.
%
%   [WEEK, YEAR] = WEEKOFYEAR(...) also returns the year the week belongs
%   to.
%
%   The week number is an integer between 1 and 53, inclusive.
%
%   Any missing MONTH or DAY will be replaced by ones.  Any missing HOUR,
%   MINUTE or SECOND will be replaced by zeros.  If no date is specified,
%   the current date and time is used.
%
%   This function is ISO 8601 compliant:  The first week of a given year is
%   the first week which has more days in the given year than in the
%   previous year.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:49:15 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   nargsin = nargin;
   error(nargchk(0, 6, nargsin));
   if nargsin
      argv = { 1 1 1 0 0 0 };
      argv(1:nargsin) = varargin;
   else
      argv = num2cell(clock);
   end
   [year, month, day, hour, minute, second] = deal(argv{:});

   % ISO 8601 states that a week that lies partly in one year and partly
   % in another is assigned a number in the year in which most of its
   % days lie.  Consequence: Week 1 of any year is the week that
   % contains 4 January.  Hence, the first week started on the following
   % day of the year
   %
   %    yd1 =  4: week 1 started on 4 January of current year
   %    yd1 =  3: week 1 started on 3 January of current year
   %    yd1 =  2: week 1 started on 2 January of current year
   %    yd1 =  1: week 1 started on 1 January of current year
   %    yd1 =  0: week 1 started on 31 December of previous year
   %    yd1 = -1: week 1 started on 30 December of previous year
   %    yd1 = -2: week 1 started on 29 December of previous year
   %
   yd1 = 5 - dayofweek(year, 1, 4);

   % Get the ordinal day number and calculate the "raw" week number.
   yd2 = dayofyear(year, month, day);
   w = 1 + floor( (yd2 - yd1)/7 );

   % Now, if the week number is 0, the week is either week 52 or 53 of
   % the previous year.  If the week number is 53, the week is either
   % week 53 of the current year or week 1 of the next year.

   y = year;
   if length(y) == 1, y = y(ones(size(w))); end;     % scalar expansion

   % Take care of the case when the week number is 0.
   i = find(w == 0);
   if ~isempty(i)
      y(i) = y(i) - 1;
      w(i) = weeksinyear(y(i));
   end

   % Take care of the case when the week number is 53.
   i = find(w == 53);
   if ~isempty(i)
      j = find(weeksinyear(y(i)) == 52)
      if any(j(:))
         y(i(j)) = y(i(j)) + 1;
         w(i(j)) = 1;
      end
   end

   % Now add the fractional part of the day.
   w = w + ( 86400*( dayofweek(year, month, day) - 1 ) ...
             + 3600*hour + 60*minute + second )/604800;
