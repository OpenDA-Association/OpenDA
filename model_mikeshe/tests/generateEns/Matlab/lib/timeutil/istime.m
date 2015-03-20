function t = istime(varargin)
%ISTIME True for valid times.
%
%   ISTIME(HOUR, MINUTE, SECOND) returns 1 if input is a valid
%   hour-minute-second triple and 0 otherwise.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:51:42 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   nargsin = nargin;
   error(nargchk(1, 3, nargsin));
   argv = {0 0 0};
   argv(1:nargsin) = varargin;
   [hour, minute, second] = deal(argv{:});

   % Hour is an integer, 0 <= hour <= 24.  Minute is an integer, 0 <= minute
   % < 60, with one exception: when hour is 24, minute must be 0.  Second is
   % a real number, 0 <= second < 60, with two exceptions: firstly, when
   % hour is 24, second must be 0; secondly, when hour is 23 and minute is
   % 59, second may be 0 <= second < 61 (to allow for positive leap
   % seconds).  A positive leap second is introduced by letting the last
   % minute of the last hour (of the last day of the month) have 61 seconds.
   % I.e., 23:59:60 <= leap second < 23:59:61 = 00:00:00 the following day.

   t =   ~imag(hour) & ~imag(minute) & ~imag(second)     ...  % real
       & hour == round(hour) & minute == round(minute)   ...  % integers
       & 0 <= hour & 0 <= minute & 0 <= second           ...  % positive
       & (   ( hour <= 23 & minute <= 59 & second < 60 ) ...  % most times
           | ( hour == 23 & minute == 59 & second < 61 ) ...  % allow leap sec
           | ( hour == 24 & minute ==  0 & second == 0 ) ...  % midnight
         );
