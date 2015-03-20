function out = isleap(year)

% Script to assess if a given year (in the Gregorian Calendar system) is a leap year or not.
% It returns 0 if it is not a leap year, 
% it returns 1 if it is a leap year.
% 
% Isaac M. M., Trieste(Italy). June 8, 2011 @16h05:37
% Istituto Nazionale di Oceanografia e di Geofisica Sperimentale
% Trieste, ITALY.
%
% Developed under Matlab(TM) 7.1.0.183 (R14) Service Pack 3
% Last modified on June 8, 2011 @16h05:37

out = ( (mod(year,4 ) == 0 ) | (mod(year,400) == 0) ) & ~( (mod(year,100) == 0) & ~(mod(year,400) == 0) );

% This is the pseudo-code applied:
%
%year = year(:);
%
%if mod(year,400) == 0
%   out = 1;
%else
%   if mod(year,100) == 0
%      out = 0;
%   else
%      if mod(year,4) == 0
%         out = 1;
%      else
%         out = 0;
%      end;
%   end;
%end;
