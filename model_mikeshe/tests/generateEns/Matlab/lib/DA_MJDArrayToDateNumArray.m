function [ dnArray ] = DA_MJDArrayToDateNumArray( MJDArray )
%DA_MJDArrayToDateNumArray Converts an array of Modified Julian Dates to the Matlab DateNum
%format.
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com) 2012
%
%   Inputs: 
%       - Array of length (t) of Modified Julian dates.
%   Outputs:
%       - DateNum time vector (tnew)
%
%   Uses library: 
%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:52:13 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam
%

lgth = length(MJDArray);
dnArray = zeros(lgth,1);
for i=1:lgth
    [year, month, day, hour, minute, second] = mjd2date( MJDArray(i) );
    dnArray(i) = datenum( year,month,day,hour,minute,second);

end
end

