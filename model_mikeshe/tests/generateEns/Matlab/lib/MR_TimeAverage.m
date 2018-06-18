function [dataAvg , timeAvg ] = MR_TimeAverage(data, time, window)
%% MR_TimeAverage Calculates time averaged data for the given time series (data and time)
% where the data a will be averaged given the window in days. Therefore
% window = 1  daily aveages,  window = 1/24 is hourly.
%
% INPUTS
%   data       : data array
%   time       : time array (same length as data)
%   window     : windows over which to average.


% Compare inputs
if ~all(size(data)==size(time)); error 'data and time must be the same size'; end

% Check for NaN
tmp = ~or(isnan(data),isnan(time));
data = data(tmp);
time = time(tmp);

binEdges = time(min(time)):window:time(max(time))-window;
aj = binEdges(1:end-1);     %# bins lower edge
bj = binEdges(2:end);       %# bins upper edge
cj = ( aj + bj ) ./ 2;      %# bins center

%# assign values to bins
[~,binIdx] = histc(M, [binEdges(1:end-1) Inf]);

%# count number of values in each bin
nj = accumarray(binIdx, 1, [nbins 1], @sum);



end