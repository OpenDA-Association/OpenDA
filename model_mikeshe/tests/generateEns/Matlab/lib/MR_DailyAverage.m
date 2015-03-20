function [dataAvg , timeAvg ] = MR_DailyAverage(data, time)
%% MR_TimeAverage MR_DailyAverage time averaged data for the given time series (data and time)
% where the data a will be averaged over the day

% INPUTS
%   data       : data array
%   time       : time array (same length as data)


% Compare inputs
if ~all(length(data)==length(time)); error 'data and time must be the same size'; end

% Check for NaN
tmp = ~or(isnan(data),isnan(time));
data = data(tmp);
time = time(tmp);

timev = datevec(time);

% Group by day
[unDates, ~, subs] = unique(timev(:,1:3),'rows');
timeAvg = datenum(unDates);

% Accumulate by day
dataAvg = accumarray(subs, data, [], @mean);

%[unDates accumarray(subs, data, [], @mean)]

% Similarly by hour
%[unHours, ~, subs] = unique(timev(:,4:5),'rows');
%[unHours accumarray(subs, x, [], @mean)]




end