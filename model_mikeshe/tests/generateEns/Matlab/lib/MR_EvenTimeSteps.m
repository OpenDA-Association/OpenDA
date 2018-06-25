function [datenumVec , dataVec ] = MR_EvenTimeSteps(tVec, DataMatrix, dtSec)
%MR_EvenTimeSteps Takes a time series with uneven dt - and resamples it to a given
%even dt.
%
% Created,  Marc-Etienne Ridler  (mer@dhigroup.com) 2012
%
%   Inputs: 
%       - time axis vector of length (t) in Datenum, Modified Julian or Julian
%       - A Matrix of data of size --> number of time series (n) x length (t)
%       - The desired dt spacing in Seconds
%   Outputs:
%       - time vector (tnew)
%       - the data of (n x tnew)

tts = timeseries(DataMatrix,tVec);
dtDays = dtSec / (24*60*60);

datenumVec = [tVec(1): dtDays: tVec(end)];

res_ts=resample(tts,datenumVec);
dataVec = res_ts.Data;

end