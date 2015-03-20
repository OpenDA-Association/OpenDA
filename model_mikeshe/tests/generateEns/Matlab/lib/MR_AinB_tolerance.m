function[AA, BB, idx] = MR_AinB_tolerance(A, B, Aidx, Bidx, tolerance)
%MR_AinB_tolerance Takes two Data series ( A and B ) each with a 'Time axis' and 'Data'
%and returns a truncated data series where the time indices overlap with given tolerance.
%One data series corresponds to another data series. Note that there is
%no interpolation or resampling.

% Remove nan from the time series.
Anan = find(isnan(A));
A(Anan) = [];
Aidx(Anan) = [];

Bnan = find(isnan(B));
B(Bnan) = [];
Bidx(Bnan) = [];

AA = [];
BB = [];
idx = [];

% finds the set intersection.
idxCount = 1;
for i = 1: length(A)
    f = abs(Bidx - Aidx(i)); 
    [fvalue,findex] = min(f);
    % if within tolerance, then keep.
    if fvalue < tolerance
       AA(idxCount) = A(i);
       BB(idxCount) = B(findex);
       idx(idxCount) = Aidx(i);
       idxCount = idxCount + 1;
    end
   
end

AA = AA';
BB = BB';
idx = idx';


