% fun = sangoma_compact_locfun(len,r)
% 
% Smooth compact localization function described in Gaspari et al. (1999), 
% equation 4.10.
%
% Input:
%   len: correlation length-scale (scalar)
%   r: matrix of distance
% Output:
%   fun: array of weights (same size as r). fun is zero if r > 2 len
%   
% Reference:
% @article {QJ:QJ49712555417,
% author = {Gaspari, Gregory and Cohn, Stephen E.},
% title = {Construction of correlation functions in two and three dimensions},
% journal = {Quarterly Journal of the Royal Meteorological Society},
% volume = {125},
% number = {554},
% publisher = {John Wiley & Sons, Ltd},
% issn = {1477-870X},
% url = {http://dx.doi.org/10.1002/qj.49712555417},
% doi = {10.1002/qj.49712555417},
% pages = {723--757},
% keywords = {Compactly supported, Convolution, Correlation functions, Data assimilation, Space-limited},
% year = {1999},
% }

function fun = sangoma_compact_locfun(len,r)

r = r/len;
fun = zeros(size(r));


i = r <= 1;
ri = r(i);

fun(i) = (((-ri/4 + 1/2) .* ri + 5/8) .* ri - 5/3) .* ri.^2 + 1;

i = 1 < r & r  <= 2;
ri = r(i);

fun(i) = ((((ri/12 - 1/2) .* ri + 5/8) .* ri + 5/3) .* ri - 5) .* ri + 4 ...
         - 2./(3*ri);


