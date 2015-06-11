% r = rms(x,y,norm)
% returns rms between x and y (taking norm into account if present)

% Alexander Barth
function r = rms(x,y,norm)

d = x-y;

if nargin == 3
  d = d .* sqrt(norm);
end

m = ~isnan(d);
r = mean(d(m).^2);

if nargin == 3
  r = r/mean(norm(m));
end

r = sqrt(r);