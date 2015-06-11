function H = householder(w)

n = length(w)-1;

H = zeros(n+1,n);

w2 = w;
w2(n+1) = w2(n+1) + sign(w(n+1));


H = [eye(n); zeros(1,n)] - 1/(abs(w(end))+1) *  w2 * w(1:end-1)';

