function etkf_cmp_decomp()

N = 100; m = 1000; 
%S = randn(m,N); 
S = randnvec(m,N,10); 

whos S
fprintf('diagonal R\n')
% trival R
R = speye(m,m);

etkf_cmp(S,R)

fprintf('non-diagonal R\n')
% non-trival R
tmp = randn(m,m);
R = tmp*tmp'/m;

etkf_cmp(S,R)


function etkf_cmp(S,R)

N = size(S,2);

%sqrtR = sqrtm(R);
sqrtR = chol(R)';

Stilde = 1/sqrt(N-1)* (sqrtR \ S);

[U,Sigma,V] = svd(Stilde','econ'); 
%whos U Sigma V
T = U * (diag(1./sqrt(1 + diag(Sigma*Sigma'))) * U');

invTTt = eye(N) + Stilde' * Stilde;



fprintf('SVD decomposition of Stilde         %g\n',...
       norm(T'*(invTTt*T) - eye(N),'fro'))


invTTt2 = eye(N) + 1/(N-1) * (S' * (R \ S));

%norm(invTTt-invTTt2,'fro')

%[U2,Lambda2] = eig(invTTt2);
[U2,Lambda2] = eig(invTTt);
T2 = U2 * diag((1./sqrt(diag(Lambda2)))) * U2';




fprintf('Eigenvector decomposition of invTTt %g\n',...
       norm(T2'*(invTTt*T2) - eye(N),'fro'))
