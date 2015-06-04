[t,ens]=load_ensemble('enkf_results');
[tobs,obs]=load_obs('enkf_results');
ens1=reshape(ens(1,:,:),size(ens,2),size(ens,3));
ensm=mean(ens1,2);
plot(t,ensm)
hold on;
plot(tobs,obs,'r*');
hold off


