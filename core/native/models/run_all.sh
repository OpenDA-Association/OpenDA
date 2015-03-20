#!/bin/sh
costawb coffee_advec1d.xml | tee all.out
costawb coffee_heat.xml | tee -a all.out
costawb coffee_lorenz.xml | tee -a all.out
costawb coffee_lorenz96.xml | tee -a all.out
costawb coffee_oscill.xml | tee -a all.out

costawb ens_advec1d.xml | tee -a all.out
costawb ens_heat.xml | tee -a all.out
costawb ens_lorenz.xml | tee -a all.out
costawb ens_lorenz96.xml | tee -a all.out
costawb ens_oscill.xml | tee -a all.out

costawb ensrf_advec1d.xml | tee -a all.out
costawb ensrf_heat.xml | tee -a all.out
costawb ensrf_lorenz.xml | tee -a all.out
costawb ensrf_lorenz96.xml | tee -a all.out
costawb ensrf_oscill.xml | tee -a all.out

costawb rrsqrt_advec1d.xml | tee -a all.out
costawb rrsqrt_heat.xml | tee -a all.out
costawb rrsqrt_lorenz.xml | tee -a all.out
costawb rrsqrt_lorenz96.xml | tee -a all.out
costawb rrsqrt_oscill.xml | tee -a all.out

costawb conjugrad_oscill.xml | tee -a all.out
costawb conjugrad_lorenz.xml | tee -a all.out
costawb conjugrad_lorenz96.xml | tee -a all.out

costawb lbfgs_oscill.xml | tee -a all.out
costawb lbfgs_lorenz.xml | tee -a all.out
costawb lbfgs_lorenz96.xml | tee -a all.out


costawb simplex_oscill.xml | tee -a all.out
costawb simplex_lorenz96.xml | tee -a all.out
costawb simplex_lorenz.xml | tee -a all.out



