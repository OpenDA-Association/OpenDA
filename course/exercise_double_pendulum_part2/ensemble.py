# -*- coding: utf-8 -*-
"""
use as:
load ensemble
load simulation_ensemble_results as res ###adapt to your output filename
(t,ens)=ensemble.reshape_ensemble(res)

Created on Fri Jul 17 14:26:09 2015

@author: verlaanm
"""
import numpy as np

def reshape_ensemble(ens):
    ''' (t,ens)=ensemble.reshape_ensemble(ens)'''
    t=ens.analysis_time
    all_vars=dir(ens)
    for i in range(10000):
       try:
           all_vars.index("xi_f_"+str(i))
       except ValueError:
           break
    ensemble_size=i
    if(ensemble_size>0):
        dims=np.shape(ens.xi_f_0)
        result=np.zeros((dims[0],dims[1],ensemble_size))
        for i in range(ensemble_size):
            result[:,:,i]=eval("ens.xi_f_"+str(i))
    else:
        raise Exception("No ensembles found in result file.")
    return (t,result)



if __name__ == '__main__':
    #only used for testing
    import simulation_ensemble_results as res
    (t,ens)=reshape_ensemble(res)
    print "size="+str(np.shape(ens))