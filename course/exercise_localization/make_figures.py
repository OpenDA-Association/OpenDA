import matplotlib.pyplot as plt
import numpy as np

import enkf_results as enkf
import enkf_autoloc_results as autoloc
import enkf_distloc_results as distloc
import enkf_distloc_20_results as distloc20

def plaatje_ensemble():
    plt.figure()
    x=enkf.xi_f_5[10]
    plt.plot(x)
    plt.show()


def plaatje_timeseries():
    plt.figure()
    plt.subplot(321)
    plt.plot(enkf.analysis_time, enkf.pred_f)
    plt.hold(True)
    plt.plot(enkf.analysis_time, enkf.obs, '*')

    plt.subplot(322)
    diff=np.linalg.norm(enkf.obs-enkf.pred_f, axis=1)

    plt.plot(enkf.analysis_time, diff)


    #plt.subplot(222)
    #plt.plot(autoloc.analysis_time, autoloc.pred_f)
    #plt.hold(True)
    #plt.plot(autoloc.analysis_time, autoloc.obs, '*')
    #print("Done")

    plt.subplot(323)
    plt.plot(distloc.analysis_time, distloc.pred_f)
    plt.hold(True)
    plt.plot(distloc.analysis_time, distloc.obs, '*')


    plt.subplot(324)

    diff=np.linalg.norm(distloc.obs-distloc.pred_f, axis=1)
    plt.plot(distloc.analysis_time, diff)


    plt.subplot(325)
    plt.plot(distloc20.analysis_time, distloc20.pred_f)
    plt.hold(True)
    plt.plot(distloc20.analysis_time, distloc20.obs, '*')


    plt.subplot(326)

    diff=np.linalg.norm(distloc20.obs-distloc20.pred_f, axis=1)
    plt.plot(distloc.analysis_time, diff)


    plt.show()



def main():
    plaatje_timeseries()
    plaatje_ensemble()

#    plaatje_timeseries()

if __name__ == "__main__":
    main()