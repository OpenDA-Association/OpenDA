import matplotlib.pyplot as plt
import numpy as np
import enkf50_results as enkf
import true_state


def main():
    print (len(true_state.b_true_150));
    plt.figure()


    a=enkf.x_f[0][0:999]
    b=enkf.x_f[0][1000:1999]
    plt.subplot(3,2,1)
    plt.plot(a)
    plt.plot(true_state.a_true_0,'r')

    plt.subplot(3,2,2)
    plt.plot(b)
    plt.plot(true_state.b_true_0,'r')


    a=enkf.x_f[29][0:999]
    b=enkf.x_f[29][1000:1999]
    plt.subplot(3,2,3)
    plt.plot(a)
    plt.plot(true_state.a_true_150,'r')

    plt.subplot(3,2,4)
    plt.plot(b)
    plt.plot(true_state.b_true_150,'r')

    a=enkf.x_f[59][0:999]
    b=enkf.x_f[59][1000:1999]
    plt.subplot(3,2,5)
    plt.plot(a)
    plt.plot(true_state.a_true_300,'r')
    plt.subplot(3,2,6)
    plt.plot(b)
    plt.plot(true_state.b_true_300,'r')


    plt.show()








#    plaatje_timeseries()

if __name__ == "__main__":
    main()