import matplotlib.pyplot as plt
import numpy as np
import enkf25_results as enkf
import enkf25_loc_results as enkf_loc


def compute_b(a):
    n=len(a)
    b= np.zeros(n)
    for i in range(1,n-1):
        b[i]=5.0*(a[i+1]-a[i-1])
    b[n-1] = 5.0*(a[0]-a[n-2])
    b[0] = 5.0*(a[1]-a[n-1])

    return b


def main():

    #t=300 -> index 59
    x=enkf.x_f[0]

    a=enkf.x_f[0][0:1000]
    b=np.array(enkf.x_f[0][1000:2000])

    a_loc=enkf_loc.x_f[59][0:1000]
    b_loc=enkf_loc.x_f[59][1000:2000]

    b_check=compute_b(a)
    b_loc_check=compute_b(a_loc)

    plt.figure()
    plt.plot(b-b_check,'r')
    plt.plot(b_loc-b_loc_check,'b')

    plt.legend(["25 EnKF", "25 EnKF, localization"])
    #plt.plot(b_loc_check-b_loc,'b')
    plt.show()





if __name__ == "__main__":
    main()