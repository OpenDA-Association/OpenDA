import matplotlib.pyplot as plt
import enkf50_results as enkf
import true_state


def main():
    print (len(true_state.b_true_150));
    fig=plt.figure()

    fig.suptitle("a and b (columns) at t=0,150,300 (rows)")

    a=enkf.x_f[0][0:1000]
    b=enkf.x_f[0][1000:2000]

    plt.subplot(3,2,1)
    plt.plot(a)
    plt.plot(true_state.a_true_0,'r')

    plt.subplot(3,2,2)
    plt.plot(b)
    plt.plot(true_state.b_true_0,'r')

    a=enkf.x_f[30][0:1000]
    b=enkf.x_f[30][1000:2000]
    plt.subplot(3,2,3)
    plt.plot(a)
    plt.plot(true_state.a_true_150,'r')

    plt.subplot(3,2,4)
    plt.plot(b)
    plt.plot(true_state.b_true_150,'r')

    a=enkf.x_f[60][0:1000]
    b=enkf.x_f[60][1000:2000]
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
