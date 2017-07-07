import numpy as np
import math
import matplotlib.pyplot as plt



def generate_random_coefficients(N):
    """
    Generate the random coefficients of a single ensemble member
    :param N:
    :return:
    """

    Ak=[None]*N
    Phi=[None]*N
    for k in range(N):
        Ak[k] = np.random.uniform()
        Phi[k] = 2.0*math.pi*np.random.uniform()

    return Ak, Phi


def generate_a(NComb, N):
    """

    :param NComb: Number of combinations of geometic functions
    :param N: Lengt of domain (grid-size)
    :return: a
    """



    #Generate reference field
    Ak, Phi = generate_random_coefficients(NComb)

    a = np.zeros(N)

    pi_div_500 = (2.0 * math.pi) / 1000.0

    for i in range(N):
        for k in range(NComb):
            a[i] = a[i] + Ak[k]*np.sin(pi_div_500*k*(i+1)+ Phi[k])

    return a

def compute_b(a):
    n=len(a)
    b= np.zeros(n)
    for i in range(1,n-1):
        b[i]=5.0*(a[i+1]-a[i-1])
    b[n-1] = 5.0*(a[0]-a[n-2])
    b[0] = 5.0*(a[1]-a[n-1])

    return b

def compute_b2(a):
    b= np.zeros(len(a))
    for i in range(len(a)-1):
        b[i]=5.0*(a[i+1]-a[i-1])
    b[len(a)-1] = 5.0*(a[0]-a[len(a)-2])

    return b


def normalize_var(a):
    var_a=np.var(a)
    a=a/np.sqrt(var_a)
    var_a=np.var(a)
    mean_a = np.mean(a)
    print("mean = "+str(mean_a)+" var = "+str(var_a))
    return a

def compute_ensemble_mean(ensemble):
    mean_ensemble = np.zeros(len(ensemble[0]))
    for iEns in range(len(ensemble)):
        mean_ensemble += ensemble[iEns]

    # Compute mean of ensemble
    mean_ensemble /= float(len(ensemble))

    return mean_ensemble


def main():
    """
    Generate ensemble members according to
    Oke et al(2007) "Impacts of localisation in the EnKF and EnOI: experiments with a small model"
    :return:
    """

    np.random.seed(2017)

    NComb = 25+1
    N     = 1000
    Nens = int(input("Enter the number of ensembles: "))

    # Generate first sample
    a_ref = generate_a(NComb, N)
    a_ref = normalize_var(a_ref)
    b_ref = compute_b(a_ref)
    a_ref = a_ref + 6.0
    b_ref = b_ref +0.5

    #Generate true field
    a_true = a_ref+ normalize_var(generate_a(NComb, N))
    b_true = compute_b(a_true)

    #Now generate the ensemble
    ensemble_a=[None]*Nens
    ensemble_b=[None]*Nens

    mean_ensemble = np.zeros(len(a_ref))
    for iEns in range(Nens):
        ensemble_a[iEns] = generate_a(NComb, N)
        mean_ensemble += ensemble_a[iEns]

    # Compute mean of ensemble
    mean_ensemble /= Nens

    # Subtract mean from ensemble members, normalize and add reference
    for iEns in range(Nens):
        ensemble_a[iEns] = normalize_var(ensemble_a[iEns])
        ensemble_a[iEns] -= mean_ensemble
        ensemble_a[iEns] += a_ref

    # Compute b for ensemble
    for iEns in range(Nens):
        ensemble_b[iEns] = compute_b(ensemble_a[iEns])


    # Generate observations
    Locations = [125-1, 375-1, 625-1, 875-1]
    dt = 5
    obs_times = range(0, 305, dt)
    observations = [[] for i in range(len(Locations))]

    for iLoc in range(len(Locations)):
        for t in range(0, 305, dt):
            ix=Locations[iLoc] -t + len(a_true)
            ix=ix%len(a_true)
            obs=a_true[ix]+0.01*np.random.normal()
            observations[iLoc].append(obs)

    #plt.figure()
    #plt.plot(observations[0])
    #plt.show()

    #write observation file
    fobs = open("observations.csv","w")
    fobs.write("time, location, value, std, type\n")
    for iTime in range(len(obs_times)):
        for iLoc in range(len(Locations)):
            t=obs_times[iTime]
            loc=Locations[iLoc]
            obs=observations[iLoc][iTime]

            fobs.write(str(t)+","+str(loc)+","+str(obs)+",0.01,0,1\n")
    fobs.close()



    mean_a = compute_ensemble_mean(ensemble_a)
    mean_b = compute_ensemble_mean(ensemble_b)
    mean_b_check=compute_b(mean_a)

    #plt.figure()
    #plt.plot(mean_b)
    #plt.show()


    #Write ensembles (write mean in first column)
    filename="ensembles_"+str(Nens)+".csv"
    fens = open(filename,"w")
    for i in range(N):
        line = str(mean_a[i])+","
        for iEns in range(Nens-1):
            line+=str(ensemble_a[iEns][i])+","
        line+=str(ensemble_a[Nens-1][i])+"\n"
        fens.write(line)
    for i in range(N):
        line =str(mean_b[i])+","
        for iEns in range(Nens-1):
            line+=str(ensemble_b[iEns][i])+","
        line+=str(ensemble_b[Nens-1][i])+"\n"
        fens.write(line)

    #Write true_state


    a_true=a_true.tolist()
    b_true=b_true.tolist()

    a_true_0=a_true
    t_shift=149
    a_true_150=a_true[1000-t_shift:1000]+a_true[0:1000-t_shift]
    t_shift=299
    a_true_300=a_true[1000-t_shift:1000]+a_true[0:1000-t_shift]


    b_true_0=b_true
    t_shift=149
    b_true_150=b_true[1000-t_shift:1000]+b_true[0:1000-t_shift]
    t_shift=299
    b_true_300=b_true[1000-t_shift:1000]+b_true[0:1000-t_shift]
    ftrue = open("true_state.py","w")
    ftrue.write("a_true_0="+str(a_true_0)+"\n")
    ftrue.write("a_true_150="+str(a_true_150)+"\n")
    ftrue.write("a_true_300="+str(a_true_300)+"\n")
    ftrue.write("b_true_0="+str(b_true_0)+"\n")
    ftrue.write("b_true_150="+str(b_true_150)+"\n")
    ftrue.write("b_true_300="+str(b_true_300)+"\n")
    ftrue.close()


    # Make plot of true initial state and stars at the observation locations
    if False:
        plt.figure()
        plt.subplot(2,1,1)
        plt.plot(a_true,'b')
        plt.plot(Locations, list(a_true[i] for i in Locations),'m*')
        plt.title("Variable a")

        plt.subplot(2,1,2)
        plt.plot(b_true,'r')
        plt.plot(Locations, list(b_true[i] for i in Locations),'m*')
        plt.title("Variable b = 0.5 + 10 da/dx")
        plt.show()


    if True:
        plt.figure()
        plt.subplot(2,1,1)
        plt.plot(a_true,'r*')
        plt.plot(ensemble_a[0],'b')
        plt.plot(ensemble_a[1],'b')
        plt.plot(ensemble_a[2],'b')
        plt.plot(ensemble_a[3],'b')
        plt.title("Variable a")


        plt.subplot(2,1,2)
        plt.plot(b_true,'r*')
        plt.plot(ensemble_b[0],'b')
        plt.plot(ensemble_b[1],'b')
        plt.plot(ensemble_b[2],'b')
        plt.plot(ensemble_b[3],'b')
        plt.title("Variable b")
        plt.show()



    print("Done")

if __name__ == "__main__":
    main()
