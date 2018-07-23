import enkf50_results as enkf
import true_state
from plot_enkf import plot_enkf

if __name__ == "__main__":
    plot_enkf(true_state, enkf)

    plot_enkf()
