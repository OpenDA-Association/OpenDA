[![Java CI](https://github.com/OpenDA-Association/OpenDA/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/OpenDA-Association/OpenDA/actions/workflows/ci.yml)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/465833e082b54b279105a280b36c75b8)](https://www.codacy.com/gh/OpenDA-Association/OpenDA/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=OpenDA-Association/OpenDA&amp;utm_campaign=Badge_Grade)

# OpenDA

OpenDA is an open interface standard for (and free implementation of) a set of tools to quickly apply data-assimilation and calibration to arbitrary numerical models. It aims to lower implementation costs and promote the sharing of software among researchers and practitioners. 
Models compatible with the OpenDA standard can access all OpenDA tools, enabling experimentation with data-assimilation and calibration methods without extensive programming. Similarly, developers who make their methods OpenDA-compatible can share them with all users, either freely or commercially. OpenDA has been designed for high performance, making it suitable for large-scale models. It allows users to optimize the interaction between their models and the data-assimilation or calibration methods, achieving efficiency comparable to custom implementations.

OpenDA is an open-source project. The OpenDA documentation is available at https://docs.openda.org. Contributions are welcome from anyone interesting in enhancing the toolset. More information about the development process can be found [here](./INSTALL.md).

## Features of OpenDA

Data-assimilation methods

- Ensemble KF (EnKF)
- Ensemble SquareRoot KF (EnSR)
- Steady State KF
- Particle Filter
- 3DVar
- DudEnKF (still under research)
- DudEnSR (still under research)

Parameter estimation (calibration) methods:

- Dud and Sparse Dud
- Simplex, Powell
- Gridded full search
- Shuffled Complex Evolution (SCE)
- Generalized Likelihood Uncertainty Estimation (GLUE)
- (L)BFGS
- Conjugate Gradient: Fleetjer-Reeves, Polak-Ribiere, Steepest Descent
- Uncertainty Analysis methods
- DELSA

Language interfaces

- C/C++
- Java
- Fortran77/90

For more information, visit the OpenDA website: https://openda.org.
