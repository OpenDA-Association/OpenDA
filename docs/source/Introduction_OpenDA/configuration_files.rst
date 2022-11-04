===================
Configuration files
===================


In OpenDA, the following configuration files are used: 

- Main configuration file (with XML schema ``openDaApplication.xsd``): In the main configuration file, the OpenDA java class names, working directories and configuration file names of all the used data-assimilation components are specified.
- Stochastic observer: In this configuration file, the user specifies the observation data used in the application as well as the information about its uncertainty.
- Stochastic model: In this configuration file, the user specifies the model-related information. 
- Algorithm: In this configuration file, the user specifies the input parameters required by the data-assimilation or parameter-calibration algorithm being used.
