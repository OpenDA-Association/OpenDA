dcmv5_kalman:
example to be finished when the meteo module of DFlowFM is available.

Input files for DFlowFM were created by conversion from a SIMONA with OpenDA configuration.
The wind data from SIMONA is not converted.

The initial restart file dcsmv5_map.nc is created calling DFlowFM with a simulation period of 0.
(See dcsmv5.init.mdu in stochModel/input_dflowfm).

This model uses a noos observer with the observations the same as used in the SIMONA model

A spatially correlated noise is added to the wind in x-direction.

TODO:
 check amplitude of wind noise compared to SIMONA.