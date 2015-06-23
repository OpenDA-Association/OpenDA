lake_kalman:

Input files for DFlowFM were created by conversion from example model_delft3d/tests/d3d_lake. Note: we changed the eastern boundary from velocity boundary conditions to waterlevel boundary conditions.

This example consists of a two dimensional square lake, with a tidal forcing (M2 and S2) on the eastern boundary and a time varying uniform northerly wind. The simulation can be run with the following configurations:
- Simulation.oda
- SequentialSimulation.oda

A wind noise field is configured with a horizontal correlation scale of 10 km and a correlation time of 12 hours. To run the simulation with the noise field added use these OpenDA configurations
- SimulationNoise.oda
- SequentialSimulationNoise.oda

Time series are written by D-Flow FM to the lake2d_his.nc file for a number of stations (S1, S2, ...., S8). These can directly be used with the stochObserver. The file provided in this example is created by running SimulationNoise.oda. 

With these observations we can run a indentical twin experiment for demonstrating the ensemble Kalman filter. Stations S1 to S4 are used in this experiment. The ensemble Kalman filter kan run using the Enkf.oda configuration.
