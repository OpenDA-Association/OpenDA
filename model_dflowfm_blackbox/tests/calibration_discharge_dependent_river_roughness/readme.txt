See also http://content.oss.deltares.nl/delft3d/manuals/D-Flow_FM_User_Manual.pdf#subsection.B.6.1 for trachytopes and chapter 19 about OpenDA

NOTE: To run these experiments make sure that run_dflowfm.sh can be found on the path!

The m-files can be used to plot results after running the corresponding
simulations. The results are not very good yet, settings of the noise model
and the number of ensembles are to be tuned after the restart functionality of
DFlow FM is improved.

In this example noise is added to a waterlevel boundary specified as astronomical components.



Model description

The model is a very simple representation of a prototype river with a western boundary (1)
that connets to the open sea and a river flowing into the model at the eastern end. The main 
characteristics of the initial model BEFORE calibration are:
- domain x=0 to 99km with 500m grid-cells
- sloping bathymetry from -5m to -5 m
- constant sealevel of at western boundary of -2.5m
- constant river influx of 251m^3/s at eastern boundary
- trachytope bedfriction in ruw.ttd
- 10 minute timestep
- observation points at 10, 30, 50, 70 and 90 km from the western boundary called M20, M60, M100, M140 and M180 (cell count from left bound)
- the simulation is starting 2012 Jan 1st for 5 days
