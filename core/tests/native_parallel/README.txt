The pollution 2d is a 2d (air) pollution model written in Fortran90
The model can be run in three different modes.
1) sequential
2) parallel in worker-worker mode
3) parallel in master-worker mode

The two subdirectories contain input in order to run the model in these
different parallel modes. 

Note: this model serves as an illustration on how to couple and run
parallel models in openDA. It is not an example on how to parallelize a
model. Due to the small size of the model and the way that the model is
parallelized it is likely that the sequential version of the model is the
fastest.

1) in order to run the model in sequential mode just run

   Application.sh PolluteEnKFOpenDaConfig.oda   (linux)

   or

   run_openda_app.bat PolluteEnKFOpenDaConfig.oda (windows)

2,3) parallel runs can be started using the scripts run.sh or run.bat in
     the masterworker and workerworker directories. Note that you have to
     edit the scripts for the used paths on your system.

Some short notes on the input files
-----------------------------------

In order to start any parallel runs with the openDA application runner you
must use the optional -p flag for Application.sh or use the
run_openda_mpi.bat script. Parallel runs will look for the input file

   parallel_config.xml

This configuration file specifies the number of processes used by the
model(s). Based on this file all MPI-communication groups will be created.

Note that the pollute2d_worker.exe process that is used when the model runs
in master-worker mode has its own (quite similar) configuration file
called pollute2d_worker_config.xml. If you want to experiment with
different numbers of processes, don't forget to update these files.

(Some) more information on parallel computing and the configuration can be
found in the documentation in the native part of OpenDA.






