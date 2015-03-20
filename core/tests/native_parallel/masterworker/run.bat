
if not defined MPI_DIR set MPI_DIR=c:\MPICH2\bin

%MPI_DIR%\mpiexec -np 2 %OPENDA_BINDIR%\run_openda_mpi.bat PolluteEnKFOpenDAConfigMW.oda : -np 1 pollute2d_worker.exe pollute2d_worker_config.xml
