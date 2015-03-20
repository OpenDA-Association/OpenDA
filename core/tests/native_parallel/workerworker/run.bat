if not defined MPI_DIR set MPI_DIR=c:\MPICH2\bin

%MPI_DIR%\mpiexec -np 3 %OPENDA_BINDIR%\run_openda_mpi.bat PolluteEnKFOpenDAConfig.oda
