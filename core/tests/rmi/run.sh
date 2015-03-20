#! /bin/sh

# Run with one 'remote' instance over rmi.
# In this example we use this computer also for remote to keep the example simple.
./start_rmi.sh
oda_run.sh lorenzEnkf.oda
./stop_rmi.sh

# Now with 4 'remote' computers. 
# This computer is used for the local and all 4 remote machines.
./start_rmi4.sh
oda_run.sh lorenzEnkf4.oda
./stop_rmi.sh

# The 4 'remote' computers are now used in parallel
# one computer.
./start_rmi4.sh
oda_run.sh lorenzEnkf4Thread.oda
./stop_rmi.sh
