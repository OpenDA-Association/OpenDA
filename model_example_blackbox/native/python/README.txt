
Introduction

This folder contains a simple 1D advection model. This can be seen as a simplified model for e.g.
the transport of pollution in a river without branches. The model allows multiple point sources and
a prescribed concentration at the inflow boundary.
The output file will contain timeseries at specified output locations as well as the concentrations at
the final time for all gridcells.

Use

If python is installed, one can run the python code of the model directly:
>>python pollution_model.py pollution_model.input
The filename for the output is specified in the input file.
The python code has also been compiled into executables for windows and linux (both 32bit).
On windows one can start with:
c:\python> pollution_model.exe pollution_model.input
On linux this becomes
~\python>> pollution_model pollution_model.input

As a very simple testcase the model can also be started without input. In that case a default model 
built into the code will be run.

known issues

The advection numerics was simplified. The advection is probably wrong if the cross-section and 
velocity vary in space. 
There are insufficient tests for catching wrong input.
