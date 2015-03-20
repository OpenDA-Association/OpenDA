@echo off
ifort inputFileVariables.f90 pollution_model.f90 computeNumberOfData.f90 computeNextTimeStep.f90 collectOutput.f90 writeOutput.f90 -o pollution_model.exe
