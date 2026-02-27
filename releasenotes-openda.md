Release Notes
=============

OpenDA - Version 3.4.0
----------------------

### Improvements

- Fixed time rounding issue in DFlowFMTimeInfo
- Fixed Delft3D calibration wind example
- Improved (additional) stop criteria in Dud
- Allow writing larger files for Kalman Gain in netcdf_cf format
- D-Flow FM examples update to use D-Flow FM version 2026.01
- Documentation improvements
- Update summer school exercises

### New functionalities

- FileCopier extended with option to add time stamp in file name
- Option to add timeOffset to last analysis in order to let the simulation run until the end time without analysis step
- Method added to estimate missing observations in steady state filter
- Add HEC-HMS wrapper
- Add option in Dud for maximum total evaluations
- Extend run files with memory option
- Geometry info added to SwanState to support localization

### Removed functionalities

- Removed Costa for Windows
