2015-01-29 Downloaded BMI Python Language Binding (file BMI.py) from https://csdms.colorado.edu/svn/bmi/trunk/python/bmi/BMI.py

2015-02-04 Downloaded BMI Python implementation example from https://csdms.colorado.edu/svn/bmi/trunk/python/bmi/BMI_impl.py

2015-03-06 Downloaded most recent version of the CSDMS BMI Python Language Binding (file bmi.py) from https://github.com/csdms/bmi-python/blob/master/bmi/bmi.py

2015-03-06 Extended the original version of the CSDMS BMI Python Language Binding (file bmi.py) from https://github.com/csdms/bmi-python/blob/master/bmi/bmi.py so that it can be used in OpenDA.
The following changes were made:
1. All grid information functions have been merged into the Bmi class, so that different variables within the same model can have different grids.
2. Added function get_grid_type to get the grid type for a given variable.
3. Added function save_state to ask the model to save its state to disk.
4. Added comments. Where the original version of the CSDMS BMI Python Language Binding was ambiguous, the information from http://csdms.colorado.edu/wiki/BMI_Description and common sense were used to fill in most of the gaps.

2015-07-30 Added extension EBmi. Moved function save_state to EBmi.

2015-09-15 Updated from GitHub

2015-10-27 Downloaded most recent version of file bmi.py from https://github.com/eWaterCycle/bmi/blob/master/src/main/python/bmi.py

2015-10-30 Downloaded most recent version of file bmi.py from https://github.com/eWaterCycle/bmi/blob/master/src/main/python/bmi.py
