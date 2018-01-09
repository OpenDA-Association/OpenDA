:: NHI model configuration
python.exe d:\NHI\LHM\configuratie_scripts\svn948\nhi.py -conf -cfgfile NHI3.2.0_2015-2016.ini -cfgdir e:\LHM\Projects\2017\SBIR\control\control_2015-2016
:: NHI pre-processing
python.exe d:\NHI\LHM\configuratie_scripts\svn948\nhi.py -pre -cfgdir e:\LHM\Projects\2017\SBIR\control\control_2015-2016 -rundir e:\LHM\Projects\2017\SBIR\runs\NHI320_2015-2016 -comp mod sim tran moz dm -tstart 20150101 -tend 20170101 -transient

pause
