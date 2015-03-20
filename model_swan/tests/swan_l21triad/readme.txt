
Introduction

This test contains contains a laboratory experiment known as the Beji-Battjes bar. It is a wave shoaling and breaking experiement. Multiple parameters are measured at several points from deep water to the "coast".
The parameters used for estimation are:
	"cfjon" value="0.067" stdv="0.015" "bottom friction"
	"gamma" value="0.73" stdv="0.15" "wave breaking"
	"trfac" value="0.05" stdv="0.0125" "triads"
	"cutfr" value="2.5" stdv="1.0" "triads"
It is important to note that the optimization of gamma and cfjon are degenerate in this test. This 
makes a weak-constraint penalty-term necessary to make this problem well posed. Without this 
constraint the result will depend much on the algorithm that is used and its settings.

The directory structure

stochobserver : directory with observations
algorithm : directory with settings for the optimization algorithms
refout : directory with reference output to check your output against
output_test : examples of input files with various forms of output

Input files for windows-systems in this directory

swanDudOpenDaConfig.xml : calibration with Dud without constraint
swanDudOpenDaConfig_withConstraint.xml : calibration with Dud with constraint
swanPowellOpenDaConfig.xml : calibration with Powell without constraint
swanPowellOpenDaConfig_withConstraint.xml : calibration with Powell with constraint
swanSimplexOpenDaConfig.xml : calibration with Simplex without constraint
swanSimplexOpenDaConfig_withConstraint.xml : calibration with Simplex with constraint

There are corresponding input files for linux. The only difference is that they 
point to a linux version of the Swan executable.

smawanDudOpenDaConfig_linux.xml : calibration with Dud without constraint
swanDudOpenDaConfig_withConstraint_linux.xml : calibration with Dud with constraint
swanPowellOpenDaConfig_linux.xml : calibration with Powell without constraint
swanPowellOpenDaConfig_withConstraint_linux.xml : calibration with Powell with constraint
swanSimplexOpenDaConfig_linux.xml : calibration with Simplex without constraint
swanSimplexOpenDaConfig_withConstraint_linux.xml : calibration with Simplex with constraint

Other files in this directory

readme.txt : this file
clean.sh : linux script to remove old output
run.sh : linux script to run all tests at once

The directory "refout"

refout : reference output for these tests
	The estimation starts with:
	cost_obs=12.9622
	parameters [0.067,0.73,0.05,2.5]
swanDudOpenDaConfig_linux-results.txt : this run stops on assumed convergence because of the rather large 
	tolerance in the convergence criterion. Final values are
	cost_obs=10.6929 evaluations=10 
	parameters=[0.057,0.778,0.061,2.527]
swanDudOpenDaConfig_withConstraint_linux-results.txt : with constraint the problem is well posed 
	and converges. Final values arer:
	cost_obs=10.6534 total_cost=11.6589 evaluations=12
	parameters=[0.064,0.742,0.062,2.506]
swanPowellOpenDaConfig_linux-results.txt : this calibration runs away to large values for the 2nd parameter "gamma". It is stopped by the criterion testing for a maximum number of steps in the linesearch.
	cost_obs=12.6535 evaluations=39
swanPowellOpenDaConfig_withConstraint_linux-results.txt : This run converges, but slowly.
	cost_obs=10.1933 total_cost=10.3556 evaluations=219
	parameters=[0.066,0.730,0.049,2.904]
swanSimplexOpenDaConfig_linux-results.txt
	cost_obs=9.9416 evaluations=24 
	parameters=[0.079,0.444,0.060,2.875]
swanSimplexOpenDaConfig_withConstraint_linux-results.txt
	cost_obs=10.2608 total_cost=10.4297 evaluations=23
	parameters=[0.066,0.733,0.054,2.676]

