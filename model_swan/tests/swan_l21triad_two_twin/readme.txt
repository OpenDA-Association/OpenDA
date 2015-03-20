
Introduction

This test contains contains a laboratory experiment known as the Beji-Battjes bar. It is a wave shoaling and breaking experiement. Multiple parameters are measured at several points from deep water to the "coast".
The parameters used for estimation are:
	"trfac" value="0.05" stdv="0.0125" "triads"
	"cutfr" value="2.5" stdv="1.0" "triads"

experiment          trfac       cutfr       cost       no-runs
---------------------------------------------------------------------
true 1              0.0600      2.8000
true 2              0.0580      2.6000
initial             0.0500      2.5000      3.636
---------------------------------------------------------------------
dud 1               0.0602      2.7941      6.3E-5     6
dud 2               0.0576      2.6021      2.1E-4     5
---------------------------------------------------------------------
dud 1+2             0.0582      2.6831      0.3016    18
simplex 1+2         0.0582      2.6842      0.3017    39
powell 1+2          0.0710      2.5145      0.3239    76
fullsearch 1+2      0.0525      2.8000      0.3130   272 
---------------------------------------------------------------------
dud 1+2 constr      0.0515      2.7191      0.8651   121 (quick start)
simplex 1+2 constr  0.0519      2.7059      0.8746    82
powell 1+2 constr   0.0513      2.7197      0.8648   144


