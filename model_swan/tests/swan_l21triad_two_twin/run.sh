#! /bin/sh

. ../set_path.sh

#
# simulations
#
#Application.sh Simulation1.oda 
#Application.sh Simulation2.oda 
#Application.sh Simulation_combined12.oda 


#
# calibration without constraint
#

Application.sh Dud1.oda
Application.sh Dud2.oda
Application.sh Dud_combined12.oda
Application.sh Simplex_combined12.oda 
Application.sh Powell_combined12.oda
Application.sh GriddedFullSearch_combined12.oda

#
# with a weak constraint
#

Application.sh DudWithConstraint_combined12.oda
Application.sh SimplexWithConstraint_combined12.oda 
Application.sh PowellWithConstraint_combined12.oda
