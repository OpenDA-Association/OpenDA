#! /bin/sh

. ../set_path.sh

#
# simulation and generation of observations
#
Application.sh Simulation.oda

#
# Kalman filtering
#
Application.sh Enkf.oda
Application.sh Ensr.oda
Application.sh ParticleFilter.oda

#
# calibration without constraint
# !! DOES NOT MAKE SENSE FOR THIS MODEL WITH THIS SETUP !!
#

#Application.sh lorenz96SimplexOpenDaConfig_linux.xml 
#Application.sh lorenz96PowellOpenDaConfig_linux.xml
#Application.sh lorenz96DudOpenDaConfig_linux.xml

#
# calibration with a weak constraint
# !! DOES NOT MAKE SENSE FOR THIS MODEL WITH THIS SETUP !!
#

#Application.sh lorenz96SimplexOpenDaConfig_withConstraint_linux.xml 
#Application.sh lorenz96PowellOpenDaConfig_withConstraint_linux.xml
#Application.sh lorenz96DudOpenDaConfig_withConstraint_linux.xml

