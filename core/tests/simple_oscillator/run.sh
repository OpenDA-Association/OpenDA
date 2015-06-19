#! /bin/sh


#
# simulation and generation of observations
#
Application.sh Simulation.oda 
#also simulation but with run broken in pieces
Application.sh SequentialSimulation.oda 
Application.sh SequentialSimulation_fixedAnalysisTimes.oda 

#
# Kalman filtering
#
Application.sh Enkf.oda 
Application.sh ParticleFilter.oda 
Application.sh Ensr.oda 
Application.sh Enkf_fixedAnalysis.oda 
Application.sh ParticleFilter_fixedAnalysis.oda 
Application.sh Ensr_fixedAnalysis.oda 

#
# steady state filtering and generation from enkf
#
Application.sh Enkf_generate_gain.oda
Application.sh Steadystate.oda
#and the asynchronous version
Application.sh Enkf_async_generate_gain.oda
Application.sh Steadystate_async.oda

#
# calibration without constraint
#

Application.sh Simplex.oda 
Application.sh Powell.oda
Application.sh Dud.oda
Application.sh GriddedFullSearch.oda

#
# calibration with a weak constraint
#

Application.sh SimplexWithConstraint.oda 
Application.sh PowellWithConstraint.oda
Application.sh DudWithConstraint.oda

