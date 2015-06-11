% Weakly Constrained Ensembles (WCE)
% This package creates ensemble perturbations that have to satisfy an a priori linear constraint. It can also be used to create perturbations that are aware of the land-sea mask or that use space- (or time-) dependent correlation length. 
% version 1.1 02-Oct-2012
%
% Details of the procedure are explained in: 
%
% A. Barth, A. Alvera-Azcárate, J.-M. Beckers, R. H. Weisberg, L. Vandenbulcke, F. Lenartz, and M. Rixen. Dynamically constrained ensemble perturbations - application to tides on the West Florida Shelf. Ocean Science, 5(3):259–270, 2009. doi: 10.5194/os-5-259-2009
%
% A. Barth, A. Alvera-Azcárate, K.-W. Gurgel, J. Staneva, A. Port, J.-M. Beckers, and E. V. Stanev. Ensemble perturbation smoother for optimizing tidal boundary conditions by assimilation of High-Frequency radar surface currents - application to the German Bight. Ocean Science, 6(1):161–178, 2010. doi: 10.5194/os-6-161-2010
%
% wce_simple         - Generates ensemble perturbations taking into account the land-sea mask, correlation length and possibly a vector field 
% wce_tides          - Generates ensemble perturbations constrained by the harmonic shallow water equations
% statevector_init   - Initialization structure for packing and unpacking multiple variables using a mask
% statevector_pack   - Pack different variables into a vector
% statevector_unpack - Unpack a vector into the different variables
%
