

Latest version available at http://modb.oce.ulg.ac.be/mediawiki/index.php/WCE


Weakly Constrained Ensembles (WCE)
----------------------------------

This package creates ensemble perturbations that have to satisfy an a priori linear constraint. It can also be used to create perturbations that are aware of the land-sea mask or that use space- (or time-) dependent correlation length.

Details of the procedure are explained in:

% A. Barth, A. Alvera-Azcárate, J.-M. Beckers, R. H. Weisberg, L. Vandenbulcke, F. Lenartz, and M. Rixen. Dynamically constrained ensemble perturbations - application to tides on the West Florida Shelf. Ocean Science, 5(3):259–270, 2009. doi: 10.5194/os-5-259-2009
%
% A. Barth, A. Alvera-Azcárate, K.-W. Gurgel, J. Staneva, A. Port, J.-M. Beckers, and E. V. Stanev. Ensemble perturbation smoother for optimizing tidal boundary conditions by assimilation of High-Frequency radar surface currents - application to the German Bight. Ocean Science, 6(1):161–178, 2010. doi: 10.5194/os-6-161-2010


Please cite these manuscripts if you use this package.


Requirements
------------

This packages requires either GNU Octave or MATLAB. If you use Octave version 3.0.5 or earlier, you need also the Octave package arpack.

Installation
------------

    * Extract the package at a location that suits you
    * Add the path to your MATLAB/Octave search path in the menu File > Set Path (in MATLAB) or by adding: 

addpath('/path/to/WCE-x.y');

in your start-up file called startup.m in MATLAB or .octaverc in GNU Octave.

Testing
-------

The scripts wce_demo will show you several examples:

wce_demo

Examples
--------

    * Perturbation aware of land-sea mask
    * Perturbation with variable correlation length
    * Perturbation aligned with vector field
    * Perturbation constrained by harmonic shallow-water equations 
