! $Id: sangoma_base.f90 547 2015-02-19 12:32:23Z sp709689 $

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!    Module defining real and integer types for Sangoma routines. 
!!!    Copyright (C) 2015  Sanita Vetra-Carvalho
!!!
!!!    This program is distributed under the Lesser General Public License (LGPL)
!!!    version 3, for more details see <https://www.gnu.org/licenses/lgpl.html>.
!!!
!!!    Email: s.vetra-carvalho @ reading.ac.uk
!!!    Mail:  School of Mathematical and Physical Sciences,
!!!    	      University of Reading,
!!!	      Reading, UK
!!!	      RG6 6BB
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module sangoma_base

use, intrinsic :: ISO_C_BINDING
implicit none

integer, parameter :: REALPREC = C_DOUBLE
integer, parameter :: INTPREC = C_INT

end module sangoma_base
