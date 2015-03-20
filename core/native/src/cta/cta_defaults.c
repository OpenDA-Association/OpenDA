/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_defaults.c $
$Revision: 3406 $, $Date: 2012-08-16 15:25:53 +0200 (Thu, 16 Aug 2012) $

COSTA: Problem solving environment for data assimilation
Copyright (C) 2005  Nils van Velzen

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "cta_defaults.h"

/** default dense vector class implementation */
CTA_VecClass  CTA_DEFAULT_VECTOR;

/** default stochastic observer class implementation */
CTA_SObsClass CTA_DEFAULT_SOBS;

/** observer description class implementation based on a simple table */
CTA_ObsDescrClass CTA_OBSDESCR_TABLE;

/** stochastic observer class implementation
 * that concatenates stochastic observers of arbitrary kind */
CTA_SObsClass CTA_COMBINE_SOBS;

/** stochastic observer class implementation
 * based on NetCDF input files                       */
CTA_SObsClass CTA_NETCDF_SOBS;

/** stochastic observer class implementation
 * based on MAORI observations library               */
CTA_SObsClass CTA_MAORI_SOBS;

/** stochastic observer class implementation
 * based on user provided library                    */
CTA_SObsClass CTA_USER_SOBS;

/** default dense matrix class implementation */
CTA_MatClass CTA_DEFAULT_MATRIX;

/** The SP (Single processor) model builder */
CTA_ModelClass CTA_MODBUILD_SP;

/** The PAR (multiple processor parallel) model builder */
CTA_ModelClass CTA_MODBUILD_PAR;

/** File handle that actually prints to standard out */
CTA_File CTA_FILE_STDOUT;

/** The modelcombiner */
CTA_ModelClass CTA_MODELCOMBINER;

/** The B3B (Black box) model builder */
CTA_ModelClass CTA_MODBUILD_B3B;

/** The 'New' (Black box) model builder */
CTA_ModelClass CTA_MODBUILD_BB;

/** Operator for dispaying the scaled RMS of the roots of a treevector to use with method
 *  CTA_TreeVector_OpOnLeafs */
CTA_Func CTA_OP_ROOT_RMS;

/** Operator for displaying the values in given indices of the roots of a 
  * treevector to use with method CTA_TreeVector_OpOnLeafs */
CTA_Func CTA_OP_ROOT_PRINTI;

/** Operator for calculating the locations of maxabs-values of the roots
  * of a treevector to use with method CTA_TreeVector_OpOnLeafs */
CTA_Func CTA_OP_ROOT_AMAX;

/** Operator for dispaying the scaled Sum-of-Squares of the roots of a 
  * treevector to use with method CTA_TreeVector_OpOnLeafs */
CTA_Func CTA_OP_ROOT_SSQ;

/** Initial random seed */
long int CTA_INITIAL_RANDOM_SEED;

/** Name of the user provided dynamic library with user functions */
char userDefaultDynamicLibrary[256];
