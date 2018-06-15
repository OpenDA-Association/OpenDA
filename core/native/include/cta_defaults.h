/*
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

/**
\file  cta_defaults.h
\brief A list of all default class implementations.
*/

#ifndef CTA_DEFAULTS_H
#define CTA_DEFAULTS_H
#include "cta_system.h"
#include "cta_vector.h"
#include "cta_matrix.h"
#include "cta_sobs.h"
#include "cta_model.h"
#include "cta_file.h"

#ifdef __cplusplus
extern "C" {
#endif
/** default dense vector class implementation */
CTAEXPORT extern CTA_VecClass  CTA_DEFAULT_VECTOR;

/** default stochastic observer class implementation 
 *  based on an sqlite3 database                     */
CTAEXPORT extern CTA_SObsClass CTA_DEFAULT_SOBS;

/** stochastic observer class implementation
 * that concatenates stochastic observers of arbitrary kind */
CTAEXPORT extern CTA_SObsClass CTA_COMBINE_SOBS;

/** stochastic observer class implementation
 * based on NetCDF input files                       */
CTAEXPORT extern CTA_SObsClass CTA_NETCDF_SOBS;

/** stochastic observer class implementation
 * based on NetCDF input files                       */
CTAEXPORT extern CTA_SObsClass CTA_MAORI_SOBS;

/** stochastic observer class implementation
 * based on user provided library                    */
CTAEXPORT extern CTA_SObsClass CTA_USER_SOBS;

/** observer description class implementation based on a simple table */
CTAEXPORT extern CTA_ObsDescrClass CTA_OBSDESCR_TABLE;

/** default dense matrix class implementation */
CTAEXPORT extern CTA_MatClass CTA_DEFAULT_MATRIX;

/** The SP (Single processor) model builder */
CTAEXPORT extern CTA_ModelClass CTA_MODBUILD_SP;

/** The PAR (multiple processor parallel) model builder */
CTAEXPORT extern CTA_ModelClass CTA_MODBUILD_PAR;


/** File handle that actually prints to standard out */
CTAEXPORT extern CTA_File CTA_FILE_STDOUT;

/** The modelcombiner */
CTAEXPORT extern CTA_ModelClass CTA_MODELCOMBINER;

/** The B3B (Black box) model builder */
CTAEXPORT extern CTA_ModelClass CTA_MODBUILD_B3B;

/** The 'New' (Black box) model builder */
CTAEXPORT extern CTA_ModelClass CTA_MODBUILD_BB;

/** Operator for dispaying the scaled RMS of the roots of a treevector to use with method
 *  CTA_TreeVector_OpOnLeafs */
CTAEXPORT CTA_Func extern CTA_OP_ROOT_RMS;

/** Operator for finding the index of the maxabs of the roots of a 
  * treevector to use with method CTA_TreeVector_OpOnLeafs */
CTAEXPORT CTA_Func extern CTA_OP_ROOT_AMAX;

/** Operator for finding displaying the values at given indices of the
  * roots of a treevector to use with method CTA_TreeVector_OpOnLeafs */
CTAEXPORT CTA_Func extern CTA_OP_ROOT_PRINTI;

/** Operator for dispaying the scaled Sum-of-Squares of the roots of a 
    treevector to use with method CTA_TreeVector_OpOnLeafs */
CTAEXPORT CTA_Func extern CTA_OP_ROOT_SSQ;

/** Initial random seed */
CTAEXPORT extern long int CTA_INITIAL_RANDOM_SEED;

/** Name of the user provided dynamic library with user functions */
CTAEXPORT extern char userDefaultDynamicLibrary[256];


#ifdef __cplusplus
}
#endif
#endif
