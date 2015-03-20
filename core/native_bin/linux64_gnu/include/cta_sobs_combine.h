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
\file  cta_sobs_combine.h
\brief  COMBINE implementation of the stochastic observer interface.
*/


#include "cta_f77blas.h"
#include "cta_datatypes.h"
#include "cta_handles.h"
#include "cta_functions.h"
#include "cta_util_sqlite3.h"
#include "cta_sobs.h"
#include "cta_obsdescr.h"


/* Dit is een struct om wat op te slaan wat we nodig hebben 
   Enigszins analoog aan CTAI_SOBS en de datablock van de modelcombiner */

typedef struct {
int nstations;
double timeoffset;
int nmeasr;
int * stations;
int nofsubsobs;      // number of stochastic observers
CTA_Vector subsobs; // list of handles of stochastic observers
} CTAI_SObs_combine;


void CTA_SObs_combine_initialise(CTA_SObsClass *hsobscl);
