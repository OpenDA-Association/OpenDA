/*
$URL: https://repos.deltares.nl/repos/openda/trunk/costa/src/cta/cta_obsdescr_netcdf.c $
$Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (Tue, 07 Oct 2008) $

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

#include <string.h>
#include <stdio.h>
#include "cta.h"

void CTA_SObs_maori_initialize(CTA_SObsClass *hsobscl)
{
   CTA_Func h_func[CTA_SOBS_NUMFUNC];
   char *libraryName;
   char *functionName;
   CTA_ObsDescrClass hobsdescrcl;
   int i;

   // The vector h_func is filled with function read from the default user dynamic library
   libraryName="libleoda.so";
   for (i=0;i<CTA_SOBS_NUMFUNC;i++){
      h_func[i]=CTA_NULL;
   }
   functionName="ctai_sobs_maori_create_size";
   h_func[CTA_SOBS_CREATE_SIZE]      = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_sobs_maori_create_init";
   h_func[CTA_SOBS_CREATE_INIT]      = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_sobs_maori_create_selection";
   h_func[CTA_SOBS_CREATE_SELECTION] = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_sobs_maori_count";
   h_func[I_CTA_SOBS_COUNT]          = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_sobs_maori_get_values";
   h_func[CTA_SOBS_GET_VALUES]       = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_sobs_maori_get_times";
   h_func[CTA_SOBS_GET_TIMES]        = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_sobs_maori_get_expectation";
   h_func[CTA_SOBS_GET_EXPECTATION]  = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_sobs_maori_get_realisation";
   h_func[CTA_SOBS_GET_REALISATION]  = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_sobs_maori_get_variance";
   h_func[CTA_SOBS_GET_VARIANCE]     = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_sobs_maori_get_cov_matrix";
   h_func[CTA_SOBS_GET_COV_MATRIX]   = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_sobs_maori_export";
   h_func[I_CTA_SOBS_EXPORT]         = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_sobs_maori_free";
   h_func[I_CTA_SOBS_FREE]           = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);


   CTA_ObsDescr_maori_initialize(&hobsdescrcl);

   CTA_SObs_DefineClass("cta_sobs_maori",h_func,hobsdescrcl,hsobscl);
}





