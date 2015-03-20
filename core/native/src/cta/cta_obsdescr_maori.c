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

void CTA_ObsDescr_maori_initialize(CTA_ObsDescrClass *hobsdescrcl)
{
   CTA_Func h_func[I_CTA_OBSDESCR_NUMFUNC];
   char *libraryName;
   char *functionName;

   // The vector h_func is filled with function read from the maori user dynamic library

   libraryName="libleoda.so";
   functionName="ctai_obsdescr_maori_create_size";
   h_func[I_CTA_OBSDESCR_CREATE_SIZE]        = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_obsdescr_maori_create_init";
   h_func[I_CTA_OBSDESCR_CREATE_INIT]        = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_obsdescr_maori_property_count";
   h_func[I_CTA_OBSDESCR_COUNT_PROPERTIES]   = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_obsdescr_maori_get_properties";
   h_func[I_CTA_OBSDESCR_GET_PROPERTIES]     = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_obsdescr_maori_observations_count";
   h_func[I_CTA_OBSDESCR_COUNT_OBSERVATIONS] = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_obsdescr_maori_get_keys";
   h_func[I_CTA_OBSDESCR_GET_KEYS]           = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_obsdescr_maori_free";
   h_func[I_CTA_OBSDESCR_FREE]               = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);
   functionName="ctai_obsdescr_maori_selection";
   h_func[I_CTA_OBSDESCR_SELECTION]          = CTA_CreateFuncDynamicLib(libraryName, functionName, functionName, functionName);

   CTA_ObsDescr_DefineClass("cta_obsdescr_maori",h_func,hobsdescrcl);


}



