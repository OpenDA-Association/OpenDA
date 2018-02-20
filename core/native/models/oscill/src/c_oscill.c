/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/oscill/src/c_oscill.c $
$Revision: 1483 $, $Date: 2010-04-16 16:31:25 +0200 (Fri, 16 Apr 2010) $

COSTA: Problem solving environment for data assimilation
Copyright (C) 2007  Nils van Velzen

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

#include "f_cta_utils.h"
#include "cta_treevector.h"
#include "c_oscill.h"

#define OSCILL_MODEL_CREATEFUNC_F77  CF77_CALL(oscill_model_createfunc,OSCILL_MODEL_CREATEFUNC)

#define OSCILL_MODEL_SETGRID_F77   F77_CALL(oscill_model_setgrid,OSCILL_MODEL_SETGRID)

void OSCILL_MODEL_CREATEFUNC_F77();

void Oscill_Model_CreateFunc(){
   OSCILL_MODEL_CREATEFUNC_F77();
}

int Oscill_Model_setgrid(CTA_TreeVector state){
   CTA_Metainfo minfo;
   CTAI_Gridm thisgrid;

    printf("entering oscill_model_setgrid \n");


   CTA_Metainfo_Create(&minfo); 
   CTA_Metainfo_SetTag(minfo,"ar1-noise");
   // fill grid

   strcpy(thisgrid.name,"oscill grid");
   thisgrid.type = 1;

   thisgrid.nx = 2;
   thisgrid.ny = 1;
   thisgrid.nz = 1;
   thisgrid.x_origin = 0.0;
   thisgrid.y_origin = 0.0;
   thisgrid.z_origin = 0.0;
   thisgrid.dx = .2;
   thisgrid.dy = .1;
   thisgrid.dz = .1;
   thisgrid.nsize = thisgrid.nx*thisgrid.ny*thisgrid.nz;

   CTA_Metainfo_SetGrid(minfo,&thisgrid);
   CTA_TreeVector_SetMetainfo(state, minfo);

 

   return CTA_OK;
}





void OSCILL_MODEL_SETGRID_F77(int *hstate, int*ierr){
   *ierr=Oscill_Model_setgrid((CTA_TreeVector) *hstate);
}
