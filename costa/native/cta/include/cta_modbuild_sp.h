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
\file  cta_modbuild_sp.h
\brief SP-Modelbuilder.
*/

#ifndef CTA_MODBUILD_SP_H
#define CTA_MODBUILD_SP_H
#include "cta_handles.h"
#include "cta_datatypes.h"
#include "cta_metainfo.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create the model class of the SP-Modelbuilder
 *
 * \note This is not a user function. It is called at initialization of the
 *       COSTA environment.
 *
 *  \param modelcls  O  receives handle of the SP-modelbuilder class
 */
CTANOEXPORT void CTA_Modbuild_sp_CreateClass(CTA_ModelClass *modelcls);
CTANOEXPORT void modbuild_sp_create_size(CTA_Handle userdata, int *memsize, int *ierr);
CTANOEXPORT void modbuild_sp_create_init(CTA_Handle *this_obj, CTA_Handle *data, CTA_Handle *hinput, int *ierr);
CTANOEXPORT void modbuild_sp_free(CTA_Handle *data ,int *ierr);
CTANOEXPORT void modbuild_sp_compute(CTA_Handle *data, CTA_Time *timespan, int *ierr);
CTANOEXPORT void modbuild_sp_setstate(CTA_Handle *data, CTA_TreeVector *state, int *ierr);
CTANOEXPORT void modbuild_sp_getstate(CTA_Handle *data, CTA_TreeVector *state, int *ierr);
CTANOEXPORT void modbuild_sp_axpymodel(CTA_Handle *datay, double *alpha, CTA_Handle *datax, int *ierr);
CTANOEXPORT void modbuild_sp_axpystate(CTA_Handle *data, double *alpha, CTA_TreeVector *statex, int *ierr);
CTANOEXPORT void modbuild_sp_setforc(CTA_Handle *data, CTA_TreeVector *state, int *ierr);
CTANOEXPORT void modbuild_sp_getforc(CTA_Handle *data, CTA_TreeVector *state, int *ierr);
CTANOEXPORT void modbuild_sp_axpyforc(CTA_Handle *data, CTA_Time *tspan, double *alpha,  CTA_TreeVector *statex, int *ierr);
CTANOEXPORT void modbuild_sp_setparam(CTA_Handle *data, CTA_TreeVector *state, int *ierr);
CTANOEXPORT void modbuild_sp_getparam(CTA_Handle *data, CTA_TreeVector *state, int *ierr);
CTANOEXPORT void modbuild_sp_axpyparam(CTA_Handle *data, double *alpha, CTA_TreeVector *statex, int *ierr);
CTANOEXPORT void modbuild_sp_getnoisecount(CTA_Handle *data, int* nnoise, int* ierr);
CTANOEXPORT void modbuild_sp_getnoisecovar(CTA_Handle *data, CTA_TreeVector *colsvar, int* ierr);
CTANOEXPORT void modbuild_sp_getobsvalues(CTA_Handle *data, CTA_Time *ttime, CTA_ObsDescr *hdescr, CTA_Vector *vval, int* ierr);
CTANOEXPORT void modbuild_sp_getobsselect(CTA_Handle *data, CTA_Time *ttime, CTA_ObsDescr *hdescr, CTA_String *sselect, int* ierr);
CTANOEXPORT void modbuild_sp_addnoise(CTA_Handle *data, CTA_Time *ttime, int* ierr);
CTANOEXPORT void modbuild_sp_ar1_create(CTA_Handle *hinput, CTA_TreeVector *state, CTA_TreeVector *sbound, 
                            CTA_TreeVector *sparam, int *nnoise, CTA_Time  *time0,
                            CTA_String *snamnoise, CTA_Handle *husrdata, int *ierr);
CTANOEXPORT void  modbuild_sp_ar1_covar(CTA_TreeVector *colsvar,int* nnoise, CTA_Handle husrdata,int* ierr);
CTANOEXPORT void modbuild_sp_ar1_compute(CTA_Time *timesspan,CTA_TreeVector *state, CTA_TreeVector *saxpyforc,
                             BOOL *baddnoise, CTA_TreeVector *sparam, CTA_Handle husrdata,
                             int* ierr);
CTANOEXPORT void modbuild_sp_ar1_getobsval(CTA_TreeVector *state, CTA_ObsDescr *hdescr, CTA_Vector *vval, CTA_Handle *husrdata, int *ierr);
CTANOEXPORT int modbuild_sp_compute_covars(int nmodel, double *Qar1, double *p0, CTAI_Gridm hgrid);
CTANOEXPORT void CTA_Modbuild_sp_CreateClass(CTA_ModelClass *modelcls);

#ifdef __cplusplus
}
#endif

#endif

