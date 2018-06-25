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
\file  cta_model.h
\brief Interface description of the COSTA default model component. For user implementation see cta_usr_model.h.

Functions for creating and working with models. CTA_Model is the default class implementation for models.
*/

#ifndef CTA_MODEL_FACTORY_H
#define CTA_MODEL_FACTORY_H


#ifdef __cplusplus
extern "C" {
#endif
 
 
/** \brief Create a COSTA modell class from XML input file
*          (load from methods from dynamic load library)
*
*  \param fName  I  XML-configuration file
*  \param modelClass O Class of new model Factory
*  \return          Model class handle
*/
CTAEXPORT int CTA_ModelFactory_New(const char *fName, CTA_ModelClass* modelClass );


/** \brief Create a COSTA modell class from XML
*          (load from methods from dynamic load library). 
*
*  \param cur_node  I  Current XML node 
*  \return             Handle to create or CTA_NULL in case of an error.
*/
CTAEXPORT CTA_ModelClass CTAI_XML_CreateModelClass(xmlNode *cur_node);

/** \brief Define a new class (=implementation) of a COSTA model component
 *
 * \param name     I  name of the new model class
 * \param h_func   I  COSTA function handles for functions that implement class,
 *                    missing functions must have value CTA_NULL
 * \param hmodcl   O  receives handle of new model class
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_DefineClass(const char *name, const CTA_Func h_func[CTA_MODEL_NUMFUNC], CTA_ModelClass *hmodcl);

const char *CTAI_ModelFac_GetImplements(CTA_ModelClass hmodcl);

int CTAI_ModelFac_GetParallelData(CTA_ModelClass hmodcl, char **implements, char **parallel_type,
                char **spawn_workers, char **nproc, char **ntimes, char **dumProcs);


int CTAI_ModelFac_GetBlock(CTA_ModelClass hmodcl, CTA_Model hmodel);
int CTAI_ModelFac_GetBarrierData(CTA_ModelClass hmodcl, char **flag_barrier, double *t_step );
int CTAI_ModelFac_AddModelInstance(CTA_ModelClass hmodcl, CTA_Model hmodel);
int CTAI_ModelFac_SetBlock(CTA_ModelClass hmodcl, CTA_Model hmodel);
int CTAI_ModelFac_TimeStepAllModels(CTA_ModelClass hmodcl, CTA_Function *function, double tstart, double tstop);



#ifdef __cplusplus
}
#endif


#endif

