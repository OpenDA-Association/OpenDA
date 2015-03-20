/*
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

/**
\file  cta_modbuild_sp_model_template.h

\brief Interfaces of the user-routines that can be provided in order to create a model using the SP-modelbuilder

Copy this file to your_model.c and fill in the routines.


*/
#include "cta.h"
#include "f_cta_utils.h"

#define CREATEFUNC_F77  CF77_CALL(usr_createfunc,USR_CREATEFUNC)




/** \brief Creates a new model instance
 *  This routine creates and initialises a new model instance.
 *
 *  \param hinput    I   Model configuration CTA_Tree of CTA_String
 *  \param state     O   Model state (initialized to initial value. Note this statevector must be created
 *  \param sbound    O   State-vector for the offset on the forcings. Set CTA_NULL if not used. Note this statevector must be created
 *  \param nnoise    O   The number of noise parameters in model state. Set 0 in case of a deterministic model
 *  \param time0     O   Time instance of the initial state state. The time object is already created
 *  \param snamnoise O   Name of the substate containing the noise parameters. The string is already created
 *  \param husrdata  O   Handle that can be used for storing instance specific data
 *  \param ierr      O   Return flag CTA_OK if successful
*/
void usr_create(CTA_Handle *hinput,  CTA_TreeVector *state, CTA_TreeVector *sbound, 
                CTA_TreeVector *sparam, int *nnoise, CTA_Time *time0, 
                CTA_String *snamnoise, CTA_Handle *husrdata, int *ierr);

/** \brief Compute timestep(s)
 *  This routine is computes several timesteps over a giving timespan.
 *
 *  \param timespan  I  Timespan to simulate
 *  \param state     IO State vector
 *  \param saxpyforc I  Offset on models forcings
 *  \param baddnoise I  flag (CTA_TRUE/CTA_FALSE) whether to add noise
 *  \param sparam    I  Model parameters
 *  \param husrdata  IO Instance specific data
 *  \param ierr      O  Return flag CTA_OK if successful
*/
void usr_compute(CTA_Time *timespan, CTA_TreeVector *state, CTA_TreeVector *saxpyforc,
                     int *baddnoise, CTA_TreeVector *sparam, CTA_Handle *husrdata,
                     int *ierr);

/** \brief Return the root of noise covariance matrix
 *  This routine is responsible for returning the covariance matrix of the noise parameters.
 *
 *  \param colsvar         O   Covariance of noise parameters array of noise. 
 *                             Represented als an array (nnoise) of tree-vectors.
 *                             Note the sub-tree-vectors are already allocated.
 *  \param nnoise          I   Number of noise parameters
 *  \param husrdata        IO  Instance specific data
 *  \param ierr            O   Return flag CTA_OK if successful
*/
void usr_covar(CTA_TreeVector *colsvar, int *nnoise, CTA_Handle *husrdata, int *ierr);

/** \brief Return values that correspond to observed values
 *  This routine is responsible for the transformation of the state-vector to the observations. 
 *
 *  \param state    I  state vector
 *  \param hdescr   I  Observation description of observations
 *  \param vval     O  Model (state) values corresponding to observations in hdescr
 *  \param husrdata IO Instance specific data
 *  \param ierr     O  Return flag CTA_OK if successful
*/
void usr_obs(CTA_TreeVector *state, CTA_ObsDescr *hdescr, CTA_Vector *vval,
                CTA_Handle *husrdata, int *ierr);


/** \brief Select criterion for observations that can be used for the model.
 *  This routine is responsible for producing a selection criterion that will filter out all invalid observations.
 *  Invalid observations are observations for which the model cannot produce a corresponding value. For example
 *  observations that are outside the computational domain.
 *
 *  \param state    I  state vector
 *  \param ttime    I  timespan for selection
 *  \param hdescr   I  observation description of all available observations
 *  \param sselect  O  The select criterion to filter out all invalid observations
 *  \param husrdata IO Instance specific data
 *  \param ierr     O  Return flag CTA_OK if successful
*/
void usr_obssel(CTA_TreeVector *state, CTA_Time *ttime, CTA_ObsDescr *hdescr,
                CTA_String *sselect, CTA_Handle *husrdata, int* ierr);


void usr_SP_Model_CreateFunc(){
CTA_Intf hintf;
CTA_Func func;

   CTA_Func_Create("usr_create",  usr_create,  hintf, &func);
   CTA_Func_Create("usr_compute", usr_compute, hintf, &func);
   CTA_Func_Create("usr_covar",   usr_covar,   hintf, &func);
   CTA_Func_Create("usr_obs",     usr_obs,     hintf, &func);
   CTA_Func_Create("usr_obssel",  usr_obssel,  hintf, &func);
}

void CREATEFUNC_F77(){
   usr_CreateFunc();
}

