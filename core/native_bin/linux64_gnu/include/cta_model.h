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

#ifndef CTA_MODEL_H
#define CTA_MODEL_H
#include "cta_handles.h"
#include "cta_datatypes.h"
#include "cta_functions.h"
#include "cta_treevector.h"
#include "cta_time.h"
#include "cta_obsdescr.h"

/* Function Handle */
typedef CTA_Handle CTA_Model;
typedef CTA_Handle CTA_ModelClass;

/* parameters for different user functions */
#define I_CTA_MODEL_CREATE_SIZE      ( 0)
#define I_CTA_MODEL_CREATE_INIT      ( 1)
#define I_CTA_MODEL_FREE             ( 2)
#define I_CTA_MODEL_COMPUTE          ( 3)
#define I_CTA_MODEL_SET_STATE        ( 4)
#define I_CTA_MODEL_GET_STATE        ( 5)
#define CTA_MODEL_AXPY_STATE       ( 6)
#define CTA_MODEL_AXPY_MODEL       ( 7)
#define CTA_MODEL_SET_FORC         ( 8)
#define CTA_MODEL_GET_FORC         ( 9)
#define CTA_MODEL_AXPY_FORC        (10)
#define CTA_MODEL_SET_PARAM        (11)
#define CTA_MODEL_GET_PARAM        (12)
#define CTA_MODEL_AXPY_PARAM       (13)
#define CTA_MODEL_GET_STATESCALING (14)
#define CTA_MODEL_GET_TIMEHORIZON  (15)
#define CTA_MODEL_GET_CURRENTTIME  (16)

/* stochastic functions */
#define CTA_MODEL_GET_NOISE_COUNT  (17)
#define CTA_MODEL_GET_NOISE_COVAR  (18)

/* handling of observations */
#define CTA_MODEL_GET_OBSVALUES      (19)
#define CTA_MODEL_GET_OBSSELECT      (20)
#define CTA_MODEL_ANNOUNCE_OBSVALUES (21)

#define CTA_MODEL_ADD_NOISE          (22)
#define I_CTA_MODEL_EXPORT           (23)
#define I_CTA_MODEL_IMPORT           (24)

/* Methods for adjoint models */
#define CTA_MODEL_ADJ_SET_FORC       (25)
#define CTA_MODEL_ADJ_COMPUTE        (26)
#define CTA_MODEL_ADJ_PREPARE        (27)

/* Localization */
#define I_CTA_MODEL_GETOBSLOCALIZATION (28)

/* Loading and restoring of state */
#define CTA_MODEL_SAVE_INTERNALSTATE         (30)
#define CTA_MODEL_RESTORE_INTERNALSTATE      (31)
#define CTA_MODEL_RELEASE_INTERNALSTATE      (32)
#define CTA_MODEL_SAVE_PERSISTENTSTATE       (33)
#define CTA_MODEL_LOAD_PERSISTENTSTATE       (29)

#define CTA_MODEL_NUMFUNC                   (34)

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create a model instance
 *
 * \param hmodcl   I  model class of new instance
 * \param userdata IO user data needed for creation (depends on modelclass)
 * \param hmodel   O  receives handle of new model instance
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_Create(CTA_ModelClass hmodcl, CTA_Handle userdata, CTA_Model *hmodel);

/** \brief Compute model for given timespan
 *
 * \param hmodel   IO handle of model instance
 * \param htime    I  timespan for which to compute
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_Compute(CTA_Model hmodel, CTA_Time htime);

/** \brief Add noise during during the given timespan at 
 *        the Compute
 *
 * \note Noise is added in the compute-method
 * \param hmodel   IO handle of model instance
 * \param htime    I  timespan for which to compute adding noise
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_AddNoise(CTA_Model hmodel, CTA_Time htime);

/** \brief Set the internal state of the model.
 *
 * \note A copy of the state is set
 *
 * \param hmodel   IO handle of model instance
 * \param hstate   I  handle of new state
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_SetState(CTA_Model hmodel, CTA_TreeVector hstate);

/** \brief Get a copy of the internal state.
 *
 * \note Optionally a tree-vector is created. In that case the caller of this
 * method is responsible for freeing that tree-vector. The input state must be compatible
 * (same size and or composition) as the models internal state.
 * \note If *hstate == CTA_NULL a new object is created, user is responsible for freeing this object.
 *
 * \param hmodel   I  handle of model instance
 * \param hstate   IO receives state of the model, *hstate can be CTA_NULL on calling (see note)
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_GetState(CTA_Model hmodel, CTA_TreeVector *hstate);

/** \brief Perform axpy operation on the internal state.
 *
 * \note AXPY: y=alpha*x+y. y corresponds to the models
 *       internal state and x can be a state vector or a model
 
 * \param hmodel   IO handle of model instance (y)
 * \param alpha    I  alpha
 * \param hx       I  handle of x (state or model)
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_AxpyState(CTA_Model hmodel, double alpha, CTA_Handle hx);

/** \brief Get element-wise scaling for model state
 *
 * The values in the state-vector are compared on "importance" in various
 * algorithms like RRSQRT and COFFEE. The model state holds in general
 * various quantities like concentration, velicity, location etc in
 * arbitrary units. The scaling vector (that can be model state dependend)
 * makes it possible to meaningfull compare elements in the state-vector
 * for importance. Various methods are available like a transformation to
 * enery.
 *
 * The scaling vector represents a diagonal scaling matrix but is
 * respresented by a tree-vector. 
 *
 * \note The elementwise scaling is returned in the form of a tree-vector
 *       with same build-up as the tree-vector of the model state. The scaling vector
 *       is created whenever hscale==CTA_NULL on input, the caller is
 *       responsible for freeing this object.
 *
 * \param hmodel   I  handle of model instance
 * \param hscale   IO receives state scaling vector for the model state,
                      *hstate can be CTA_NULL on calling (see note)
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_GetStateScaling(CTA_Model hmodel, CTA_TreeVector *hscale);


/** \brief Set the models forcings.
 *
 * \note Set the forcings (constant) for the given timespan.
 *       The model will fall back to its own forcings definition
 *       outside the given timespan.
 *
 * \param hmodel   IO handle of model instance
 * \param tspan    I  time span on which to set the forcing values
 * \param hforc    I  handle of vector with new forcings
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_SetForc(CTA_Model hmodel, CTA_Time tspan, CTA_TreeVector hforc);

/** \brief Get a copy of the values of the models forcings
 *
 * \note Optionally a tree-vector is created in that case the caller of this
 * method is responsible for freeing that tree-vector. The input tree-vector
 * must be compatible (same size and or composition) as the models
 * internal tree-vector representing the forcings.
 * If the forcings of the model are not constant for the given timespan
 * the result is dependent on the model-implementation
 * \note If *hforc == CTA_NULL a new object is created, user is responsible for freeing this object.
 *
 * \param hmodel   I  handle of model instance
 * \param tspan    I  timespan for wich the given forcings are valid
 * \param hforc    IO receives models forcings, *hforc can be CTA_NULL on calling (see note)
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_GetForc(CTA_Model hmodel, CTA_Time tspan, CTA_TreeVector *hforc);

/** \brief Perform axpy operation on the models forcings.
 *
 * \note AXPY: y=alpha*x+y. y corresponds to the models
 *       internal forcings.
 *       The adjustment to the forcings (alpha*x) is only valid for the given 
 *       time span. Note that the model will use y(t)+x for the given time span
 *       where y(t) denotes the default forcings of the model.
 *
 * \param hmodel   IO handle of model instance (y)
 * \param tspan    I  time span for wich the given forcings are valid
 * \param alpha    I  scalar
 * \param hx       I  handle of forcings tree-vector x
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_AxpyForc( CTA_Model hmodel, CTA_Time tspan, double alpha, CTA_TreeVector hx);

/** \brief Set parameters of the model.
 *
 * \param hmodel   IO handle of model instance
 * \param hparam   I  handle of parameters vector
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_SetParam(CTA_Model hmodel, CTA_TreeVector hparam);

/** \brief Get a copy of the parameters of the model.
 *
 * \note Optionally a tree-vector is created in that case the caller of this
 * method is responsible for freeing that tree-vector. The input tree-vector
 * must be compatible (same size and or composition) as the models
 * internal tree-vector representing the parameters.
 * \note If *hforc == CTA_NULL a new object is created, user is responsible for freeing this object.
 *
 * \param hmodel   I  handle of model instance
 * \param hparam   IO receives model forcings, *hforc can equal CTA_NULL on calling (see note)
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_GetParam(CTA_Model hmodel, CTA_TreeVector *hparam);

/** \brief Perform axpy operation on the models parameters.
 *
 * \note AXPY: y=alpha*x+y where y corresponds to the models
 *       internal parameters.
 *
 * \param hmodel   IO handle of model instance (y)
 * \param alpha    I  alpha
 * \param hx       I  handle of treevector of parameters (x)
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_AxpyParam( CTA_Model hmodel, double alpha, CTA_TreeVector hx);

/** \brief Return the timehorizon on the model.
 * The time horizon is the initial overal simulation span for which the mode is configured 
 *
 * \param hmodel   I handle of model instance
 * \param tHorizon I time horizon of model
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_GetTimeHorizon( CTA_Model hmodel, CTA_Time tHorizon);

/** \brief Return the current time of the model.
 *
 * \param hmodel   I handle of model instance
 * \param tCurrent I time corresponding the the model state
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_GetCurrentTime( CTA_Model hmodel, CTA_Time tCurrent);

/** \brief Get covariance matrix of noise parameters.
 *
 * \note ONLY for Stochastic models.
 *       The covariance matrix is represented by an array
 *       of tree-vectors (columns of the matrix)
 *       optionally a tree-vector is created in that case the caller of this
 *       method is responsible for freeing that tree-vector. The input tree-vector
 *       must be compatible (same size and or composition) as the models
 *       internal tree-vector.
 * \note If hstmat[icol] == CTA_NULL a new object is created, user is responsible for freeing this object.
 *
 * \param hmodel   I  handle of model instance
 * \param hstmat   O  receives array of tree-vectors, *hstmat can equal CTA_NULL on calling (see note)
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_GetNoiseCovar(CTA_Model hmodel, CTA_TreeVector *hstmat);

/** \brief Get number of noise parameters: the number of columns of the noise covariance matrix.
 *
 * \note ONLY for Stochastic models. 
 *
 * \param hmodel   I  handle of model instance
 * \param nnoise   O  receives number of noise parameters
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_GetNoiseCount(CTA_Model hmodel,int *nnoise);

/** \brief Free model instance.
 *
 * \note ONLY for Stochastic models.
 *
 * \param hmodel   IO handle of model instance, replaced by CTA_NULL on return
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_Free(CTA_Model *hmodel);

/** \brief Announce to the model what observations will be requested.
 *
 *  Before the compute method this method is used to announce what
 *  obeservation will be requested after the CTA_Model_Compute using the
 *  CTA_Model_GetObsvalues method.
 *
 *  For some simulation models it is more efficient to do a single simulation
 *  (a single CTA_Model_Compute call) for a particular simulation span then
 *  simulating the same simulation span in a number of steps (multiple
 *  CTA_Model_Compute calls).
 *  
 *  This method can be used to announce for what observations the model
 *  must provide a prediction in advance. This method must be called prior
 *  to the CTA_Compute method and makes it possible to perform simulations
 *  over a longer time interval without the need to interupt the computations
 *  in order to get the predictions at intermediate time instances.
 *
 *  Notes on the behavior of the method:
 *  - The observation description used in the first CTA_Model_GetObsValues
 *    after the compute MUST be the same as the observation description
 *    used in the announce.
 *  - All observations that are announced MUST be in the timespan of the
 *    following CTA_Model_Compute.
 *  - The announced observations can only be retreved ONCE after the 
 *    CTA_Model_Compute.
 *  - A CTA_Model_SetState or CTA_Model_AxpyState will reset the announced 
 *    CTA_Model_AnnounceObsValues administration (since stored predictions
 *    might not be valid anymore)
 *
 * \param hmodel   I  handle of model instance
 * \param hdescr   I  observation description component
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_AnnounceObsValues(CTA_Model hmodel, CTA_ObsDescr hdescr);

/** \brief Get (interpolate) the models internal state to the
 *   observations described as specified in the observation
 *   description component.
 *
 * \note The interface supports a the time instance for time-interpolation.
 *       It depends on the model whether and how this is supported.
 *
 * \param hmodel   I  handle of model instance
 * \param htime    I  time instance (for checking and time-interpolation if
 *                    supported by model)
 * \param hdescr   I  observation description component
 * \param values   O  receives values of the models internal state corresponding to
 *                    observations as described in hdescr
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_GetObsValues(CTA_Model hmodel, CTA_Time htime,
   CTA_ObsDescr hdescr, CTA_Vector values);


/** \brief Get for each observation a localization scaling vector 
 *  
 * \param hmodel   I  handle of model instance
 * \param hdescr   I  observation description for which we want localization scaling vectors
 * \param distance I  characteristic distance
 * \param locVecs  O  costa vector of handles to treevectors (scaling vectors). The treevectors
 *                    are created when the indices are CTA_NULL on entry
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_GetObsLocalization( CTA_Model hmodel, 
                   CTA_ObsDescr hdescr, double distance, CTA_Vector locVecs);


/** \brief Get a query for the stochastic observer in order to
 *  filter out the observations that can actually be provided by the model.
 *
 * \param hmodel   I  handle of model instance
 * \param htime    I  time instance
 * \param hdescr   I  observation description component
 * \param sselect  O  receives a query to filter out the observations, must exist before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_GetObsSelect(CTA_Model hmodel, CTA_Time htime,
   CTA_ObsDescr hdescr, CTA_String sselect);

/* \brief  Save the current state of the model in a treevector
 *
 * \param hmodel     I  handle of model instance
 * \param instanceID O  ID to retrieve the CTA_Handle to the treevector
 * \return error status: CTA_OK if successful
*/
CTAEXPORT int CTA_Model_SaveInternalState(CTA_Model hmodel, CTA_String *instanceID);

/* \brief  Restore the state of the model to a previously saved state
 *
 * \param hmodel     I  handle of model instance
 * \param instanceID O  ID to retrieve the CTA_Handle to the treevector with the previously saved state
 * \return error status: CTA_OK if successful
 */
  CTAEXPORT int CTA_Model_RestoreInternalState(CTA_Model hmodel, CTA_String instanceID);

/* \brief  Free memory of a previously saved state
 *
 * \param hmodel     I  handle of model instance
 * \param instanceID I  ID to retrieve the CTA_Handle to the treevector to release
 * \return error status: CTA_OK if successful
 */
  CTAEXPORT int CTA_Model_ReleaseInternalState(CTA_Model hmodel, CTA_String instanceID);

/* \brief  Save a previously saved internal state to file
 *
 * \param hmodel     I  handle of model instance
 * \param filename   I  name of file to be written
 * \param instanceID I  ID to retrieve the CTA_Handle to the treevector to export to file
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_SavePersistentState(CTA_Model hmodel, CTA_String filename, CTA_String instanceID);

/* \brief  Load a model state from file and save it as an internal state
 *
 * \param hmodel     I  handle of model instance
 * \param filename   I  name of file to read state from
 * \param instanceID O  ID to retrieve the CTA_Handle to the treevector that holds the state
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_LoadPersistentState(CTA_Model hmodel, CTA_String filename, CTA_String *instanceID);

/** \brief Export the whole internal state of a model
 *  This export function will export the whole state of the model such that
 *  a so called "restart" start from this point yielding the same results.
 *  There are no ruled on the format that is used to store the data.
 *  Various extra otions are valid but a model will in most cases support an export 
 *  to a file and to a COSTA pack object.
 *  
 *
 * \param hmodel   I  handle of model instance
 * \param hexport  I  target for export e.g. CTA_File or CTA_Pack
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_Export( CTA_Model hmodel, CTA_Handle hexport);

/** \brief Import the whole internal state of a model
 *  After the inport the models internal state is exactly the same as the point that
 *  the export was created using CTA_Model_Export.
 *  
 *
 * \param hmodel   I  handle of model instance
 * \param himport  I  handle with data created by CTA_MODEL_Export e.g. CTA_File or CTA_Pack
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_Import( CTA_Model hmodel, CTA_Handle himport);

/** \brief 
 *  The adjoint model is mostly forced with scaled observation minus model residuals
 *  This routines is used to set the forcing.
 *
 * \param hmodel   IO handle of model instance
 * \param hdescr   I  handle to observation descriptions needed for interpretation of the forcing vector
 * \param vforc    I  vector with the forcing values
 *
 * \return error status: CTA_OK if successful
 */
CTANOEXPORT int CTA_Model_AdjSetointForc(CTA_Model hmodel, CTA_ObsDescr hdescr, CTA_Vector vforc);

/** \brief 
 *  bla
 *  HOMEWORK FOR JULIUS!
 *
 * \param hmodel   I  handle of model instance
 *
 * \return error status: CTA_OK if successful
 */
CTANOEXPORT int CTA_Model_AdjPrepare(CTA_Model hmodel, CTA_Time time);

/** \brief 
 *  bla
 *  HOMEWORK FOR JULIUS!
 *
 * \param hmodel   I  handle of model instance
 *
 * \return error status: CTA_OK if successful
 */
CTANOEXPORT int CTA_Model_AdjCompute(CTA_Model hmodel, CTA_Time time);

/*Initernal non-user routines */
int CTAI_Model_PerformTimesteps(
   CTA_Model hmodel,         /* handle of model */
   CTA_Function *function,   /* Function that must be called */
   CTA_Time htime,           /* timespan to compute */
   int mindBarrier           /* Flag to activate barrier check fixed timestepping */
);
#ifdef __cplusplus
}
#endif


#endif

