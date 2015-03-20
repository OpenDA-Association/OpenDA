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
\file  cta_usr_model.h

\brief In this file a description is given of the interface of user model functions.
When creating your own user model class use the following as template.

The Usr_Model to which is being referred in this template can be substituted by your own user model object.

<b>Step 1</b>: for creating your own user model class call the function CTA_Model_DefineClass().

Example:

\code
typedef CTA_Handle CTA_ModelClass;

CTA_Func h_func[CTA_MODEL_NUMFUNC];
CTA_ModelClass my_own_model_class;

ierr=CTA_Func_Create(" ",&usr_model_create_size, hintf, &h_func[I_CTA_MODEL_CREATE_SIZE]);
//...for all implementation functions...

CTA_Model_DefineClass("classname", h_func, &my_own_model_class);\endcode

Making a user method class involves the implementation of the following functions:

CTA_MODEL_CREATE_SIZE  \n
CTA_MODEL_CREATE_INIT  \n
CTA_MODEL_FREE         \n
CTA_MODEL_COMPUTE      \n
CTA_MODEL_SET_STATE    \n
CTA_MODEL_GET_STATE    \n
CTA_MODEL_AXPY_STATE   \n
CTA_MODEL_AXPY_MODEL   \n
CTA_MODEL_SET_FORC     \n
CTA_MODEL_GET_FORC     \n
CTA_MODEL_AXPY_FORC    \n
CTA_MODEL_SET_PARAM    \n
CTA_MODEL_GET_PARAM    \n
CTA_MODEL_AXPY_PARAM   \n
CTA_MODEL_IMPORT       \n
CTA_MODEL_EXPORT       \n
CTA_MODEL_GET_STATESCALING

For creating an implementation function see documentation of CTA_Func_Create().

<b>Step 2</b>: to create an object of the newly defined model class call CTA_Model_Create() in the
same way as creating a CTA_Model object but with a different class handle, i.e. the user class handle from step 1 above.

Example:

\code
Usr_Model usrmodel; //user model object
CTA_Handle userdata = CTA_NULL;
CTA_Model_Create(my_own_model_class, &userdata, &usrmodel);
\endcode
\n
<b>Note 1: </b> with object data is meant only the object itself including pointer(s) to its contents, but
not the contents themselves.\n\n
*/


//#define CTA_MODEL_CREATE_SIZE 
/** \brief Implementation that forms part of the create process. 
 * 
 * Must give the memory size of a new user model object.
 *
 *  Example: 
 *  \code
//in header file:
typedef struct {
   //your own user object data goes here...
}USR_MODEL;

//user implementation:
void usr_model_create_size(...){
   *memsize = sizeof(USR_MODEL);
   *retval = CTA_OK;
}
 \endcode
 *
 * \note At index I_CTA_MODEL_CREATE_SIZE in the function list of the class descriptor.
 *
 * \param userdata IO pointer to user data
 * \param memsize  O  must receive the number of bytes which are necessary to store one
                      user method class, with a pointer to the contents (data), but without the
                      contents themselves
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_create_size(void *userdata, int *memsize, int *retval);


//#define CTA_MODEL_CREATE_INIT
/** \brief Implementation that forms part of the create process.
 *
 * The user model object needs to be made ready for use.
 *
 * \note At index CTA_MODEL_CREATE_INIT in the function list of the class descriptor.
 *
 * \param hmodel      I  handle of own model instance
 * \param objectdata  IO pointer to object data
 * \param userdata    IO pointer to user data
 * \param retval      O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_create_init(CTA_Model *hmodel, Usr_Model *objectdata, void *userdata, int *retval);


//#define CTA_MODEL_FREE
/** \brief Implementation for freeing the object data and associated resources.
 *
 * \note At index CTA_MODEL_FREE in the function list of the class descriptor.
 *
 * \param objectdata    IO pointer to object data
 * \param retval        O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_free(Usr_Model *objectdata, int *retval);


//#define CTA_MODEL_COMPUTE
/** \brief Implementation for computing a simulation of user model. A simulation is computed at the given time span.
 *
 * 
 *
 * \note At index CTA_MODEL_COMPUTE in the function list of the class descriptor.
 *
 * \param objectdata  IO pointer to object data
 * \param htime       I  handle of time object describing time span of simulation
 * \param retval      O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_compute(Usr_Model *objectdata, CTA_Time *htime, int *retval);


//#define CTA_MODEL_SET_STATE
/** \brief Implementation for setting model state.
 *
 * \note At index I_CTA_MODEL_SET_STATE in the function list of the class descriptor.
 *
 * \param objectdata IO pointer to object data
 * \param hstate     I  handle of tree-vector describing new model state
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_set_state(Usr_Model *objectdata, CTA_TreeVector *hstate, int *retval);


//#define CTA_MODEL_GET_STATE
/** \brief Implementation for getting a model state.
 *
 * \note At index I_CTA_MODEL_GET_STATE in the function list of the class descriptor.
 *
 * \param objectdata  I  pointer to object data
 * \param hstate      O  handle of tree-vector (if CTA_NULL a new object must be created and caller is responsible for freeing) that must receive model state description
 * \param retval      O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_get_state(Usr_Model *objectdata, CTA_Handle *hstate, int *retval);


//#define CTA_MODEL_AXPY_STATE
/** \brief Implementation for: modelstate=alpha*state+modelstate
 *
 * \note At index CTA_MODEL_AXPY_STATE in the function list of the class descriptor.
 *
 * \note If not implemented, a default implementation will be used.
 * \param objectdatay  IO  pointer to object data of model y
 * \param alpha        I  scalar
 * \param hstatex      I  handle of tree-vector  x
 * \param retval       O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_axpy_state(Usr_Model *objectdatay, double *alpha, CTA_TreeVector *hstatex, int *retval);


//#define CTA_MODEL_AXPY_MODEL
/** \brief Implementation for: modelstatey=alpha*modelstatex+modelstatey
 *
 * \note At index CTA_MODEL_AXPY_MODEL in the function list of the class descriptor.
 *
 * \note If not implemented, a default implementation will be used.
 * \param alpha       I  scalar
 * \param objectdatax IO pointer to object data of model x
 * \param objectdatay I  pointer to object data of model y
 * \param retval      O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_axpy_model(Usr_Model *objectdatay, double *alpha, Usr_Model *objectdatax, int *retval);


//#define CTA_MODEL_SET_FORC
/** \brief Implementation for setting the forcing values of the user model
 *
 * \note At index CTA_MODEL_SET_FORC in the function list of the class descriptor.
 *
 * \param objectdata IO pointer to object data
 * \param tspan      I  time span on which to set the forcing values
 * \param forc       I  handle of tree-vector object with new forcings
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_set_forc(Usr_Model *objectdata, CTA_Time *tspan, CTA_Handle *forc, int *retval);


//#define CTA_MODEL_GET_FORC
/** \brief Implementation for getting the forcing values.
 *
 * \note At index CTA_MODEL_GET_FORC in the function list of the class descriptor.
 * \note If forc equals CTA_NULL a new object is created, the caller is responsible for freeing it after usage
 *
 * \param objectdata I  pointer to object data
 * \param alpha      I  scalar
 * \param forc       I  handle of tree-vector object that must receive forcing values (if equal to CTA_NULL, it must be created and caller is responsible for freeing)
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_get_forc(Usr_Model *objectdata, double *alpha, CTA_TreeVector *forc, int *retval);


//#define CTA_MODEL_AXPY_FORC
/** \brief Implementation for performing axpy operation on the models forcings.
 *
 * \note At index CTA_MODEL_AXPY_FORC in the function list of the class descriptor.
 *
 * \note AXPY: y=alpha*x+y. y corresponds to the models
 *       internal forcings.
 *       The adjustment to the forcings (alpha*x) is only valid for the given 
 *       time span. Note that the model will use y(t)+x for the given time span
 *       where y(t) denotes the default forcings of the model.
 *
 * \param objectdata IO pointer to object data
 * \param tspan      I  handle of time span object
 * \param alpha      I  scalar
 * \param forc       I  handle of forcings x
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_axpy_forc(Usr_Model *objectdata, CTA_Time *tspan, int *alpha, CTA_TreeVector *forc, int *retval);


//#define CTA_MODEL_SET_PARAM
/** \brief Implementation for setting model parameters.
 *
 * \note At index CTA_MODEL_SET_PARAM in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to object data
 * \param hparam    I  handle of parameter object
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_set_param(Usr_Model *objectdata, CTA_TreeVector *hparam, int *retval);


//#define CTA_MODEL_GET_PARAM
/** \brief Implementation for getting model parameters.
 *
 * \note At index CTA_MODEL_GET_PARAM in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to object data
 * \param hparam     O  handle of object (if equal to CTA_NULL it must be created and caller is responsible for freeing) that must receive model parameters
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_get_param(Usr_Model *objectdata, CTA_TreeVector *hparam, int *retval);


//#define CTA_MODEL_AXPY_PARAM
/** \brief Implementation for performing axpy operation on the models parameters.
 *
 * \note At index CTA_MODEL_AXPY_PARAM in the function list of the class descriptor.
 *
 * \note AXPY: y=alpha*x+y where y corresponds to the models
 *       internal parameters.
 *
 * \param objectdata IO  pointer to object data
 * \param alpha     I  scalar
 * \param hparamx   I  handle of parameter x that is added to parameters of the model
 * \param retval    O  must receive return value of user implementation function
 * \return no return value
 */
void usr_model_axpy_param(Usr_Model *objectdata, double *alpha, CTA_TreeVector *hparamx, int *retval);

//#define CTA_MODEL_GET_NOISE_COUNT
/** \brief Implementation for returning the number of noise parameters:
 *         the number of columns of the noise covariance matrix.
 *
 * \note ONLY for Stochastic models. 
 *
 * \param objectdata IO  pointer to object data
 * \param nnoise      O  receives number of noise parameters
 * \param retval      O  error status: CTA_OK if successful
 */
void usr_model_get_noisecount(Usr_Model *objectdata, int *nnoise, int *retval);

//#define CTA_MODEL_GET_NOISE_COVAR
/** \brief Implementation of returning covariance matrix of noise parameters.
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
 * \param objectdata IO  pointer to object data
 * \param hstmat      O  receives array of tree-vectors, *hstmat can equal CTA_NULL on calling (see note)
 * \param retval      O  error status: CTA_OK if successful
 */
void usr_model_get_noisecovar(Usr_Model *objectdata, CTA_TreeVector *hstmat, int *retval);

//#define CTA_MODEL_GET_OBSVALUES
/** \brief Implementations to get (interpolate) the models internal state to the
 *   observations described as specified in the observation
 *   description component.
 *
 * \note The interface supports a the time instance for time-interpolation.
 *       It depends on the model whether and how this is supported.
 *
 * \param objectdata IO  pointer to object data
 * \param htime      I   time instance (for checking and time-interpolation if
 *                       supported by model)
 * \param hdescr     I   observation description component
 * \param values     O   receives values of the models internal state corresponding to
 *                       observations as described in hdescr
 * \param retval     O   error status: CTA_OK if successful
 */
void usr_model_getobsvalues(Usr_Model *objectdata, CTA_Time *htime, CTA_ObsDescr *hdescr, CTA_Vector *values, int *retval);

//#define CTA_MODEL_GET_OBSSELECT
/** \brief Implements to get a query for the stochastic observer in order to
 *  filter out the observations that can actually be provided by the model.
 *
 * \param objectdata IO  pointer to object data
 * \param htime    I  time instance
 * \param hdescr   I  observation description component
 * \param sselect  O  receives a query to filter out the observations, must exist before calling
 * \param retval     O   error status: CTA_OK if successful
 */
void usr_model_getobsselect(Usr_Model *objectdata, CTA_Time *htime, CTA_ObsDescr *hdescr, CTA_String *sselect, int *retval);

//#define CTA_MODEL_ADD_NOISE
/** \brief Add noise during during the given timespan at 
 *        the Compute
 *
 * \note Noise is added in the compute-method
 *
 * \param objectdata IO  pointer to object data
 * \param htime      I  timespan for which to compute adding noise
 * \param retval     O   error status: CTA_OK if successful
 */
void usr_model_addnoise(Usr_Model *objectdata, CTA_Time *htime, int *retval);

//#define CTA_MODEL_EXPORT
/** \brief Implements the export the whole internal state of a model
 *  This export function will export the whole state of the model such that
 *  a so called "restart" start from this point yielding the same results.
 *  There are no ruled on the format that is used to store the data.
 *  Various extra otions are valid but a model will in most cases support an export 
 *  to a file and to a COSTA pack object.
 *  
 *
 * \param objectdata IO  pointer to object data
 * \param hexport    I   target for export e.g. CTA_File or CTA_Pack
 * \param retval     O   error status: CTA_OK if successful
 */
void usr_model_export(Usr_Model *objectdata, CTA_Time *hexport, int *retval);

//#define CTA_MODEL_IMPORT
/** \brief Implements the import the whole internal state of a model
 *  After the inport the models internal state is exactly the same as the point that
 *  the export was created using CTA_Model_Export.
 *  
 *
 * \param objectdata IO  pointer to object data
 * \param himport  I  handle with data created by CTA_MODEL_Export e.g. CTA_File or CTA_Pack
 * \param retval     O   error status: CTA_OK if successful
 */
void usr_model_import(Usr_Model *objectdata, CTA_Time *himport, int *retval);


//#define cta_model_get_statescaling
/** \brief implements the return of an element wise scaling vector for the
 *         model state (method cta_model_getstatescaling)
 *  
 *
 * \param objectdata io  pointer to object data
 * \param hscale     o   tree-vector containing scaling vector. is created
 *                       when hscale==cta_null on input
 * \param retval     o   error status: cta_ok if successful
 */
void usr_model_getstatescaling(Usr_Model *objectdata, CTA_TreeVector *hscale, int *retval);

//#define CTA_MODEL_GETCURRENTTIME
/** \brief implements the return of the current time of the model instance
 *         (method CTA_Model_GetCurrentTime)
 *  
 *
 * \param objectdata io  pointer to object data
 * \param tCurrent    o  Current time of the model instance
 * \param retval     o   error status: cta_ok if successful
 */
void usr_model_getcurrenttime(Usr_Model *objectdata, int* tCurrent, int* retval);

//#define CTA_MODEL_GETTIMEHORIZON
/** \brief implements the return of the time horizon of the model instance
 *         (method CTA_Model_GetTimeHorizon)
 *  
 *
 * \param objectdata io  pointer to object data
 * \param tCurrent    o  Time horizon of the model instance
 * \param retval      o   error status: cta_ok if successful
 */
void usr_model_gettimehorizon(Usr_Model *objectdata, int* tHorizon, int* retval);


