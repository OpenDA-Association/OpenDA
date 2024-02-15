module cta_f90_model

  implicit none

  public

  !  \brief Create a model instance
  ! 
  !  \param hmodcl   I  model class of new instance
  !  \param userdata IO user data needed for creation (depends on modelclass)
  !  \param hmodel   O  receives handle of new model instance
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_Create
    subroutine CTA_Model_Create( hmodcl, userdata, hmodel, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodcl
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  userdata
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hmodel
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_Create
  end interface

  !  \brief Compute model for given timespan
  ! 
  !  \param hmodel   IO handle of model instance
  !  \param htime    I  timespan for which to compute
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_Compute
    subroutine CTA_Model_Compute( hmodel, htime, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_Compute
  end interface

  !  \brief Add noise during during the given timespan at
  !         the Compute
  ! 
  !  \note Noise is added in the compute-method
  !  \param hmodel   IO handle of model instance
  !  \param htime    I  timespan for which to compute adding noise
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_AddNoise
    subroutine CTA_Model_AddNoise( hmodel, htime, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_AddNoise
  end interface

  !  \brief Set the internal state of the model.
  ! 
  !  \note A copy of the state is set
  ! 
  !  \param hmodel   IO handle of model instance
  !  \param hstate   I  handle of new state
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_SetState
    subroutine CTA_Model_SetState( hmodel, hstate, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hstate
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_SetState
  end interface

  !  \brief Get a copy of the internal state.
  ! 
  !  \note Optionally a tree-vector is created. In that case the caller of this
  !  method is responsible for freeing that tree-vector. The input state must be compatible
  !  (same size and or composition) as the models internal state.
  !  \note If *hstate == CTA_NULL a new object is created, user is responsible for freeing this object.
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param hstate   IO receives state of the model, *hstate can be CTA_NULL on calling (see note)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetState
    subroutine CTA_Model_GetState( hmodel, hstate, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hstate
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetState
  end interface

  !  \brief Perform axpy operation on the internal state.
  ! 
  !  \note AXPY: y=alpha*x+y. y corresponds to the models
  !        internal state and x can be a state vector or a model
  ! 
  !  \param hmodel   IO handle of model instance (y)
  !  \param alpha    I  alpha
  !  \param hx       I  handle of x (state or model)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_AxpyState
    subroutine CTA_Model_AxpyState( hmodel, alpha, hx, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmodel
      real(8)                       , intent(in   )     ::  alpha
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hx
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_AxpyState
  end interface

  !  \brief Get element-wise scaling for model state
  ! 
  !  The values in the state-vector are compared on "importance" in various
  !  algorithms like RRSQRT and COFFEE. The model state holds in general
  !  various quantities like concentration, velicity, location etc in
  !  arbitrary units. The scaling vector (that can be model state dependend)
  !  makes it possible to meaningfull compare elements in the state-vector
  !  for importance. Various methods are available like a transformation to
  !  enery.
  ! 
  !  The scaling vector represents a diagonal scaling matrix but is
  !  respresented by a tree-vector.
  ! 
  !  \note The elementwise scaling is returned in the form of a tree-vector
  !        with same build-up as the tree-vector of the model state. The scaling vector
  !        is created whenever hscale==CTA_NULL on input, the caller is
  !        responsible for freeing this object.
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param hscale   IO receives state scaling vector for the model state,
  ! hstate can be CTA_NULL on calling (see note)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetStateScaling
    subroutine CTA_Model_GetStateScaling( hmodel, hscale, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hscale
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetStateScaling
  end interface

  !  \brief Set the models forcings.
  ! 
  !  \note Set the forcings (constant) for the given timespan.
  !        The model will fall back to its own forcings definition
  !        outside the given timespan.
  ! 
  !  \param hmodel   IO handle of model instance
  !  \param tspan    I  time span on which to set the forcing values
  !  \param hforc    I  handle of vector with new forcings
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_SetForc
    subroutine CTA_Model_SetForc( hmodel, tspan, hforc, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  tspan
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hforc
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_SetForc
  end interface

  !  \brief Get a copy of the values of the models forcings
  ! 
  !  \note Optionally a tree-vector is created in that case the caller of this
  !  method is responsible for freeing that tree-vector. The input tree-vector
  !  must be compatible (same size and or composition) as the models
  !  internal tree-vector representing the forcings.
  !  If the forcings of the model are not constant for the given timespan
  !  the result is dependent on the model-implementation
  !  \note If *hforc == CTA_NULL a new object is created, user is responsible for freeing this object.
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param tspan    I  timespan for wich the given forcings are valid
  !  \param hforc    IO receives models forcings, *hforc can be CTA_NULL on calling (see note)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetForc
    subroutine CTA_Model_GetForc( hmodel, tspan, hforc, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  tspan
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hforc
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetForc
  end interface

  !  \brief Perform axpy operation on the models forcings.
  ! 
  !  \note AXPY: y=alpha*x+y. y corresponds to the models
  !        internal forcings.
  !        The adjustment to the forcings (alpha*x) is only valid for the given
  !        time span. Note that the model will use y(t)+x for the given time span
  !        where y(t) denotes the default forcings of the model.
  ! 
  !  \param hmodel   IO handle of model instance (y)
  !  \param tspan    I  time span for wich the given forcings are valid
  !  \param alpha    I  scalar
  !  \param hx       I  handle of forcings tree-vector x
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_AxpyForc
    subroutine CTA_Model_AxpyForc( hmodel, tspan, alpha, hx, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  tspan
      real(8)                       , intent(in   )     ::  alpha
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hx
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_AxpyForc
  end interface

  !  \brief Set parameters of the model.
  ! 
  !  \param hmodel   IO handle of model instance
  !  \param hparam   I  handle of parameters vector
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_SetParam
    subroutine CTA_Model_SetParam( hmodel, hparam, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hparam
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_SetParam
  end interface

  !  \brief Get a copy of the parameters of the model.
  ! 
  !  \note Optionally a tree-vector is created in that case the caller of this
  !  method is responsible for freeing that tree-vector. The input tree-vector
  !  must be compatible (same size and or composition) as the models
  !  internal tree-vector representing the parameters.
  !  \note If *hforc == CTA_NULL a new object is created, user is responsible for freeing this object.
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param hparam   IO receives model forcings, *hforc can equal CTA_NULL on calling (see note)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetParam
    subroutine CTA_Model_GetParam( hmodel, hparam, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hparam
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetParam
  end interface

  !  \brief Perform axpy operation on the models parameters.
  ! 
  !  \note AXPY: y=alpha*x+y where y corresponds to the models
  !        internal parameters.
  ! 
  !  \param hmodel   IO handle of model instance (y)
  !  \param alpha    I  alpha
  !  \param hx       I  handle of treevector of parameters (x)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_AxpyParam
    subroutine CTA_Model_AxpyParam( hmodel, alpha, hx, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmodel
      real(8)                       , intent(in   )     ::  alpha
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hx
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_AxpyParam
  end interface

  !  \brief Return the timehorizon on the model.
  !  The time horizon is the initial overal simulation span for which the mode is configured
  ! 
  !  \param hmodel   I handle of model instance
  !  \param tHorizon I time horizon of model
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetTimeHorizon
    subroutine CTA_Model_GetTimeHorizon( hmodel, tHorizon, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  tHorizon
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetTimeHorizon
  end interface

  !  \brief Return the current time of the model.
  ! 
  !  \param hmodel   I handle of model instance
  !  \param tCurrent I time corresponding the the model state
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetCurrentTime
    subroutine CTA_Model_GetCurrentTime( hmodel, tCurrent, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  tCurrent
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetCurrentTime
  end interface

  !  \brief Get covariance matrix of noise parameters.
  ! 
  !  \note ONLY for Stochastic models.
  !        The covariance matrix is represented by an array
  !        of tree-vectors (columns of the matrix)
  !        optionally a tree-vector is created in that case the caller of this
  !        method is responsible for freeing that tree-vector. The input tree-vector
  !        must be compatible (same size and or composition) as the models
  !        internal tree-vector.
  !  \note If hstmat[icol] == CTA_NULL a new object is created, user is responsible for freeing this object.
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param hstmat   O  receives array of tree-vectors, *hstmat can equal CTA_NULL on calling (see note)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetNoiseCovar
    subroutine CTA_Model_GetNoiseCovar( hmodel, hstmat, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hstmat
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetNoiseCovar
  end interface

  !  \brief Get number of noise parameters: the number of columns of the noise covariance matrix.
  ! 
  !  \note ONLY for Stochastic models.
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param nnoise   O  receives number of noise parameters
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetNoiseCount
    subroutine CTA_Model_GetNoiseCount( hmodel, nnoise, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer                       , intent(out  )     ::  nnoise
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetNoiseCount
  end interface

  !  \brief Free model instance.
  ! 
  !  \note ONLY for Stochastic models.
  ! 
  !  \param hmodel   IO handle of model instance, replaced by CTA_NULL on return
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_Free
    subroutine CTA_Model_Free( hmodel, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmodel
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_Free
  end interface

  !  \brief Announce to the model what observations will be requested.
  ! 
  !   Before the compute method this method is used to announce what
  !   obeservation will be requested after the CTA_Model_Compute using the
  !   CTA_Model_GetObsvalues method.
  ! 
  !   For some simulation models it is more efficient to do a single simulation
  !   (a single CTA_Model_Compute call) for a particular simulation span then
  !   simulating the same simulation span in a number of steps (multiple
  !   CTA_Model_Compute calls).
  ! 
  !   This method can be used to announce for what observations the model
  !   must provide a prediction in advance. This method must be called prior
  !   to the CTA_Compute method and makes it possible to perform simulations
  !   over a longer time interval without the need to interupt the computations
  !   in order to get the predictions at intermediate time instances.
  ! 
  !   Notes on the behavior of the method:
  !   - The observation description used in the first CTA_Model_GetObsValues
  !     after the compute MUST be the same as the observation description
  !     used in the announce.
  !   - All observations that are announced MUST be in the timespan of the
  !     following CTA_Model_Compute.
  !   - The announced observations can only be retreved ONCE after the
  !     CTA_Model_Compute.
  !   - A CTA_Model_SetState or CTA_Model_AxpyState will reset the announced
  !     CTA_Model_AnnounceObsValues administration (since stored predictions
  !     might not be valid anymore)
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param hdescr   I  observation description component
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_AnnounceObsValues
    subroutine CTA_Model_AnnounceObsValues( hmodel, hdescr, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hdescr
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_AnnounceObsValues
  end interface

  !  \brief Get (interpolate) the models internal state to the
  !    observations described as specified in the observation
  !    description component.
  ! 
  !  \note The interface supports a the time instance for time-interpolation.
  !        It depends on the model whether and how this is supported.
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param htime    I  time instance (for checking and time-interpolation if
  !                     supported by model)
  !  \param hdescr   I  observation description component
  !  \param values   O  receives values of the models internal state corresponding to
  !                     observations as described in hdescr
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetObsValues
    subroutine CTA_Model_GetObsValues( hmodel, htime, hdescr, values, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hdescr
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  values
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetObsValues
  end interface

  !  \brief Get for each observation a localization scaling vector
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param hdescr   I  observation description for which we want localization scaling vectors
  !  \param distance I  characteristic distance
  !  \param locVecs  O  costa vector of handles to treevectors (scaling vectors). The treevectors
  !                     are created when the indices are CTA_NULL on entry
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetObsLocalization
    subroutine CTA_Model_GetObsLocalization( hmodel, hdescr, distance, locVecs, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hdescr
      real(8)                       , intent(in   )     ::  distance
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  locVecs
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetObsLocalization
  end interface

  !  \brief Get a query for the stochastic observer in order to
  !   filter out the observations that can actually be provided by the model.
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param htime    I  time instance
  !  \param hdescr   I  observation description component
  !  \param sselect  O  receives a query to filter out the observations, must exist before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetObsSelect
    subroutine CTA_Model_GetObsSelect( hmodel, htime, hdescr, sselect, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hdescr
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  sselect
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetObsSelect
  end interface

  !  \brief Export the whole internal state of a model
  !   This export function will export the whole state of the model such that
  !   a so called "restart" start from this point yielding the same results.
  !   There are no ruled on the format that is used to store the data.
  !   Various extra otions are valid but a model will in most cases support an export
  !   to a file and to a COSTA pack object.
  ! 
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param hexport  I  target for export e.g. CTA_File or CTA_Pack
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_Export
    subroutine CTA_Model_Export( hmodel, hexport, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hexport
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_Export
  end interface

  !  \brief Import the whole internal state of a model
  !   After the inport the models internal state is exactly the same as the point that
  !   the export was created using CTA_Model_Export.
  ! 
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param himport  I  handle with data created by CTA_MODEL_Export e.g. CTA_File or CTA_Pack
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_Import
    subroutine CTA_Model_Import( hmodel, himport, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  himport
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_Import
  end interface

  !  \brief Get the number of domains for local analysis
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param distance I  characteristic distance
  !  \param ndomains O  number of domains
  !  \param locVecs  O  costa vector of handles to treevectors (scaling vectors). The treevectors
  !                     are created when the indices are CTA_NULL on entry
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetNumDomains
    subroutine CTA_Model_GetNumDomains( hmodel, distance, ndomains, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      real(8)                       , intent(in   )     ::  distance
      integer                       , intent(out  )     ::  ndomains
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetNumDomains
  end interface

  !  \brief Get selection of observations that are relevnet for assimilation in the given domain
  ! 
  !  \param hmodel    I  handle of model instance
  !  \param hdescr    I  observation description of all observations
  !  \param distance  I  characteristic distance
  !  \param idomain   I  domain number
  !  \param selection O  costa vector with the indices of the relevant observations (0 based)
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetObsSelector
    subroutine CTA_Model_GetObsSelector( hmodel, hdescr, distance, idomain, selection, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hdescr
      real(8)                       , intent(in   )     ::  distance
      integer                       , intent(in   )     ::  idomain
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  selection
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetObsSelector
  end interface

  !  \brief Get for each observation a localization scaling vector for single domain
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param hdescr   I  observation description for which we want localization scaling vectors
  !  \param distance I  characteristic distance
  !  \param idomain  I  domain number
  !  \param locVecs  O  costa vector of handles to treevectors (scaling vectors). The treevectors
  !                     are created when the indices are CTA_NULL on entry
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetObsLocalizationDomain
    subroutine CTA_Model_GetObsLocalizationDomain( hmodel, hdescr, distance, idomain, locVecs, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hdescr
      real(8)                       , intent(in   )     ::  distance
      integer                       , intent(in   )     ::  idomain
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  locVecs
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetObsLocalizationDomain
  end interface

  !  \brief Get a copy of the internal state.
  ! 
  !  \note Optionally a tree-vector is created. In that case the caller of this
  !  method is responsible for freeing that tree-vector. The input state must be compatible
  !  (same size and or composition) as the models internal state.
  !  \note If *hstate == CTA_NULL a new object is created, user is responsible for freeing this object.
  ! 
  !  \param hmodel   I  handle of model instance
  !  \param idomain  I  domain number
  !  \param hstate   IO receives state of the model, *hstate can be CTA_NULL on calling (see note)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_GetStateDomain
    subroutine CTA_Model_GetStateDomain( hmodel, idomain, hstate, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmodel
      integer                       , intent(in   )     ::  idomain
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hstate
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_GetStateDomain
  end interface

  !  \brief Perform axpy operation on the internal state for a single domain
  ! 
  !  \note AXPY: y=alpha*x+y. y corresponds to the models
  !        internal state and x can be a state vector or a model
  ! 
  !  \param hmodel   IO handle of model instance (y)
  !  \param alpha    I  alpha
  !  \param hx       I  handle of x (state or model)
  !  \param idomain  I  domain number
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_AxpyStateDomain
    subroutine CTA_Model_AxpyStateDomain( hmodel, alpha, idomain, hx, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmodel
      real(8)                       , intent(in   )     ::  alpha
      integer                       , intent(in   )     ::  idomain
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hx
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_AxpyStateDomain
  end interface


end module cta_f90_model

