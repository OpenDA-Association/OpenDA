module cta_f90_sobs

  implicit none

  public

  !  \brief Create a new class (=implementation) of a COSTA stochastic observer component.
  ! 
  !  \param name          I  name of the new stochastic observer class
  !  \param h_func        I  COSTA function handles for functions that implement class,
  !                          missing functions must have value CTA_NULL
  !  \param descrcl       I  class of the observation description that is created by stochastic observer
  !  \param hstochobscl   O  handle of new stochastic observer class
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_DefineClass
    subroutine CTA_SObs_DefineClass( name, h_func, descrcl, hstochobscl, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  name
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h_func(*)
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  descrcl
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hstochobscl
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_DefineClass
  end interface

  !  \brief Create an instance of a stocastic observer
  ! 
  !  \param hstochobscl I  stochastic observer class of new stochastic observer
  !  \param userdata    IO userdata for creation (depends on class)
  !  \param hstochobs   O  receives handle of new stochastic observer object
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_Create
    subroutine CTA_SObs_Create( hstochobscl, userdata, hstochobs, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hstochobscl
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  userdata
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hstochobs
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_Create
  end interface

  !  \brief Create a new stochastic observer that is subset of existing stochastic observer.
  ! 
  !  \param hsobsin  I  handle of the existing stochastic observer of
  !                     which  a selection is to be made
  !  \param userdata IO inputs necessary for making a selection (depends on user implementation)
  !  \param hsobsout O  receives handle of the new COSTA-stochastic observer, empty before calling, caller responsible for freeing after use
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_CreateSel
    subroutine CTA_SObs_CreateSel( hsobsin, userdata, hsobsout, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobsin
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  userdata
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hsobsout
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_CreateSel
  end interface

  !  \brief Create a new stoch observer that is subset in time of existing stochastic observer.
  ! 
  !          All observations in the closed interval [t1,t2] of the time span are selected.
  ! 
  !  \param hsobsin  I  handle of the stochastic observer of
  !                     which a selection is to be made
  !  \param timespan I  time span over which selection has to be made
  !  \param hsobsout O  receives handle of the new COSTA-stochastic observer, empty before calling
  !  \param status O error states: CTA_OK if successful
  !
  interface CTA_F90_SObs_CreateTimSel
    subroutine CTA_SObs_CreateTimSel( hsobsin, timespan, hsobsout, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobsin
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  timespan
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hsobsout
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_CreateTimSel
  end interface

  !  \brief Count the number of elements in stochastic observer.
  !  \param hsobs    I  handle of the stochastic observer
  !  \param nmeasr   O  receives number of measurements in this observer
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_Count
    subroutine CTA_SObs_Count( hsobs, nmeasr, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobs
      integer                       , intent(out  )     ::  nmeasr
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_Count
  end interface

  !  \brief Get a vector with the measurements.
  ! 
  !  \param hsobs    I  handle of the stochastic observer
  !  \param hvec     IO handle of vector that receives the measurements; must exist before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_GetVal
    subroutine CTA_SObs_GetVal( hsobs, hvec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobs
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_GetVal
  end interface

  !  \brief Count the times associated to the measurements.
  ! 
  !  \param hsobs    I  handle of the stochastic observer
  !  \param hvec     IO handle to vector that receives the times; must exist before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_GetTimes
    subroutine CTA_SObs_GetTimes( hsobs, hvec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobs
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_GetTimes
  end interface

  !  \brief Draw random values (measurements) according to the probability density
  !          function of the mesurements.
  ! 
  !  \param hsobs    I  handle of the stochastic observer
  !  \param hvec     IO handle of vector that receives the draw (measurements); must exist before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_GetRealisation
    subroutine CTA_SObs_GetRealisation( hsobs, hvec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobs
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_GetRealisation
  end interface

  !  \brief Get expectation of the probability density function of the mesurements.
  ! 
  !  \param hsobs    I  handle of the stochastic observer
  !  \param hvec     IO handle of vector that receives the expectation values; must exist before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_GetExpectation
    subroutine CTA_SObs_GetExpectation( hsobs, hvec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobs
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_GetExpectation
  end interface

  !  \brief Get the value of the probability density function of the mesurements at given location.
  ! 
  !  \param hsobs    I  handle of the stochastic observer
  !  \param hvecx    I  handle of vector with location for evaluating pdf
  !  \param hvecy    IO handle of vector that is to contain the pdf-value; must exist before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_EvalPDF
    subroutine CTA_SObs_EvalPDF( hsobs, hvecx, hvecy, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobs
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvecx
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvecy
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_EvalPDF
  end interface

  !  \brief Get covariance matrix of probability density function of the measurements.
  ! 
  !  \param hsobs    I  handle of the stochastic observer
  !  \param hmat     IO handle of matrix that receives the covariance matrix; must exist before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_GetCovMat
    subroutine CTA_SObs_GetCovMat( hsobs, hmat, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobs
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmat
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_GetCovMat
  end interface

  !  \brief Get variance of probability density function of the mesurements.
  ! 
  !  \param hsobs    I  handle of the stochastic observer
  !  \param hvec     IO handle of vector that receives the variance; must exist before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_GetVar
    subroutine CTA_SObs_GetVar( hsobs, hvec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobs
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_GetVar
  end interface

  !  \brief Get standard deviation of probability density function of the measurements.
  ! 
  !  \param hsobs    I  handle of the stochastic observer
  !  \param hvec     IO handle of vector that is to contain the standard deviation
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_GetStd
    subroutine CTA_SObs_GetStd( hsobs, hvec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobs
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_GetStd
  end interface

  !  \brief Create the observation description corresponding to the stochastic observer.
  ! 
  !  \note Caller is responsible for freeing the here created observation description
  ! 
  !  \param hsobs      I  handle of the stochastic observer
  !  \param hobsdescr  O  receives handle of newly created observation description class, empty before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_GetDescription
    subroutine CTA_SObs_GetDescription( hsobs, hobsdescr, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobs
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hobsdescr
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_GetDescription
  end interface

  !  \brief Export the stochastic observer.
  ! 
  !  \note Supported by CTA_DEFAULT_SOBS:\n
  !        output to file (userdata must contain handle of COSTA file)\n
  ! 
  !  \param hsobs    I  handle of the stochastic observer
  !  \param userdata I  configuration of output
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_Export
    subroutine CTA_SObs_Export( hsobs, userdata, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobs
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  userdata
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_Export
  end interface

  !  \brief Free the stochastic observer
  ! 
  !  \Note hsobs=CTA_NULL is allowed
  ! 
  !  \param hsobs  IO handle of the stochastic observer, replaced by CTA_NULL on return
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_SObs_Free
    subroutine CTA_SObs_Free( hsobs, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hsobs
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SObs_Free
  end interface


end module cta_f90_sobs

