module cta_f90_obsdescr

  implicit none

  public

  !  \brief Create a new class (=implementation) of a COSTA observation description component.
  ! 
  !  \param name       I  name of the new observation description class
  !  \param h_func     I  COSTA function handles for functions that implement class,
  !                       missing functions must have value CTA_NULL
  !  \param hobsdscrcl O  receives handle of new observation description class
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_ObsDescr_DefineClass
    subroutine CTA_ObsDescr_DefineClass( name, h_func, hobsdscrcl, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  name
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h_func(*)
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hobsdscrcl
      integer                       , intent(out  )     ::  status
    end subroutine CTA_ObsDescr_DefineClass
  end interface

  !  \brief Create a new observation description instance.
  ! 
  !  \param hsobscl   I  class of new observation description
  !  \param usrdat    IO data of the stochastic observer for which
  !                      a descriptor is to be created
  !  \param hobsdscr  O  receives handle of created observation description
  !                      object
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_ObsDescr_Create
    subroutine CTA_ObsDescr_Create( hsobscl, usrdat, hobsdscr, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hsobscl
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  usrdat
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hobsdscr
      integer                       , intent(out  )     ::  status
    end subroutine CTA_ObsDescr_Create
  end interface

  !  \brief Create a new observation description that is subset of existing observation description.
  ! 
  !  \param hobsdescr     I the observation description to create a subset
  !                         from
  !  \param selection     I selection criterion (subset of SQL)
  !  \param reltab        O Relation table specifying the relation between
  !                         the original and new observation description
  !                         component. Note no relation table is created when
  !                         reltab==CTA_NULL on entry
  !  \param hobsdescrout  O the new COSTA-stochastic observer, empty before
  !                         calling, caller responsible for freeing after use
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_ObsDescr_CreateSel
    subroutine CTA_ObsDescr_CreateSel( hobsdescr, selection, reltab, hobsdescrout, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hobsdescr
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  selection
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  reltab
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hobsdescrout
      integer                       , intent(out  )     ::  status
    end subroutine CTA_ObsDescr_CreateSel
  end interface

  !  \brief Create a new observation description that is subset of existing observation description.
  !          All observations in the interval (t1,t2] (note t1 is not part
  !          of the interval!) of the time span are selected.
  ! 
  !  \param hobsdescr     I the observation description to create a subset
  !                         from
  !  \param timespan      I  time span over which selection has to be made
  !  \param reltab        O Relation table specifying the relation between
  !                         the original and new observation description
  !                         component. Note no relation table is created when
  !                         reltab==CTA_NULL on enty
  !  \param hobsdescrout  O the new COSTA-stochastic observer, empty before
  !                         calling, caller responsible for freeing after use
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_ObsDescr_CreateTimSel
    subroutine CTA_ObsDescr_CreateTimSel( hobsdescr, timespan, reltab, hobsdescrout, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hobsdescr
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  timespan
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  reltab
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hobsdescrout
      integer                       , intent(out  )     ::  status
    end subroutine CTA_ObsDescr_CreateTimSel
  end interface

  !  \brief Get properties/values that correspond to a given key.
  ! 
  !  \param hobsdscr   I  handle of observation description
  !  \param Key        I  key for which the value is asked
  !  \param Properties IO COSTA-vector that is to receive the values
  !  \param datatype   I  data type of elements in properties vector, must be the same as of queried properties
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_ObsDescr_Get_ValueProperties
    subroutine CTA_ObsDescr_Get_ValueProperties( hobsdscr, Key, Properties, datatype, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hobsdscr
      character(len=*)              , intent(in   )     ::  Key
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  Properties
      integer                       , intent(in   )     ::  datatype
      integer                       , intent(out  )     ::  status
    end subroutine CTA_ObsDescr_Get_ValueProperties
  end interface

  !  \brief Get all keys names.
  ! 
  !  \param hobsdscr   I  handle of observation description
  !  \param Keys       O  receives all keys (COSTA-string vector); must exist before calling and be large enough
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_ObsDescr_Get_PropertyKeys
    subroutine CTA_ObsDescr_Get_PropertyKeys( hobsdscr, Keys, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hobsdscr
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  Keys
      integer                       , intent(out  )     ::  status
    end subroutine CTA_ObsDescr_Get_PropertyKeys
  end interface

  !  \brief Get number of properties/keys.
  ! 
  !  \param hobsdscr   I  handle of observation description
  !  \param nkeys      O  receives number of properties/keys
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_ObsDescr_Property_Count
    subroutine CTA_ObsDescr_Property_Count( hobsdscr, nkeys, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hobsdscr
      integer                       , intent(out  )     ::  nkeys
      integer                       , intent(out  )     ::  status
    end subroutine CTA_ObsDescr_Property_Count
  end interface

  !  \brief Get number of observations.
  ! 
  !  \param hobsdscr   I  handle of observation description
  !  \param nobs       O  receives the number of observations
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_ObsDescr_Observation_Count
    subroutine CTA_ObsDescr_Observation_Count( hobsdscr, nobs, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hobsdscr
      integer                       , intent(out  )     ::  nobs
      integer                       , intent(out  )     ::  status
    end subroutine CTA_ObsDescr_Observation_Count
  end interface

  !  \brief Export observation description.
  ! 
  !  The default observation description CTA_DEFAULT_OBSDESC supports exporting to:\n
  !  TODO
  ! 
  !  \param hdescr     I  handle of observation description
  !  \param usrdat     IO export configuration/medium
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_ObsDescr_Export
    subroutine CTA_ObsDescr_Export( hdescr, usrdat, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hdescr
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  usrdat
      integer                       , intent(out  )     ::  status
    end subroutine CTA_ObsDescr_Export
  end interface

  !  \brief Free observation description object.
  ! 
  !  \param hobsdscr  IO handle of observation description, replaced by CTA_NULL on return.
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_ObsDescr_Free
    subroutine CTA_ObsDescr_Free( hobsdscr, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hobsdscr
      integer                       , intent(out  )     ::  status
    end subroutine CTA_ObsDescr_Free
  end interface


end module cta_f90_obsdescr

