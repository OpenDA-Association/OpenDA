module cta_f90_metainfo

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
  interface CTA_F90_Metainfo_DefineClass
    subroutine CTA_Metainfo_DefineClass( name, h_func, hobsdscrcl, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  name
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h_func(*)
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hobsdscrcl
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Metainfo_DefineClass
  end interface

  !  \brief Create a new observation description instance.
  ! 
  !  \param hsobscl   I  class of new observation description
  !  \param usrdat    IO data of the stochastic observer for which
  ! a descriptor is to be created
  !  \param hobsdscr  O  receives handle of created observation description object
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Metainfo_Create
    subroutine CTA_Metainfo_Create( hobsdscr, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hobsdscr
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Metainfo_Create
  end interface

  !  \brief Get properties/values that correspond to a given key.
  ! 
  !  \param hobsdscr   I  handle of observation description
  !  \param Key        I  key for which the value is asked
  !  \param Properties IO COSTA-vector that is to receive the values
  !  \param datatype   I  data type of elements in properties vector, must be the same as of queried properties
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Metainfo_SetUnit
    subroutine CTA_Metainfo_SetUnit( hobsdscr, Key, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hobsdscr
      character(len=*)              , intent(in   )     ::  Key
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Metainfo_SetUnit
  end interface


end module cta_f90_metainfo

