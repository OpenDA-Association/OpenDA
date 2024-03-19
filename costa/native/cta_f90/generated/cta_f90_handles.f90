module cta_f90_handles

  implicit none

  public

  !  \brief Create a new COSTA handle
  ! 
  !  \param name     I  name associated with handle
  !  \param datatype I  data type of handle
  !  \param data     I  block of data associated to handle
  !  \param handle   O  receives COSTA handle
  !  \param status O error status: CTA_OK
  !
  !interface CTA_F90_Handle_Create
  !  subroutine CTA_Handle_Create( name, datatype, data, handle, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    character(len=*)              , intent(in   )     ::  name
  !    integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  datatype
  !    void                          , intent(in   )     ::  data(*)
  !    integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  handle
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_Handle_Create
  !end interface

  !  \brief Free a COSTA handle
  ! 
  !   \note The data part of the handle is NOT freed.
  ! 
  !   \param handle IO handle that is to be freed, replaced by CTA_NULL on return.
  !   \param status O error status: CTA_OK, CTA_ILLEGAL_HANDLE
  !
  interface CTA_F90_Handle_Free
    subroutine CTA_Handle_Free( handle, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  handle
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Handle_Free
  end interface

  !  \brief Free a COSTA handle
  ! 
  !   \note Calls datatype-specific free methods, if available.
  ! 
  !   \param handle IO handle that must be freed, replaced by CTA_NULL on return.
  !   \param status O error status: CTA_OK, CTA_ILLEGAL_HANDLE
  !
  interface CTA_F90_Handle_Free_All
    subroutine CTA_Handle_Free_All( handle, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  handle
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Handle_Free_All
  end interface

  !  \brief Check whether a handle is valid and checks type
  ! 
  !   \note The handle CTA_NULL is not valid.
  ! 
  !  \param handle   I  COSTA handle
  !  \param datatype I  data type to compare handle with
  !  \param status O error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
  ! 
  !
  interface CTA_F90_Handle_Check
    subroutine CTA_Handle_Check( handle, datatype, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  handle
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  datatype
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Handle_Check
  end interface

  !  \brief Get pointer to data element of handle
  ! 
  !   \param handle I  COSTA handle
  !   \param data   O  receives pointer to data element
  !   \param status O error status: CTA_OK, CTA_ILLEGAL_HANDLE
  !
  !interface CTA_F90_Handle_GetData
  !  subroutine CTA_Handle_GetData( handle, data, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  handle
  !    void                          , intent(out  )     ::  data
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_Handle_GetData
  !end interface

  !  \brief Get the value the handle points to
  ! 
  !   \param handle 	I  COSTA handle
  !   \param value   	O  receives pointer to data element
  !   \param datatype     I  specify the data type of *value, must be the same as data type of handle
  !   \param status O error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
  !
  !interface CTA_F90_Handle_GetValue
  !  subroutine CTA_Handle_GetValue( handle, value, datatype, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  handle
  !    void                          , intent(out  )     ::  value
  !    integer                       , intent(in   )     ::  datatype
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_Handle_GetValue
  !end interface

  !  \brief Get name associated with handle
  ! 
  !   \param handle   I  COSTA handle
  !   \param hname    O  receives name of data type
  !   \param status O error status: CTA_OK, CTA_ILLEGAL_HANDLE
  !
  interface CTA_F90_Handle_GetName
    subroutine CTA_Handle_GetName( handle, hname, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  handle
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hname
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Handle_GetName
  end interface

  !  \brief Get data type associated with handle
  ! 
  !   \param handle   I  COSTA handle
  !   \param datatype O  receives data type of handle
  !   \param status O error status: CTA_OK, CTA_ILLEGAL_HANDLE
  !
  interface CTA_F90_Handle_GetDatatype
    subroutine CTA_Handle_GetDatatype( handle, datatype, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  handle
      integer                       , intent(out  )     ::  datatype
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Handle_GetDatatype
  end interface

  !  \brief Find a handle by name and data type in the COSTA handle administration
  ! 
  !   \param sname    I  name of handle
  !   \param datatype I  data type of handle
  !   \param handlenr O  receives the handle (only if return value is CTA_OK)
  !   \param status O error status: CTA_OK, CTA_HANDLE_NOT_FOUND
  !
  interface CTA_F90_Handle_Find
    subroutine CTA_Handle_Find( sname, datatype, handlenr, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  sname
      integer                       , intent(in   )     ::  datatype
      integer                       , intent(out  )     ::  handlenr
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Handle_Find
  end interface

  !  \brief Print overview of all COSTA handles
  ! 
  !   \param status O error status: CTA_OK
  !
  interface CTA_F90_Handle_Printall
    subroutine CTA_Handle_Printall( status )
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Handle_Printall
  end interface

  !  \brief Counts all handles sorts them by type and prints overview.
  !  This function can be usefull for detecting memory leaks that are
  !  the result of not freeing instances of COSTA objects costa objects.
  ! 
  !   \param location I String to indicate location of call
  !   \param status O error status: CTA_OK
  !
  interface CTA_F90_Handle_PrintInfo
    subroutine CTA_Handle_PrintInfo( location, status )
      character(len=*)              , intent(in   )     ::  location
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Handle_PrintInfo
  end interface

  !  \brief Get the reference count of the handle
  ! 
  !   \param handle   I COSTA handle
  !   \param refCount O reference count of handle
  !   \param status O error status: CTA_OK
  ! 
  !
  interface CTA_F90_Handle_GetRefCount
    subroutine CTA_Handle_GetRefCount( handle, refCount, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  handle
      integer                       , intent(out  )     ::  refCount
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Handle_GetRefCount
  end interface

  !  \brief Increase the reference count of the handle
  ! 
  !   \param handle   I COSTA handle
  !   \param status O error status: CTA_OK
  ! 
  !
  interface CTA_F90_Handle_IncRefCount
    subroutine CTA_Handle_IncRefCount( handle, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  handle
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Handle_IncRefCount
  end interface

  !  \brief Decrease the reference count of the handle
  ! 
  !   \param handle   I COSTA handle
  !   \param status O error status: CTA_OK
  ! 
  !
  interface CTA_F90_Handle_DecrRefCount
    subroutine CTA_Handle_DecrRefCount( handle, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  handle
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Handle_DecrRefCount
  end interface


end module cta_f90_handles

