module cta_f90_functions

  implicit none

  public

  !  \brief Create a new COSTA function.
  ! 
  !   \note Argument name is only used for debugging and output.
  ! 
  !   \param name     I  name of the new function for debugging purposes
  !   \param function I  pointer to function that has to be associated
  !                      with new COSTA function
  !   \param hintf    I  handle of associated interface
  !   \param hfunc    O  receives handle of created COSTA function
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Func_Create
    subroutine CTA_Func_Create( name, function, hintf, hfunc, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  name
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  function(*)
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hintf
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hfunc
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Func_Create
  end interface

  !  \brief Duplicates a user defined function
  ! 
  !  \param hfunc      I COSTA user function handle
  !  \param hdupl      I duplication of hfunc
  !  \param status O error status: CTA_OK
  !
  interface CTA_F90_Func_Duplicate
    subroutine CTA_Func_Duplicate( hfunc, hdupl, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfunc
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hdupl(*)
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Func_Duplicate
  end interface

  !  \brief Free a new COSTA function.
  ! 
  !   \note
  ! 
  !   \param hfunc  IO handle of COSTA function, replaced by CTA_NULL on return
  !   \param status O CTA_OK if sucessful
  !
  interface CTA_F90_Func_Free
    subroutine CTA_Func_Free( hfunc, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hfunc
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Func_Free
  end interface

  !  \brief Get interface of COSTA function.
  ! 
  !   \note For performance reasons, the interface is not a copy but a handle
  !         to the actual interface, it should NOT be freed by the calling routine!
  ! 
  !   \param hfunc  I  handle of COSTA function
  !   \param hintf  O  receives handle of interface of function
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Func_GetIntf
    subroutine CTA_Func_GetIntf( hfunc, hintf, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfunc
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hintf
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Func_GetIntf
  end interface

  !  \brief Get function pointer of function
  ! 
  !   \note There is no FORTRAN verion of this function available
  ! 
  !   \param hfunc     I  handle of COSTA function.
  !   \param function  O  receives pointer to function
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Func_GetFunc
    subroutine CTA_Func_GetFunc( hfunc, function, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfunc
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  function
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Func_GetFunc
  end interface

  !  \brief Get name of function
  ! 
  !   \note Future versions will return a COSTA string handle.
  ! 
  !   \param hfunc     I  handle of COSTA function.
  !   \param name      O  handle of string object that is to receive function name, must exist before calling
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Func_GetName
    subroutine CTA_Func_GetName( hfunc, name, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfunc
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  name
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Func_GetName
  end interface

  !  \brief Set userdata of function
  ! 
  !   \note Frees existing user data and replaces it with userdata
  ! 
  !   \param hfunc     IO handle of COSTA function.
  !   \param userdata  I  new userdata handles
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Func_SetUserdata
    subroutine CTA_Func_SetUserdata( hfunc, userdata, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hfunc
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  userdata
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Func_SetUserdata
  end interface

  !  \brief Get userdata of function
  ! 
  !   \param hfunc     I  handle of COSTA function.
  !   \param userdata  O  userdata handle
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Func_GetUserdata
    subroutine CTA_Func_GetUserdata( hfunc, userdata, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfunc
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  userdata
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Func_GetUserdata
  end interface


end module cta_f90_functions

