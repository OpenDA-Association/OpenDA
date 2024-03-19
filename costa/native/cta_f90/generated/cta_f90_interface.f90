module cta_f90_interface

  implicit none

  public

  !  \brief Create a new interface
  ! 
  !  \param name     I  name of the new interface
  !  \param argtyp   I  list with the data types of arguments
  !  \param narg     I  number of arguments of interface
  !  \param hintf    O  receives the new COSTA interface handle
  !  \param status O error status: CTA_OK
  !
  interface CTA_F90_Intf_Create
    subroutine CTA_Intf_Create( name, argtyp, narg, hintf, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  name
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  argtyp(*)
      integer                       , intent(in   )     ::  narg
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hintf
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Intf_Create
  end interface

  !  \brief Free an interface
  ! 
  !   \note Freeing CTA_NULL is allowed.
  ! 
  !   \param hintf  IO  handle of interface, replaced by CTA_NULL on return
  !   \param status O error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
  !
  interface CTA_F90_Intf_Free
    subroutine CTA_Intf_Free( hintf, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hintf
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Intf_Free
  end interface

  !  \brief Match two interfaces for compatibility argumentlist-argumentlist
  ! 
  !   \note Two interfaces are compatible if all arguments have the same datatype,
  !         CTA_VOID is compatible with all other arguments except for CTA_FSTRING
  ! 
  !   \param argtyp1  I  list with the data types of arguments of first interface
  !   \param narg1    I  number of arguments in first interface
  !   \param argtyp2  I  list with the data types of arguments of second interface
  !   \param narg2    I  number of arguments in second interface
  !   \param flag     O  receives TRUE if interfaces are compatible FALSE ortherwise
  !   \param status O error status: CTA_OK
  !
  interface CTA_F90_Intf_Match_aa
    subroutine CTA_Intf_Match_aa( argtyp1, narg1, argtyp2, narg2, flag, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  argtyp1(*)
      integer                       , intent(in   )     ::  narg1
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  argtyp2(*)
      integer                       , intent(in   )     ::  narg2
      integer                       , intent(out  )     ::  flag
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Intf_Match_aa
  end interface

  !  \brief Match two interfaces for compatibility handle-argumentlist
  ! 
  !   \note Two interfaces are compatible if all arguments have the same datatype,
  !         CTA_VOID is compatible with all other arguments except for CTA_FSTRING
  ! 
  !   \param hintf1   I  handle of first interface
  !   \param argtyp2  I  list with the data types of arguments of second interface
  !   \param narg2    I  number of arguments in second interface
  !   \param flag     O  receives TRUE if interfaces are compatible FALSE ortherwise
  !   \param status O error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
  !
  interface CTA_F90_Intf_Match_ha
    subroutine CTA_Intf_Match_ha( hintf1, argtyp2, narg2, flag, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hintf1
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  argtyp2(*)
      integer                       , intent(in   )     ::  narg2
      integer                       , intent(out  )     ::  flag
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Intf_Match_ha
  end interface

  !  \brief Match two interfaces for compatibility handle-handle
  ! 
  !   \note two interfaces are compatible if all arguments have the same datatype,
  !         CTA_VOID is compatible with all other arguments except for CTA_FSTRING
  ! 
  !   \param hintf1   I  handle of first interface
  !   \param hintf2   I  handle of second interface
  !   \param flag     O  receives TRUE if interfaces are compatible FALSE ortherwise
  !   \param status O error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
  !
  interface CTA_F90_Intf_Match_hh
    subroutine CTA_Intf_Match_hh( hintf1, hintf2, flag, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hintf1
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hintf2
      integer                       , intent(out  )     ::  flag
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Intf_Match_hh
  end interface


end module cta_f90_interface

