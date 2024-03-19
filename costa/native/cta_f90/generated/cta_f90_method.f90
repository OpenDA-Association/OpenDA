module cta_f90_method

  implicit none

  public

  !  \brief Create a new class (=implementation) of a COSTA method.
  ! 
  !  \param name     I  name of the new method class
  !  \param h_func   I  COSTA function handles for functions that implement class,
  !                     missing functions must have value CTA_NULL
  !  \param hmethcl  O  receives handle of new method class
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Meth_DefineClass
    subroutine CTA_Meth_DefineClass( name, h_func, hmethcl, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  name
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h_func(*)
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hmethcl
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Meth_DefineClass
  end interface

  !  \brief Create an instance of a method.
  ! 
  !  \param hmethcl   I  method class of new object
  !  \param userdata  IO user data for creation (depends on class)
  !  \param hmeth     O  receives handle of new method
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Meth_Create
    subroutine CTA_Meth_Create( hmethcl, userdata, hmeth, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmethcl
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  userdata
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hmeth
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Meth_Create
  end interface

  !  \brief Run method.
  !  \param hmeth     I  handle of method
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Meth_Run
    subroutine CTA_Meth_Run( hmeth, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmeth
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Meth_Run
  end interface

  !  \brief Free the method object.
  ! 
  !  \Note hmeth=CTA_NULL is allowed
  ! 
  !  \param hmeth  IO handle of method, replaced by CTA_NULL on return.
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Meth_Free
    subroutine CTA_Meth_Free( hmeth, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmeth
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Meth_Free
  end interface


end module cta_f90_method

