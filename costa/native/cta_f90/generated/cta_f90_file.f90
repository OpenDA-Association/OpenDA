module cta_f90_file

  implicit none

  public

  !  \brief Create a new COSTA file
  !    for holding a C file descriptor of a FORTRAN file LUN
  ! 
  !   \note This call does not open a file.
  !    No FORTRAN support in this version
  ! 
  !   \param hfile  O  receives handle of created file
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_File_Create
    subroutine CTA_File_Create( hfile, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hfile
      integer                       , intent(out  )     ::  status
    end subroutine CTA_File_Create
  end interface

  !  \brief Free a new COSTA file-handle
  ! 
  !   \note The File is not closed (in this version)
  ! 
  !   \param hfile  IO  handle of COSTA file CTA_NULL on return
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_File_Free
    subroutine CTA_File_Free( hfile, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hfile
      integer                       , intent(out  )     ::  status
    end subroutine CTA_File_Free
  end interface

  !  \brief Get the C-file descriptor of the COSTA file
  ! 
  !   \note
  ! 
  !   \param hfile  I  handle of COSTA file-handle
  !   \param file   O  receives file descriptor
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_File_Get
    subroutine CTA_File_Get( hfile, file, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfile
      integer                       , intent(out  )     ::  file
      integer                       , intent(out  )     ::  status
    end subroutine CTA_File_Get
  end interface

  !  \brief Get the NETCDF file id of the COSTA file
  ! 
  !   \note
  ! 
  !   \param hfile  I  handle of COSTA file-handle
  !   \param ncid   O  receives NETCDF file id
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_File_GetNetcdf
    subroutine CTA_File_GetNetcdf( hfile, ncid, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfile
      integer                       , intent(out  )     ::  ncid
      integer                       , intent(out  )     ::  status
    end subroutine CTA_File_GetNetcdf
  end interface

  !  \brief Set the C-file descriptor of the COSTA file
  ! 
  !   \note
  ! 
  !   \param hfile  IO  handle of COSTA file-handle
  !   \param file   I   file descriptor
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_File_Set
    subroutine CTA_File_Set( hfile, file, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hfile
      integer                       , intent(in   )     ::  file
      integer                       , intent(out  )     ::  status
    end subroutine CTA_File_Set
  end interface

  !  \brief Open a C-file and set descriptor
  ! 
  !   \note
  ! 
  !   \param hfile   I  provide a valid handle of COSTA file
  !   \param sname   I  file path
  !   \param smode   I  open-mode (see C fopen documentation)
  !                     if CTA_NULL is provided, the file will be
  !                     opened with read/write access (file pointer at begin
  !                     of file)
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_File_Open
    subroutine CTA_File_Open( hfile, sname, smode, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfile
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  sname
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  smode
      integer                       , intent(out  )     ::  status
    end subroutine CTA_File_Open
  end interface

  !  \brief check whether file is a NETCDF file
  ! 
  !   \param hfile    I  provide a valid handle of COSTA file
  !   \param isnetcdf O  CTA_TRUE if file is NETCDF file CTA_FALSE otherwise
  ! 
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_File_IsNetcdf
    subroutine CTA_File_IsNetcdf( hfile, isnetcdf, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfile
      integer                       , intent(out  )     ::  isnetcdf
      integer                       , intent(out  )     ::  status
    end subroutine CTA_File_IsNetcdf
  end interface

  !  \brief Write a string to file
  ! 
  !   \note
  ! 
  !   \param hfile   I  handle of COSTA file
  !   \param str     I  string that must be written to file
  !   \param eol     I  Add end of line, CTA_TRUE for adding end of line or CTA_FALSE otherwise
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_File_WriteStr
    subroutine CTA_File_WriteStr( hfile, str, eol, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfile
      character(len=*)              , intent(in   )     ::  str(*)
      integer                       , intent(in   )     ::  eol
      integer                       , intent(out  )     ::  status
    end subroutine CTA_File_WriteStr
  end interface


end module cta_f90_file

