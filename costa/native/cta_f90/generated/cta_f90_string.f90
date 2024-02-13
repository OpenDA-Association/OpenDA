module cta_f90_string

  implicit none

  public

  !  \brief Create a new COSTA string instance.
  ! 
  !   \param hstring  O  handle of created string
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_String_Create
    subroutine CTA_String_Create( hstring, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hstring
      integer                       , intent(out  )     ::  status
    end subroutine CTA_String_Create
  end interface

  !  \brief Create a new COSTA string that is a copy of an existing one
  ! 
  !   \param hto    O  receives the handle of the created string
  !   \param hfrom  I  handle of string to copy
  ! 
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_String_Copy
    subroutine CTA_String_Copy( hto, hfrom, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hto
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfrom
      integer                       , intent(out  )     ::  status
    end subroutine CTA_String_Copy
  end interface

  !  \brief Free the COSTA string instance.
  ! 
  !   \note
  ! 
  !   \param hstring  IO handle of the string instance, replaced by CTA_NULL on return
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_String_Free
    subroutine CTA_String_Free( hstring, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hstring
      integer                       , intent(out  )     ::  status
    end subroutine CTA_String_Free
  end interface

  !  \brief Get the number of characters in string.
  ! 
  !   \note The returned length is the number of characters excluding the
  !         0-character.
  ! 
  !   \param hstring  I  handle of the string
  !   \param len      O  receives the number of characters in string
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_String_GetLength
    subroutine CTA_String_GetLength( hstring, len, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hstring
      integer                       , intent(out  )     ::  len
      integer                       , intent(out  )     ::  status
    end subroutine CTA_String_GetLength
  end interface

  !  \brief Set the string to new content.
  ! 
  !   \param hstring  IO handle of the string
  !   \param str      I  new content
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_String_Set
    subroutine CTA_String_Set( hstring, str, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hstring
      character(len=*)              , intent(in   )     ::  str
      integer                       , intent(out  )     ::  status
    end subroutine CTA_String_Set
  end interface

  !  \brief Get a copy of the string.
  ! 
  !   \note It is the responsibility of the caller making str large enough to
  !         hold the string and trailing 0-character.
  ! 
  !   \param hstring  I  handle of the string
  !   \param str      O  buffer that receives a copy of the string including trailing 0-character
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_String_Get
    subroutine CTA_String_Get( hstring, str, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hstring
      character(len=*)              , intent(out  )     ::  str
      integer                       , intent(out  )     ::  status
    end subroutine CTA_String_Get
  end interface

  !  \brief Get the (scalar) value of a string
  ! 
  !   \note It is the responsibility of the caller that parameter value is large enough to
  !         hold the value as specified by the datatype.
  ! 
  !   \param hstring  I  handle of the string
  !   \param value    O  receives the value
  !   \param datatype I  data type of value
  !   \param status O CTA_OK if successful
  !
  !interface CTA_F90_String_GetValue
  !  subroutine CTA_String_GetValue( hstring, value, datatype, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hstring
  !    void                          , intent(out  )     ::  value
  !    integer                       , intent(in   )     ::  datatype
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_String_GetValue
  !end interface

  !  \brief Create new string that is a concatination of existing strings.
  ! 
  !   \param istring  IO handle of the string (first string in concatination)
  !                      and whole concatinated string on return
  !   \param xstring  I  handle of the second string (extension string)
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_String_Conc
    subroutine CTA_String_Conc( istring, xstring, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  istring
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  xstring
      integer                       , intent(out  )     ::  status
    end subroutine CTA_String_Conc
  end interface

  !  \brief Imports string.
  ! 
  !   Supports: pack objects (usrdata must be handle of pack object to import from)
  ! 
  !   \param hstring  IO handle of the string
  !   \param usrdata  I  configuration of import
  !   \param status O CTA_OK if successful
  ! 
  !   \note Only CTA_Pack is currently supported fot usrdata
  !
  interface CTA_F90_String_Import
    subroutine CTA_String_Import( hstring, usrdata, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hstring
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  usrdata
      integer                       , intent(out  )     ::  status
    end subroutine CTA_String_Import
  end interface

  !  \brief Exports length of string and string itself.
  ! 
  !   Supports: pack objects (usrdata must be handle of pack object to export to)
  ! 
  !   \param hstring  I  handle of the string
  !   \param usrdata  IO configuration of export
  !   \param status O CTA_OK if successful
  ! 
  !   \note Only CTA_Pack is currently supported fot usrdata
  !
  interface CTA_F90_String_Export
    subroutine CTA_String_Export( hstring, usrdata, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hstring
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  usrdata
      integer                       , intent(out  )     ::  status
    end subroutine CTA_String_Export
  end interface

  !  \brief Check whether string is equal to COSTA string.
  ! 
  ! 
  !   \param hstring  I  handle of the string
  !   \param str0     I  string to compare hsting with
  !   \param status O CTA_TRUE/CTA_FALSE
  ! 
  !   \note Only CTA_Pack is currently supported fot usrdata
  !
  interface CTA_F90_String_Equals_Char
    subroutine CTA_String_Equals_Char( hstring, str0, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hstring
      character(len=*)              , intent(in   )     ::  str0
      integer                       , intent(out  )     ::  status
    end subroutine CTA_String_Equals_Char
  end interface

  !  \brief Check whether two COSTA strings are equal.
  ! 
  ! 
  !   \param hstring1  I  handle of first string
  !   \param hstring2  I  handle of second string
  !   \param status O CTA_TRUE/CTA_FALSE
  ! 
  !   \note Only CTA_Pack is currently supported fot usrdata
  !
  interface CTA_F90_Strings_Equal
    subroutine CTA_Strings_Equal( hstring1, hstring2, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hstring1
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hstring2
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Strings_Equal
  end interface

  !  \brief Create a duplication of a COSTA string
  ! 
  !   \param hfrom  I  handle of string to copy
  !   \param hto    O  handle of created string
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_String_Duplicate
    subroutine CTA_String_Duplicate( hfrom, hto, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfrom
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hto
      integer                       , intent(out  )     ::  status
    end subroutine CTA_String_Duplicate
  end interface


end module cta_f90_string

