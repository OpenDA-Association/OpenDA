module cta_f90_xml

  implicit none

  public

  !  \brief Read a COSTA XML file into a new tree.
  ! 
  !   \param hfname  I  file name of XML file to read
  !   \param hroot   O  handle of a new COSTA tree
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_XML_Read
    subroutine CTA_XML_Read( hfname, hroot, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfname
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hroot
      integer                       , intent(out  )     ::  status
    end subroutine CTA_XML_Read
  end interface

  !  \brief Write a tree to a COSTA XML file
  ! 
  !   \param hfname  I  file name of XML file to write
  !   \param hroot   I  handle of a COSTA tree
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_XML_Write
    subroutine CTA_XML_Write( hfname, hroot, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfname
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hroot
      integer                       , intent(out  )     ::  status
    end subroutine CTA_XML_Write
  end interface


end module cta_f90_xml

