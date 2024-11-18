module cta_f90_model_utilities

  implicit none

  public

  !  \brief Handles model configuration tree or name of input xml-file
  ! 
  !  When a new instance of the a model component is created it needs some
  !  input. The most convenient way is to provide the root to a COSTA-tree
  !  with configuration information the name of a XML-configuration file
  !  This routine will check whether the input handle is a COSTA-tree or
  !  the name of a XML-configuration file. A COSTA-tree containing the content
  !  of the XML-file is created when the input is the name of the XML-file.
  ! 
  !  \note A COSTA-tree is created if the input is the name of an XML-file.
  !        the handle of the input tree is returned otherwise. This means that
  !        depending on the input, a tree is created. The routine will return whether
  !        the returned tree is created by this routine. The caller is responsible
  !        for freeing the tree when necessary.
  ! 
  !  \param hinput   I  handle of tree (CTA_Tree) with model input or string
  !                     (CTA_Sring) with name of xml-input file
  !  \param tinput   O  receives handle of tree (CTA_Tree) with model input
  !  \param cleanup  O  receives flag (CTA_TRUE/CTA_FALSE) indicating whether tinput is
  !                     created and must be freed by the caller
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_Util_InputTree
    subroutine CTA_Model_Util_InputTree( hinput, tinput, cleanup, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hinput
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  tinput
      integer                       , intent(out  )     ::  cleanup
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_Util_InputTree
  end interface


end module cta_f90_model_utilities

