module cta_f90_model_factory

  implicit none

  public

  !  \brief Create a COSTA modell class from XML input file
  !           (load from methods from dynamic load library)
  ! 
  !   \param fName  I  XML-configuration file
  !   \param modelClass O Class of new model Factory
  !   \param status O          Model class handle
  !
  interface CTA_F90_ModelFactory_New
    subroutine CTA_ModelFactory_New( fName, modelClass, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  fName
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  modelClass
      integer                       , intent(out  )     ::  status
    end subroutine CTA_ModelFactory_New
  end interface

  !  \brief Define a new class (=implementation) of a COSTA model component
  ! 
  !  \param name     I  name of the new model class
  !  \param h_func   I  COSTA function handles for functions that implement class,
  !                     missing functions must have value CTA_NULL
  !  \param hmodcl   O  receives handle of new model class
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Model_DefineClass
    subroutine CTA_Model_DefineClass( name, h_func, hmodcl, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  name
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h_func(*)
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hmodcl
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Model_DefineClass
  end interface


end module cta_f90_model_factory

