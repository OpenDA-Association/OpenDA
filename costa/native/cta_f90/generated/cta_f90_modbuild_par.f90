module cta_f90_modbuild_par

  implicit none

  public

  !  \brief Create the model class of the Par-Modelbuilder and initilizes MPI
  ! 
  !  \note This is not a user function. It is called at initialization of the
  !        COSTA environment.
  ! 
  !   \param modelcls  O  receives handle of the SP-modelbuilder class
  !
  interface CTA_F90_Modbuild_par_CreateClass
    subroutine CTA_Modbuild_par_CreateClass( modelcls )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  modelcls
    end subroutine CTA_Modbuild_par_CreateClass
  end interface

  !  \brief Stop the model class of the Par-Modelbuilder and finalize MPI
  ! 
  !  \note This is not a user function. It is called at the finalization of the
  !        COSTA environment.
  ! 
  !
  interface CTA_F90_Modbuild_par_Finalize
    subroutine CTA_Modbuild_par_Finalize(  )
    end subroutine CTA_Modbuild_par_Finalize
  end interface


end module cta_f90_modbuild_par

