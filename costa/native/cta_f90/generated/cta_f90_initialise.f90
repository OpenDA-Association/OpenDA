module cta_f90_initialise

  implicit none

  public

  !  \brief Initialize the COSTA environment
  !   \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Core_Initialise
    subroutine CTA_Core_Initialise( status )
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Core_Initialise
  end interface

  !  \brief Finalise the COSTA environment
  !   \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Core_Finalise
    subroutine CTA_Core_Finalise( status )
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Core_Finalise
  end interface


end module cta_f90_initialise

