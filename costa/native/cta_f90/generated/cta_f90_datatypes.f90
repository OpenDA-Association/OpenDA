module cta_f90_datatypes

  implicit none

  public

  !  \brief Get the result of the C-function sizeof for a COSTA datatype
  ! 
  !   \param datatype I COSTA data type
  !   \param size     O receives result sizeof-function
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_SizeOf
    subroutine CTA_SizeOf( datatype, size, status )
      integer                       , intent(in   )     ::  datatype
      integer                       , intent(out  )     ::  size
      integer                       , intent(out  )     ::  status
    end subroutine CTA_SizeOf
  end interface


end module cta_f90_datatypes

