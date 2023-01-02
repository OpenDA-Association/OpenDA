module cta_f90_util_statistics

  implicit none

  public

  !  \brief Initialize the random generator.
  !   \note only initialize the random generator once.
  !  \param seed     I some positive initial seed
  !
  interface CTA_F90_rand_seed
    subroutine CTA_rand_seed( seed )
      integer                       , intent(in   )     ::  seed
    end subroutine CTA_rand_seed
  end interface

  !  \brief Get an uniform random number from the interval [0 1].
  ! 
  !  \param x     O  receives the random number
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_rand_u
    subroutine CTA_rand_u( x, status )
      real(8)                       , intent(out  )     ::  x
      integer                       , intent(out  )     ::  status
    end subroutine CTA_rand_u
  end interface

  !  \brief Get a random number from a normal distribution whit mean 0 and
  !  standard deviation 1.
  ! 
  !  \param x     O  receives the random number
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_rand_n
    subroutine CTA_rand_n( x, status )
      real(8)                       , intent(out  )     ::  x
      integer                       , intent(out  )     ::  status
    end subroutine CTA_rand_n
  end interface


end module cta_f90_util_statistics

