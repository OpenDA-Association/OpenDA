! constants.f90 --
!     Define some useful constants
!
module m_constants
    implicit none

    integer, parameter :: everything_okay    = 0
    integer, parameter :: error_reading_file = -1
    integer, parameter :: error_writing_file = -2
    integer, parameter :: out_of_memory      = -3
end module m_constants
