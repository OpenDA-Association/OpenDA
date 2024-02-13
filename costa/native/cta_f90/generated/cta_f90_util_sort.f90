module cta_f90_util_sort

  implicit none

  public

  !  \brief Sort an integer array using the Quicksort algorithm.
  !   An additional interger array is permutated in way as the unsorted array
  ! 
  ! 
  !   \param list       (input/output) array, dimension (N)
  !           On entry, the array to be sorted.
  !           On exit, list has been sorted into increasing order
  ! 
  !  \param indx (input/output) array, dimension (N)
  ! 
  !   \param n       (input) INTEGER
  !           The length of the array D.
  ! 
  !
  interface CTA_F90_Util_IQSort2
    subroutine CTA_Util_IQSort2( list, indx, n )
      integer                       , intent(inout)     ::  list
      integer                       , intent(inout)     ::  indx
      integer                       , intent(in   )     ::  n
    end subroutine CTA_Util_IQSort2
  end interface


end module cta_f90_util_sort

