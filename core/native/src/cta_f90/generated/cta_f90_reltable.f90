module cta_f90_reltable

  implicit none

  public

  !  \brief Create a relation table
  ! 
  !  \param hreltable  O  created relation table
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_RelTable_Create
    subroutine CTA_RelTable_Create( hreltable, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hreltable
      integer                       , intent(out  )     ::  status
    end subroutine CTA_RelTable_Create
  end interface

  !  \brief Free a relation table object.
  ! 
  !  \param hreltable  IO relation table to be freed,
  !                       value is set to CTA_NULL on return.
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_RelTable_Free
    subroutine CTA_RelTable_Free( hreltable, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hreltable
      integer                       , intent(out  )     ::  status
    end subroutine CTA_RelTable_Free
  end interface

  !  \brief Copy elements according to relation table
  ! 
  !  \note we currently only support copying of elements
  !        between two vector instances. Other types of
  !        COSTA object will be supported when needed
  !        in later versions
  ! 
  !  \param hreltable  I handle of relation table
  !  \param hfrom      I Origin object to copy data from
  !  \param hto        I Target object to copy data to
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_RelTable_Apply
    subroutine CTA_RelTable_Apply( hreltable, hfrom, hto, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hreltable
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfrom
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hto
      integer                       , intent(out  )     ::  status
    end subroutine CTA_RelTable_Apply
  end interface

  !  \brief Copy elements according to inverse of relation table
  ! 
  !  \note we currently only support copying of elements
  !        between two vector instances. Other types of
  !        COSTA object will be supported when needed
  !        in later versions
  ! 
  !  \param hreltable  I handle of relation table
  !  \param hfrom      I Origin object to copy data from
  !  \param hto        I Target object to copy data to
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_RelTable_ApplyInv
    subroutine CTA_RelTable_ApplyInv( hreltable, hfrom, hto, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hreltable
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfrom
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hto
      integer                       , intent(out  )     ::  status
    end subroutine CTA_RelTable_ApplyInv
  end interface

  !  \brief Set a relation table
  !   A Set a relation table that defines a selection of elements
  ! 
  !  \param hreltable  O relation table that is set
  !  \param vselect    I (integer) vector with indices of elements
  ! from the target set that are selected.
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_RelTable_SetSelect
    subroutine CTA_RelTable_SetSelect( hreltable, vselect, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hreltable
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  vselect
      integer                       , intent(out  )     ::  status
    end subroutine CTA_RelTable_SetSelect
  end interface

  !  \brief Get the number of elements that are copied when the table is applied
  ! 
  ! 
  !  \param hreltable  I  relation table
  !  \param nelt     O  number of elements that are copied
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_RelTable_Count
    subroutine CTA_RelTable_Count( hreltable, nelt, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hreltable
      integer                       , intent(out  )     ::  nelt
      integer                       , intent(out  )     ::  status
    end subroutine CTA_RelTable_Count
  end interface

  !  \brief Set a relation table that is combination of two
  !          relation tables.
  ! 
  !  Set a relation table that is the combination of two exisiting relation
  !  tables. It is possible to use the inverse of the relation tables when
  !  needed
  ! .
  !  A usefull application of this method is to create a relation table that
  !  defines a relation between a subset of elements from set1 and a subset of
  !  the elements of set2. In order to set a relation table of this kind first
  !  create two relation tables:
  !  hrel1 elements from set 1 that have a relation with the elements from set 2,
  !  hrel2 elements from set 2 that have a relation with the elements from set 1
  ! 
  !  The combined relation table of hrel1 and inverse(hrel2) is a relation
  !  table that spcifies the relation of a subset of elements from set1 and a
  !  subset of elements from set2.
  ! 
  !  \param hreltable  O relation table that is set
  !  \param hrel1      I first relation table
  !  \param inverse1   I use inverse of hrel1 (CTA_TRUE/CTA_FALSE)
  !  \param hrel2      I first relation table
  !  \param inverse2   I use inverse of hrel2 (CTA_TRUE/CTA_FALSE)
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_RelTable_SetTableCombine
    subroutine CTA_RelTable_SetTableCombine( hreltable, hrel1, inverse1, hrel2, inverse2, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hreltable
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hrel1
      integer                       , intent(in   )     ::  inverse1
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hrel2
      integer                       , intent(in   )     ::  inverse2
      integer                       , intent(out  )     ::  status
    end subroutine CTA_RelTable_SetTableCombine
  end interface


end module cta_f90_reltable

