module cta_f90_tree

  implicit none

  public

  !  \brief Create a new COSTA tree instance
  ! 
  !   \note
  ! 
  !   \param htree  O  receives handle of created tree
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Tree_Create
    subroutine CTA_Tree_Create( htree, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  htree
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Tree_Create
  end interface

  !  \brief Free the COSTA tree instance
  ! 
  !   \note
  ! 
  !   \param htree  IO handle of the tree instance, replaced by CTA_NULL on return
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Tree_Free
    subroutine CTA_Tree_Free( htree, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  htree
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Tree_Free
  end interface

  !  \brief Add a COSTA handle to the COSTA tree
  ! 
  !   \note
  ! 
  !   \param htree  IO handle of the tree object (parent)
  !   \param name   I  name of the COSTA item
  !   \param hitem  I  handle of the COSTA item to add (do not free the object after adding it to the tree)
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Tree_AddHandle
    subroutine CTA_Tree_AddHandle( htree, name, hitem, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  htree
      character(len=*)              , intent(in   )     ::  name
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hitem
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Tree_AddHandle
  end interface

  !  \brief Count the number of COSTA handles specified by the given path.
  ! 
  !   \param htree  I  handle of the tree object
  !   \param path   I  path of the item, separated by / or \\
  !   \param count  O  receives the number of items found
  !   \param status O CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
  !
  interface CTA_F90_Tree_CountHandles
    subroutine CTA_Tree_CountHandles( htree, path, count, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htree
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  path
      integer                       , intent(out  )     ::  count
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Tree_CountHandles
  end interface

  !  \brief Count the number of COSTA handles specified by the given path.
  ! 
  !   \param htree  I  handle of the tree object
  !   \param path   I  path of the item, separated by / or \
  !   \param count  O  receives the number of items found
  !   \param status O CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
  !
  interface CTA_F90_Tree_CountHandlesStr
    subroutine CTA_Tree_CountHandlesStr( htree, path, count, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htree
      character(len=*)              , intent(in   )     ::  path(*)
      integer                       , intent(out  )     ::  count
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Tree_CountHandlesStr
  end interface

  !  \brief Get a COSTA handle from the COSTA tree (by path)
  ! 
  !   \note In case of trees with default values, returns the default value.
  !   \note The returned handle must not be freed.
  ! 
  !   \param htree  I  handle of the tree object
  !   \param path   I  path of the item, separated by / or \\
  !   \param hitem  O  receives the handle of the COSTA item, or CTA_NULL in case not found, do not free this handle.
  !   \param status O CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
  !
  interface CTA_F90_Tree_GetHandle
    subroutine CTA_Tree_GetHandle( htree, path, hitem, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htree
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  path
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hitem
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Tree_GetHandle
  end interface

  !  \brief Get the value of a COSTA handle from the COSTA tree (by path)
  ! 
  !   \note In case of trees with default values, returns the default value.
  ! 
  !   \param htree    I  handle of the tree object
  !   \param path     I  COSTA string describing path of the item, separated by / or \
  !   \param value    O  receives the value of the COSTA item, or CTA_NULL in case of not found
  !   \param datatype I  data type of parameter value, must be the same as item in tree
  !   \param status O CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
  !
  !interface CTA_F90_Tree_GetValue
  !  subroutine CTA_Tree_GetValue( htree, path, value, datatype, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htree
  !    integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  path
  !    void                          , intent(out  )     ::  value
  !    integer                       , intent(in   )     ::  datatype
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_Tree_GetValue
  !end interface

  !  \brief Get a COSTA handle from the COSTA tree (by path)
  ! 
  !   \note In case of trees with default values, returns the default value.
  !   \note The returned handle must not be freed.
  ! 
  !   \param htree  I  handle of the tree object
  !   \param str    I  C string describing path of the item, separated by / or \
  !   \param hitem  O  receives the handle of the COSTA item, or CTA_NULL in case of not found, do not free this handle
  !   \param status O CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
  !
  interface CTA_F90_Tree_GetHandleStr
    subroutine CTA_Tree_GetHandleStr( htree, str, hitem, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htree
      character(len=*)              , intent(in   )     ::  str
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hitem
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Tree_GetHandleStr
  end interface

  !  \brief Get the value of a COSTA handle from the COSTA tree (by path)
  ! 
  !   \note In case of trees with default values, returns the default value.
  ! 
  !   \param htree    I  handle of the tree instance
  !   \param str      I  C string describing path of the item, separated by / or \
  !   \param value    O  receives the value of the COSTA item, or CTA_NULL in case of not found
  !   \param datatype I  data type of the value specified
  !   \param status O CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
  !
  !interface CTA_F90_Tree_GetValueStr
  !  subroutine CTA_Tree_GetValueStr( htree, str, value, datatype, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htree
  !    character(len=*)              , intent(in   )     ::  str
  !    void                          , intent(out  )     ::  value
  !    integer                       , intent(in   )     ::  datatype
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_Tree_GetValueStr
  !end interface

  !  \brief Count the number of elements on the current level of the COSTA tree
  ! 
  !   \param htree  I  handle of the tree level
  !   \param count  O  receives the number of elements on the current tree level
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Tree_CountItems
    subroutine CTA_Tree_CountItems( htree, count, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htree
      integer                       , intent(out  )     ::  count
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Tree_CountItems
  end interface

  !  \brief Get a handle (by index) on the current level of the COSTA tree
  ! 
  !   \param htree  I  handle of the tree level
  !   \param index  I  index of the item to return, 1 <= index <= CTA_Tree_CountItems()
  !   \param hitem  O  receives handle of the item at given index
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Tree_GetItem
    subroutine CTA_Tree_GetItem( htree, index, hitem, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htree
      integer                       , intent(in   )     ::  index
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hitem
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Tree_GetItem
  end interface

  !  \brief Get the value of a COSTA handle from the COSTA tree (by index)
  ! 
  !   \note In case of trees with default values, returns the default value.
  ! 
  !   \param htree    I  handle of the tree instance
  !   \param index    I  index of the item
  !   \param value    O  receives value of the COSTA item, or CTA_NULL in case of not found
  !   \param datatype I  data type of the value specified
  !   \param status O CTA_OK if successful or CTA_ITEM_NOT_FOUND in case not found
  !
  !interface CTA_F90_Tree_GetItemValue
  !  subroutine CTA_Tree_GetItemValue( htree, index, value, datatype, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htree
  !    integer                       , intent(in   )     ::  index
  !    void                          , intent(out  )     ::  value
  !    integer                       , intent(in   )     ::  datatype
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_Tree_GetItemValue
  !end interface

  !  \brief Print a COSTA tree to STDOUT
  ! 
  !   \note
  ! 
  !   \param htree  I  handle of the tree
  !   \param status O CTA_OK if successful
  !
  interface CTA_F90_Tree_Print
    subroutine CTA_Tree_Print( htree, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htree
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Tree_Print
  end interface


end module cta_f90_tree

