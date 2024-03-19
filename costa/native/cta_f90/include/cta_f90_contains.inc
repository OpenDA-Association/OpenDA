  ! return error message given error code

  function CTA_F90_StrError( ierr )

    use CTA_F90_Parameters, only : CTA_OK
    use CTA_F90_Parameters, only : CTA_ARRAY_TOO_SHORT
    use CTA_F90_Parameters, only : CTA_ILLEGAL_DATATYPE
    use CTA_F90_Parameters, only : CTA_DIMENSION_ERROR
    use CTA_F90_Parameters, only : CTA_INCOMPATIBLE_VECTORS
    use CTA_F90_Parameters, only : CTA_CONCAT_NOT_POSSIBLE
    use CTA_F90_Parameters, only : CTA_SETVAL_NOT_POSSIBLE
    use CTA_F90_Parameters, only : CTA_ITEM_NOT_FOUND
    use CTA_F90_Parameters, only : CTA_UNINITIALISED_SUBSTATES
    use CTA_F90_Parameters, only : CTA_STATES_NOT_COMPATIBLE
    use CTA_F90_Parameters, only : CTA_INCOMPATIBLE_MATRICES
    use CTA_F90_Parameters, only : CTA_NOT_IMPLEMENTED

    ! --- in/out ---------------------------------

    character(len=80)     ::  CTA_F90_StrError
    integer, intent(in)   ::  ierr

    ! --- begin ----------------------------------

    select case ( ierr )
      case ( CTA_OK                      ) ! ok, no message
      case ( CTA_ARRAY_TOO_SHORT         ) ; CTA_F90_StrError = 'CTA - ERROR - Array too short'
      case ( CTA_ILLEGAL_DATATYPE        ) ; CTA_F90_StrError = 'CTA - ERROR - Illegal datatype'
      case ( CTA_DIMENSION_ERROR         ) ; CTA_F90_StrError = 'CTA - ERROR - Dimension error'
      case ( CTA_INCOMPATIBLE_VECTORS    ) ; CTA_F90_StrError = 'CTA - ERROR - Incompatible vectors'
      case ( CTA_CONCAT_NOT_POSSIBLE     ) ; CTA_F90_StrError = 'CTA - ERROR - Concat not possible'
      case ( CTA_SETVAL_NOT_POSSIBLE     ) ; CTA_F90_StrError = 'CTA - ERROR - Setval not possible'
      case ( CTA_ITEM_NOT_FOUND          ) ; CTA_F90_StrError = 'CTA - ERROR - Item not found'
      case ( CTA_UNINITIALISED_SUBSTATES ) ; CTA_F90_StrError = 'CTA - ERROR - Uninitialised substates'
      case ( CTA_STATES_NOT_COMPATIBLE   ) ; CTA_F90_StrError = 'CTA - ERROR - States not compatible'
      case ( CTA_INCOMPATIBLE_MATRICES   ) ; CTA_F90_StrError = 'CTA - ERROR - Incompatible matrices'
      case ( CTA_NOT_IMPLEMENTED         ) ; CTA_F90_StrError = 'CTA - ERROR - Not implemented'
      case default
        write (CTA_F90_StrError,'("CTA - ERROR - Unknown error code: ",i8)') ierr
    end select

  end function CTA_F90_StrError
