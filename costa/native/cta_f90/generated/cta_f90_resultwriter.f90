module cta_f90_resultwriter

  implicit none

  public

  !  \brief Handle a string message send to the resultwriter
  ! 
  !  \param idWriter    I  ID of this resultwriter (Counter of number of native result writers)
  !  \param config      I  Name of XML configuration file containting the function pointers and additional information
  !  \param workingDir  I  Full path to working directory
  !  \param message     I  Message send to resultwriter
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Resultwriter_putmessage
    subroutine CTA_Resultwriter_putmessage( idWriter, config, workingDir, message, status )
      integer                       , intent(in   )     ::  idWriter
      character(len=*)              , intent(in   )     ::  config(*)
      character(len=*)              , intent(in   )     ::  workingDir(*)
      character(len=*)              , intent(in   )     ::  message(*)
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Resultwriter_putmessage
  end interface

  !  \brief Handle a string message send to the resultwriter
  ! 
  !  \param idWriter    I  ID of this resultwriter (Counter of number of native result writers)
  !  \param config      I  Name of XML configuration file containting the function pointers and additional information
  !  \param workingDir  I  Full path to working directory
  !  \param id          I  Name of the variable/array send to the resultwriter
  !  \param handle      I  Handle (Vector or TreeVector) of variable
  !  \param outputLevel I  Selected output level (see opendabridge for possible values)
  !  \param context     I  Location from which the resultwriter was called
  !  \param iteration   I  Iteration number from which the resultwriter was called
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Resultwriter_putvalue
    subroutine CTA_Resultwriter_putvalue( idWriter, config, workingDir, id, handle, outputLevel, context, iteration, status )
      integer                       , intent(in   )     ::  idWriter
      character(len=*)              , intent(in   )     ::  config(*)
      character(len=*)              , intent(in   )     ::  workingDir(*)
      character(len=*)              , intent(in   )     ::  id(*)
      integer                       , intent(in   )     ::  handle
      integer                       , intent(in   )     ::  outputLevel
      character(len=*)              , intent(in   )     ::  context(*)
      integer                       , intent(in   )     ::  iteration
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Resultwriter_putvalue
  end interface

  !  \brief Handle a string message send to the resultwriter
  ! 
  !  \param idWriter    I  ID of this resultwriter (Counter of number of native result writers)
  !  \param config      I  Name of XML configuration file containting the function pointers and additional information
  !  \param workingDir  I  Full path to working directory
  !  \param iteration   I  Iteration number from which the resultwriter was called
  !  \param cost        I  Value of cost function
  !  \param handle      I  Handle (Vector or TreeVector) of the current parameters
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Resultwriter_putiterationreport
    subroutine CTA_Resultwriter_putiterationreport( idWriter, config, workingDir, iteration, cost, handle, status )
      integer                       , intent(in   )     ::  idWriter
      character(len=*)              , intent(in   )     ::  config(*)
      character(len=*)              , intent(in   )     ::  workingDir(*)
      integer                       , intent(in   )     ::  iteration
      real(8)                       , intent(in   )     ::  cost
      integer                       , intent(in   )     ::  handle
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Resultwriter_putiterationreport
  end interface

  !  \brief Free a resultwriter (close output files etc).
  ! 
  !  \param idWriter    I  ID of this resultwriter (Counter of number of native result writers)
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Resultwriter_free
    subroutine CTA_Resultwriter_free( idWriter, status )
      integer                       , intent(in   )     ::  idWriter
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Resultwriter_free
  end interface


end module cta_f90_resultwriter

