module cta_f90_message

  implicit none

  public

  !  \brief Write a message
  ! 
  !  \param className I name of class that writes the message
  !  \param method    I name of the method that writes the message
  !  \param message   I message
  !  \param type      I type of message
  !                     -'M':message
  !                     -'I':Info
  !                     -'W':Warning
  !                     -'E':Error
  !                     -'F':Fatal error  (will terminate application)
  !
  interface CTA_F90_Message_Write
    subroutine CTA_Message_Write( className, method, message, type )
      character(len=*)              , intent(in   )     ::  className
      character(len=*)              , intent(in   )     ::  method
      character(len=*)              , intent(in   )     ::  message
      character(len=*)              , intent(in   )     ::  type
    end subroutine CTA_Message_Write
  end interface

  !  \brief Set an external writer for handling messages
  ! 
  !   An external writer must comply to the following C interface:
  ! 
  !   void my_writer(char *className, char *method, char *message, char type);
  ! 
  !  \param externalWriter I External writer
  ! 
  ! 
  !   \note Fortran writers are not yet supported
  !
  interface CTA_F90_Message_SetExternalWriter
    subroutine CTA_Message_SetExternalWriter( externalWriter )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  externalWriter
    end subroutine CTA_Message_SetExternalWriter
  end interface

  !  \brief Toggle message handler between quiet and normal mode.
  !   in the quiet mode no messages are send (not even to external writers)
  ! 
  !  \param setting  I set message handler in quiet mode CTA_TRUE/CTA_FALSE
  ! 
  !
  interface CTA_F90_Message_Quiet
    subroutine CTA_Message_Quiet( setting )
      integer                       , intent(in   )     ::  setting
    end subroutine CTA_Message_Quiet
  end interface


end module cta_f90_message

