module cta_f90_time

  implicit none

  public

  !  \brief Create a time object.
  ! 
  !  \param htime  O  receives handle of newly created time object
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_Create
    subroutine CTA_Time_Create( htime, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  htime
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_Create
  end interface

  !  \brief Free a time object.
  ! 
  !  \param htime    IO handle of time object to be freed, replaced by CTA_NULL on return.
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_Free
    subroutine CTA_Time_Free( htime, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  htime
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_Free
  end interface

  !  \brief Set the time span.
  ! 
  !  \param htime    IO time object of which to set time span
  !  \param tstart   I  starting time
  !  \param tend     I  ending time
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_SetSpan
    subroutine CTA_Time_SetSpan( htime, tstart, tend, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_TIME_RKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  htime
      real(CTA_TIME_RKIND)          , intent(in   )     ::  tstart
      real(CTA_TIME_RKIND)          , intent(in   )     ::  tend
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_SetSpan
  end interface

  !  \brief Get the time span.
  ! 
  !  \param htime    I  time object of which to get time span
  !  \param tstart   O  receives the starting time
  !  \param tend     O  receives ending time
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_GetSpan
    subroutine CTA_Time_GetSpan( htime, tstart, tend, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_TIME_RKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      real(CTA_TIME_RKIND)          , intent(out  )     ::  tstart
      real(CTA_TIME_RKIND)          , intent(out  )     ::  tend
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_GetSpan
  end interface

  !  \brief Set the time step.
  ! 
  !  \param htime    IO time object of which to set time step
  !  \param tstep    I  new time step
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_SetStep
    subroutine CTA_Time_SetStep( htime, tstep, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_TIME_RKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  htime
      real(CTA_TIME_RKIND)          , intent(in   )     ::  tstep
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_SetStep
  end interface

  !  \brief Get time step.
  ! 
  !  \param htime    IO time object of which to get time step
  !  \param tstep    O  receives time step
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_GetStep
    subroutine CTA_Time_GetStep( htime, tstep, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_TIME_RKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  htime
      real(CTA_TIME_RKIND)          , intent(out  )     ::  tstep
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_GetStep
  end interface

  !  \brief Count number of timesteps in time
  ! 
  !  \param htime    I time object (see function description)
  !  \param nsteps   O number of timesteps
  ! 
  !  \param status O error status: CTA_OK if successful
  ! 
  !  \note number of steps is rounded to nearest integer
  !
  interface CTA_F90_Time_CountSteps
    subroutine CTA_Time_CountSteps( htime, nsteps, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      integer                       , intent(out  )     ::  nsteps
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_CountSteps
  end interface

  !  \brief Get interval of i-th step
  ! 
  !  \param htime    I time object (see function description)
  !  \param istep    I interval of step
  !  \param hstep    O time step of model
  ! 
  !  \param status O error status: CTA_OK if successful
  ! 
  !  \note intervals are counted from 1 to nsteps
  !
  interface CTA_F90_Time_GetTimeStep
    subroutine CTA_Time_GetTimeStep( htime, istep, hstep, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      integer                       , intent(in   )     ::  istep
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hstep
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_GetTimeStep
  end interface

  !  \brief Check whether htimesub is within time span of htime.
  ! 
  !  \param htimesub I time object (see function description)
  !  \param htime    I time object (see function description)
  !  \param inspan   O receives TRUE if htimesub is within time span of htime or FALSE otherwise
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_InSpan
    subroutine CTA_Time_InSpan( htimesub, htime, inspan, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htimesub
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      integer                       , intent(out  )     ::  inspan
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_InSpan
  end interface

  !  \brief Check whether time step of time object equals t
  ! 
  !  \param htime    I time object
  !  \param t        I time step to compare
  !  \param isstep   O receives TRUE if t equals time step of time object or FALSE otherwise
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_IsStep
    subroutine CTA_Time_IsStep( htime, t, isstep, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_TIME_RKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      real(CTA_TIME_RKIND)          , intent(in   )     ::  t
      integer                       , intent(out  )     ::  isstep
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_IsStep
  end interface

  !  \brief Copy a time object.
  ! 
  !  \param hfrom    I time object to copy from
  !  \param hto      O handle of time object that receives copy, must exist before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_Copy
    subroutine CTA_Time_Copy( hfrom, hto, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hfrom
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hto
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_Copy
  end interface

  !  \brief Export a time object.
  !  exports the whole internal state of the time object to given target
  !  CTA_FILE will export the time component in a MATLAB/OCTAVE readable form
  !  CTA_PACK will pack the content
  ! 
  !  \param htime    I time object to export
  !  \param hexport  I target for export (CTA_FILE or CTA_PACK)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_Export
    subroutine CTA_Time_Export( htime, hexport, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hexport
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_Export
  end interface

  !  \brief Import a time object.
  !  imports the whole internal state of the time object from given source
  ! 
  !  \param htime    I time object to import to
  !  \param himport  I source of import (CTA_PACK)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_Import
    subroutine CTA_Time_Import( htime, himport, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  himport
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_Import
  end interface

  !  \brief Returns whether time object describes an timespan or a single
  !  instance.
  ! 
  !  \param htime    I time object to import to
  !  \param isspan   O time object is a time timespan (CTA_TRUE/CTA_FALSE)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Time_IsSpan
    subroutine CTA_Time_IsSpan( htime, isspan, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  htime
      integer                       , intent(out  )     ::  isspan
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Time_IsSpan
  end interface


end module cta_f90_time

