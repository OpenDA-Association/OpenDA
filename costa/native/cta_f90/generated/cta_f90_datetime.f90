module cta_f90_datetime

  implicit none

  public

  !  \brief Julian day number from Gregorian date.
  ! 
  !  \param year    I  Year
  !  \param month   I  Month
  !  \param day     I  Day
  !  \param hour    I  Hour
  !  \param minute  I  Minute
  !  \param second  I  Second
  !  \param jd      O  Julian day number
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_DateTime_GregorianToJulian
    subroutine CTA_DateTime_GregorianToJulian( year, month, day, hour, minute, second, jd, status )
      use CTA_F90_Parameters, only : CTA_TIME_RKIND
      integer                       , intent(in   )     ::  year
      integer                       , intent(in   )     ::  month
      integer                       , intent(in   )     ::  day
      integer                       , intent(in   )     ::  hour
      integer                       , intent(in   )     ::  minute
      integer                       , intent(in   )     ::  second
      real(CTA_TIME_RKIND)          , intent(out  )     ::  jd
      integer                       , intent(out  )     ::  status
    end subroutine CTA_DateTime_GregorianToJulian
  end interface

  !  \brief Modified Julian day number from Gregorian date.
  ! 
  !  \param year    I  Year
  !  \param month   I  Month
  !  \param day     I  Day
  !  \param hour    I  Hour
  !  \param minute  I  Minute
  !  \param second  I  Second
  !  \param mjd     O  Modified Julian day number
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_DateTime_GregorianToModifiedJulian
    subroutine CTA_DateTime_GregorianToModifiedJulian( year, month, day, hour, minute, second, mjd, status )
      use CTA_F90_Parameters, only : CTA_TIME_RKIND
      integer                       , intent(in   )     ::  year
      integer                       , intent(in   )     ::  month
      integer                       , intent(in   )     ::  day
      integer                       , intent(in   )     ::  hour
      integer                       , intent(in   )     ::  minute
      integer                       , intent(in   )     ::  second
      real(CTA_TIME_RKIND)          , intent(out  )     ::  mjd
      integer                       , intent(out  )     ::  status
    end subroutine CTA_DateTime_GregorianToModifiedJulian
  end interface

  !  \brief Convert days into hours, minutes, and seconds.
  ! 
  !  \param days    I  Year
  !  \param hour    O  Hour
  !  \param minute  O  Minute
  !  \param second  O  Second
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_DateTime_DaysToHMS
    subroutine CTA_DateTime_DaysToHMS( days, hour, minute, second, status )
      use CTA_F90_Parameters, only : CTA_TIME_RKIND
      real(CTA_TIME_RKIND)          , intent(in   )     ::  days
      integer                       , intent(out  )     ::  hour
      integer                       , intent(out  )     ::  minute
      integer                       , intent(out  )     ::  second
      integer                       , intent(out  )     ::  status
    end subroutine CTA_DateTime_DaysToHMS
  end interface

  !  \brief Julian day number from Modified Julian day number
  ! 
  !  \param mjd     I  Modified Julian day number
  !  \param jd      O  Julian day number
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_DateTime_ModifiedJulianToJulian
    subroutine CTA_DateTime_ModifiedJulianToJulian( mjd, jd, status )
      use CTA_F90_Parameters, only : CTA_TIME_RKIND
      real(CTA_TIME_RKIND)          , intent(in   )     ::  mjd
      real(CTA_TIME_RKIND)          , intent(out  )     ::  jd
      integer                       , intent(out  )     ::  status
    end subroutine CTA_DateTime_ModifiedJulianToJulian
  end interface

  !  \brief Gregorian calendar date from Julian day number
  ! 
  !  \param jd      I  Julian day number
  !  \param year    O  Year
  !  \param month   O  Month
  !  \param day     O  Day
  !  \param hour    O  Hour
  !  \param minute  O  Minute
  !  \param second  O  Second
  !  \param jd      O  Julian day number
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_DateTime_JulianToGregorian
    subroutine CTA_DateTime_JulianToGregorian( jd, year, month, day, hour, minute, second, status )
      use CTA_F90_Parameters, only : CTA_TIME_RKIND
      real(CTA_TIME_RKIND)          , intent(out  )     ::  jd
      integer                       , intent(out  )     ::  year
      integer                       , intent(out  )     ::  month
      integer                       , intent(out  )     ::  day
      integer                       , intent(out  )     ::  hour
      integer                       , intent(out  )     ::  minute
      integer                       , intent(out  )     ::  second
      integer                       , intent(out  )     ::  status
    end subroutine CTA_DateTime_JulianToGregorian
  end interface

  !  \brief Gregorian calendar date from Modified Julian day number
  ! 
  !  \param mjd     I  Modified Julian day number
  !  \param year    O  Year
  !  \param month   O  Month
  !  \param day     O  Day
  !  \param hour    O  Hour
  !  \param minute  O  Minute
  !  \param second  O  Second
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_DateTime_ModifiedJulianToGregorian
    subroutine CTA_DateTime_ModifiedJulianToGregorian( mjd, year, month, day, hour, minute, second, status )
      use CTA_F90_Parameters, only : CTA_TIME_RKIND
      real(CTA_TIME_RKIND)          , intent(in   )     ::  mjd
      integer                       , intent(out  )     ::  year
      integer                       , intent(out  )     ::  month
      integer                       , intent(out  )     ::  day
      integer                       , intent(out  )     ::  hour
      integer                       , intent(out  )     ::  minute
      integer                       , intent(out  )     ::  second
      integer                       , intent(out  )     ::  status
    end subroutine CTA_DateTime_ModifiedJulianToGregorian
  end interface


end module cta_f90_datetime

