module CTA_F90_Parameters

  implicit none
  

  ! --- in/out -----------------------------------
  
  public


  ! --- const ------------------------------------
  
  include 'cta_f90.inc'
  
  ! integer kind used for handles:
  integer, parameter  :: CTA_HANDLE_IKIND = 4

  ! Do not use the 'sizeof' function! Not standard ...
  !integer, parameter  :: CTA_HANDLE_IKIND = sizeof(CTA_HANDLE)
  
  ! real kind used for time values:
  integer, parameter  :: CTA_TIME_RKIND = 8
  
  
end module CTA_F90_Parameters
