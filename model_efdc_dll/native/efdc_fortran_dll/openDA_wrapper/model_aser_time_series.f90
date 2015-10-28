!! MOD_V2.0
!! Copyright (c) 2012 OpenDA Association
!! All rights reserved.
!!
!! This file is part of OpenDA.
!!
!! OpenDA is free software: you can redistribute it and/or modify
!! it under the terms of the GNU Lesser General Public License as
!! published by the Free Software Foundation, either version 3 of
!! the License, or (at your option) any later version.
!!
!! OpenDA is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU Lesser General Public License for more details.
!!
!! You should have received a copy of the GNU Lesser General Public License
!! along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
 
!! @author Werner Kramer       VORtech BV  
!!                             P.O. Box 260
!!                             2600 AG Delft
!!                             The Netherlands
!!                             www.vortech.nl

! ----------------------------------------------------------------------------
! Module for storing the ASER time series for each model instance.
! 
! Storage can be allocated, deallocated for a model instance and enlarged.  
! The time series can be copied to and from EFDC memory into instance memory
!
! This module also contains the identifiers for the exchange items used to set
! or get values for these ASER time series.   
! ----------------------------------------------------------------------------
module model_aser_time_series
  
  type aser_time_series

     ! time series length
     integer :: NDASER, NASER
     integer, allocatable, dimension(:) :: MASER

     !atmospheric forcings 
     real, allocatable, dimension(:,:) :: TASER !  (NDASER,NASERM)
     real, allocatable, dimension(:,:) :: RAIN, EVAP, TDRY, TWET  ! (NDASER,NASERM)
     real, allocatable, dimension(:,:) :: CLOUD, SOLSWR, PATM ! (NDASER,NASERM)

     ! daily averages for algal growth 
     ! these values should be stored and restores
     ! Normally on restart these are set to a default values specified in WQ3D
     real :: WQI3, WQI2, WQI1, WQI0, WQI0OPT

  end type aser_time_series

  ! maximum size of time series
  integer :: ndaser_max, naser_max
  type(aser_time_series), pointer, dimension(:) :: aser => NULL()
  logical, private, parameter :: debug = .false.

contains

  ! --------------------------------------------------------------------------
  ! Subroutine for allocating arrays for given model instance.
  ! --------------------------------------------------------------------------
  subroutine model_aser_allocate(id, n, m)

    implicit none

    ! arguments
    integer, intent(in) :: id ! model instance identifier
    integer, intent(in) :: n  ! number of data points
    integer, intent(in) :: m  ! number of locations 

    if (debug) print*, "model_aser_allocate", n, m
    aser(id)%NDASER = n
    aser(id)%NASER  = m

    allocate(aser(id)%MASER(m))
    allocate(aser(id)%TASER(n,m))    
    allocate(aser(id)%RAIN(n,m))
    allocate(aser(id)%EVAP(n,m))
    allocate(aser(id)%TDRY(n,m))
    allocate(aser(id)%TWET(n,m))
    allocate(aser(id)%CLOUD(n,m))
    allocate(aser(id)%SOLSWR(n,m))
    allocate(aser(id)%PATM(n,m))
    if (debug) print*, "model_aser_allocate", allocated(aser(id)%MASER)

    aser(id)%MASER = n
    aser(id)%TASER = 0.0
    aser(id)%TASER(n,:) = 1.0e30
    aser(id)%RAIN  = 0.0
    aser(id)%EVAP  = 0.0
    aser(id)%TDRY  = 0.0
    aser(id)%TWET  = 0.0
    aser(id)%CLOUD = 0.0
    aser(id)%SOLSWR= 0.0
    aser(id)%PATM  = 0.0
    
  end subroutine model_aser_allocate

  ! --------------------------------------------------------------------------
  ! Subroutine for deallocating ASER arrays for given model instance.
  ! --------------------------------------------------------------------------
  subroutine model_aser_deallocate(id)

    implicit none

    ! arguments
    integer, intent(in) :: id ! model instance identifier

    !local
    integer :: ALLOC_ERR

    if (debug) print*, 'deallocate aser time series', id, allocated(aser(id)%TASER)

    deallocate(aser(id)%MASER, STAT = ALLOC_ERR)
    deallocate(aser(id)%TASER)
    deallocate(aser(id)%RAIN)
    deallocate(aser(id)%EVAP)
    deallocate(aser(id)%TDRY)
    deallocate(aser(id)%TWET)
    deallocate(aser(id)%CLOUD)
    deallocate(aser(id)%SOLSWR)
    deallocate(aser(id)%PATM)

    aser(id)%NDASER=0
    aser(id)%NASER=0
    

  end subroutine model_aser_deallocate

  ! --------------------------------------------------------------------------
  ! Function for storing the current ASER time series in instance memory
  ! --------------------------------------------------------------------------
  function model_get_aser(id) result (ret_val)
    
    use global, only: MASER, TASER, EVAP, RAIN, TDRY, TWET, CLOUD, SOLSWR, PATM
    
    implicit none

    ! return argument
    integer :: ret_val        ! ret_val = 0: succes, ret_val = -1: error

    !arguments
    integer, intent(in) :: id ! model instance identifier

    !local
    integer :: n,m

    ret_val = -1
    if (debug) then 
       print*, 'get_aser: time_series is allocated ', allocated(aser(id)%TASER)
       print*, 'size: ', id, aser(id)%NDASER, aser(id)%NASER
       print*, 'max size ', ndaser_max, naser_max
    end if
    n = aser(id)%NDASER
    m = aser(id)%NASER
    aser(id)%MASER(1:m)     = MASER(1:m)
    aser(id)%TASER(1:n,1:m) = TASER(1:n,1:m) 
    aser(id)%EVAP(1:n,1:m)  = EVAP(1:n,1:m)
    aser(id)%RAIN(1:n,1:m)  = RAIN(1:n,1:m)
    aser(id)%TDRY(1:n,1:m)   = TDRY(1:n,1:m)
    aser(id)%TWET(1:n,1:m)  = TWET(1:n,1:m)
    aser(id)%CLOUD(1:n,1:m) = CLOUD(1:n,1:m)
    aser(id)%SOLSWR(1:n,1:m)= SOLSWR(1:n,1:m)
    aser(id)%PATM(1:n,1:m)  = PATM(1:n,1:m)
    
    ret_val = 0

  end function model_get_aser

  ! --------------------------------------------------------------------------
  ! Function for setting the ASER time series in EFDC from instance memory
  ! --------------------------------------------------------------------------
  function model_set_aser(id) result(ret_val)

    use global, only: MASER, TASER, EVAP, RAIN, &
         TDRY, TWET, CLOUD, SOLSWR, PATM, &
         NDASER, NASERM

    implicit none

    ! return value
    integer :: ret_val        ! ret_val = 0: success, ret_val = -1: error

    ! arguments
    integer, intent(in) :: id ! model instance identifier

    ! local
    integer :: n, m

    ret_val = -1
    if (debug) print*, "model_set_aser"

    NDASER = ndaser_max
    NASERM = naser_max

    n = aser(id)%NDASER
    m = aser(id)%NASER
    MASER(1:m)     = aser(id)%MASER(1:m)
    TASER(1:n,1:m) = aser(id)%TASER(1:n,1:m)
    EVAP(1:n,1:m)  = aser(id)%EVAP(1:n,1:m)
    RAIN(1:n,1:m)  = aser(id)%RAIN(1:n,1:m)
    TDRY(1:n,1:m)  = aser(id)%TDRY(1:n,1:m)
    TWET(1:n,1:m)  = aser(id)%TWET(1:n,1:m)
    CLOUD(1:n,1:m) = aser(id)%CLOUD(1:n,1:m)
    SOLSWR(1:n,1:m)= aser(id)%SOLSWR(1:n,1:m)
    PATM(1:n,1:m)  = aser(id)%PATM(1:n,1:m)
    ret_val = 0

  end function model_set_aser

  ! --------------------------------------------------------------------------
  ! Function for storing the daily averages solar intensity in instance memory
  ! --------------------------------------------------------------------------
  function model_get_daily_solar_intensity(id, call_on_init) result (ret_val)
    
    use global, only: WQI3, WQI2, WQI1, WQI0, WQI0OPT, TIMEDAY, DTWQ, IWQSUN
    
    implicit none

    ! return argument
    integer :: ret_val        ! ret_val = 0: succes, ret_val = -1: error

    !arguments
    integer, intent(in) :: id ! model instance identifier
    logical, intent(in) :: call_on_init ! true if called on init

    ret_val = -1
    if ( ((TIMEDAY - float(int(TIMEDAY))) <= DTWQ) .and. (.not.call_on_init) ) then
       WQI3 = WQI2
       WQI2 = WQI1
       WQI1 = WQI0OPT
       IF(IWQSUN.GT.0) WQI0OPT = 0.0
    endif
    aser(id)%WQI3 = WQI3
    aser(id)%WQI2 = WQI2
    aser(id)%WQI1 = WQI1
    aser(id)%WQI0 = WQI0
    aser(id)%WQI0OPT = WQI0OPT
    ret_val = 0

  end function model_get_daily_solar_intensity

  ! --------------------------------------------------------------------------
  ! Function for setting the daily averages solar intensity in EFDC from instance memory
  ! --------------------------------------------------------------------------
  function model_set_daily_solar_intensity(id) result(ret_val)
    
    use global, only: WQI3, WQI2, WQI1, WQI0, WQI0OPT

    implicit none

    ! return value
    integer :: ret_val        ! ret_val = 0: success, ret_val = -1: error

    ! arguments
    integer, intent(in) :: id ! model instance identifier
    WQI3 =aser(id)%WQI3
    WQI2 =aser(id)%WQI2
    WQI1 =aser(id)%WQI1
    WQI0 = aser(id)%WQI0
    WQI0OPT = aser(id)%WQI0OPT 
    ret_val = 0

  end function model_set_daily_solar_intensity 
  
  
  ! --------------------------------------------------------------------------
  ! Function for enlarging (if required) the arrays in instance memory 
  ! and EFDC memory, if longer time series are passed than are currently 
  ! allocated
  ! Note: Only the NDASER dimension is considered
  ! --------------------------------------------------------------------------
  function enlarge_aser_time_series(id,size_n,size_m) result(ret_val)

    use global, only: NDASER ,NASERM, &
         MASER, TASER, EVAP, RAIN, &
         TDRY, TWET, CLOUD, SOLSWR, PATM, &
         SOLFRD, SOLSRD, TSSRD, IRELH, ATMWHT, MATLAST, TAASER, TCASER, &
         LCM, NASER

    implicit none

    ! result
    integer :: ret_val            ! ret_val = 0: success, ret_val =-1:  error

    ! arguments
    integer, intent(in) :: id     ! model instance identifier enlarged
    integer, intent(in) :: size_n ! requested time series length
    integer, intent(in) :: size_m ! number of locations

    ! locals
    type(aser_time_series) :: aser_orig
    integer :: n, m
    integer :: new_n,new_m
    real, allocatable, dimension(:,:) :: TASER_orig, &!  (NDASER,NASERM)
                                         RAIN_orig, EVAP_orig, &
                                         TDRY_orig, TWET_orig, &
                                         CLOUD_orig, SOLSWR_orig, &
                                         PATM_orig
    
    ret_val = -1

    n = aser(id)%NDASER
    m = aser(id)%NASER 
    
    new_m = size_m
    new_n = size_n

    if ((size_n > aser(id)%NDASER)) then 
       if (debug) print*, "enlarge_aser_time_series", id, n, m 
       if (debug) print*, "enlarge_aser_time_series", id, size_n, size_m 

       allocate(aser_orig%MASER(m))
       allocate(aser_orig%TASER(n,m))    
       allocate(aser_orig%RAIN(n,m))
       allocate(aser_orig%EVAP(n,m))
       allocate(aser_orig%TDRY(n,m))
       allocate(aser_orig%TWET(n,m))
       allocate(aser_orig%CLOUD(n,m))
       allocate(aser_orig%SOLSWR(n,m))
       allocate(aser_orig%PATM(n,m))

       aser_orig%MASER = aser(id)%MASER  
       aser_orig%TASER = aser(id)%TASER  
       aser_orig%EVAP  = aser(id)%EVAP
       aser_orig%RAIN  = aser(id)%RAIN
       aser_orig%TDRY  = aser(id)%TDRY
       aser_orig%TWET  = aser(id)%TWET
       aser_orig%CLOUD = aser(id)%CLOUD
       aser_orig%SOLSWR= aser(id)%SOLSWR 
       aser_orig%PATM  = aser(id)%PATM

       call model_aser_deallocate(id)
       call model_aser_allocate(id, new_n, new_m)
       ! copy orig to new       
       aser(id)%MASER(1:m) = aser_orig%MASER(1:m)
       aser(id)%TASER(1:n,1:m) = aser_orig%TASER(1:n,1:m) 
       aser(id)%EVAP(1:n,1:m) = aser_orig%EVAP(1:n,1:m)
       aser(id)%RAIN(1:n,1:m) = aser_orig%RAIN(1:n,1:m)
       aser(id)%TDRY(1:n,1:m) = aser_orig%TDRY(1:n,1:m)
       aser(id)%TWET(1:n,1:m) = aser_orig%TWET(1:n,1:m)
       aser(id)%CLOUD(1:n,1:m) = aser_orig%CLOUD(1:n,1:m)
       aser(id)%SOLSWR(1:n,1:m) = aser_orig%SOLSWR(1:n,1:m)
       aser(id)%PATM(1:n,1:m) = aser_orig%PATM(1:n,1:m)

       deallocate(aser_orig%MASER)
       deallocate(aser_orig%TASER)    
       deallocate(aser_orig%RAIN)
       deallocate(aser_orig%EVAP)
       deallocate(aser_orig%TDRY)
       deallocate(aser_orig%TWET)
       deallocate(aser_orig%CLOUD)
       deallocate(aser_orig%SOLSWR)
       deallocate(aser_orig%PATM)

       if ( (aser(id)%NDASER > ndaser_max ) & 
            .or. ( aser(id)%NASER  > naser_max )  ) then 
          ! enlarge EFDC arrays 
          ! deallocate(MASER)
          allocate(TASER_orig(n, NASERM))
          allocate(EVAP_orig(n, NASERM))
          allocate(RAIN_orig(n, NASERM))
          allocate(TDRY_orig(n, NASERM))
          allocate(TWET_orig(n, NASERM))
          allocate(CLOUD_orig(n, NASERM))
          allocate(SOLSWR_orig(n, NASERM))
          allocate(PATM_orig(n, NASERM))
          
          TASER_orig = TASER  
          EVAP_orig  = EVAP
          RAIN_orig  = RAIN
          TDRY_orig  = TDRY
          TWET_orig  = TWET
          CLOUD_orig = CLOUD
          SOLSWR_orig= SOLSWR 
          PATM_orig  = PATM
          
          deallocate(TASER, EVAP, RAIN, TDRY, TWET, CLOUD, SOLSWR, PATM)
          deallocate(SOLFRD, SOLSRD, TSSRD)

          ndaser_max = aser(id)%NDASER
          naser_max = aser(id)%NASER
          NDASER = ndaser_max
          NASERM = naser_max

          !allocate(MASER(NASERM))
          allocate(TASER(NDASER, NASERM))
          allocate(EVAP(NDASER, NASERM))
          allocate(RAIN(NDASER, NASERM))
          allocate(TDRY(NDASER, NASERM))
          allocate(TWET(NDASER, NASERM))
          allocate(CLOUD(NDASER, NASERM))
          allocate(SOLSWR(NDASER, NASERM))
          allocate(PATM(NDASER, NASERM))

          TASER(1:n,1:m)  = TASER_orig
          EVAP(1:n,1:m)   = EVAP_orig
          RAIN(1:n,1:m)   = RAIN_orig
          TDRY(1:n,1:m)   = TDRY_orig
          TWET(1:n,1:m)   = TWET_orig
          CLOUD(1:n,1:m)  = CLOUD_orig
          SOLSWR(1:n,1:m) = SOLSWR_orig
          PATM(1:n,1:m)   = PATM_orig
          
          ! other (temporary) arrays with this size
          ALLOCATE(SOLFRD(NDASER))
          ALLOCATE(SOLSRD(NDASER))
          ALLOCATE(TSSRD(NDASER))
          
         deallocate(TASER_orig, EVAP_orig, RAIN_orig, TDRY_orig)
         deallocate(TWET_orig, CLOUD_orig, SOLSWR_orig, PATM_orig)
         
       end if
       ret_val = 0
    else 
       ret_val = 0    
    end if

  end function enlarge_aser_time_series

end module model_aser_time_series
