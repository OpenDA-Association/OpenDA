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
 
!! @author Dirk van Eijkeren   VORtech BV  
!!                             P.O. Box 260
!!                             2600 AG Delft
!!                             The Netherlands
!!                             www.vortech.nl

! ----------------------------------------------------------------------------
! Module for storing the WSER time series for each model instance.
! 
! Storage can be allocated, deallocated for a model instance and enlarged.  
! The time series can be copied to and from EFDC memory into instance memory
!
! This module also contains the identifiers for the exchange items used to set
! or get values for these WSER time series.   
! ----------------------------------------------------------------------------
module model_wser_time_series

  type wser_time_series

     ! time series length
     integer :: NDWSER, NWSER
     integer, allocatable, dimension(:) :: MWSER

     ! wind-series contains the time-series, wind-direction and wind-speed
     ! NOTE: In specific cases wind-direction and wind-speed are used to
     !       store the north and eastward component for the wind-velocity
     !       vector instead.
     real, allocatable, dimension(:,:) :: TWSER !  (NDWSER,NWSERM)
     real, allocatable, dimension(:,:) :: WINDD, WINDS  ! (NDWSER,NWSERM)

  end type wser_time_series

  type(wser_time_series), pointer, dimension(:) :: wsert
  
  integer :: ndwser_max, nwser_max
  logical, parameter, private :: debug= .false.

contains

  subroutine model_wser_allocate(id, n, m)

    implicit none

    ! arguments
    integer, intent(in) :: id
    integer, intent(in) :: n
    integer, intent(in) :: m  ! size

    if (debug) print*, "model_wser_allocate", n, m
    wsert(id)%NDWSER = n
    wsert(id)%NWSER = m

    allocate(wsert(id)%MWSER(m))
    allocate(wsert(id)%TWSER(n,m))    
    allocate(wsert(id)%WINDD(n,m))
    allocate(wsert(id)%WINDS(n,m))

    wsert(id)%MWSER = n
    wsert(id)%TWSER = 0.0
    wsert(id)%TWSER(n,:) = 1.0e30 
    wsert(id)%WINDD  = 0.0
    wsert(id)%WINDS  = 0.0

  end subroutine model_wser_allocate

  ! --------------------------------------------------------------------------
  ! Subroutine for allocating arrays for instance.
  ! --------------------------------------------------------------------------
  subroutine model_wser_deallocate(id)

    implicit none

    integer, intent(in) :: id ! model instance identifier

    if (debug) print*, 'deallocate wser time series', allocated(wsert(id)%MWSER)
    deallocate(wsert(id)%MWSER)
    deallocate(wsert(id)%TWSER)
    deallocate(wsert(id)%WINDD)
    deallocate(wsert(id)%WINDS)
    wsert(id)%NDWSER=0
    wsert(id)%NWSER=0
    
  end subroutine model_wser_deallocate

  ! --------------------------------------------------------------------------
  ! Function for storing the current WSER time series in instance memory
  ! --------------------------------------------------------------------------
  function model_get_wser(id) result (ret_val)
    
    use global, only: WINDD, WINDS, MWSER, TWSER
    
    implicit none
    ! return argument
    integer :: ret_val        ! ret_val = 0: succes, ret_val = -1: error

    !arguments
    integer, intent(in) :: id ! model instance identifier
    
    ! local variables
    integer :: ndser, nser ! Size of the time-series and its arrays
    
    ret_val = -1
    if (debug) print*, 'time_series is allocated ', allocated(wsert(id)%TWSER)  
    ndser = wsert(id)%NDWSER
    nser  = wsert(id)%NWSER
    wsert(id)%MWSER(        1:nser) = MWSER(        1:nser)
    wsert(id)%TWSER(1:ndser,1:nser) = TWSER(1:ndser,1:nser)
    wsert(id)%WINDD(1:ndser,1:nser) = WINDD(1:ndser,1:nser)
    wsert(id)%WINDS(1:ndser,1:nser) = WINDS(1:ndser,1:nser)
    ret_val = 0

  end function model_get_wser


  ! --------------------------------------------------------------------------
  ! Function for setting the WSER time series in EFDC from instance memory
  ! --------------------------------------------------------------------------
  function model_set_wser(id) result(ret_val)

    use global, only: WINDD, WINDS, MWSER, TWSER, NDWSER, NWSERM
 
    implicit none

    ! return value
    integer :: ret_val         ! ret_val = 0: success, ret_val = -1: error

    ! arguments
    integer, intent(in) :: id ! model instance identifier
    
    ! local variables
    integer :: ndser, nser

    ret_val = 0

    NDWSER = ndwser_max
    NWSERM = nwser_max
    ndser = wsert(id)%NDWSER
    nser  = wsert(id)%NWSER
    
    MWSER(        1:nser) = wsert(id)%MWSER(        1:nser)
    TWSER(1:ndser,1:nser) = wsert(id)%TWSER(1:ndser,1:nser)
    WINDD(1:ndser,1:nser) = wsert(id)%WINDD(1:ndser,1:nser)
    WINDS(1:ndser,1:nser) = wsert(id)%WINDS(1:ndser,1:nser)

  end function model_set_wser



  ! --------------------------------------------------------------------------
  ! Function for enlarging (if required) the arrays in instance memory 
  ! and EFDC memory, if longer time series are passed than are currently 
  ! allocated
  ! Note: only de NDPSER dimension is considered
  ! --------------------------------------------------------------------------
  function enlarge_wser_time_series(id,size_n,size_m) result(ret_val)

    use global, only: NDWSER ,NWSERM, TWSER, &
         MWTLAST, TAWSER, TCWSER, & 
         MWSER, TWSER, WINDD, WINDS

    implicit none

    ! result
    integer :: ret_val ! =0 succes; <0 error

    ! arguments
    integer, intent(in) :: id     ! model instance identifer
    integer, intent(in) :: size_n ! requested length of time series
    integer, intent(in) :: size_m ! number of locations

    ! locals
    type(wser_time_series):: wsert_orig
    integer :: n, m
    integer :: new_n,new_m
    REAL,ALLOCATABLE,DIMENSION(:,:)::WINDD_orig
    REAL,ALLOCATABLE,DIMENSION(:,:)::WINDS_orig
    REAL,ALLOCATABLE,DIMENSION(:,:)::TWSER_orig

    ret_val = -1

    n = wsert(id)%NDWSER
    m = wsert(id)%NWSER

    new_m = size_m
    new_n = size_n

    if ((size_n > wsert(id)%NDWSER)) then 
       if (debug) print*, "enlarge_wser_time_series from", id, n, m 
       if (debug) print*, "enlarge_wser_time_series to", id, size_n, size_m 

       allocate(wsert_orig%MWSER(m))
       allocate(wsert_orig%TWSER(n,m))    
       allocate(wsert_orig%WINDD(n,m))
       allocate(wsert_orig%WINDS(n,m))

       wsert_orig%MWSER = wsert(id)%MWSER  
       wsert_orig%TWSER = wsert(id)%TWSER  
       wsert_orig%WINDD = wsert(id)%WINDD
       wsert_orig%WINDS = wsert(id)%WINDS

       call model_wser_deallocate(id)
       call model_wser_allocate(id, new_n, new_m)

       wsert(id)%MWSER(1:m)     = wsert_orig%MWSER(1:m)
       wsert(id)%TWSER(1:n,1:m) = wsert_orig%TWSER(1:n,1:m) 
       wsert(id)%WINDD(1:n,1:m) = wsert_orig%WINDD(1:n,1:m)
       wsert(id)%WINDS(1:n,1:m) = wsert_orig%WINDS(1:n,1:m)

       deallocate(wsert_orig%MWSER)
       deallocate(wsert_orig%TWSER)    
       deallocate(wsert_orig%WINDD)
       deallocate(wsert_orig%WINDS)

       if (wsert(id)%NDWSER > ndwser_max) then
          if (debug) print*, 'reallocating WSER times series variables'
          allocate(WINDD_orig(n,m))
          allocate(WINDS_orig(n,m))
          allocate(TWSER_orig(n,m))
          
          TWSER_orig = TWSER
          WINDD_orig  = WINDD
          WINDS_orig  = WINDS
          deallocate(TWSER)
          deallocate(WINDD)
          deallocate(WINDS)

          ndwser_max = wsert(id)%NDWSER
          nwser_max = wsert(id)%NWSER
          NDWSER = ndwser_max
          NWSERM = nwser_max

          allocate(TWSER(NDWSER, NWSERM))
          allocate(WINDD(NDWSER, NWSERM))
          allocate(WINDS(NDWSER, NWSERM))
          
          TWSER(1:n,1:m) = TWSER_orig
          WINDD(1:n,1:m)= WINDD_orig
          WINDS(1:n,1:m)= WINDS_orig
          deallocate(TWSER_orig)
          deallocate(WINDD_orig)
          deallocate(WINDS_orig)

       end if
       ret_val = 0
    else 
       ret_val = 0   
    end if

  end function enlarge_wser_time_series

end module model_wser_time_series
