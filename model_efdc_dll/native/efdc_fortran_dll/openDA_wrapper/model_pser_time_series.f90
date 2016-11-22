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
! Module for storing the PSER time series for each model instance.
! 
! Storage can be allocated, deallocated for a model instance and enlarged.  
! The time series can be copied to and from EFDC memory into instance memory
!
! This module also contains the identifiers for the exchange items used to set
! or get values for these PSER time series.   
! ----------------------------------------------------------------------------
module model_pser_time_series

  type pser_time_series

     ! time series length
     integer :: NDPSER, NPSER
     integer, allocatable, dimension(:) :: MPSER

     ! waterlevel
     real, allocatable, dimension(:,:) :: TPSER !  (NDPSER,NPSERM)
     real, allocatable, dimension(:,:) :: PSER  ! (NDPSER,NPSERM)

  end type pser_time_series

  type(pser_time_series), pointer, dimension(:) :: psert
  
  integer :: ndpser_max, npser_max
  logical, parameter, private :: debug= .false.

contains

  subroutine model_pser_allocate(id, n, m)

    implicit none

    ! arguments
    integer, intent(in) :: id
    integer, intent(in) :: n
    integer, intent(in) :: m  ! size

    if (debug) print*, "model_pser_allocate", n, m
    psert(id)%NDPSER = n
    psert(id)%NPSER = m

    allocate(psert(id)%MPSER(m))
    allocate(psert(id)%TPSER(n,m))    
    allocate(psert(id)%PSER(n,m))

    psert(id)%MPSER = n
    psert(id)%TPSER = 0.0
    psert(id)%TPSER(n,:) = 1.0e30 
    psert(id)%PSER  = 0.0

  end subroutine model_pser_allocate

  ! --------------------------------------------------------------------------
  ! Subroutine for allocating arrays for instance.
  ! --------------------------------------------------------------------------
  subroutine model_pser_deallocate(id)

    implicit none

    integer, intent(in) :: id ! model instance identifier

    if (debug) print*, 'deallocate pser time series', allocated(psert(id)%MPSER)
    deallocate(psert(id)%MPSER)
    deallocate(psert(id)%TPSER)
    deallocate(psert(id)%PSER)
    psert(id)%NDPSER=0
    psert(id)%NPSER=0
    
  end subroutine model_pser_deallocate

  ! --------------------------------------------------------------------------
  ! Function for storing the current PSER time series in instance memory
  ! --------------------------------------------------------------------------
  function model_get_pser(id) result (ret_val)
    
    use global, only: PSER, MPSER, TPSER
    
    implicit none

    ! return argument
    integer :: ret_val        ! ret_val = 0: succes, ret_val = -1: error

    !arguments
    integer, intent(in) :: id ! model instance identifier
    
    ret_val = -1
    if (debug) print*, 'time_series is allocated ', allocated(psert(id)%TPSER)  
    psert(id)%MPSER(1:psert(id)%NPSER) = MPSER(1:psert(id)%NPSER)  
    psert(id)%TPSER(1:psert(id)%NDPSER,1:psert(id)%NPSER) = TPSER(1:psert(id)%NDPSER,1:psert(id)%NPSER) 
    psert(id)%PSER(1:psert(id)%NDPSER,1:psert(id)%NPSER) = PSER(1:psert(id)%NDPSER,1:psert(id)%NPSER)
    ret_val = 0

  end function model_get_pser


  ! --------------------------------------------------------------------------
  ! Function for setting the PSER time series in EFDC from instance memory
  ! --------------------------------------------------------------------------
  function model_set_pser(id) result(ret_val)

    use global, only: PSER, MPSER, TPSER, NDPSER, NPSERM
 
    implicit none

    ! return value
    integer :: ret_val         ! ret_val = 0: success, ret_val = -1: error

    ! arguments
    integer, intent(in) :: id ! model instance identifier

    ret_val = 0

    NDPSER = ndpser_max
    NPSERM = npser_max

    MPSER(1:psert(id)%NPSER) = psert(id)%MPSER(1:psert(id)%NPSER)
    TPSER(1:psert(id)%NDPSER,1:psert(id)%NPSER) = psert(id)%TPSER(1:psert(id)%NDPSER,1:psert(id)%NPSER)
    PSER(1:psert(id)%NDPSER,1:psert(id)%NPSER) = psert(id)%PSER(1:psert(id)%NDPSER,1:psert(id)%NPSER)


  end function model_set_pser



  ! --------------------------------------------------------------------------
  ! Function for enlarging (if required) the arrays in instance memory 
  ! and EFDC memory, if longer time series are passed than are currently 
  ! allocated
  ! Note: only de NDPSER dimension is considered
  ! --------------------------------------------------------------------------
  function enlarge_pser_time_series(id,size_n,size_m) result(ret_val)

    use global, only: NDPSER ,NPSERM, TPSER, &
         MPTLAST, TAPSER, TCPSER, & 
         MPSER, TPSER, PSER
    use global, only: PSERT_EFDC => PSERT

    implicit none

    ! result
    integer :: ret_val ! =0 succes; <0 error

    ! arguments
    integer, intent(in) :: id     ! model instance identifer
    integer, intent(in) :: size_n ! requested length of time series
    integer, intent(in) :: size_m ! number of locations

    ! locals
    type(pser_time_series):: psert_orig
    integer :: n, m
    integer :: new_n,new_m
    REAL,ALLOCATABLE,DIMENSION(:,:)::PSER_orig
    REAL,ALLOCATABLE,DIMENSION(:,:)::TPSER_orig

    ret_val = -1

    n = psert(id)%NDPSER
    m = psert(id)%NPSER

    new_m = size_m
    new_n = size_n

    if ((size_n > psert(id)%NDPSER)) then 
       if (debug) print*, "enlarge_pser_time_series", id, n, m 
       if (debug) print*, "enlarge_pser_time_series", id, size_n, size_m 

       allocate(psert_orig%MPSER(m))
       allocate(psert_orig%TPSER(n,m))    
       allocate(psert_orig%PSER(n,m))

       psert_orig%MPSER = psert(id)%MPSER  
       psert_orig%TPSER = psert(id)%TPSER  
       psert_orig%PSER  = psert(id)%PSER

       call model_pser_deallocate(id)
       call model_pser_allocate(id, new_n, new_m)

       psert(id)%MPSER(1:m) = psert_orig%MPSER(1:m)
       psert(id)%TPSER(1:n,1:m) = psert_orig%TPSER(1:n,1:m) 
       psert(id)%PSER(1:n,1:m) = psert_orig%PSER(1:n,1:m)

       deallocate(psert_orig%MPSER)
       deallocate(psert_orig%TPSER)    
       deallocate(psert_orig%PSER)

       if (psert(id)%NDPSER > ndpser_max) then
          if (debug) print*, 'reallocating PSER times series variables'
          allocate(PSER_orig(n,m))
          allocate(TPSER_orig(n,m))
          
          TPSER_orig = TPSER
          PSER_orig  = PSER
          deallocate(TPSER, PSER)

          ndpser_max = psert(id)%NDPSER
          npser_max = psert(id)%NPSER
          NDPSER = ndpser_max
          NPSERM = npser_max

          allocate(TPSER(NDPSER, NPSERM))
          allocate(PSER(NDPSER, NPSERM))
          
          TPSER(1:n,1:m) = TPSER_orig
          PSER(1:n,1:m)= PSER_orig
          deallocate(TPSER_orig, PSER_orig)
          

       end if
       ret_val = 0
    else 
       ret_val = 0   
    end if

  end function enlarge_pser_time_series

end module model_pser_time_series
