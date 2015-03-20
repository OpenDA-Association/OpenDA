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
! Module for storing the QSER time series for each model instance.
! 
! Storage can be allocated, deallocated for a model instance and enlarged.  
! The time series can be copied to and from EFDC memory into instance memory
!
! This module also contains the identifiers for the exchange items used to set
! or get values for these QSER time series.   
! ----------------------------------------------------------------------------
module model_qser_time_series

  type qser_time_series

     ! time series length
     integer :: NDQSER, KCM, NQSER
     integer, allocatable, dimension(:) :: MQSER

     ! waterlevel
     real, allocatable, dimension(:,:) :: TQSER !  (NDQSER,NQSERM)
     real, allocatable, dimension(:,:,:) :: QSER  ! (NDQSER,NQSERM)

  end type qser_time_series

  type(qser_time_series), pointer, dimension(:) :: qsert 

  integer :: ndqser_max, nqser_max
  logical, private, parameter :: debug = .false.
  integer, private, parameter :: debug_file_handle = 666
  

contains

  ! --------------------------------------------------------------------------
  ! Subroutine for allocating arrays for given model instance.
  ! --------------------------------------------------------------------------
  subroutine model_qser_allocate(id, n, k, m)

    implicit none

    ! arguments
    integer, intent(in) :: id ! model instance identifier
    integer, intent(in) :: n  ! length of time series
    integer, intent(in) :: k  ! number of layers
    integer, intent(in) :: m  ! number of locations

    if (debug) write(debug_file_handle,*) "model_qser_allocate", n, k, m
    qsert(id)%NDQSER = n
    qsert(id)%NQSER = m
    qsert(id)%KCM = k

    allocate(qsert(id)%MQSER(m))
    allocate(qsert(id)%TQSER(n,m))    
    allocate(qsert(id)%QSER(n,k,m))

    qsert(id)%MQSER(m)    = n 
    qsert(id)%TQSER(n,m)  = 0.0
    qsert(id)%TQSER(n,:)  = 1.0e30   
    qsert(id)%QSER(n,k,m) = 0.0
  
  end subroutine model_qser_allocate

  ! --------------------------------------------------------------------------
  ! Subroutine for deallocating arrays for given model instance.
  ! --------------------------------------------------------------------------
  subroutine model_qser_deallocate(id)

    implicit none

    integer, intent(in) :: id ! model instance identifier

    if (debug) write(debug_file_handle,*) 'deallocate qser time series', allocated(qsert(id)%MQSER)
    deallocate(qsert(id)%MQSER)
    deallocate(qsert(id)%TQSER)
    deallocate(qsert(id)%QSER)
    qsert(id)%NDQSER=0
    qsert(id)%NQSER=0
    qsert(id)%KCM=0

  end subroutine model_qser_deallocate


  ! --------------------------------------------------------------------------
  ! Function for storing the current QSER time series in instance memory
  ! --------------------------------------------------------------------------
  function model_get_qser(id) result (ret_val)
    
    use global, only: QSER, MQSER, TQSER
    
    implicit none

    ! return argument
    integer :: ret_val        ! ret_val = 0: succes, ret_val = -1: error

    !arguments
    integer, intent(in) :: id ! model instance identifier 
    
    ret_val = -1
    if (debug) write(debug_file_handle,*) 'time_series is allocated ', allocated(qsert(id)%TQSER)  
    qsert(id)%MQSER = MQSER(1:qsert(id)%NQSER)  
    qsert(id)%TQSER = TQSER(1:qsert(id)%NDQSER,1:qsert(id)%NQSER)
    qsert(id)%QSER = QSER(1:qsert(id)%NDQSER,1:qsert(id)%KCM ,1:qsert(id)%NQSER)
    ret_val = 0

  end function model_get_qser


  ! --------------------------------------------------------------------------
  ! Function for setting the QSER time series in EFDC from instance memory
  ! --------------------------------------------------------------------------
  function model_set_qser(id) result(ret_val)

    use global, only: QSER, NDQSER, NQSERM, KCM, MQSER, TQSER 

    implicit none

    ! return value
    integer :: ret_val        ! =0 succes; <0 error

    ! arguments
    integer, intent(in) :: id ! model instance identifier

    ret_val = 0

    NDQSER = ndqser_max
    NQSERM = nqser_max

    if (ret_val == 0) then
       MQSER(1:qsert(id)%NQSER) = qsert(id)%MQSER
       TQSER(1:qsert(id)%NDQSER,1:qsert(id)%NQSER)=qsert(id)%TQSER
       QSER(1:qsert(id)%NDQSER,1:qsert(id)%KCM,1:qsert(id)%NQSER) = qsert(id)%QSER
       ret_val = 0
    end if

  end function model_set_qser

  ! --------------------------------------------------------------------------
  ! Function for enlarging (if required) the arrays in instance memory 
  ! and EFDC memory, if longer time series are passed than are currently 
  ! allocated 
  ! --------------------------------------------------------------------------
  function enlarge_qser_time_series(id,size_n,size_k,size_m) result(ret_val)

    use global, only: NDQSER ,NQSERM, KCM, &
          MQSER, TQSER, QSER, &
          MQTLAST, QSRTLPN, QSRTLPP, &
          TAGWSER, TAQSER, TCGWSER, TCQSER
    use global, only: QSERT_EFDC => QSERT 

    implicit none

    ! result
    integer :: ret_val              ! =0 succes; <0 error

    ! arguments
    integer, intent(in) :: id       ! time series to be enlarged
    integer, intent(in) :: size_n   ! new number of time points
    integer, intent(in) :: size_k   ! number layers
    integer, intent(in) :: size_m   ! number of locations

    !locals 
    type(qser_time_series) :: qsert_orig
    integer :: n, m, k
    integer :: new_n,new_m, new_k

    n = qsert(id)%NDQSER
    m = qsert(id)%NQSER 
    k = qsert(id)%KCM

    new_m = size_m
    new_n = size_n
    new_k = size_k

    ret_val = -1

    if ((size_n > qsert(id)%NDQSER)) then 
       if (debug) write(debug_file_handle,*) "enlarge_qser_time_series", size_n, size_k, size_m 

       allocate(qsert_orig%MQSER(m))
       allocate(qsert_orig%TQSER(n,m))    
       allocate(qsert_orig%QSER(n,k,m))

       qsert_orig%MQSER = qsert(id)%MQSER  
       qsert_orig%TQSER = qsert(id)%TQSER  
       qsert_orig%QSER  = qsert(id)%QSER

       call model_qser_deallocate(id)
       call model_qser_allocate(id, new_n, new_k, new_m)
       ! copy orig to new       
       qsert(id)%MQSER(1:m) = qsert_orig%MQSER(1:m)
       qsert(id)%TQSER(1:n,1:m) = qsert_orig%TQSER(1:n,1:m) 
       qsert(id)%QSER(1:n,1:k,1:m) = qsert_orig%QSER(1:n,1:k,1:m)

       deallocate(qsert_orig%MQSER)
       deallocate(qsert_orig%TQSER)    
       deallocate(qsert_orig%QSER)

       if (qsert(id)%NDQSER > ndqser_max ) then
          if (debug) write(debug_file_handle,*) 'reallocating QSER times series variables'

          deallocate(TQSER, QSER)

          ndqser_max = qsert(id)%NDQSER
          nqser_max = qsert(id)%NQSER
          NDQSER = ndqser_max
          NQSERM = nqser_max

          allocate(TQSER(NDQSER, NQSERM))
          allocate(QSER(NDQSER, KCM ,NQSERM))
         
       end if
       ret_val = 0
    else 
       ret_val = 0    
    end if

  end function enlarge_qser_time_series

end module model_qser_time_series
