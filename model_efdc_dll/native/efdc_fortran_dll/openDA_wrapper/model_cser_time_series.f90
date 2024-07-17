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
! Module for storing the CSER time series for each model instance.
! 
! Storage can be allocated, deallocated for a model instance and enlarged.  
! The time series can be copied to and from EFDC memory into instance memory
! The time series that are changed by exchange items needs to be activated 
! with model_cser_set_items. 
!
! This module also contains the identifiers for the exchange items used to set
! or get values for these CSER time series.   
! ----------------------------------------------------------------------------
module model_cser_time_series

  type cser_time_series

     ! time series length
     integer :: NDCSER, KCM, NSTVM, NCSERM
     !integer, allocatable, dimension(:) :: NCSER ! NSTVM 
     integer, allocatable, dimension(:,:) :: MCSER ! NCSERM, NSTVM

     ! waterlevel
     real, allocatable, dimension(:,:,:) :: TCSER !  (NDCSER,NCSERM,NSTVM)
     real, allocatable, dimension(:,:,:,:) :: CSER  ! (NDCSER,KCM,NCSERM,NSTVM)
 
  end type cser_time_series

  type(cser_time_series), pointer, dimension(:) :: csert 
  integer :: ndcser_max, ncser_max

  logical, private, parameter :: debug = .false.

contains

  ! --------------------------------------------------------------------------
  ! Subroutine for allocating arrays for given model instance.
  ! --------------------------------------------------------------------------
  subroutine model_cser_allocate(id, n, k, m)

    use global, only: NSTVM

    implicit none

    ! arguments
    integer, intent(in) :: id ! model instance identifier
    integer, intent(in) :: n  ! number of time points
    integer, intent(in) :: k  ! number of layers
    integer, intent(in) :: m  ! number of times series / locations

    if (debug) print*, "model_cser_allocate", n, k, m, NSTVM
    csert(id)%NDCSER = n
    csert(id)%NCSERM = m
    csert(id)%KCM = k
    csert(id)%NSTVM = NSTVM ! number of variables stored in CSER

    !allocate(csert(id)%NCSER(NSTVM)) 
    allocate(csert(id)%MCSER(m,NSTVM))
    allocate(csert(id)%TCSER(n,m,NSTVM))
    allocate(csert(id)%CSER(n,k,m,NSTVM))

    csert(id)%MCSER = n
    csert(id)%TCSER = 0.0 
    csert(id)%TCSER(2,:,:) = 730
    csert(id)%CSER  = 0.0

  end subroutine model_cser_allocate

  ! --------------------------------------------------------------------------
  ! Subroutine for deallocating CSER arrays for given model instance.
  ! --------------------------------------------------------------------------
  subroutine model_cser_deallocate(id)

    implicit none

    integer, intent(in) :: id

    if (debug) print*, 'deallocate cser time series', allocated(csert(id)%MCSER)
    deallocate(csert(id)%MCSER)
    deallocate(csert(id)%TCSER)
    deallocate(csert(id)%CSER)

    csert(id)%NDCSER=0
    csert(id)%NCSERM=0
    csert(id)%KCM=0
    csert(id)%NSTVM=0

  end subroutine model_cser_deallocate

  ! --------------------------------------------------------------------------
  ! Function for storing the current ASER time series in instance memory
  ! --------------------------------------------------------------------------
  function model_get_cser(id) result (ret_val)
    
    use global, only: CSER, MCSER, NCSER, TCSER 

    implicit none

    ! return argument
    integer :: ret_val

    !arguments
    integer, intent(in) :: id ! ret_val = 0: succes, ret_val = -1: error
    
    !local
    integer :: n              ! number of time points
    integer :: k              ! number of layers
    integer :: m              ! number of times series / locations

    ret_val = -1

    if (debug) print*, 'cser_time_series is allocated ', allocated(csert(id)%TCSER)  
    n = csert(id)%NDCSER
    m = csert(id)%NCSERM
    k = csert(id)%KCM
    csert(id)%MCSER(1:m,:) = MCSER(1:m,:)  
    csert(id)%TCSER(1:n,1:m,:) = TCSER(1:n,1:m,:) 
    csert(id)%CSER(1:n,1:k,1:m,:) =  CSER(1:n,1:k,1:m,:)
    ret_val = 0

  end function model_get_cser

  ! --------------------------------------------------------------------------
  ! Function for setting the CSER time series in EFDC from instance memory
  ! --------------------------------------------------------------------------
  function model_set_cser(id) result(ret_val)

    use global, only: NDCSER, NCSERM, KCM, NSTVM, &
         CSER, NCSER, MCSER, TCSER

    implicit none

    ! return value
    integer :: ret_val        ! ret_val = 0: success, ret_val = -1: error

    ! arguments
    integer, intent(in) :: id ! model instance identifier

    !local
    integer :: n, m, k

    NDCSER = ndcser_max 
    NCSERM = ncser_max 

    n = csert(id)%NDCSER
    m = csert(id)%NCSERM
    k = csert(id)%KCM

    !NCSER = csert(id)%NCSER 
    MCSER(1:m,:)        = csert(id)%MCSER(1:m,:)
    TCSER(1:n,1:m,:)    = csert(id)%TCSER(1:n,1:m,:)
    CSER(1:n,1:k,1:m,:) = csert(id)%CSER(1:n,1:k,1:m,:)

    ret_val = 0

  end function model_set_cser

  ! --------------------------------------------------------------------------
  ! Function for enlarging (if required) the arrays in instance memory 
  ! and EFDC memory, if longer time series are passed than are currently 
  ! allocated
  ! Note only the NDCSER dimension is considered
  ! --------------------------------------------------------------------------
  function enlarge_cser_time_series(id,size_n,size_k,size_m) result(ret_val)

    use global, only: NDCSER ,NCSERM, TCSER, NSTVM, KCM, MCSER, CSER, &
         MCTLAST, TACSER, TCCSER
    use global, only: CSERT_EFDC => CSERT

    implicit none

    ! result
    integer :: ret_val            ! =0 succes; <0 error

    ! arguments
    integer, intent(in) :: id     ! time series to be enlarged
    integer, intent(in) :: size_n ! requested length of time series
    integer, intent(in) :: size_k ! number of layers
    integer, intent(in) :: size_m ! number of time series/locations

    ! locals
    type(cser_time_series) :: csert_new
    REAL,ALLOCATABLE,DIMENSION(:,:,:)::TCSER_orig
    REAL,ALLOCATABLE,DIMENSION(:,:,:,:)::CSER_orig

    integer :: n, m, k
    integer :: new_n,new_m, new_k
    
    ret_val = -1

    n = csert(id)%NDCSER
    m = csert(id)%NCSERM
    k = csert(id)%KCM

    new_m = size_m
    new_n = size_n
    new_k = size_k
    
    if ((size_n > csert(id)%NDCSER)) then 

       !reallocate instance memory
       if (debug) print*, "enlarge_cser_time_series", id, n, m, k
       if (debug) print*, "enlarge_cser_time_series", id, size_n, size_m, size_k 

       allocate(csert_new%MCSER(new_m,NSTVM))
       allocate(csert_new%TCSER(new_n,new_m,NSTVM))
       allocate(csert_new%CSER(new_n,new_k,new_m,NSTVM))

       csert_new%MCSER(1:m,:) = csert(id)%MCSER  
       csert_new%TCSER(1:n,1:m,:) = csert(id)%TCSER  
       csert_new%CSER(1:n,1:k,1:m,:)  = csert(id)%CSER
       
       call move_alloc( to=csert(id)%MCSER, from=csert_new%MCSER )
       call move_alloc( to=csert(id)%TCSER, from=csert_new%TCSER )
       call move_alloc( to=csert(id)%CSER, from=csert_new%CSER )
       
       csert(id)%NDCSER = size_n
       csert(id)%NCSERM = size_m
       csert(id)%KCM = size_k
       csert(id)%NSTVM = NSTVM
       
       !csert_orig%MCSER = csert(id)%MCSER  
       !csert_orig%TCSER = csert(id)%TCSER  
       !csert_orig%CSER  = csert(id)%CSER
       
       !call model_cser_deallocate(id)
       !call model_cser_allocate(id, new_n, new_k, new_m)

       !csert(id)%MCSER(1:m,:) = csert_orig%MCSER  
       !csert(id)%TCSER(1:n,1:m,:) = csert_orig%TCSER  
       !csert(id)%CSER(1:n,1:k,1:m,:)  = csert_orig%CSER
       
       !deallocate(csert_orig%MCSER)
       !deallocate(csert_orig%TCSER)
       !deallocate(csert_orig%CSER)

       ! reallocate model memory       
       if (csert(id)%NDCSER > ndcser_max ) then
          if (debug) print*, 'reallocating CSER times series variables'

          allocate(CSER_orig(n,k,m,NSTVM))
          allocate(TCSER_orig(n,m,NSTVM))
          
          TCSER_orig = TCSER  
          CSER_orig  = CSER
          
          deallocate(TCSER, CSER)
          !deallocate(CSERT_EFDC, MCTLAST, TACSER, TCCSER)
          
          NDCSER = csert(id)%NDCSER
          NCSERM = csert(id)%NCSERM
          KCM = csert(id)%KCM
          NSTVM = csert(id)%NSTVM

          allocate(TCSER(NDCSER, NCSERM, NSTVM))
          allocate(CSER(NDCSER, KCM ,NCSERM, NSTVM))

          TCSER(1:n,1:m,:) = TCSER_orig
          CSER(1:n,1:k,1:m,:)= CSER_orig
                  
          deallocate(TCSER_orig, CSER_orig)
          
          !ALLOCATE(CSERT_EFDC(KCM,0:NCSERM,NSTVM))
          !ALLOCATE(MCTLAST(NCSERM,NSTVM))
          !ALLOCATE(TACSER(NCSERM,NSTVM))
          !ALLOCATE(TCCSER(NCSERM,NSTVM))

       end if
       ret_val = 0 
    else 
       ret_val = 0    
    end if

  end function enlarge_cser_time_series

end module model_cser_time_series
