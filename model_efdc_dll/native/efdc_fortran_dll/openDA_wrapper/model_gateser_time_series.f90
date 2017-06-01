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
! Module for storing the GATESER time series for each model instance.
! 
! Only EFDC variable SELL1 is available as an exchange item 
!
! Storage can be allocated, deallocated for a model instance and enlarged.  
! The time series can be copied to and from EFDC memory into instance memory 
! ----------------------------------------------------------------------------
module model_gateser_time_series
  
  type gateser_time_series

     ! time series length
     integer :: NDQCLT, NQCTLM
     integer, allocatable, dimension(:) :: MQCTL
     
     !gate controls
     real,    allocatable, dimension(:,:) :: GCSER       !(NDQCLT,NQCTLT)
     integer, allocatable, dimension(:,:) :: IAG, NGATE  ! (NDQCLT,NQCTLT)
     real, allocatable, dimension(:,:)    :: SEL1, SEL2, GUPH, GQSUM ! (NDQCLT,NQCTLT)
     real, allocatable, dimension(:,:,:)    :: GKMUL !(NDQCLT,KCM,NQCTLT)
     real, allocatable, dimension(:) :: HUPG, HDWG !(NGTYPEM)
     integer, allocatable, dimension(:) :: NGCOUNT,NGCHECK, NGCCHECK ! NGTYPEM
     
  end type gateser_time_series

  ! maximum size of time series
  integer :: NDQCLT_max, NQCTLM_max
  type(gateser_time_series), pointer, dimension(:) :: gateser => NULL()
  logical, private, parameter :: debug = .false.

contains

  ! --------------------------------------------------------------------------
  ! Subroutine for allocating arrays for given model instance.
  ! --------------------------------------------------------------------------
  subroutine model_gateser_allocate(id, n, m)

    use global, only: KCM, NGTYPEM
  
    implicit none

    ! arguments
    integer, intent(in) :: id ! model instance identifier
    integer, intent(in) :: n  ! number of data points
    integer, intent(in) :: m  ! number of locations 

    if (debug) print*, "model_gateser_allocate", n, m
    gateser(id)%NDQCLT = n
    gateser(id)%NQCTLM = m

    allocate(gateser(id)%MQCTL(m))
    allocate(gateser(id)%GCSER(n,m))    
    allocate(gateser(id)%IAG(n,m))
    allocate(gateser(id)%NGATE(n,m))
    allocate(gateser(id)%SEL1(n,m))
    allocate(gateser(id)%SEL2(n,m))
    allocate(gateser(id)%GUPH(n,m))
    allocate(gateser(id)%GQSUM(n,m))
    allocate(gateser(id)%GKMUL(n,KCM,m))
    allocate(gateser(id)%HUPG(NGTYPEM))
    allocate(gateser(id)%HDWG(NGTYPEM))
    allocate(gateser(id)%NGCHECK(NGTYPEM))
    allocate(gateser(id)%NGCCHECK(NGTYPEM))
    allocate(gateser(id)%NGCOUNT(NGTYPEM))
    
    if (debug) print*, "model_gateser_allocate", allocated(gateser(id)%MQCTL)

    gateser(id)%MQCTL = n
    gateser(id)%GCSER = 0.0
    gateser(id)%GCSER(n,:) = 1.0e30
    gateser(id)%SEL1  = 0.0    
    gateser(id)%IAG   = 0
    gateser(id)%NGATE = 0
    gateser(id)%SEL2  = 0.0
    gateser(id)%GUPH  = 0.0
    gateser(id)%GQSUM = 0.0
    gateser(id)%GKMUL = 0.0
    gateser(id)%HUPG = 0.0
    gateser(id)%HDWG = 0.0
    gateser(id)%NGCOUNT = 0
    gateser(id)%NGCHECK = 0
    gateser(id)%NGCCHECK = 0

    
  end subroutine model_gateser_allocate

  ! --------------------------------------------------------------------------
  ! Subroutine for deallocating ASER arrays for given model instance.
  ! --------------------------------------------------------------------------
  subroutine model_gateser_deallocate(id)

    implicit none

    ! arguments
    integer, intent(in) :: id ! model instance identifier

    !local
    integer :: ALLOC_ERR

    if (debug) print*, 'deallocate gateser time series', id, allocated(gateser(id)%GCSER)

    deallocate(gateser(id)%MQCTL, STAT = ALLOC_ERR)
    deallocate(gateser(id)%GCSER)
    deallocate(gateser(id)%IAG)
    deallocate(gateser(id)%NGATE)
    deallocate(gateser(id)%SEL1)
    deallocate(gateser(id)%SEL2)
    deallocate(gateser(id)%GUPH)
    deallocate(gateser(id)%GQSUM)
    deallocate(gateser(id)%GKMUL)
    deallocate(gateser(id)%HUPG)
    deallocate(gateser(id)%HDWG)
    deallocate(gateser(id)%NGCHECK)
    deallocate(gateser(id)%NGCCHECK)
    deallocate(gateser(id)%NGCOUNT)
    
    
    gateser(id)%NDQCLT=0
    gateser(id)%NQCTLM=0
    

  end subroutine model_gateser_deallocate

  ! --------------------------------------------------------------------------
  ! Function for storing the current ASER time series in instance memory
  ! --------------------------------------------------------------------------
  function model_get_gateser(id) result (ret_val)
    
    use global, only: GCSER, IAG, NGATE, SEL1, SEL2, GUPH, GQSUM, GKMUL, MQCTL, KCM, &
                      HUPG, HDWG, NGCCHECK, NGCOUNT, NGCHECK
    
    
    implicit none

    ! return argument
    integer :: ret_val        ! ret_val = 0: succes, ret_val = -1: error

    !arguments
    integer, intent(in) :: id ! model instance identifier

    !local
    integer :: n,m

    ret_val = -1
    if (debug) then 
       print*, 'get_gateser: time_series is allocated ', allocated(gateser(id)%GCSER)
       print*, 'size: ', id, gateser(id)%NDQCLT, gateser(id)%NQCTLM
       print*, 'max size ',  NDQCLT_max, NQCTLM_max
    end if
    n = gateser(id)%NDQCLT
    m = gateser(id)%NQCTLM
    gateser(id)%MQCTL(1:m)     = MQCTL(1:m)
    gateser(id)%GCSER(1:n,1:m) = GCSER(1:n,1:m) 
    gateser(id)%IAG(1:n,1:m)   = IAG(1:n,1:m)
    gateser(id)%NGATE(1:n,1:m) = NGATE(1:n,1:m)
    gateser(id)%SEL1(1:n,1:m)  = SEL1(1:n,1:m)
    gateser(id)%SEL2(1:n,1:m)  = SEL2(1:n,1:m)
    gateser(id)%GUPH(1:n,1:m)  = GUPH(1:n,1:m)
    gateser(id)%GQSUM(1:n,1:m) = GQSUM(1:n,1:m)
    gateser(id)%GKMUL(1:n,1:KCM,1:m) = GKMUL(1:n,1:KCM,1:m)
    gateser(id)%HUPG(:) = HUPG(:)
    gateser(id)%HDWG(:) = HDWG(:)
    gateser(id)%NGCHECK(:) = NGCHECK(:)
    gateser(id)%NGCCHECK(:) = NGCCHECK(:)
    gateser(id)%NGCOUNT(:) = NGCOUNT(:)  
    
    
    ret_val = 0

  end function model_get_gateser

  ! --------------------------------------------------------------------------
  ! Function for setting the ASER time series in EFDC from instance memory
  ! --------------------------------------------------------------------------
  function model_set_gateser(id) result(ret_val)
    
    use global, only: GCSER, IAG, NGATE, SEL1, SEL2, GUPH, GQSUM, GKMUL, &
         NDQCLT, NQCTLM, MQCTL, KCM, &
         HUPG, HDWG, NGCCHECK, NGCOUNT, NGCHECK
    
    implicit none

    ! return value
    integer :: ret_val        ! ret_val = 0: success, ret_val = -1: error

    ! arguments
    integer, intent(in) :: id ! model instance identifier

    ! local
    integer :: n, m

    ret_val = -1
    if (debug) print*, "model_set_gateser"

    NDQCLT = NDQCLT_max
    NQCTLM = NQCTLM_max

    n = gateser(id)%NDQCLT
    m = gateser(id)%NQCTLM
    MQCTL(1:m)     = gateser(id)%MQCTL(1:m)
    GCSER(1:n,1:m) = gateser(id)%GCSER(1:n,1:m)
    IAG(1:n,1:m)   = gateser(id)%IAG(1:n,1:m)
    NGATE(1:n,1:m) = gateser(id)%NGATE(1:n,1:m)
    SEL1(1:n,1:m)  = gateser(id)%SEL1(1:n,1:m)
    SEL2(1:n,1:m)  = gateser(id)%SEL2(1:n,1:m)
    GUPH(1:n,1:m)  = gateser(id)%GUPH(1:n,1:m)
    GQSUM(1:n,1:m) = gateser(id)%GQSUM(1:n,1:m)
    GKMUL(1:n,1:KCM,1:m) = gateser(id)%GKMUL(1:n,1:KCM,1:m)
    ! restore time averaged elevation levels
    HUPG(:) = gateser(id)%HUPG(:)
    HDWG(:) = gateser(id)%HDWG(:)
    ! restore gate control checks
    NGCHECK(:) = gateser(id)%NGCHECK(:)
    NGCCHECK(:) = gateser(id)%NGCCHECK(:)
    NGCOUNT(:) = gateser(id)%NGCOUNT(:)
    
    ret_val = 0
    
  end function model_set_gateser

  
  ! --------------------------------------------------------------------------
  ! Function for enlarging (if required) the arrays in instance memory 
  ! and EFDC memory, if longer time series are passed than are currently 
  ! allocated 
  ! --------------------------------------------------------------------------
  function enlarge_gateser_time_series(id,size_n,size_m) result(ret_val)

    use global, only: NDQCLT ,NDQCLT2, NQCTLM, NQCTTM, KCM, &
         NGATE, GCSER, IAG, NGATE, &
         SEL1, SEL2, GUPH, GQSUM, GKMUL, &
         QCTL, HDIFCTL, HDIFCTD
      
    implicit none

    ! result
    integer :: ret_val            ! ret_val = 0: success, ret_val =-1:  error

    ! arguments
    integer, intent(in) :: id     ! model instance identifier enlarged
    integer, intent(in) :: size_n ! requested time series length
    integer, intent(in) :: size_m ! number of locations

    ! locals
    type(gateser_time_series) :: gateser_orig
    integer :: n, m
    integer :: new_n,new_m, index

    real, dimension(2, NQCTLM) :: sel2_orig, &
        guph_orig, gqsum_orig
    integer, dimension(2, NQCTLM) :: iag_orig, ngate_orig
    integer, dimension(2,KCM, NQCTLM) :: gkmul_orig

    ret_val = -1

    n = gateser(id)%NDQCLT
    m = gateser(id)%NQCTLM 
    
    new_m = size_m
    new_n = size_n

    if ((size_n > gateser(id)%NDQCLT)) then 
       if (debug) print*, "enlarge_gateser_time_series", id, n, m 
       if (debug) print*, "enlarge_gateser_time_series", id, size_n, size_m 

       allocate(gateser_orig%MQCTL(m))
       allocate(gateser_orig%GCSER(n,m))    
       allocate(gateser_orig%IAG(n,m))
       allocate(gateser_orig%NGATE(n,m))
       allocate(gateser_orig%SEL1(n,m))
       allocate(gateser_orig%SEL2(n,m))
       allocate(gateser_orig%GUPH(n,m))
       allocate(gateser_orig%GQSUM(n,m))
       allocate(gateser_orig%GKMUL(n,KCM,m))
       
       gateser_orig%MQCTL = gateser(id)%MQCTL  
       gateser_orig%GCSER = gateser(id)%GCSER  
       gateser_orig%IAG  = gateser(id)%IAG
       gateser_orig%NGATE  = gateser(id)%NGATE
       gateser_orig%SEL1  = gateser(id)%SEL1
       gateser_orig%SEL2  = gateser(id)%SEL2
       gateser_orig%GUPH = gateser(id)%GUPH
       gateser_orig%GQSUM= gateser(id)%GQSUM 
       gateser_orig%GKMUL  = gateser(id)%GKMUL

       call model_gateser_deallocate(id)
       call model_gateser_allocate(id, new_n, new_m)
       ! copy orig to new       
       gateser(id)%MQCTL(1:m) = gateser_orig%MQCTL(1:m)
       gateser(id)%SEL1(1:n,1:m) = gateser_orig%SEL1(1:n,1:m)
       
       ! assume time independent use second value
       ! NGATE = 0 at t = 1. and  NGATE = 1 at t=1.01 ? 
       do index = 1, new_n
         gateser(id)%GCSER(index,1:m) = gateser_orig%GCSER(2,1:m) 
         gateser(id)%IAG(index,1:m) = gateser_orig%IAG(2,1:m)
         gateser(id)%NGATE(index,1:m) = gateser_orig%NGATE(2,1:m)
         gateser(id)%SEL2(index,1:m) = gateser_orig%SEL2(2,1:m)
         gateser(id)%GUPH(index,1:m) = gateser_orig%GUPH(2,1:m)
         gateser(id)%GQSUM(index,1:m) = gateser_orig%GQSUM(2,1:m)
         gateser(id)%GKMUL(index,1:KCM,1:m) = gateser_orig%GKMUL(2,1:KCM,1:m)
       end do
       
       deallocate(gateser_orig%MQCTL)
       deallocate(gateser_orig%GCSER)    
       deallocate(gateser_orig%IAG)
       deallocate(gateser_orig%NGATE)
       deallocate(gateser_orig%SEL1)
       deallocate(gateser_orig%SEL2)
       deallocate(gateser_orig%GUPH)
       deallocate(gateser_orig%GQSUM)
       deallocate(gateser_orig%GKMUL)

       if ( (gateser(id)%NDQCLT > NDQCLT_max ) & 
            .or. ( gateser(id)%NQCTLM  > NQCTLM_max )  ) then 
          ! enlarge EFDC arrays 
          iag_orig(1:2,:)   = IAG(1:2,:) 
          ngate_orig(1:2,:) = NGATE(1:2,:) 
          sel2_orig(1:2,:)  = SEL2(1:2,:) 
          gqsum_orig(1:2,:) = GQSUM(1:2,:) 
          guph_orig(1:2,:)  = GUPH(1:2,:) 
          gkmul_orig(1:2,:,:) = GKMUL(1:2,:,:) 
          
          deallocate(GCSER, IAG, NGATE, SEL1, SEL2, GQSUM, GUPH, GKMUL)
          deallocate(HDIFCTD, HDIFCTL, QCTL) !?

          NDQCLT_max = gateser(id)%NDQCLT
          NQCTLM_max = gateser(id)%NQCTLM
          NDQCLT = NDQCLT_max
          NQCTLM = NQCTLM_max

          allocate(GCSER(NDQCLT,NQCTLM)) ! GEOSR JGCHO 2011.10.27
          allocate(IAG(NDQCLT,NQCTLM)) ! GEOSR JGCHO 2011.10.27
          allocate(NGATE(NDQCLT,NQCTLM)) ! GEOSR JGCHO 2011.10.27
          allocate(SEL1(NDQCLT,NQCTLM)) ! GEOSR JGCHO 2011.10.27
          allocate(SEL2(NDQCLT,NQCTLM)) ! GEOSR JGCHO 2011.10.27
          allocate(GQSUM(NDQCLT,NQCTLM)) ! GEOSR JGCHO 2011.10.27
          allocate(GUPH(NDQCLT,NQCTLM)) ! GEOSR JGCHO 2011.10.27
          allocate(GKMUL(NDQCLT,KCM,NQCTLM)) ! GEOSR JGCHO 2011.10.27
      
          IAG(1,:)   = iag_orig(1,:)   
          NGATE(1,:) = ngate_orig(1,:) 
          SEL2(1,:)  = sel2_orig(1,:)  
          GQSUM(1,:) = gqsum_orig(1,:) 
          GUPH(1,:)  = guph_orig(1,:)  
          GKMUL(1,:,:) = gkmul_orig(1,:,:) 

          do index = 2, new_n
          
          IAG(index,:)   = iag_orig(2,:)   
          NGATE(index,:) = ngate_orig(2,:) 
          SEL2(index,:)  = sel2_orig(2,:)  
          GQSUM(index,:) = gqsum_orig(2,:) 
          GUPH(index,:)  = guph_orig(2,:)  
          GKMUL(index,:,:) = gkmul_orig(2,:,:) 
          end do
          ! other arrays with this size
          allocate(HDIFCTD(NDQCLT,NQCTTM))               ! nodig?
          allocate(HDIFCTL(NDQCLT,NQCTTM))               ! nodig?
          allocate(QCTL(NDQCLT,NDQCLT2,KCM,NQCTTM))      ! nodig?
          
       end if
       ret_val = 0
    else 
       ret_val = 0    
    end if

  end function enlarge_gateser_time_series

end module model_gateser_time_series
