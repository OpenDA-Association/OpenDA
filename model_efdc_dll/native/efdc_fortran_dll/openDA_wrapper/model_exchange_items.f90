!! This library is free software; you can redistribute it and/or
!! modify it under the terms of the GNU Lesser General Public
!! License as published by the Free Software Foundation; either
!! version 2.1 of the License, or (at your option) any later version.

!! This library is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!! Lesser General Public License for more details.

!! You should have received a copy of the GNU Lesser General Public
!! License along with this library; if not, write to the Free Software
!! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 
!! @author Werner Kramer       VORtech BV  
!!                             P.O. Box 260
!!                             2600 AG Delft
!!                             The Netherlands
!!                             www.vortech.nl

module model_exchange_items
  
  implicit none

  ! atmospheric forcing exchange items
  integer, parameter :: Precipitation           = 101
  integer, parameter :: AirTemperature          = 102
  integer, parameter :: CloudCover              = 103
  integer, parameter :: GlobalRadiation         = 104
  integer, parameter :: AtmosphericPressure     = 105
  integer, parameter :: RelativeHumidity        = 106
  integer, parameter :: PotentialEvaporation    = 107
  
  ! wind forcing
  integer, parameter :: nrExchangeItemsWind     = 2
  integer, parameter :: indexWind               = 150  ! timeseries (151:152)
  integer, parameter :: WindSpeed               = 151
  integer, parameter :: WindDirection           = 152

  ! water level
  integer, parameter :: WaterLevel      = 201
  integer, parameter :: Grid_WaterLevel = 1201

  ! discharge
  integer, parameter :: Discharge      = 301
  integer, parameter :: Grid_Discharge = 1301

  ! water temperature
  integer, parameter :: WaterTemperature      =  401
  integer, parameter :: Grid_WaterTemperature = 1401

  ! water quality
  integer, parameter :: nrExchangeItemsWQ = 22
  integer, parameter :: indexWQ           = 500   ! timeseries (501:522)
  integer, parameter :: gridIndexWQ       = 1500  ! grid       (1501:1522)
  integer :: NC_wq_start   ! index in WQV array

  ! toxics
  integer, parameter :: nrExchangeItemsTOX = 16
  integer, parameter :: indexTOX           = 600  ! timeseries (601:616)
  integer, parameter :: gridindexTOX       = 1600 ! grid       (1601:1616)
  integer :: NC_tox_start  ! index in WQV array

  !controls
  integer, parameter :: nrExchangeItemsControl    = 2
  integer, parameter :: indexControl              = 700  ! timeseries (701:702)
  integer, parameter :: ControlsGateWaterLevel    = 701
  integer, parameter :: ControlsGateOpeningHeight = 702
  
  ! x-species
  integer, parameter :: nrMaxXspecies     =   99
  integer, parameter :: indexXspecies     =  800
  integer, parameter :: gridIndexXspecies = 1800
  integer :: NC_xspecies_start   ! index in WQV array
  
  type exchangeItem
      integer :: active = 0
      integer :: id = 0 
      character(len=32) :: name = '--undefined--'
      character(len=32) :: efdc_name = '--undefined--'
  end type

  ! exchange items for time series
  type(exchangeItem), dimension(nrExchangeItemsWind), private, save :: exchangeItemsWind
  type(exchangeItem), dimension(nrExchangeItemsWQ),  private, save :: exchangeItemsWQ
  type(exchangeItem), dimension(nrExchangeItemsTOX), private, save :: exchangeItemsTOX
  type(exchangeItem), dimension(nrExchangeItemsControl), private, save :: exchangeItemsControl
  type(exchangeItem), allocatable, private :: exchangeItemsXspecies(:)

  
  !exchangeItems for grid
  type(exchangeItem), dimension(nrExchangeItemsWQ),  private, save :: gridExchangeItemsWQ
  type(exchangeItem), dimension(nrExchangeItemsTOX), private, save :: gridExchangeItemsTOX
  type(exchangeItem), allocatable, private :: gridExchangeItemsXspecies(:)

contains

  function model_exchange_items_setup(general_log_handle) result(ret_val)
  ! --------------------------------------------------------------------------
  ! Create a list of exchange items and check if they are active 
  ! --------------------------------------------------------------------------

    use global, only: NWSER, NTOX, NWQV, WQTSNAME, TXID0, NQCTL, NQCTYP1, NXSP

    implicit none

    ! return value
    integer :: ret_val ! <0 : error

    ! arguments
    integer, intent(IN) :: general_log_handle

    ! local
    integer :: i
    character(len=32) ::xSpeciesName
    character(len=32) ::xSpeciesGridName
!    character(len=20) :: txname
!    character(len=11) :: txid
!    real    :: r
    ret_val = 0

    exchangeItemsWind(1)  = exchangeItem( id = 151, name = "WindSpeed", efdc_name = "WINDS")
    exchangeItemsWind(2)  = exchangeItem( id = 152, name = "WindDirection", efdc_name = "WINDD")
    ! water quality
    exchangeItemsWQ(1)  = exchangeItem( id = 501, name = 'AlgalCyanobacteria')
    exchangeItemsWQ(2)  = exchangeItem( id = 502, name = 'AlgalDiatom')
    exchangeItemsWQ(3)  = exchangeItem( id = 503, name = 'AlgalGreenAlgae')
    exchangeItemsWQ(4)  = exchangeItem( id = 504, name = 'RefractoryPOCarbon')
    exchangeItemsWQ(5)  = exchangeItem( id = 505, name = 'LabilePOCarbon')
    exchangeItemsWQ(6)  = exchangeItem( id = 506, name = 'DissolvedOCarbon')
    exchangeItemsWQ(7)  = exchangeItem( id = 507, name = 'RefractoryPOPhosphorus')
    exchangeItemsWQ(8)  = exchangeItem( id = 508, name = 'LabilePOPhosporus')
    exchangeItemsWQ(9)  = exchangeItem( id = 509, name = 'DissolvedOPhosphorus')
    exchangeItemsWQ(10) = exchangeItem( id = 510, name = 'Phosphate')
    exchangeItemsWQ(11) = exchangeItem( id = 511, name = 'RefractoryPONitrogen')
    exchangeItemsWQ(12) = exchangeItem( id = 512, name = 'LabilePONitrogen')
    exchangeItemsWQ(13) = exchangeItem( id = 513, name = 'DissolvedONitrogen')
    exchangeItemsWQ(14) = exchangeItem( id = 514, name = 'Ammonia')
    exchangeItemsWQ(15) = exchangeItem( id = 515, name = 'Nitrate')
    exchangeItemsWQ(19) = exchangeItem( id = 519, name = 'DisolvedOxygen')

    gridExchangeItemsWQ(1)  = exchangeItem( id = 1501, name = 'GRID.AlgalCyanobacteria')
    gridExchangeItemsWQ(2)  = exchangeItem( id = 1502, name = 'GRID.AlgalDiatom')
    gridExchangeItemsWQ(3)  = exchangeItem( id = 1503, name = 'GRID.AlgalGreenAlgae')
    gridExchangeItemsWQ(4)  = exchangeItem( id = 1504, name = 'GRID.RefractoryPOCarbon')
    gridExchangeItemsWQ(5)  = exchangeItem( id = 1505, name = 'GRID.LabilePOCarbon')
    gridExchangeItemsWQ(6)  = exchangeItem( id = 1506, name = 'GRID.DissolvedOCarbon')
    gridExchangeItemsWQ(7)  = exchangeItem( id = 1507, name = 'GRID.RefractoryPOPhosphorus')
    gridExchangeItemsWQ(8)  = exchangeItem( id = 1508, name = 'GRID.LabilePOPhosporus')
    gridExchangeItemsWQ(9)  = exchangeItem( id = 1509, name = 'GRID.DissolvedOPhosphorus')
    gridExchangeItemsWQ(10) = exchangeItem( id = 1510, name = 'GRID.Phosphate')
    gridExchangeItemsWQ(11) = exchangeItem( id = 1511, name = 'GRID.RefractoryPONitrogen')
    gridExchangeItemsWQ(12) = exchangeItem( id = 1512, name = 'GRID.LabilePONitrogen')
    gridExchangeItemsWQ(13) = exchangeItem( id = 1513, name = 'GRID.DissolvedONitrogen')
    gridExchangeItemsWQ(14) = exchangeItem( id = 1514, name = 'GRID.Ammonia')
    gridExchangeItemsWQ(15) = exchangeItem( id = 1515, name = 'GRID.Nitrate')
    gridExchangeItemsWQ(19) = exchangeItem( id = 1519, name = 'GRID.DisolvedOxygen')

    ! toxics
    exchangeItemsTOX(1)  = exchangeItem( id = 601, name = "Cadmium",         efdc_name = "1")
    exchangeItemsTOX(2)  = exchangeItem( id = 602, name = "Copper",          efdc_name = "2")
    exchangeItemsTOX(3)  = exchangeItem( id = 603, name = "Lead",            efdc_name = "3")
    exchangeItemsTOX(4)  = exchangeItem( id = 604, name = "Zinc",            efdc_name = "4")
    exchangeItemsTOX(5)  = exchangeItem( id = 605, name = "Benzopyrene",     efdc_name = "50-32-8")
    exchangeItemsTOX(6)  = exchangeItem( id = 606, name = "Pcb",             efdc_name = "6")
    exchangeItemsTOX(7)  = exchangeItem( id = 607, name = "Phenol",          efdc_name = "131-52-2")
    exchangeItemsTOX(8)  = exchangeItem( id = 608, name = "cisChlordane",    efdc_name = "5103-71-9")
    exchangeItemsTOX(9)  = exchangeItem( id = 609, name = "transChlordane",  efdc_name = "5103-74-2")
    exchangeItemsTOX(10) = exchangeItem( id = 610, name = "Chlorbenzen",     efdc_name =  "1003")
    exchangeItemsTOX(11) = exchangeItem( id = 611, name = "MethylParathion", efdc_name =  "1004")
    exchangeItemsTOX(12) = exchangeItem( id = 612, name = "PcbArchor1242",   efdc_name =  "1005")
    exchangeItemsTOX(13) = exchangeItem( id = 613, name = "Gasoline",        efdc_name = "4441")
    exchangeItemsTOX(14) = exchangeItem( id = 614, name = "Diesel",          efdc_name =  "4442")
    exchangeItemsTOX(15) = exchangeItem( id = 615, name = "BunkerC",         efdc_name = "4443")
    exchangeItemsTOX(16) = exchangeItem( id = 616, name = "UserDefined",     efdc_name = "4444")

    gridExchangeItemsTOX(1)  = exchangeItem( id = 1601, name = "GRID.Cadmium",         efdc_name = "1")
    gridExchangeItemsTOX(2)  = exchangeItem( id = 1602, name = "GRID.Copper",          efdc_name = "2")
    gridExchangeItemsTOX(3)  = exchangeItem( id = 1603, name = "GRID.Lead",            efdc_name = "3")
    gridExchangeItemsTOX(4)  = exchangeItem( id = 1604, name = "GRID.Zinc",            efdc_name = "4")
    gridExchangeItemsTOX(5)  = exchangeItem( id = 1605, name = "GRID.Benzopyrene",     efdc_name = "50-32-8")
    gridExchangeItemsTOX(6)  = exchangeItem( id = 1606, name = "GRID.Pcb",             efdc_name = "6")
    gridExchangeItemsTOX(7)  = exchangeItem( id = 1607, name = "GRID.Phenol",          efdc_name = "131-52-2")
    gridExchangeItemsTOX(8)  = exchangeItem( id = 1608, name = "GRID.cisChlordane",    efdc_name = "5103-71-9"  )
    gridExchangeItemsTOX(9)  = exchangeItem( id = 1609, name = "GRID.transChlordane",  efdc_name = "5103-74-2")
    gridExchangeItemsTOX(10) = exchangeItem( id = 1610, name = "GRID.Chlorbenzen",     efdc_name =  "1003")
    gridExchangeItemsTOX(11) = exchangeItem( id = 1611, name = "GRID.MethylParathion", efdc_name =  "1004")
    gridExchangeItemsTOX(12) = exchangeItem( id = 1612, name = "GRID.PcbArchor1242",   efdc_name =  "1005")
    gridExchangeItemsTOX(13) = exchangeItem( id = 1613, name = "GRID.Gasoline",        efdc_name = "4441")
    gridExchangeItemsTOX(14) = exchangeItem( id = 1614, name = "GRID.Diesel",          efdc_name =  "4442")
    gridExchangeItemsTOX(15) = exchangeItem( id = 1615, name = "GRID.BunkerC",         efdc_name = "4443")
    gridExchangeItemsTOX(16) = exchangeItem( id = 1616, name = "GRID.UserDefined",     efdc_name = "4444")

    exchangeItemsControl(1)  = exchangeItem( id = 701, name = "ControlsGateWaterLevel", efdc_name = "SEL1")
    exchangeItemsControl(2)  = exchangeItem( id = 702, name = "ControlsGateOpenHeight", efdc_name = "GUPH")
    
    allocate(exchangeItemsXspecies(NXSP))
    allocate(gridExchangeItemsXspecies(NXSP))
    do i = 1,NXSP
      write(xSpeciesName,'(A,I0)') "xSpecies",i
      write(xSpeciesGridName,'(A,I0)') "GRID.xSpecies",i
      exchangeItemsXspecies(i) = exchangeItem( id = 800+i, name = xSpeciesName)
      gridExchangeItemsXspecies(i) = exchangeItem( id = 1800+i, name = xSpeciesGridName)
      exchangeItemsXspecies(i)%active = 1 
      write(general_log_handle,'(A,A,A,I4,A,I1)' ) "exchangeItem: " ,exchangeItemsXspecies(i)%name , &
                                              " id: ", exchangeItemsXspecies(i)%id, &
                                              " active: ", exchangeItemsXspecies(i)%active
      gridExchangeItemsXspecies(i)%active = 1
      write(general_log_handle,'(A,A,A,I4,A,I1)' ) "exchangeItem: " ,gridExchangeItemsXspecies(i)%name , &
                                              " id: ", gridExchangeItemsXspecies(i)%id, &
                                              " active: ", gridExchangeItemsXspecies(i)%active
    end do
    
    do i = 1,nrExchangeItemsWind
      if (NWSER.GE.1) exchangeItemsWind(i)%active = 1 
      write(general_log_handle,'(A,A,A,I4,A,I1)' ) "exchangeItem: " ,exchangeItemsWind(i)%name , &
                                              " id: ", exchangeItemsWind(i)%id, &
                                              " active: ", exchangeItemsWind(i)%active
    end do
    
    write(general_log_handle,*) "number of water quality specified in EFDC: ", NWQV
    do i = 1, nrExchangeItemsWQ
      if (i .le. NWQV) then
        exchangeItemsWQ(i)%efdc_name = WQTSNAME(i)
        if (exchangeItemsWQ(i)%id .ne. 0 ) then
          exchangeItemsWQ(i)%active = 1
        end if
      end if 
      write(general_log_handle,'(A,A,A,I4,A,I1)' ) "exchangeItem: " ,exchangeItemsWQ(i)%name , &
                                              " id: ", exchangeItemsWQ(i)%id, &
                                              " active: ", exchangeItemsWQ(i)%active
      write(general_log_handle,'(A,A)' ) "efdc name: " ,exchangeItemsWQ(i)%efdc_name
      ! log matching 
    end do

    do i = 1, nrExchangeItemsWQ
      if (i .le. NWQV) then
        gridExchangeItemsWQ(i)%efdc_name = WQTSNAME(i)
        if (gridExchangeItemsWQ(i)%id .ne. 0 ) then
          gridExchangeItemsWQ(i)%active = 1
        end if
      end if 
      write(general_log_handle,'(A,A,A,I4,A,I1)' ) "exchangeItem: " ,gridExchangeItemsWQ(i)%name , &
                                              " id: ", gridExchangeItemsWQ(i)%id, &
                                              " active: ", gridExchangeItemsWQ(i)%active
      write(general_log_handle,'(A,A)' ) "efdc name: " ,gridExchangeItemsWQ(i)%efdc_name
      ! log matching 
    end do


    write(general_log_handle,*) "number of toxic species specified in EFDC: ", NTOX
    do i = 1,nrExchangeItemsTOX
      !if ( exchangeItemsTOX(i)%efdc_name .eq. TXID0 ) exchangeItemsTOX(i)%active = 1
      ! toxic forcings are not supported
      write(general_log_handle,'(A,A,A,I4,A,I1)' ) "exchangeItem: " ,exchangeItemsTOX(i)%name , &
                                              " id: ", exchangeItemsTOX(i)%id, &
                                              " active: ", exchangeItemsTOX(i)%active
      !write(general_log_handle,'(A,A)' ) "efdc name: " ,exchangeItemsTOX(i)%efdc_name
      ! log matching 

    end do

    do i = 1,nrExchangeItemsTOX
      if ( gridExchangeItemsTOX(i)%efdc_name .eq. TXID0 ) gridExchangeItemsTOX(i)%active = 1
      write(general_log_handle,'(A,A,A,I4,A,I1)' ) "exchangeItem: " ,gridExchangeItemsTOX(i)%name , &
                                              " id: ", gridExchangeItemsTOX(i)%id, &
                                              " active: ", gridExchangeItemsTOX(i)%active
      !write(general_log_handle,'(A,A)' ) "efdc name: " ,exchangeItemsTOX(i)%efdc_name
      ! log matching 

    end do
    
    do i = 1,nrExchangeItemsControl
      if (NQCTL.GE.1 .AND. NQCTYP1.GE.3) exchangeItemsControl(i)%active = 1 
      write(general_log_handle,'(A,A,A,I4,A,I1)' ) "exchangeItem: " ,exchangeItemsControl(i)%name , &
                                              " id: ", exchangeItemsControl(i)%id, &
                                              " active: ", exchangeItemsControl(i)%active
    end do
  
  end function model_exchange_items_setup
  
  function model_exchange_items_destroy() result(ret_val)
    implicit none
    ! return value
    integer :: ret_val
    
    ret_val = -1
    if (allocated(exchangeItemsXspecies)) deallocate(exchangeItemsXspecies)
    if (allocated(gridExchangeItemsXspecies)) deallocate(gridExchangeItemsXspecies)
    ret_val = 0
  end function model_exchange_items_destroy

  function model_exchange_items_supports( exchange_item_id ) result(ret_val)
    use global, only: NXSP
    implicit none

    ! return value
    integer :: ret_val

    !arguments 
    integer, intent(IN) :: exchange_item_id

    !local
    integer :: id

    ret_val = -1
    select case(exchange_item_id)
    case (indexWind+1:indexWind+nrExchangeItemsWind)
      id = exchange_item_id - indexWind
      ret_val = exchangeItemsWind(id)%active
    case (indexWQ+1:indexWQ+nrExchangeItemsWQ)
      id = exchange_item_id - indexWQ
      ret_val = exchangeItemsWQ(id)%active
    case (indexTOX+1:indexTOX+nrExchangeItemsTOX)
      id = exchange_item_id - indexTOX
      ret_val = exchangeItemsTOX(id)%active
    case (indexControl+1:indexControl+nrExchangeItemsControl)
      id = exchange_item_id - indexControl
      ret_val = exchangeItemsControl(id)%active
    case (indexXspecies+1:indexXspecies+nrMaxXspecies)
      if (exchange_item_id <= indexXspecies+NXSP) then
        id = exchange_item_id - indexXspecies
        ret_val = exchangeItemsXspecies(id)%active
      else
        ret_val = -2
      end if
    case (gridIndexWQ+1:gridIndexWQ+nrExchangeItemsWQ)
      id = exchange_item_id - gridIndexWQ
      ret_val = gridExchangeItemsWQ(id)%active
    case (gridindexTOX+1:gridindexTOX+nrExchangeItemsTOX)
      id = exchange_item_id - gridindexTOX
      ret_val = gridExchangeItemsTOX(id)%active
    case (gridIndexXspecies+1:gridIndexXspecies+nrMaxXspecies)
      if (exchange_item_id <= gridIndexXspecies+NXSP) then
        id = exchange_item_id - gridIndexXspecies
        ret_val = gridExchangeItemsXspecies(id)%active
      else
        ret_val = -2
      end if
    case default
      ret_val = -2
    end select

  end function model_exchange_items_supports


end module 
