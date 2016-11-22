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

module model_state


  type state_vector

     ! times
     real(kind=8) :: current_time, start_time, end_time
     real :: TBEGIN, TIMESEC
     ! time series length
     integer :: NDASER, NASERM

     ! external mode
     real, allocatable, dimension(:) :: HP, H1P ! (LCM)
     real, allocatable, dimension(:) :: HWQ, H2WQ ! (LCM)
     real, allocatable, dimension(:) :: BELV ! (LCM)
     ! external (barotropic) mode
     real, allocatable, dimension(:) :: UHDYE, UHDY1E ! (LCM)
     real, allocatable, dimension(:) :: VHDXE, VHDX1E ! (LCM)
     ! horizontal velocity fields
     real, allocatable, dimension(:,:) :: U, U1 ! (LCM,KCM)
     real, allocatable, dimension(:,:) :: V, V1 ! (LCM,KCM)
     ! turbulence intensities
     real, allocatable, dimension(:,:) :: QQ, QQ1 ! (LCM,0:KCM)
     real, allocatable, dimension(:,:) :: QQL, QQL1 ! (LCM,0:KCM)
     real, allocatable, dimension(:,:) :: DML ! (LCM,0:KCM)
     ! salinity
     real, allocatable, dimension(:,:) :: SAL, SAL1 ! (LCM,KCM)
     ! temperature
     real, allocatable, dimension(:,:) :: TEM, TEM1 ! (LCM,KCM)
     ! passive tracer?
     real, allocatable, dimension(:,:) :: DYE, DYE1 !  (LCM,KCM)

     ! shell fish larvae
     real, allocatable, dimension(:) ::  SFLSBOT ! (LCM)
     real, allocatable, dimension(:,:) :: SFL, SFL2 ! (LCM,KCM)
     !
     real, allocatable, dimension(:,:,:) :: TOX, TOX1 !  (LCM,KCM,NTXM)
     real, allocatable, dimension(:,:,:) :: TOXB, TOXB1 ! (LCM,KBM,NTXM)
     ! sediment
     real, allocatable, dimension(:,:,:) :: SED, SED1 ! (LCM,KCM,NSCM)
     real, allocatable, dimension(:,:,:) :: SEDB, SEDB1 ! (LCM,KBM,NSCM)
     !
     real, allocatable, dimension(:,:,:) :: SND, SND1 ! (LCM,KCM,NSNM)
     real, allocatable, dimension(:,:,:) :: SNDB, SNDB1 ! (LCM,KBM,NSNM)
     ! thickness of bed layers?
     real, allocatable, dimension(:,:) :: HBED, HBED1 ! (LCM,KBM)
     real, allocatable, dimension(:,:) :: VDRBED, VDRBED1 !(LCM,KBM)

     !boundary conditions (open?)
     !
     real, allocatable, dimension(:,:,:) :: NLOS, CLOS !(NBBSM,KCM,NSTVM)
     real, allocatable, dimension(:,:,:) :: NLOW, CLOW !(NBBWM,KCM,NSTVM)
     real, allocatable, dimension(:,:,:) :: NLOE, CLOE !(NBBEM,KCM,NSTVM)
     real, allocatable, dimension(:,:,:) :: NLON, CLON !(NBBNM,KCM,NSTVM)

     !
     real, allocatable, dimension(:) :: QSUME !(LCM)
     real, allocatable, dimension(:,:) :: QSUM !(LCM,KCM)

     ! subgrid channels
     real, allocatable, dimension(:) :: QCHANU, QCHANV ! (NCHANM)
     real, allocatable, dimension(:) :: AGWELV, AGWELV1 ! (LCM)

     ! bed temperature
     real, allocatable, dimension(:) :: TEMB ! (LCM)

     ! drying and wetting
     integer, allocatable, dimension(:) :: ISCDRY, IMASKDRY ! (LCM)
     real, allocatable, dimension(:) :: NATDRY ! (LCM)


     real, allocatable, dimension(:) :: SUB, SUBO !(0:LCM)
     real, allocatable, dimension(:) :: SVB, SVBO !(0:LCM)

     !water quality
     real,allocatable,dimension(:,:,:) :: WQV !(LCMWQ,KCM,0:NWQVM)
     real,allocatable,dimension(:,:,:) :: WQVX !(LCMWQ,KCM,0:NXSP)

  end type state_vector

  type(state_vector), pointer, dimension(:) :: state => NULL()

  logical, parameter, private :: debug = .false.

contains

  ! --------------------------------------------------------------------------
  ! allocate arrays for instance
  ! --------------------------------------------------------------------------
  subroutine model_state_allocate(id)


    use global, only : DT, NTS, LCM, KCM, NTXM, NSCM, NSNM, KBM, NCHANM, &
        LCMWQ, NSTVM, NWQVM, NXSP, &
        NBBSM, NBBWM, NBBEM, NBBNM

    implicit none

    ! arguments
    integer, intent(in) :: id  ! identifier of instance

    if (debug) print *, 'allocate state', NTS * DT

    ! external mode
    allocate(state(id)%HP(LCM))
    allocate(state(id)%H1P(LCM))
    allocate(state(id)%HWQ(LCM))
    allocate(state(id)%H2WQ(LCM))
    allocate(state(id)%BELV(LCM))

    ! external mode
    allocate(state(id)%UHDYE(LCM))
    allocate(state(id)%UHDY1E(LCM))
    allocate(state(id)%VHDXE(LCM))
    allocate(state(id)%VHDX1E(LCM))

    ! horizontal velocities
    allocate(state(id)%U(LCM,KCM))
    allocate(state(id)%U1(LCM,KCM))
    allocate(state(id)%V(LCM,KCM))
    allocate(state(id)%V1(LCM,KCM))

    ! turbulence intensities
    allocate(state(id)%QQ(LCM,0:KCM))
    allocate(state(id)%QQ1(LCM,0:KCM))
    allocate(state(id)%QQL(LCM,0:KCM))
    allocate(state(id)%QQL1(LCM,0:KCM))
    allocate(state(id)%DML(LCM,0:KCM))

    ! salinity, temperature & passive tracer concentration
    allocate(state(id)%SAL(LCM,KCM))
    allocate(state(id)%SAL1(LCM,KCM))
    allocate(state(id)%TEM(LCM,KCM))
    allocate(state(id)%TEM1(LCM,KCM))
    allocate(state(id)%DYE(LCM,KCM))
    allocate(state(id)%DYE1(LCM,KCM))

    ! shell fish larvae
    allocate(state(id)%SFLSBOT(LCM))
    allocate(state(id)%SFL(LCM,KCM))
    allocate(state(id)%SFL2(LCM,KCM))

    allocate(state(id)%TOX(LCM,KCM,NTXM))
    allocate(state(id)%TOX1(LCM,KCM,NTXM))
    allocate(state(id)%TOXB(LCM,KBM,NTXM))
    allocate(state(id)%TOXB1(LCM,KBM,NTXM))

    allocate(state(id)%SED(LCM,KCM,NSCM))
    allocate(state(id)%SED1(LCM,KCM,NSCM))
    allocate(state(id)%SEDB(LCM,KBM,NSCM))
    allocate(state(id)%SEDB1(LCM,KBM,NSCM))

    allocate(state(id)%SND(LCM,KCM,NSNM))
    allocate(state(id)%SND1(LCM,KCM,NSNM))
    allocate(state(id)%SNDB(LCM,KBM,NSNM))
    allocate(state(id)%SNDB1(LCM,KBM,NSNM))

    allocate(state(id)%HBED(LCM,KBM))
    allocate(state(id)%HBED1(LCM,KBM))
    allocate(state(id)%VDRBED(LCM,KBM))
    allocate(state(id)%VDRBED1(LCM,KBM))

    ALLOCATE(STATE(ID)%NLOS(NBBSM,KCM,NSTVM))
    ALLOCATE(STATE(ID)%CLOS(NBBSM,KCM,NSTVM))
    ALLOCATE(STATE(ID)%NLOW(NBBWM,KCM,NSTVM))
    ALLOCATE(STATE(ID)%CLOW(NBBWM,KCM,NSTVM))
    ALLOCATE(STATE(ID)%NLOE(NBBEM,KCM,NSTVM))
    ALLOCATE(STATE(ID)%CLOE(NBBEM,KCM,NSTVM))
    ALLOCATE(STATE(ID)%NLON(NBBNM,KCM,NSTVM))
    ALLOCATE(STATE(ID)%CLON(NBBNM,KCM,NSTVM))
    ALLOCATE(STATE(ID)%QSUME(LCM))
    ALLOCATE(STATE(ID)%QSUM(LCM,KCM))
    ALLOCATE(STATE(ID)%QCHANU(NCHANM))
    ALLOCATE(STATE(ID)%QCHANV(NCHANM))
    ALLOCATE(STATE(ID)%AGWELV(LCM))
    ALLOCATE(STATE(ID)%AGWELV1(LCM))
    ALLOCATE(STATE(ID)%TEMB(LCM))
    ALLOCATE(STATE(ID)%ISCDRY(LCM))
    ALLOCATE(STATE(ID)%IMASKDRY(LCM))
    ALLOCATE(STATE(ID)%NATDRY(LCM))
    ALLOCATE(STATE(ID)%SUB(0:LCM))
    ALLOCATE(STATE(ID)%SUBO(0:LCM))
    ALLOCATE(STATE(ID)%SVB(0:LCM))
    ALLOCATE(STATE(ID)%SVBO(0:LCM))
    ALLOCATE(STATE(ID)%WQV(LCMWQ,KCM,0:NWQVM))
    IF (NXSP>0) ALLOCATE(STATE(ID)%WQVX(LCMWQ,KCM,0:NXSP))

    state(id)%HP  = 0.0
    state(id)%H1P = 0.0
    state(id)%HWQ = 0.0
    state(id)%H2WQ= 0.0
    state(id)%BELV= 0.0

    ! external mode
    state(id)%UHDYE = 0.0
    state(id)%UHDY1E= 0.0
    state(id)%VHDXE = 0.0
    state(id)%VHDX1E= 0.0

    ! horizontal velocities
    state(id)%U = 0.0
    state(id)%U1= 0.0
    state(id)%V = 0.0
    state(id)%V1= 0.0

    ! turbulence intensities
    state(id)%QQ  = 0.0
    state(id)%QQ1 = 0.0
    state(id)%QQL = 0.0
    state(id)%QQL1= 0.0
    state(id)%DML = 0.0

    ! salinity, temperature & passive tracer concentration
    state(id)%SAL = 0.0
    state(id)%SAL1= 0.0
    state(id)%TEM = 0.0
    state(id)%TEM1= 0.0
    state(id)%DYE = 0.0
    state(id)%DYE1= 0.0

    ! shell fish larvae
    state(id)%SFLSBOT= 0.0
    state(id)%SFL    = 0.0
    state(id)%SFL2   = 0.0

    state(id)%TOX  = 0.0
    state(id)%TOX1 = 0.0
    state(id)%TOXB = 0.0
    state(id)%TOXB1= 0.0

    state(id)%SED  = 0.0
    state(id)%SED1 = 0.0
    state(id)%SEDB = 0.0
    state(id)%SEDB1= 0.0

    state(id)%SND  = 0.0
    state(id)%SND1 = 0.0
    state(id)%SNDB = 0.0
    state(id)%SNDB1= 0.0

    state(id)%HBED   = 0.0
    state(id)%HBED1  = 0.0
    state(id)%VDRBED = 0.0
    state(id)%VDRBED1= 0.0

    STATE(ID)%NLOS= 0.0
    STATE(ID)%CLOS= 0.0
    STATE(ID)%NLOW= 0.0
    STATE(ID)%CLOW= 0.0
    STATE(ID)%NLOE= 0.0
    STATE(ID)%CLOE= 0.0
    STATE(ID)%NLON= 0.0
    STATE(ID)%CLON= 0.0
    STATE(ID)%QSUME= 0.0
    STATE(ID)%QSUM= 0.0
    STATE(ID)%QCHANU= 0.0
    STATE(ID)%QCHANV= 0.0
    STATE(ID)%AGWELV= 0.0
    STATE(ID)%AGWELV1= 0.0
    STATE(ID)%TEMB= 0.0
    STATE(ID)%ISCDRY= 0.0
    STATE(ID)%IMASKDRY= 0.0
    STATE(ID)%NATDRY= 0.0
    STATE(ID)%SUB= 0.0
    STATE(ID)%SUBO= 0.0
    STATE(ID)%SVB= 0.0
    STATE(ID)%SVBO = 0.0
    STATE(ID)%WQV  = 0.0
    if (NXSP>0) STATE(ID)%WQVX  = 0.0

  end subroutine model_state_allocate

  ! --------------------------------------------------------------------------
  ! deallocate arrays for model instance
  ! --------------------------------------------------------------------------
  subroutine model_state_deallocate(id)
    use GLOBAL, only: NXSP
    implicit none

    integer, intent(in) :: id ! instance identifier

    if (debug) print*, 'deallocate state(id)'

    ! external mode
    deallocate(state(id)%HP)
    deallocate(state(id)%H1P)
    deallocate(state(id)%HWQ)
    deallocate(state(id)%H2WQ)
    deallocate(state(id)%BELV)

    ! external mode
    deallocate(state(id)%UHDYE)
    deallocate(state(id)%UHDY1E)
    deallocate(state(id)%VHDXE)
    deallocate(state(id)%VHDX1E)

    ! horizontal velocities
    deallocate(state(id)%U)
    deallocate(state(id)%U1)
    deallocate(state(id)%V)
    deallocate(state(id)%V1)

    ! turbulence intensities
    deallocate(state(id)%QQ)
    deallocate(state(id)%QQ1)
    deallocate(state(id)%QQL)
    deallocate(state(id)%QQL1)
    deallocate(state(id)%DML)

    ! salinity, temperature & passive tracer concentration
    deallocate(state(id)%SAL)
    deallocate(state(id)%SAL1)
    deallocate(state(id)%TEM)
    deallocate(state(id)%TEM1)
    deallocate(state(id)%DYE)
    deallocate(state(id)%DYE1)

    ! shell fish larvae
    deallocate(state(id)%SFLSBOT)
    deallocate(state(id)%SFL)
    deallocate(state(id)%SFL2)

    deallocate(state(id)%TOX)
    deallocate(state(id)%TOX1)
    deallocate(state(id)%TOXB)
    deallocate(state(id)%TOXB1)

    deallocate(state(id)%SED)
    deallocate(state(id)%SED1)
    deallocate(state(id)%SEDB)
    deallocate(state(id)%SEDB1)

    deallocate(state(id)%SND)
    deallocate(state(id)%SND1)
    deallocate(state(id)%SNDB)
    deallocate(state(id)%SNDB1)

    deallocate(state(id)%HBED)
    deallocate(state(id)%HBED1)
    deallocate(state(id)%VDRBED)
    deallocate(state(id)%VDRBED1)

    deALLOCATE(STATE(ID)%NLOS)
    deALLOCATE(STATE(ID)%CLOS)
    deALLOCATE(STATE(ID)%NLOW)
    deALLOCATE(STATE(ID)%CLOW)
    deALLOCATE(STATE(ID)%NLOE)
    deALLOCATE(STATE(ID)%CLOE)
    deALLOCATE(STATE(ID)%NLON)
    deALLOCATE(STATE(ID)%CLON)
    deALLOCATE(STATE(ID)%QSUME)
    deALLOCATE(STATE(ID)%QSUM)
    deALLOCATE(STATE(ID)%QCHANU)
    deALLOCATE(STATE(ID)%QCHANV)
    deALLOCATE(STATE(ID)%AGWELV)
    deALLOCATE(STATE(ID)%AGWELV1)
    deALLOCATE(STATE(ID)%TEMB)
    deALLOCATE(STATE(ID)%ISCDRY)
    deALLOCATE(STATE(ID)%IMASKDRY)
    deALLOCATE(STATE(ID)%NATDRY)
    deALLOCATE(STATE(ID)%SUB)
    deALLOCATE(STATE(ID)%SUBO)
    deALLOCATE(STATE(ID)%SVB)
    deALLOCATE(STATE(ID)%SVBO)
    deALLOCATE(STATE(ID)%WQV)
    if (NXSP>0) deALLOCATE(STATE(ID)%WQVX)

  end subroutine model_state_deallocate

  ! --------------------------------------------------------------------------
  ! function for storing the current state in model instance memory
  ! --------------------------------------------------------------------------
  function model_get_state(id) result (ret_val)

    use global, only : TIMESEC, TBEGIN, TCON, &
         HP, H1P, HWQ, H2WQ, BELV, &
         UHDYE, UHDY1E, VHDXE, VHDX1E, &
         U, U1, V, V1, &
         QQ, QQ1, QQL, QQL1, DML, &
         SAL, SAL1, TEM, TEM1, DYE,DYE1, &
         SFLSBOT, SFL, SFL2, &
         TOX, TOX1, TOXB, TOXB1, &
         SED, SED1, SEDB, SEDB1, &
         SND, SND1, SNDB, SNDB1, &
         HBED, HBED1, VDRBED, VDRBED1, &
         NLOS, CLOS, NLOW, CLOW, NLOE, CLOE, NLON, CLON, &
         QSUME, QSUM, QCHANU, QCHANV, &
         AGWELV, AGWELV1, TEMB, ISCDRY, IMASKDRY, NATDRY, &
         SUB, SUBO, SVB, SVBO, &
         WQV, WQVX, NXSP

    implicit none

    ! return argument
    integer :: ret_val        ! ret_val = 0: succes, ret_val = -1: error.  

    !arguments
    integer, intent(in) :: id ! model instance identifier

    ret_val = -1
    if (debug) print*, 'model_get_state(id) is allocated ', allocated(state(id)%HP)

    ! time book keeping
    if (TIMESEC < TBEGIN * TCON) then
       state(id)%TIMESEC = TBEGIN * TCON
    else
       state(id)%TIMESEC = TIMESEC
    end if
    state(id)%TBEGIN = TBEGIN

    ! make copy of state(id)
    state(id)%HP = HP
    state(id)%H1P = H1P
    state(id)%HWQ = HWQ
    state(id)%H2WQ = H2WQ
    state(id)%BELV = BELV

    state(id)%UHDYE = UHDYE
    state(id)%UHDY1E = UHDY1E
    state(id)%VHDXE = VHDXE
    state(id)%VHDX1E = VHDX1E

    state(id)%U = U
    state(id)%U1 = U1
    state(id)%V = V
    state(id)%V1 = V1

    state(id)%QQ = QQ
    state(id)%QQ1 = QQ1
    state(id)%QQL = QQL
    state(id)%QQL1 = QQL1
    state(id)%DML = DML

    state(id)%SAL = SAL
    state(id)%SAL1 = SAL1
    state(id)%TEM = TEM
    state(id)%TEM1 = TEM1
    state(id)%DYE = DYE
    state(id)%DYE1 = DYE1

    state(id)%SFLSBOT = SFLSBOT
    state(id)%SFL = SFL
    state(id)%SFL2 = SFL2

    state(id)%TOX = TOX
    state(id)%TOX1  = TOX1
    state(id)%TOXB = TOXB
    state(id)%TOXB1  = TOXB1
    ! sediment
    state(id)%SED = SED
    state(id)%SED1  = SED1
    state(id)%SEDB = SEDB
    state(id)%SEDB1  = SEDB1
    !
    state(id)%SND = SND
    state(id)%SND1  = SND1
    state(id)%SNDB = SNDB
    state(id)%SNDB1  = SNDB1
    ! thickness of bed layers?
    state(id)%HBED = HBED
    state(id)%HBED1  = HBED1
    state(id)%VDRBED = VDRBED
    state(id)%VDRBED1  = VDRBED1

    STATE(ID)%NLOS = NLOS
    STATE(ID)%CLOS = CLOS
    STATE(ID)%NLOW = NLOW
    STATE(ID)%CLOW = CLOW
    STATE(ID)%NLOE = NLOE
    STATE(ID)%CLOE = CLOE
    STATE(ID)%NLON = NLON
    STATE(ID)%CLON = CLON
    STATE(ID)%QSUME = QSUME
    STATE(ID)%QSUM = QSUM
    STATE(ID)%QCHANU = QCHANU
    STATE(ID)%QCHANV = QCHANV
    STATE(ID)%AGWELV = AGWELV
    STATE(ID)%AGWELV1 = AGWELV1
    STATE(ID)%TEMB = TEMB
    STATE(ID)%ISCDRY = ISCDRY
    STATE(ID)%IMASKDRY = IMASKDRY
    STATE(ID)%NATDRY = NATDRY
    STATE(ID)%SUB = SUB
    STATE(ID)%SUBO = SUBO
    STATE(ID)%SVB = SVB
    STATE(ID)%SVBO = SVBO
    STATE(ID)%WQV = WQV
    if (NXSP>0) STATE(ID)%WQVX = WQVX
	 
    ret_val = 0

  end function model_get_state

  ! --------------------------------------------------------------------------
  ! function for setting the current state from model instance memory
  ! --------------------------------------------------------------------------
  function model_set_state(id) result(ret_val)

    use global, only : TIMESEC, TBEGIN, TCON, &
         HP, H1P, HWQ, H2WQ, BELV, &
         UHDYE, UHDY1E, VHDXE, VHDX1E, &
         U, U1, V, V1, &
         QQ, QQ1, QQL, QQL1, DML, &
         SAL, SAL1, TEM, TEM1, DYE,DYE1, &
         SFLSBOT, SFL, SFL2, &
         TOX, TOX1, TOXB, TOXB1, &
         SED, SED1, SEDB, SEDB1, &
         SND, SND1, SNDB, SNDB1, &
         HBED, HBED1, VDRBED, VDRBED1, &
         NLOS, CLOS, NLOW, CLOW, NLOE, CLOE, NLON, CLON, &
         QSUME, QSUM, QCHANU, QCHANV, &
         AGWELV, AGWELV1, TEMB, ISCDRY, IMASKDRY, NATDRY, &
         SUB, SUBO, SVB, SVBO, &
         WQV, WQVX,&
         ISRESTI, TIMEDAY, NXSP
    use model_extra_global

    implicit none

    ! return value
    integer :: ret_val         ! ret_val = 0: succes, ret_val = -1: error

    ! arguments
    integer, intent(in) :: id  ! model instance identifier

    ! locals
    character(len=80) :: TITLE

    ! zero all arrays
    call VARZEROReal
    call VARZEROInt

    call INPUT(TITLE)

    call model_init_2

    ! Act like this is a restart
    ISRESTI = 1

    ! time book keeping
    TIMESEC = state(id)%TIMESEC
    TIMEDAY = TIMESEC / 86400.0
    TBEGIN = TIMESEC/TCON

    ! set state(id) variables of EFDC model
    HP = state(id)%HP
    H1P = state(id)%H1P
    HWQ = state(id)%HWQ
    H2WQ = state(id)%H2WQ
    BELV = state(id)%BELV

    UHDYE = state(id)%UHDYE
    UHDY1E = state(id)%UHDY1E
    VHDXE  = state(id)%VHDXE
    VHDX1E = state(id)%VHDX1E

    U = state(id)%U
    U1 = state(id)%U1
    V = state(id)%V
    V1 = state(id)%V1

    QQ = state(id)%QQ
    QQ1 = state(id)%QQ1
    QQL = state(id)%QQL
    QQL1 = state(id)%QQL1
    DML = state(id)%DML

    SAL = state(id)%SAL
    SAL1 = state(id)%SAL1
    TEM = state(id)%TEM
    TEM1 = state(id)%TEM1
    DYE = state(id)%DYE
    DYE1 = state(id)%DYE1

    SFLSBOT = state(id)%SFLSBOT
    SFL = state(id)%SFL
    SFL2 = state(id)%SFL2

    TOX  = state(id)%TOX
    TOX1  = state(id)%TOX1
    TOXB  = state(id)%TOXB
    TOXB1  = state(id)%TOXB1
    ! sediment
    SED  = state(id)%SED
    SED1  = state(id)%SED1
    SEDB  = state(id)%SEDB
    SEDB1  = state(id)%SEDB1
    !
    SND  = state(id)%SND
    SND1  = state(id)%SND1
    SNDB  = state(id)%SNDB
    SNDB1  = state(id)%SNDB1
    ! thickness of bed layers?
    HBED  = state(id)%HBED
    HBED1  = state(id)%HBED1
    VDRBED  = state(id)%VDRBED
    VDRBED1  = state(id)%VDRBED1

    ! open boundary conditions
    ! last iteration with outflow
    ! last value at boundary
    !NLOS = STATE(ID)%NLOS
    NLOS = 0 ! restart from N = 0 
    CLOS = STATE(ID)%CLOS
    !NLOW = STATE(ID)%NLOW
    NLOW = 0 ! restart from N = 0 
    CLOW = STATE(ID)%CLOW
    !NLOE = STATE(ID)%NLOE
    NLOE = 0 ! restart from N = 0 
    CLOE = STATE(ID)%CLOE
    !NLON = STATE(ID)%NLON
    NLON = 0 ! restart from N = 0 
    CLON = STATE(ID)%CLON
    
    
    QSUME = STATE(ID)%QSUME
    QSUM = STATE(ID)%QSUM
    QCHANU = STATE(ID)%QCHANU
    QCHANV = STATE(ID)%QCHANV
    AGWELV = STATE(ID)%AGWELV
    AGWELV1 = STATE(ID)%AGWELV1
    TEMB = STATE(ID)%TEMB
    ISCDRY = STATE(ID)%ISCDRY
    IMASKDRY = STATE(ID)%IMASKDRY
    NATDRY = STATE(ID)%NATDRY
    SUB = STATE(ID)%SUB
    SUBO = STATE(ID)%SUBO
    SVB = STATE(ID)%SVB
    SVBO = STATE(ID)%SVBO

    call model_restart_init
    call model_init_3

    WQV = STATE(ID)%WQV
    if (NXSP > 0) WQVX = STATE(ID)%WQVX
    ret_val = 0

  end function model_set_state

  ! --------------------------------------------------------------------------
  ! function for setting the EFDC time variables
  ! --------------------------------------------------------------------------
  function model_set_time(time_in) result(ret_val)

    use global, only: tbegin, timesec, tcon, timeday

    implicit none

    ! return value
    integer :: ret_val          ! ret_val = 0: succes, ret_val = -1: error

    ! arguments
    real, intent(in) :: time_in ! time in seconds

    ! reset time parameters [seconds]
    tbegin = time_in/tcon ! begin time scaled with tcon (EFDC.INP)
    timesec = time_in     ! time in seconds
    timeday = timesec/86400.0
    ret_val = 0

  end function model_set_time

  ! --------------------------------------------------------------------------
  ! function for getting the time from EFDC
  ! --------------------------------------------------------------------------
  function model_get_time(time_out) result(ret_val)

    use global, only: tbegin, timesec, tcon, timeday

    implicit none

    ! return value
    integer :: ret_val            ! ret_val = 0: success

    ! arguments
    real, intent(out) :: time_out ! time in seconds

    ! get current time [seconds]
    if (timesec < tbegin*tcon) then
       time_out = tbegin*tcon
    else
       time_out = timesec
    end if
    ret_val = 0

  end function model_get_time

  ! --------------------------------------------------------------------------
  ! function for getting the time period of EFDC model
  ! --------------------------------------------------------------------------
  function model_get_time_period(time_period_out) result(ret_val)

    use global, only: nts, dt

    implicit none

    ! return value
    integer :: ret_val                   ! ret_val = 0: success

    ! arguments
    real, intent(out) :: time_period_out ! time period in seconds 

    ! get current time [seconds]
    time_period_out = NTS * DT
    ret_val = 0

  end function model_get_time_period

end module model_state
