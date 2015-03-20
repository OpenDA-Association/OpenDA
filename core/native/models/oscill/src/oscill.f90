!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/oscill/src/oscill.f90 $
! $Revision: 3786 $, $Date: 2013-01-29 16:56:53 +0100 (Tue, 29 Jan 2013) $
!
! COSTA: Problem solving environment for data assimilation
! Copyright (C) 2006  Nils van Velzen
! 
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
! 
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

! The oscillator model
!----------------------
! This is a stochastic model of a simple linear oscillator 
! (e.g. mass-spring system with friction)
! d(x)/d(t) = u
! d(u)/d(t) = - omega^2 * x - (2/t_damp) u
!
! White noise is used. Noise parameters are:
! noise_dt for the time integration
! std_vel for the velocity u
! std_pos for the position x
!
! This model is an sp-model builder model (see the corresponding memo)
! It has the following configuration, as shown in the XML-tree:
!       <modelbuild_sp>
!       <functions>
!          <create>oscill_create</create>
!          <covariance>oscill_covar</covariance>
!          <getobsvals>oscill_obs</getobsvals>
!          <compute>oscill_compute</compute>
!       </functions>
!       </modelbuild_sp>
!

module oscill

type oscill_param
   real(kind=8) ::avg_t_damp   !characteristic time-scale for friction [seconds]
   real(kind=8) ::avg_omega    !oscilation frequency [rad/s]
!   real(kind=8) ::stdev_t_damp !characteristic time-scale for friction [seconds]
!   real(kind=8) ::stdev_omega  !oscilation frequency [rad/s]
   real(kind=8) ::dt            !integration timestep
   real(kind=8) ::std_vel
   real(kind=8) ::std_pos
   real(kind=8) ::noise_dt
   
end type oscill_param


type(oscill_param), public ::pars

public ::dx_oscill
public ::oscill_param
public ::noise_oscill
public ::addx_oscill
private

contains

subroutine dx_oscill(t,x,dx,param)
implicit none
real(kind=8), dimension(2) ::param
real(kind=8), intent(in)  ::t
real(kind=8), intent(in)  ::x(2)
real(kind=8), intent(out) ::dx(2)
!
! The oscillator model
!
! simple linear oscilator (e.g. mass-spring system with friction)
! d(x)/d(t) = u
! d(u)/d(t) = - omega^2 * x - (2/t_damp) u
!
dx(1) = x(2)
dx(2) = -(param(2)*param(2))*x(1) - (2.0d0/param(1))*x(2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end subroutine dx_oscill

subroutine noise_oscill(t,dt,x,noise,dw)
implicit none
real(kind=8), intent(in)               ::t
real(kind=8), intent(in)               ::dt
real(kind=8), intent(in)               ::x(2)
real(kind=8), intent(out)              ::dw(2)
real(kind=8), intent(inout)            ::noise(3,2)

real(kind=8) ::rnd, t1, t2
integer ::ierr
!
! The oscillator model
!
   t1=noise(1,1)
   t2=noise(1,2)
   if (t>t2) then
      noise(:,1)=noise(:,2)
      noise(1,2)=noise(1,2)+pars%noise_dt
      call CTA_RAND_N(rnd,ierr)
      noise(2,2)=pars%std_pos*rnd
      call CTA_RAND_N(rnd,ierr)
      noise(3,2)=pars%std_vel*rnd
   endif
!
! Interpolate (linear) and compute the increment
!
  dw=(t2-t)*noise(2:3,1)+(t-t1)*noise(2:3,2)
  dw=dw-(t2-(t-dt))*noise(2:3,1)+((t-dt)-t1)*noise(2:3,2)
   
end subroutine noise_oscill

subroutine addx_oscill(t,x,addx,param,adx,adp)
! Adjoint model of oscill. Monday, 14 May 2007, JHS
implicit none
real(kind=8), intent(in)  ::t       ! time
real(kind=8), intent(in)  ::x(2)    ! fwd run state
real(kind=8), intent(in)  ::addx(2) ! adjoint for dx/dt
real(kind=8), intent(out) ::adx(2)  ! adjoint for x
real(kind=8), intent(out), optional ::adp(2)  ! adjoint for parameter
real(kind=8), intent(in)  ::param(2)! model parameters
!                           param(1)=t_damp, characteristic time-scale for friction [seconds]
!                           param(2)=omega, oscillation frequency [rad/s]
!
! The oscillator model
!
! simple linear oscilator (e.g. mass-spring system with friction)
! d(x)/d(t) = u
! d(u)/d(t) = - omega^2 * x - (2/t_damp) u
!
! This has adjoint code:
adx(1) = -(param(2)*param(2))*addx(2)
adx(2) = addx(1) - (2.0d0/param(1))*addx(2)

! Adjoint code for parameters:
if (present(adp)) then
    adp(1) = 2/param(1)/param(1)*x(2)*addx(2)
    adp(2) = -2*param(2)*x(1)*addx(2)
end if

end subroutine addx_oscill




end module oscill

!---------------------------------------------------------------------


!Initialize a model-instance:
!DEC$ ATTRIBUTES DLLEXPORT::oscill_create
subroutine oscill_create(hinput, state, sbound, sparam, nnoise, &
                     tHorizon, snamnoise, husrdata, ierr)
use oscill
implicit none
include "cta_f90.inc"

integer hinput    !(I) Model configuration string or COSTA-tree
integer state     !(O)  Een costa-state vector van jouw model
                  !     (met initiele waardes)
integer sbound    !(O) Een state-vector die gebruikt kan worden om een offset
                  !    op de forcings te geven (dus zoals in het model).
                  !    Je mag CTA_NULL terug geven.
integer sparam    !(O) Een state-vector waarmee de model-parameters gezet
                  !    kunnen worden. (dit is aleen relevant voor 
                  !    model-calibratie) je mag CTA_NULL terug geven.
integer nnoise    !(O) Het aantal noise parameters. Dit is aleen ongelijk 0
                  !    als we een stochastisch model hebben. In dat geval is zal
                  !    sbound en sparam in het algemeen CTA_NULL zijn.
integer tHorizon  !(O) costa time object met de tijd spanne van de simulatie
integer snamnoise !(O) indien nnoise>0 een costa-string met de naam van de
                  !    (sub)-state waarin alle noise zit
integer husrdata  !(O) hier kan je zelf een costa-tree in stoppen met extra
                  !    informatie die specifiek is voor deze instantiatie.
                  !    Deze info word aan alle functies doorgegeven. Met een
                  !    beetje geluk heb je deze niet nodig en zet je hem op
                  !    CTA_NULL 
integer ierr      !(O)  Als iets fout gegaan is zet je hem op een waarde
                  !     ongelijk CTA_OK.

integer                    ::inputTimeHorizon
real(kind=8), dimension(2) ::x0
real(kind=8), dimension(3,2) ::noise
integer ::vx

real(kind=8), dimension(2) ::p0    !:      model parameters 
                                   !       param(1)=characteristic time-scale for friction [seconds]
                                   !       param(2)=oscillation frequency [rad/s]
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! Initialise model (should be from input-tree)
   pars%dt         = 0.05
   pars%std_vel    = 1.0d0 !????
   pars%std_pos    = 0.2d0 !????
   pars%noise_dt   = 1.0d0 !????

  ! Get the initial model-parameters from the input
   call cta_tree_getvaluestr(hinput,'parameters/avg_t_damp',p0(1),CTA_DOUBLE,ierr)
   call cta_tree_getvaluestr(hinput,'parameters/avg_omega',p0(2),CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) then
      print *,'Error in oscill_create. Initial parameters not specified in input'
      return
   endif

   inputTimeHorizon=CTA_NULL
   call cta_tree_gethandlestr(hinput,'timehorizon',inputTimeHorizon,ierr)

   
   call cta_treevector_create('oscill model',   'oscill_model', state, ierr)
   if (ierr/=CTA_OK) return
   call cta_vector_create(CTA_DEFAULT_VECTOR, 2, CTA_DOUBLE, &
                          CTA_NULL, vx, ierr)
   if (ierr/=CTA_OK) return
   x0(1)=1.d0
   x0(2)=1.d0
   call cta_vector_setvals(vx,x0,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return
   call cta_treevector_setvec(state,vx,ierr)
   if (ierr/=CTA_OK) return

   sbound   =CTA_NULL
   
   !Creating and initializing the new state 'sparam'
   call cta_treevector_create('oscill sparam',   'oscill_sparam', sparam, ierr)
   if (ierr/=CTA_OK) return
   call cta_vector_create(CTA_DEFAULT_VECTOR, 2, CTA_DOUBLE, &
                          CTA_NULL, vx, ierr)
   if (ierr/=CTA_OK) return
   call cta_vector_setvals(vx,p0,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return
   call cta_treevector_setvec(sparam,vx,ierr)
   if (ierr/=CTA_OK) return

   nnoise = 2

   ! Set time horizon
   if (inputTimeHorizon==CTA_NULL) then
   ! First set default time horizon
      call cta_time_setspan(tHorizon,0.0d0,10.0d0, ierr)
   else
      call cta_time_copy(inputTimeHorizon,tHorizon, ierr)
   endif
   if (ierr/=CTA_OK) then
      print *, 'Error in oscill_create: Cannot set Time Horizon. Error ',ierr
      return
   endif

   call cta_string_set(snamnoise,'oscill_model',ierr)
   call cta_matrix_create(CTA_DEFAULT_MATRIX, 3, 2, CTA_DOUBLE, CTA_NULL, husrdata , ierr)
   noise=0.0d0
   call cta_matrix_setvals(husrdata,noise,3,2,CTA_DOUBLE,ierr)


   ierr     =CTA_OK

end subroutine oscill_create

!------------------------------------------------

!Initialize a model-instance:

!DEC$ ATTRIBUTES DLLEXPORT::oscill_create_det
subroutine oscill_create_det(hinput, state, sbound, sparam, nnoise, &
                     time0, snamnoise, husrdata, ierr)
use oscill
implicit none
include "cta_f90.inc"

integer hinput    !(I) Model configuration string or COSTA-tree
integer state     !(O)  Een costa-state vector van jouw model
                  !     (met initiele waardes)
integer sbound    !(O) Een state-vector die gebruikt kan worden om een offset
                  !    op de forcings te geven (dus zoals in het model).
                  !    Je mag CTA_NULL terug geven.
integer sparam    !(O) Een state-vector waarmee de model-parameters gezet
                  !    kunnen worden. (dit is aleen relevant voor 
                  !    model-calibratie) je mag CTA_NULL terug geven.
integer nnoise    !(O) Het aantal noise parameters. Dit is aleen ongelijk 0
                  !    als we een stochastisch model hebben. In dat geval is zal
                  !    sbound en sparam in het algemeen CTA_NULL zijn.
integer time0     !(O) costa time object met de initieele tijd van de simulatie
integer snamnoise !(O) indien nnoise>0 een costa-string met de naam van de
                  !    (sub)-state waarin alle noise zit
integer husrdata  !(O) hier kan je zelf een costa-tree in stoppen met extra
                  !    informatie die specifiek is voor deze instantiatie.
                  !    Deze info word aan alle functies doorgegeven. Met een
                  !    beetje geluk heb je deze niet nodig en zet je hem op
                  !    CTA_NULL 
integer ierr      !(O)  Als iets fout gegaan is zet je hem op een waarde
                  !     ongelijk CTA_OK.

real(kind=8), dimension(2) ::x0
real(kind=8), dimension(3,2) ::noise
integer ::vx, idum

real(kind=8), dimension(2) ::p0    !:      model parameters 
                                   !       param(1)=characteristic time-scale for friction [seconds]
                                   !       param(2)=oscillation frequency [rad/s]
integer minfo    ! metainfo
integer hgrid    ! grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! Initialise model (should be from input-tree)
   pars%dt         = 0.05
   pars%std_vel    = 1.0d0 !????
   pars%std_pos    = 0.2d0 !????
   pars%noise_dt   = 1.0d0 !????

  ! Get the initial model-parameters from the input
   call cta_tree_getvaluestr(hinput,'parameters/avg_t_damp',p0(1),CTA_DOUBLE,ierr)
   call cta_tree_getvaluestr(hinput,'parameters/avg_omega',p0(2),CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) then
      print *,'Error in oscill_create. Initial parameters not specified in input'
      return
   endif
   
   call cta_treevector_create('oscill model',   'oscill_model', state, ierr)
   if (ierr/=CTA_OK) return
   call cta_vector_create(CTA_DEFAULT_VECTOR, 2, CTA_DOUBLE, &
                          idum, vx, ierr)
   if (ierr/=CTA_OK) return
   x0(1)=1.d0
   x0(2)=1.d0
   call cta_vector_setvals(vx,x0,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return
   call cta_treevector_setvec(state,vx,ierr)
   if (ierr/=CTA_OK) return

   sbound   =CTA_NULL
   
   !Creating and initializing the new state 'sparam'
   call cta_treevector_create('oscill sparam',   'oscill_sparam', sparam, ierr)
   if (ierr/=CTA_OK) return
   call cta_vector_create(CTA_DEFAULT_VECTOR, 2, CTA_DOUBLE, &
                          idum, vx, ierr)
   if (ierr/=CTA_OK) return
   call cta_vector_setvals(vx,p0,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return
   call cta_treevector_setvec(sparam,vx,ierr)
   if (ierr/=CTA_OK) return
  ! deterministic
   nnoise = 0

   call cta_time_setspan(time0,0.0d0,0.0d0, ierr)
   call cta_string_set(snamnoise,'oscill_model',ierr)
   call cta_matrix_create(CTA_DEFAULT_MATRIX, 3, 2, CTA_DOUBLE, idum, husrdata , ierr)
   noise=0.0d0
   call cta_matrix_setvals(husrdata,noise,3,2,CTA_DOUBLE,ierr)

!  make metainfo; such that the ar1 model and this model can recognize their states

! removed call oscill_model_setgrid(state, ierr)

!   call cta_metainfo_create(minfo,ierr)
!   print *,'metainfo_create: ',ierr

!   call cta_metainfo_settag(minfo,'ar1-noise',ierr)
!   print *,'metainfo_settag: ',ierr, minfo, state

!  fill grid

!   call cta_metainfo_setgrid(minfo,hgrid,ierr);

!   call cta_treevector_setmetainfo(state, minfo,ierr);
!   print *,'metainfo_create: ',ierr

   ierr     =CTA_OK

end subroutine oscill_create_det

!------------------------------------------------





!Perform one or more time steps
!DEC$ ATTRIBUTES DLLEXPORT::oscill_compute
subroutine oscill_compute(timespan,state, saxpyforc, baddnoise, &
                      sparam, husrdata, ierr)
use oscill
use ode
implicit none
include "cta_f90.inc"

integer timespan    !:(I)   timespan to be simulated
integer state       !:(I/O) state-vector of the model 
integer saxpyforc   !:(I)   costa-state vector with values to be added to the forcings 
                    !       during the timespan
integer baddnoise   !:(I)   flag CTA_TRUE/CTA_FALSE add / don't add noise
                    !       during tijdstap (only for stochastic models)
integer sparam      !:(I)   costa state with the  model parameters
integer husrdata    !:(I/O) made in the create function
integer ierr        !:(O)   Error flag

real(kind=8), dimension(2) ::x_in, x_out
real(kind=8)               ::t_out, t1, t2
real(kind=8), dimension(3,2)            ::noise
real(kind=8), dimension(2) ::param !:      model parameters
                                   !       param(1)=characteristic time-scale for friction [seconds]
                                   !       param(2)=oscillation frequency [rad/s]


   call cta_time_getspan(timespan,t1,t2,ierr)
   if (ierr/=CTA_OK) return
   call cta_treevector_getvals(state,x_in,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return

   ! Get 'sparam' and set the value of the local variable 'param'
   call cta_treevector_getvals(sparam,param,2,CTA_DOUBLE,ierr)
   !call cta_treevector_duplicate(sparam,param,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return

   call cta_matrix_getvals(husrdata,noise,3,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return

   call runge_kutta4(t1,t2,pars%dt,x_in,dx_oscill, noise_oscill, (baddnoise==CTA_TRUE), &
                     noise,x_out, t_out,param)
   if (ierr/=CTA_OK) return

   call cta_matrix_setvals(husrdata,noise,3,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return
   call cta_treevector_setvals(state,x_out,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return


end subroutine oscill_compute



!------------------------------------------------

!Perform one or more time steps
!DEC$ ATTRIBUTES DLLEXPORT::oscill_compute_det
subroutine oscill_compute_det(timespan,state, saxpyforc, baddnoise, &
                      sparam, husrdata, ierr)
use oscill
use ode
implicit none
include "cta_f90.inc"

integer timespan    !:(I)   timespan to be simulated
integer state       !:(I/O) state-vector of the model 
integer saxpyforc   !:(I)   costa-state vector with values to be added to the forcings 
                    !       during the timespan
integer baddnoise   !:(I)   flag CTA_TRUE/CTA_FALSE add / don't add noise
                    !       during tijdstap (only for stochastic models)
integer sparam      !:(I)   costa state with the  model parameters
integer husrdata    !:(I/O) made in the create function
integer ierr        !:(O)   Error flag

real(kind=8), dimension(2) ::x_in, x_out
real(kind=8)               ::t_out, t1, t2
real(kind=8), dimension(3,2)            ::noise
real(kind=8), dimension(2) ::param !:      model parameters
                                   !       param(1)=characteristic time-scale for friction [seconds]
                                   !       param(2)=oscillation frequency [rad/s]

   call cta_handle_check(husrdata,CTA_MATRIX,ierr)
   call cta_handle_check(husrdata,CTA_VECTOR,ierr)

   call cta_time_getspan(timespan,t1,t2,ierr)
   if (ierr/=CTA_OK) return
   call cta_treevector_getvals(state,x_in,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return

   ! Get 'sparam' and set the value of the local variable 'param'
   call cta_treevector_getvals(sparam,param,2,CTA_DOUBLE,ierr)
   !call cta_treevector_duplicate(sparam,param,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return

   call cta_matrix_getvals(husrdata,noise,3,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return
   
   ! force model to be deterministic 
   baddnoise = CTA_FALSE
!   print *,'oscill_compute_det:  deterministic!'

   call runge_kutta4(t1,t2,pars%dt,x_in,dx_oscill, noise_oscill, (baddnoise==CTA_TRUE), &
                     noise,x_out, t_out,param)
   if (ierr/=CTA_OK) return

!   print *,' Time is ',t_out, '; state(1) : ( ',x_in(1),' -->', x_out(1),')'
                     
   call cta_matrix_setvals(husrdata,noise,3,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return
   call cta_treevector_setvals(state,x_out,2,CTA_DOUBLE,ierr)
   if (ierr/=CTA_OK) return


end subroutine oscill_compute_det

!-----------------------------------------------




!Provide the covariantie matrix of the noise parameters (only for
!stochastic models.
!DEC$ ATTRIBUTES DLLEXPORT::oscill_covar
subroutine oscill_covar(colsvar, nnoise, husrdata,ierr)
use oscill
implicit none
include "cta_f90.inc"
integer colsvar(*)  !:(O)   array with handles to costa state-vectors 
                    !       representing root of covariance matrix of noise
                    !       the i-th state-vector is the i-th column of
                    !       root covariance
                    !       matrix. 
integer nnoise      !:(I)   # noise parameters (dimension of colsvar)
integer husrdata    !:(I/O) Made in create
integer ierr        !:(O)   Error flag
! Friday, 17 November 2006 by JHS
! Set the variance equals to one for each noise parameter and assume that
! the noise terms are independent.
! The first state represents the velocity, while the second one the position.
! This function gives the sqrt(Q), instead of Q itself.
   call cta_treevector_setval(colsvar(1),1,pars%std_vel,CTA_DOUBLE,ierr)
   call cta_treevector_setval(colsvar(1),2,0.0d0,CTA_DOUBLE,ierr)
   call cta_treevector_setval(colsvar(2),1,0.0d0,CTA_DOUBLE,ierr)
   call cta_treevector_setval(colsvar(2),2,pars%std_pos,CTA_DOUBLE,ierr)

!   if (nnoise/=0) then
!      print *,'number of noise parameters (nnoise=',nnoise,') should be 0' 
!      stop    'Fatal error in oscill_covar'
!   endif
end subroutine oscill_covar

!Interpolatie naar de observaties
!DEC$ ATTRIBUTES DLLEXPORT::oscill_obs
subroutine oscill_obs(state ,hdescr, vval, husrdata, ierr)
implicit none
include "cta_f90.inc"
integer state   ! :(I) Modelstate to be interpolated
integer hdescr  ! :(I) Oobservation description  with meta-information of the
                !      observations like location and measurement dimension
integer vval    ! :(O) costa-vector with interpolated value of stae
                !     to observations as described in hdescr
                !      Does not need to be created
integer husrdata! :(I/O) Made in create
integer ierr    ! :(O)   Error flag


integer nmeasr ! Number of requested values/observations
real(kind=8), dimension(:), allocatable ::values !Interpolated values
character(len=78) ::str !String for holding the unit
integer           ::imeasr, vindex, indx
integer           ::nKeys, vKeys, iKey, aKey
character(len=78) ::sKey
logical           ::isNOOS

   ! Determine the number of values that must be returned
   call cta_obsdescr_observation_count(hdescr,nmeasr,ierr)
   if (ierr.ne.CTA_OK) return
   if (nmeasr<1) then
      ierr=CTA_OK
      return
   endif
   ! Greate work variables
   call cta_vector_create(CTA_DEFAULT_VECTOR, nmeasr, CTA_INTEGER, &
                          CTA_NULL, vindex, ierr)
   if (ierr.ne.CTA_OK) return
   allocate(values(nmeasr))
 
   ! we support various observers. Check out the available keys
   call cta_obsdescr_property_count( hdescr, nKeys, ierr); if (ierr.ne.CTA_OK) return
   call cta_vector_create(CTA_DEFAULT_VECTOR, nKeys, CTA_STRING, CTA_NULL, vKeys, ierr);  if (ierr.ne.CTA_OK) return
   call cta_obsdescr_get_propertykeys(hdescr, vKeys, ierr); if (ierr.ne.CTA_OK) return

   !!call cta_vector_export(vKeys,CTA_FILE_STDOUT, ierr); if (ierr.ne.CTA_OK) return 
   isNOOS=.FALSE.
   call cta_string_create(aKey,ierr)

   do iKey=1,nKeys
      call cta_vector_getval(vKeys, iKey, aKey, CTA_STRING, ierr);
      call cta_string_get(aKey, sKey, ierr);
      isNOOS = (sKey .eq. 'xposition')
      if (isNOOS) exit
   enddo
   call cta_vector_free(vKeys, ierr)
   call cta_string_free(aKey, ierr)

   if (isNOOS) then
      call cta_obsdescr_get_valueproperties(hdescr, 'xposition', vindex, CTA_INTEGER, ierr)
   else
      ! Use the INDX-propery of the observation
      call cta_obsdescr_get_valueproperties(hdescr, 'INDX', vindex, CTA_INTEGER, ierr)
   endif
   
   if (ierr.ne.CTA_OK) then
      print *,'Cannot get property "INDX" from observation description'
      print *,'Did you use the wrong observation database?'
      stop    'Fatal error in oscill_obs'
   endif

   !Get the interpolated value from the state

!   print *,'oscill_obs: state: '
!   call cta_treevector_export(state,CTA_FILE_STDOUT,ierr)

   do imeasr=1,nmeasr
      call cta_vector_getval(vindex,imeasr,indx,CTA_INTEGER,ierr)
      call cta_treevector_getval(state,indx, values(imeasr),CTA_DOUBLE,ierr)
   enddo
   !Set the interpolated values
   call cta_vector_setvals(vval,values,nmeasr,CTA_DOUBLE,ierr)
   if (ierr.ne.CTA_OK) return


!   print *,'oscill_obs: nmeasr,values: ',nmeasr, values(1)

   ! Free work variables
   call cta_vector_free(vindex,ierr)
   deallocate(values)
   if (ierr.ne.CTA_OK) return
   ierr=CTA_OK

end subroutine oscill_obs

! This subroutine registers all functions in the COSTA administration.
! Afterwards, this model is ready to use
subroutine oscill_model_createfunc
implicit none
include "cta_f90.inc"
external ::oscill_create
external ::oscill_create_det
external ::oscill_covar
external ::oscill_obs
external ::oscill_compute
external ::oscill_compute_det

integer               ::hintf
integer               ::func
integer, dimension(6) ::ierr

hintf=CTA_NULL

call cta_func_create('oscill_create', oscill_create, hintf, func, ierr(1))
call cta_func_create('oscill_create_det', oscill_create_det, hintf, func, ierr(2))
call cta_func_create('oscill_covar',  oscill_covar,  hintf, func, ierr(3))
call cta_func_create('oscill_obs',    oscill_obs,    hintf, func, ierr(4))
call cta_func_create('oscill_compute',oscill_compute,hintf, func, ierr(5))
call cta_func_create('oscill_compute_det',oscill_compute_det,hintf, func, ierr(6))

if (any(ierr/=CTA_OK)) then
   print *,'oscill_model_createfunc: something whent wrong with creating',&
           'the function handles'
   print *,'The returned status codes are:'
   print *,ierr
   stop 'FATAL ERROR'
endif


end subroutine oscill_model_createfunc


