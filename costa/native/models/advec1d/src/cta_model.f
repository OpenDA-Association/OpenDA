!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/advec1d/src/cta_model.f $
! $Revision: 807 $, $Date: 2009-03-13 13:48:06 +0100 (Fri, 13 Mar 2009) $
!
c COSTA: Problem solving environment for data assimilation
c Copyright (C) 2006  Nils van Velzen
c 
c This library is free software; you can redistribute it and/or
c modify it under the terms of the GNU Lesser General Public
c License as published by the Free Software Foundation; either
c version 2.1 of the License, or (at your option) any later version.
c 
c This library is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
c Lesser General Public License for more details.
c 
c You should have received a copy of the GNU Lesser General Public
c License along with this library; if not, write to the Free Software
c Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

      subroutine advecstoch_createclass_old(modelcls)
c
c     This routine glues this model into COSTA by creating
c     a model class for this model of which instances can be created
c
      implicit none
      include 'cta_f77.inc'
      include 'advec_model.inc'

c     Header variables:
c     handle of model class
      integer modelcls


c     Local variables:
c     external subroutines
      external advecstoch_create_size
      external advecstoch_create_init
      external advecstoch_compute
      external advecstoch_getstate
      external advecstoch_setstate
      external advecstoch_getnoisecovar
      external advecstoch_getnoisecount
      external advecstoch_getobsvalues
      external advecstoch_addnoise


c     return error flag of COSTA calls
      integer ierr
c     handles of all user functions needed by model class
      integer h_func(FCTA_MODEL_NUMFUNC)
c     handle to interface of functions (not used in implementation)
      integer hintf
c     loop counter
      integer i
c
c     Initialise 
c
      do i=1,FCTA_MODEL_NUMFUNC
         h_func(i)=CTA_NULL
      enddo
c
c     Create COSTA function handles for all user functions     
c
      call cta_func_create(" ",advecstoch_create_size,hintf,
     +                     h_func(FCTA_MODEL_CREATE_SIZE), ierr);
      call cta_func_create(" ",advecstoch_create_init,hintf,
     +                     h_func(FCTA_MODEL_CREATE_INIT), ierr);
      call cta_func_create(" ",advecstoch_compute,hintf,
     +                     h_func(FCTA_MODEL_COMPUTE), ierr);
      call cta_func_create(" ",advecstoch_getstate,hintf,
     +                     h_func(FCTA_MODEL_GET_STATE), ierr);
      call cta_func_create(" ",advecstoch_setstate,hintf,
     +                     h_func(FCTA_MODEL_SET_STATE), ierr);
      call cta_func_create(" ",advecstoch_getnoisecount,hintf,
     +                     h_func(FCTA_MODEL_GET_NOISE_COUNT), ierr);
      call cta_func_create(" ",advecstoch_getnoisecovar,hintf,
     +                     h_func(FCTA_MODEL_GET_NOISE_COVAR), ierr);
      call cta_func_create(" ",advecstoch_getobsvalues,hintf,
     +                     h_func(FCTA_MODEL_GET_OBSVALUES), ierr);
      call cta_func_create(" ",advecstoch_addnoise,hintf,
     +                     h_func(FCTA_MODEL_ADD_NOISE), ierr)
 
      call cta_model_defineclass("advec_stoch",h_func,modelcls,ierr);


      end subroutine advecstoch_createclass_old

!DEC$ ATTRIBUTES DLLEXPORT::advecstoch_create_size
      subroutine advecstoch_create_size(userdata,memsize,ierr)
      implicit none
      include 'cta_f77.inc'
      include 'advec_model.inc'

      integer userdata
      integer memsize

      integer ierr
c
c     The datablock that must be created contains a single state vector
c     and a time instance
c
      call cta_sizeof(CTA_INTEGER,memsize,ierr)
      memsize=memsize*SIZE_DATA

      end subroutine advecstoch_create_size

!DEC$ ATTRIBUTES DLLEXPORT::advecstoch_create_init
      subroutine advecstoch_create_init(this,modeldata,userdata,
     +                                  ierr)
      implicit none
      include 'cta_f77.inc'
      include 'advec_model.inc'
      
      integer this
      integer modeldata(SIZE_DATA)
      integer userdata
      integer hstate
      integer ierr,retval
      integer hstates(2)
      integer hdeterm,hnoise
      integer hvec1
      integer htime
      integer tnoise
      integer thorz

c     initialise deterministic model
      call advec_model_initialise(userdata,hdeterm,ierr)
      if (ierr.ne.0) goto 9999


c     create overal state vector. The structure is
c
c                / determ state 
c     main_state+
c                \      
c                 \noise
c                       

c    - create the states
      
      call cta_treevector_create('diff model stochastic',
     +   'advecStoch', hstate,retval)
      if (retval.ne.CTA_OK) goto 9999
      call cta_treevector_create('diff model complete state',
     +   'advecNoise', hnoise,retval)
      if (retval.ne.CTA_OK) goto 9999

c    -concatinate the states in orde to create the overall structure

      hstates(1)=hdeterm
      hstates(2)=hnoise
      call cta_treevector_conc(hstate, hstates,2,retval)
      if (retval.ne.CTA_OK) stop 'cta_model (7) Error niet goed'

c     -allocate array and set initial values
      call cta_vector_create(CTA_DEFAULT_VECTOR, 1, CTA_DOUBLE,
     +                          userdata,hvec1, ierr)
      call cta_vector_setconstant(hvec1,0.0d0,CTA_DOUBLE,ierr)
      if (ierr.ne.CTA_OK) stop 'cta_model (8) Error niet goed'

      call cta_treevector_setvec(hnoise,hvec1,retval)
      if (retval.ne.CTA_OK) stop 'cta_model (9) Error niet goed'
c
c     Initialise time
c
      call cta_time_create(htime,ierr)
      call cta_time_setspan(htime,0.0d0,0.0d0,ierr)

      call cta_time_create(thorz,ierr)
      call cta_time_setspan(thorz,0.0d0,5.0d0,ierr)



c     Create time object for adding noise
      call cta_time_create(tnoise,retval)
      if (retval.ne.CTA_OK) stop 'cta_model (20) Error niet goed'
      call cta_time_setspan(tnoise,0.0d0, -1.0d0, retval)
      if (retval.ne.CTA_OK) stop 'cta_model (21) Error niet goed'
c
c     set datablock
c
      modeldata(INDX_STATE)  = hstate
      modeldata(INDX_TIME)   = htime 
      modeldata(INDX_TNOISE) = tnoise
      modeldata(INDX_THORZ)  = thorz 

      return
9999  stop 'Error in cta_model_initialise'

      end subroutine advecstoch_create_init

!DEC$ ATTRIBUTES DLLEXPORT::advecstoch_compute
      subroutine advecstoch_compute(modeldata,time,ierr)

      implicit none
      include 'cta_f77.inc'
      include 'advec_model.inc'
      include 'modelparam.inc'
      integer modeldata(SIZE_DATA), ierr, time


      integer tnoise
      integer hmodel,hnoise,hnvec,hdeterm,nnoise
      double precision dtime,rdum
      integer hstate,htime,idum
      double precision forget_bc, rnd, noise
      logical inspan
      parameter (forget_bc=0.7d0)

      hstate=modeldata(INDX_STATE)
      htime =modeldata(INDX_TIME)
      tnoise=modeldata(INDX_TNOISE)

c     Get time      
      call cta_time_getspan(htime,dtime,rdum,ierr)
      if (ierr.ne.CTA_OK) stop 'heatstoch_compute: checking timespan'

      call cta_time_inspan(time, tnoise, inspan,ierr)
      if (ierr.ne.CTA_OK) stop 'heatstoch_compute: checking timespan'


c
c     Get handles of the sub-states
c
      hdeterm=CTA_NULL
      hnoise =CTA_NULL
      hnvec  =CTA_NULL
      call cta_treevector_getsubtreevec(hstate, 'advecXC',
     +                           hdeterm,ierr) 
      call cta_treevector_getsubtreevec(hstate, 'advecNoise',
     +                                 hnoise,ierr) 
      call cta_treevector_getsize(hnoise,nnoise,ierr)
      call cta_vector_create(CTA_DEFAULT_VECTOR,nnoise,
     +                       CTA_DOUBLE,idum,
     +                       hnvec,ierr)
      call cta_treevector_getvec(hnoise,hnvec,ierr)
c
c     Add noise
c
      if (inspan) then
         call cta_rand_n(rnd,ierr)
         call cta_vector_getvals(hnvec,noise,1,CTA_DOUBLE,ierr)
         noise=noise+rnd
         call cta_vector_setvals(hnvec,noise,1,CTA_DOUBLE,ierr)
      endif

c
c     Deterministic model
c
      call advec_model_timestep(hdeterm,hnvec,dtime)
c
c     Stochastic model (noise)
c
      call cta_vector_scal(hnvec,forget_bc,ierr)
      call cta_treevector_setvec(hnoise,hnvec,ierr)
      call cta_vector_free(hnvec,ierr)

c     Set new time
      dtime=dtime+dt
      call cta_time_setspan(htime,dtime,dtime,ierr)

      end subroutine advecstoch_compute

!DEC$ ATTRIBUTES DLLEXPORT::advecstoch_setstate
      subroutine advecstoch_setstate(modeldata,hstatevec,
     +                               ierr)
      implicit none
      include 'cta_f77.inc'
      include 'advec_model.inc'
      integer modeldata(SIZE_DATA),hstate,hstatevec
      integer ierr


      hstate=modeldata(INDX_STATE)
      call cta_treevector_copy(hstatevec,hstate,ierr)
      if (ierr.ne.CTA_OK) return
      ierr=CTA_OK

      end subroutine advecstoch_setstate

!DEC$ ATTRIBUTES DLLEXPORT::advecstoch_getstate
      subroutine advecstoch_getstate(modeldata,hstatevec,ierr)
      implicit none
      include 'cta_f77.inc'
      include 'advec_model.inc'
      integer modeldata(SIZE_DATA),hstate,hstatevec
      integer ierr

      hstate=modeldata(INDX_STATE)
      if (hstatevec.eq.CTA_NULL) then
         call cta_treevector_duplicate(hstate,hstatevec,ierr)
         if (ierr.ne.CTA_OK) return
      endif
      call cta_treevector_copy(hstate,hstatevec,ierr)
      if (ierr.ne.CTA_OK) return
      ierr=CTA_OK

      end subroutine advecstoch_getstate

!DEC$ ATTRIBUTES DLLEXPORT::advecstoch_getnoisecount
      subroutine advecstoch_getnoisecount(modeldata,
     +                                   nnoise,ierr)
      implicit none
      include 'cta_f77.inc'
      include 'advec_model.inc'
      integer modeldata(SIZE_DATA)
      integer nnoise, ierr

      nnoise=1
      ierr=CTA_OK
      end subroutine advecstoch_getnoisecount

!DEC$ ATTRIBUTES DLLEXPORT::advecstoch_getnoisecovar
      subroutine advecstoch_getnoisecovar(modeldata,
     +                                   hstatevec,ierr)
      implicit none
      include 'cta_f77.inc'
      include 'advec_model.inc'
      integer modeldata(SIZE_DATA)
      integer hstatevec(*),ierr


      integer hstate
      integer hvec1
      double precision Qbc(1,1)
      integer n
      integer idum(1)


      hstate=modeldata(INDX_STATE)
c
c     Initialize Q-matrix
c
      Qbc(1,1)=1.0d0
c
c     Create state vector when necessary
c     

      if (hstatevec(1).eq.CTA_NULL) then
         call cta_treevector_duplicate(hstate,hstatevec(1),ierr)
         if (ierr.ne.CTA_OK) return
      endif
      call cta_treevector_getsize(hstatevec(1),n,ierr)
      if (ierr.ne.CTA_OK) return

      call cta_vector_create(CTA_DEFAULT_VECTOR, n,CTA_DOUBLE,
     +                       idum, hvec1,ierr)
      call cta_vector_setconstant(hvec1,0.0d0,CTA_DOUBLE,ierr)
      if (ierr.ne.CTA_OK) return
c
c     Set Q-'matrix' in last position of matrix
c
      call cta_vector_setval(hvec1,n,Qbc(1,1),CTA_DOUBLE,ierr)
      if (ierr.ne.CTA_OK) return

      call cta_treevector_setvec(hstatevec(1),hvec1,ierr)
      if (ierr.ne.CTA_OK) return

      ierr=CTA_OK
      end subroutine advecstoch_getnoisecovar


!DEC$ ATTRIBUTES DLLEXPORT::advecstoch_getobsvalues
      subroutine advecstoch_getobsvalues(
     +           modeldata,htime,hdescr,values,ierr)
      implicit none
      include "cta_f77.inc"
      include 'advec_model.inc'
      ! INPUTS
      integer modeldata(SIZE_DATA)
      integer htime   ! CTA_Time object for the prediction-time 
      integer hdescr  ! CTA_ObsDescr object, description of measurements
      ! OUTPUTS
      integer values  ! CTA_Vector object with predictions
      integer ierr    ! error code (CTA_OK: no error)

      ! LOCAL VARIABLES
      integer nmeasr  ! number of measurements
      integer imeasr  ! loop counter 1:nmeasr
      integer vN,vM   ! CTA_Vector objects with station (M,N) coordinates
      integer M,N     ! (M,N) coordinates of station
      integer idum    ! dummy input/output 
      integer hdeterm ! CTA_TreeVector object for Temperatures of the state
      integer vdeterm ! CTA_Vector object for Temperatures of the state
      double precision value ! Predicted value for one station
      integer hstate  ! CTA_TreeVector object from which prediction is wanted
      integer hitime   ! current model simulation time
      integer nstate  ! Size of state of deterministic model

      hstate=modeldata(INDX_STATE)
      hitime=modeldata(INDX_TIME)

      call cta_obsdescr_observation_count(hdescr,nmeasr,ierr)
      if (ierr.ne.CTA_OK) stop 
     +  'error advecstoch_getobsvalue: count observations'

      call cta_vector_create(CTA_DEFAULT_VECTOR, nmeasr, CTA_INTEGER,
     +                       idum, vN,ierr)
      if (ierr.ne.CTA_OK) stop 
     +  'error advecstoch_getobsvalue: create n-vector'
      call cta_vector_create(CTA_DEFAULT_VECTOR, nmeasr, CTA_INTEGER,
     +                       idum, vM,ierr)
      if (ierr.ne.CTA_OK) stop 
     +  'error advecstoch_getobsvalue: create m-vector'

      call cta_obsdescr_get_valueproperties(hdescr,'N',vN,CTA_INTEGER,
     +                       ierr)
      if (ierr.ne.CTA_OK) stop 
     +  'error advecstoch_getobsvalue: get n property'

      call cta_obsdescr_get_valueproperties(hdescr,'M',vM,CTA_INTEGER,
     +                       ierr)
      if (ierr.ne.CTA_OK) stop 
     +  'error advecstoch_getobsvalue: get m property'
c
c     Get the (water) levels from the state
c
      call cta_treevector_getsubtreevec(hstate, 'advecXC',hdeterm,ierr)
      if (ierr.ne.CTA_OK) stop 'error advecstoch_getsubtreevec'

      call cta_treevector_getsize(hdeterm, nstate,ierr)
      if (ierr.ne.CTA_OK) stop 'error advecstoch_getstatesize'
     
      call cta_vector_create(CTA_DEFAULT_VECTOR, nstate, CTA_DOUBLE,
     +                       idum,vdeterm,ierr)
      if (ierr.ne.CTA_OK) stop 'advecstoch_compute: create vdeterm'
      call cta_treevector_getvec(hdeterm, vdeterm,ierr);
      if (ierr.ne.CTA_OK) stop 'advecstoch_getobsvalue: get vdeterm'

      do imeasr = 1,nmeasr
c
c     FOR (all measurements) DO
c
c        Get the coordinates of the station 
c
         call cta_vector_getval(vN,imeasr,N,CTA_INTEGER,ierr)
         if (ierr.ne.CTA_OK) stop 
     +      'error heatstoch_getobsvalue: get n'
         call cta_vector_getval(vM,imeasr,M,CTA_INTEGER,ierr)
         if (ierr.ne.CTA_OK) stop 
     +      'error heatstoch_getobsvalue: get m'
c
c        Get the observed value
c
         call cta_vector_getval(vdeterm,n,value,
     +                          CTA_DOUBLE,ierr)
         if (ierr.ne.CTA_OK) stop 
     +      'error advecstoch_getobsvalue: get interpolated value'
         
         call cta_vector_setval(values,imeasr,value,CTA_DOUBLE,ierr)
         if (ierr.ne.CTA_OK) stop 
     +      'error advecstoch_getobsvalue: put interpolated value'
      end do

      call cta_vector_free(vN,ierr)
      if (ierr.ne.CTA_OK) stop 'heatstoch_compute: free vN'
      call cta_vector_free(vM,ierr)
      if (ierr.ne.CTA_OK) stop 'heatstoch_compute: free vM'
      call cta_vector_free(vdeterm,ierr)
      if (ierr.ne.CTA_OK) stop 'heatstoch_compute: free vdeterm'

      end subroutine advecstoch_getobsvalues

!DEC$ ATTRIBUTES DLLEXPORT::advecstoch_addnoise
      subroutine advecstoch_addnoise(datblc,tspan,ierr)
      implicit none
      include 'advec_model.inc'
      integer datblc(SIZE_DATA)
      integer tspan,ierr

      integer state,tnoise
      
      tnoise=datblc(INDX_TNOISE) 
      call cta_time_copy(tspan,tnoise,ierr)

      end subroutine advecstoch_addnoise

!DEC$ ATTRIBUTES DLLEXPORT::advecstoch_getcurrenttime
      subroutine advecstoch_getcurrenttime(modeldata, htime, retval)
      implicit none
      include 'cta_f77.inc'
      include 'advec_model.inc'
      integer modeldata(SIZE_DATA)

      integer retval
      integer htime

      call cta_time_copy(modeldata(INDX_TIME),htime,retval)

      end subroutine advecstoch_getcurrenttime

!DEC$ ATTRIBUTES DLLEXPORT::advecstoch_gettimehorizon
      subroutine advecstoch_gettimehorizon(modeldata, htime, retval)
      implicit none
      include 'cta_f77.inc'
      include 'advec_model.inc'

      integer modeldata(SIZE_DATA)
      integer retval
      integer htime

      call cta_time_copy(modeldata(INDX_THORZ),htime,retval)

      end subroutine advecstoch_gettimehorizon




