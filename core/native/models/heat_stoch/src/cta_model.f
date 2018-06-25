!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/heat_stoch/src/cta_model.f $
! $Revision: 4063 $, $Date: 2013-07-15 12:19:48 +0200 (Mon, 15 Jul 2013) $
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
!
!====================================================================
!
! This file implements the interface between the stochastic heat model
! and COSTA. It implements the methods that will be called from COSTA
! data assimilation programs. The main interfacing routine is:
!
!     subroutine heatstoch_createclass
!
! which links the remaining routines to their COSTA names. The remaining
! routines are: 
!
!	subroutine heatstoch_create_size
!	subroutine heatstoch_create_init
!	subroutine heatstoch_compute
!	subroutine heatstoch_setstate
!	subroutine heatstoch_getstate
!	subroutine heatstoch_getnoisecount
!	subroutine heatstoch_getnoisecovar
!	subroutine heatstoch_getobsvalues
!
! These are not called by COSTA directly, but only through their COSTA
! names.
!
!====================================================================
!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_createclass_old

      subroutine heatstoch_createclass_old(modelcls)
!--------------------------------------------------------------------
!     This routine glues this model into COSTA by creating
!     a model class for this model of which instances can be created
!
!     ARGUMENTS
!
!     integer modelcls  O  handle of the created model class
!
!--------------------------------------------------------------------
      implicit none
!     include the file with definitions of COSTA parameters
      include 'heat_stoch.inc'
      include 'cta_f77.inc'
!
!     ARGUMENTS
      integer modelcls
!
!     LOCAL VARIABLES
      external heatstoch_create_size
      external heatstoch_create_init
      external heatstoch_compute
      external heatstoch_getstate
      external heatstoch_setstate
      external heatstoch_getnoisecovar
      external heatstoch_getnoisecount
      external heatstoch_getobsvalues
      external heatstoch_addnoise

c     return error flag of COSTA calls
      integer ierr
c     handles of all user functions needed by model class
      integer h_func(FCTA_MODEL_NUMFUNC)
c     handle to interface of functions (not used in implementation)
      integer hintf
c     loop counter
      integer i
c
c     Initialise the list of function handles
      do i=1,FCTA_MODEL_NUMFUNC
         h_func(i)=CTA_NULL
      enddo
c
c     Create COSTA function handles for all user functions     
c
      call cta_func_create(" ",heatstoch_create_size,hintf,
     +                     h_func(FCTA_MODEL_CREATE_SIZE), ierr);
      call cta_func_create(" ",heatstoch_create_init,hintf,
     +                     h_func(FCTA_MODEL_CREATE_INIT), ierr);
      call cta_func_create(" ",heatstoch_compute,hintf,
     +                     h_func(FCTA_MODEL_COMPUTE), ierr);
      call cta_func_create(" ",heatstoch_getstate,hintf,
     +                     h_func(FCTA_MODEL_GET_STATE), ierr);
      call cta_func_create(" ",heatstoch_setstate,hintf,
     +                     h_func(FCTA_MODEL_SET_STATE), ierr);
      call cta_func_create(" ",heatstoch_getnoisecount,hintf,
     +                     h_func(FCTA_MODEL_GET_NOISE_COUNT), ierr);
      call cta_func_create(" ",heatstoch_getnoisecovar,hintf,
     +                     h_func(FCTA_MODEL_GET_NOISE_COVAR), ierr);
      call cta_func_create(" ",heatstoch_getobsvalues,hintf,
     +                     h_func(FCTA_MODEL_GET_OBSVALUES), ierr);
      call cta_func_create(" ",heatstoch_addnoise,hintf,
     +                     h_func(FCTA_MODEL_ADD_NOISE), ierr);
     
      call cta_model_defineclass('heat_stoch',h_func,modelcls,ierr);
      

      end subroutine heatstoch_createclass_old

!====================================================================

!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_create_size
      subroutine heatstoch_create_size(userdata,memsize,ierr)
!--------------------------------------------------------------------
!     This routine returns the size of the memory block that is needed
!     by a model instance.
!
!     ARGUMENTS
!
!     integer userdata     I  user data
!     integer memsize      O  size of memory block needed for model instance
!     integer ierr         O  result of this routine; cta_ok when succesful
!
!--------------------------------------------------------------------
      implicit none
      include 'cta_f77.inc'
      include 'heat_stoch.inc'

!     ARGUMENTS
      integer userdata
      integer memsize
      integer ierr
      integer intsiz
c
c     The datablock that must be created only contains a single state vector
c
      call cta_sizeof(CTA_INTEGER,intsiz,ierr)
      memsize=SIZE_DATABLOCK*intsiz

      end subroutine heatstoch_create_size

!====================================================================
!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_create_init
      subroutine heatstoch_create_init(this,datblc,userdata,
     +                                 ierr)
!--------------------------------------------------------------------
!     This routine initialises a new model instance
!
!     ARGUMENTS
!
!     integer this         I  handle of this model
!     integer datblc     IO memory block to store data related to this
!                             model instance; in this case: handle to the state
!     integer userdata     I  user data
!     integer ierr         O  result of this routine; cta_ok when succesful
!
!     create a state vector. The structure is
!
!                 deterministic state  
!                / 
!     main_state+
!                \       / NHEAT
!                 \noise +
!                        \ NBC
!--------------------------------------------------------------------
      implicit none
      include 'cta_f77.inc'
      include 'modelparam.inc'
      include 'stochparam.inc'
      include 'heat_stoch.inc'

!     ARGUMENTS
      integer this
      integer userdata
      integer datblc(SIZE_DATABLOCK)
      integer ierr

      integer hstate
      integer tnoise
      integer hdescr

      integer retval, i, dum(1),handles(3),hintf
c     (sub) state vectors
      integer hdeterm,hnoise,hnheat,hnbc,hstates(2)
c     COSTA vectors
      integer hvec1, hvec2, hvec3
      integer curTime, tHorizon
      logical first
      data first /.true./
      save first

      ierr=0
!
!     create the five elements from which the state is constructed
!
!
!     first, the state that will be the concatenation of the
!     deterministic part and the noise part
      call cta_treevector_create('diff model stochastic',
     +   'stoch', datblc(INDX_STATE),retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error creating datblc'

!     next, the deterministic substate
      call cta_treevector_create('diff model deterministic',
     +   'determ', hdeterm,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error creating hdeterm'

!     next, the noise substate, which will be the concatenation
!     of the two noise sources: heat and boundary noise
      call cta_treevector_create('diff model complete noise',
     +   'noise', hnoise,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error creating hnoise'

!     next, the heat noise substate
      call cta_treevector_create('diff model heat noise','nheat',
     +                       hnheat,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error creating hnheat'

!     finally, the boundary noise substate
      call cta_treevector_create('diff model boundary noise','nbc',
     +                       hnbc,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error creating hnbc'

!     concatenate the heat noise substate and the boundary substate
!     to form the noise substate
      hstates(1)=hnheat
      hstates(2)=hnbc
      call cta_treevector_conc(hnoise, hstates,2,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error conc.hnheat hnbc'

!     concatenate the deterministic substate and the noise substate
!     to form the full state
      hstates(1)=hdeterm
      hstates(2)=hnoise
      call cta_treevector_conc(datblc(INDX_STATE), hstates,2,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error conc.hdeterm hnois'

!     create meta information for the deterministic state
!
      call cta_metainfo_create(hdescr,ierr)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error cta_metainfo_create'

      call cta_metainfo_setreggrid(hdescr, 'heatGrid', NX, NY, 0,
     +                      0.0d0, 0.0d0,
     +                      0.0d0,
     +                      1.0d0, 1.0d0, 0.0d0, retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error cta_metainfo_setregg
     +rid'
      call cta_treevector_setmetainfo(hdeterm, hdescr, ierr);


!
!
!     Create and initialize vectors for the values of the state vectors.
!     This will be done by creating vectors with default values that
!     are then copied into the state.
!
!
!     first, create a vector with the size of the deterministic
!     state and set all elements to zero
      call cta_vector_create(CTA_DEFAULT_VECTOR, NMODEL, CTA_DOUBLE,
     +                       dum,hvec1,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error creating hvec1'
      call cta_vector_setconstant(hvec1,0d0,CTA_DOUBLE,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error setting hvec1'

!     next, create a vector with the size of the heat noise state
!     and set all elements to zero
      call cta_vector_create(CTA_DEFAULT_VECTOR, NHEAT, CTA_DOUBLE,
     +                       dum,hvec2,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error creating hvec2'
      call cta_vector_setconstant(hvec2,0d0,CTA_DOUBLE,retval)
      if (retval.ne.CTA_OK) 
     +       stop 'heatstoch_create_init: Error setting hvec2'

!     finally, create a vector with the size of the boundary condition
!     noise and set all elements to zero
      call cta_vector_create(CTA_DEFAULT_VECTOR, NBC,   CTA_DOUBLE,
     +                       dum,hvec3,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error creating hvec3'
      call cta_vector_setconstant(hvec3,0d0,CTA_DOUBLE,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error setting hvec3'
!
!
!     Use the vectors to set the state
!     NB: the vectors are copied into the state; they do NOT become
!         part of the state
!
!
      call cta_treevector_setvec(hdeterm,hvec1,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error setting hdeterm'
      call cta_treevector_setvec(hnheat, hvec2,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error setting hnheat'
      call cta_treevector_setvec(hnbc,   hvec3,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error setting hnbc'

!
!
!     The vectors must now be destroyed because they are no longer
!     needed
!
!
      call cta_vector_free(hvec1,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error freeing hvec1'
      call cta_vector_free(hvec2,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error freeing hvec2'
      call cta_vector_free(hvec3,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_create_init: Error freeing hvec3'

c     Create time object for adding noise
      call cta_time_create(tnoise,retval)
      if (retval.ne.CTA_OK) stop 'cta_model (20) Error niet goed'
      call cta_time_setspan(tnoise,0.0d0, -1.0d0, retval)
      if (retval.ne.CTA_OK) stop 'cta_model (21) Error niet goed'
      
      datblc(INDX_TNOISE) = tnoise

      call cta_time_create(curTime,retval)
      call cta_time_create(tHorizon,retval)
      call cta_time_setspan(curTime, 0.0d0, 0.0d0,retval)
      call cta_time_setspan(tHorizon,0.0d0,50.0d0,retval)

      datblc(INDX_TIME)     = curTime
      datblc(INDX_THORIZON) = tHorizon

      ierr=CTA_OK
      return
9999  stop 'Error in cta_model_initialise'

      end subroutine heatstoch_create_init

!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_getcurrenttime
      subroutine heatstoch_getcurrenttime(datblc, htime, retval)
      implicit none
      include 'cta_f77.inc'
      include 'heat_stoch.inc'

      integer datblc(SIZE_DATABLOCK)
      integer retval
      integer htime

      call cta_time_copy(datblc(INDX_TIME),htime,retval)

      end subroutine heatstoch_getcurrenttime

!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_gettimehorizon
      subroutine heatstoch_gettimehorizon(datblc, htime, retval)
      implicit none
      include 'cta_f77.inc'
      include 'heat_stoch.inc'

      integer datblc(SIZE_DATABLOCK)
      integer retval
      integer htime

      call cta_time_copy(datblc(INDX_THORIZON),htime,retval)

      end subroutine heatstoch_gettimehorizon


!====================================================================
!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_compute
      subroutine heatstoch_compute(datblc,htime,ierr)
!--------------------------------------------------------------------
!     This routine performs a single timestep of the stochastic heat
!     model
!
!     ARGUMENTS
!      
!     integer datblc I    memory block to store data related to this
!                           model instance; in this case: handle to the state
!     integer time     I    handle to the time information
!     integer ierr     O    result of this routine; cta_ok when succesful
!
!--------------------------------------------------------------------
      implicit none
      include 'cta_f77.inc'
      include 'modelparam.inc'
      include 'stochparam.inc'
      include 'heat_stoch.inc'

!     ARGUMENTS
      integer datblc(SIZE_DATABLOCK)
      integer ierr
      integer htime
!
!     LOCAL VARIABLES
!     the temperature field, heat noise field and boundary noise:
      double precision temp(NX,NY), heat_noise(NHEAT),
     +                 boundary_noise(NBC)
!     handles of (sub) state vectors:
      integer hdeterm,hnoise,hnheat,hnbc
!     COSTA vectors used in the manipulations
      integer vdeterm,vnoise,vnheat,vnbc

      integer retval, idum, tnoise,i,j,info
      double precision normdeterm, rndheat(nheat), rndbc(nbc), rdum
      logical inspan
      integer tstep, isspan, cTime
      double precision t1,t2,t3,t4
      integer istep,nsteps
!
      double precision QHeat(nheat,nheat), Qbc(nbc,nbc)
!
!     Check time
!
      call cta_time_create(tstep, ierr)
      if (ierr.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error create time'
      cTime=datblc(INDX_TIME)

      call cta_time_getspan(cTime,t1,t2,ierr)
      if (ierr.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error getspan (1)'
      call cta_time_getspan(htime,t3,t4,ierr)
      if (ierr.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error getspan (2)'
      call cta_time_isspan(htime, isspan, ierr)
      if (isspan==CTA_TRUE) then
          call cta_time_copy(htime,tstep, ierr)
      if (ierr.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error copy span'
      else
         call cta_time_setspan(tstep,t1,t3,ierr)
      if (ierr.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error set span'
      endif
      call cta_time_setspan(cTime,t4,t4,ierr)
      if (ierr.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error set span'

      call cta_time_setstep(tstep,1.0d0,ierr)
      if (ierr.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error set step'

      print *,'heatstoch_compute', t1,t2

      tnoise=datblc(INDX_TNOISE) 
      call cta_time_inspan(tstep, tnoise, inspan,retval)
!
!     do all timesteps
!
      call CTA_Time_CountSteps(tstep, nsteps, retval)

      print *,'heatstoch_compute #timesteps=',nsteps 
      
      do istep=1,nsteps



      if (retval.ne.CTA_OK) stop 'heatstoch_compute: checking timespan'
!
!     Get the handles to the subvectors of the state vector
!
!
!     first the handle to the state of the deterministic model
      call cta_treevector_getsubtreevec(datblc(INDX_STATE),
     +   'determ',hdeterm,
     +   retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error getting hdeterm'
!     next the handle to the (composite) noise state
      call cta_treevector_getsubtreevec(datblc(INDX_STATE),
     +   'noise', hnoise, retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error getting hnoise'
!     next the handle to the heat noise substate
      call cta_treevector_getsubtreevec(hnoise, 'nheat',  hnheat,
     +   retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error getting hnheat'
!     next the handle to the boundary condition noise substate
      call cta_treevector_getsubtreevec(hnoise, 'nbc',    hnbc  ,
     +   retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error getting hnbc'
!
!
!     Create COSTA-vector for state vector parts
!     These vectors will be used as intermediary for transferring data 
!     from the state to arrays, which can then be used in the actual
!     model.
!
!
!     first the vector that will hold the values for the deterministic
!     state
      call cta_vector_create(CTA_DEFAULT_VECTOR, NMODEL, CTA_DOUBLE,
     +                       idum,vdeterm,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error creating vdeterm'

!     next the vector that will hold the values for the heat noise substate
      call cta_vector_create(CTA_DEFAULT_VECTOR, NHEAT, CTA_DOUBLE,
     +                       idum,vnheat,retval)
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error creating vnheat'
!     finally, the vector that will hold the values for the boundary condition
!     substate
      call cta_vector_create(CTA_DEFAULT_VECTOR, NBC, CTA_DOUBLE,
     +                       idum,vnbc,retval)
      if (retval.ne.CTA_OK) 
     +    stop 'heatstoch_compute: Error creating vnbc'
!
!
!     Get COSTA-vectors with the state vector parts
!
!
!     first get the vector holding the deterministic model substate
      call cta_treevector_getvec(hdeterm, vdeterm,retval);
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error getting vdeterm'
!     next, the vector that will hold the heat noise substate
      call cta_treevector_getvec(hnheat, vnheat,retval);
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error getting vnheat'
!     finally, the vector that will hold the boundary noise substate
      call cta_treevector_getvec(hnbc, vnbc,retval);
      if (retval.ne.CTA_OK) stop 'heatstoch_compute: Error getting vnbc'
!
!
!     transfer values from COSTA-vectors to arrays
!
!
!     first the temperature field from the deterministic model substate
      call cta_vector_getvals(vdeterm,temp,NX*NY,CTA_DOUBLE, retval);
      if (retval.ne.CTA_OK) stop 'heatstoch_compute: Error getting temp'
!     next the heat noise field from the heat noise substate
      call cta_vector_getvals(vnheat,Heat_Noise,NHEAT,CTA_DOUBLE,
     +                        retval);
!     finally, the boundary noise from the boundary noise substate
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error getting heat noise'
      call cta_vector_getvals(vnbc,boundary_noise,NBC,CTA_DOUBLE,
     +                        retval);
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error getting boundary noise'
!


c     add correlated noise
      if (inspan) then 

c     Get covariance matrices and make lower triangular
      call compute_covars(QHeat, Qbc)         

         do i=2,NHEAT
           do j=1,i-1
              QHeat(i,j)=0.0d0
           enddo
         enddo
         do i=2,nbc
           do j=1,i-1
              Qbc(i,j)=0.0d0
           enddo
         enddo

         
         
c        Compute Cholesky decomposition of these matrices
         call dposv( 'L', nheat, 0, QHeat, nheat, rdum, nheat, info)
         if (info.ne.0) stop 'heatstoch_compute: error in dposv'

         call dposv( 'L', nbc, 0, Qbc, nbc, rdum, nbc, info)
         if (info.ne.0) stop 'heatstoch_compute: error in dposv'
         
         
         do i=1,nheat
            call cta_rand_n(rndheat(i),ierr)
         enddo

c        noise=noise+L*rnd_n         
         call dgemv('N',nheat,nheat,1.0d0,QHeat,nheat,
     +              rndheat,1,1.0d0,Heat_Noise,1)
         do i=1,nbc
            call cta_rand_n(rndbc(i),ierr)
         enddo
         
         call dgemv('N',nbc,nbc,1.0d0,Qbc,nbc,
     +              rndbc,1,1.0d0,Boundary_Noise,1)
      endif
!      print *,'Heat_Noise=',Heat_Noise
!      print *,'Boundary_Noise=',Boundary_Noise

!
!--------------------------------------------------------
!
!     Carry out the Time step Calculation
!


!
      call Linear_FullState_Step(temp, Heat_Noise, boundary_noise)
!
!!
!     Put values back in COSTA-vectors, so that they can be transferred
!     to the state
!
!
!     first, the temperature field
      call cta_vector_setvals(vdeterm,temp,NX*NY,CTA_DOUBLE, retval);
      if (retval.ne.CTA_OK) stop 'heatstoch_compute: Error setting temp'
!     next, the heat noise field
      call cta_vector_setvals(vnheat,heat_noise,NHEAT,CTA_DOUBLE,
     +                        retval);
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error setting heat noise'
!     finally, the boundary noise field
      call cta_vector_setvals(vnbc,boundary_noise,NBC,CTA_DOUBLE,
     +                        retval);
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error setting boundary noise'
!
!
!     Put the new values back into the state
!
!
!     first, the temperature field into the deterministic substate
      call cta_treevector_setvec(hdeterm, vdeterm,retval);
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error setting vdeterm'
!     next, the heat noise field into the heat noise substate
      call cta_treevector_setvec(hnheat, vnheat,retval);
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error setting vnheat'
!     finally, the boundary noise into the boundary noise field
      call cta_treevector_setvec(hnbc, vnbc,retval);
      if (retval.ne.CTA_OK) stop 'heatstoch_compute: Error setting vnbc'
!
!
!     Destroy the vectors; they are no longer needed as they have
!     only been used to transfer data between arrays and states
!
!
!     first, the vector holding the deterministic state
      call cta_vector_free(vdeterm,retval);
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error freeing vdeterm'
!     next, the vector holding the heat noise substate
      call cta_vector_free(vnheat,retval);
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error freeing vnheat'
!     finally, the vector holding the boundary noise substate
      call cta_vector_free(vnbc,retval);
      if (retval.ne.CTA_OK) 
     +           stop 'heatstoch_compute: Error freeing vnbc'
      enddo



      ierr=CTA_OK


      call cta_time_free(tstep,ierr)


      end subroutine heatstoch_compute


!====================================================================
!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_setstate
      subroutine heatstoch_setstate(datblc,hstatevec,ierr)
!--------------------------------------------------------------------
!     This routine fills the state with the values offered in hstatevec
!
!     ARGUMENTS
!      
!     integer datblc   I    memory block to store data related to this
!                             model instance; in this case: handle to the state
!     integer hstatevec  I    handle to the copy of the state vector
!     integer ierr       O    result of this routine; cta_ok when succesful
!--------------------------------------------------------------------
      implicit none

      include 'cta_f77.inc'
      include 'heat_stoch.inc'
!     include 'modelparam.inc'
!	include 'stochparam.inc'

!     ARGUMENTS
      integer datblc(SIZE_DATABLOCK)
      integer hstatevec
      integer ierr
!
!     just call the copy function
!
      call cta_treevector_copy(hstatevec,datblc(INDX_STATE),ierr)
      if (ierr.ne.CTA_OK) stop 'heatstoch_setstate: Error'

      ierr=CTA_OK

      end subroutine heatstoch_setstate


!====================================================================
!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_getstate
      subroutine heatstoch_getstate(datblc,hstatevec,ierr)
!--------------------------------------------------------------------
!     This routine returns a copy of the current state
!
!     ARGUMENTS
!      
!     integer datblc   I    memory block to store data related to this
!                             model instance; in this case: handle to the state
!     integer hstatevec  I/O  handle to the copy of the state vector
!                             (if CTA_NULL, the vector is created, else
!                              the contents are modified)
!     integer ierr       O    result of this routine; cta_ok when succesful
!--------------------------------------------------------------------
      implicit none
      include 'cta_f77.inc'
      include 'heat_stoch.inc'
!     include 'modelparam.inc'
!     include 'stochparam.inc'
!     ARGUMENTS
      integer datblc(SIZE_DATABLOCK)
      integer hstatevec
      integer ierr

      if (hstatevec.eq.CTA_NULL) then
         call cta_treevector_duplicate(datblc(INDX_STATE),hstatevec,
     +        ierr)
         if (ierr.ne.CTA_OK) 
     +       stop 'heatstoch_getstate: Error duplicating'
      endif
      call cta_treevector_copy(datblc(INDX_STATE),hstatevec,ierr)
      if (ierr.ne.CTA_OK) stop 'heatstoch_getstate: Error copying'
      ierr=CTA_OK

      end subroutine heatstoch_getstate


!====================================================================
!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_getnoisecount
      subroutine heatstoch_getnoisecount(datblc,nnoise,ierr)
!--------------------------------------------------------------------
!     This routine returns the number of noise parameters (i.e. the
!     number elements in the noise substate
!
!     ARGUMENTS
!      
!     integer datblc   I    memory block to store data related to this
!                             model instance; in this case: handle to the state
!     integer nnoise     O    the number of noise parameters
!     integer ierr       O    result of this routine; cta_ok when succesful
!--------------------------------------------------------------------
      implicit none
	include 'cta_f77.inc'
      include 'heat_stoch.inc'
      include 'modelparam.inc'
      include 'stochparam.inc'

!     ARGUMENTS
      integer datblc(SIZE_DATABLOCK)
      integer nnoise
      integer ierr

      nnoise=NPARAM
      ierr=CTA_OK
      end subroutine heatstoch_getnoisecount

!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_getnoisecovar
!====================================================================
      subroutine heatstoch_getnoisecovar(datblc,hstatevec,ierr)
!--------------------------------------------------------------------
!     This routine returns the number of noise parameters (i.e. the
!     number elements in the noise substate
!
!     ARGUMENTS
!      
!     integer datblc   I    memory block to store data related to this
!                             model instance; in this case: handle to the state
!     integer hstatevec  IO   list of handles to state vectors, states
!                             are modified upon return
!     integer ierr       O    result of this routine; cta_ok when succesful
!--------------------------------------------------------------------
      implicit none
      include 'cta_f77.inc'
      include 'heat_stoch.inc'
      include 'modelparam.inc'
      include 'stochparam.inc'
!
!     ARGUMENTS
	integer datblc(SIZE_DATABLOCK)
      integer hstatevec(*)
      integer ierr,info
!
!     LOCAL VARIABLES
!     counter of noise parameters
      integer inoise
!     dummy handle to a substate
      integer hsubstate
!     vectors to transfer data from the state to arrays and vice versa
      integer hvec1,hvec2,hvec3

!     array form of the covariance matrices for heat noise and boundary
!     noise
      double precision QHeat(NHEAT,NHEAT), Qbc(NBC,NBC), rdum
!     loop counters
      integer i,j,n
!     integer array used for creating vectors
      integer userdata(1)
!     variables used for computing the covariance values
      double precision x1, y1, x2, y2,vals(196)
      integer nperline, lastcol, j0
!     
!     maintain vectors for later use
!
      save hvec1, hvec2, hvec3

!     initialise handles to vectors
      data hvec1 /CTA_NULL/
      data hvec2 /CTA_NULL/
      data hvec3 /CTA_NULL/

!
!
!     Initialize covariance-matrices in their array form
!
!
!     for each heat parameter (from location)
      do i = 1,NHEAT
!        determine (x1,y1) of the from location
         x1 = mod(i-1,PNXH)*PDXH
         y1 = ((i-1)/PNXH)*PDYH
!        for each heat parameter (to location)
         do j = 1,NHEAT
!           determine (x2,y2) of the to location
            x2 = mod(j-1,PNXH)*PDXH
            y2 = ((j-1)/PNXH)*PDYH

!           covariance between from location and to location
!           decays exponentially with distance
!           Note: if decay is not strong enough, then not positive definite
!           --> Cholesky will not work!!!
            QHeat(i,j) = QdiagH * 
     +                         exp(-(x1-x2)**2d0/STDVXH**2
     +                             -(y1-y2)**2d0/STDVYH**2)
         end do
      end do
!
!     initialise the boundary condition covariance matrix
!
      do i = 1,NBC
         do j = 1,NBC
            Qbc(i,j) = 0d0
         end do
      end do
!
!     for each parameter in the x-direction (from location)
      do i = 1,PNXBC
!        determine x1, the x-coordinate of the from location
         x1 = (i-1)*pDxbc
!        for each parameter in the x-direction (to location)
         do j = 1,PNXBC
!           determine x2, the x-coordinate of the to location
            x2 = (j-1)*pDxbc
!           the covariance between the from location and the
!           to location decays exponentially
            Qbc(i,j) = Qdiagbc *
     +                 exp(-(x1-x2)**2d0/STDVXBC**2)
!           and the same for the other boundary along the x-direction
            Qbc(i+PNXBC+PNYBC,j+PNXBC+PNYBC) = Qbc(i,j) 
         end do
      end do

!     do the same for the boundary along the y-direction
      do i = 1,PNYBC
         y1 = (i-1)*pDybc
         do j = 1,PNYBC
            y2 = (j-1)*pDybc
            Qbc(i+PNXBC,j+PNXBC) = Qdiagbc *
     +                 exp(-(y1-y2)**2d0/STDVYBC**2)  
            Qbc(i+2*PNXBC+PNYBC,j+2*PNXBC+PNYBC) = Qbc(i+PNXBC,j+PNXBC) 
         end do
      end do
!
!
!
c     Compute Cholesky decomposition (root) of these matrices

  !    nperline = 6
  !    print *,'Qheat = zeros(',nheat,',',nheat,');'
  !    do j0 = 1,NHEAT,nperline
  !       lastcol=min(j0+nperline-1,NHEAT)
  !       print *,'Qheat(:,',j0,':',lastcol,')=['
  !       do i=1,NHEAT
  !          write(6,'(10F10.4)'), (QHeat(i,j),j=j0,lastcol)
  !       enddo
  !       print *,'];'
  !    end do

      call dposv( 'L', NHEAT, 0, QHeat, NHEAT, rdum, NHEAT, info)
      if (info.ne.0) stop 'heatstoch_compute3: error in dposv'

      call dposv( 'L', NBC, 0, Qbc, NBC, rdum, NBC, info)
      if (info.ne.0) stop 'heatstoch_compute4: error in dposv'



!     Create state vectors when necessary
!     
!
!     for each noise parameter
      do inoise=1,NPARAM
!        if we do not yet have a valid state vector for this parameter
         if (hstatevec(inoise).eq.CTA_NULL) then
!           create one
            call cta_treevector_duplicate(datblc(INDX_STATE),
     +         hstatevec(inoise), ierr)
            if (ierr.ne.CTA_OK) 
     +          stop 'heatstoch_getnoisecovar: Error duplicating memblk'
!           if hvec3 has not yet been created, create it an initialise to zero
            if (hvec3.eq.CTA_NULL) then
               call cta_treevector_getsize(hstatevec(inoise),n,ierr)
               call cta_vector_create(CTA_DEFAULT_VECTOR, n,
     +                                CTA_DOUBLE, userdata,hvec3,
     +                                ierr)
               if (ierr.ne.CTA_OK) 
     +             stop 'heatstoch_getnoisecovar: Error creating hvec3'
               call cta_vector_setconstant(hvec3,0.0d0,CTA_DOUBLE,ierr)
               if (ierr.ne.CTA_OK) 
     +          stop 'heatstoch_getnoisecovar: Error initialising hvec3'
            endif
!           fill the state with hvec3, effectively setting state to all zeros
            call cta_treevector_setvec(hstatevec(inoise),hvec3,ierr)
            if (ierr.ne.CTA_OK) 
     +      stop 'heatstoch_getnoisecovar: Error initialising hstatevec'
         endif
      enddo
!     if hvec1 has not yet been created in previous call of this routine,
!     create it; it will be used to hold the heat noise substate
      if (hvec1.eq.CTA_NULL) then
         call cta_vector_create(CTA_DEFAULT_VECTOR, NHEAT, CTA_DOUBLE,
     +                          userdata,hvec1, ierr)
      endif
!     the same for hvec2, which will be used to hold the boundary noise
!     substate
      if (hvec2.eq.CTA_NULL) then
         call cta_vector_create(CTA_DEFAULT_VECTOR, NBC, CTA_DOUBLE,
     +                          userdata, hvec2, ierr)
      endif
      
!     for each noise parameter inoise
      do inoise=1,NPARAM
!        if this is a heat parameter, fill the heat substate with Qheat(:,inoise)
         if (inoise.le.NHEAT) then
            call cta_treevector_getsubtreevec(hstatevec(inoise),
     +         'nheat', hsubstate,ierr) 
            if (ierr.ne.CTA_OK) 
     +       stop 'heatstoch_getnoisecovar: Error getting heat substate'
            call cta_vector_setvals(hvec1,Qheat(1,inoise),NHEAT,
     +                              CTA_DOUBLE,ierr)
            if (ierr.ne.CTA_OK) 
     +         stop 'heatstoch_getnoisecovar: Error setting hvec1'
            call cta_treevector_setvec(hsubstate, hvec1,ierr)
            if (ierr.ne.CTA_OK) 
     +       stop 'heatstoch_getnoisecovar: Error setting heat substate'
!        else, this is a boundary noise parameter, fill the boundary noise
!        substate with Qbc(:,inoise-NHEAT)
         else
            call cta_treevector_getsubtreevec(hstatevec(inoise), 
     +         'nbc', hsubstate,ierr) 
            if (ierr.ne.CTA_OK) 
     +      stop 'heatstoch_getnoisecovar: Error getting bound substate'
            call cta_vector_setvals(hvec2,Qbc(1,inoise-NHEAT),NBC,
     +                              CTA_DOUBLE,ierr)
            if (ierr.ne.CTA_OK) 
     +       stop 'heatstoch_getnoisecovar: Error setting hvec2'
            call cta_treevector_setvec(hsubstate, hvec2,ierr)
            if (ierr.ne.CTA_OK) 
     +      stop 'heatstoch_getnoisecovar: Error setting bound.substate'
         endif
      enddo

      ierr=CTA_OK
      end subroutine heatstoch_getnoisecovar

!
!------------------------------------------------------------------
!

      subroutine compute_covars(QHeat, Qbc)
      implicit none
      include 'cta_f77.inc'
      include 'heat_stoch.inc'
      include 'modelparam.inc'
      include 'stochparam.inc'

      double precision QHeat(nheat,nheat), Qbc(nbc,nbc)

      integer hQstate(nparam)
      integer inoise,ierr,hsubstate
      integer hvec1,hvec2,hvec3

      integer i,j,n
      integer userdata(1)
      double precision x1, y1, x2, y2,vals(196)
      save hvec1, hvec2, hvec3

      data hvec1 /CTA_NULL/
      data hvec2 /CTA_NULL/
      data hvec3 /CTA_NULL/

c
c     Initialize Q-matrices 
c
      do i = 1,nheat
         x1 = mod(i-1,PNXH)*PDXH
         y1 = ((i-1)/PNXH)*PDXH
         do j = 1,nheat
            x2 = mod(j-1,PNYH)*PDYH
            y2 = ((j-1)/PNYH)*PDYH

            QHeat(i,j) = QdiagH * 
     +                         exp(-(x1-x2)**2d0/stdvxH**2
     +                             -(y1-y2)**2d0/stdvyH**2)
         end do
      end do

      do i = 1,nbc
         do j = 1,nbc
            Qbc(i,j) = 0d0
         end do
      end do

      do i = 1,pnxbc
         x1 = (i-1)*pDxbc
         do j = 1,pnxbc
            x2 = (j-1)*pDxbc
            Qbc(i,j) = Qdiagbc *
     +                 exp(-(x1-x2)**2d0/stdvxbc**2)
            Qbc(i+pnxbc+pnybc,j+pnxbc+pnybc) = Qbc(i,j) 
         end do
      end do

      do i = 1,pnybc
         y1 = (i-1)*pDybc
         do j = 1,pnybc
            y2 = (j-1)*pDybc
            Qbc(i+pnxbc,j+pnxbc) = Qdiagbc *
     +                 exp(-(y1-y2)**2d0/stdvybc**2)  
            Qbc(i+2*pnxbc+pnybc,j+2*pnxbc+pnybc) = Qbc(i+pnxbc,j+pnxbc) 
         end do
      end do

      end subroutine compute_covars




!====================================================================
!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_getobsvalues
      subroutine heatstoch_getobsvalues(
     +           datblc,htime,hdescr,values,ierr)
!--------------------------------------------------------------------
!     This routine returns a vector with values predicted by the model
!     for the observation locations specified by hdescr
!
!     ARGUMENTS
!
!     integer datblc   I    memory block to store data related to this
!                             model instance; in this case: handle to the state
!     integer htime      I    CTA_Time object for the prediction-time 
!     integer hdescr     I    CTA_ObsDescr object, description of measurements
!     integer values     O    CTA_Vector object with predictions
!     integer ierr       O    result of this routine; cta_ok when succesful
!--------------------------------------------------------------------
      implicit none
      include "cta_f77.inc"
      include 'heat_stoch.inc'
      include "modelparam.inc"
      include 'stochparam.inc'
!
!     ARGUMENTS
      integer datblc(SIZE_DATABLOCK)
      integer htime 
      integer hdescr 
      integer values 
      integer ierr
!
!     LOCAL VARIABLES
      integer nmeasr  ! number of measurements
      integer imeasr  ! loop counter 1:nmeasr
      integer vN,vM   ! CTA_Vector objects with station (M,N) coordinates
      integer M,N     ! (M,N) coordinates of station
      integer idum    ! dummy input/output 
      integer hdeterm ! CTA_TreeVector object for temperatures of the state
      integer vdeterm ! CTA_Vector object for temperatures of the state
      double precision value ! Predicted value for one station
      double precision normdeterm

!     get number of observation points from hdescr
      call cta_obsdescr_observation_count(hdescr,nmeasr,ierr)
      if (ierr.ne.CTA_OK) stop 
     +  'heatstoch_getobsvalues: Error count observations'

!     create vector to hold N(=Y)-coordinates of observation points
      call cta_vector_create(CTA_DEFAULT_VECTOR, nmeasr, CTA_INTEGER,
     +                       idum, vN,ierr)
      if (ierr.ne.CTA_OK) stop 
     +  'heatstoch_getobsvalues: Error creating n-vector'

!     create vector to hold M(=X)-coordinates of observation points
      call cta_vector_create(CTA_DEFAULT_VECTOR, nmeasr, CTA_INTEGER,
     +                       idum, vM,ierr)
      if (ierr.ne.CTA_OK) stop 
     +  'heatstoch_getobsvalues: Error creating m-vector'

!     get N-values from the observation properties in the N-vector
      call cta_obsdescr_get_valueproperties(hdescr,'N',vN,CTA_INTEGER,
     +                       ierr)
      if (ierr.ne.CTA_OK) stop 
     +  'heatstoch_getobsvalues: Error getting n property'

!     get M-values from the observation properties in the M-vector
      call cta_obsdescr_get_valueproperties(hdescr,'M',vM,CTA_INTEGER,
     +                       ierr)
      if (ierr.ne.CTA_OK) stop 
     +  'heatstoch_getobsvalues: Error getting m property'
!
!
!     Get the temperatures from the state
!
!
      call cta_treevector_getsubtreevec(datblc(INDX_STATE),
     +   'determ', hdeterm, ierr)
      if (ierr.ne.CTA_OK) 
     +    stop 'heatstoch_getobsvalues: Error getting hdeterm'
      call cta_vector_create(CTA_DEFAULT_VECTOR, NMODEL, CTA_DOUBLE,
     +                       idum,vdeterm,ierr)
      if (ierr.ne.CTA_OK) 
     +    stop 'heatstoch_getobsvalues: Error creating vdeterm'
      call cta_treevector_getvec(hdeterm, vdeterm,ierr);
      if (ierr.ne.CTA_OK) 
     +    stop 'heatstoch_getobsvalues: Error getting vdeterm'

!     compute 2-norm of state vector
      call cta_vector_nrm2(vdeterm, normdeterm, ierr);
      if (ierr.ne.CTA_OK) 
     +    stop 'heatstoch_getobsvalues: Error in norm vdeterm'
!      print *,'norm of model state:',normdeterm

      do imeasr = 1,nmeasr
!
!     FOR (all measurements) DO
!
!        Get the coordinates of the station 
         call cta_vector_getval(vN,imeasr,N,CTA_INTEGER,ierr)
         if (ierr.ne.CTA_OK) 
     +       stop 'heatstoch_getobsvalues: Error getting n'
         call cta_vector_getval(vM,imeasr,M,CTA_INTEGER,ierr)
         if (ierr.ne.CTA_OK) 
     +       stop 'heatstoch_getobsvalues: Error getting m'
!
!        Get the observed value
         call cta_vector_getval(vdeterm,m+NX*(n-1),value,
     +                          CTA_DOUBLE,ierr)
         if (ierr.ne.CTA_OK) stop 
     +     'heatstoch_getobsvalues: Error getting interpolated value'

!        and put it in the measurement vector
         call cta_vector_setval(values,imeasr,value,CTA_DOUBLE,ierr)
         if (ierr.ne.CTA_OK) stop 
     +   'heatstoch_getobsvalues: Error putting interpolated value'
      end do

!     destroy vectors vN,vM and vdeterm
      call cta_vector_free(vN,ierr)
      if (ierr.ne.CTA_OK) 
     +    stop 'heatstoch_getobsvalues: Error freeing vN'
      call cta_vector_free(vM,ierr)
      if (ierr.ne.CTA_OK) 
     +    stop 'heatstoch_getobsvalues: Error freeing vM'
      call cta_vector_free(vdeterm,ierr)
      if (ierr.ne.CTA_OK) 
     +    stop 'heatstoch_getobsvalues: Error freeing vdeterm'

      end subroutine heatstoch_getobsvalues

!DEC$ ATTRIBUTES DLLEXPORT::heatstoch_addnoise
      subroutine heatstoch_addnoise(datblc,tspan,ierr)
      implicit none
      include 'heat_stoch.inc'
      integer datblc(SIZE_DATABLOCK)
      integer tspan,ierr

      integer state,tnoise
      
      state=datblc(INDX_STATE) 
      tnoise=datblc(INDX_TNOISE) 
      call cta_time_copy(tspan,tnoise,ierr)

      end subroutine heatstoch_addnoise


