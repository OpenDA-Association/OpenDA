!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/pollute2d/src/pollute2d.f90 $
! $Revision: 3216 $, $Date: 2012-01-30 17:19:10 +0100 (Mon, 30 Jan 2012) $
!
! COSTA: Problem solving environment for data assimilation
! Copyright (C) 2008  Nils van Velzen & Martin Verlaan
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

module pollute2d_params
!
! general parameters:
!
integer, parameter ::CONT       = 0
integer, parameter ::FINALIZE   = 1
integer, save      ::nmodels    = 0
logical, save      ::isparallel = .false.

!
! parameters of the model:
!
!grid
integer, parameter         ::m=30
integer, parameter         ::n=31
integer, parameter         ::nC=m*n
real(kind=8), dimension(m) ::config_parameters_x
real(kind=8)               ::config_parameters_dx
real(kind=8), dimension(n) ::config_parameters_y
real(kind=8)               ::config_parameters_dy
!
! parameters that can be used as control variables
!
! diffusion coefficient
real(kind=8),parameter ::config_parameters_avg_D        = 0.2
! concentration at inflow
real(kind=8),parameter ::config_parameters_avg_c_inflow = 0.0
! flowfield is modelled as a vortex with voricity concentrated in one point  (the center)
! location of center of vortex
real(kind=8),parameter ::config_parameters_avg_x_center   = 15      
real(kind=8),parameter ::config_parameters_avg_y_center   = 15
! angular velocity around center
real(kind=8),parameter ::config_parameters_avg_v_scale    = 0.9/15 
real(kind=8),parameter ::config_parameters_stdev_D        = 0.1      
real(kind=8),parameter ::config_parameters_stdev_c_inflow = 0.0      
! location of center of vortex
real(kind=8),parameter ::config_parameters_stdev_x_center = 0.0
real(kind=8),parameter ::config_parameters_stdev_y_center = 0.0
real(kind=8),parameter ::config_parameters_stdev_v_scale  = 0.0
!
!forcing with noise
!                 
! average value for discharge in sources
integer, parameter ::nSources=5

real(kind=8), dimension(nSources), parameter ::config_parameters_noise_mean_sources  = 2.0d0

! locations for sources
integer, dimension(nSources,2), parameter ::config_parameters_sourcepoints  = 5
! standard deviation for error in sources
real(kind=8), dimension(nSources), parameter ::config_parameters_noise_std_sources   = 2.0d0
! temporal correlation for sources
! note: 1.0 is timestep of model
real(kind=8), dimension(nSources), parameter ::config_parameters_T                   = 0.9d0
!
! time integration
!
real(kind=8), parameter ::config_time_timestep           = 1.0
! maximum length of simulation (eg. end of available forcing)
real(kind=8), parameter ::config_time_tstop              = 100.0 

!
! initialization
!
real(kind=8),                      parameter ::config_init_tstart             = 0.0d0
real(kind=8), dimension(m,n),      parameter ::config_init_state_mean_c       = config_parameters_avg_c_inflow
real(kind=8), dimension(nSources), parameter ::config_init_state_mean_sources = 0.01d0
real(kind=8), dimension(m,n),      parameter ::config_init_state_std_c        = 0.0
real(kind=8), dimension(nSources), parameter ::config_init_state_std_sources  = config_parameters_noise_std_sources

contains

subroutine pollute2d_params_init
implicit none
! Loop counters
integer ::i,j

do i=1,m
   config_parameters_x(i)=dble(i)
enddo
! Assume regular grid
config_parameters_dx=(config_parameters_x(2)-config_parameters_x(1)) 

do j=1,n
   config_parameters_y(j)=dble(j)
enddo
! Assume regular grid
config_parameters_dy=(config_parameters_y(2)-config_parameters_y(1)) 


end subroutine pollute2d_params_init

end module pollute2d_params


module pollute2d
use pollute2d_params


real(kind=8), dimension(:,:), allocatable ::u_adv
real(kind=8), dimension(:,:), allocatable ::v_adv
real(kind=8), dimension(:,:), allocatable ::xi
real(kind=8), dimension(:,:), allocatable ::yi


!Location of the sources (x,y)
!integer,      private, dimension(:,:), allocatable ::sources_locs  
!real(kind=8), private, dimension(:),   allocatable ::sources_mean  
!integer,      private                              ::m=0
!integer,      private                              ::n=0
real(kind=8), private                              ::c_inflow


contains


subroutine deltax_pollute2d(t,x,nlo,nup,userdat)
implicit none
include 'cta_f90.inc'
!% function/script         : function x=deltax_pollute2d(t,x,userdat)
!% purpose                 : this function contains the model kernel of the
!%                           pollute2d model
!% arguments               : t   - time
!%                           x   - state x(1:N) as flat vector
!%                           userdat - struct with userdat.xxx
!% output                  : dx   - dx/xt at this time and for this state
!% other requirements      : none
!% global changes          : none
!% author                  : M. Verlaan
!% license                 : GPL
!% log                     : 20061128 started with a copy of dx_advection1d
!
!%caching of velocity field avoids repeated computation
!Time to compute to
real(kind=8), intent(in) :: t
integer, intent(in)      :: x       !State of model instance
integer, intent(in)      ::nlo     !First colomn of local region to compute
integer, intent(in)      ::nup     !last colomn of local region to compute
integer                  :: userdat !Additional information

real(kind=8), dimension(:,:), allocatable ::c
real(kind=8), dimension(:,:), allocatable ::new_c
real(kind=8), dimension(:),   allocatable ::sources
!Loop indices
integer ::i, j, isource
! Sub-treevector of concentrations
integer ::sC
! Sub-treevector of sources
integer ::sSources
! Return code of COSTA method
integer ::ierr

real(kind=8), dimension(nSources) ::decay
logical      ::myIsNaN

decay           = exp(-config_time_timestep/  config_parameters_T)

! Get the values from the state-vector
call CTA_TreeVector_GetSubTreeVec(x, 'concentrations', sC,       ierr)
call CTA_TreeVector_GetSubTreeVec(x, 'sources',        sSources, ierr)

!call CTA_TreeVector_GetSize (sSources, nSources, ierr)

allocate(c(m,n))
allocate(new_c(m,n))
allocate(sources(nSources))

call CTA_TreeVector_GetVals (sC,       c,       nC, CTA_DOUBLE, ierr)
call CTA_TreeVector_GetVals (sSources, sources, nSources, CTA_DOUBLE, ierr)


!
! lagrangian advection
!
! use backward integrated tracks and interpolate from these points

if (.not. allocated(u_adv)) then
   allocate(u_adv(m,n))
   allocate(v_adv(m,n))
   allocate(xi(m,n))
   allocate(yi(m,n))
   
   call compute_flow_vortex(config_parameters_x, &
                            config_parameters_y, &
                            config_parameters_avg_x_center, &
                            config_parameters_avg_y_center, &
                            config_parameters_avg_v_scale, u_adv,v_adv)
   call integrate_flow_backwards(config_parameters_x, &
                                 config_parameters_y, &
                                 u_adv,v_adv,         &
                                 config_time_timestep,&
                                 xi,yi)
endif   

!now perform interpolation
!print *,'Calling myinterp2 from compute'
!print *,' config_parameters_x=',config_parameters_x
!print *,' config_parameters_y=',config_parameters_y
!print *,' xi=',xi
!print *,' yi=',yi

call myinterp2(config_parameters_x,config_parameters_y,c,xi,yi, nlo, nup,'linear',new_c)
!print *,'MAXVAL OF new_c na interp=',maxval(new_c)

!inflow boundary
do j=1,n
  do i=1,m
     if (myIsNaN(new_c(i,j))) new_c(i,j)=c_inflow
  enddo
enddo

! add sources
do isource=1,nSources
   i=config_parameters_sourcepoints(isource,1)
   j=config_parameters_sourcepoints(isource,2)
   new_c(i,j)=new_c(i,j)+max(sources(isource),0.0d0)
enddo
!print *,'MAXVAL OF new_c=',maxval(new_c)

! Update the source at next timestep
sources    = (sources-config_parameters_noise_mean_sources ) * decay + config_parameters_noise_mean_sources
!sources    = (sources-config_parameters_noise_mean_sources ) * 0.0d0 + config_parameters_noise_mean_sources


! Set new x-vector
call CTA_TreeVector_SetVals (sC,       new_c,       nC,       CTA_DOUBLE, ierr)
call CTA_TreeVector_SetVals (sSources, sources, nSources, CTA_DOUBLE, ierr)

deallocate(new_c)
deallocate(c)
deallocate(sources)

end subroutine deltax_pollute2d


subroutine compute_flow_vortex(x,y,xc,yc,omega, u_adv,v_adv)
! function/script         : function [u_adv,v_adv] = compute_flow_vortex(x,y,xc,yc,omega)
! purpose                 : compute flow field from the parameters
! arguments               : x - vector with x-coordinates of the grid
!                           y - vector with y-coordinates of the grid
!                           xc - x of center of the vortex
!                           yc - y of center of the vortex
!                           omega - angular velocity  
! output                  : 
! other requirements      : none
! global changes          : none
! author                  : M. Verlaan
! license                 : GPL
! log                     : 20061218 start

real(kind=8), dimension(:),   intent(in)  ::x
real(kind=8), dimension(:),   intent(in)  ::y
real(kind=8),                 intent(in)  ::xc
real(kind=8),                 intent(in)  ::yc
real(kind=8),                 intent(in)  ::omega
real(kind=8), dimension(:,:), intent(out) ::u_adv
real(kind=8), dimension(:,:), intent(out) ::v_adv

integer ::i,j

do j=1,size(y)
  do i=1,size(x)
     u_adv(i,j)=-omega*(y(j)-yc)
     v_adv(i,j)= omega*(x(i)-xc)
  enddo
enddo

end subroutine compute_flow_vortex

subroutine integrate_flow_backwards(x,y,u_adv,v_adv,dt, xi,yi)
! function/script         : function [xi,yi] = integrate_flow_backwards(x,y,u_adv,v_adv,dt)
! purpose                 : Backwards integration of particles with a stationary flow. This routine 
!                           is part of a Lagrangian advection scheme. The particles are initialized with
!                           the gridpoints.
! arguments               : x - vector with x-coordinates of the grid
!                           y - vector with y-coords of grid
!                           u_adv - matrix with x-component of flow 
!                           v_adv - ad u_adv but for y-component
!                           dt - lenght of integration in time
! output                  : xi - x-coords of particles after integration
!                           yi - dito for y
! other requirements      : none
! global changes          : none
! author                  : M. Verlaan
! license                 : GPL
! log                     : 20061213 started with algorithm from original model

real(kind=8), dimension(:),   intent(in)  ::x
real(kind=8), dimension(:),   intent(in)  ::y
real(kind=8), dimension(:,:), intent(in)  ::u_adv
real(kind=8), dimension(:,:), intent(in)  ::v_adv
real(kind=8),                 intent(in)  ::dt
real(kind=8), dimension(:,:), intent(out) ::xi
real(kind=8), dimension(:,:), intent(out) ::yi

real(kind=8), dimension(:,:), allocatable ::dxi, dyi
real(kind=8) , dimension (1) ::x_max, x_min, y_max, y_min
real(kind=8) ::dt_sub, t
integer      ::istep
logical      ::myIsNaN

!find boundaries (assume rectangular)
x_max = maxval(x)
x_min = minval(x)
y_max = maxval(y)
y_min = minval(y)

!backward advection scheme (called in init because flow is stationary)
!backward advect velocity points
!make 10 substeps 
dt_sub=0.1*dt 

allocate(dxi(size(x),size(y)))
allocate(dyi(size(x),size(y)))
do j=1,size(y)
   do i=1,size(x)
      xi(i,j)=x(i)
      yi(i,j)=y(j)
   enddo
enddo

do istep=1,10
   t=istep*dt_sub
!   print *,'Interp dxi for istep=',istep
   call myinterp2(x,y,u_adv,xi,yi,1,n,'linear', dxi)
!   print *,'Interp dyi for istep=',istep
   call myinterp2(x,y,v_adv,xi,yi,1,n,'linear', dyi)

   do j=1,n
      do i=1,m
         if (myIsNaN(dxi(i,j))) dxi(i,j)=0.0d0
         if (myIsNaN(dyi(i,j))) dyi(i,j)=0.0d0
      enddo
   enddo

   do j=1,n
     do i=1,m
        xi(i,j) = xi(i,j) - dxi(i,j)*dt_sub
        yi(i,j) = yi(i,j) - dyi(i,j)*dt_sub
        xi(i,j) = max(xi(i,j),x_min(1))
        xi(i,j) = min(xi(i,j),x_max(1))
        yi(i,j) = max(yi(i,j),y_min(1))
        yi(i,j) = min(yi(i,j),y_max(1))
     enddo
   enddo
enddo

deallocate(dxi)
deallocate(dyi)


end subroutine integrate_flow_backwards



subroutine myinterp2(x,y,z,xi,yi,nlo,nup,method,zi)
!% function/script         : function zi=myinterp2(x,y,z,xi,yi,method)
!% purpose                 : two dimensional interpolation on a regular grid
!% arguments               : x   - vector with x coordinates
!%                           y   - vector with y coordinates
!%                           z   - matrix with values correponding to the grid given by x and y
!%                           xi  - matrox/vector with requested interpolation locations : x-values
!%                           yi  - same but for y coordinates
!%                           method - nearest or linear - default is linear
!% output                  : interpolated values
!% other requirements      : none
!% global changes          : none
!% author                  : M. Verlaan
!% license                 : GPL
!% log                     : 20061127 modified from interp1

!%checks
real(kind=8), dimension(:), intent(in) ::x,y
real(kind=8), dimension(:,:), intent(in) ::z
real(kind=8), dimension(:,:), intent(in) ::xi
real(kind=8), dimension(:,:), intent(in) ::yi
integer,                      intent(in) ::nlo
integer,                      intent(in) ::nup
character(len=*),           intent(in) ::method
real(kind=8), dimension(:,:), intent(out) ::zi


real(kind=8) ::NaN

NaN=0.0d0
NaN=1.0d0/NaN
NaN=NaN/NaN

if (size(x)/=size(z,1) .or. size(y)/=size(z,2)) then 
   print *, 'FATAL ERROR IN myinterp2  size of x is not matching size of z'
   call exit(-1)
endif

!print *,'myinterp2', NaN
!print *,'z=',z(1:5,1:5)
zi = NaN

!do j=1,size(xi,dim=2)

do j=nlo,nup   
   do i=1,size(xi,dim=1)
      this_x = xi(i,j)
      this_y = yi(i,j)

      ind_x  = count(x<=this_x)
      ind_y  = count(y<=this_y)

      if( (ind_x>0) .and. (ind_x<size(x)) .and. (ind_y>0) .and. (ind_y<size(y)) ) then
         alpha_x    = (this_x-x(ind_x))/(x(ind_x+1)-x(ind_x))
         alpha_y    = (this_y-y(ind_y))/(y(ind_y+1)-y(ind_y))
      endif
      if (trim(method)=='nearest') then
              alpha_x =  dble((nint(alpha_x)))
          alpha_y =  dble((nint(alpha_y)))
      endif
      
      !Is this correct since we cannot extrapolate
      ind_xup = min(ind_x+1,size(x))
      ind_yup = min(ind_y+1,size(y))
      
      zi(i,j) = (1-alpha_y) * (  alpha_x*z(ind_xup,ind_y)   + (1-alpha_x)*z(ind_x,ind_y)    ) &
            +    alpha_y  * (  alpha_x*z(ind_xup,ind_yup) + (1-alpha_x)*z(ind_x,ind_yup)  )
   enddo
enddo


!print *,'zi=',zi(1:5,1:5)
!pause 'druk op een toets'

end subroutine myinterp2


subroutine par_dims_info(nproc, iproc, nlo, nup, nSourcesLoc, sourceIndx)
implicit none

integer, intent(in)  ::nproc
integer, intent(in)  ::iproc
integer, intent(out) ::nlo
integer, intent(out) ::nup
integer, intent(out) ::nSourcesLoc
integer, dimension(:), intent(out) ::sourceIndx

integer ::dn
integer ::iSource


   dn=n/nproc
   nlo=dn*(iproc)+1
   nup=dn*(iproc+1)
   if ((iproc+1)==nproc) nup=n   
 
   nSourcesLoc=0
   do iSource=1,nSources
      if (config_parameters_sourcepoints(iSource,2)>=nlo .and. config_parameters_sourcepoints(iSource,2)<=nup) then
         nSourcesLoc=nSourcesLoc+1
         sourceIndx(nSourcesLoc)=iSource
      endif
   enddo



end subroutine par_dims_info



subroutine create_state_vector(nproc, iproc, state, ierr)
implicit none
include 'cta_f77.inc'
!Header variables
integer, intent(in)  ::nproc
integer, intent(in)  ::iproc
integer, intent(out) ::state
integer, intent(out) ::ierr

!Local variables
! Sub-(tree)vector of concentrations
integer ::sC, vC
! Sub-(tree)vector of sources
integer ::sSources, vSources
! Array of sub-treeVectors
integer, dimension(2) ::subVecs

integer ::mLoc, nLoc, nCLoc, nSourcesLoc
integer ::nlo, nup
integer, dimension(nSources) ::sourceIndx
integer ::hdescr
character(len=16) ::nameRoot

  ! print *,' nproc=',nproc,' iproc=',iproc
   call par_dims_info(nproc, iproc, nlo, nup, nSourcesLoc, sourceIndx)

   ! Compute size of concentration field
   mLoc=m
   nLoc=nup-nlo+1
   nCLoc=mLoc*nLoc 

  ! print *,'mLoc=',mLoc,' nLoc=',nLoc,' nCLoc=',nCLoc ,' nSourcesLoc=',nSourcesLoc


   !Create a state-vector

   !In a parallel run make the name of the root unique
   write(nameRoot,'(''pollute2d_'',i2.2)') iproc
   call CTA_TreeVector_Create(nameRoot,nameRoot,state,    ierr); if (ierr/=CTA_OK) goto 9999
   call CTA_TreeVector_Create('concentrations',  'concentrations',   sC,       ierr); if (ierr/=CTA_OK) goto 9999
   call CTA_TreeVector_Create('sources',  'sources',  sSources, ierr); if (ierr/=CTA_OK) goto 9999

   call CTA_Vector_Create(CTA_DEFAULT_VECTOR, nCLoc,       CTA_DOUBLE, CTA_NULL, vC,       ierr); if (ierr/=CTA_OK) goto 9999
   call CTA_Vector_Create(CTA_DEFAULT_VECTOR, nSourcesLoc, CTA_DOUBLE, CTA_NULL, vSources, ierr); if (ierr/=CTA_OK) goto 9999
   
   call CTA_Vector_SetConstant(vC,       config_init_state_mean_c, CTA_DOUBLE, ierr); if (ierr/=CTA_OK) goto 9999
   call CTA_Vector_SetConstant(vSources, 0.0d0,                    CTA_DOUBLE, ierr); if (ierr/=CTA_OK) goto 9999

   call CTA_TreeVector_SetVec (sC,       vC,       ierr); if (ierr/=CTA_OK) goto 9999
   call CTA_TreeVector_SetVec (sSources, vSources, ierr); if (ierr/=CTA_OK) goto 9999
   
   subVecs(1)=sC
   subVecs(2)=sSources
   call CTA_TreeVector_Conc (state, subVecs, 2, ierr); if (ierr/=CTA_OK) goto 9999
 
   ! set meta information for the deterministic state
   call cta_metainfo_create(hdescr,ierr); if (ierr/=CTA_OK) goto 9999
   call cta_metainfo_setreggrid(hdescr, 'grid', mLoc, nLoc, 0,  &
                                0.0d0, 0.0d0, 0.0d0,            &
                                1.0d0, 1.0d0, 0.0d0, ierr)
   if (ierr/=CTA_OK) goto 9999
   call cta_treevector_setmetainfo(sC, hdescr, ierr);
   if (ierr/=CTA_OK) goto 9999

   return
   9999 continue
   print *,'FATAL ERROR IN create_state_vector ierr=',ierr
   call exit(-1)


end subroutine create_state_vector

subroutine get_par_info(nproc,iproc)
use pollute2d_params, only: isparallel
implicit none
include 'cta_f77.inc'
integer, intent(out) ::nproc
integer, intent(out) ::iproc


integer ::cta_comm_world, cta_comm_myworld, cta_comm_master_worker
integer ::ierr

   if (isparallel) then

      call CTA_Par_GetFComm(cta_comm_world, cta_comm_myworld, cta_comm_master_worker)

      call MPI_COMM_SIZE(cta_comm_myworld, nproc, ierr)
      call MPI_COMM_RANK(cta_comm_myworld, iproc, ierr)
   else
      nproc=1
      iproc=0
   endif


   return
   9999 continue
   print *,'FATAL (MPI) ERROR IN get_par_info ierr=',ierr
   call exit(-1)


end subroutine get_par_info



subroutine communicate_state_vector_mw(state, from_master)
implicit none
include 'cta_f77.inc'
include 'mpif.h'
integer, intent(in) ::state
logical, intent(in) ::from_master

real(kind=8), dimension (:), allocatable ::x
integer ::nstate, nsend, iproc, nproc, nlo, nup

! Communicators
integer ::cta_comm_world, cta_comm_myworld, cta_comm_master_worker

integer, dimension(2) ::irange

integer ::ierr
integer, parameter ::tag=99

logical, parameter ::WHOLE_GLOBAL_STATE=.false.
integer, dimension(nSources) ::sourceIndx
integer ::nSourcesLoc


   ! Get communicators
   call CTA_Par_GetFComm(cta_comm_world, cta_comm_myworld, cta_comm_master_worker)
   call get_par_info(nproc,iproc)
   call par_dims_info(nproc, iproc, nlo, nup, nSourcesLoc, sourceIndx)

   ! We are lazy and send whole state to all processes since this model is only
   ! used as a proof of concept

   call cta_treevector_getsize(state,nstate,ierr)
   if (ierr.ne.0) goto 9999
   allocate(x(nstate))

   if (from_master) then
     if (iproc==0) then
       call CTA_TreeVector_GetVals(state,x,nstate,CTA_DOUBLE, ierr)
       if (ierr.ne.0) goto 9999
       do iproc=2,nproc
          call MPI_SEND(x,    nstate, MPI_DOUBLE_PRECISION, iproc-1, tag, cta_comm_myworld, ierr)
          if (ierr.ne.0) goto 9998
       enddo
     else 
          call MPI_RECV(x,        nstate, MPI_DOUBLE_PRECISION, 0, tag, cta_comm_myworld, MPI_STATUS_IGNORE, ierr)
          if (ierr.ne.0) goto 9998
          call CTA_TreeVector_SetVals(state,x,nstate,CTA_DOUBLE, ierr)
     endif
   else
     call CTA_TreeVector_GetVals(state,x,nstate,CTA_DOUBLE, ierr)
     if (ierr.ne.0) goto 9999
     if (iproc==0) then
       do iproc=2,nproc
          call MPI_RECV(irange, 2, MPI_INTEGER, iproc-1, tag, cta_comm_myworld, MPI_STATUS_IGNORE, ierr)
          if (ierr.ne.0) goto 9998
          nsend=irange(2)-irange(1)+1
          call MPI_RECV(x(irange(1)), nsend, MPI_DOUBLE_PRECISION, iproc-1, tag, cta_comm_myworld, MPI_STATUS_IGNORE, ierr)
          if (ierr.ne.0) goto 9998
       enddo
       call CTA_TreeVector_SetVals(state,x,nstate,CTA_DOUBLE, ierr)
       if (ierr.ne.0) goto 9999
     else 
       ! Send range
       irange(1)=(nlo-1)*m+1
       irange(2)=(nup)*m
       call MPI_SEND(irange, 2, MPI_INTEGER, 0, tag, cta_comm_myworld, ierr)
       if (ierr.ne.0) goto 9998
       nsend=irange(2)-irange(1)+1
       call MPI_SEND(x(irange(1)), nsend, MPI_DOUBLE_PRECISION, 0, tag, cta_comm_myworld, ierr)
       if (ierr.ne.0) goto 9998
     endif
   endif


   deallocate(x)

   return

   9998 continue
   print *,'FATAL (MPI) ERROR IN communicate_state_vector_mw ierr=',ierr
   call exit(-1)

   9999 continue
   print *,'FATAL ERROR IN communicate_state_vector_mw ierr=',ierr
   call exit(-1)


end subroutine communicate_state_vector_mw



subroutine communicate_state_vector(stateLoc, stateGlob, only_copy_to_local)
implicit none
include 'cta_f77.inc'
include 'mpif.h'
integer, intent(in) ::stateLoc
integer, intent(in) ::stateGlob
logical, intent(in) ::only_copy_to_local

! Tree-vector handles
integer ::sCLoc, sSourcesLoc, sCGlob, sSourcesGlob

! Dimensions
integer ::nSourcesLoc
integer ::nlo, nup
integer, dimension(nSources) ::sourceIndx
real(kind=8), dimension(nSources) ::sourcesGlob
integer ::nproc, iproc
integer ::nCLoc, nCGlob
real(kind=8), dimension (:), allocatable ::cLoc, cGlob
real(kind=8)                 ::source
real(kind=8), dimension(nSources) ::sources

integer ::indx1, indx2, i

integer, dimension(:), allocatable ::recvcounts,displs

! Communicators
integer ::cta_comm_world, cta_comm_myworld, cta_comm_master_worker

integer ::ierr
integer ::isweep
integer, parameter ::tag=99
logical, parameter ::WHOLE_GLOBAL_STATE=.false.


   call CTA_Par_GetFComm(cta_comm_world, cta_comm_myworld, cta_comm_master_worker)
   call get_par_info(nproc,iproc)
   call par_dims_info(nproc, iproc, nlo, nup, nSourcesLoc, sourceIndx)

   ! We start with a lazy approach we make shure everybody has a global
   ! copy of the state-vector

   call CTA_TreeVector_GetSubTreeVec(stateLoc,  'concentrations', sCLoc,        ierr)
   call CTA_TreeVector_GetSubTreeVec(stateLoc,  'sources',        sSourcesLoc,  ierr)
   call CTA_TreeVector_GetSubTreeVec(stateGlob, 'concentrations', sCGlob,       ierr)
   call CTA_TreeVector_GetSubTreeVec(stateGlob, 'sources',        sSourcesGlob, ierr)

   nCLoc  = m*(nup-nlo+1)
   nCGlob = m*n
   allocate(cLoc(nCLoc))
   allocate(cGlob(nCGlob))
   allocate(recvcounts(nproc))
   allocate(displs(nproc))

   ! Copy the local elements of the global state-vector to the local
   ! state-vector (NO COMMUNICATION)
   if (only_copy_to_local) then
      ! Get Concentrations from the global state
      call CTA_TreeVector_GetVals(sCGlob,cGlob,nCGlob,CTA_DOUBLE, ierr)
      if (ierr.ne.CTA_OK) goto 9999
      ! Copy Concentrations to local state
      indx1=(nlo-1)*m+1  !Index to first element of local domain
      call CTA_TreeVector_SetVals(sCLoc,cGlob(indx1),nCLoc,CTA_DOUBLE, ierr)
      if (ierr.ne.CTA_OK) goto 9999

      ! Copy all sources
      do i=1,nSourcesLoc
         indx1=sourceIndx(i)
         call CTA_TreeVector_GetVal (sSourcesGlob, indx1, source, CTA_DOUBLE, ierr)
         if (ierr.ne.CTA_OK) goto 9999
         call CTA_TreeVector_SetVal (sSourcesLoc,  i,     source, CTA_DOUBLE, ierr)
         if (ierr.ne.CTA_OK) goto 9999
      enddo
   else

      if (.not. WHOLE_GLOBAL_STATE) then
         ! Get concentrations from local state vectors
         call CTA_TreeVector_GetVals(sCLoc,cLoc,nCLoc,CTA_DOUBLE, ierr)
         if (ierr.ne.CTA_OK) goto 9999

         indx1=(nlo-1)*m+1
         indx2=nup*m
         cGlob(indx1:indx2)=cLoc

         do isweep=1,2
            if (mod(iproc+isweep,2)==0) then
               ! Send to left
               if (iproc>0) then
                  call MPI_SEND(cLoc(1:m), m, MPI_DOUBLE_PRECISION, iproc-1, tag, cta_comm_myworld, ierr)
                  if (ierr.ne.0) goto 9998
               endif
               ! Send to right
               indx1=(nup-nlo)*m+1
               if (iproc+1<nproc) then
                  call MPI_SEND(cLoc(indx1:indx1+m-1), m, MPI_DOUBLE_PRECISION, iproc+1, tag, cta_comm_myworld, ierr)
                  if (ierr.ne.0) goto 9998
               endif
            else
               ! Receive from right
               indx1=nup*m+1
               if (iproc+1<nproc) then
                  call MPI_RECV(cGlob(indx1:indx1+m-1), m, &
                     MPI_DOUBLE_PRECISION, iproc+1, tag, cta_comm_myworld, &
                     MPI_STATUS_IGNORE, ierr)
                  if (ierr.ne.0) goto 9998
               endif

               ! Receive from left
               indx1=(nlo-2)*m+1
               if (iproc>0) then
                  call MPI_RECV(cGlob(indx1:indx1+m-1), m, &
                     MPI_DOUBLE_PRECISION, iproc-1, tag, cta_comm_myworld, &
                     MPI_STATUS_IGNORE, ierr)
                  if (ierr.ne.0) goto 9998
               endif
            endif
         enddo
      else
       
         ! Get concentrations from local state vectors
         call CTA_TreeVector_GetVals(sCLoc,cLoc,nCLoc,CTA_DOUBLE, ierr)
         if (ierr.ne.CTA_OK) goto 9999

         ! Receive number of elements of each process
         call MPI_ALLGATHER(nCLoc,      1,     MPI_INTEGER, &
                            recvcounts, 1, MPI_INTEGER, &
                            cta_comm_myworld, ierr)
         ! Set displacement array
         displs(1)=0
         do i=1,nproc-1
            displs(i+1)=displs(i)+recvcounts(i)
         enddo

         ! Gather all elements of concentrations the state
         call MPI_ALLGATHERV(cLoc,  nCLoc,  MPI_DOUBLE_PRECISION, &
                            cGlob,  recvcounts,displs,MPI_DOUBLE_PRECISION, &
                            cta_comm_myworld, ierr)
         if (ierr.ne.0) goto 9998

      endif

      ! Create a global vector of sources and fill only own locations
      sources=0.0d0
      do i=1,nSourcesLoc
         call CTA_TreeVector_GetVal (sSourcesLoc,  i, source, CTA_DOUBLE, ierr)
         if (ierr.ne.CTA_OK) goto 9999
         indx1=sourceIndx(i)
         sources(indx1)=source
      enddo


      ! Sum global sources (is same as gather)
      call MPI_ALLREDUCE(sources, sourcesGlob, nSources, MPI_DOUBLE_PRECISION, MPI_SUM, cta_comm_myworld, ierr)
      if (ierr.ne.0) goto 9998

      ! Set values in global arrays
      call CTA_TreeVector_SetVals(sCGlob,cGlob,nCGlob,CTA_DOUBLE, ierr)
      if (ierr.ne.CTA_OK) goto 9999
      call CTA_TreeVector_SetVals(sSourcesGlob,sourcesGlob,nSources,CTA_DOUBLE, ierr)
      if (ierr.ne.CTA_OK) goto 9999
   endif

   ! Free workspace
   deallocate(cLoc)
   deallocate(cGlob)
   deallocate(recvcounts)
   deallocate(displs)
   return

   9998 continue
   print *,'FATAL (MPI) ERROR IN communicate_state_vector ierr=',ierr
   call exit(-1)

   9999 continue
   print *,'FATAL ERROR IN communicate_state_vector ierr=',ierr
   call exit(-1)


end subroutine communicate_state_vector















end module pollute2d




!---------------------------------------------------------------------


!Initialize a model-instance:
subroutine pollute2d_create(hinput, state, sbound, sparam, nnoise, &
                     tHorizon, snamnoise, husrdata, ierr, parWW)
use pollute2d_params
use pollute2d, only: create_state_vector, &
                     get_par_info, nmodels

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
integer tHorizon  !(O) costa time object met de tijdsspanne van de simulatie
integer snamnoise !(O) indien nnoise>0 een costa-string met de naam van de
                  !    (sub)-state waarin alle noise zit
integer husrdata  !(O) hier kan je zelf een costa-tree in stoppen met extra
                  !    informatie die specifiek is voor deze instantiatie.
                  !    Deze info word aan alle functies doorgegeven. Met een
                  !    beetje geluk heb je deze niet nodig en zet je hem op
                  !    CTA_NULL 
integer ierr      !(O)  Als iets fout gegaan is zet je hem op een waarde
                  !     ongelijk CTA_OK.
logical parWW     !(I) parallel model using Worker Worker concept                  

real(kind=8), dimension(2) ::x0
real(kind=8), dimension(3,2) ::noise
integer ::vx

real(kind=8), dimension(2) ::p0    !:      model parameters 
                                   !       param(1)=characteristic time-scale for friction [seconds]
                                   !       param(2)=oscillation frequency [rad/s]
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer ::nproc, iproc


   ! Determine whether we run sequential or parallel
   call mpi_initialized(isparallel,ierr)
 
   !Init global constants
   call pollute2d_params_init

   !Increase the model counter
   nmodels=nmodels+1

   if (parWW) then
      print *, 'Creating a pollute2d model instance in Worker-Worker mode'

      !Worker-Worker: My state is sub-domain and I have whole state for
      !computations

      !Create a local  state-vector
      call get_par_info(nproc,iproc)
      call create_state_vector(nproc, iproc, state, ierr)

      !Create a global state-vector
      call create_state_vector(1, 0, husrdata, ierr)
   else
      print *, 'Creating a pollute2d model instance in Master-Worker mode'
      !Master-Worker: My state is the whole state     
      call create_state_vector(1, 0, state, ierr)
   endif
   
   sbound    = CTA_NULL
   sparam    = CTA_NULL
   nnoise    = nSources
   call CTA_Time_SetSpan(tHorizon,0.0d0,10.0d0,ierr); if (ierr/=CTA_OK) goto 9999
   call cta_string_set(snamnoise,'pollute2d',ierr)


   ierr=CTA_OK
   return
   9999 continue
   print *,'FATAL ERROR IN pollute2d_create ierr=',ierr
   call exit(-1)

end subroutine pollute2d_create

!Perform one or more time steps
subroutine pollute2d_compute(timespan,state, saxpyforc, baddnoise, &
                      sparam, husrdata, ierr, parWW)
use pollute2d_params, only:config_parameters_noise_std_sources, &
                           nSources, &
                           isparallel
use pollute2d,        only:deltax_pollute2d, &
                           communicate_state_vector, &
                           communicate_state_vector_mw, &
                           get_par_info, &
                           par_dims_info

!use oscill
!use ode
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
logical parWW       !:(I) parallel model using Worker Worker concept                  

real(kind=8) ::xRand, xSource
integer      ::isource
integer      ::sSources
integer      ::stateGlob, stateLoc

integer ::nproc,iproc
integer ::mLoc, nLoc, nSourcesLoc
integer ::nlo, nup
integer, dimension(nSources) ::sourceIndx

   ! Get domain information   
   call get_par_info(nproc,iproc)
   call par_dims_info(nproc, iproc, nlo, nup, nSourcesLoc, sourceIndx)

   if (isparallel) then

      if (parWW) then
         ! First send message that we are continuing performing timesteps     
         stateGlob = husrdata
         stateLoc  = state

         ! Communicate state local to global
         call communicate_state_vector(stateLoc, stateGlob, .false.)

      else

         stateGlob = state
         stateLoc  = state

         ! Communicate global state to worker processes
         call communicate_state_vector_mw(stateLoc, .true.)

      endif
   else
      stateGlob = state
      stateLoc  = state
   endif        

   !Perform the computations
   call deltax_pollute2d(0.0d0,stateGlob,nlo,nup,CTA_NULL)

   ! Add noise !NOTE we add it to the GLOBAL state!
   if (baddnoise == CTA_TRUE) then
      call CTA_TreeVector_GetSubTreeVec(stateGlob, 'sources', sSources, ierr)
      if (ierr/=CTA_OK) goto 9999
      
      do isource=1,nSourcesLoc
         !Generate random number:
         call CTA_Rand_n(xRand, ierr)
         if (ierr/=CTA_OK) goto 9999
         xRand=xrand*config_parameters_noise_std_sources(sourceIndx(isource))
         call CTA_TreeVector_GetVal (sSources, sourceIndx(isource), xSource, CTA_DOUBLE, ierr)
         if (ierr/=CTA_OK) goto 9999
         xSource=max(xSource+xRand,0.0d0)
         call CTA_TreeVector_SetVal (sSources, sourceIndx(isource), xSource, CTA_DOUBLE, ierr)
         if (ierr/=CTA_OK) goto 9999
      enddo
   endif

   if (isparallel) then
      if (parWW) then

         ! Copy data from global state to local 
         call communicate_state_vector(stateLoc, stateGlob, .true.)

      else

!      ! Communicate global state to worker processes
         call communicate_state_vector_mw(stateLoc, .false.)

      endif
   endif
   ierr=CTA_OK
   return
   9999 continue
   print *,'FATAL ERROR IN pollute2d_compute ierr=',ierr
   call exit(-1)


end subroutine pollute2d_compute

!Interpolatie naar de observaties
subroutine pollute2d_obs(state ,hdescr, vval, husrdata, ierr, parWW)
use pollute2d_params, only:m,n, nSources
use pollute2d,        only:get_par_info, &
                           par_dims_info
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
logical parWW   ! :(I) parallel model using Worker Worker concept                  

integer      ::nObs, vX, vY, indx, ix, iy, sC, iObs
real(kind=8) ::val

integer ::nproc,iproc
integer ::mLoc, nLoc, nSourcesLoc
integer ::nlo, nup
integer, dimension(nSources) ::sourceIndx

   ! Get domain information   
   call get_par_info(nproc,iproc)
   call par_dims_info(nproc, iproc, nlo, nup, nSourcesLoc, sourceIndx)

   ! Set range for global domain
   if (.not. parWW) then
      nlo=1
      nup=n
   endif

   call CTA_ObsDescr_Observation_Count (hdescr, nObs, ierr)
   if (ierr/=CTA_OK) goto 9999

   call CTA_Vector_Create(CTA_DEFAULT_VECTOR, nObs, CTA_INTEGER, CTA_NULL, vX, ierr)
   if (ierr/=CTA_OK) goto 9999
   call CTA_Vector_Create(CTA_DEFAULT_VECTOR, nObs, CTA_INTEGER, CTA_NULL, vY, ierr)
   if (ierr/=CTA_OK) goto 9999

   call CTA_ObsDescr_Get_ValueProperties (hdescr, 'X', vX, CTA_INTEGER, ierr)
   if (ierr/=CTA_OK) goto 9999

   call CTA_ObsDescr_Get_ValueProperties (hdescr, 'Y', vY, CTA_INTEGER, ierr)
   if (ierr/=CTA_OK) goto 9999

   call CTA_TreeVector_GetSubTreeVec(state, 'concentrations', sC, ierr)
   if (ierr/=CTA_OK) goto 9999

!   print *,'Worker:',iproc,'X-coordinates are:'
!   do iObs=1,nObs
!      call CTA_Vector_GetVal (vX, iObs, ix, CTA_INTEGER, ierr)
!      print *,'ix(',iObs,')=',ix
!   enddo 
!   print *,'Worker:',iproc,'X-coordinates are:'
!   do iObs=1,nObs
!      call CTA_Vector_GetVal (vY, iObs, iy, CTA_INTEGER, ierr)
!      print *,'iy(',iObs,')=',iy
!   enddo 


   do iObs=1,nObs
      call CTA_Vector_GetVal (vX, iObs, ix, CTA_INTEGER, ierr)
      if (ierr/=CTA_OK) goto 9999
      call CTA_Vector_GetVal (vY, iObs, iy, CTA_INTEGER, ierr)
      if (ierr/=CTA_OK) goto 9999

      indx=m*(iy-1)+ix -(nlo-1)*m

      call CTA_TreeVector_GetVal (sC, indx, val, CTA_DOUBLE, ierr)
      if (ierr/=CTA_OK) goto 9999
      call CTA_Vector_SetVal (vval, iObs, val, CTA_DOUBLE, ierr)
      if (ierr/=CTA_OK) goto 9999
   enddo


   call CTA_Vector_Free(vX, ierr)
   if (ierr/=CTA_OK) goto 9999
   call CTA_Vector_Free(vY, ierr)
   if (ierr/=CTA_OK) goto 9999

   ierr=CTA_OK
   return
   9999 continue
   print *,'FATAL ERROR IN pollute2d_obs ierr=',ierr
   call exit(-1)


end subroutine pollute2d_obs

!Provide the covariantie matrix of the noise parameters (only for
!stochastic models.
subroutine pollute2d_covar(colsvar, nnoise, husrdata,ierr, parWW)
use pollute2d_params
use pollute2d,        only:get_par_info, &
                           par_dims_info



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
logical parWW       !(I) parallel model using Worker Worker concept                  

integer inoise
integer subNoise
integer nproc,iproc,nlo,nup,nSourcesLoc,icol
integer, dimension(nSources) ::sourceIndx
real(kind=8) ::sigma2

   do inoise=1,nnoise
      call CTA_TreeVector_SetConstant (colsvar(inoise), 0.0d0 , CTA_DOUBLE, ierr); if (ierr/=CTA_OK) goto 9999
   enddo

   if (parWW) then
      !Only set my noise parameters

      ! Get domain information   
      call get_par_info(nproc,iproc)
      call par_dims_info(nproc, iproc, nlo, nup, nSourcesLoc, sourceIndx)
      
      do inoise=1,nSourcesLoc
        icol=sourceIndx(inoise)
        sigma2=config_parameters_noise_std_sources(icol)
        call CTA_TreeVector_GetSubTreeVec(colsvar(icol),'sources', subNoise, ierr); if (ierr/=CTA_OK) goto 9999
        call cta_TreeVector_SetVal(subNoise, inoise, sigma2 ,CTA_DOUBLE, ierr); if (ierr/=CTA_OK) goto 9999
     enddo
   else
     do inoise=1,nnoise
        call CTA_TreeVector_GetSubTreeVec(colsvar(inoise), 'sources', subNoise, ierr)
        call cta_TreeVector_SetVal(subNoise, inoise, &
           config_parameters_noise_std_sources(inoise) ,CTA_DOUBLE, ierr)
           if (ierr/=CTA_OK) goto 9999
     enddo
   endif
   ierr=CTA_OK
   return
   9999 continue
   print *,'FATAL ERROR IN pollute2d_covar ierr=',ierr
   call exit(-1)

end subroutine pollute2d_covar


!Initialize a model-instance:
!#ifdef WIN32_IFORT
    !DEC$ ATTRIBUTES DLLEXPORT::pollute2d_ww_create
!#endif
subroutine pollute2d_ww_create(hinput, state, sbound, sparam, nnoise, &
                     tHorizon, snamnoise, husrdata, ierr)
implicit none
integer hinput, state, sbound, sparam, nnoise, tHorizon, snamnoise, husrdata, ierr      

   call pollute2d_create(hinput, state, sbound, sparam, nnoise, &
                         tHorizon, snamnoise, husrdata, ierr, .true.)


end subroutine pollute2d_ww_create

!#ifdef WIN32_IFORT
    !DEC$ ATTRIBUTES DLLEXPORT::pollute2d_ww_compute
!#endif
subroutine pollute2d_ww_compute(timespan,state, saxpyforc, baddnoise, &
                      sparam, husrdata, ierr)
implicit none
integer timespan, state, saxpyforc, baddnoise, sparam, husrdata, ierr      

   call pollute2d_compute(timespan,state, saxpyforc, baddnoise, &
                             sparam, husrdata, ierr, .true.)

end subroutine pollute2d_ww_compute

!Interpolatie naar de observaties
!#ifdef WIN32_IFORT
    !DEC$ ATTRIBUTES DLLEXPORT::pollute2d_ww_obs
!#endif
subroutine pollute2d_ww_obs(state ,hdescr, vval, husrdata, ierr)
implicit none
include "cta_f90.inc"
integer state, hdescr, vval, husrdata, ierr    

   call pollute2d_obs(state ,hdescr, vval, husrdata, ierr, .true.)

end subroutine pollute2d_ww_obs

!#ifdef WIN32_IFORT
    !DEC$ ATTRIBUTES DLLEXPORT::pollute2d_ww_covar
!#endif
subroutine pollute2d_ww_covar(colsvar, nnoise, husrdata,ierr)
implicit none
integer colsvar(*), nnoise, husrdata, ierr, inoise

   call pollute2d_covar(colsvar, nnoise, husrdata,ierr, .true.)

end subroutine pollute2d_ww_covar

!Initialize a model-instance:
!#ifdef WIN32_IFORT
    !DEC$ ATTRIBUTES DLLEXPORT::pollute2d_mw_create
!#endif
subroutine pollute2d_mw_create(hinput, state, sbound, sparam, nnoise, &
                     tHorizon, snamnoise, husrdata, ierr)
implicit none
integer hinput, state, sbound, sparam, nnoise, tHorizon, snamnoise, husrdata, ierr      

   call pollute2d_create(hinput, state, sbound, sparam, nnoise, &
                         tHorizon, snamnoise, husrdata, ierr, .false.)


end subroutine pollute2d_mw_create

!#ifdef WIN32_IFORT
    !DEC$ ATTRIBUTES DLLEXPORT::pollute2d_mw_compute
!#endif
subroutine pollute2d_mw_compute(timespan,state, saxpyforc, baddnoise, &
                      sparam, husrdata, ierr)
use pollute2d_params, only:CONT, &
                           isparallel 
use pollute2d,        only:get_par_info
implicit none
include 'mpif.h'
integer timespan, state, saxpyforc, baddnoise, sparam, husrdata, ierr      

integer ::cta_comm_world, cta_comm_myworld, cta_comm_master_worker
integer, parameter ::tag=99
integer ::iproc,nproc
   
   if (isparallel) then
      call CTA_Par_GetFComm(cta_comm_world, cta_comm_myworld, cta_comm_master_worker)
      call get_par_info(nproc,iproc)
      do iproc=2,nproc
         call MPI_SEND(CONT, 1, MPI_INTEGER, iproc-1, tag, cta_comm_myworld, ierr)
      enddo
   endif   
   call pollute2d_compute(timespan,state, saxpyforc, baddnoise, &
                             sparam, husrdata, ierr, .false.)

end subroutine pollute2d_mw_compute

!#ifdef WIN32_IFORT
    !DEC$ ATTRIBUTES DLLEXPORT::pollute2d_mw_free
!#endif
subroutine pollute2d_mw_free(husrdata,ierr)
use pollute2d_params, only:FINALIZE, nmodels, &
                           isparallel
use pollute2d,        only:get_par_info
implicit none
include 'mpif.h'
integer husrdata, ierr
integer ::cta_comm_world, cta_comm_myworld, cta_comm_master_worker
integer ::iproc, nproc
integer, parameter ::tag=99

   nmodels=nmodels-1
   if (nmodels==0) then
      if (isparallel) then
         call CTA_Par_GetFComm(cta_comm_world, cta_comm_myworld, cta_comm_master_worker)
         call get_par_info(nproc,iproc)
         do iproc=2,nproc
            call MPI_SEND(FINALIZE, 1, MPI_INTEGER, iproc-1, tag, cta_comm_myworld, ierr)
         enddo
      endif
   endif

end subroutine pollute2d_mw_free







!Interpolatie naar de observaties
!#ifdef WIN32_IFORT
    !DEC$ ATTRIBUTES DLLEXPORT::pollute2d_mw_obs
!#endif
subroutine pollute2d_mw_obs(state ,hdescr, vval, husrdata, ierr)
implicit none
include "cta_f90.inc"
integer state, hdescr, vval, husrdata, ierr    

   call pollute2d_obs(state ,hdescr, vval, husrdata, ierr, .false.)

end subroutine pollute2d_mw_obs

!#ifdef WIN32_IFORT
    !DEC$ ATTRIBUTES DLLEXPORT::pollute2d_mw_covar
!#endif
subroutine pollute2d_mw_covar(colsvar, nnoise, husrdata,ierr)
implicit none
integer colsvar(*), nnoise, husrdata, ierr, inoise

   call pollute2d_covar(colsvar, nnoise, husrdata,ierr, .false.)

end subroutine pollute2d_mw_covar


!Interpolatie naar de observaties
!#ifdef WIN32_IFORT
    !DEC$ ATTRIBUTES DLLEXPORT::pollute2d_ww_obssel
!#endif
subroutine pollute2d_ww_obssel(state ,ttime, hdescr, sselect, husrdata, ierr)
use pollute2d_params, only:m,n, nSources
use pollute2d,        only:get_par_info, &
                           par_dims_info
implicit none
include "cta_f90.inc"
integer state   ! :(I) Modelstate to be interpolated
integer ttime   ! :(I) Timespan of simulation
integer hdescr  ! :(I) Oobservation description  with meta-information of the
                !      observations like location and measurement dimension
integer sselect ! :(O) costa-string containing selection
                !      criterion for the observations
integer husrdata! :(I/O) Made in create
integer ierr    ! :(O)   Error flag


integer ::nproc,iproc
integer ::mLoc, nLoc, nSourcesLoc
integer ::nlo, nup
integer, dimension(nSources) ::sourceIndx
character(len=78) ::str
   ! Get domain information   
   call get_par_info(nproc,iproc)
   call par_dims_info(nproc, iproc, nlo, nup, nSourcesLoc, sourceIndx)

   !
   write (str,*) 'Y >= ',nlo,' AND Y <= ',nup
   call CTA_String_Set (sselect, str, ierr)
   if (ierr/=CTA_OK) goto 9999

   return
   9999 continue
   print *,'FATAL ERROR IN pollute2d_obssel ierr=',ierr
   call exit(-1)


end subroutine pollute2d_ww_obssel

logical function myIsNaN(x)
implicit none
real(kind=8) ::x

   myIsNaN= .not. (x<0.0d0 .or. x>=0.0d0)

end function myIsNaN

