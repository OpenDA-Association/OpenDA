!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/advec1d/src/advec_model.f $
! $Revision: 833 $, $Date: 2009-04-05 14:42:12 +0200 (Sun, 05 Apr 2009) $
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

      subroutine advec_model_initialise(userdata,hstate,ierr)
      implicit none
      include 'cta_f77.inc'
      include 'modelparam.inc'


      integer userdata
      integer hstate
      integer ierr

c     Handle of vector that holds values of the state vector
      integer nstate
      parameter (nstate=mmax+1)
      integer retval
      integer dum
      integer m

      logical isinit
      data isinit /.false./
c
c     Initialise model itself
c
      if (.not. isinit) then
         isinit=.true.
         nmodel=0
c
c        set constants in model common
c
         do m=0,mmax
            af(m)=1.0d0           !cross-section area at cell-faces
         enddo
         do m=0,mmax
            xc(m) = m*dx          !grid at cell-centers
            xf(m) = (m+0.5d0)*dx  !grid at cell-faces
            uf(m) = q/af(m)       !velocity at cell-faces
         enddo
         do m=1,mmax
            ac(m)  = 0.5d0*(af(m)+af(m-1)) !area at cell-centers
            dxc(m) = xf(m)-xf(m-1)         !gridstep at cell-centers
         enddo
      endif
      nmodel=nmodel+1
c
c     compute vector for holding state values
c
      call cta_vector_create(CTA_DEFAULT_VECTOR, nstate, CTA_DOUBLE,
     +                       dum,hveccc,retval)
c
c     create state:
c
       call cta_treevector_create("advection model","advecXC",hstate,
     +                       retval)
       if (retval.ne.CTA_OK) goto 9999
c
c     initialise vector for initial state
c
      if (retval.ne.CTA_OK) goto 9999
      call cta_vector_setconstant(hveccc,0.0d0,CTA_DOUBLE,retval)
      if (retval.ne.CTA_OK) goto 9999
c
c     set values of state
c
      call cta_treevector_setvec(hstate,hveccc,retval)
      if (retval.ne.CTA_OK) goto 9999


      return
9999  stop 'error in advec_model_initialise'

      end

      subroutine advec_model_timestep(hstate,hnoisebnd,time)
      implicit none
      include 'cta_f77.inc'
      include 'modelparam.inc'
      integer hstate
      integer hnoisebnd
      double precision time

      double precision cc(0:mmax)
      double precision cn(0:mmax)
      double precision qfc(0:mmax)
      double precision bnd_noise(1)
      
      integer          m
      integer          nstate
      parameter(nstate=mmax+1)
      integer retval
      external leftbound
      double precision leftbound

      call cta_treevector_getvec(hstate,hveccc,retval)
      if (retval.ne.CTA_OK) goto 9999
      call cta_vector_getvals(hveccc,cc,nstate,CTA_DOUBLE,retval)
      if (retval.ne.CTA_OK) goto 9999

      call cta_vector_getvals(hnoisebnd,bnd_noise,1,CTA_DOUBLE,retval)
      if (retval.ne.CTA_OK) goto 9999

      cn(0) = leftbound(time+dt)+bnd_noise(1)
      qfc(0) = uf(0)*af(0)*cc(0)
      ! interior domain
      do m=1,mmax  
!         qfc(m) = uf(m)*af(m)*cc(m)
!         cn(m)  = cc(m) - dt * (qfc(m)-qfc(m-1))/(ac(m)*dxc(m)) !+dt*s

         ! Now just the simple equation
         cn(m) = cc(m) -1.0*(dt/dxc(m))*(cc(m)-cc(m-1))

      enddo
!      write(*,*) 'qfc = ',qfc(1:5)
!      write(*,*) 'cn = ',cn(1:5)
      do m=0,mmax
         cc(m)=cn(m)
      enddo

      call cta_vector_setvals(hveccc,cc,nstate,CTA_DOUBLE,retval)
      if (retval.ne.CTA_OK) goto 9999
      call cta_treevector_setvec(hstate,hveccc,retval)
      if (retval.ne.CTA_OK) goto 9999

      return
9999  stop 'error in advec_model_timestep'

      end

      subroutine advec_model_finalise(userdata,hstate,ierr)
      implicit none
      include 'cta_f77.inc'
      include 'modelparam.inc'
      integer userdata
      integer hstate
      integer ierr

      integer retval

      call cta_treevector_free(hstate,retval)
      if (retval.ne.CTA_OK) goto 9999

      if (nmodel.eq.1) then
         call cta_vector_free(hveccc,retval)
         if (retval.ne.CTA_OK) goto 9999
      endif

      return
9999  stop 'error in advec_model_finalise'

      end

      FUNCTION leftbound(time)
      double precision time
      double precision leftbound
      double precision  pi
      parameter(pi=3.14d0)
      double precision  omega
      parameter(omega=2.0d0*pi)
      !parameter(omega=2.0d0*pi/10.0d0)
      
      leftbound = 1.0d0 + dsin(omega*time)
       
      END FUNCTION leftbound
      
