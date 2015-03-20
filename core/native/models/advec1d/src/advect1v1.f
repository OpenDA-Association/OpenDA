!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/advec1d/src/advect1v1.f $
! $Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (Tue, 07 Oct 2008) $
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

      PROGRAM advect1dv1
      implicit none
      ! Name    : advect1v1  
      ! Purpose : 1d advection problem solved programmed in different
      !           way to check for clarity of code and speed
      !         : This is version 1 : classical approach with do-loop over grid
      ! Author  : Martin Verlaan
      ! License : Copyright Martin Verlaan
      ! Log     : 20040626 First version
      
      !size of the problem
      double precision tstart , tstop , dt
      parameter(tstart=0 , tstop=100 , dt=0.0005d0)
      double precision xmin, xmax
      parameter(xmin=0   , xmax=10)
      integer mmax
      parameter(mmax=1000)
      double precision dx
      parameter(dx=(xmax-xmin)/(mmax+1.0))

      !grid  (stagered grid c  at m*dx and u at (m+1/2)*dx
      double precision xc (0:mmax) !grid at cell-centers
      double precision xf (0:mmax) !grid at cell-faces
      double precision dxc(0:mmax) !gridstep at cell-centers
      
      !parameters of the problem and initial value
      double precision q         !discharge m^3/s
      parameter(q=1.0d0)
      double precision af (0:mmax) !cross-section area at cell-faces
      double precision ac (0:mmax) !cross-section area at cell-centres
      double precision uf (0:mmax) !velocity at cell-faces
      double precision qfc(0:mmax) !concentrationflux at cell-faces
      double precision cc (0:mmax) !concentrations at cell-centers time t
      double precision cn (0:mmax) !concentrations at cell-centers time t+dt

      !output specifiers
      integer pmax
      parameter(pmax=5)
      integer p(1:pmax)
      
      !counters, temporary variables, etc
      double precision           t
      integer        count
      double precision           qcf(0:mmax)  !flux at cell-faces time t+dt
      integer        m ! cell counter
      integer        pi !counter for output points
      
      double precision leftbound


      !initialisations
      data p /0 , 200 , 400 , 800 , 1000/
      count=0
      do m=0,mmax
         xc(m)=0.0d0
         xf(m)=0.0d0
         af(m)=1.0d0
         cc(m)=0.0d0
         cn(m)=0.0d0
      enddo


      !precomputations
      do m=0,mmax
         xc(m) = m*dx
         xf(m) = (m+0.5d0)*dx 
         uf(m) = q/af(m)
      enddo
      do m=1,mmax
         ac(m)  = 0.5d0*(af(m)+af(m-1)) !area at cell-centers
         dxc(m) = xf(m)-xf(m-1)
      enddo
      
      
      write(*,*) '--- begin loop ---'
      do t=tstart,tstop,dt
         ! counter
         count = count+1
         if (mod(count,5000)==1) then
            write(*,*) 'time , vals  =',t , (cc(p(pi)),pi=1,pmax)
         endif
         !---------------------------------------------
         ! one timestep
         ! 1) traditional method -- first order upwind
         !---------------------------------------------
         ! left boundary
         cn(0) = leftbound(t+dt)
         qfc(0) = uf(0)*af(0)*cc(0)
         ! interior domain
         do m=1,mmax  
            qfc(m) = uf(m)*af(m)*cc(m)
         cn(m)  = cc(m) - dt * (qfc(m)-qfc(m-1))/(ac(m)*dxc(m))
         enddo
      !write(*,*) 'qfc = ',qfc(1:5)
      !write(*,*) 'cn = ',cn(1:5)
         do m=0,mmax
            cc(m)=cn(m)
         enddo
         !-------------------------------------------------------------------------------
            enddo
            write(*,*) '--- END ---'
            
      END PROGRAM
            
      FUNCTION leftbound(time)
      double precision time
      double precision leftbound
      double precision  pi
      parameter(pi=3.14d0)
      double precision  omega
      parameter(omega=2*pi/10.0d0)
      
      leftbound = 1.0d0+sin(omega*time)
       
      END FUNCTION leftbound
      
      
      
      
      
      
      
      
      
      
      
