!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/heat_stoch/src/modelstep.f $
! $Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (Tue, 07 Oct 2008) $
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
! This file implements a single time step of the heat transport model
!
!====================================================================
      subroutine modelstep(nx,ny,dt,dx,dy,temp,heat,
     +           bcneumann, bcdirichlet, bcrhs)
!--------------------------------------------------------------------
!    This routine performs a single time step of the heat transport
!    model
!
!    ARGUMENTS
!
!     integer nx                            I   size of domain in x-direction
!     integer ny                            I   size of domain in y-direction
!     double precision dt                   I   timestep
!     double precision dx                   I   grid size in x-direction
!     double precision dy                   I   grid size in y-direction
!     double precision temp(nx,ny)          I/O state
!     double precision heat(nx,ny)          I   heat source on entire domain
!     double precision bcneumann(nx+ny,2)   I   coefficient of Neumann boundary 
!     double precision bcdirichlet(nx+ny,2) I   coefficient of Dirichlet bound
!     double precision bcrhs(nx+ny,2)       I   righ hand side of boundary eq.
!
!--------------------------------------------------------------------
      implicit none
!
!     ARGUMENTS
      integer nx                            
      integer ny                            
      double precision dt                   
      double precision dx                   
      double precision dy                   
      double precision temp(nx,ny)          
      double precision heat(nx,ny)          
      double precision bcneumann(nx+ny,2)   
      double precision bcdirichlet(nx+ny,2) 
      double precision bcrhs(nx+ny,2)       
!
!     LOCAL VARIABLES
      integer ix, iy                        ! grid point counters in x,y-direct.
      double precision temp_old(0:nx+1,0:ny+1) ! temperature at prev. timestep




!     copy current temperature distribution to temp_old
      do iy = 1,ny
         do ix = 1,nx
            temp_old(ix,iy) = temp(ix,iy)
         end do
      end do

!     apply boundary condition on the boundaries at iy=0 and iy=ny+1
      do ix = 1,nx

         temp_old(ix,0) =
     +   (bcrhs(ix,1) - bcneumann(ix,1)*temp_old(ix,1)/dx)/
     +      (bcdirichlet(ix,1)-bcneumann(ix,1)/dx)

         temp_old(ix,ny+1) = 
     +   (bcrhs(ix,2) + bcneumann(ix,2)*temp_old(ix,ny)/dx)/
     +       (bcdirichlet(ix,2)+bcneumann(ix,2)/dx)

      end do

!     apply boundary condition on the boundaries at ix=0 and ix=nx+1
      do iy = 1,ny

         temp_old(0,iy) =
     +   (bcrhs(nx+iy,1) - bcneumann(nx+iy,1)*temp_old(1,iy)/dx)/
     +      (bcdirichlet(nx+iy,1)-bcneumann(nx+iy,1)/dx)

         temp_old(nx+1,iy) =
     +   (bcrhs(nx+iy,2) + bcneumann(nx+iy,2)*temp_old(nx,iy)/dx)/
     +       (bcdirichlet(nx+iy,2)+bcneumann(nx+iy,2)/dx)

      end do

!     compute new temperature distribution
      do iy = 1,ny
         do ix = 1,nx
           temp(ix,iy) =
     +       temp_old(ix,iy) 
     +       + dt*
     +          (temp_old(ix-1,iy)+temp_old(ix+1,iy)-2*temp_old(ix,iy))/
     +          (dx*dx)
     +       + dt*
     +          (temp_old(ix,iy-1)+temp_old(ix,iy+1)-2*temp_old(ix,iy))/
     +          (dy*dy)
     +       + dt * heat(ix,iy)
         end do
      end do




      end subroutine modelstep
