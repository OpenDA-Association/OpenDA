!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/heat_stoch/src/interpolate_noise_params.f $
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
!====================================================================
!
! This file implements the interpolation of heat noise parameters
! to the heat field
!
!====================================================================
      subroutine interpolate_heat_params(
     +      nx,ny,pnx,pny,dx,dy,pheat,heat)
!--------------------------------------------------------------------
!     This routine implements the interpolation of boundary noise
!     parameters to the right hand side of the boundary equation
!
!     ARGUMENTS
!     integer nx                          I  number of grid points in X
!     integer ny                          I  number of grid points in Y
!     integer pnx                         I  number of par. points in X
!     integer pny                         I  number of par. points in Y
!     double precision dx                 I  grid size in X
!     double precision dy                 I  grid size in Y
!     double precision pheat(pnx,pny)     I  heat noise parameters
!     double precision heat(nx,ny)        O  heat field
!--------------------------------------------------------------------
      implicit none
!
!     ARGUMENTS
      integer nx, ny, pnx, pny
      double precision dx, dy
      double precision pheat(pnx,pny), heat(nx,ny)
!
!     LOCAL VARIABLES
      integer ix,iy             ! counters of grid points in X and Y
	integer pix,piy           ! counters of parameter points in X and Y
      double precision x, y     ! X,Y-coordinates of a grid point
	double precision px, py   ! X,Y-coordinates of a parameter point
	double precision pdx, pdy ! distance between parameter points in X and Y
!
!     determine distance between parameter points
!     NB: grid points are located at (i-0.5)dx, parameter points at j*pdx.
!         for the parameters to cover the full x-range, with the first 
!         grid point at 0, the distance dx*nx must be covered by pnx-1
!         parameter points
      pdx = dx * dble(nx)/dble(pnx-1)
      pdy = dy * dble(ny)/dble(pny-1)
!
!     initialise the heat field
      do ix = 1,nx
         do iy = 1,ny
            heat(ix,iy) = 0d0
         end do
      end do

!     interpolate the heat parameters to the heat field
!     effectively, the four parameter points surrounding the grid
!     point are interpolated to the gridpoint, weighted by their
!     distance
      do ix = 1,nx
         x = (ix-0.5)*dx  ! x-coordinate of grid point
         do pix = 1,pnx
            px = (pix-1)*dx*nx/dble(pnx) ! x-coord of parameter
            do iy = 1,ny
               y = (iy-0.5)*dy  ! y-coordinate of grid point
               do piy = 1,pny
                  py = (piy-1)*dy*ny/dble(pny) ! y-coord of parameter
                  heat(ix,iy) = heat(ix,iy) +
     +                  pheat(pix,piy)
     +                  * max(0d0,
     +                        min(1d0-abs(px-x)/pdx,
     +                            1d0-abs(py-y)/pdy))
               end do
            end do
         end do
      end do

      end subroutine interpolate_heat_params

