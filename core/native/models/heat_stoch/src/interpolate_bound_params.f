!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/heat_stoch/src/interpolate_bound_params.f $
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
! This file implements the interpolation of bounary noise parameters
! to the right hand side of the boundary equation
!
!====================================================================
      subroutine interpolate_bound_params(nx,ny,pnx,pny,dx,dy,pbc_rhs,
     +                                    bc_rhs)
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
!     double precision pbc_rhs(pnx+pny,2) I  boundary noise parameters
!     double precision bc_rhs(nx+ny,2)    O  right hand side of b.equation
!--------------------------------------------------------------------
      implicit none
!
!     ARGUMENTS
      integer nx, ny, pnx, pny
      double precision dx, dy
      double precision pbc_rhs(pnx+pny,2), bc_rhs(nx+ny,2)
!
!     LOCAL VARIABLES
      integer ix,iy              ! counters of boundary points in X and Y
	integer pix,piy            ! counters of parameter points in X and Y
      double precision x,y       ! an x,y location in the grid
	double precision px,py     ! an x,y location of a parameter point
	double precision pdx, pdy  ! distance between parameters in X and Y

!     determine distance between parameter points
!     NB: Boundary points are located at (i-.5)*dx, boundary parameters at j*pdx.
!         For the boundary parameters to cover the whole grid, the boundary
!         parameters must range from 0 to nx*dx. Since the first boundary
!         parameter is at 0, the distance nx*dx must be covered by pnx-1 parameters.
      pdx = dx * dble(nx)/dble(pnx-1)
      pdy = dy * dble(ny)/dble(pny-1)

!     initialise right hand side of boundary equation
      do ix = 1,nx+ny
         bc_rhs(ix,1) = 0d0
         bc_rhs(ix,2) = 0d0
      end do

!     interpolate boundary parameters along y-boundary to the
!     right hand side of the boundary equation
      do ix = 1,nx
!        determine x-coordinate of boundary point
         x = (ix-0.5)*dx
!        for each boundary parameter along the y-boundary
         do pix = 1,pnx
!           determine the x-coordinate of the boundary parameter
            px = (pix-1)*pdx
!           interpolate the boundary parameter to the boundary point
!           by a linear weighing of the distance; only the parameters
!           directly surrounding the boundary point are taken into
!           account
            bc_rhs(ix,1) = bc_rhs(ix,1)  
     +                  + pbc_rhs(pix,1) * max(0d0,1d0-abs(px-x)/pdx)
            bc_rhs(ix,2) = bc_rhs(ix,2)  
     +                  + pbc_rhs(pix,2) * max(0d0,1d0-abs(px-x)/pdx)
         end do
      end do

!     similarly, interpolate boundary parameters along the x-boundary to
!     the right hand side of the boundary equation
      do iy = 1,ny
         y = (iy-0.5)*dy
         do piy = 1,pny
            py = (piy-1)*pdy
            bc_rhs(nx+iy,1) = bc_rhs(nx+iy,1)  
     +                 + pbc_rhs(pnx+piy,1) * max(0d0,1d0-abs(py-y)/pdy)
            bc_rhs(nx+iy,2) = bc_rhs(nx+iy,2)  
     +                 + pbc_rhs(pnx+piy,2) * max(0d0,1d0-abs(py-y)/pdy)
         end do
      end do
      
	end subroutine interpolate_bound_params

