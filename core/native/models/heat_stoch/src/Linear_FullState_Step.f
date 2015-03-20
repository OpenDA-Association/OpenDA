!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/heat_stoch/src/Linear_FullState_Step.f $
! $Revision: 807 $, $Date: 2009-03-13 13:48:06 +0100 (Fri, 13 Mar 2009) $
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
! This file implements a single time step of the full, stochastic
! heat transport model
!
!====================================================================
      subroutine linear_fullstate_step(temp, heat_noise, boundary_noise)
!--------------------------------------------------------------------
!     This routine implements a single time step of the full, stochastic
!     heat transport model
!
!     ARGUMENTS
!     double precision temp(NX,NY)         IO  the temperature field
!     double precision heat_noise(NHEAT)   IO  the heat noise field
!     double precision boundary_noise(NBC) IO  the boundary noise
!--------------------------------------------------------------------
      implicit none
      include 'modelparam.inc'
	include 'stochparam.inc'
!
!     ARGUMENTS
      double precision temp(NX,NY)
	double precision heat_noise(NHEAT)
      double precision boundary_noise(NBC)
!
!     LOCAL VARIABLES
      double precision heat(NX,NY), bc_rhs(NX+NY,2)
      integer i,j 


!
!     interpolate heat noise to the heat field
!
      call interpolate_heat_params(
     +     NX,NY,PNXH,PNYH,DX,DY,heat_noise,heat)


!
!     interpolate boundary noise to the right hand side
!     of the boundary equation
!
      call interpolate_bound_params(
     +     NX,NY,pnxbc,pnybc,DX,DY,boundary_noise,
     +     bc_rhs)
!
!     perform a timestep of the deterministic model
!
      call modelstep(
     +           NX,NY,Dt,DX,DY,
     +           temp, heat,
     +           bcNeumann, bcDirichlet, bc_rhs)
!

!     decrease the heat noise because a time step has passed
!
      do i = 1,NHEAT
         heat_noise(i) = heat_noise(i)*FORGET_HEAT
      end do
!
!     decrease the boundary noise because a time step has passed
!
      do i = 1,NBC
         boundary_noise(i) = boundary_noise(i)*FORGET_BC
      end do

      end subroutine linear_fullstate_step

