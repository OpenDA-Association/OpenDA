c
c COSTA: Problem solving environment for data assimilation
c Copyright (C) 2005  Nils van Velzen
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
c

      integer PNXH,PNYH
      parameter (PNXH=NX,PNYH=NY)
c
c     PNXH, PNYH    dimension of the noise parameter grid for 
c                   heat production in x, y direction
c
      integer PNXBC,PNYBC
      parameter (PNXBC=NX,PNYBC=NY)
c
c     PNXBC, PNYBC  dimension of the boundary noise grid in x, y direction
c
      integer NHEAT, NBC, NPARAM, NSTATE
      parameter (NHEAT=PNXH*PNYH)
      parameter (NBC=2*(PNXBC+PNYBC),  NPARAM=NHEAT+NBC)
      parameter (NSTATE=NMODEL+NPARAM)
c
c     NHEAT    dimension of the heat noise parameters
c     NBC      dimension of the boundary noise parameters
c     NPARAM   dimension of the noise parameters
c     NSTATE   dimension of the combined (Kalman) state

      double precision FORGET_HEAT, FORGET_BC
      parameter (FORGET_HEAT=0.95, FORGET_BC=0.9)
c
c     FORGET_HEAT      time correlation coefficient for heat noise
c     FORGET_BC        time correlation coefficient for boundary noise
c
      double precision STDVXBC, STDVYBC, STDVXH,  STDVYH
ce      parameter (STDVXBC=2d0, STDVYBC=1d0, STDVXH=3d0, STDVYH=2.5d0)
      parameter (STDVXBC=2d0, STDVYBC=1d0, STDVXH=0.8d0, STDVYH=0.75d0)
c
c     STDVXBC, STDVYBC   characteristic length in the boundary noise (grid
c                        sizes)
c     STDVXH, STDVYH   characteristic length in the heat noise (grid sizes)

      double precision QDIAGH, QDIAGBC
      parameter (QDIAGH=0.4, QDIAGBC=1.0)
c
c     QDIAGH      value of the Q-matrix for heat on the diagonal
c     QDIAGBC     value of the Q-matrix for boundary noise on the diagonal 
c
      double precision PDXH, PDYH, PDXBC, PDYBC
c
c     PDXH, PDYH    grid size in x,y direction in the heat noise grid
c     PDXBC, PDYBC  grid size in x,y direction in the boundary noise 
c                   grid.

c
c     Calculate grid sizes of noise grids
c
      parameter (PDXH  = (DX * NX)/(PNXH-1))
      parameter (PDYH  = (DY * NY)/(PNYH-1))
      parameter (PDXBC = (DX * NX)/(PNXBC-1))
      parameter (PDYBC = (DY * NY)/(PNYBC-1))
c



