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

      integer NX,NY
      parameter (NX=16,NY=20)
c      parameter (NX=8,NY=10)
c
c     NX,NY  dimension of the grid in x,y directions
c
      double precision DX, DY, DT
      parameter (DX=0.25, DY = 0.25, DT = 0.01)
c
c     DX,DY    grid size in x,y direction
c     DT       time step size
c
      integer NMODEL
      parameter (NMODEL=NX*NY)
c
c     NMODEL   dimension of the model (ex noise params)

      double precision bcNeumann(NX+NY,2), bcDirichlet(NX+NY,2)
      integer NBOUND 
      parameter (NBOUND = (NX+NY)*2)
      data bcNeumann   / NBOUND*0d0 /
      data bcDirichlet / NBOUND*1d0 /
      save bcNeumann
      save bcDirichlet
c
c     bcNeumann     boundary condition coefficient (Neumann part)
c     bcDirichlet   boundary condition coefficient (Dirichlet part)
c
c        bc:   bcNeumann * dz/dn + bcDirichlet * z = RHS
c


