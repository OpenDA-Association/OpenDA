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

      double precision dt
      parameter (dt=0.0005d0)
      double precision xmin, xmax
      parameter(xmin=0   , xmax=1)
      integer mmax
      parameter(mmax=100)
      double precision dx
      parameter(dx=(xmax-xmin)/(mmax+1.0))
      double precision xc (0:mmax) !grid at cell-centers
      double precision xf (0:mmax) !grid at cell-faces
      double precision dxc(0:mmax) !gridstep at cell-centers
      double precision q           !discharge m^3/s
      parameter(q=1.0d0)
      double precision af (0:mmax) !cross-section area at cell-faces
      double precision ac (0:mmax) !cross-section area at cell-centres
      double precision uf (0:mmax) !velocity at cell-faces
 
      integer hveccc
      integer nmodel
      common /advecmodel/ xc,xf,dxc,af,ac,uf,hveccc,nmodel
      save /advecmodel/ 
