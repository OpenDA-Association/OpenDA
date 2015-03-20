!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/heat_stoch/src/usr_obs.f $
! $Revision: 3852 $, $Date: 2013-02-22 11:49:14 +0100 (Fri, 22 Feb 2013) $
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
      subroutine usr_getcovar(covdiag, time)
      implicit none
      include 'cta_f77.inc'
      include 'modelparam.inc'
      integer time, covdiag

      integer diag,idum(1),ierr

      print *,'urs_obs: get covariance matrix'
      call cta_vector_create(CTA_DEFAULT_VECTOR,1,CTA_DOUBLE,idum,
     +                          covdiag,ierr)
      call cta_vector_setvals(covdiag,1.0d1,1,CTA_DOUBLE,ierr)

      end
      subroutine usr_getobs(time,assim,valid,all,obs)
      implicit none
      include 'cta_f77.inc'
      include 'modelparam.inc'
      integer time, assim, valid, all, obs

      integer idum(1),ierr

      if (assim.eq.CTA_NULL) then
         call cta_vector_create(CTA_DEFAULT_VECTOR,1,CTA_INTEGER,idum,
     +                          assim,ierr)
         call cta_vector_create(CTA_DEFAULT_VECTOR,1,CTA_INTEGER,idum,
     +                          valid,ierr)
         call cta_vector_create(CTA_DEFAULT_VECTOR,1,CTA_INTEGER,idum,
     +                          all,ierr)
         call cta_vector_create(CTA_DEFAULT_VECTOR,1,CTA_DOUBLE,idum,
     +                          obs,ierr)
      endif      

      call cta_vector_setvals(assim,1,1,CTA_INTEGER,ierr)
      call cta_vector_setvals(valid,1,1,CTA_INTEGER,ierr)
      call cta_vector_setvals(all,1,1,CTA_INTEGER,ierr)
      call cta_vector_setvals(obs,1.0d0,1,CTA_DOUBLE,ierr)

      end
      subroutine usr_getintpol(i,time,row);
      implicit none
      include 'cta_f77.inc'
      include 'modelparam.inc'
      integer i, time, row

      integer idum(4),ierr
      integer htemp
      integer hdeterm,ndeterm
      integer ipos

      if (row.ne.CTA_NULL) then
c        create a state vector like the model

c        I know that a model handle is a state for this model
c        and that it is initalised to zero
         call heatstoch_create_init(row,idum,0,ierr)
         if (ierr.ne.CTA_OK) goto 9999
      endif
c
c     Now set 1 at observation location (middle of grid )
c
      call cta_treevector_getsubtreevec(row, 'diff:determ',hdeterm,ierr)
      if (ierr.ne.CTA_OK) goto 9999

      call cta_treevector_GetSize(hdeterm,ndeterm,ierr)
      if (ierr.ne.CTA_OK) goto 9999
      call cta_vector_create(CTA_DEFAULT_VECTOR,ndeterm,CTA_DOUBLE,idum,
     +                          htemp,ierr)
      if (ierr.ne.CTA_OK) goto 9999


      call cta_treevector_getvec(hdeterm, htemp, ierr)
      if (ierr.ne.CTA_OK) goto 9999

      ipos=nx/2+(ny/2-1)*nx
      call cta_vector_setval(htemp,ipos,1.0d0,CTA_DOUBLE,ierr)
      if (ierr.ne.CTA_OK) goto 9999
      call cta_treevector_setvec(hdeterm, htemp, ierr)
      if (ierr.ne.CTA_OK) goto 9999
      call cta_vector_free(htemp,ierr)
      if (ierr.ne.CTA_OK) goto 9999

      return
9999  stop 'error in usr_getintpol'

      end
