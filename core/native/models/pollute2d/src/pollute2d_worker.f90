!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/pollute2d/src/pollute2d_worker.f90 $
! $Revision: 1582 $, $Date: 2010-05-17 22:46:14 +0200 (Mon, 17 May 2010) $
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

program pollute2d_worker
use pollute2d_params, only:pollute2d_params_init, CONT, FINALIZE, isparallel
use pollute2d,        only: create_state_vector
implicit none
include 'cta_f77.inc'
include 'mpif.h'

integer ::state   !State vector
integer ::ierr    !Error code
integer ::hfile   !Name of configuration file
integer ::htree   !Costa tree representation of input file
character(len=1024) ::inpfile
integer ::whatnext !Task for worker 
integer ::cta_comm_world, cta_comm_myworld, cta_comm_master_worker
integer, parameter ::tag=99

   !Get name of input file from command line
!   if (iargc()/=1) then
!      print *,'Program must have 1 argument'
!      call exit(-1)
!   endif
   call getarg(1, inpfile)

   call CTA_Core_Initialise(ierr)
   isparallel=.true.

   ! Read configuration file (contains the procress-group information)
   call CTA_String_Create(hfile, ierr)
   call CTA_String_Set(hfile, inpfile, ierr)
   call CTA_XML_Read(hfile,htree, ierr)
   if (ierr/=CTA_OK) then
      print *, 'Error opening or parsing file ',inpfile
      call exit(1)
   endif

   ! Create process groups but do not start model builder
   call CTA_Par_CreateGroups(htree,CTA_FALSE, ierr)

   ! Get communicators
   call CTA_Par_GetFComm(cta_comm_world, cta_comm_myworld, cta_comm_master_worker)


   ! Perform initializations
   call pollute2d_params_init

   ! Create state vector
   call create_state_vector(1, 0, state, ierr)

   ! Wait for tasks of the master
   do 
      call MPI_RECV(whatnext, 1,      MPI_INTEGER,          0, tag, cta_comm_myworld, MPI_STATUS_IGNORE, ierr)
      if (whatnext==CONT) then
!         print *,'Pollute 2d worker, performing timestep'

         call pollute2d_compute(CTA_NULL,state, CTA_NULL, CTA_FALSE , CTA_NULL, CTA_NULL, ierr, .false.)
      elseif (whatnext==FINALIZE) then
!         print *,'Pollute 2d worker, I have to finalize'
         call CTA_Core_Finalise()
         exit
      else
         print *, 'Error in pollute2d_worker, received unkown message for what to do:',whatnext
      endif


   enddo

end program
