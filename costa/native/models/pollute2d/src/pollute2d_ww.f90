!
! $URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/examples/pollute2d/src/pollute2d_worker.f90 $
! $Revision: 1582 $, $Date: 2010-05-17 22:46:14 +0200 (Mon, 17 May 2010) $
!
! OpenDA: Problem solving environment for data assimilation
! Copyright (C) 2010  Nils van Velzen
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




!** This is the main program that can be used to run the pollute model in
!** worker-worker mode using mpi spawn
program pollute2d_ww
implicit none
include 'cta_f77.inc'
include 'mpif.h'

   integer ierr !Error return code of an MPI/COSTA call
   character(len=1024) inpfile
   integer hfile, htree


   !Get input argument of executable
   call getarg(1, inpfile)

   call CTA_Core_Initialise(ierr)

   inpfile='parallel_config.xml'
   ! Read configuration file (contains the procress-group information)
   call CTA_String_Create(hfile, ierr)
   call CTA_String_Set(hfile, inpfile, ierr)
   call CTA_XML_Read(hfile,htree, ierr)
   if (ierr/=CTA_OK) then
      print *, 'Error opening or parsing file ',inpfile
      call exit(1)
   endif

   call CTA_Par_WorkerSpawn(CTA_TRUE, ierr)

   print *,'Done for the moment'
   call MPI_Finalize(ierr)


end program
