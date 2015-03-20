!$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/src/cta/cta_time.c $
!$Revision: 2751 $, $Date: 2011-09-09 08:58:46 +0200 (Fri, 09 Sep 2011) $
!
!Copyright (C) 2013  Nils van Velzen
!
!This library is free software; you can redistribute it and/or
!modify it under the terms of the GNU Lesser General Public
!License as published by the Free Software Foundation; either
!version 2.1 of the License, or (at your option) any later version.
!
!This library is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!Lesser General Public License for more details.
!
!You should have received a copy of the GNU Lesser General Public
!License along with this library; if not, write to the Free Software
!Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


!Example/template implementation of a native resultwriter

module resultwriter
use cta_f90

integer, private, parameter ::maxwriters=50;
logical, private, dimension(maxwriters), save ::notInitialized=.true.
integer, private, dimension(maxwriters), save ::lunFile=0;


contains 

subroutine getLun(iDWriter,config, lun)
  implicit none
  integer, intent(in)  ::iDWriter
  integer, intent(in)  ::config
  integer, intent(out) ::lun


   integer soutput, ierr
   character(len=120) ::output

  if (iDWriter<1 .or. iDWriter>maxwriters) stop 'ERROR IDWriter has incorrect value'
  
  if (notInitialized(iDWriter)) then
     notInitialized(iDWriter)=.false.
     call CTA_Tree_GetHandleStr(config, 'output_file', soutput, ierr)
     if (ierr/=CTA_OK) then
        stop 'Cannot get field output_file, is it not specified in the input?'
     endif
     call CTA_String_Get(soutput,output, ierr)
     lunFile(iDWriter)=iDWriter+100
     open (unit=lunFile(iDWriter), file=output)
  endif
  lun=lunFile(iDWriter) 
  

end subroutine getLun
    



!DEC$ ATTRIBUTES DLLEXPORT::putmessage
subroutine putmessage(iDWriter, config, workingDir, message, retval,         &
                      len_workingDir, len_message)                           &
   bind(C, NAME='putmessage')
   use iso_c_binding
   implicit none

   integer(C_INT), VALUE  ::iDWriter, config, len_workingDir, len_message
   integer(C_INT)         ::retval
   character(kind=c_char) ::workingdir(*), message(*)
   
   integer ::lun

   call getLun(iDWriter,config, lun)
   
   write(lun,*) message(1:len_message)


   retval=CTA_OK

end subroutine putmessage



!DEC$ ATTRIBUTES DLLEXPORT::putvalue
subroutine putvalue(iDWriter, config, workingDir, id, handle,                 &
                    outputlevel, context, iteration, retval,                  &
                    len_workingDir, len_id, len_context)                      &
   bind(C, NAME='putvalue')
   use iso_c_binding
   implicit none
   integer(C_INT), VALUE  ::iDWriter, config, handle, outputlevel, iteration, &
                            len_workingDir, len_id, len_context
   integer(C_INT)         ::retval
   character(kind=c_char) ::workingdir(*), id(*), context(*)

   integer ::lun

   call getLun(iDWriter,config, lun)
   
   write(lun,*) 'variable ',id(1:len_id)
   write(lun,*) 'native handle of vector is ',handle




   retval=CTA_OK
end subroutine putvalue

!DEC$ ATTRIBUTES DLLEXPORT::putiterationreport
subroutine putiterationreport(iDWriter, config, workingDir, iteration, cost,  &
                              handle, retval, len_workingDir)                 &
   bind(C, NAME='putiterationreport')
   use iso_c_binding
   implicit none
   integer(C_INT), VALUE    ::iDWriter, config, iteration, handle, len_workingDir
   real(C_DOUBLE), VALUE    ::cost
   integer(C_INT)           ::retval
   character(kind=c_char)   ::workingdir(*)

   integer ::lun

   call getLun(iDWriter,config, lun)

   write(lun,*), 'Iteration: ',iteration, ' Cost: ',cost


   retval=CTA_OK
end subroutine putiterationreport

!DEC$ ATTRIBUTES DLLEXPORT::freewriter
subroutine freewriter(iDWriter)                                             &
   bind(C, NAME='freewriter')
   use iso_c_binding
   implicit none
   integer(C_INT), VALUE    ::iDWriter

   integer ::lun

   call getLun(iDWriter,CTA_NULL, lun)
   write(lun,*), 'Close output file'
   close(lun)

end subroutine freewriter


end module resultwriter

