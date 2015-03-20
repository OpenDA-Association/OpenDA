      subroutine pbm_cut(ks,kstart,kend,nthds)
      integer ks(2,0:*)
c
      kend0=kend
      kstart0=kstart
c     ! assign range to threads
      ithds=0
      do while ( ithds.lt.nthds-1 ) 
            nn=kend0-kstart0+1
            nblk=nn/(nthds-ithds)
            if(nblk.lt.0) nblk=0 
            if(mod(nblk,2).eq.1) then
               nblk=nblk+1
            endif
            ks(1,ithds)=kstart0
            ks(2,ithds)=kstart0+nblk-1
            kstart0=kstart0+nblk
            ithds=ithds+1
      end do
c     ! last thread
      nn=kend0-kstart0+1
      nblk=nn 
      if(nblk.lt.0) nblk=0 
      ks(1,ithds)=kstart0
      ks(2,ithds)=kstart0+nblk-1
c
      return
      end
