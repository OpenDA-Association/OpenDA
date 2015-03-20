
SUBROUTINE WELCOME  
    
  ! *** CHANGE RECORD  
  ! ***   
  ! DATE MODIFIED     BY           
  ! 06/25/2006        Paul M. Craig
  !                   Updated Code to Fortran 90
! { GEOSR 2011.4.26 JGCHO
  ! 04/30/2011        USER : 
  !                   VER : 
  ! ############################# 
  iver=1 ! CHECK version no.
  ! ############################# 
!  open(2011,file='VER.INP')
!  read(2011,*) iverread
! close(2011)
!  if (iver.ne.iverread) then
!    write(*,*) 'CHECK EFDC VERSION. iver=',iver,', iverread=',iverread
!    stop
!  endif
  ! #############################
! } GEOSR 2011.4.26 JGCHO


  WRITE(6,1)  

1 FORMAT('***********************************************************************'  &
      ,/,'*                                                                     *'  &  
      ,/,'*                                                                     *'  &  
      ,/,'*            EEEEEEEEE    FFFFFFFFF    DDDDDDDD       CCCCCCCC        *'  &  
      ,/,'*           EEE          FFF          DDD     DD    CCC      CC       *'  &  
      ,/,'*          EEE          FFF          DDD     DD    CCC                *'  &  
      ,/,'*         EEEEEEEE     FFFFFFFF     DDD     DD    CCC                 *'  &  
      ,/,'*        EEE          FFF          DDD     DD    CCC                  *'  &  
      ,/,'*       EEE          FFF          DDD     DD    CCC      CC           *'  &  
      ,/,'*      EEEEEEEEE    FFF          DDDDDDDDDD      CCCCCCCCC            *'  &  
      ,/,'*                                                                     *'  &  
      ,/,'*                                                                     *'  &  
      ,/,'*                                                                     *'  &  
      ,/,'*                  ENVIRONMENTAL FLUID DYNAMICS CODE                  *'  &  
      ,/,'*                  DEVELOPED BY JOHN M. HAMRICK                       *'  &  
      ,/,'*                                                                     *'  &  
      ,/,'*     EFDC AND "ENVIRONMENTAL FLUID DYNAMICS CODE" ARE TRADEMARKS     *'  &  
      ,/,'*     OF JOHN M. HAMRICK                                              *'  &  
      ,/,'*                                                                     *'  &  
      ,/,'*                                                                     *'  &  
      ,/,'*     EFDC_DS:  UPDATED AND ENHANCED BY DYNAMIC SOLUTIONS, LLC        *'  &  
      ,/,'*                                                                     *'  &  
      ,/,'*               EFDC_EXPLORER CUSTOMIZED VERSION                      *'  &  
      ,/,'*                                                                     *'  &  
      ,/,'*                  VERSION DATE: 28 OCT 2010                          *'  &  
      ,/,'*                                                                     *'  &  
      ,/,'*                      EDITED BY GEOSR                                *'  &  
      ,/,'*                                                                     *'  &  
      ,/,'***********************************************************************') 
RETURN  
END  

