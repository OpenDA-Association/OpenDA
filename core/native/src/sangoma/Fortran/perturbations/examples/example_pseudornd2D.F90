program testpseudo2D
! Generates a 100x100 2-dimensional random perturbation 
! The code for generating a 3D (x-y-z) or (x-y-t) random field is commented. 
! A random seed is generated based on the machine clock to avoid generating 
! the same field each time. 
!
! The result is written to the file 'PERT.txt' and can be plottin gnuplot
! with
!    splot 'PERT.txt' w l 
! or 
!    plot 'PERT.txt' with image

      use mod_sangoma_pseudornd, only: sangoma_pseudornd2D
      use mod_set_random_seed, only: set_random_seed
      implicit none 

      character(len=80) outfile

      real rv,rh, alp,bet
      integer i,j,n1,n2
      integer idm,jdm,kdm

      integer k,l,iostat
      real, allocatable, dimension(:,:) :: fld2d,ranfld,accranfld

      ! Initialize
      idm=100; jdm=100;kdm=30

      allocate(fld2d    (idm,jdm))
      allocate(ranfld   (idm,jdm))
      allocate(accranfld(idm,jdm))


      rv=2.0e00  ! vertical correlation range (in nb of layers)
      rh=25.0   ! horizontal decorrelation scale (number of grid cells)

      !  Generates the vertical correlation of the ensemble.
      alp=exp(-1.0/rv)
      bet=sqrt(1.0-alp**2) ! keeps var(enstmp)=var(ensmem)

      ! FFT dimensions are powers of 2 for best performance 
      n1=2**(ceiling(log(float(idm))/log(2.)))
      n2=2**(ceiling(log(float(jdm))/log(2.)))
      ! set file names
      outfile='PERT.txt'

      call set_random_seed

      open (unit=801,file=trim(outfile),status='replace')


      !do k=1,kdm 

            ! 3D vertically correlated
            call sangoma_pseudornd2D(   ranfld,idm,jdm,1,rh,n1,n2)
            !if (k==1) then
               !call sangoma_pseudornd2D(accranfld,idm,jdm,1,rh,n1,n2)
            !else
               !accranfld=alp*accranfld+bet*ranfld
            !end if
      !end do

      do i=1,idm 
      do j=1,jdm 
         write(801,'(2i6,f6.2)') i,j,ranfld(i,j)
      end do  
      end do  

      ! Close up shop
      close(801)

   deallocate(accranfld,fld2d,ranfld)

end program testpseudo2d


