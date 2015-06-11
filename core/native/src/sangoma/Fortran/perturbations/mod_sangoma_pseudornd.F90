!$id: $
module mod_sangoma_pseudornd

! The spatial covariance function is by default Gaussian
! alternatively, one can use an exponential function here by CPP flag
#undef EXPCOV
#define FFTW

contains

subroutine sangoma_pseudornd2D(Amat,nx,ny,nens,rh,n1,n2) &
     bind(C, name="sangoma_pseudornd2d_")
! This routine calculates the pseudo random 2D fields using
! the spectral procedure outlined in Evensen (1994) \cite{eve94a}.

   use, intrinsic :: ISO_C_BINDING
   use sangoma_base, only : REALPREC, INTPREC 
   implicit none
   integer(INTPREC), intent(in) :: nx,ny           ! horizontal dimensions
   integer(INTPREC), intent(in) :: nens             ! number of fields stored at the time
   real(REALPREC), intent(out)  :: Amat(nx,ny,nens) ! generated random fields
   real(REALPREC), intent(in)   :: rh              ! Horizontal decorrelation length, in nb of horizontal grid cells
   integer(INTPREC), intent(in) :: n1,n2           ! horizontal dimensions in fft grid
   ! Suggestion for grid sizes in FFT grid:
   ! n1 = ceiling(log(float(nx)+2.*rh)/log(2.))
   ! n1 = 2**n1

   ! Different libraries are supported below. Select only one. 
#ifdef CRAY
   real, allocatable, dimension(:), save :: work 
   real, allocatable, dimension(:), save :: table 
#endif

#ifdef ESSL
   integer :: maxnn,s1
   integer,save :: naux1,naux2,naux3
   double precision, dimension(:), allocatable,save :: aux1,aux2,aux3
#endif


#ifdef DEC
   integer status
   record /dxml_d_fft_structure_2d/ fft_struct
#endif

#ifdef SGI
   real, allocatable, dimension(:), save :: coeff
#endif

#if defined(FFTW)
! Verify  the FFTW library is installed and included at compile-time, no need to copy fftw3.f here
   integer*8, save :: plan
   real*8 :: fftwy(n1,n2)
   include 'fftw3.f'
#endif


   real, save ::  rh_save=0.0  ! saving rh used in preprocessing.  Allow for new call to
                               ! zeroin if rh changes.

   real, save :: sigma,sigma2
   real, save :: c
   integer, save :: n1_save=0
   integer, save :: n2_save=0

   integer l,p,j,n,m,i
   real kappa2,lambda2,kappa,lambda
   real pi2,deltak,sum,scale
   real a1,b1,tol,fval

   real, allocatable    :: fampl(:,:,:)
   real, allocatable    :: phi(:,:)
   real, allocatable    :: y(:,:)   ! Physical field
   complex*16, allocatable :: x(:,:)   ! Fourier amplitudes
   complex, allocatable :: xx(:,:)   ! complex physical field

   real, parameter :: dx=1.0
   real, parameter :: pi=3.141592653589

   !real, external :: func2D

   if (nens < 1) then
      stop 'pseudo2D: error nens < 1'
   end if
   if (rh <= 0.0) then
      stop 'pseudo2D: error, rh <= 0.0'
   end if

   allocate(fampl(0:n1/2,-n2/2:n2/2,2))
   allocate(phi(0:n1/2,-n2/2:n2/2))
   allocate(y(0:n1+1,0:n2-1))
   allocate(x(0:n1/2,0:n2-1))


#ifndef CRAY
#ifndef ESSL
#ifndef DEC
#ifndef SGI
#if  !defined(FFTW)
#error No FFT method defined
   print *,'pseudo2D is only running on the following machines:'
   print *,'   CRAY'
   print *,'   AIX having essl'
   print *,'   DEC having dxml'
   print *,'   SGI'
   print *,'   Any machine with FFTW'
   stop  '(pseudo2D)'
#endif
#endif
#endif
#endif
#endif

   pi2=2.0*pi
   deltak=pi2**2/(float(n1*n2)*dx*dx)
   kappa=pi2/(float(n1)*dx)
   kappa2=kappa**2
   lambda=pi2/(float(n2)*dx)
   lambda2=lambda**2
   scale=1.0

   if (rh /= rh_save .or. n1 /= n1_save .or. n2 /= n2_save) then
      rh_save=rh
      n1_save=n1
      n2_save=n2
#ifdef SGI
      if(allocated(coeff)) deallocate(coeff)
      allocate( coeff((n1+15) + 2*(n2+15)) )
      call dzfft2dui(n1,n2,coeff)
#endif
#ifdef CRAY
      if(allocated(work)) deallocate(work)
      allocate(work(512*n1))

      if(allocated(table)) deallocate(table)
      allocate(table(100+2*(n1+n2)))

      call scfft2d(0,n1,n2,scale,x,n1/2+1,y,n1+2,table,work,0)
#endif
#ifdef ESSL
      maxnn   = max(n1/2,n2)

      if(allocated(aux1)) deallocate(aux1)
      if(allocated(aux2)) deallocate(aux2)
      if(allocated(aux3)) deallocate(aux3)

      if (maxnn<=2048) then
         naux1= 42000 
      else
         naux1= ceiling(40000+1.64*n1+2.28*n2)
      end if

      if (n1 <= 4096 ) then
         naux2 = 20000
      else if (n1 > 4096 ) then 
         naux2 = ceiling(20000+1.14*n1)
      end if

      if ( n2 > 252) then 
         s1 = min(64, 1+n1/2)
         naux2 = naux2 + ceiling((2*n2+256)*(2.28+s1))
      end if

      naux3=1

      allocate(aux1(naux1))
      allocate(aux2(naux2))
      allocate(aux3(naux3))

      call dcrft2(1,x,n1/2+1,y,n1+2,n1,n2,-1,scale,&
               aux1,naux1,aux2,naux2,aux3,naux3)
#endif
#ifdef DEC
      if (mod(n1,2) /= 0) then
         print *,'pseudo2D: n1 is not even. n1=',n1
      endif
      status=dfft_init_2d(n1,n2,fft_struct,.true.)
      if (status /= 0 ) print *,'status: dfft_init_2d',status
#endif
#if defined(FFTW)
      print *,'Using FFTW for Fourier transform'
      print *,'Feel the power of the Fastest Fourier Transform in the West!'
#endif

      rh_save=rh
      print *,'pseudo2D: Solving for sigma',rh,dx
      a1=0.1e-07
      b1=0.1e-06
      tol=0.1e-10
      call zeroin(func2D,sigma,a1,b1,tol,rh,dx,fval,n1,n2)

      sigma2=sigma**2
      sum=0.0
      do p=-n2/2+1,n2/2
      do l=-n1/2+1,n1/2
#if defined(EXPCOV)
         sum=sum+1./((1.+(kappa2*float(l*l)+lambda2*float(p*p))/sigma2)**3.)
#else
         sum=sum+exp(-2.0*(kappa2*float(l*l)+lambda2*float(p*p))/sigma2)
#endif
      enddo
      enddo
      c=sqrt(1.0/(deltak*sum))

      print *,'pseudo2D: sigma  ',sigma
      print *,'pseudo2D: c=     ',c
   endif

   do j=1,nens

      ! Calculating the random wave phases
      call random_number(phi)
      phi=pi2*phi

      ! Calculating the wave amplitues
      do p=-n2/2,n2/2
      do l=0,n1/2 
#if defined(EXPCOV)
         fampl(l,p,1)=&
            ((1.+(kappa2*float(l*l)+lambda2*float(p*p))/sigma2)**(-1.5))*&
            cos(phi(l,p))*sqrt(deltak)*c
         fampl(l,p,2)=&
            ((1.+(kappa2*float(l*l)+lambda2*float(p*p))/sigma2)**(-1.5))*&
            sin(phi(l,p))*sqrt(deltak)*c
#else
         fampl(l,p,1)=&
            exp(-(kappa2*float(l*l)+lambda2*float(p*p))/sigma2)*&
            cos(phi(l,p))*sqrt(deltak)*c
         fampl(l,p,2)=&
            exp(-(kappa2*float(l*l)+lambda2*float(p*p))/sigma2)*&
            sin(phi(l,p))*sqrt(deltak)*c
#endif
      enddo
      enddo
      fampl(0,0,2)=0.0

      do p=0,n2/2-1
         x(:,p)=cmplx(fampl(:,p,1),fampl(:,p,2))
      enddo

      do p=n2/2,n2-1
         x(:,p)=cmplx(fampl(:,-n2+p,1),fampl(:,-n2+p,2))
      enddo

#ifdef CRAY
      call csfft2d(-1,n1,n2,scale,x,n1/2+1,y,n1+2,table,work,0)
#endif
#ifdef SGI
      call zdfft2du(-1,n1,n2,x,n1+2,coeff)
      y=reshape(transfer(x,(/0.0 /) ),(/n1+2,n2/))
#endif

#ifdef ESSL
      call dcrft2(0,x,n1/2+1,y,n1+2,n1,n2,-1,scale,&
               aux1,naux1,aux2,naux2,aux3,naux3)
#endif

#ifdef DEC
      status=dfft_apply_2d('C','R','B',x,y,n1+2,fft_struct,1,1)
      if (status /= 0 ) print *,'status: dfft_apply_2d',status
      y=y*float(n1*n2)
#endif
#if defined(FFTW)
      !print *,'fft ...',nx,ny,n1,n2
      call dfftw_plan_dft_c2r_2d(plan,n1,n2,x,fftwy,FFTW_ESTIMATE)
      call dfftw_execute(plan)
      call dfftw_destroy_plan(plan)
      !print *,'fft done...'
      y(0:n1-1 ,0:n2-1)=fftwy(1:n1,1:n2)
      y(n1:n1+1,0:n2-1)=fftwy(1:2 ,1:n2)
#endif

      do m=1,ny
      do i=1,nx
         Amat(i,m,j)=y(i-1,m-1)
      enddo
      enddo
      !print *,'min,max Amat:',minval(Amat),maxval(Amat)

   enddo


   deallocate(fampl, phi, y, x)

end subroutine sangoma_pseudornd2D




subroutine zeroin(func,zeropkt,ax,bx,tol,length,dx,fval,n1,n2)
! Finds zero of function f.
! A zero of the function  $func(x,length,dx,n1,n2)$
! is computed in the interval $[ax,bx]$.
! Zeroin| returns a zero $x$ in the given interval
! to within a tolerance  $4*macheps*abs(x) + tol$, where macheps
! is the relative machine precision.

! This function subprogram is a slightly  modified  translation  of
! the algol 60 procedure  zero  given in  richard brent, algorithms for
! minimization without derivatives, prentice - hall, inc. (1973).

   real, external :: func

   integer n1,n2
   real zeropkt,length,dx
   real ax   ! left endpoint of initial interval
   real bx   ! right endpoint of initial interval
   real tol  !  desired length of the interval of uncertainty of the
   real  a,b,c,d,e,eps,fa,fb,fc,tol1,xm,p,q,r,s
   real  abs,sign,fval

!  compute eps, the relative machine precision

#ifdef DEBUG
   write(*,*)'A=',ax,bx,tol,length,dx
#endif
   icorr=0

   eps = 1.0
 10 eps = eps/2.0
   tol1 = 1.0 + eps
   if (tol1 .gt. 1.0) go to 10

! initialization
 77 a = ax
   b = bx
   fa = func(a,length,dx,n1,n2)
   fb = func(b,length,dx,n1,n2)


   if (fa*fb.gt.0.0) then
#ifdef DEBUG
      write(*,*)'fa=',fa
      write(*,*)'fb=',fb
      write(*,*)'fa*fb =',fa*fb,'is greater than zero'
#endif
      ax=0.1*ax
      bx=10.0*bx
      icorr=icorr+1
      if (icorr < 20) then
         goto 77
      else
         write(*,'(2(a,g13.5))')'zeroin: No convergence, ax=',ax,' bx=',bx
         stop  '(zeroin)'
      endif
   endif

! begin step

 20 c = a
   fc = fa
   d = b - a
   e = d
 30 if (abs(fc) .ge. abs(fb)) go to 40
   a = b
   b = c
   c = a
   fa = fb
   fb = fc
   fc = fa

! convergence test

 40 tol1 = 2.0*eps*abs(b) + 0.5*tol
   xm = .5*(c - b)
   if (abs(xm) .le. tol1) go to 90
   if (fb .eq. 0.0) go to 90

! is bisection necessary

   if (abs(e) .lt. tol1) go to 70
   if (abs(fa) .le. abs(fb)) go to 70

! is quadratic interpolation possible

   if (a .ne. c) go to 50

! linear interpolation

   s = fb/fa
   p = 2.0*xm*s
   q = 1.0 - s
   go to 60

! inverse quadratic interpolation

 50 q = fa/fc
   r = fb/fc
   s = fb/fa
   p = s*(2.0*xm*q*(q - r) - (b - a)*(r - 1.0))
   q = (q - 1.0)*(r - 1.0)*(s - 1.0)

! adjust signs

 60 if (p .gt. 0.0) q = -q
   p = abs(p)

! is interpolation acceptable

   if ((2.0*p) .ge. (3.0*xm*q - abs(tol1*q))) go to 70
   if (p .ge. abs(0.5*e*q)) go to 70
   e = d
   d = p/q
   go to 80

! bisection

 70 d = xm
   e = d

! complete step

 80 a = b
   fa = fb
   if (abs(d) .gt. tol1) b = b + d
   if (abs(d) .le. tol1) b = b + sign(tol1, xm)
   fb = func(b,length,dx,n1,n2)
   if ((fb*(fc/abs(fc))) .gt. 0.0) go to 20
   go to 30

! done

 90 zeropkt = b
   fval=func(b,length,dx,n1,n2)
end subroutine zeroin




real function func2D(sigma,length,dx,n1,n2)
! Function used to calculate $sigma$ and $c$.
   implicit none
   real sum1,sum2,sigma,length
   real sigma2,pi2,kappa,kappa2,lambda,lambda2,dx
   integer l,p,n1,n2
   real, parameter :: pi=3.141592653589

   sigma2=sigma**2

   pi2=2.0*pi
   kappa=pi2/(float(n1)*dx)
   kappa2=kappa**2

   lambda=pi2/(float(n2)*dx)
   lambda2=lambda**2


   ! Calculate sum1
   sum1=0.0
   do p=-n2/2+1,n2/2
   do l=-n1/2+1,n1/2
      sum1=sum1+exp(-2.0*(kappa2*float(l*l)+lambda2*float(p*p))/sigma2)&
      *cos(kappa*float(l)*length)
   enddo
   enddo

   ! Calculate sum2
   sum2=0.0
   do p=-n2/2+1,n2/2
   do l=-n1/2+1,n1/2
      sum2=sum2+exp(-2.0*(kappa2*float(l*l)+lambda2*float(p*p))/sigma2)
   enddo
   enddo

   func2D = sum1/sum2 - exp(-1.0)

end function func2D
end module mod_sangoma_pseudornd
