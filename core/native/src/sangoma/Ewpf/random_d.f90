! $Id: random_d.f90 547 2015-02-19 12:32:23Z sp709689 $

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! 
!!!    A module for random number generation from various distributions. 
!!!     
!!!    Copyright (C) 2015  Sanita Vetra-Carvalho
!!!
!!!    This program is distributed under the Lesser General Public License (LGPL) 
!!!    version 3, for more details see <https://www.gnu.org/licenses/lgpl.html>.
!!!
!!!    Email: s.vetra-carvalho @ reading.ac.uk
!!!    Mail:  School of Mathematical and Physical Sciences,
!!!    	      University of Reading,
!!!	      Reading, UK
!!!	      RG6 6BB
!!!
!!!    The original code is public domain written by Alan Miller
!!!    (see http://jblevins.org/mirror/amiller/).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!     A module for random number generation from the following distributions:
!!!
!!!     Distribution                    Function/subroutine name
!!!
!!!     Normal (Gaussian)               random_normal
!!!     Gamma                           random_gamma
!!!     Chi-squared                     random_chisq
!!!     Exponential                     random_exponential
!!!     Weibull                         random_Weibull
!!!     Beta                            random_beta
!!!     t                               random_t
!!!     Multivariate normal             random_mvnorm
!!!     Generalized inverse Gaussian    random_inv_gauss
!!!     Poisson                         random_Poisson
!!!     Binomial                        random_binomial1   *
!!!                                     random_binomial2   *
!!!     Negative binomial               random_neg_binomial
!!!     von Mises                       random_von_Mises
!!!     Cauchy                          random_Cauchy

MODULE random
!  Generate a random ordering of the integers 1 .. N
!                                     random_order
!     Initialize (seed) the uniform random number generator for ANY compiler
!                                     seed_random_number

!     Lognormal - see note below.

!  ** Two functions are provided for the binomial distribution.
!  If the parameter values remain constant, it is recommended that the
!  first function is used (random_binomial1).   If one or both of the
!  parameters change, use the second function (random_binomial2).

! The compilers own random number generator, SUBROUTINE RANDOM_NUMBER(r),
! is used to provide a source of uniformly distributed random numbers.

! N.B. At this stage, only one random number is generated at each call to
!      one of the functions above.

! The module uses the following functions which are included here:
! bin_prob to calculate a single binomial probability
! lngamma  to calculate the logarithm to base e of the gamma function

! Some of the code is adapted from Dagpunar's book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
!
! In most of Dagpunar's routines, there is a test to see whether the value
! of one or two floating-point parameters has changed since the last call.
! These tests have been replaced by using a logical variable FIRST.
! This should be set to .TRUE. on the first call using new values of the
! parameters, and .FALSE. if the parameter values are the same as for the
! previous call.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Lognormal distribution
! If X has a lognormal distribution, then log(X) is normally distributed.
! Here the logarithm is the natural logarithm, that is to base e, sometimes
! denoted as ln.  To generate random variates from this distribution, generate
! a random deviate from the normal distribution with mean and variance equal
! to the mean and variance of the logarithms of X, then take its exponential.

! Relationship between the mean & variance of log(X) and the mean & variance
! of X, when X has a lognormal distribution.
! Let m = mean of log(X), and s^2 = variance of log(X)
! Then
! mean of X     = exp(m + 0.5s^2)
! variance of X = (mean(X))^2.[exp(s^2) - 1]

! In the reverse direction (rarely used)
! variance of log(X) = log[1 + var(X)/(mean(X))^2]
! mean of log(X)     = log(mean(X) - 0.5var(log(X))

! N.B. The above formulae relate to population parameters; they will only be
!      approximate if applied to sample values.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



use sangoma_base, only: REALPREC, INTPREC

IMPLICIT NONE

real(REALPREC), PRIVATE      :: zero = 0.0, half = 0.5, one = 1.0, two = 2.0,   &
                      vsmall = TINY(1.0), vlarge = HUGE(1.0)
INTEGER(INTPREC), PARAMETER :: dp = SELECTED_REAL_KIND(12, 60)


CONTAINS

!> function to get random normal with zero mean and stdev 1
!> @return fn_val
FUNCTION random_normal() RESULT(fn_val)

! Adapted from the following Fortran 77 code
!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

!  The function random_normal() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.

!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
!  and J.F. Monahan augmented with quadratic bounding curves.

real(REALPREC) :: fn_val

!     Local variables
real(REALPREC)     :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472,           &
            r1 = 0.27597, r2 = 0.27846, u, v, x, y, q

!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

DO
  CALL RANDOM_NUMBER(u)
  CALL RANDOM_NUMBER(v)
  v = 1.7156 * (v - half)

!     Evaluate the quadratic form
  x = u - s
  y = ABS(v) - t
  q = x**2 + y*(a*y - b*x)

!     Accept P if inside inner ellipse
  IF (q < r1) EXIT
!     Reject P if outside outer ellipse
  IF (q > r2) CYCLE
!     Reject P if outside acceptance region
  IF (v**2 < -4.0*LOG(u)*u**2) EXIT
END DO

!     Return ratio of P's coordinates as the normal deviate
fn_val = v/u
RETURN

END FUNCTION random_normal


END MODULE random
