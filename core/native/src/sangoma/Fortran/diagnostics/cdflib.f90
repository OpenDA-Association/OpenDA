function algdiv ( a, b )

!*****************************************************************************80
!
!! ALGDIV computes ln ( Gamma ( B ) / Gamma ( A + B ) ) when 8 <= B.
!
!  Discussion:
!
!    In this algorithm, DEL(X) is the function defined by
!
!      ln ( Gamma(X) ) = ( X - 0.5 ) * ln ( X ) - X + 0.5 * ln ( 2 * PI )
!                      + DEL(X).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, define the arguments.
!
!    Output, real ( kind = 8 ) ALGDIV, the value of ln(Gamma(B)/Gamma(A+B)).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) algdiv
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: c0 =  0.833333333333333D-01
  real ( kind = 8 ), parameter :: c1 = -0.277777777760991D-02
  real ( kind = 8 ), parameter :: c2 =  0.793650666825390D-03
  real ( kind = 8 ), parameter :: c3 = -0.595202931351870D-03
  real ( kind = 8 ), parameter :: c4 =  0.837308034031215D-03
  real ( kind = 8 ), parameter :: c5 = -0.165322962780713D-02
  real ( kind = 8 ) d
  real ( kind = 8 ) h
  real ( kind = 8 ) s11
  real ( kind = 8 ) s3
  real ( kind = 8 ) s5
  real ( kind = 8 ) s7
  real ( kind = 8 ) s9
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  if ( b < a ) then
    h = b / a
    c = 1.0D+00 / ( 1.0D+00 + h )
    x = h / ( 1.0D+00 + h )
    d = a + ( b - 0.5D+00 )
  else
    h = a / b
    c = h / ( 1.0D+00 + h )
    x = 1.0D+00 / ( 1.0D+00 + h )
    d = b + ( a - 0.5D+00 )
  end if
!
!  Set SN = (1 - X^N)/(1 - X).
!
  x2 = x * x
  s3 = 1.0D+00 + ( x + x2 )
  s5 = 1.0D+00 + ( x + x2 * s3 )
  s7 = 1.0D+00 + ( x + x2 * s5 )
  s9 = 1.0D+00 + ( x + x2 * s7 )
  s11 = 1.0D+00 + ( x + x2 * s9 )
!
!  Set W = DEL(B) - DEL(A + B).
!
  t = ( 1.0D+00 / b )**2
  w = (((( &
          c5 * s11  * t &
        + c4 * s9 ) * t &
        + c3 * s7 ) * t &
        + c2 * s5 ) * t &
        + c1 * s3 ) * t &
        + c0

  w = w * ( c / b )
!
!  Combine the results.
!
  u = d * alnrel ( a / b )
  v = a * ( log ( b ) - 1.0D+00 )

  if ( v < u ) then
    algdiv = ( w - v ) - u
  else
    algdiv = ( w - u ) - v
  end if

  return
end
function alnrel ( a )

!*****************************************************************************80
!
!! ALNREL evaluates the function ln ( 1 + A ).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the argument.
!
!    Output, real ( kind = 8 ) ALNREL, the value of ln ( 1 + A ).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alnrel
  real ( kind = 8 ), parameter :: p1 = -0.129418923021993D+01
  real ( kind = 8 ), parameter :: p2 =  0.405303492862024D+00
  real ( kind = 8 ), parameter :: p3 = -0.178874546012214D-01
  real ( kind = 8 ), parameter :: q1 = -0.162752256355323D+01
  real ( kind = 8 ), parameter :: q2 =  0.747811014037616D+00
  real ( kind = 8 ), parameter :: q3 = -0.845104217945565D-01
  real ( kind = 8 ) t
  real ( kind = 8 ) t2
  real ( kind = 8 ) w
  real ( kind = 8 ) x

  if ( abs ( a ) <= 0.375D+00 ) then

    t = a / ( a +  2.0D+00  )
    t2 = t * t

    w = ((( p3 * t2 + p2 ) * t2 + p1 ) * t2 + 1.0D+00 ) &
      / ((( q3 * t2 + q2 ) * t2 + q1 ) * t2 + 1.0D+00 )

    alnrel =  2.0D+00  * t * w

  else

    x = 1.0D+00 + real ( a, kind = 8 )
    alnrel = log ( x )

  end if

  return
end
function apser ( a, b, x, eps )

!*****************************************************************************80
!
!! APSER computes the incomplete beta ratio I(SUB(1-X))(B,A).
!
!  Discussion:
!
!    APSER is used only for cases where
!
!      A <= min ( EPS, EPS * B ),
!      B * X <= 1, and
!      X <= 0.5.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, X, the parameters of the
!    incomplete beta ratio.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, real ( kind = 8 ) APSER, the computed value of the
!    incomplete beta ratio.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) aj
  real ( kind = 8 ) apser
  real ( kind = 8 ) b
  real ( kind = 8 ) bx
  real ( kind = 8 ) c
  real ( kind = 8 ) eps
  real ( kind = 8 ), parameter :: g = 0.577215664901533D+00
  real ( kind = 8 ) j
  real ( kind = 8 ) psi
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  real ( kind = 8 ) x

  bx = b * x
  t = x - bx

  if ( b * eps <= 0.02D+00 ) then
    c = log ( x ) + psi ( b ) + g + t
  else
    c = log ( bx ) + g + t
  end if

  tol = 5.0D+00 * eps * abs ( c )
  j = 1.0D+00
  s = 0.0D+00

  do

    j = j + 1.0D+00
    t = t * ( x - bx / j )
    aj = t / j
    s = s + aj

    if ( abs ( aj ) <= tol ) then
      exit
    end if

  end do

  apser = -a * ( c + s )

  return
end
function bcorr ( a0, b0 )

!*****************************************************************************80
!
!! BCORR evaluates DEL(A0) + DEL(B0) - DEL(A0 + B0).
!
!  Discussion:
!
!    The function DEL(A) is a remainder term that is used in the expression:
!
!      ln ( Gamma ( A ) ) = ( A - 0.5 ) * ln ( A )
!        - A + 0.5 * ln ( 2 * PI ) + DEL ( A ),
!
!    or, in other words, DEL ( A ) is defined as:
!
!      DEL ( A ) = ln ( Gamma ( A ) ) - ( A - 0.5 ) * ln ( A )
!        + A + 0.5 * ln ( 2 * PI ).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A0, B0, the arguments.
!    It is assumed that 8 <= A0 and 8 <= B0.
!
!    Output, real ( kind = 8 ) BCORR, the value of the function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) b
  real ( kind = 8 ) b0
  real ( kind = 8 ) bcorr
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: c0 =  0.833333333333333D-01
  real ( kind = 8 ), parameter :: c1 = -0.277777777760991D-02
  real ( kind = 8 ), parameter :: c2 =  0.793650666825390D-03
  real ( kind = 8 ), parameter :: c3 = -0.595202931351870D-03
  real ( kind = 8 ), parameter :: c4 =  0.837308034031215D-03
  real ( kind = 8 ), parameter :: c5 = -0.165322962780713D-02
  real ( kind = 8 ) h
  real ( kind = 8 ) s11
  real ( kind = 8 ) s3
  real ( kind = 8 ) s5
  real ( kind = 8 ) s7
  real ( kind = 8 ) s9
  real ( kind = 8 ) t
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  a = min ( a0, b0 )
  b = max ( a0, b0 )

  h = a / b
  c = h / ( 1.0D+00 + h )
  x = 1.0D+00 / ( 1.0D+00 + h )
  x2 = x * x
!
!  Set SN = (1 - X**N)/(1 - X)
!
  s3 = 1.0D+00 + ( x + x2 )
  s5 = 1.0D+00 + ( x + x2 * s3 )
  s7 = 1.0D+00 + ( x + x2 * s5 )
  s9 = 1.0D+00 + ( x + x2 * s7 )
  s11 = 1.0D+00 + ( x + x2 * s9 )
!
!  Set W = DEL(B) - DEL(A + B)
!
  t = ( 1.0D+00 / b )**2

  w = (((( &
       c5 * s11  * t &
     + c4 * s9 ) * t &
     + c3 * s7 ) * t &
     + c2 * s5 ) * t &
     + c1 * s3 ) * t &
     + c0

  w = w * ( c / b )
!
!  Compute  DEL(A) + W.
!
  t = ( 1.0D+00 / a )**2

  bcorr = ((((( &
         c5   * t &
       + c4 ) * t &
       + c3 ) * t &
       + c2 ) * t &
       + c1 ) * t &
       + c0 ) / a + w

  return
end
function beta ( a, b )

!*****************************************************************************80
!
!! BETA evaluates the beta function.
!
!  Modified:
!
!    03 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the arguments of the beta function.
!
!    Output, real ( kind = 8 ) BETA, the value of the beta function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_log

  beta = exp ( beta_log ( a, b ) )

  return
end
function beta_asym ( a, b, lambda, eps )

!*****************************************************************************80
!
!! BETA_ASYM computes an asymptotic expansion for IX(A,B), for large A and B.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.  It is assumed that both A and B
!    are greater than or equal to 15.
!
!    Input, real ( kind = 8 ) LAMBDA, the value of ( A + B ) * Y - B.
!    It is assumed that 0 <= LAMBDA.
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
  implicit none

  integer ( kind = 4 ), parameter :: num = 20

  real ( kind = 8 ) a
  real ( kind = 8 ) a0(num+1)
  real ( kind = 8 ) b
  real ( kind = 8 ) b0(num+1)
  real ( kind = 8 ) bcorr
  real ( kind = 8 ) beta_asym
  real ( kind = 8 ) bsum
  real ( kind = 8 ) c(num+1)
  real ( kind = 8 ) d(num+1)
  real ( kind = 8 ) dsum
  real ( kind = 8 ), parameter :: e0 = 1.12837916709551D+00
  real ( kind = 8 ), parameter :: e1 = 0.353553390593274D+00
  real ( kind = 8 ) eps
  real ( kind = 8 ) error_fc
  real ( kind = 8 ) f
  real ( kind = 8 ) h
  real ( kind = 8 ) h2
  real ( kind = 8 ) hn
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) j0
  real ( kind = 8 ) j1
  real ( kind = 8 ) lambda
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm1
  integer ( kind = 4 ) mmj
  integer ( kind = 4 ) n
  integer ( kind = 4 ) np1
  real ( kind = 8 ) r
  real ( kind = 8 ) r0
  real ( kind = 8 ) r1
  real ( kind = 8 ) rlog1
  real ( kind = 8 ) s
  real ( kind = 8 ) sum1
  real ( kind = 8 ) t
  real ( kind = 8 ) t0
  real ( kind = 8 ) t1
  real ( kind = 8 ) u
  real ( kind = 8 ) w
  real ( kind = 8 ) w0
  real ( kind = 8 ) z
  real ( kind = 8 ) z0
  real ( kind = 8 ) z2
  real ( kind = 8 ) zn
  real ( kind = 8 ) znm1

  beta_asym = 0.0D+00

  if ( a < b ) then
    h = a / b
    r0 = 1.0D+00 / ( 1.0D+00 + h )
    r1 = ( b - a ) / b
    w0 = 1.0D+00 / sqrt ( a * ( 1.0D+00 + h ))
  else
    h = b / a
    r0 = 1.0D+00 / ( 1.0D+00 + h )
    r1 = ( b - a ) / a
    w0 = 1.0D+00 / sqrt ( b * ( 1.0D+00 + h ))
  end if

  f = a * rlog1 ( - lambda / a ) + b * rlog1 ( lambda / b )
  t = exp ( - f )
  if ( t == 0.0D+00 ) then
    return
  end if

  z0 = sqrt ( f )
  z = 0.5D+00 * ( z0 / e1 )
  z2 = f + f

  a0(1) = ( 2.0D+00 / 3.0D+00 ) * r1
  c(1) = -0.5D+00 * a0(1)
  d(1) = -c(1)
  j0 = ( 0.5D+00 / e0 ) * error_fc ( 1, z0 )
  j1 = e1
  sum1 = j0 + d(1) * w0 * j1

  s = 1.0D+00
  h2 = h * h
  hn = 1.0D+00
  w = w0
  znm1 = z
  zn = z2

  do n = 2, num, 2

    hn = h2 * hn
    a0(n) = 2.0D+00 * r0 * ( 1.0D+00 + h * hn ) &
      / ( n + 2.0D+00 )
    np1 = n + 1
    s = s + hn
    a0(np1) = 2.0D+00 * r1 * s / ( n + 3.0D+00 )

    do i = n, np1

      r = -0.5D+00 * ( i + 1.0D+00 )
      b0(1) = r * a0(1)
      do m = 2, i
        bsum = 0.0D+00
        mm1 = m - 1
        do j = 1, mm1
          mmj = m - j
          bsum = bsum + ( j * r - mmj ) * a0(j) * b0(mmj)
        end do
        b0(m) = r * a0(m) + bsum / m
      end do

      c(i) = b0(i) / ( i + 1.0D+00 )

      dsum = 0.0
      do j = 1, i-1
        dsum = dsum + d(i-j) * c(j)
      end do
      d(i) = - ( dsum + c(i) )

    end do

    j0 = e1 * znm1 + ( n - 1.0D+00 ) * j0
    j1 = e1 * zn + n * j1
    znm1 = z2 * znm1
    zn = z2 * zn
    w = w0 * w
    t0 = d(n) * w * j0
    w = w0 * w
    t1 = d(np1) * w * j1
    sum1 = sum1 + ( t0 + t1 )

    if ( ( abs ( t0 ) + abs ( t1 )) <= eps * sum1 ) then
      u = exp ( - bcorr ( a, b ) )
      beta_asym = e0 * t * u * sum1
      return
    end if

  end do

  u = exp ( - bcorr ( a, b ) )
  beta_asym = e0 * t * u * sum1

  return
end
function beta_frac ( a, b, x, y, lambda, eps )

!*****************************************************************************80
!
!! BETA_FRAC evaluates a continued fraction expansion for IX(A,B).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.  It is assumed that both A and
!    B are greater than 1.
!
!    Input, real ( kind = 8 ) X, Y.  X is the argument of the
!    function, and should satisy 0 <= X <= 1.  Y should equal 1 - X.
!
!    Input, real ( kind = 8 ) LAMBDA, the value of ( A + B ) * Y - B.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, real ( kind = 8 ) BETA_FRAC, the value of the continued
!    fraction approximation for IX(A,B).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) an
  real ( kind = 8 ) anp1
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_frac
  real ( kind = 8 ) beta_rcomp
  real ( kind = 8 ) bn
  real ( kind = 8 ) bnp1
  real ( kind = 8 ) c
  real ( kind = 8 ) c0
  real ( kind = 8 ) c1
  real ( kind = 8 ) e
  real ( kind = 8 ) eps
  real ( kind = 8 ) lambda
  real ( kind = 8 ) n
  real ( kind = 8 ) p
  real ( kind = 8 ) r
  real ( kind = 8 ) r0
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) yp1

  beta_frac = beta_rcomp ( a, b, x, y )

  if ( beta_frac == 0.0D+00 ) then
    return
  end if

  c = 1.0D+00 + lambda
  c0 = b / a
  c1 = 1.0D+00 + 1.0D+00 / a
  yp1 = y + 1.0D+00

  n = 0.0D+00
  p = 1.0D+00
  s = a + 1.0D+00
  an = 0.0D+00
  bn = 1.0D+00
  anp1 = 1.0D+00
  bnp1 = c / c1
  r = c1 / c
!
!  Continued fraction calculation.
!
  do

    n = n + 1.0D+00
    t = n / a
    w = n * ( b - n ) * x
    e = a / s
    alpha = ( p * ( p + c0 ) * e * e ) * ( w * x )
    e = ( 1.0D+00 + t ) / ( c1 + t + t )
    beta = n + w / s + e * ( c + n * yp1 )
    p = 1.0D+00 + t
    s = s +  2.0D+00
!
!  Update AN, BN, ANP1, and BNP1.
!
    t = alpha * an + beta * anp1
    an = anp1
    anp1 = t
    t = alpha * bn + beta * bnp1
    bn = bnp1
    bnp1 = t

    r0 = r
    r = anp1 / bnp1

    if ( abs ( r - r0 ) <= eps * r ) then
      beta_frac = beta_frac * r
      exit
    end if
!
!  Rescale AN, BN, ANP1, and BNP1.
!
    an = an / bnp1
    bn = bn / bnp1
    anp1 = r
    bnp1 = 1.0D+00

  end do

  return
end
subroutine beta_grat ( a, b, x, y, w, eps, ierr )

!*****************************************************************************80
!
!! BETA_GRAT evaluates an asymptotic expansion for IX(A,B).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.  It is assumed that 15 <= A
!    and B <= 1, and that B is less than A.
!
!    Input, real ( kind = 8 ) X, Y.  X is the argument of the
!    function, and should satisy 0 <= X <= 1.  Y should equal 1 - X.
!
!    Input/output, real ( kind = 8 ) W, a quantity to which the
!    result of the computation is to be added on output.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, integer ( kind = 4 ) IERR, an error flag, which is 0 if no error
!    was detected.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) algdiv
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) b
  real ( kind = 8 ) bm1
  real ( kind = 8 ) bp2n
  real ( kind = 8 ) c(30)
  real ( kind = 8 ) cn
  real ( kind = 8 ) coef
  real ( kind = 8 ) d(30)
  real ( kind = 8 ) dj
  real ( kind = 8 ) eps
  real ( kind = 8 ) gam1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) j
  real ( kind = 8 ) l
  real ( kind = 8 ) lnx
  integer ( kind = 4 ) n
  real ( kind = 8 ) n2
  real ( kind = 8 ) nu
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) sum1
  real ( kind = 8 ) t
  real ( kind = 8 ) t2
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  bm1 = ( b - 0.5D+00 ) - 0.5D+00
  nu = a + 0.5D+00 * bm1

  if ( y <= 0.375D+00 ) then
    lnx = alnrel ( - y )
  else
    lnx = log ( x )
  end if

  z = -nu * lnx

  if ( b * z == 0.0D+00 ) then
    ierr = 1
    return
  end if
!
!  Computation of the expansion.
!
!  Set R = EXP(-Z)*Z^B/GAMMA(B)
!
  r = b * ( 1.0D+00 + gam1 ( b ) ) * exp ( b * log ( z ))
  r = r * exp ( a * lnx ) * exp ( 0.5D+00 * bm1 * lnx )
  u = algdiv ( b, a ) + b * log ( nu )
  u = r * exp ( - u )

  if ( u == 0.0D+00 ) then
    ierr = 1
    return
  end if

  call gamma_rat1 ( b, z, r, p, q, eps )

  v = 0.25D+00 * ( 1.0D+00 / nu )**2
  t2 = 0.25D+00 * lnx * lnx
  l = w / u
  j = q / r
  sum1 = j
  t = 1.0D+00
  cn = 1.0D+00
  n2 = 0.0D+00

  do n = 1, 30

    bp2n = b + n2
    j = ( bp2n * ( bp2n + 1.0D+00 ) * j &
      + ( z + bp2n + 1.0D+00 ) * t ) * v
    n2 = n2 +  2.0D+00
    t = t * t2
    cn = cn / ( n2 * ( n2 + 1.0D+00 ))
    c(n) = cn
    s = 0.0D+00

    coef = b - n
    do i = 1, n-1
      s = s + coef * c(i) * d(n-i)
      coef = coef + b
    end do

    d(n) = bm1 * cn + s / n
    dj = d(n) * j
    sum1 = sum1 + dj

    if ( sum1 <= 0.0D+00 ) then
      ierr = 1
      return
    end if

    if ( abs ( dj ) <= eps * ( sum1 + l ) ) then
      ierr = 0
      w = w + u * sum1
      return
    end if

  end do

  ierr = 0
  w = w + u * sum1

  return
end
subroutine beta_inc ( a, b, x, y, w, w1, ierr )

!*****************************************************************************80
!
!! BETA_INC evaluates the incomplete beta function IX(A,B).
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.
!
!    Input, real ( kind = 8 ) X, Y.  X is the argument of the
!    function, and should satisy 0 <= X <= 1.  Y should equal 1 - X.
!
!    Output, real ( kind = 8 ) W, W1, the values of IX(A,B) and
!    1-IX(A,B).
!
!    Output, integer ( kind = 4 ) IERR, the error flag.
!    0, no error was detected.
!    1, A or B is negative;
!    2, A = B = 0;
!    3, X < 0 or 1 < X;
!    4, Y < 0 or 1 < Y;
!    5, X + Y /= 1;
!    6, X = A = 0;
!    7, Y = B = 0.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) apser
  real ( kind = 8 ) b
  real ( kind = 8 ) b0
  real ( kind = 8 ) beta_asym
  real ( kind = 8 ) beta_frac
  real ( kind = 8 ) beta_pser
  real ( kind = 8 ) beta_up
  real ( kind = 8 ) eps
  real ( kind = 8 ) fpser
  integer ( kind = 4 ) ierr
  integer ( kind = 4 ) ierr1
  integer ( kind = 4 ) ind
  real ( kind = 8 ) lambda
  integer ( kind = 4 ) n
  real ( kind = 8 ) t
  real ( kind = 8 ) w
  real ( kind = 8 ) w1
  real ( kind = 8 ) x
  real ( kind = 8 ) x0
  real ( kind = 8 ) y
  real ( kind = 8 ) y0
  real ( kind = 8 ) z

  eps = epsilon ( eps )
  w = 0.0D+00
  w1 = 0.0D+00

  if ( a < 0.0D+00 .or. b < 0.0D+00 ) then
    ierr = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a,i8)' ) '  IERR = ', ierr
    return
  end if

  if ( a == 0.0D+00 .and. b == 0.0D+00 ) then
    ierr = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a,i8)' ) '  IERR = ', ierr
    return
  end if

  if ( x < 0.0D+00 .or. 1.0D+00 < x ) then
    ierr = 3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a,i8)' ) '  IERR = ', ierr
    return
  end if

  if ( y < 0.0D+00 .or. 1.0D+00 < y ) then
    ierr = 4
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a,i8)' ) '  IERR = ', ierr
    return
  end if

  z = ( ( x + y ) - 0.5D+00 ) - 0.5D+00

  if ( 3.0D+00 * eps < abs ( z ) ) then
    ierr = 5
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a,i8)' ) '  IERR = ', ierr
    return
  end if

  ierr = 0

  if ( x == 0.0D+00 ) then
    w = 0.0D+00
    w1 = 1.0D+00
    if ( a == 0.0D+00 ) then
      ierr = 6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BETA_INC - Fatal error!'
      write ( *, '(a,i8)' ) '  IERR = ', ierr
    end if
    return
  end if

  if ( y == 0.0D+00 ) then
    if ( b == 0.0D+00 ) then
      ierr = 7
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BETA_INC - Fatal error!'
      write ( *, '(a,i8)' ) '  IERR = ', ierr
      return
    end if
    w = 1.0D+00
    w1 = 0.0D+00
    return
  end if

  if ( a == 0.0D+00 ) then
    w = 1.0D+00
    w1 = 0.0D+00
    return
  end if

  if ( b == 0.0D+00 ) then
    w = 0.0D+00
    w1 = 1.0D+00
    return
  end if

  eps = max ( eps, 1.0D-15 )

  if ( max ( a, b ) < 0.001D+00 * eps ) then
    go to 260
  end if

  ind = 0
  a0 = a
  b0 = b
  x0 = x
  y0 = y

  if ( 1.0D+00 < min ( a0, b0 ) ) then
    go to 40
  end if
!
!  Procedure for A0 <= 1 or B0 <= 1
!
  if ( 0.5D+00 < x ) then
    ind = 1
    a0 = b
    b0 = a
    x0 = y
    y0 = x
  end if

  if ( b0 < min ( eps, eps * a0 ) ) then
    go to 90
  end if

  if ( a0 < min ( eps, eps * b0 ) .and. b0 * x0 <= 1.0D+00 ) then
    go to 100
  end if

  if ( 1.0D+00 < max ( a0, b0 ) ) then
    go to 20
  end if

  if ( min ( 0.2D+00, b0 ) <= a0 ) then
    go to 110
  end if

  if ( x0**a0 <= 0.9D+00 ) then
    go to 110
  end if

  if ( 0.3D+00 <= x0 ) then
    go to 120
  end if

  n = 20
  go to 140

20 continue

  if ( b0 <= 1.0D+00 ) then
    go to 110
  end if

  if ( 0.3D+00 <= x0 ) then
    go to 120
  end if

  if ( 0.1D+00 <= x0 ) then
    go to 30
  end if

  if ( ( x0 * b0 )**a0 <= 0.7D+00 ) then
    go to 110
  end if

30 continue

  if ( 15.0D+00 < b0 ) then
    go to 150
  end if

  n = 20
  go to 140
!
!  PROCEDURE for 1 < A0 and 1 < B0.
!
40 continue

  if ( a <= b ) then
    lambda = a - ( a + b ) * x
  else
    lambda = ( a + b ) * y - b
  end if

  if ( lambda < 0.0D+00 ) then
    ind = 1
    a0 = b
    b0 = a
    x0 = y
    y0 = x
    lambda = abs ( lambda )
  end if

70 continue

  if ( b0 < 40.0D+00 .and. b0 * x0 <= 0.7D+00 ) then
    go to 110
  end if

  if ( b0 < 40.0D+00 ) then
    go to 160
  end if

  if ( b0 < a0 ) then
    go to 80
  end if

  if ( a0 <= 100.0D+00 ) then
    go to 130
  end if

  if ( 0.03D+00 * a0 < lambda ) then
    go to 130
  end if

  go to 200

80 continue

  if ( b0 <= 100.0D+00 ) then
    go to 130
  end if

  if ( 0.03D+00 * b0 < lambda ) then
    go to 130
  end if

  go to 200
!
!  Evaluation of the appropriate algorithm.
!
90 continue

  w = fpser ( a0, b0, x0, eps )
  w1 = 0.5D+00 + ( 0.5D+00 - w )
  go to 250

100 continue

  w1 = apser ( a0, b0, x0, eps )
  w = 0.5D+00 + ( 0.5D+00 - w1 )
  go to 250

110 continue

  w = beta_pser ( a0, b0, x0, eps )
  w1 = 0.5D+00 + ( 0.5D+00 - w )
  go to 250

120 continue

  w1 = beta_pser ( b0, a0, y0, eps )
  w = 0.5D+00 + ( 0.5D+00 - w1 )
  go to 250

130 continue

  w = beta_frac ( a0, b0, x0, y0, lambda, 15.0D+00 * eps )
  w1 = 0.5D+00 + ( 0.5D+00 - w )
  go to 250

140 continue

  w1 = beta_up ( b0, a0, y0, x0, n, eps )
  b0 = b0 + n

150 continue

  call beta_grat ( b0, a0, y0, x0, w1, 15.0D+00 * eps, ierr1 )
  w = 0.5D+00 + ( 0.5D+00 - w1 )
  go to 250

160 continue

  n = b0
  b0 = b0 - n

  if ( b0 == 0.0D+00 ) then
    n = n - 1
    b0 = 1.0D+00
  end if

170 continue

  w = beta_up ( b0, a0, y0, x0, n, eps )

  if ( x0 <= 0.7D+00 ) then
    w = w + beta_pser ( a0, b0, x0, eps )
    w1 = 0.5D+00 + ( 0.5D+00 - w )
    go to 250
  end if

  if ( a0 <= 15.0D+00 ) then
    n = 20
    w = w + beta_up ( a0, b0, x0, y0, n, eps )
    a0 = a0 + n
  end if

190 continue

  call beta_grat ( a0, b0, x0, y0, w, 15.0D+00 * eps, ierr1 )
  w1 = 0.5D+00 + ( 0.5D+00 - w )
  go to 250

200 continue

  w = beta_asym ( a0, b0, lambda, 100.0D+00 * eps )
  w1 = 0.5D+00 + ( 0.5D+00 - w )
  go to 250
!
!  Termination of the procedure.
!
250 continue

  if ( ind /= 0 ) then
    t = w
    w = w1
    w1 = t
  end if

  return
!
!  Procedure for A and B < 0.001 * EPS
!
260 continue

  w = b / ( a + b )
  w1 = a / ( a + b )

  return
end
subroutine beta_inc_values ( n_data, a, b, x, fx )

!*****************************************************************************80
!
!! BETA_INC_VALUES returns some values of the incomplete Beta function.
!
!  Discussion:
!
!    The incomplete Beta function may be written
!
!      BETA_INC(A,B,X) = Integral (0 to X) T^(A-1) * (1-T)^(B-1) dT
!                      / Integral (0 to 1) T^(A-1) * (1-T)^(B-1) dT
!
!    Thus,
!
!      BETA_INC(A,B,0.0) = 0.0
!      BETA_INC(A,B,1.0) = 1.0
!
!    Note that in Mathematica, the expressions:
!
!      BETA[A,B]   = Integral (0 to 1) T^(A-1) * (1-T)^(B-1) dT
!      BETA[X,A,B] = Integral (0 to X) T^(A-1) * (1-T)^(B-1) dT
!
!    and thus, to evaluate the incomplete Beta function requires:
!
!      BETA_INC(A,B,X) = BETA[X,A,B] / BETA[A,B]
!
!  Modified:
!
!    17 February 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!    Karl Pearson,
!    Tables of the Incomplete Beta Function,
!    Cambridge University Press, 1968.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, B, X, the arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 30

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
     0.5D+00,  0.5D+00,  0.5D+00,  1.0D+00, &
     1.0D+00,  1.0D+00,  1.0D+00,  1.0D+00, &
     2.0D+00,  2.0D+00,  2.0D+00,  2.0D+00, &
     2.0D+00,  2.0D+00,  2.0D+00,  2.0D+00, &
     2.0D+00,  5.5D+00, 10.0D+00, 10.0D+00, &
    10.0D+00, 10.0D+00, 20.0D+00, 20.0D+00, &
    20.0D+00, 20.0D+00, 20.0D+00, 30.0D+00, &
    30.0D+00, 40.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), save, dimension ( n_max ) :: b_vec = (/ &
     0.5D+00,  0.5D+00,  0.5D+00,  0.5D+00, &
     0.5D+00,  0.5D+00,  0.5D+00,  1.0D+00, &
     2.0D+00,  2.0D+00,  2.0D+00,  2.0D+00, &
     2.0D+00,  2.0D+00,  2.0D+00,  2.0D+00, &
     2.0D+00,  5.0D+00,  0.5D+00,  5.0D+00, &
     5.0D+00, 10.0D+00,  5.0D+00, 10.0D+00, &
    10.0D+00, 20.0D+00, 20.0D+00, 10.0D+00, &
    10.0D+00, 20.0D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.0637686D+00, 0.2048328D+00, 1.0000000D+00, 0.0D+00,       &
    0.0050126D+00, 0.0513167D+00, 0.2928932D+00, 0.5000000D+00, &
    0.028D+00,     0.104D+00,     0.216D+00,     0.352D+00,     &
    0.500D+00,     0.648D+00,     0.784D+00,     0.896D+00,     &
    0.972D+00,     0.4361909D+00, 0.1516409D+00, 0.0897827D+00, &
    1.0000000D+00, 0.5000000D+00, 0.4598773D+00, 0.2146816D+00, &
    0.9507365D+00, 0.5000000D+00, 0.8979414D+00, 0.2241297D+00, &
    0.7586405D+00, 0.7001783D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.01D+00, 0.10D+00, 1.00D+00, 0.0D+00,  &
    0.01D+00, 0.10D+00, 0.50D+00, 0.50D+00, &
    0.1D+00,  0.2D+00,  0.3D+00,  0.4D+00,  &
    0.5D+00,  0.6D+00,  0.7D+00,  0.8D+00,  &
    0.9D+00,  0.50D+00, 0.90D+00, 0.50D+00, &
    1.00D+00, 0.50D+00, 0.80D+00, 0.60D+00, &
    0.80D+00, 0.50D+00, 0.60D+00, 0.70D+00, &
    0.80D+00, 0.70D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0.0D+00
    b = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    b = b_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
function beta_log ( a0, b0 )

!*****************************************************************************80
!
!! BETA_LOG evaluates the logarithm of the beta function.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A0, B0, the parameters of the function.
!    A0 and B0 should be nonnegative.
!
!    Output, real ( kind = 8 ) BETA_LOG, the value of the logarithm
!    of the Beta function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) algdiv
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) b
  real ( kind = 8 ) b0
  real ( kind = 8 ) bcorr
  real ( kind = 8 ) beta_log
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: e = 0.918938533204673D+00
  real ( kind = 8 ) gamma_log
  real ( kind = 8 ) gsumln
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) z

  a = min ( a0, b0 )
  b = max ( a0, b0 )
!
!  8 < A.
!
  if ( 8.0D+00 <= a ) then

    w = bcorr ( a, b )
    h = a / b
    c = h / ( 1.0D+00 + h )
    u = - ( a - 0.5D+00 ) * log ( c )
    v = b * alnrel ( h )

    if ( v < u ) then
      beta_log = ((( -0.5D+00 * log ( b ) + e ) + w ) - v ) - u
    else
      beta_log = ((( -0.5D+00 * log ( b ) + e ) + w ) - u ) - v
    end if

    return
  end if
!
!  Procedure when A < 1
!
  if ( a < 1.0D+00 ) then

    if ( b < 8.0D+00 ) then
      beta_log = gamma_log ( a ) + ( gamma_log ( b ) - gamma_log ( a + b ) )
    else
      beta_log = gamma_log ( a ) + algdiv ( a, b )
    end if

    return

  end if
!
!  Procedure when 1 <= A < 8
!
  if ( 2.0D+00 < a ) then
    go to 40
  end if

  if ( b <= 2.0D+00 ) then
    beta_log = gamma_log ( a ) + gamma_log ( b ) - gsumln ( a, b )
    return
  end if

  w = 0.0D+00

  if ( b < 8.0D+00 ) then
    go to 60
  end if

  beta_log = gamma_log ( a ) + algdiv ( a, b )
  return

40 continue
!
!  Reduction of A when 1000 < B.
!
  if ( 1000.0D+00 < b ) then

    n = a - 1.0D+00
    w = 1.0D+00
    do i = 1, n
      a = a - 1.0D+00
      w = w * ( a / ( 1.0D+00 + a / b ))
    end do

    beta_log = ( log ( w ) - n * log ( b ) ) &
      + ( gamma_log ( a ) + algdiv ( a, b ) )

    return
  end if

  n = a - 1.0D+00
  w = 1.0D+00
  do i = 1, n
    a = a - 1.0D+00
    h = a / b
    w = w * ( h / ( 1.0D+00 + h ) )
  end do
  w = log ( w )

  if ( 8.0D+00 <= b ) then
    beta_log = w + gamma_log ( a ) + algdiv ( a, b )
    return
  end if
!
!  Reduction of B when B < 8.
!
60 continue

  n = b - 1.0D+00
  z = 1.0D+00
  do i = 1, n
    b = b - 1.0D+00
    z = z * ( b / ( a + b ))
  end do

  beta_log = w + log ( z ) + ( gamma_log ( a ) + ( gamma_log ( b ) &
    - gsumln ( a, b ) ) )

  return
end
function beta_pser ( a, b, x, eps )

!*****************************************************************************80
!
!! BETA_PSER uses a power series expansion to evaluate IX(A,B)(X).
!
!  Discussion:
!
!    BETA_PSER is used when B <= 1 or B*X <= 0.7.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters.
!
!    Input, real ( kind = 8 ) X, the point where the function
!    is to be evaluated.
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
!    Output, real ( kind = 8 ) BETA_PSER, the approximate value of IX(A,B)(X).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) algdiv
  real ( kind = 8 ) apb
  real ( kind = 8 ) b
  real ( kind = 8 ) b0
  real ( kind = 8 ) beta_log
  real ( kind = 8 ) beta_pser
  real ( kind = 8 ) c
  real ( kind = 8 ) eps
  real ( kind = 8 ) gam1
  real ( kind = 8 ) gamma_ln1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  real ( kind = 8 ) n
  real ( kind = 8 ) sum1
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  real ( kind = 8 ) u
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) z

  beta_pser = 0.0D+00

  if ( x == 0.0D+00 ) then
    return
  end if
!
!  Compute the factor X**A/(A*BETA(A,B))
!
  a0 = min ( a, b )

  if ( 1.0D+00 <= a0 ) then

    z = a * log ( x ) - beta_log ( a, b )
    beta_pser = exp ( z ) / a

  else

    b0 = max ( a, b )

    if ( b0 <= 1.0D+00 ) then

      beta_pser = x**a
      if ( beta_pser == 0.0D+00 ) then
        return
      end if

      apb = a + b

      if ( apb <= 1.0D+00 ) then
        z = 1.0D+00 + gam1 ( apb )
      else
        u = a + b - 1.0D+00
        z = ( 1.0D+00 + gam1 ( u ) ) / apb
      end if

      c = ( 1.0D+00 + gam1 ( a ) ) &
        * ( 1.0D+00 + gam1 ( b ) ) / z
      beta_pser = beta_pser * c * ( b / apb )

    else if ( b0 < 8.0D+00 ) then

      u = gamma_ln1 ( a0 )
      m = b0 - 1.0D+00

      c = 1.0D+00
      do i = 1, m
        b0 = b0 - 1.0D+00
        c = c * ( b0 / ( a0 + b0 ))
      end do

      u = log ( c ) + u
      z = a * log ( x ) - u
      b0 = b0 - 1.0D+00
      apb = a0 + b0

      if ( apb <= 1.0D+00 ) then
        t = 1.0D+00 + gam1 ( apb )
      else
        u = a0 + b0 - 1.0D+00
        t = ( 1.0D+00 + gam1 ( u ) ) / apb
      end if

      beta_pser = exp ( z ) * ( a0 / a ) &
        * ( 1.0D+00 + gam1 ( b0 )) / t

    else if ( 8.0D+00 <= b0 ) then

      u = gamma_ln1 ( a0 ) + algdiv ( a0, b0 )
      z = a * log ( x ) - u
      beta_pser = ( a0 / a ) * exp ( z )

    end if

  end if

  if ( beta_pser == 0.0D+00 .or. a <= 0.1D+00 * eps ) then
    return
  end if
!
!  Compute the series.
!
  sum1 = 0.0D+00
  n = 0.0D+00
  c = 1.0D+00
  tol = eps / a

  do

    n = n + 1.0D+00
    c = c * ( 0.5D+00 + ( 0.5D+00 - b / n ) ) * x
    w = c / ( a + n )
    sum1 = sum1 + w

    if ( abs ( w ) <= tol ) then
      exit
    end if

  end do

  beta_pser = beta_pser * ( 1.0D+00 + a * sum1 )

  return
end
function beta_rcomp ( a, b, x, y )

!*****************************************************************************80
!
!! BETA_RCOMP evaluates X^A * Y^B / Beta(A,B).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the Beta function.
!    A and B should be nonnegative.
!
!    Input, real ( kind = 8 ) X, Y, define the numerator of the fraction.
!
!    Output, real ( kind = 8 ) BETA_RCOMP, the value of X^A * Y^B / Beta(A,B).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) algdiv
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) apb
  real ( kind = 8 ) b
  real ( kind = 8 ) b0
  real ( kind = 8 ) bcorr
  real ( kind = 8 ) beta_log
  real ( kind = 8 ) beta_rcomp
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: const = 0.398942280401433D+00
  real ( kind = 8 ) e
  real ( kind = 8 ) gam1
  real ( kind = 8 ) gamma_ln1
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  real ( kind = 8 ) lambda
  real ( kind = 8 ) lnx
  real ( kind = 8 ) lny
  integer ( kind = 4 ) n
  real ( kind = 8 ) rlog1
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) x
  real ( kind = 8 ) x0
  real ( kind = 8 ) y
  real ( kind = 8 ) y0
  real ( kind = 8 ) z

  beta_rcomp = 0.0D+00
  if ( x == 0.0D+00 .or. y == 0.0D+00 ) then
    return
  end if

  a0 = min ( a, b )

  if ( a0 < 8.0D+00 ) then

    if ( x <= 0.375D+00 ) then
      lnx = log ( x )
      lny = alnrel ( - x )
    else if ( y <= 0.375D+00 ) then
      lnx = alnrel ( - y )
      lny = log ( y )
    else
      lnx = log ( x )
      lny = log ( y )
    end if

    z = a * lnx + b * lny

    if ( 1.0D+00 <= a0 ) then
      z = z - beta_log ( a, b )
      beta_rcomp = exp ( z )
      return
    end if
!
!  Procedure for A < 1 or B < 1
!
    b0 = max ( a, b )

    if ( b0 <= 1.0D+00 ) then

      beta_rcomp = exp ( z )
      if ( beta_rcomp == 0.0D+00 ) then
        return
      end if

      apb = a + b

      if ( apb <= 1.0D+00 ) then
        z = 1.0D+00 + gam1 ( apb )
      else
        u = a + b - 1.0D+00
        z = ( 1.0D+00 + gam1 ( u ) ) / apb
      end if

      c = ( 1.0D+00 + gam1 ( a ) ) &
        * ( 1.0D+00 + gam1 ( b ) ) / z
      beta_rcomp = beta_rcomp * ( a0 * c ) &
        / ( 1.0D+00 + a0 / b0 )

    else if ( b0 < 8.0D+00 ) then

      u = gamma_ln1 ( a0 )
      n = b0 - 1.0D+00

      c = 1.0D+00
      do i = 1, n
        b0 = b0 - 1.0D+00
        c = c * ( b0 / ( a0 + b0 ))
      end do
      u = log ( c ) + u

      z = z - u
      b0 = b0 - 1.0D+00
      apb = a0 + b0

      if ( apb <= 1.0D+00 ) then
        t = 1.0D+00 + gam1 ( apb )
      else
        u = a0 + b0 - 1.0D+00
        t = ( 1.0D+00 + gam1 ( u ) ) / apb
      end if

      beta_rcomp = a0 * exp ( z ) * ( 1.0D+00 + gam1 ( b0 ) ) / t

    else if ( 8.0D+00 <= b0 ) then

      u = gamma_ln1 ( a0 ) + algdiv ( a0, b0 )
      beta_rcomp = a0 * exp ( z - u )

    end if

  else

    if ( a <= b ) then
      h = a / b
      x0 = h / ( 1.0D+00 + h )
      y0 = 1.0D+00 / (  1.0D+00 + h )
      lambda = a - ( a + b ) * x
    else
      h = b / a
      x0 = 1.0D+00 / ( 1.0D+00 + h )
      y0 = h / ( 1.0D+00 + h )
      lambda = ( a + b ) * y - b
    end if

    e = -lambda / a

    if ( abs ( e ) <= 0.6D+00 ) then
      u = rlog1 ( e )
    else
      u = e - log ( x / x0 )
    end if

    e = lambda / b

    if ( abs ( e ) <= 0.6D+00 ) then
      v = rlog1 ( e )
    else
      v = e - log ( y / y0 )
    end if

    z = exp ( - ( a * u + b * v ) )
    beta_rcomp = const * sqrt ( b * x0 ) * z * exp ( - bcorr ( a, b ))

  end if

  return
end
function beta_rcomp1 ( mu, a, b, x, y )

!*****************************************************************************80
!
!! BETA_RCOMP1 evaluates exp(MU) * X^A * Y^B / Beta(A,B).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MU, ?
!
!    Input, real ( kind = 8 ) A, B, the parameters of the Beta function.
!    A and B should be nonnegative.
!
!    Input, real ( kind = 8 ) X, Y, quantities whose powers form part of
!    the expression.
!
!    Output, real ( kind = 8 ) BETA_RCOMP1, the value of
!    exp(MU) * X**A * Y**B / Beta(A,B).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) algdiv
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) apb
  real ( kind = 8 ) b
  real ( kind = 8 ) b0
  real ( kind = 8 ) bcorr
  real ( kind = 8 ) beta_log
  real ( kind = 8 ) beta_rcomp1
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: const = 0.398942280401433D+00
  real ( kind = 8 ) e
  real ( kind = 8 ) esum
  real ( kind = 8 ) gam1
  real ( kind = 8 ) gamma_ln1
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  real ( kind = 8 ) lambda
  real ( kind = 8 ) lnx
  real ( kind = 8 ) lny
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) n
  real ( kind = 8 ) rlog1
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) x
  real ( kind = 8 ) x0
  real ( kind = 8 ) y
  real ( kind = 8 ) y0
  real ( kind = 8 ) z

  a0 = min ( a, b )
!
!  Procedure for 8 <= A and 8 <= B.
!
  if ( 8.0D+00 <= a0 ) then

    if ( a <= b ) then
      h = a / b
      x0 = h / ( 1.0D+00 + h )
      y0 = 1.0D+00 / ( 1.0D+00 + h )
      lambda = a - ( a + b ) * x
    else
      h = b / a
      x0 = 1.0D+00 / ( 1.0D+00 + h )
      y0 = h / ( 1.0D+00 + h )
      lambda = ( a + b ) * y - b
    end if

    e = -lambda / a

    if ( abs ( e ) <= 0.6D+00 ) then
      u = rlog1 ( e )
    else
      u = e - log ( x / x0 )
    end if

    e = lambda / b

    if ( abs ( e ) <= 0.6D+00 ) then
      v = rlog1 ( e )
    else
      v = e - log ( y / y0 )
    end if

    z = esum ( mu, - ( a * u + b * v ))
    beta_rcomp1 = const * sqrt ( b * x0 ) * z * exp ( - bcorr ( a, b ) )
!
!  Procedure for A < 8 or B < 8.
!
  else

    if ( x <= 0.375D+00 ) then
      lnx = log ( x )
      lny = alnrel ( - x )
    else if ( y <= 0.375D+00 ) then
      lnx = alnrel ( - y )
      lny = log ( y )
    else
      lnx = log ( x )
      lny = log ( y )
    end if

    z = a * lnx + b * lny

    if ( 1.0D+00 <= a0 ) then
      z = z - beta_log ( a, b )
      beta_rcomp1 = esum ( mu, z )
      return
    end if
!
!  Procedure for A < 1 or B < 1.
!
    b0 = max ( a, b )

    if ( 8.0D+00 <= b0 ) then
      u = gamma_ln1 ( a0 ) + algdiv ( a0, b0 )
      beta_rcomp1 = a0 * esum ( mu, z-u )
      return
    end if

    if ( 1.0D+00 < b0 ) then
!
!  Algorithm for 1 < B0 < 8
!
      u = gamma_ln1 ( a0 )
      n = b0 - 1.0D+00

      c = 1.0D+00
      do i = 1, n
        b0 = b0 - 1.0D+00
        c = c * ( b0 / ( a0 + b0 ) )
      end do
      u = log ( c ) + u

      z = z - u
      b0 = b0 - 1.0D+00
      apb = a0 + b0

      if ( apb <= 1.0D+00 ) then
        t = 1.0D+00 + gam1 ( apb )
      else
        u = a0 + b0 - 1.0D+00
        t = ( 1.0D+00 + gam1 ( u ) ) / apb
      end if

      beta_rcomp1 = a0 * esum ( mu, z ) &
        * ( 1.0D+00 + gam1 ( b0 ) ) / t
!
!  Algorithm for B0 <= 1
!
    else

      beta_rcomp1 = esum ( mu, z )
      if ( beta_rcomp1 == 0.0D+00 ) then
        return
      end if

      apb = a + b

      if ( apb <= 1.0D+00 ) then
        z = 1.0D+00 + gam1 ( apb )
      else
        u = real ( a, kind = 8 ) + real ( b, kind = 8 ) - 1.0D+00
        z = ( 1.0D+00 + gam1 ( u )) / apb
      end if

      c = ( 1.0D+00 + gam1 ( a ) ) &
        * ( 1.0D+00 + gam1 ( b ) ) / z
      beta_rcomp1 = beta_rcomp1 * ( a0 * c ) / ( 1.0D+00 + a0 / b0 )

    end if

  end if

  return
end
function beta_up ( a, b, x, y, n, eps )

!*****************************************************************************80
!
!! BETA_UP evaluates IX(A,B) - IX(A+N,B) where N is a positive integer.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.
!
!    Input, real ( kind = 8 ) X, Y, ?
!
!    Input, integer ( kind = 4 ) N, the increment to the first argument of IX.
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
!    Output, real ( kind = 8 ) BETA_UP, the value of IX(A,B) - IX(A+N,B).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) ap1
  real ( kind = 8 ) apb
  real ( kind = 8 ) b
  real ( kind = 8 ) beta_rcomp1
  real ( kind = 8 ) beta_up
  real ( kind = 8 ) d
  real ( kind = 8 ) eps
  real ( kind = 8 ) exparg
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real ( kind = 8 ) l
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) n
  real ( kind = 8 ) r
  real ( kind = 8 ) t
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) y
!
!  Obtain the scaling factor EXP(-MU) AND
!  EXP(MU)*(X**A*Y**B/BETA(A,B))/A
!
  apb = a + b
  ap1 = a + 1.0D+00
  mu = 0
  d = 1.0D+00

  if ( n /= 1 ) then

    if ( 1.0D+00 <= a ) then

      if ( 1.1D+00 * ap1 <= apb ) then
        mu = abs ( exparg ( 1 ) )
        k = exparg ( 0 )
        if ( k < mu ) then
          mu = k
        end if
        t = mu
        d = exp ( - t )
      end if

    end if

  end if

  beta_up = beta_rcomp1 ( mu, a, b, x, y ) / a

  if ( n == 1 .or. beta_up == 0.0D+00 ) then
    return
  end if

  w = d
!
!  Let K be the index of the maximum term.
!
  k = 0

  if ( 1.0D+00 < b ) then

    if ( y <= 0.0001D+00 ) then

      k = n - 1

    else

      r = ( b - 1.0D+00 ) * x / y - a

      if ( 1.0D+00 <= r ) then
        k = n - 1
        t = n - 1
        if ( r < t ) then
          k = r
        end if
      end if

    end if
!
!  Add the increasing terms of the series.
!
    do i = 1, k
      l = i - 1
      d = ( ( apb + l ) / ( ap1 + l ) ) * x * d
      w = w + d
    end do

  end if
!
!  Add the remaining terms of the series.
!
  do i = k+1, n-1
    l = i - 1
    d = ( ( apb + l ) / ( ap1 + l ) ) * x * d
    w = w + d
    if ( d <= eps * w ) then
      beta_up = beta_up * w
      return
    end if
  end do

  beta_up = beta_up * w

  return
end
subroutine binomial_cdf_values ( n_data, a, b, x, fx )

!*****************************************************************************80
!
!! BINOMIAL_CDF_VALUES returns some values of the binomial CDF.
!
!  Discussion:
!
!    CDF(X)(A,B) is the probability of at most X successes in A trials,
!    given that the probability of success on a single trial is B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!    Daniel Zwillinger,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition, CRC Press, 1996, pages 651-652.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) A, real ( kind = 8 ) B, integer X, the
!    arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 17

  integer ( kind = 4 ) a
  integer ( kind = 4 ), save, dimension ( n_max ) :: a_vec = (/ &
     2,  2,  2,  2, &
     2,  4,  4,  4, &
     4, 10, 10, 10, &
    10, 10, 10, 10, &
    10 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), save, dimension ( n_max ) :: b_vec = (/ &
    0.05D+00, 0.05D+00, 0.05D+00, 0.50D+00, &
    0.50D+00, 0.25D+00, 0.25D+00, 0.25D+00, &
    0.25D+00, 0.05D+00, 0.10D+00, 0.15D+00, &
    0.20D+00, 0.25D+00, 0.30D+00, 0.40D+00, &
    0.50D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.9025D+00, 0.9975D+00, 1.0000D+00, 0.2500D+00, &
    0.7500D+00, 0.3164D+00, 0.7383D+00, 0.9492D+00, &
    0.9961D+00, 0.9999D+00, 0.9984D+00, 0.9901D+00, &
    0.9672D+00, 0.9219D+00, 0.8497D+00, 0.6331D+00, &
    0.3770D+00 /)
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) x
  integer ( kind = 4 ), save, dimension ( n_max ) :: x_vec = (/ &
     0, 1, 2, 0, &
     1, 0, 1, 2, &
     3, 4, 4, 4, &
     4, 4, 4, 4, &
     4 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0
    b = 0.0D+00
    x = 0
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    b = b_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine cdfbet ( which, p, q, x, y, a, b, status, bound )

!*****************************************************************************80
!
!! CDFBET evaluates the CDF of the Beta Distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the beta distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly by code associated with the reference.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The beta density is proportional to t^(A-1) * (1-t)^(B-1).
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which of the next four 
!    argument values is to be calculated from the others.
!    1: Calculate P and Q from X, Y, A and B;
!    2: Calculate X and Y from P, Q, A and B;
!    3: Calculate A from P, Q, X, Y and B;
!    4: Calculate B from P, Q, X, Y and A.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to X of the
!    chi-square distribution.  Input range: [0, 1].
!
!    Input/output, real ( kind = 8 ) Q, equals 1-P.  Input range: [0, 1].
!
!    Input/output, real ( kind = 8 ) X, the upper limit of integration
!    of the beta density.  If it is an input value, it should lie in
!    the range [0,1].  If it is an output value, it will be searched for
!    in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Y, equal to 1-X.  If it is an input
!    value, it should lie in the range [0,1].  If it is an output value,
!    it will be searched for in the range [0,1].
!
!    Input/output, real ( kind = 8 ) A, the first parameter of the beta
!    density.  If it is an input value, it should lie in the range
!    (0, +infinity).  If it is an output value, it will be searched
!    for in the range [1D-300,1D300].
!
!    Input/output, real ( kind = 8 ) B, the second parameter of the beta
!    density.  If it is an input value, it should lie in the range
!    (0, +infinity).  If it is an output value, it will be searched
!    for in the range [1D-300,1D300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +4, if X + Y /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) b
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+300
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which
  real ( kind = 8 ) x
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) y

  status = 0
  bound = 0.0D+00

  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFBET - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if

  if ( 4 < which ) then
    bound = 4.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFBET - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      bound = 0.0D+00
      status = -2
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      bound = 1.0D+00
      status = -2
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      bound = 0.0D+00
      status = -3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      bound = 1.0D+00
      status = -3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Unless X is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( x < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter X is out of range.'
      return
    else if ( 1.0D+00 < x ) then
      bound = 1.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter X is out of range.'
      return
    end if
  end if
!
!  Unless Y is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( y < 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Y is out of range.'
      return
    else if ( 1.0D+00 < y ) then
      bound = 1.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Y is out of range.'
      return
    end if
  end if
!
!  Unless A is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( a <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter A is out of range.'
      return
    end if
  end if
!
!  Unless B is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( b <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -7
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter B is out of range.'
      return
    end if
  end if
!
!  Check that P + Q = 1.
!
  if ( which /= 1 ) then
    if ( 3.0D+00 * epsilon ( p ) < abs ( ( p + q ) - 1.0D+00 ) ) then
      status = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Check that X + Y = 1.
!
  if ( which /= 2 ) then
    if ( 3.0D+00 * epsilon ( x ) < abs ( ( x + y ) - 1.0D+00 ) ) then
      status = 4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  X + Y /= 1.'
      return
    end if
  end if
!
!  Compute P and Q.
!
  if ( which == 1 ) then

    call cumbet ( x, y, a, b, p, q )
    status = 0
!
!  Compute X and Y.
!
  else if ( which == 2 ) then

    call dstzr ( 0.0D+00, 1.0D+00, atol, tol )

    if ( p <= q ) then

      status = 0
      fx = 0.0D+00
      call dzror ( status, x, fx, xlo, xhi, qleft, qhi )
      y = 1.0D+00 - x

      do while ( status == 1 )

        call cumbet ( x, y, a, b, cum, ccum )
        fx = cum - p
        call dzror ( status, x, fx, xlo, xhi, qleft, qhi )
        y = 1.0D+00 - x

      end do

    else

      status = 0
      fx = 0.0D+00
      call dzror ( status, y, fx, xlo, xhi, qleft, qhi )
      x = 1.0D+00 - y

      do while ( status == 1 )

        call cumbet ( x, y, a, b, cum, ccum )
        fx = ccum - q
        call dzror ( status, y, fx, xlo, xhi, qleft, qhi )
        x = 1.0D+00 - y

      end do

    end if

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = 1.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Compute A.
!
  else if ( which == 3 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    a = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, a, fx, qleft, qhi )

    do while ( status == 1 )

      call cumbet ( x, y, a, b, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, a, fx, qleft, qhi )

    end do

    if ( status == -1 ) then

      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if

    end if
!
!  Compute B.
!
  else if ( which == 4 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    b = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, b, fx, qleft, qhi )

    do while ( status == 1 )

      call cumbet ( x, y, a, b, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, b, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if

  end if

  return
end
subroutine cdfbin ( which, p, q, s, xn, pr, ompr, status, bound )

!*****************************************************************************80
!
!! CDFBIN evaluates the CDF of the Binomial distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the binomial distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    P is the probablility of S or fewer successes in XN binomial trials,
!    each trial having an individual probability of success of PR.
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.24.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which of argument values is to
!    be calculated from the others.
!    1: Calculate P and Q from S, XN, PR and OMPR;
!    2: Calculate S from P, Q, XN, PR and OMPR;
!    3: Calculate XN from P, Q, S, PR and OMPR;
!    4: Calculate PR and OMPR from P, Q, S and XN.
!
!    Input/output, real ( kind = 8 ) P, the cumulation, from 0 to S,
!    of the binomial distribution.  If P is an input value, it should
!    lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) S, the number of successes observed.
!    Whether this is an input or output value, it should lie in the
!    range [0,XN].
!
!    Input/output, real ( kind = 8 ) XN, the number of binomial trials.
!    If this is an input value it should lie in the range: (0, +infinity).
!    If it is an output value it will be searched for in the
!    range [1.0D-300, 1.0D+300].
!
!    Input/output, real ( kind = 8 ) PR, the probability of success in each
!    binomial trial.  Whether this is an input or output value, it should
!    lie in the range: [0,1].
!
!    Input/output, real ( kind = 8 ) OMPR, equal to 1-PR.  Whether this is an
!    input or output value, it should lie in the range [0,1].  Also, it should
!    be the case that PR + OMPR = 1.
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +4, if PR + OMPR /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+300
  real ( kind = 8 ) ompr
  real ( kind = 8 ) p
  real ( kind = 8 ) pr
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  real ( kind = 8 ) s
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xn

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFBET - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if

  if ( 4 < which ) then
    bound = 4.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFBET - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      status = -3
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Unless XN is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( xn <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter XN is out of range.'
      return
    end if
  end if
!
!  Unless S is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( s < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter S is out of range.'
      return
    else if ( which /= 3 .and. xn < s ) then
      bound = xn
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter S is out of range.'
      return
    end if
  end if
!
!  Unless PR is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( pr < 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter PR is out of range.'
      return
    else if ( 1.0D+00 < pr ) then
      bound = 1.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter PR is out of range.'
      return
    end if
  end if
!
!  Unless OMPR is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( ompr < 0.0D+00 ) then
      bound = 0.0D+00
      status = -7
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter OMPR is out of range.'
      return
    else if ( 1.0D+00 < ompr ) then
      bound = 1.0D+00
      status = -7
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter OMPR is out of range.'
      return
    end if
  end if
!
!  Check that P + Q = 1.
!
  if ( which /= 1 ) then
    if ( 3.0D+00 * epsilon ( p ) < abs ( ( p + q ) - 1.0D+00 ) ) then
      status = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Check that PR + OMPR = 1.
!
  if ( which /= 4 ) then
    if ( 3.0D+00 * epsilon ( 1.0D+00 ) &
      < abs ( ( pr + ompr ) - 1.0D+00 ) ) then
      status = 4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  PR + OMPR /= 1.'
      return
    end if
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    call cumbin ( s, xn, pr, ompr, p, q )
    status = 0
!
!  Calculate S.
!
  else if ( which == 2 ) then

    call dstinv ( 0.0D+00, xn, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    s = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, s, fx, qleft, qhi )

    do while ( status == 1 )

      call cumbin ( s, xn, pr, ompr, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, s, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBIN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = xn
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBIN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate XN.
!
  else if ( which == 3 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    xn = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, xn, fx, qleft, qhi )

    do while ( status == 1 )

      call cumbin ( s, xn, pr, ompr, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, xn, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBIN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
        return
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBIN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
        return
      end if
    end if
!
!  Calculate PR and OMPR.
!
  else if ( which == 4 ) then

    call dstzr ( 0.0D+00, 1.0D+00, atol, tol )

    if ( p <= q ) then

      status = 0
      call dzror ( status, pr, fx, xlo, xhi, qleft, qhi )
      ompr = 1.0D+00 - pr

      do while ( status == 1 )

        call cumbin ( s, xn, pr, ompr, cum, ccum )
        fx = cum - p
        call dzror ( status, pr, fx, xlo, xhi, qleft, qhi )
        ompr = 1.0D+00 - pr

      end do

    else

      status = 0
      call dzror ( status, ompr, fx, xlo, xhi, qleft, qhi )
      pr = 1.0D+00 - ompr

      do while ( status == 1 )

        call cumbin ( s, xn, pr, ompr, cum, ccum )
        fx = ccum - q
        call dzror ( status, ompr, fx, xlo, xhi, qleft, qhi )
        pr = 1.0D+00 - ompr

      end do

    end if

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBIN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = 1.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBIN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if

  end if

  return
end
subroutine cdfchi ( which, p, q, x, df, status, bound )

!*****************************************************************************80
!
!! CDFCHI evaluates the CDF of the chi square distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the chi square distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The CDF of the chi square distribution can be evaluated
!    within Mathematica by commands such as:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF [ ChiSquareDistribution [ DF ], X ]
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.4.19.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from X and DF;
!    2: Calculate X from P, Q and DF;
!    3: Calculate DF from P, Q and X.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to X of
!    the chi-square distribution.  If this is an input value, it should
!    lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) X, the upper limit of integration
!    of the chi-square distribution.  If this is an input
!    value, it should lie in the range: [0, +infinity).  If it is an output
!    value, it will be searched for in the range: [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) DF, the degrees of freedom of the
!    chi-square distribution.  If this is an input value, it should lie
!    in the range: (0, +infinity).  If it is an output value, it will be
!    searched for in the range: [ 1.0D-300, 1.0D+300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +10, an error was returned from CUMGAM.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+300
  real ( kind = 8 ) p
  real ( kind = 8 ) porq
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which
  real ( kind = 8 ) x

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFCHI - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 3.'
    return
  end if

  if ( 3 < which ) then
    bound = 3.0
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFCHI - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 3.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      status = -3
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Unless X is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( x < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter X is out of range.'
      return
    end if
  end if
!
!  Unless DF is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( df <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DF is out of range.'
      return
    end if
  end if
!
!  Check that P + Q = 1.
!
  if ( which /= 1 ) then
    if ( 3.0D+00 * epsilon ( p ) < abs ( ( p + q ) - 1.0D+00 ) ) then
      status = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHI - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Select the minimum of P and Q.
!
  if ( which /= 1 ) then
    porq = min ( p, q )
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    status = 0
    call cumchi ( x, df, p, q )

    if ( 1.5D+00 < porq ) then
      status = 10
      return
    end if
!
!  Calculate X.
!
  else if ( which == 2 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    x = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, x, fx, qleft, qhi )

    do while ( status == 1 )

      call cumchi ( x, df, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      if ( 1.5D+00 < fx + porq ) then
        status = 10
        return
      end if

      call dinvr ( status, x, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if

    end if
!
!  Calculate DF.
!
  else if ( which == 3 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    df = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, df, fx, qleft, qhi )

    do while ( status == 1 )

      call cumchi ( x, df, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      if ( 1.5D+00 < fx + porq ) then
        status = 10
        return
      end if

      call dinvr ( status, df, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if

  end if

  return
end
subroutine cdfchn ( which, p, q, x, df, pnonc, status, bound )

!*****************************************************************************80
!
!! CDFCHN evaluates the CDF of the Noncentral Chi-Square.
!
!  Discussion:
!
!    This routine calculates any one parameter of the noncentral chi-square
!    distribution given values for the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The computation time required for this routine is proportional
!    to the noncentrality parameter (PNONC).  Very large values of
!    this parameter can consume immense computer resources.  This is
!    why the search range is bounded by 10,000.
!
!    The CDF of the noncentral chi square distribution can be evaluated
!    within Mathematica by commands such as:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF[ NoncentralChiSquareDistribution [ DF, LAMBDA ], X ]
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.25.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from X, DF and PNONC;
!    2: Calculate X from P, DF and PNONC;
!    3: Calculate DF from P, X and PNONC;
!    4: Calculate PNONC from P, X and DF.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to X of
!    the noncentral chi-square distribution.  If this is an input
!    value, it should lie in the range: [0, 1.0-1.0D-16).
!
!    Input/output, real ( kind = 8 ) Q, is generally not used by this
!    subroutine and is only included for similarity with other routines.
!    However, if P is to be computed, then a value will also be computed
!    for Q.
!
!    Input, real ( kind = 8 ) X, the upper limit of integration of the
!    noncentral chi-square distribution.  If this is an input value, it
!    should lie in the range: [0, +infinity).  If it is an output value,
!    it will be sought in the range: [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) DF, the number of degrees of freedom
!    of the noncentral chi-square distribution.  If this is an input value,
!    it should lie in the range: (0, +infinity).  If it is an output value,
!    it will be searched for in the range: [ 1.0D-300, 1.0D+300].
!
!    Input/output, real ( kind = 8 ) PNONC, the noncentrality parameter of
!    the noncentral chi-square distribution.  If this is an input value, it
!    should lie in the range: [0, +infinity).  If it is an output value,
!    it will be searched for in the range: [0,1.0D+4]
!
!    Output, integer ( kind = 4 ) STATUS, reports on the calculation.
!    0, if calculation completed correctly;
!    -I, if input parameter number I is out of range;
!    1, if the answer appears to be lower than the lowest search bound;
!    2, if the answer appears to be higher than the greatest search bound.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol=1.0D-50
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf=1.0D+300
  real ( kind = 8 ) p
  real ( kind = 8 ) pnonc
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tent4=1.0D+04
  real ( kind = 8 ), parameter :: tol=1.0D-08
  integer ( kind = 4 ) which
  real ( kind = 8 ) x

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFCHN - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if

  if ( 4 < which ) then
    bound = 4.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFCHN - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless X is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( x < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter X is out of range.'
      return
    end if
  end if
!
!  Unless DF is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( df <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DF is out of range.'
      return
    end if
  end if
!
!  Unless PNONC is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( pnonc < 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter PNONC is out of range.'
      return
    end if
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    call cumchn ( x, df, pnonc, p, q )
    status = 0
!
!  Calculate X.
!
  else if ( which == 2 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    x = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, x, fx, qleft, qhi )

    do while ( status == 1 )

      call cumchn ( x, df, pnonc, cum, ccum )
      fx = cum - p
      call dinvr ( status, x, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate DF.
!
  else if ( which == 3 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    df = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, df, fx, qleft, qhi )

    do while ( status == 1 )

      call cumchn ( x, df, pnonc, cum, ccum )
      fx = cum - p
      call dinvr ( status, df, fx, qleft, qhi )

    end do

    if ( status == -1 )then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate PNONC.
!
  else if ( which == 4 ) then

    call dstinv ( 0.0D+00, tent4, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    pnonc = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, pnonc, fx, qleft, qhi )

    do while ( status == 1 )

      call cumchn ( x, df, pnonc, cum, ccum )
      fx = cum - p
      call dinvr ( status, pnonc, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = tent4
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if

  end if

  return
end
subroutine cdff ( which, p, q, f, dfn, dfd, status, bound )

!*****************************************************************************80
!
!! CDFF evaluates the CDF of the F distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the F distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The value of the cumulative F distribution is not necessarily
!    monotone in either degrees of freedom.  There thus may be two
!    values that provide a given CDF value.  This routine assumes
!    monotonicity and will find an arbitrary one of the two values.
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.6.2.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from F, DFN and DFD;
!    2: Calculate F from P, Q, DFN and DFD;
!    3: Calculate DFN from P, Q, F and DFD;
!    4: Calculate DFD from P, Q, F and DFN.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to F of
!    the F-density.  If it is an input value, it should lie in the
!    range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) F, the upper limit of integration
!    of the F-density.  If this is an input value, it should lie in the
!    range [0, +infinity).  If it is an output value, it will be searched
!    for in the range [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) DFN, the number of degrees of
!    freedom of the numerator sum of squares.  If this is an input value,
!    it should lie in the range: (0, +infinity).  If it is an output value,
!    it will be searched for in the range: [ 1.0D-300, 1.0D+300].
!
!    Input/output, real ( kind = 8 ) DFD, the number of degrees of freedom
!    of the denominator sum of squares.  If this is an input value, it should
!    lie in the range: (0, +infinity).  If it is an output value, it will
!    be searched for in the  range: [ 1.0D-300, 1.0D+300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) bound
  real ( kind = 8 ) bound_hi
  real ( kind = 8 ) bound_lo
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) dfd
  real ( kind = 8 ) dfn
  real ( kind = 8 ) f
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+300
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFF - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if

  if ( 4 < which ) then
    bound = 4.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFF - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      status = -3
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Unless F is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( f < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter F is out of range.'
      return
    end if
  end if
!
!  Unless DFN is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( dfn <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DFN is out of range.'
      return
    end if
  end if
!
!  Unless DFD is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( dfd <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DFD is out of range.'
      return
    end if
  end if
!
!  Check that P + Q = 1.
!
  if ( which /= 1 ) then
    if ( 3.0D+00 * epsilon ( p ) < abs ( ( p + q ) - 1.0D+00 ) ) then
      status = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    call cumf ( f, dfn, dfd, p, q )
    status = 0
!
!  Calculate F.
!
  else if ( which == 2 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    f = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, f, fx, qleft, qhi )

    do while ( status == 1 )

      call cumf ( f, dfn, dfd, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, f, fx, qleft, qhi )

    end do

    if ( status == -1 ) then

      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if

    end if
!
!  Calculate DFN.
!
!  Note that, in the original calculation, the lower bound for DFN was 0.
!  Using DFN = 0 causes an error in CUMF when it calls BETA_INC.
!  The lower bound was set to the more reasonable value of 1.
!  JVB, 14 April 2007.
!
  else if ( which == 3 ) then

    bound_lo = 1.0D+00
    bound_hi = inf

    call dstinv ( bound_lo, bound_hi, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    dfn = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, dfn, fx, qleft, qhi )

    do while ( status == 1 )

      call cumf ( f, dfn, dfd, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, dfn, fx, qleft, qhi )

    end do

    if ( status == -1 ) then

      if ( qleft ) then
        status = 1
        bound = bound_lo
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound_lo
        return
      else
        status = 2
        bound = bound_hi
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound_hi
        return
      end if

    end if
!
!  Calculate DFD.
!
!  Note that, in the original calculation, the lower bound for DFD was 0.
!  Using DFD = 0 causes an error in CUMF when it calls BETA_INC.
!  The lower bound was set to the more reasonable value of 1.
!  JVB, 14 April 2007.
!
  else if ( which == 4 ) then

    bound_lo = 1.0D+00
    bound_hi = inf

    call dstinv ( bound_lo, bound_hi, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    dfd = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, dfd, fx, qleft, qhi )

    do while ( status == 1 )

      call cumf ( f, dfn, dfd, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, dfd, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = bound_lo
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound_lo
      else
        status = 2
        bound = bound_hi
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound_hi
      end if
    end if

  end if

  return
end
subroutine cdffnc ( which, p, q, f, dfn, dfd, pnonc, status, bound )

!*****************************************************************************80
!
!! CDFFNC evaluates the CDF of the Noncentral F distribution.
!
!  Discussion:
!
!    This routine originally used 1.0D+300 as the upper bound for the
!    interval in which many of the missing parameters are to be sought.
!    Since the underlying rootfinder routine needs to evaluate the
!    function at this point, it is no surprise that the program was
!    experiencing overflows.  A less extravagant upper bound
!    is being tried for now!
!
!    This routine calculates any one parameter of the Noncentral F distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The computation time required for this routine is proportional
!    to the noncentrality parameter PNONC.  Very large values of
!    this parameter can consume immense computer resources.  This is
!    why the search range is bounded by 10,000.
!
!    The value of the cumulative noncentral F distribution is not
!    necessarily monotone in either degree of freedom.  There thus
!    may be two values that provide a given CDF value.  This routine
!    assumes monotonicity and will find an arbitrary one of the two
!    values.
!
!    The CDF of the noncentral F distribution can be evaluated
!    within Mathematica by commands such as:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF [ NoncentralFRatioDistribution [ DFN, DFD, PNONC ], X ]
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.6.20.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from F, DFN, DFD and PNONC;
!    2: Calculate F from P, Q, DFN, DFD and PNONC;
!    3: Calculate DFN from P, Q, F, DFD and PNONC;
!    4: Calculate DFD from P, Q, F, DFN and PNONC;
!    5: Calculate PNONC from P, Q, F, DFN and DFD.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to F of
!    the noncentral F-density.  If P is an input value it should
!    lie in the range [0,1) (Not including 1!).
!
!    Dummy, real ( kind = 8 ) Q, is not used by this subroutine,
!    and is only included for similarity with the other routines.
!    Its input value is not checked.  If P is to be computed, the
!    Q is set to 1 - P.
!
!    Input/output, real ( kind = 8 ) F, the upper limit of integration
!    of the noncentral F-density.  If this is an input value, it should
!    lie in the range: [0, +infinity).  If it is an output value, it
!    will be searched for in the range: [0,1.0D+30].
!
!    Input/output, real ( kind = 8 ) DFN, the number of degrees of freedom
!    of the numerator sum of squares.  If this is an input value, it should
!    lie in the range: (0, +infinity).  If it is an output value, it will
!    be searched for in the range: [ 1.0, 1.0D+30].
!
!    Input/output, real ( kind = 8 ) DFD, the number of degrees of freedom
!    of the denominator sum of squares.  If this is an input value, it should
!    be in range: (0, +infinity).  If it is an output value, it will be
!    searched for in the range [1.0, 1.0D+30].
!
!    Input/output, real ( kind = 8 ) PNONC, the noncentrality parameter
!    If this is an input value, it should be nonnegative.
!    If it is an output value, it will be searched for in the range: [0,1.0D+4].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) dfd
  real ( kind = 8 ) dfn
  real ( kind = 8 ) f
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+30
  real ( kind = 8 ) p
  real ( kind = 8 ) pnonc
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tent4 = 1.0D+04
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFFNC - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 5.'
    return
  end if

  if ( 5 < which ) then
    bound = 5.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFFNC - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 5.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless F is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( f < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter F is out of range.'
      return
    end if
  end if
!
!  Unless DFN is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( dfn <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DFN is out of range.'
      return
    end if
  end if
!
!  Unless DFD is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( dfd <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DFD is out of range.'
      return
    end if
  end if
!
!  Unless PNONC is to be computed, make sure it is legal.
!
  if ( which /= 5 ) then
    if ( pnonc < 0.0D+00 ) then
      bound = 0.0D+00
      status = -7
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter PNONC is out of range.'
      return
    end if
  end if
!
! Calculate P and Q.
!
  if ( which == 1 ) then

    call cumfnc ( f, dfn, dfd, pnonc, p, q )
    status = 0
!
!  Calculate F.
!
  else if ( which == 2 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    f = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, f, fx, qleft, qhi )

    do while ( status == 1 )

      call cumfnc ( f, dfn, dfd, pnonc, cum, ccum )
      fx = cum - p
      call dinvr ( status, f, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
        return
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
        return
      end if
    end if
!
!  Calculate DFN.
!
  else if ( which == 3 ) then

    call dstinv ( 1.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    dfn = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, dfn, fx, qleft, qhi )

    do while ( status == 1 )

      call cumfnc ( f, dfn, dfd, pnonc, cum, ccum )
      fx = cum - p

      call dinvr ( status, dfn, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate DFD.
!
  else if ( which == 4 ) then

    call dstinv ( 1.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    dfd = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, dfd, fx, qleft, qhi )

    do while ( status == 1 )

      call cumfnc ( f, dfn, dfd, pnonc, cum, ccum )
      fx = cum - p
      call dinvr ( status, dfd, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate PNONC.
!
  else if ( which == 5 ) then

    call dstinv ( 0.0D+00, tent4, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    pnonc = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, pnonc, fx, qleft, qhi )

    do while ( status == 1 )

      call cumfnc ( f, dfn, dfd, pnonc, cum, ccum )
      fx = cum - p

      call dinvr ( status, pnonc, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = tent4
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if

  end if

  return
end
subroutine cdfgam ( which, p, q, x, shape, scale, status, bound )

!*****************************************************************************80
!
!! CDFGAM evaluates the CDF of the Gamma Distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the Gamma distribution
!    given the others.
!
!    The cumulative distribution function P is calculated directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The gamma density is proportional to T**(SHAPE - 1) * EXP(- SCALE * T)
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 654:
!    Computation of the incomplete gamma function ratios and their inverse,
!    ACM Transactions on Mathematical Software,
!    Volume 12, 1986, pages 377-393.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from X, SHAPE and SCALE;
!    2: Calculate X from P, Q, SHAPE and SCALE;
!    3: Calculate SHAPE from P, Q, X and SCALE;
!    4: Calculate SCALE from P, Q, X and SHAPE.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to X of the
!    Gamma density.  If this is an input value, it should lie in the
!    range: [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) X, the upper limit of integration of
!    the Gamma density.  If this is an input value, it should lie in the
!    range: [0, +infinity).  If it is an output value, it will lie in
!    the range: [0,1E300].
!
!    Input/output, real ( kind = 8 ) SHAPE, the shape parameter of the
!    Gamma density.  If this is an input value, it should lie in the range:
!    (0, +infinity).  If it is an output value, it will be searched for
!    in the range: [1.0D-300,1.0D+300].
!
!    Input/output, real ( kind = 8 ) SCALE, the scale parameter of the
!    Gamma density.  If this is an input value, it should lie in the range
!    (0, +infinity).  If it is an output value, it will be searched for
!    in the range: (1.0D-300,1.0D+300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +10, if the Gamma or inverse Gamma routine cannot compute the answer.
!    This usually happens only for X and SHAPE very large (more than 1.0D+10.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) fx
  integer ( kind = 4 ) ierr
  real ( kind = 8 ), parameter :: inf=1.0D+300
  real ( kind = 8 ) p
  real ( kind = 8 ) porq
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  real ( kind = 8 ) scale
  real ( kind = 8 ) shape
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) status,which
  real ( kind = 8 ) x
  real ( kind = 8 ) xscale
  real ( kind = 8 ) xx

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFGAM - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if

  if ( 4 < which ) then
    bound = 4.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFGAM - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      status = -3
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Unless X is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( x < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter X is out of range.'
      return
    end if
  end if
!
!  Unless SHAPE is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( shape <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter SHAPE is out of range.'
      return
    end if
  end if
!
!  Unless SCALE is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( scale <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter SCALE is out of range.'
      return
    end if
  end if
!
!  Check that P + Q = 1.
!
  if ( which /= 1 ) then
    if ( 3.0D+00 * epsilon ( p ) < abs ( ( p + q ) - 1.0D+0 ) ) then
      status = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Select the minimum of P or Q.
!
  if ( which /= 1 ) then
    porq = min ( p, q )
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    status = 0
    xscale = x * scale
    call cumgam ( xscale, shape, p, q )

    if ( 1.5D+00 < porq ) then
      status = 10
    end if
!
!  Calculate X.
!
  else if ( which == 2 ) then

    call gamma_inc_inv ( shape, xx, -1.0D+00, p, q, ierr )

    if ( ierr < 0.0D+00 ) then
      status = 10
      return
    end if

    x = xx / scale
    status = 0
!
!  Calculate SHAPE.
!
  else if ( which == 3 ) then

    xscale = x * scale

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    shape = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, shape, fx, qleft, qhi )

    do while ( status == 1 )

      call cumgam ( xscale, shape, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      if ( p <= q .and. 1.5D+00 < cum ) then
        status = 10
        return
      else if ( q < p .and. 1.5D+00 < ccum ) then
        status = 10
        return
      end if

      call dinvr ( status, shape, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFGAM - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFGAM - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate SCALE.
!
  else if ( which == 4 ) then

    call gamma_inc_inv ( shape, xx, -1.0D+00, p, q, ierr )

    if ( ierr < 0.0D+00 ) then
      status = 10
    else
      scale = xx / x
      status = 0
    end if

  end if

  return
end
subroutine cdfnbn ( which, p, q, f, s, pr, ompr, status, bound )

!*****************************************************************************80
!
!! CDFNBN evaluates the CDF of the Negative Binomial distribution
!
!  Discussion:
!
!    This routine calculates any one parameter of the negative binomial
!    distribution given values for the others.
!
!    The cumulative negative binomial distribution returns the
!    probability that there will be F or fewer failures before the
!    S-th success in binomial trials each of which has probability of
!    success PR.
!
!    The individual term of the negative binomial is the probability of
!    F failures before S successes and is
!    Choose( F, S+F-1 ) * PR^(S) * (1-PR)^F
!
!    Computation of other parameters involve a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.26.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from F, S, PR and OMPR;
!    2: Calculate F from P, Q, S, PR and OMPR;
!    3: Calculate S from P, Q, F, PR and OMPR;
!    4: Calculate PR and OMPR from P, Q, F and S.
!
!    Input/output, real ( kind = 8 ) P, the cumulation from 0 to F of
!    the negative binomial distribution.  If P is an input value, it
!    should lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) F, the upper limit of cumulation of
!    the binomial distribution.  There are F or fewer failures before
!    the S-th success.  If this is an input value, it may lie in the
!    range [0,+infinity), and if it is an output value, it will be searched
!    for in the range [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) S, the number of successes.
!    If this is an input value, it should lie in the range: [0, +infinity).
!    If it is an output value, it will be searched for in the range:
!    [0, 1.0D+300].
!
!    Input/output, real ( kind = 8 ) PR, the probability of success in each
!    binomial trial.  Whether an input or output value, it should lie in the
!    range [0,1].
!
!    Input/output, real ( kind = 8 ) OMPR, the value of (1-PR).  Whether an
!    input or output value, it should lie in the range [0,1].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +4, if PR + OMPR /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) f
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+300
  real ( kind = 8 ) ompr
  real ( kind = 8 ) p
  real ( kind = 8 ) pr
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  real ( kind = 8 ) s
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFNBN - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if

  if ( 4 < which ) then
    bound = 4.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFNBN - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      status = -3
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Unless F is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( f < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter F is out of range.'
      return
    end if
  end if
!
!  Unless S is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( s < 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter S is out of range.'
      return
    end if
  end if
!
!  Unless PR is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( pr < 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter PR is out of range.'
      return
    else if ( 1.0D+00 < pr ) then
      bound = 1.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter PR is out of range.'
      return
    end if
  end if
!
!  Unless OMPR is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( ompr < 0.0D+00 ) then
      bound = 0.0D+00
      status = -7
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter OMPR is out of range.'
      return
    else if ( 1.0D+00 < ompr ) then
      bound = 1.0D+00
      status = -7
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter OMPR is out of range.'
      return
    end if
  end if
!
!  Check that P + Q = 1.
!
  if ( which /= 1 ) then
    if ( 3.0D+00 * epsilon ( p ) < abs ( ( p + q ) - 1.0D+00 ) ) then
      status = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Check that PR + OMPR = 1.
!
  if ( which /= 4 ) then
    if ( 3.0D+00 * epsilon ( pr ) < abs ( ( pr + ompr ) - 1.0D+00 ) ) then
      status = 4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNBN - Fatal error!'
      write ( *, '(a)' ) '  PR + OMPR /= 1.'
      return
    end if
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    call cumnbn ( f, s, pr, ompr, p, q )
    status = 0
!
!  Calculate F.
!
  else if ( which == 2 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    f = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, f, fx, qleft, qhi )

    do while ( status == 1 )

      call cumnbn ( f, s, pr, ompr, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, f, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFNBN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFNBN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate S.
!
  else if ( which == 3 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    s = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, s, fx, qleft, qhi )

    do while ( status == 1 )

      call cumnbn ( f, s, pr, ompr, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, s, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFNBN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFNBn - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate PR and OMPR.
!
  else if ( which == 4 ) then

    call dstzr ( 0.0D+00, 1.0D+00, atol, tol )

    if ( p <= q ) then

      status = 0
      call dzror ( status, pr, fx, xlo, xhi, qleft, qhi )
      ompr = 1.0D+00 - pr

      do while ( status == 1 )

        call cumnbn ( f, s, pr, ompr, cum, ccum )
        fx = cum - p
        call dzror ( status, pr, fx, xlo, xhi, qleft, qhi )
        ompr = 1.0D+00 - pr

      end do

    else

      status = 0
      call dzror ( status, ompr, fx, xlo, xhi, qleft, qhi )
      pr = 1.0D+00 - ompr

      do while ( status == 1 )

        call cumnbn ( f, s, pr, ompr, cum, ccum )
        fx = ccum - q
        call dzror ( status, ompr, fx, xlo, xhi, qleft, qhi )
        pr = 1.0D+00 - ompr

      end do

    end if

    if ( status == -1 ) then

      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFNBN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = 1.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFNBN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if

    end if

  end if

  return
end
subroutine cdfnor ( which, p, q, x, mean, sd, status, bound )

!*****************************************************************************80
!
!! CDFNOR evaluates the CDF of the Normal distribution.
!
!  Discussion:
!
!    A slightly modified version of ANORM from SPECFUN
!    is used to calculate the cumulative standard normal distribution.
!
!    The rational functions from pages 90-95 of Kennedy and Gentle
!    are used as starting values to a Newton iteration which
!    compute the inverse standard normal.  Therefore no searches are
!    necessary for any parameter.
!
!    For X < -15, the asymptotic expansion for the normal is used  as
!    the starting value in finding the inverse standard normal.
!
!    The normal density is proportional to
!    exp ( - 0.5D+00 * (( X - MEAN)/SD)**2)
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.2.12.
!
!    William Cody,
!    Algorithm 715:
!    SPECFUN - A Portable FORTRAN Package of
!    Special Function Routines and Test Drivers,
!    ACM Transactions on Mathematical Software,
!    Volume 19, Number 1, pages 22-32, 1993.
!
!    William Kennedy, James Gentle,
!    Statistical Computing,
!    Marcel Dekker, NY, 1980,
!    QA276.4 K46
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from X, MEAN and SD;
!    2: Calculate X from P, Q, MEAN and SD;
!    3: Calculate MEAN from P, Q, X and SD;
!    4: Calculate SD from P, Q, X and MEAN.
!
!    Input/output, real ( kind = 8 ) P, the integral from -infinity to X
!    of the Normal density.  If this is an input or output value, it will
!    lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) X, the upper limit of integration of
!    the Normal density.
!
!    Input/output, real ( kind = 8 ) MEAN, the mean of the Normal density.
!
!    Input/output, real ( kind = 8 ) SD, the standard deviation of the
!    Normal density.  If this is an input value, it should lie in the
!    range (0,+infinity).
!
!    Output, integer ( kind = 4 ) STATUS, the status of the calculation.
!    0, if calculation completed correctly;
!    -I, if input parameter number I is out of range;
!    1, if answer appears to be lower than lowest search bound;
!    2, if answer appears to be higher than greatest search bound;
!    3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ) bound
  real ( kind = 8 ) dinvnr
  real ( kind = 8 ) mean
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) sd
  integer ( kind = 4 ) status
  integer ( kind = 4 ) which
  real ( kind = 8 ) x
  real ( kind = 8 ) z

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  status = 0

  if ( which < 1 ) then
    status = -1
    bound = 1.0D+00
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFNOR - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  else if ( 4 < which ) then
    status = -1
    bound = 4.0D+00
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFNOR - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      status = -3
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Check that P + Q = 1.
!
  if ( which /= 1 ) then
    if ( 3.0D+00 * epsilon ( p ) < abs ( ( p + q ) - 1.0D+00 ) ) then
      status = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if

  if ( which /= 4 ) then
    if ( sd <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
      write ( *, '(a)' ) '  Input parameter SD is out of range.'
      return
    end if
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    z = ( x - mean ) / sd
    call cumnor ( z, p, q )
!
!  Calculate X.
!
  else if ( which == 2 ) then

    z = dinvnr ( p, q )
    x = sd * z + mean
!
!  Calculate MEAN.
!
  else if ( which == 3 ) then

    z = dinvnr ( p, q )
    mean = x - sd * z
!
!  Calculate SD.
!
  else if ( which == 4 ) then

    z = dinvnr ( p, q )
    sd = ( x - mean ) / z

  end if

  return
end
subroutine cdfpoi ( which, p, q, s, xlam, status, bound )

!*****************************************************************************80
!
!! CDFPOI evaluates the CDF of the Poisson distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the Poisson distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of other parameters involve a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.4.21.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from S and XLAM;
!    2: Calculate A from P, Q and XLAM;
!    3: Calculate XLAM from P, Q and S.
!
!    Input/output, real ( kind = 8 ) P, the cumulation from 0 to S of the
!    Poisson density.  Whether this is an input or output value, it will
!    lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) S, the upper limit of cumulation of
!    the Poisson CDF.  If this is an input value, it should lie in
!    the range: [0, +infinity).  If it is an output value, it will be
!    searched for in the range: [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) XLAM, the mean of the Poisson
!    distribution.  If this is an input value, it should lie in the range
!    [0, +infinity).  If it is an output value, it will be searched for
!    in the range: [0,1E300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+300
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  real ( kind = 8 ) s
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which
  real ( kind = 8 ) xlam

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFPOI - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 3.'
    return
  end if

  if ( 3 < which ) then
    bound = 3.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFPOI - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 3.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      status = -3
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Unless S is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( s < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter S is out of range.'
      return
    end if
  end if
!
!  Unless XLAM is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( xlam < 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter XLAM is out of range.'
      return
    end if
  end if
!
!  Check that P + Q = 1.
!
  if ( which /= 1 ) then
    if ( 3.0D+00 * epsilon ( p ) < abs ( ( p + q ) - 1.0D+00 ) ) then
      status = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    call cumpoi ( s, xlam, p, q )
    status = 0
!
!  Calculate S.
!
  else if ( which == 2 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    s = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, s, fx, qleft, qhi )

    do while ( status == 1 )

      call cumpoi ( s, xlam, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, s, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFPOI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFPOI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate XLAM.
!
  else if ( which == 3 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    xlam = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, xlam, fx, qleft, qhi )

    do while ( status == 1 )

      call cumpoi ( s, xlam, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, xlam, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFPOI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFPOI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if

  end if

  return
end
subroutine cdft ( which, p, q, t, df, status, bound )

!*****************************************************************************80
!
!! CDFT evaluates the CDF of the T distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the T distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of other parameters involve a seach for a value that
!    produces the desired value of P.   The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The original version of this routine allowed the search interval
!    to extend from -1.0D+300 to +1.0D+300, which is fine until you
!    try to evaluate a function at such a point!
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.27.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1 : Calculate P and Q from T and DF;
!    2 : Calculate T from P, Q and DF;
!    3 : Calculate DF from P, Q and T.
!
!    Input/output, real ( kind = 8 ) P, the integral from -infinity to T of
!    the T-density.  Whether an input or output value, this will lie in the
!    range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) T, the upper limit of integration of
!    the T-density.  If this is an input value, it may have any value.
!    It it is an output value, it will be searched for in the range
!    [ -1.0D+30, 1.0D+30 ].
!
!    Input/output, real ( kind = 8 ) DF, the number of degrees of freedom
!    of the T distribution.  If this is an input value, it should lie
!    in the range: (0 , +infinity).  If it is an output value, it will be
!    searched for in the range: [1, 1.0D+10].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) dt1
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+30
  real ( kind = 8 ), parameter :: maxdf = 1.0D+10
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  integer ( kind = 4 ) status
  real ( kind = 8 ) t
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFT - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 3.'
    return
  end if

  if ( 3 < which ) then
    bound = 3.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFT - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 3.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFT - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFT - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      status = -3
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFT - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFT - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Unless DF is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( df <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFT - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DF is out of range.'
      return
    end if
  end if
!
!  Check that P + Q = 1.
!
  if ( which /= 1 ) then
    if ( 3.0D+00 * epsilon ( 1.0D+00 ) &
      < abs ( ( p + q ) - 1.0D+00 ) ) then
      status = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFT - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    call cumt ( t, df, p, q )
    status = 0
!
!  Calculate T.
!
  else if ( which == 2 ) then

    call dstinv ( -inf, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    t = dt1 ( p, q, df )
    fx = 0.0D+00

    call dinvr ( status, t, fx, qleft, qhi )

    do while ( status == 1 )

      call cumt ( t, df, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, t, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft )then
        status = 1
        bound = -inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFT - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFT - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate DF.
!
  else if ( which == 3 ) then

    call dstinv ( 1.0D+00, maxdf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    df = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, df, fx, qleft, qhi )

    do while ( status == 1 )

      call cumt ( t, df, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, df, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFT - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = maxdf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFT - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if

  end if

  return
end
subroutine chi_noncentral_cdf_values ( n_data, x, lambda, df, cdf )

!*****************************************************************************80
!
!! CHI_NONCENTRAL_CDF_VALUES returns values of the noncentral chi CDF.
!
!  Discussion:
!
!    The CDF of the noncentral chi square distribution can be evaluated
!    within Mathematica by commands such as:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF [ NoncentralChiSquareDistribution [ DF, LAMBDA ], X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) LAMBDA, the noncentrality parameter.
!
!    Output, integer ( kind = 4 ) DF, the number of degrees of freedom.
!
!    Output, real ( kind = 8 ) CDF, the noncentral chi CDF.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 27

  real ( kind = 8 ) cdf
  real, save, dimension ( n_max ) :: cdf_vec = (/ &
    0.839944D+00, 0.695906D+00, 0.535088D+00, &
    0.764784D+00, 0.620644D+00, 0.469167D+00, &
    0.307088D+00, 0.220382D+00, 0.150025D+00, &
    0.307116D-02, 0.176398D-02, 0.981679D-03, &
    0.165175D-01, 0.202342D-03, 0.498448D-06, &
    0.151325D-01, 0.209041D-02, 0.246502D-03, &
    0.263684D-01, 0.185798D-01, 0.130574D-01, &
    0.583804D-01, 0.424978D-01, 0.308214D-01, &
    0.105788D+00, 0.794084D-01, 0.593201D-01 /)
  integer ( kind = 4 ) df
  integer ( kind = 4 ), save, dimension ( n_max ) :: df_vec = (/ &
      1,   2,   3, &
      1,   2,   3, &
      1,   2,   3, &
      1,   2,   3, &
     60,  80, 100, &
      1,   2,   3, &
     10,  10,  10, &
     10,  10,  10, &
     10,  10,  10 /)
  real ( kind = 8 ) lambda
  real, save, dimension ( n_max ) :: lambda_vec = (/ &
     0.5D+00,  0.5D+00,  0.5D+00, &
     1.0D+00,  1.0D+00,  1.0D+00, &
     5.0D+00,  5.0D+00,  5.0D+00, &
    20.0D+00, 20.0D+00, 20.0D+00, &
    30.0D+00, 30.0D+00, 30.0D+00, &
     5.0D+00,  5.0D+00,  5.0D+00, &
     2.0D+00,  3.0D+00,  4.0D+00, &
     2.0D+00,  3.0D+00,  4.0D+00, &
     2.0D+00,  3.0D+00,  4.0D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real, save, dimension ( n_max ) :: x_vec = (/ &
     3.000D+00,  3.000D+00,  3.000D+00, &
     3.000D+00,  3.000D+00,  3.000D+00, &
     3.000D+00,  3.000D+00,  3.000D+00, &
     3.000D+00,  3.000D+00,  3.000D+00, &
    60.000D+00, 60.000D+00, 60.000D+00, &
     0.050D+00,  0.050D+00,  0.050D+00, &
     4.000D+00,  4.000D+00,  4.000D+00, &
     5.000D+00,  5.000D+00,  5.000D+00, &
     6.000D+00,  6.000D+00,  6.000D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    lambda = 0.0D+00
    df = 0
    cdf = 0.0D+00
  else
    x = x_vec(n_data)
    lambda = lambda_vec(n_data)
    df = df_vec(n_data)
    cdf = cdf_vec(n_data)
  end if

  return
end
subroutine chi_square_cdf_values ( n_data, a, x, fx )

!*****************************************************************************80
!
!! CHI_SQUARE_CDF_VALUES returns some values of the Chi-Square CDF.
!
!  Discussion:
!
!    The value of CHI_CDF ( DF, X ) can be evaluated in Mathematica by
!    commands like:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF[ChiSquareDistribution[DF], X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) A, real ( kind = 8 ) X, the arguments of
!    the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 21

  integer ( kind = 4 ) a
  integer ( kind = 4 ), save, dimension ( n_max ) :: a_vec = (/ &
     1,  2,  1,  2, &
     1,  2,  3,  4, &
     1,  2,  3,  4, &
     5,  3,  3,  3, &
     3,  3, 10, 10, &
    10 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.0796557D+00, 0.00498752D+00, 0.112463D+00,    0.00995017D+00, &
    0.472911D+00,  0.181269D+00,   0.0597575D+00,   0.0175231D+00, &
    0.682689D+00,  0.393469D+00,   0.198748D+00,    0.090204D+00, &
    0.0374342D+00, 0.427593D+00,   0.608375D+00,    0.738536D+00, &
    0.828203D+00,  0.88839D+00,    0.000172116D+00, 0.00365985D+00, &
    0.0185759D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.01D+00, 0.01D+00, 0.02D+00, 0.02D+00, &
    0.40D+00, 0.40D+00, 0.40D+00, 0.40D+00, &
    1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, &
    1.00D+00, 2.00D+00, 3.00D+00, 4.00D+00, &
    5.00D+00, 6.00D+00, 1.00D+00, 2.00D+00, &
    3.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine cumbet ( x, y, a, b, cum, ccum )

!*****************************************************************************80
!
!! CUMBET evaluates the cumulative incomplete beta distribution.
!
!  Discussion:
!
!    This routine calculates the CDF to X of the incomplete beta distribution
!    with parameters A and B.  This is the integral from 0 to x
!    of (1/B(a,b))*f(t)) where f(t) = t**(a-1) * (1-t)**(b-1)
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the upper limit of integration.
!
!    Input, real ( kind = 8 ) Y, the value of 1-X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the values of the cumulative
!    density function and complementary cumulative density function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x <= 0.0D+00 ) then

    cum = 0.0
    ccum = 1.0D+00

  else if ( y <= 0.0D+00 ) then

    cum = 1.0D+00
    ccum = 0.0

  else

    call beta_inc ( a, b, x, y, cum, ccum, ierr )

  end if

  return
end
subroutine cumbin ( s, xn, pr, ompr, cum, ccum )

!*****************************************************************************80
!
!! CUMBIN evaluates the cumulative binomial distribution.
!
!  Discussion:
!
!    This routine returns the probability of 0 to S successes in XN binomial
!    trials, each of which has a probability of success, PR.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.24.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) S, the upper limit of summation.
!
!    Input, real ( kind = 8 ) XN, the number of trials.
!
!    Input, real ( kind = 8 ) PR, the probability of success in one trial.
!
!    Input, real ( kind = 8 ) OMPR, equals ( 1 - PR ).
!
!    Output, real ( kind = 8 ) CUM, the cumulative binomial distribution.
!
!    Output, real ( kind = 8 ) CCUM, the complement of the cumulative
!    binomial distribution.
!
  implicit none

  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) ompr
  real ( kind = 8 ) pr
  real ( kind = 8 ) s
  real ( kind = 8 ) xn

  if ( s < xn ) then

    call cumbet ( pr, ompr, s + 1.0D+00, xn - s, ccum, cum )

  else

    cum = 1.0D+00
    ccum = 0.0D+00

  end if

  return
end
subroutine cumchi ( x, df, cum, ccum )

!*****************************************************************************80
!
!! CUMCHI evaluates the cumulative chi-square distribution.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom of the
!    chi-square distribution.
!
!    Output, real ( kind = 8 ) CUM, the cumulative chi-square distribution.
!
!    Output, real ( kind = 8 ) CCUM, the complement of the cumulative
!    chi-square distribution.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) x
  real ( kind = 8 ) xx

  a = df * 0.5D+00
  xx = x * 0.5D+00

  call cumgam ( xx, a, cum, ccum )

  return
end
subroutine cumchn ( x, df, pnonc, cum, ccum )

!*****************************************************************************80
!
!! CUMCHN evaluates the cumulative noncentral chi-square distribution.
!
!  Discussion:
!
!    This routine calculates the cumulative noncentral chi-square
!    distribution, i.e., the probability that a random variable
!    which follows the noncentral chi-square distribution, with
!    noncentrality parameter PNONC and continuous degrees of
!    freedom DF, is less than or equal to X.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.4.25.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DF, the number of degrees of freedom.
!
!    Input, real ( kind = 8 ) PNONC, the noncentrality parameter of
!    the noncentral chi-square distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the CDF and complementary
!    CDF of the noncentral chi-square distribution.
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) EPS, the convergence criterion.  The sum
!    stops when a term is less than EPS * SUM.
!
!    Local, integer NTIRED, the maximum number of terms to be evaluated
!    in each sum.
!
!    Local, logical QCONV, is TRUE if convergence was achieved, that is,
!    the program did not stop on NTIRED criterion.
!
  implicit none

  real ( kind = 8 ) adj
  real ( kind = 8 ) ccum
  real ( kind = 8 ) centaj
  real ( kind = 8 ) centwt
  real ( kind = 8 ) chid2
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) dfd2
  real ( kind = 8 ) dg
  real ( kind = 8 ), parameter :: eps = 0.00001D+00
  real ( kind = 8 ) gamma_log
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icent
  integer ( kind = 4 ) iterb
  integer ( kind = 4 ) iterf
  real ( kind = 8 ) lcntaj
  real ( kind = 8 ) lcntwt
  real ( kind = 8 ) lfact
  integer ( kind = 4 ), parameter :: ntired = 1000
  real ( kind = 8 ) pcent
  real ( kind = 8 ) pnonc
  real ( kind = 8 ) pterm
  logical qsmall
  real ( kind = 8 ) sum1
  real ( kind = 8 ) sumadj
  real ( kind = 8 ) term
  real ( kind = 8 ) wt
  real ( kind = 8 ) x
  real ( kind = 8 ) xnonc
  real ( kind = 8 ) xx

  qsmall ( xx ) = sum1 < 1.0D-20 .or. xx < eps * sum1
  dg(i) = df +  2.0D+00  * real ( i, kind = 8 )

  if ( x <= 0.0D+00 ) then
    cum = 0.0D+00
    ccum = 1.0D+00
    return
  end if
!
!  When the noncentrality parameter is (essentially) zero,
!  use cumulative chi-square distribution
!
  if ( pnonc <= 1.0D-10 ) then
    call cumchi ( x, df, cum, ccum )
    return
  end if

  xnonc = pnonc /  2.0D+00
!
!  The following code calculates the weight, chi-square, and
!  adjustment term for the central term in the infinite series.
!  The central term is the one in which the poisson weight is
!  greatest.  The adjustment term is the amount that must
!  be subtracted from the chi-square to move up two degrees
!  of freedom.
!
  icent = int ( xnonc )
  if ( icent == 0 ) then
    icent = 1
  end if

  chid2 = x /  2.0D+00
!
!  Calculate central weight term.
!
  lfact = gamma_log ( real ( icent + 1, kind = 8 ) )
  lcntwt = - xnonc + icent * log ( xnonc ) - lfact
  centwt = exp ( lcntwt )
!
!  Calculate central chi-square.
!
  call cumchi ( x, dg(icent), pcent, ccum )
!
!  Calculate central adjustment term.
!
  dfd2 = dg(icent) /  2.0D+00
  lfact = gamma_log ( 1.0D+00 + dfd2 )
  lcntaj = dfd2 * log ( chid2 ) - chid2 - lfact
  centaj = exp ( lcntaj )
  sum1 = centwt * pcent
!
!  Sum backwards from the central term towards zero.
!  Quit whenever either
!  (1) the zero term is reached, or
!  (2) the term gets small relative to the sum, or
!  (3) More than NTIRED terms are totaled.
!
  iterb = 0
  sumadj = 0.0D+00
  adj = centaj
  wt = centwt
  i = icent
  term = 0.0D+00

  do

    dfd2 = dg(i) /  2.0D+00
!
!  Adjust chi-square for two fewer degrees of freedom.
!  The adjusted value ends up in PTERM.
!
    adj = adj * dfd2 / chid2
    sumadj = sumadj + adj
    pterm = pcent + sumadj
!
!  Adjust Poisson weight for J decreased by one.
!
    wt = wt * ( i / xnonc )
    term = wt * pterm
    sum1 = sum1 + term
    i = i - 1
    iterb = iterb + 1

    if ( ntired < iterb .or. qsmall ( term ) .or. i == 0 ) then
      exit
    end if

  end do

  iterf = 0
!
!  Now sum forward from the central term towards infinity.
!  Quit when either
!    (1) the term gets small relative to the sum, or
!    (2) More than NTIRED terms are totaled.
!
  sumadj = centaj
  adj = centaj
  wt = centwt
  i = icent
!
!  Update weights for next higher J.
!
  do

    wt = wt * ( xnonc / ( i + 1 ) )
!
!  Calculate PTERM and add term to sum.
!
    pterm = pcent - sumadj
    term = wt * pterm
    sum1 = sum1 + term
!
!  Update adjustment term for DF for next iteration.
!
    i = i + 1
    dfd2 = dg(i) /  2.0D+00
    adj = adj * chid2 / dfd2
    sumadj = sumadj + adj
    iterf = iterf + 1

    if ( ntired < iterf .or. qsmall ( term ) ) then
      exit
    end if

  end do

  cum = sum1
  ccum = 0.5D+00 + ( 0.5D+00 - cum )

  return
end
subroutine cumf ( f, dfn, dfd, cum, ccum )

!*****************************************************************************80
!
!! CUMF evaluates the cumulative F distribution.
!
!  Discussion:
!
!    This routine computes the integral from 0 to F of the F density with DFN
!    numerator and DFD denominator degrees of freedom.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.28.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) F, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DFN, DFD, the number of degrees of
!    freedom for the numerator and denominator.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the value of the F CDF and
!    the complementary F CDF.
!
  implicit none

  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) dfd
  real ( kind = 8 ) dfn
  real ( kind = 8 ) dsum
  real ( kind = 8 ) f
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) prod
  real ( kind = 8 ) xx
  real ( kind = 8 ) yy

  if ( f <= 0.0D+00 ) then
    cum = 0.0D+00
    ccum = 1.0D+00
    return
  end if

  prod = dfn * f
!
!  XX is such that the incomplete beta with parameters
!  DFD/2 and DFN/2 evaluated at XX is 1 - CUM or CCUM
!
!  YY is 1 - XX
!
!  Calculate the smaller of XX and YY accurately.
!
  dsum = dfd + prod
  xx = dfd / dsum

  if ( 0.5D+00 < xx ) then
    yy = prod / dsum
    xx = 1.0D+00 - yy
  else
    yy = 1.0D+00 - xx
  end if

  call beta_inc ( 0.5D+00 * dfd, 0.5D+00 * dfn, xx, yy, ccum, cum, ierr )

  return
end
subroutine cumfnc ( f, dfn, dfd, pnonc, cum, ccum )

!*****************************************************************************80
!
!! CUMFNC evaluates the cumulative noncentral F distribution.
!
!  Discussion:
!
!    This routine computes the noncentral F distribution with DFN and DFD
!    degrees of freedom and noncentrality parameter PNONC.
!
!    The series is calculated backward and forward from J = LAMBDA/2
!    (this is the term with the largest Poisson weight) until
!    the convergence criterion is met.
!
!    The sum continues until a succeeding term is less than EPS
!    times the sum or the sum is very small.  EPS is
!    set to 1.0D-4 in a data statement which can be changed.
!
!    The original version of this routine allowed the input values
!    of DFN and DFD to be negative (nonsensical) or zero (which
!    caused numerical overflow.)  I have forced both these values
!    to be at least 1.
!
!  Modified:
!
!    19 May 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.16, 26.6.17, 26.6.18, 26.6.20.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) F, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DFN, DFD, the number of degrees of freedom
!    in the numerator and denominator.  Both DFN and DFD must be positive,
!    and normally would be integers.  This routine requires that they
!    be no less than 1.
!
!    Input, real ( kind = 8 ) PNONC, the noncentrality parameter.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the noncentral F CDF and
!    complementary CDF.
!
  implicit none

  real ( kind = 8 ) adn
  real ( kind = 8 ) arg1
  real ( kind = 8 ) aup
  real ( kind = 8 ) b
  real ( kind = 8 ) betdn
  real ( kind = 8 ) betup
  real ( kind = 8 ) centwt
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) dfd
  real ( kind = 8 ) dfn
  real ( kind = 8 ) dnterm
  real ( kind = 8 ) dsum
  real ( kind = 8 ) dummy
  real ( kind = 8 ), parameter :: eps = 0.0001D+00
  real ( kind = 8 ) expon
  real ( kind = 8 ) f
  real ( kind = 8 ) gamma_log
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icent
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) pnonc
  real ( kind = 8 ) prod
  real ( kind = 8 ) sum1
  real ( kind = 8 ) upterm
  real ( kind = 8 ) x
  real ( kind = 8 ) xmult
  real ( kind = 8 ) xnonc
  real ( kind = 8 ) xx
  real ( kind = 8 ) yy

  if ( f <= 0.0D+00 ) then
    cum = 0.0D+00
    ccum = 1.0D+00
    return
  end if

  if ( dfn < 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CUMFNC - Fatal error!'
    write ( *, '(a)' ) '  DFN < 1.'
    stop
  end if

  if ( dfd < 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CUMFNC - Fatal error!'
    write ( *, '(a)' ) '  DFD < 1.'
    stop
  end if
!
!  Handle case in which the noncentrality parameter is essentially zero.
!
  if ( pnonc < 1.0D-10 ) then
    call cumf ( f, dfn, dfd, cum, ccum )
    return
  end if

  xnonc = pnonc /  2.0D+00
!
!  Calculate the central term of the Poisson weighting factor.
!
  icent = int ( xnonc )

  if ( icent == 0 ) then
    icent = 1
  end if
!
!  Compute central weight term.
!
  centwt = exp ( -xnonc + icent * log ( xnonc ) &
    - gamma_log ( real ( icent + 1, kind = 8  ) ) )
!
!  Compute central incomplete beta term.
!  Ensure that minimum of arg to beta and 1 - arg is computed accurately.
!
  prod = dfn * f
  dsum = dfd + prod
  yy = dfd / dsum

  if ( 0.5D+00 < yy ) then
    xx = prod / dsum
    yy = 1.0D+00 - xx
  else
    xx = 1.0D+00 - yy
  end if

  arg1 = 0.5D+00 * dfn + real ( icent, kind = 8 )
  call beta_inc ( arg1, 0.5D+00*dfd, xx, yy, betdn, dummy, ierr )

  adn = dfn / 2.0D+00 + real ( icent, kind = 8 )
  aup = adn
  b = dfd / 2.0D+00
  betup = betdn
  sum1 = centwt * betdn
!
!  Now sum terms backward from ICENT until convergence or all done.
!
  xmult = centwt
  i = icent
  dnterm = exp ( gamma_log ( adn + b ) &
    - gamma_log ( adn + 1.0D+00 ) &
    - gamma_log ( b ) + adn * log ( xx ) + b * log ( yy ) )

  do

    if ( i <= 0 ) then
      exit
    end if

    if ( sum1 < epsilon ( xmult * betdn ) .or. &
         xmult * betdn < eps * sum1 ) then
      exit
    end if

    xmult = xmult * ( real ( i, kind = 8 ) / xnonc )
    i = i - 1
    adn = adn - 1.0D+00
    dnterm = ( adn + 1.0D+00 ) / ( ( adn + b ) * xx ) * dnterm
    betdn = betdn + dnterm
    sum1 = sum1 + xmult * betdn

  end do

  i = icent + 1
!
!  Now sum forward until convergence.
!
  xmult = centwt

  if ( ( aup - 1.0D+00 + b ) == 0 ) then

    expon = - gamma_log ( aup ) - gamma_log ( b ) &
      + ( aup - 1.0D+00 ) * log ( xx ) + b * log ( yy )

  else

    expon = gamma_log ( aup - 1.0D+00 + b ) - gamma_log ( aup ) &
      - gamma_log ( b ) + ( aup - 1.0D+00 ) * log ( xx ) + b * log ( yy )

  end if
!
!  The fact that DCDFLIB blithely assumes that 1.0E+30 is a reasonable
!  value to plug into any function, and that G95 computes corresponding
!  function values of, say 1.0E-303, and then chokes with a floating point
!  error when asked to combine such a value with a reasonable floating
!  point quantity, has driven me to the following sort of check that
!  was last fashionable in the 1960's!
!
  if ( expon <= log ( epsilon ( expon ) ) ) then
    upterm = 0.0D+00
  else
    upterm = exp ( expon )
  end if

  do

    xmult = xmult * ( xnonc / real ( i, kind = 8 ) )
    i = i + 1
    aup = aup + 1.0D+00
    upterm = ( aup + b -  2.0D+00  ) * xx / ( aup - 1.0D+00 ) * upterm
    betup = betup - upterm
    sum1 = sum1 + xmult * betup

    if ( sum1 < epsilon ( xmult * betup ) .or. xmult * betup < eps * sum1 ) then
      exit
    end if

  end do

  cum = sum1
  ccum = 0.5D+00 + ( 0.5D+00 - cum )

  return
end
subroutine cumgam ( x, a, cum, ccum )

!*****************************************************************************80
!
!! CUMGAM evaluates the cumulative incomplete gamma distribution.
!
!  Discussion:
!
!    This routine computes the cumulative distribution function of the
!    incomplete gamma distribution, i.e., the integral from 0 to X of
!
!      (1/GAM(A))*EXP(-T)*T^(A-1) DT
!
!    where GAM(A) is the complete gamma function of A:
!
!      GAM(A) = integral from 0 to infinity of EXP(-T)*T^(A-1) DT
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the upper limit of integration.
!
!    Input, real ( kind = 8 ) A, the shape parameter of the incomplete
!    Gamma distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the incomplete Gamma CDF and
!    complementary CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) x

  if ( x <= 0.0D+00 ) then

    cum = 0.0D+00
    ccum = 1.0D+00

  else

    call gamma_inc ( a, x, cum, ccum, 0 )

  end if

  return
end
subroutine cumnbn ( f, s, pr, ompr, cum, ccum )

!*****************************************************************************80
!
!! CUMNBN evaluates the cumulative negative binomial distribution.
!
!  Discussion:
!
!    This routine returns the probability that there will be F or
!    fewer failures before there are S successes, with each binomial
!    trial having a probability of success PR.
!
!    Prob(# failures = F | S successes, PR)  =
!                        ( S + F - 1 )
!                        (            ) * PR^S * (1-PR)^F
!                        (      F     )
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.26.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) F, the number of failures.
!
!    Input, real ( kind = 8 ) S, the number of successes.
!
!    Input, real ( kind = 8 ) PR, OMPR, the probability of success on
!    each binomial trial, and the value of (1-PR).
!
!    Output, real ( kind = 8 ) CUM, CCUM, the negative binomial CDF,
!    and the complementary CDF.
!
  implicit none

  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) f
  real ( kind = 8 ) ompr
  real ( kind = 8 ) pr
  real ( kind = 8 ) s

  call cumbet ( pr, ompr, s, f+1.D+00, cum, ccum )

  return
end
subroutine cumnor ( arg, cum, ccum )

!*****************************************************************************80
!
!! CUMNOR computes the cumulative normal distribution.
!
!  Discussion:
!
!    This function evaluates the normal distribution function:
!
!                              / x
!                     1       |       -t*t/2
!          P(x) = ----------- |      e       dt
!                 sqrt(2 pi)  |
!                             /-oo
!
!    This transportable program uses rational functions that
!    theoretically approximate the normal distribution function to
!    at least 18 significant decimal digits.  The accuracy achieved
!    depends on the arithmetic system, the compiler, the intrinsic
!    functions, and proper selection of the machine dependent
!    constants.
!
!  Author:
!
!    William Cody
!    Mathematics and Computer Science Division
!    Argonne National Laboratory
!    Argonne, IL 60439
!
!  Reference:
!
!    William Cody,
!    Rational Chebyshev approximations for the error function,
!    Mathematics of Computation,
!    1969, pages 631-637.
!
!    William Cody,
!    Algorithm 715:
!    SPECFUN - A Portable FORTRAN Package of Special Function Routines
!    and Test Drivers,
!    ACM Transactions on Mathematical Software,
!    Volume 19, Number 1, 1993, pages 22-32.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the upper limit of integration.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the Normal density CDF and
!    complementary CDF.
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) EPS, the argument below which anorm(x)
!    may be represented by 0.5 and above which  x*x  will not underflow.
!    A conservative value is the largest machine number X
!    such that   1.0D+00 + X = 1.0D+00   to machine precision.
!
  implicit none

  real ( kind = 8 ), parameter, dimension ( 5 ) :: a = (/ &
    2.2352520354606839287D+00, &
    1.6102823106855587881D+02, &
    1.0676894854603709582D+03, &
    1.8154981253343561249D+04, &
    6.5682337918207449113D-02 /)
  real ( kind = 8 ) arg
  real ( kind = 8 ), parameter, dimension ( 4 ) :: b = (/ &
    4.7202581904688241870D+01, &
    9.7609855173777669322D+02, &
    1.0260932208618978205D+04, &
    4.5507789335026729956D+04 /)
  real ( kind = 8 ), parameter, dimension ( 9 ) :: c = (/ &
    3.9894151208813466764D-01, &
    8.8831497943883759412D+00, &
    9.3506656132177855979D+01, &
    5.9727027639480026226D+02, &
    2.4945375852903726711D+03, &
    6.8481904505362823326D+03, &
    1.1602651437647350124D+04, &
    9.8427148383839780218D+03, &
    1.0765576773720192317D-08 /)
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ), parameter, dimension ( 8 ) :: d = (/ &
    2.2266688044328115691D+01, &
    2.3538790178262499861D+02, &
    1.5193775994075548050D+03, &
    6.4855582982667607550D+03, &
    1.8615571640885098091D+04, &
    3.4900952721145977266D+04, &
    3.8912003286093271411D+04, &
    1.9685429676859990727D+04 /)
  real ( kind = 8 ) del
  real ( kind = 8 ) eps
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter, dimension ( 6 ) :: p = (/ &
    2.1589853405795699D-01, &
    1.274011611602473639D-01, &
    2.2235277870649807D-02, &
    1.421619193227893466D-03, &
    2.9112874951168792D-05, &
    2.307344176494017303D-02 /)
  real ( kind = 8 ), parameter, dimension ( 5 ) :: q = (/ &
    1.28426009614491121D+00, &
    4.68238212480865118D-01, &
    6.59881378689285515D-02, &
    3.78239633202758244D-03, &
    7.29751555083966205D-05 /)
  real ( kind = 8 ), parameter :: root32 = 5.656854248D+00
  real ( kind = 8 ), parameter :: sixten = 16.0D+00
  real ( kind = 8 ) temp
  real ( kind = 8 ), parameter :: sqrpi = 3.9894228040143267794D-01
  real ( kind = 8 ), parameter :: thrsh = 0.66291D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) xden
  real ( kind = 8 ) xnum
  real ( kind = 8 ) y
  real ( kind = 8 ) xsq
!
!  Machine dependent constants
!
  eps = epsilon ( 1.0D+00 ) * 0.5D+00

  x = arg
  y = abs ( x )

  if ( y <= thrsh ) then
!
!  Evaluate  anorm  for  |X| <= 0.66291
!
    if ( eps < y ) then
      xsq = x * x
    else
      xsq = 0.0D+00
    end if

    xnum = a(5) * xsq
    xden = xsq
    do i = 1, 3
      xnum = ( xnum + a(i) ) * xsq
      xden = ( xden + b(i) ) * xsq
    end do
    cum = x * ( xnum + a(4) ) / ( xden + b(4) )
    temp = cum
    cum = 0.5D+00 + temp
    ccum = 0.5D+00 - temp
!
!  Evaluate ANORM for 0.66291 <= |X| <= sqrt(32)
!
  else if ( y <= root32 ) then

    xnum = c(9) * y
    xden = y
    do i = 1, 7
      xnum = ( xnum + c(i) ) * y
      xden = ( xden + d(i) ) * y
    end do
    cum = ( xnum + c(8) ) / ( xden + d(8) )
    xsq = aint ( y * sixten ) / sixten
    del = ( y - xsq ) * ( y + xsq )
    cum = exp ( - xsq * xsq * 0.5D+00 ) * exp ( -del * 0.5D+00 ) * cum
    ccum = 1.0D+00 - cum

    if ( 0.0D+00 < x ) then
      call r8_swap ( cum, ccum )
    end if
!
!  Evaluate ANORM for sqrt(32) < |X|.
!
  else

    cum = 0.0D+00
    xsq = 1.0D+00 / ( x * x )
    xnum = p(6) * xsq
    xden = xsq
    do i = 1, 4
      xnum = ( xnum + p(i) ) * xsq
      xden = ( xden + q(i) ) * xsq
    end do

    cum = xsq * ( xnum + p(5) ) / ( xden + q(5) )
    cum = ( sqrpi - cum ) / y
    xsq = aint ( x * sixten ) / sixten
    del = ( x - xsq ) * ( x + xsq )
    cum = exp ( - xsq * xsq * 0.5D+00 ) &
      * exp ( - del * 0.5D+00 ) * cum
    ccum = 1.0D+00 - cum

    if ( 0.0D+00 < x ) then
      call r8_swap ( cum, ccum )
    end if

  end if

  if ( cum < tiny ( cum ) ) then
    cum = 0.0D+00
  end if

  if ( ccum < tiny ( ccum ) ) then
    ccum = 0.0D+00
  end if

  return
end
subroutine cumpoi ( s, xlam, cum, ccum )

!*****************************************************************************80
!
!! CUMPOI evaluates the cumulative Poisson distribution.
!
!  Discussion:
!
!    This routine returns the probability of S or fewer events in a Poisson
!    distribution with mean XLAM.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    Formula 26.4.21.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) S, the upper limit of cumulation of the
!    Poisson density function.
!
!    Input, real ( kind = 8 ) XLAM, the mean of the Poisson distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the Poisson density CDF and
!    complementary CDF.
!
  implicit none

  real ( kind = 8 ) ccum
  real ( kind = 8 ) chi
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) s
  real ( kind = 8 ) xlam

  df =  2.0D+00  * ( s + 1.0D+00 )
  chi =  2.0D+00  * xlam

  call cumchi ( chi, df, ccum, cum )

  return
end
subroutine cumt ( t, df, cum, ccum )

!*****************************************************************************80
!
!! CUMT evaluates the cumulative T distribution.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    Formula 26.5.27.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DF, the number of degrees of freedom of
!    the T distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the T distribution CDF and
!    complementary CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) oma
  real ( kind = 8 ) t
  real ( kind = 8 ) xx
  real ( kind = 8 ) yy

  xx = df / ( df + t**2 )
  yy = t**2 / ( df + t**2 )

  call cumbet ( xx, yy, 0.5D+00*df, 0.5D+00, a, oma )

  if ( t <= 0.0D+00 ) then
    cum = 0.5D+00 * a
    ccum = oma + cum
  else
    ccum = 0.5D+00 * a
    cum = oma + ccum
  end if

  return
end
function dbetrm ( a, b )

!*****************************************************************************80
!
!! DBETRM computes the Sterling remainder for the complete beta function.
!
!  Discussion:
!
!    Log(Beta(A,B)) = Lgamma(A) + Lgamma(B) - Lgamma(A+B)
!    where Lgamma is the log of the (complete) gamma function
!
!    Let ZZ be approximation obtained if each log gamma is approximated
!    by Sterling's formula, i.e.,
!
!      Sterling(Z) = log ( sqrt ( 2 * PI ) ) + ( Z - 0.5 ) * log ( Z ) - Z
!
!    The Sterling remainder is Log(Beta(A,B)) - ZZ.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the Beta function.
!
!    Output, real ( kind = 8 ) DBETRM, the Sterling remainder.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) dbetrm
  real ( kind = 8 ) dstrem
!
!  Try to sum from smallest to largest.
!
  dbetrm = -dstrem ( a + b )
  dbetrm = dbetrm + dstrem ( max ( a, b ) )
  dbetrm = dbetrm + dstrem ( min ( a, b ) )

  return
end
function dexpm1 ( x )

!*****************************************************************************80
!
!! DEXPM1 evaluates the function EXP(X) - 1.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the value at which exp(X)-1 is desired.
!
!    Output, real ( kind = 8 ) DEXPM1, the value of exp(X)-1.
!
  implicit none

  real ( kind = 8 ) bot
  real ( kind = 8 ) dexpm1
  real ( kind = 8 ), parameter :: p1 =  0.914041914819518D-09
  real ( kind = 8 ), parameter :: p2 =  0.238082361044469D-01
  real ( kind = 8 ), parameter :: q1 = -0.499999999085958D+00
  real ( kind = 8 ), parameter :: q2 =  0.107141568980644D+00
  real ( kind = 8 ), parameter :: q3 = -0.119041179760821D-01
  real ( kind = 8 ), parameter :: q4 =  0.595130811860248D-03
  real ( kind = 8 ) top
  real ( kind = 8 ) w
  real ( kind = 8 ) x

  if ( abs ( x ) <= 0.15D+00 ) then

    top = ( p2 * x + p1 ) * x + 1.0D+00
    bot = ((( q4 * x + q3 ) * x + q2 ) * x + q1 ) * x + 1.0D+00
    dexpm1 = x * ( top / bot )

  else

    w = exp ( x )

    if ( x <= 0.0D+00 ) then
      dexpm1 = ( w - 0.5D+00 ) - 0.5D+00
    else
      dexpm1 = w * ( 0.5D+00 &
        + ( 0.5D+00 - 1.0D+00 / w ))
    end if

  end if

  return
end
function dinvnr ( p, q )

!*****************************************************************************80
!
!! DINVNR computes the inverse of the normal distribution.
!
!  Discussion:
!
!    This routine returns X such that
!
!      CUMNOR(X) = P,
!
!    that is, so that
!
!      P = integral ( -oo <= T <= X ) exp(-U*U/2)/sqrt(2*PI) dU
!
!    The rational function on page 95 of Kennedy and Gentle is used as a
!    starting value for the Newton method of finding roots.
!
!  Reference:
!
!    William Kennedy, James Gentle,
!    Statistical Computing,
!    Marcel Dekker, NY, 1980,
!    QA276.4 K46
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, Q, the probability, and the complementary
!    probability.
!
!    Output, real ( kind = 8 ) DINVNR, the argument X for which the
!    Normal CDF has the value P.
!
  implicit none

  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) dinvnr
  real ( kind = 8 ) dx
  real ( kind = 8 ), parameter :: eps = 1.0D-13
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: maxit = 100
  real ( kind = 8 ) p
  real ( kind = 8 ) pp
  real ( kind = 8 ) q
  real ( kind = 8 ), parameter :: r2pi = 0.3989422804014326D+00
  real ( kind = 8 ) strtx
  real ( kind = 8 ) stvaln
  real ( kind = 8 ) xcur

  pp = min ( p, q )
  strtx = stvaln ( pp )
  xcur = strtx
!
!  Newton iterations.
!
  do i = 1, maxit

    call cumnor ( xcur, cum, ccum )
    dx = ( cum - pp ) / ( r2pi * exp ( -0.5D+00 * xcur * xcur ) )
    xcur = xcur - dx

    if ( abs ( dx / xcur ) < eps ) then
      if ( p <= q ) then
        dinvnr = xcur
      else
        dinvnr = -xcur
      end if
      return
    end if

  end do

  if ( p <= q ) then
    dinvnr = strtx
  else
    dinvnr = -strtx
  end if

  return
end
subroutine dinvr ( status, x, fx, qleft, qhi )

!*****************************************************************************80
!
!! DINVR bounds the zero of the function and invokes DZROR.
!
!  Discussion:
!
!    This routine seeks to find bounds on a root of the function and
!    invokes DZROR to perform the zero finding.  DSTINV must have been
!    called before this routine in order to set its parameters.
!
!  Reference:
!
!    JCP Bus, TJ Dekker,
!    Two Efficient Algorithms with Guaranteed Convergence for
!    Finding a Zero of a Function,
!    ACM Transactions on Mathematical Software,
!    Volume 1, Number 4, pages 330-345, 1975.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) STATUS.  At the beginning of a zero
!    finding problem, STATUS should be set to 0 and this routine invoked.
!    The value of parameters other than X will be ignored on this call.
!    If this routine needs the function to be evaluated, it will set STATUS
!    to 1 and return.  The value of the function should be set in FX and
!    this routine again called without changing any of its other parameters.
!    If this routine finishes without error, it returns with STATUS 0,
!    and X an approximate root of F(X).
!    If this routine cannot bound the function, it returns a negative STATUS and
!    sets QLEFT and QHI.
!
!    Output, real ( kind = 8 ) X, the value at which F(X) is to be evaluated.
!
!    Input, real ( kind = 8 ) FX, the value of F(X) calculated by the user
!    on the previous call, when this routine returned with STATUS = 1.
!
!    Output, logical QLEFT, is defined only if QMFINV returns FALSE.  In that
!    case, QLEFT is TRUE if the stepping search terminated unsucessfully
!    at SMALL, and FALSE if the search terminated unsucessfully at BIG.
!
!    Output, logical QHI, is defined only if QMFINV returns FALSE.  In that
!    case, it is TRUE if Y < F(X) at the termination of the search and FALSE
!    if F(X) < Y.
!
  implicit none

  real ( kind = 8 ) :: absstp
  real ( kind = 8 ) :: abstol
  real ( kind = 8 ) :: big
  real ( kind = 8 ) fbig
  real ( kind = 8 ) fsmall
  real ( kind = 8 ) fx
  integer ( kind = 4 ) i99999
  logical qbdd
  logical qcond
  logical qdum1
  logical qdum2
  logical qhi
  logical qincr
  logical qleft
  logical qlim
  logical qup
  real ( kind = 8 ) :: relstp
  real ( kind = 8 ) :: reltol
  real ( kind = 8 ) :: small
  integer ( kind = 4 ) status
  real ( kind = 8 ) step
  real ( kind = 8 ) :: stpmul
  real ( kind = 8 ) x
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlb
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xsave
  real ( kind = 8 ) xub
  real ( kind = 8 ) yy
  real ( kind = 8 ) zabsst
  real ( kind = 8 ) zabsto
  real ( kind = 8 ) zbig
  real ( kind = 8 ) zrelst
  real ( kind = 8 ) zrelto
  real ( kind = 8 ) zsmall
  real ( kind = 8 ) zstpmu

  save

  if ( 0 < status ) then
    go to i99999
  end if

  qcond = .not. ( small <= x .and. x <= big )

  if ( .not. ( small <= x .and. x <= big ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DINVR - Fatal error!'
    write ( *, '(a)' ) '  The values SMALL, X, BIG are not monotone.'
    stop
  end if

  xsave = x
!
!  See that SMALL and BIG bound the zero and set QINCR.
!
  x = small
!
!  GET-function-VALUE
!
  assign 10 to i99999
  status = 1
  return

   10 continue

  fsmall = fx
  x = big
!
!  GET-function-VALUE
!
  assign 20 to i99999
  status = 1
  return

   20 continue

  fbig = fx

  qincr = ( fsmall < fbig )

  if ( fsmall <= fbig ) then

    if ( 0.0D+00 < fsmall ) then
      status = -1
      qleft = .true.
      qhi = .true.
      return
    end if

    if ( fbig < 0.0D+00 ) then
      status = -1
      qleft = .false.
      qhi = .false.
      return
    end if

  else if ( fbig < fsmall ) then

    if ( fsmall < 0.0D+00 ) then
      status = -1
      qleft = .true.
      qhi = .false.
      return
    end if

    if ( 0.0D+00 < fbig ) then
      status = -1
      qleft = .false.
      qhi = .true.
      return
    end if

  end if

  x = xsave
  step = max ( absstp, relstp * abs ( x ) )
!
!  YY = F(X) - Y
!  GET-function-VALUE
!
  assign 90 to i99999
  status = 1
  return

   90 continue

  yy = fx

  if ( yy == 0.0D+00 ) then
    status = 0
    return
  end if

  100 continue

  qup = ( qincr .and. ( yy < 0.0D+00 ) ) .or. &
        ( .not. qincr .and. ( 0.0D+00 < yy ) )
!
!  Handle case in which we must step higher.
!
  if (.not. qup ) then
    go to 170
  end if

  xlb = xsave
  xub = min ( xlb + step, big )
  go to 120

  110 continue

  if ( qcond ) then
    go to 150
  end if
!
!  YY = F(XUB) - Y
!
  120 continue

  x = xub
!
!  GET-function-VALUE
!
  assign 130 to i99999
  status = 1
  return

  130 continue

  yy = fx
  qbdd = ( qincr .and. ( 0.0D+00 <= yy ) ) .or. &
    ( .not. qincr .and. ( yy <= 0.0D+00 ) )
  qlim = ( big <= xub )
  qcond = qbdd .or. qlim

  if ( .not. qcond ) then
    step = stpmul * step
    xlb = xub
    xub = min ( xlb + step, big )
  end if

  go to 110

  150 continue

  if ( qlim .and. .not. qbdd ) then
    status = -1
    qleft = .false.
    qhi = .not. qincr
    x = big
    return
  end if

  160 continue

  go to 240
!
!  Handle the case in which we must step lower.
!
  170 continue

  xub = xsave
  xlb = max ( xub - step, small )
  go to 190

  180 continue

  if ( qcond ) then
    go to 220
  end if
!
!  YY = F(XLB) - Y
!
  190 continue

  x = xlb
!
!  GET-function-VALUE
!
  assign 200 to i99999
  status = 1
  return

  200 continue

  yy = fx
  qbdd = ( qincr .and. ( yy <= 0.0D+00 ) ) .or. &
    ( .not. qincr .and. ( 0.0D+00 <= yy ) )
  qlim = xlb <= small
  qcond = qbdd .or. qlim

  if ( .not. qcond ) then
    step = stpmul * step
    xub = xlb
    xlb = max ( xub - step, small )
  end if

  go to 180

  220 continue

  if ( qlim .and. ( .not. qbdd ) ) then
    status = -1
    qleft = .true.
    qhi = qincr
    x = small
    return
  end if

  230 continue
  240 continue

  call dstzr ( xlb, xub, abstol, reltol )
!
!  If we reach here, XLB and XUB bound the zero of F.
!
  status = 0
  go to 260

  250 continue

    if ( status /= 1 ) then
      x = xlo
      status = 0
      return
    end if

  260 continue

  call dzror ( status, x, fx, xlo, xhi, qdum1, qdum2 )

  if ( status /= 1 ) then
    go to 250
  end if
!
!  GET-function-VALUE
!
  assign 270 to i99999
  status = 1
  return

  270 continue
  go to 250

entry dstinv ( zsmall, zbig, zabsst, zrelst, zstpmu, zabsto, zrelto )

!*****************************************************************************80
!
!! DSTINV SeT INverse finder - Reverse Communication
!
!  Discussion:
!
!    This routine is given a monotone function F, and a value Y,
!    and seeks an argument value X such that F(X) = Y.
!
!    This routine uses reverse communication -- see DINVR.
!    This routine sets quantities needed by DINVR.
!
!    F must be a monotone function, the results of QMFINV are
!    otherwise undefined.  QINCR must be TRUE if F is nondecreasing
!    and FALSE if F is nonincreasing.
!
!    QMFINV will return TRUE if and only if F(SMALL) and
!    F(BIG) bracket Y, i. e.,
!      QINCR is TRUE and F(SMALL) <= Y <= F(BIG) or
!      QINCR is FALSE and F(BIG) <= Y <= F(SMALL)
!
!    If QMFINV returns TRUE, then the X returned satisfies
!    the following condition.  Let
!      TOL(X) = MAX ( ABSTOL, RELTOL * ABS ( X ) )
!    then if QINCR is TRUE,
!      F(X-TOL(X)) <= Y <= F(X+TOL(X))
!    and if QINCR is FALSE
!      F(X-TOL(X)) .GE. Y .GE. F(X+TOL(X))
!
!    Compares F(X) with Y for the input value of X then uses QINCR
!    to determine whether to step left or right to bound the
!    desired X.  The initial step size is
!
!      max ( ABSSTP, RELSTP * ABS ( S ) )
!
!    for the input value of X.
!
!    Iteratively steps right or left until it bounds X.
!    At each step which doesn't bound X, the step size is doubled.
!    The routine is careful never to step beyond SMALL or BIG.  If
!    it hasn't bounded X at SMALL or BIG, QMFINV returns FALSE
!    after setting QLEFT and QHI.
!
!    If X is successfully bounded then Algorithm R of the paper
!    Bus and Dekker is employed to find the zero of the function F(X)-Y.
!    This is routine QRZERO.
!
!  Reference:
!
!    JCP Bus, TJ Dekker,
!    Two Efficient Algorithms with Guaranteed Convergence for
!    Finding a Zero of a Function,
!    ACM Transactions on Mathematical Software,
!    Volume 1, Number 4, pages 330-345, 1975.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ZSMALL, ZBIG, the left and right endpoints
!    of the interval to be searched for a solution.
!
!    Input, real ( kind = 8 ) ZABSST, ZRELSTP, the initial step size in
!    the search is max ( ZABSST, ZRELST * abs ( X ) ).
!
!    Input, real ( kind = 8 ) STPMUL.  When a step doesn't bound the zero,
!    the stepsize is multiplied by STPMUL and another step taken.  A
!    popular value is 2.0.
!
!    Input, real ( kind = 8 ) ABSTOL, RELTOL, two numbers that determine
!    the accuracy of the solution
!
  small = zsmall
  big = zbig
  absstp = zabsst
  relstp = zrelst
  stpmul = zstpmu
  abstol = zabsto
  reltol = zrelto

  return
end
function dlanor ( x )

!*****************************************************************************80
!
!! DLANOR evaluates the logarithm of the asymptotic Normal CDF.
!
!  Discussion:
!
!    This routine computes the logarithm of the cumulative normal distribution
!    from abs ( x ) to infinity for  5 <= abs ( X ).
!
!    The relative error at X = 5 is about 0.5D-5.
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.2.12.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the value at which the Normal CDF is to be
!    evaluated.  It is assumed that 5 <= abs ( X ).
!
!    Output, real ( kind = 8 ) DLANOR, the logarithm of the asymptotic
!    Normal CDF.
!
  implicit none

  real ( kind = 8 ) alnrel
  real ( kind = 8 ) approx
  real ( kind = 8 ), save, dimension ( 0:11 ) :: coef = (/ &
    -1.0D+00,  3.0D+00,  -15.0D+00,  105.0D+00,  -945.0D+00,  &
    10395.0D+00, -135135.0D+00,  2027025.0D+00,  -34459425.0D+00, &
    654729075.0D+00, -13749310575D+00,  316234143225.0D+00 /)
  real ( kind = 8 ) correc
  real ( kind = 8 ), parameter :: dlsqpi = 0.91893853320467274177D+00
  real ( kind = 8 ) eval_pol
  real ( kind = 8 ) dlanor
  real ( kind = 8 ) x
  real ( kind = 8 ) xx
  real ( kind = 8 ) xx2

  xx = abs ( x )

  if ( abs ( x ) < 5.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DLANOR - Fatal error!'
    write ( *, '(a)' ) '  The argument X is too small.'
  end if

  approx = - dlsqpi - 0.5D+00 * x**2 - log ( abs ( x ) )

  xx2 = xx * xx
  correc = eval_pol ( coef, 11, 1.0D+00 / xx2 ) / xx2
  correc = alnrel ( correc )

  dlanor = approx + correc

  return
end
function dstrem ( z )

!*****************************************************************************80
!
!! DSTREM computes the Sterling remainder ln ( Gamma ( Z ) ) - Sterling ( Z ).
!
!  Discussion:
!
!    This routine returns
!
!      ln ( Gamma ( Z ) ) - Sterling ( Z )
!
!    where Sterling(Z) is Sterling's approximation to ln ( Gamma ( Z ) ).
!
!    Sterling(Z) = ln ( sqrt ( 2 * PI ) ) + ( Z - 0.5 ) * ln ( Z ) - Z
!
!    If 6 <= Z, the routine uses 9 terms of a series in Bernoulli numbers,
!    with values calculated using Maple.
!
!    Otherwise, the difference is computed explicitly.
!
!  Modified:
!
!    14 June 2004
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Z, the value at which the Sterling
!    remainder is to be calculated.  Z must be positive.
!
!    Output, real ( kind = 8 ) DSTREM, the Sterling remainder.
!
  implicit none

  integer ( kind = 4 ), parameter :: ncoef = 9

  real ( kind = 8 ), parameter, dimension ( 0:ncoef ) :: coef = (/ &
    0.0D+00, &
    0.0833333333333333333333333333333D+00, &
    -0.00277777777777777777777777777778D+00, &
    0.000793650793650793650793650793651D+00, &
    -0.000595238095238095238095238095238D+00, &
    0.000841750841750841750841750841751D+00, &
    -0.00191752691752691752691752691753D+00, &
    0.00641025641025641025641025641026D+00, &
    -0.0295506535947712418300653594771D+00, &
    0.179644372368830573164938490016D+00 /)
  real ( kind = 8 ) dstrem
  real ( kind = 8 ) eval_pol
  real ( kind = 8 ) gamma_log
  real ( kind = 8 ), parameter :: hln2pi = 0.91893853320467274178D+00
  real ( kind = 8 ) sterl
  real ( kind = 8 ) z

  if ( z <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DSTREM - Fatal error!'
    write ( *, '(a)' ) '  Zero or negative argument Z.'
    stop
  end if

  if ( 6.0D+00 < z ) then
    dstrem = eval_pol ( coef, ncoef, 1.0D+00 / z**2 ) * z
  else
    sterl = hln2pi + ( z - 0.5D+00 ) * log ( z ) - z
    dstrem = gamma_log ( z ) - sterl
  end if

  return
end
function dt1 ( p, q, df )

!*****************************************************************************80
!
!! DT1 computes an approximate inverse of the cumulative T distribution.
!
!  Discussion:
!
!    This routine returns the inverse of the T distribution function, that is,
!    the integral from 0 to INVT of the T density is P.  This is an
!    initial approximation.
!
!    Thanks to Charles Katholi for pointing out that the RESHAPE
!    function should not use a range in the "SHAPE" field (0:4,4),
!    but simply the number of rows and columns (5,4), JVB, 04 May 2006.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, Q, the value whose inverse from the
!    T distribution CDF is desired, and the value (1-P).
!
!    Input, real ( kind = 8 ) DF, the number of degrees of freedom of the
!    T distribution.
!
!    Output, real ( kind = 8 ) DT1, the approximate value of X for which
!    the T density CDF with DF degrees of freedom has value P.
!
  implicit none

  real ( kind = 8 ), dimension(0:4,4) :: coef = reshape ( (/ &
       1.0D+00,     1.0D+00,    0.0D+00,   0.0D+00,  0.0D+00, &
       3.0D+00,    16.0D+00,    5.0D+00,   0.0D+00,  0.0D+00, &
     -15.0D+00,    17.0D+00,   19.0D+00,   3.0D+00,  0.0D+00, &
    -945.0D+00, -1920.0D+00, 1482.0D+00, 776.0D+00, 79.0D+00/), (/ 5, 4 /) )
  real ( kind = 8 ), parameter, dimension ( 4 ) :: denom = (/ &
    4.0D+00, 96.0D+00, 384.0D+00, 92160.0D+00 /)
  real ( kind = 8 ) denpow
  real ( kind = 8 ) eval_pol
  real ( kind = 8 ) df
  real ( kind = 8 ) dinvnr
  real ( kind = 8 ) dt1
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter, dimension ( 4 ) :: ideg = (/ 1, 2, 3, 4 /)
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) sum1
  real ( kind = 8 ) term
  real ( kind = 8 ) x
  real ( kind = 8 ) xp
  real ( kind = 8 ) xx

  x = abs ( dinvnr ( p, q ) )
  xx = x * x

  sum1 = x
  denpow = 1.0D+00
  do i = 1, 4
    term = eval_pol ( coef(0,i), ideg(i), xx ) * x
    denpow = denpow * df
    sum1 = sum1 + term / ( denpow * denom(i) )
  end do

  if ( 0.5D+00 <= p ) then
    xp = sum1
  else
    xp = -sum1
  end if

  dt1 = xp

  return
end
subroutine dzror ( status, x, fx, xlo, xhi, qleft, qhi )

!*****************************************************************************80
!
!! DZROR seeks a zero of a function, using reverse communication.
!
!  Discussion:
!
!    This routine performs the zero finding.  STZROR must have been called
!    before this routine in order to set its parameters.
!
!  Modified:
!
!    09 June 2004
!
!  Reference:
!
!    JCP Bus, TJ Dekker,
!    Two Efficient Algorithms with Guaranteed Convergence for
!    Finding a Zero of a Function,
!    ACM Transactions on Mathematical Software,
!    Volume 1, Number 4, pages 330-345, 1975.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) STATUS.  At the beginning of a zero
!    finding problem, STATUS should be set to 0 and ZROR invoked.  The value
!    of other parameters will be ignored on this call.
!    When ZROR needs the function evaluated, it will set
!    STATUS to 1 and return.  The value of the function
!    should be set in FX and ZROR again called without
!    changing any of its other parameters.
!    When ZROR has finished without error, it will return
!    with STATUS 0.  In that case (XLO,XHI) bound the answe
!    If ZROR finds an error (which implies that F(XLO)-Y an
!    F(XHI)-Y have the same sign, it returns STATUS -1.  In
!    this case, XLO and XHI are undefined.
!
!    Output, real ( kind = 8 ) X, the value of X at which F(X) is to
!    be evaluated.
!
!    Input, real ( kind = 8 ) FX, the value of F(X), which must be calculated
!    by the user when ZROR has returned on the previous call with STATUS = 1.
!
!    Output, real ( kind = 8 ) XLO, XHI, are lower and upper bounds for the
!    solution when ZROR returns with STATUS = 0.
!
!    Output, logical QLEFT,is TRUE if the stepping search terminated
!    unsucessfully at XLO.  If it is FALSE, the search terminated
!    unsucessfully at XHI.
!
!    Output, logical QHI, is TRUE if Y < F(X) at the termination of the
!    search and FALSE if F(X) < Y at the termination of the search.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) abstol
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  integer ( kind = 4 ) ext
  real ( kind = 8 ) fa
  real ( kind = 8 ) fb
  real ( kind = 8 ) fc
  real ( kind = 8 ) fd
  real ( kind = 8 ) fda
  real ( kind = 8 ) fdb
  logical first
  real ( kind = 8 ) ftol
  real ( kind = 8 ) fx
  integer ( kind = 4 ) i99999
  real ( kind = 8 ) m
  real ( kind = 8 ) mb
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  logical qrzero
  real ( kind = 8 ) reltol
  integer ( kind = 4 ) status
  real ( kind = 8 ) tol
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) :: xxhi = 0.0D+00
  real ( kind = 8 ) :: xxlo = 0.0D+00
  real ( kind = 8 ) zabstl
  real ( kind = 8 ) zreltl
  real ( kind = 8 ) zx
  real ( kind = 8 ) zxhi
  real ( kind = 8 ) zxlo

  save

  ftol(zx) = 0.5D+00 * max ( abstol, reltol * abs ( zx ) )

  if ( 0 < status ) then
    go to 280
  end if

  xlo = xxlo
  xhi = xxhi
  b = xlo
  x = xlo
!
!     GET-function-VALUE
!
  assign 10 to i99999
  go to 270

10 continue

  fb = fx
  xlo = xhi
  a = xlo
  x = xlo
!
!     GET-function-VALUE
!
  assign 20 to i99999
  go to 270
!
!  Check that F(ZXLO) < 0 < F(ZXHI)  or F(ZXLO) > 0 > F(ZXHI)
!
20 continue

  if ( fb < 0.0D+00 ) then
    if ( fx < 0.0D+00 ) then
      status = -1
      qleft = ( fx < fb )
      qhi = .false.
      return
    end if
  end if

  if ( 0.0D+00 < fb ) then
    if ( 0.0D+00 < fx ) then
      status = -1
      qleft = ( fb < fx )
      qhi = .true.
      return
    end if
  end if

  fa = fx
  first = .true.

70 continue

  c = a
  fc = fa
  ext = 0

80 continue

  if ( abs ( fc ) < abs ( fb ) ) then

    if ( c == a ) then
      d = a
      fd = fa
    end if

    a = b
    fa = fb
    xlo = c
    b = xlo
    fb = fc
    c = a
    fc = fa

  end if

  tol = ftol ( xlo )
  m = ( c + b ) * 0.5D+00
  mb = m - b

  if (.not. ( tol < abs ( mb ) ) ) then
    go to 240
  end if

  if ( 3 < ext ) then
    w = mb
    go to 190
  end if

  110 continue

  tol = sign ( tol, mb )
  p = ( b - a ) * fb
!
!  I had to insert a rudimentary check on the divisions here
!  to avoid ninny errors, JVB, 09 June 2004.
!
  if ( first ) then

    q = fa - fb
    first = .false.

  else

    if ( d == b ) then
      fdb = 1.0D+00
    else
      fdb = ( fd - fb ) / ( d - b )
    end if

    if ( d == a ) then
      fda = 1.0D+00
    else
      fda = ( fd - fa ) / ( d - a )
    end if

    p = fda * p
    q = fdb * fa - fda * fb

  end if

  130 continue

  if ( p < 0.0D+00 ) then
    p = -p
    q = -q
  end if

  140 continue

  if ( ext == 3 ) then
    p = p *  2.0D+00
  end if

  if (.not. ( ( p * 1.0D+00 ) == 0.0D+00 .or. p <= ( q * tol ) ) ) then
    go to 150
  end if

  w = tol
  go to 180

  150 continue

  if ( p < mb * q ) then
    w = p / q
  else
    w = mb
  end if

  180 continue
  190 continue

  d = a
  fd = fa
  a = b
  fa = fb
  b = b + w
  xlo = b
  x = xlo
!
!  GET-function-VALUE
!
  assign 200 to i99999
  go to 270

  200 continue

  fb = fx

  if ( 0.0D+00 <= fc * fb ) then

    go to 70

  else

    if ( w == mb ) then
      ext = 0
    else
      ext = ext + 1
    end if

    go to 80

  end if

  240 continue

  xhi = c
  qrzero = ( 0.0D+00 <= fc .and. fb <= 0.0D+00 ) .or. &
    ( fc < 0.0D+00 .and. fb >= 0.0D+00 )

  if ( qrzero ) then
    status = 0
  else
    status = -1
  end if

  return

entry dstzr ( zxlo, zxhi, zabstl, zreltl )

!*****************************************************************************80
!
!! DSTZR - SeT ZeRo finder - Reverse communication version
!
!  Discussion:
!
!    This routine sets quantities needed by ZROR.  The function of ZROR
!    and the quantities set is given here.
!
!    Given a function F, find XLO such that F(XLO) = 0.
!
!     Input condition. F is a real ( kind = 8 ) function of a single
!     real ( kind = 8 ) argument and XLO and XHI are such that
!          F(XLO)*F(XHI)  <=  0.0
!
!     If the input condition is met, QRZERO returns .TRUE.
!     and output values of XLO and XHI satisfy the following
!          F(XLO)*F(XHI)  <= 0.
!          ABS ( F(XLO) ) <= ABS ( F(XHI) )
!          ABS ( XLO - XHI ) <= TOL(X)
!     where
!          TOL(X) = MAX ( ABSTOL, RELTOL * ABS ( X ) )
!
!     If this algorithm does not find XLO and XHI satisfying
!     these conditions then QRZERO returns .FALSE.  This
!     implies that the input condition was not met.
!
!  Reference:
!
!    JCP Bus, TJ Dekker,
!    Two Efficient Algorithms with Guaranteed Convergence for
!    Finding a Zero of a Function,
!    ACM Transactions on Mathematical Software,
!    Volume 1, Number 4, pages 330-345, 1975.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XLO, XHI, the left and right endpoints of the
!    interval to be searched for a solution.
!
!    Input, real ( kind = 8 ) ABSTOL, RELTOL, two numbers that determine
!    the accuracy of the solution.
!
  xxlo = zxlo
  xxhi = zxhi
  abstol = zabstl
  reltol = zreltl
  return
!
!     TO GET-function-VALUE
!
  270 status = 1
  return

  280 continue
  go to i99999

end
subroutine erf_values ( n_data, x, fx )

!*****************************************************************************80
!
!! ERF_VALUES returns some values of the ERF or "error" function.
!
!  Discussion:
!
!    ERF(X) = ( 2 / sqrt ( PI ) * integral ( 0 <= T <= X ) exp ( - T^2 ) dT
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 21

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.0000000000D+00, 0.1124629160D+00, 0.2227025892D+00, 0.3286267595D+00, &
    0.4283923550D+00, 0.5204998778D+00, 0.6038560908D+00, 0.6778011938D+00, &
    0.7421009647D+00, 0.7969082124D+00, 0.8427007929D+00, 0.8802050696D+00, &
    0.9103139782D+00, 0.9340079449D+00, 0.9522851198D+00, 0.9661051465D+00, &
    0.9763483833D+00, 0.9837904586D+00, 0.9890905016D+00, 0.9927904292D+00, &
    0.9953222650D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.0D+00, 0.1D+00, 0.2D+00, 0.3D+00, &
    0.4D+00, 0.5D+00, 0.6D+00, 0.7D+00, &
    0.8D+00, 0.9D+00, 1.0D+00, 1.1D+00, &
    1.2D+00, 1.3D+00, 1.4D+00, 1.5D+00, &
    1.6D+00, 1.7D+00, 1.8D+00, 1.9D+00, &
    2.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
function error_f ( x )

!*****************************************************************************80
!
!! ERROR_F evaluates the error function.
!
!  Discussion:
!
!    Since some compilers already supply a routine named ERF which evaluates
!    the error function, this routine has been given a distinct, if
!    somewhat unnatural, name.
!
!    The function is defined by:
!
!      ERF(X) = ( 2 / sqrt ( PI ) )
!        * Integral ( 0 <= T <= X ) EXP ( - T**2 ) dT.
!
!    Properties of the function include:
!
!      Limit ( X -> -Infinity ) ERF(X) =          -1.0;
!                               ERF(0) =           0.0;
!                               ERF(0.476936...) = 0.5;
!      Limit ( X -> +Infinity ) ERF(X) =          +1.0.
!
!      0.5D+00 * ( ERF(X/sqrt(2)) + 1 ) = Normal_01_CDF(X)
!
!  Modified:
!
!    17 November 2006
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument.
!
!    Output, real ( kind = 8 ) ERF, the value of the error function at X.
!
  implicit none

  real ( kind = 8 ), parameter, dimension ( 5 ) :: a = (/ &
    0.771058495001320D-04, &
    -0.133733772997339D-02, &
    0.323076579225834D-01, &
    0.479137145607681D-01, &
    0.128379167095513D+00 /)
  real ( kind = 8 ) ax
  real ( kind = 8 ), parameter, dimension ( 3 ) :: b = (/ &
    0.301048631703895D-02, &
    0.538971687740286D-01, &
    0.375795757275549D+00 /)
  real ( kind = 8 ) bot
  real ( kind = 8 ), parameter :: c = 0.564189583547756D+00
  real ( kind = 8 ) error_f
  real ( kind = 8 ), dimension ( 8 ) :: p = (/   &
   -1.36864857382717D-07, 5.64195517478974D-01, &
    7.21175825088309D+00, 4.31622272220567D+01, &
    1.52989285046940D+02, 3.39320816734344D+02, &
    4.51918953711873D+02, 3.00459261020162D+02 /)
  real ( kind = 8 ), dimension ( 8 ) :: q = (/ &
    1.00000000000000D+00, 1.27827273196294D+01, &
    7.70001529352295D+01, 2.77585444743988D+02, &
    6.38980264465631D+02, 9.31354094850610D+02, &
    7.90950925327898D+02, 3.00459260956983D+02 /)
  real ( kind = 8 ), dimension ( 5 ) :: r = (/ &
    2.10144126479064D+00, 2.62370141675169D+01, &
    2.13688200555087D+01, 4.65807828718470D+00, &
    2.82094791773523D-01 /)
  real ( kind = 8 ), parameter, dimension ( 4 ) :: s = (/ &
    9.41537750555460D+01, 1.87114811799590D+02, &
    9.90191814623914D+01, 1.80124575948747D+02 /)
  real ( kind = 8 ) t
  real ( kind = 8 ) top
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  ax = abs ( x )

  if ( ax <= 0.5D+00 ) then

    t = x * x

    top = (((( a(1)   * t &
             + a(2) ) * t &
             + a(3) ) * t &
             + a(4) ) * t &
             + a(5) ) + 1.0D+00

    bot = (( b(1) * t + b(2) ) * t + b(3) ) * t + 1.0D+00
    error_f = ax * ( top / bot )

  else if ( ax <= 4.0D+00 ) then

    top = (((((( p(1)   * ax &
               + p(2) ) * ax &
               + p(3) ) * ax &
               + p(4) ) * ax &
               + p(5) ) * ax &
               + p(6) ) * ax &
               + p(7) ) * ax &
               + p(8)

    bot = (((((( q(1) * ax + q(2) ) * ax + q(3) ) * ax + q(4) ) * ax &
      + q(5) ) * ax + q(6) ) * ax + q(7) ) * ax + q(8)

    error_f = 0.5D+00 &
      + ( 0.5D+00 - exp ( - x * x ) * top / bot )

  else if ( ax < 5.8D+00 ) then

    x2 = x * x
    t = 1.0D+00 / x2

    top = ((( r(1) * t + r(2) ) * t + r(3) ) * t + r(4) ) * t + r(5)

    bot = ((( s(1) * t + s(2) ) * t + s(3) ) * t + s(4) ) * t &
      + 1.0D+00

    error_f = ( c - top / ( x2 * bot )) / ax
    error_f = 0.5D+00 &
      + ( 0.5D+00 - exp ( - x2 ) * error_f )

  else

    error_f = 1.0D+00

  end if

  if ( x < 0.0D+00 ) then
    error_f = -error_f
  end if

  return
end
function error_fc ( ind, x )

!*****************************************************************************80
!
!! ERROR_FC evaluates the complementary error function.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IND, chooses the scaling.
!    If IND is nonzero, then the value returned has been multiplied by
!    EXP(X*X).
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) ERROR_FC, the value of the complementary
!    error function.
!
  implicit none

  real ( kind = 8 ), dimension ( 5 ) :: a = (/ &
     0.771058495001320D-04,  -0.133733772997339D-02, &
     0.323076579225834D-01,   0.479137145607681D-01, &
     0.128379167095513D+00 /)
  real ( kind = 8 ) ax
  real ( kind = 8 ), dimension(3) :: b = (/ &
    0.301048631703895D-02, &
    0.538971687740286D-01, &
    0.375795757275549D+00 /)
  real ( kind = 8 ) bot
  real ( kind = 8 ), parameter :: c = 0.564189583547756D+00
  real ( kind = 8 ) e
  real ( kind = 8 ) error_fc
  real ( kind = 8 ) exparg
  integer ( kind = 4 ) ind
  real ( kind = 8 ), dimension ( 8 ) :: p = (/ &
    -1.36864857382717D-07, 5.64195517478974D-01, &
     7.21175825088309D+00, 4.31622272220567D+01, &
     1.52989285046940D+02, 3.39320816734344D+02, &
     4.51918953711873D+02, 3.00459261020162D+02 /)
  real ( kind = 8 ), dimension ( 8 ) :: q = (/  &
    1.00000000000000D+00, 1.27827273196294D+01, &
    7.70001529352295D+01, 2.77585444743988D+02, &
    6.38980264465631D+02, 9.31354094850610D+02, &
    7.90950925327898D+02, 3.00459260956983D+02 /)
  real ( kind = 8 ), dimension ( 5 ) :: r = (/ &
    2.10144126479064D+00, 2.62370141675169D+01, &
    2.13688200555087D+01, 4.65807828718470D+00, &
    2.82094791773523D-01 /)
  real ( kind = 8 ), dimension ( 4 ) :: s = (/ &
    9.41537750555460D+01, 1.87114811799590D+02, &
    9.90191814623914D+01, 1.80124575948747D+02 /)
  real ( kind = 8 ) t
  real ( kind = 8 ) top
  real ( kind = 8 ) w
  real ( kind = 8 ) x
!
!  ABS ( X ) <= 0.5
!
  ax = abs ( x )

  if ( ax <= 0.5D+00 ) then

    t = x * x

    top = (((( a(1) * t + a(2) ) * t + a(3) ) * t + a(4) ) * t + a(5) ) &
      + 1.0D+00

    bot = (( b(1) * t + b(2) ) * t + b(3) ) * t + 1.0D+00

    error_fc = 0.5D+00 + ( 0.5D+00 &
      - x * ( top / bot ) )

    if ( ind /= 0 ) then
      error_fc = exp ( t ) * error_fc
    end if

    return

  end if
!
!  0.5 < abs ( X ) <= 4
!
  if ( ax <= 4.0D+00 ) then

    top = (((((( p(1) * ax + p(2)) * ax + p(3)) * ax + p(4)) * ax &
      + p(5)) * ax + p(6)) * ax + p(7)) * ax + p(8)

    bot = (((((( q(1) * ax + q(2)) * ax + q(3)) * ax + q(4)) * ax &
      + q(5)) * ax + q(6)) * ax + q(7)) * ax + q(8)

    error_fc = top / bot
!
!  4 < ABS ( X )
!
  else

    if ( x <= -5.6D+00 ) then

      if ( ind == 0 ) then
        error_fc =  2.0D+00
      else
        error_fc =  2.0D+00  * exp ( x * x )
      end if

      return

    end if

    if ( ind == 0 ) then

      if ( 100.0D+00 < x ) then
        error_fc = 0.0D+00
        return
      end if

      if ( -exparg ( 1 ) < x * x ) then
        error_fc = 0.0D+00
        return
      end if

    end if

    t = ( 1.0D+00 / x )**2

    top = ((( r(1) * t + r(2) ) * t + r(3) ) * t + r(4) ) * t + r(5)

    bot = ((( s(1) * t + s(2) ) * t + s(3) ) * t + s(4) ) * t &
      + 1.0D+00

    error_fc = ( c - t * top / bot ) / ax

  end if
!
!  Final assembly.
!
  if ( ind /= 0 ) then

    if ( x < 0.0D+00 ) then
      error_fc =  2.0D+00  * exp ( x * x ) - error_fc
    end if

  else

    w = x * x
    t = w
    e = w - t
    error_fc = (( 0.5D+00 &
      + ( 0.5D+00 - e ) ) * exp ( - t ) ) * error_fc

    if ( x < 0.0D+00 ) then
      error_fc =  2.0D+00  - error_fc
    end if

  end if

  return
end
function esum ( mu, x )

!*****************************************************************************80
!
!! ESUM evaluates exp ( MU + X ).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MU, part of the argument.
!
!    Input, real ( kind = 8 ) X, part of the argument.
!
!    Output, real ( kind = 8 ) ESUM, the value of exp ( MU + X ).
!
  implicit none

  real ( kind = 8 ) esum
  integer ( kind = 4 ) mu
  real ( kind = 8 ) w
  real ( kind = 8 ) x

  if ( x <= 0.0D+00 ) then
    if ( 0 <= mu ) then
      w = mu + x
      if ( w <= 0.0D+00 ) then
        esum = exp ( w )
        return
      end if
    end if
  else if ( 0.0D+00 < x ) then
    if ( mu <= 0 ) then
      w = mu + x
      if ( 0.0D+00 <= w ) then
        esum = exp ( w )
        return
      end if
    end if
  end if

  w = mu
  esum = exp ( w ) * exp ( x )

  return
end
function eval_pol ( a, n, x )

!*****************************************************************************80
!
!! EVAL_POL evaluates a polynomial at X.
!
!  Discussion:
!
!    EVAL_POL = A(0) + A(1)*X + ... + A(N)*X**N
!
!  Modified:
!
!    15 December 1999
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(0:N), coefficients of the polynomial.
!
!    Input, integer ( kind = 4 ) N, length of A.
!
!    Input, real ( kind = 8 ) X, the point at which the polynomial
!    is to be evaluated.
!
!    Output, real ( kind = 8 ) EVAL_POL, the value of the polynomial at X.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(0:n)
  real ( kind = 8 ) eval_pol
  integer ( kind = 4 ) i
  real ( kind = 8 ) term
  real ( kind = 8 ) x

  term = a(n)
  do i = n - 1, 0, -1
    term = term * x + a(i)
  end do

  eval_pol = term

  return
end
function exparg ( l )

!*****************************************************************************80
!
!! EXPARG returns the largest or smallest legal argument for EXP.
!
!  Discussion:
!
!    Only an approximate limit for the argument of EXP is desired.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) L, indicates which limit is desired.
!    If L = 0, then the largest positive argument for EXP is desired.
!    Otherwise, the largest negative argument for EXP for which the
!    result is nonzero is desired.
!
!    Output, real ( kind = 8 ) EXPARG, the desired value.
!
  implicit none

  integer ( kind = 4 ) b
  real ( kind = 8 ) exparg
  integer ( kind = 4 ) ipmpar
  integer ( kind = 4 ) l
  real ( kind = 8 ) lnb
  integer ( kind = 4 ) m
!
!  Get the arithmetic base.
!
  b = ipmpar(4)
!
!  Compute the logarithm of the arithmetic base.
!
  if ( b == 2 ) then
    lnb = 0.69314718055995D+00
  else if ( b == 8 ) then
    lnb = 2.0794415416798D+00
  else if ( b == 16 ) then
    lnb = 2.7725887222398D+00
  else
    lnb = log ( real ( b, kind = 8 ) )
  end if

  if ( l /= 0 ) then
    m = ipmpar(9) - 1
    exparg = 0.99999D+00 * ( m * lnb )
  else
    m = ipmpar(10)
    exparg = 0.99999D+00 * ( m * lnb )
  end if

  return
end
subroutine f_cdf_values ( n_data, a, b, x, fx )

!*****************************************************************************80
!
!! F_CDF_VALUES returns some values of the F CDF test function.
!
!  Discussion:
!
!    The value of F_CDF ( DFN, DFD, X ) can be evaluated in Mathematica by
!    commands like:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF[FRatioDistribution[ DFN, DFD ], X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) A, integer B, real ( kind = 8 ) X, the
!    arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  integer ( kind = 4 ) a
  integer ( kind = 4 ), save, dimension ( n_max ) :: a_vec = (/ &
    1, 1, 5, 1, &
    2, 4, 1, 6, &
    8, 1, 3, 6, &
    1, 1, 1, 1, &
    2, 3, 4, 5 /)
  integer ( kind = 4 ) b
  integer ( kind = 4 ), save, dimension ( n_max ) :: b_vec = (/ &
     1,  5,  1,  5, &
    10, 20,  5,  6, &
    16,  5, 10, 12, &
     5,  5,  5,  5, &
     5,  5,  5,  5 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.500000D+00, 0.499971D+00, 0.499603D+00, 0.749699D+00, &
    0.750466D+00, 0.751416D+00, 0.899987D+00, 0.899713D+00, &
    0.900285D+00, 0.950025D+00, 0.950057D+00, 0.950193D+00, &
    0.975013D+00, 0.990002D+00, 0.994998D+00, 0.999000D+00, &
    0.568799D+00, 0.535145D+00, 0.514343D+00, 0.500000D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    1.00D+00,  0.528D+00, 1.89D+00,  1.69D+00, &
    1.60D+00,  1.47D+00,  4.06D+00,  3.05D+00, &
    2.09D+00,  6.61D+00,  3.71D+00,  3.00D+00, &
   10.01D+00, 16.26D+00, 22.78D+00, 47.18D+00, &
    1.00D+00,  1.00D+00,  1.00D+00,  1.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0
    b = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    b = b_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine f_noncentral_cdf_values ( n_data, a, b, lambda, x, fx )

!*****************************************************************************80
!
!! F_NONCENTRAL_CDF_VALUES returns some values of the F CDF test function.
!
!  Discussion:
!
!    The value of NONCENTRAL_F_CDF ( DFN, DFD, LAMDA, X ) can be evaluated
!    in Mathematica by commands like:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF[NoncentralFRatioDistribution[ DFN, DFD, LAMBDA ], X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) A, B, real ( kind = 8 ) LAMBDA, the
!    parameters of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 22

  integer ( kind = 4 ) a
  integer ( kind = 4 ), save, dimension ( n_max ) :: a_vec = (/ &
     1,  1,  1,  1, &
     1,  1,  1,  1, &
     1,  1,  2,  2, &
     3,  3,  4,  4, &
     5,  5,  6,  6, &
     8, 16 /)
  integer ( kind = 4 ) b
  integer ( kind = 4 ), save, dimension ( n_max ) :: b_vec = (/ &
     1,  5,  5,  5, &
     5,  5,  5,  5, &
     5,  5,  5, 10, &
     5,  5,  5,  5, &
     1,  5,  6, 12, &
    16,  8 /)
  real ( kind = 8 ) fx
  real, save, dimension ( n_max ) :: fx_vec = (/ &
    0.500000D+00, 0.636783D+00, 0.584092D+00, 0.323443D+00, &
    0.450119D+00, 0.607888D+00, 0.705928D+00, 0.772178D+00, &
    0.819105D+00, 0.317035D+00, 0.432722D+00, 0.450270D+00, &
    0.426188D+00, 0.337744D+00, 0.422911D+00, 0.692767D+00, &
    0.363217D+00, 0.421005D+00, 0.426667D+00, 0.446402D+00, &
    0.844589D+00, 0.816368D+00 /)
  real ( kind = 8 ) lambda
  real, save, dimension ( n_max ) :: lambda_vec = (/ &
    0.00D+00,  0.000D+00, 0.25D+00,  1.00D+00, &
    1.00D+00,  1.00D+00,  1.00D+00,  1.00D+00, &
    1.00D+00,  2.00D+00,  1.00D+00,  1.00D+00, &
    1.00D+00,  2.00D+00,  1.00D+00,  1.00D+00, &
    0.00D+00,  1.00D+00,  1.00D+00,  1.00D+00, &
    1.00D+00,  1.00D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real, save, dimension ( n_max ) :: x_vec = (/ &
    1.00D+00,  1.00D+00, 1.00D+00,  0.50D+00, &
    1.00D+00,  2.00D+00, 3.00D+00,  4.00D+00, &
    5.00D+00,  1.00D+00, 1.00D+00,  1.00D+00, &
    1.00D+00,  1.00D+00, 1.00D+00,  2.00D+00, &
    1.00D+00,  1.00D+00, 1.00D+00,  1.00D+00, &
    2.00D+00,  2.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0
    b = 0
    lambda = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    b = b_vec(n_data)
    lambda = lambda_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
function fpser ( a, b, x, eps )

!*****************************************************************************80
!
!! FPSER evaluates IX(A,B)(X) for very small B.
!
!  Discussion:
!
!    This routine is appropriate for use when
!
!      B < min ( EPS, EPS * A )
!
!    and
!
!      X <= 0.5.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, parameters of the function.
!
!    Input, real ( kind = 8 ) X, the point at which the function is to
!    be evaluated.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, real ( kind = 8 ) FPSER, the value of IX(A,B)(X).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) an
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) eps
  real ( kind = 8 ) exparg
  real ( kind = 8 ) fpser
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  real ( kind = 8 ) x

  fpser = 1.0D+00

  if ( 1.0D-03 * eps < a ) then
    fpser = 0.0D+00
    t = a * log ( x )
    if ( t < exparg ( 1 ) ) then
      return
    end if
    fpser = exp ( t )
  end if
!
!  1/B(A,B) = B
!
  fpser = ( b / a ) * fpser
  tol = eps / a
  an = a + 1.0D+00
  t = x
  s = t / an

  do

    an = an + 1.0D+00
    t = x * t
    c = t / an
    s = s + c

    if ( abs ( c ) <= tol ) then
      exit
    end if

  end do

  fpser = fpser * ( 1.0D+00 + a * s )

  return
end
function gam1 ( a )

!*****************************************************************************80
!
!! GAM1 computes 1 / GAMMA(A+1) - 1 for -0.5 <= A <= 1.5
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, forms the argument of the Gamma function.
!
!    Output, real ( kind = 8 ) GAM1, the value of 1 / GAMMA ( A + 1 ) - 1.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) bot
  real ( kind = 8 ) d
  real ( kind = 8 ) gam1
  real ( kind = 8 ), parameter, dimension ( 7 ) :: p = (/ &
     0.577215664901533D+00, -0.409078193005776D+00, &
    -0.230975380857675D+00,  0.597275330452234D-01, &
     0.766968181649490D-02, -0.514889771323592D-02, &
     0.589597428611429D-03 /)
  real ( kind = 8 ), dimension ( 5 ) :: q = (/ &
    0.100000000000000D+01, 0.427569613095214D+00, &
    0.158451672430138D+00, 0.261132021441447D-01, &
    0.423244297896961D-02 /)
  real ( kind = 8 ), dimension ( 9 ) :: r = (/ &
    -0.422784335098468D+00, -0.771330383816272D+00, &
    -0.244757765222226D+00,  0.118378989872749D+00, &
     0.930357293360349D-03, -0.118290993445146D-01, &
     0.223047661158249D-02,  0.266505979058923D-03, &
    -0.132674909766242D-03 /)
  real ( kind = 8 ), parameter :: s1 = 0.273076135303957D+00
  real ( kind = 8 ), parameter :: s2 = 0.559398236957378D-01
  real ( kind = 8 ) t
  real ( kind = 8 ) top
  real ( kind = 8 ) w

  d = a - 0.5D+00

  if ( 0.0D+00 < d ) then
    t = d - 0.5D+00
  else
    t = a
  end if

  if ( t == 0.0D+00 ) then

    gam1 = 0.0D+00

  else if ( 0.0D+00 < t ) then

    top = (((((    &
            p(7)   &
      * t + p(6) ) &
      * t + p(5) ) &
      * t + p(4) ) &
      * t + p(3) ) &
      * t + p(2) ) &
      * t + p(1)

    bot = ((( q(5) * t + q(4) ) * t + q(3) ) * t + q(2) ) * t &
      + 1.0D+00

    w = top / bot

    if ( d <= 0.0D+00 ) then
      gam1 = a * w
    else
      gam1 = ( t / a ) * ( ( w - 0.5D+00 ) &
        - 0.5D+00 )
    end if

  else if ( t < 0.0D+00 ) then

    top = (((((((  &
            r(9)   &
      * t + r(8) ) &
      * t + r(7) ) &
      * t + r(6) ) &
      * t + r(5) ) &
      * t + r(4) ) &
      * t + r(3) ) &
      * t + r(2) ) &
      * t + r(1)

    bot = ( s2 * t + s1 ) * t + 1.0D+00
    w = top / bot

    if ( d <= 0.0D+00 ) then
      gam1 = a * ( ( w + 0.5D+00 ) + 0.5D+00 )
    else
      gam1 = t * w / a
    end if

  end if

  return
end
function gamma ( a )

!*****************************************************************************80
!
!! GAMMA evaluates the gamma function.
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the argument of the Gamma function.
!
!    Output, real ( kind = 8 ) GAMMA, the value of the Gamma function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) bot
  real ( kind = 8 ), parameter :: d = 0.41893853320467274178D+00
  real ( kind = 8 ) exparg
  real ( kind = 8 ) g
  real ( kind = 8 ) gamma
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) lnx
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ), dimension ( 7 ) :: p = (/ &
    0.539637273585445D-03, 0.261939260042690D-02, &
    0.204493667594920D-01, 0.730981088720487D-01, &
    0.279648642639792D+00, 0.553413866010467D+00, &
    1.0D+00 /)
  real ( kind = 8 ), parameter :: pi = 3.1415926535898D+00
  real ( kind = 8 ), dimension ( 7 ) :: q = (/ &
    -0.832979206704073D-03,  0.470059485860584D-02, &
     0.225211131035340D-01, -0.170458969313360D+00, &
    -0.567902761974940D-01,  0.113062953091122D+01, &
     1.0D+00 /)
  real ( kind = 8 ), parameter :: r1 =  0.820756370353826D-03
  real ( kind = 8 ), parameter :: r2 = -0.595156336428591D-03
  real ( kind = 8 ), parameter :: r3 =  0.793650663183693D-03
  real ( kind = 8 ), parameter :: r4 = -0.277777777770481D-02
  real ( kind = 8 ), parameter :: r5 =  0.833333333333333D-01
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) top
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) z

  gamma = 0.0D+00
  x = a

  if ( abs ( a ) < 15.0D+00 ) then
!
!  Evaluation of GAMMA(A) for |A| < 15
!
    t = 1.0D+00
    m = int ( a ) - 1
!
!  Let T be the product of A-J when 2 <= A.
!
    if ( 0 <= m ) then

      do j = 1, m
        x = x - 1.0D+00
        t = x * t
      end do

      x = x - 1.0D+00
!
!  Let T be the product of A+J WHEN A < 1
!
    else

      t = a

      if ( a <= 0.0D+00 ) then

        m = - m - 1

        do j = 1, m
          x = x + 1.0D+00
          t = x * t
        end do

        x = ( x + 0.5D+00 ) + 0.5D+00
        t = x * t
        if ( t == 0.0D+00 ) then
          return
        end if

      end if
!
!  Check if 1/T can overflow.
!
      if ( abs ( t ) < 1.0D-30 ) then
        if ( 1.0001D+00 < abs ( t ) * huge ( t ) ) then
          gamma = 1.0D+00 / t
        end if
        return
      end if

    end if
!
!  Compute Gamma(1 + X) for 0 <= X < 1.
!
    top = p(1)
    bot = q(1)
    do i = 2, 7
      top = top * x + p(i)
      bot = bot * x + q(i)
    end do

    gamma = top / bot
!
!  Termination.
!
    if ( 1.0D+00 <= a ) then
      gamma = gamma * t
    else
      gamma = gamma / t
    end if
!
!  Evaluation of Gamma(A) FOR 15 <= ABS ( A ).
!
  else

    if ( 1000.0D+00 <= abs ( a ) ) then
      return
    end if

    if ( a <= 0.0D+00 ) then

      x = -a
      n = x
      t = x - n

      if ( 0.9D+00 < t ) then
        t = 1.0D+00 - t
      end if

      s = sin ( pi * t ) / pi

      if ( mod ( n, 2 ) == 0 ) then
        s = -s
      end if

      if ( s == 0.0D+00 ) then
        return
      end if

    end if
!
!  Compute the modified asymptotic sum.
!
    t = 1.0D+00 / ( x * x )

    g = (((( r1 * t + r2 ) * t + r3 ) * t + r4 ) * t + r5 ) / x

    lnx = log ( x )
!
!  Final assembly.
!
    z = x
    g = ( d + g ) + ( z - 0.5D+00 ) &
      * ( lnx - 1.0D+00 )
    w = g
    t = g - real ( w, kind = 8 )

    if ( 0.99999D+00 * exparg ( 0 ) < w ) then
      return
    end if

    gamma = exp ( w )* ( 1.0D+00 + t )

    if ( a < 0.0D+00 ) then
      gamma = ( 1.0D+00 / ( gamma * s ) ) / x
    end if

  end if

  return
end
subroutine gamma_inc ( a, x, ans, qans, ind )

!*****************************************************************************80
!
!! GAMMA_INC evaluates the incomplete gamma ratio functions P(A,X) and Q(A,X).
!
!  Modified:
!
!    16 April 2005
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, X, the arguments of the incomplete
!    gamma ratio.  A and X must be nonnegative.  A and X cannot
!    both be zero.
!
!    Output, real ( kind = 8 ) ANS, QANS.  On normal output,
!    ANS = P(A,X) and QANS = Q(A,X).  However, ANS is set to 2 if
!    A or X is negative, or both are 0, or when the answer is
!    computationally indeterminate because A is extremely large
!    and X is very close to A.
!
!    Input, integer ( kind = 4 ) IND, indicates the accuracy request:
!    0, as much accuracy as possible.
!    1, to within 1 unit of the 6-th significant digit,
!    otherwise, to within 1 unit of the 3rd significant digit.
!
!  Local Parameters:
!
!     ALOG10 = LN(10)
!     RT2PIN = 1/SQRT(2*PI)
!     RTPI   = SQRT(PI)
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2n
  real ( kind = 8 ) a2nm1
  real ( kind = 8 ) acc
  real ( kind = 8 ), dimension ( 3 ) :: acc0 = (/ &
    5.0D-15, 5.0D-07, 5.0D-04 /)
  real ( kind = 8 ), parameter :: alog10 = 2.30258509299405D+00
  real ( kind = 8 ) am0
  real ( kind = 8 ) amn
  real ( kind = 8 ) an
  real ( kind = 8 ) an0
  real ( kind = 8 ) ans
  real ( kind = 8 ) apn
  real ( kind = 8 ) b2n
  real ( kind = 8 ) b2nm1
  real ( kind = 8 ) big(3)
  real ( kind = 8 ) c
  real ( kind = 8 ) c0
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) c3
  real ( kind = 8 ) c4
  real ( kind = 8 ) c5
  real ( kind = 8 ) c6
  real ( kind = 8 ) cma
  real ( kind = 8 ) d0(13)
  real ( kind = 8 ) d1(12)
  real ( kind = 8 ) d2(10)
  real ( kind = 8 ) d3(8)
  real ( kind = 8 ) d4(6)
  real ( kind = 8 ) d5(4)
  real ( kind = 8 ) d6(2)
  real ( kind = 8 ) d10
  real ( kind = 8 ) d20
  real ( kind = 8 ) d30
  real ( kind = 8 ) d40
  real ( kind = 8 ) d50
  real ( kind = 8 ) d60
  real ( kind = 8 ) d70
  real ( kind = 8 ) e
  real ( kind = 8 ) e0
  real ( kind = 8 ) e00(3)
  real ( kind = 8 ) error_f
  real ( kind = 8 ) error_fc
  real ( kind = 8 ) g
  real ( kind = 8 ) gam1
  real ( kind = 8 ) gamma
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ind
  integer ( kind = 4 ) iop
  real ( kind = 8 ) j
  real ( kind = 8 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_max
  real ( kind = 8 ) qans
  real ( kind = 8 ) r
  real ( kind = 8 ) rexp
  real ( kind = 8 ) rlog
  real ( kind = 8 ), parameter :: rt2pin = 0.398942280401433D+00
  real ( kind = 8 ) rta
  real ( kind = 8 ), parameter :: rtpi = 1.77245385090552D+00
  real ( kind = 8 ) rtx
  real ( kind = 8 ) s
  real ( kind = 8 ) sum1
  real ( kind = 8 ) t
  real ( kind = 8 ) t1
  real ( kind = 8 ) tol
  real ( kind = 8 ) twoa
  real ( kind = 8 ) u
  real ( kind = 8 ) w
  real ( kind = 8 ) wk(20)
  real ( kind = 8 ) x
  real ( kind = 8 ) x0
  real ( kind = 8 ) x00(3)
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  data big(1)/20.0D+00/,big(2)/14.0D+00/,big(3)/10.0D+00/
  data e00(1)/0.25D-03/,e00(2)/0.25D-01/,e00(3)/0.14D+00/
  data x00(1)/31.0D+00/,x00(2)/17.0D+00/,x00(3)/9.7D+00/
  data d0(1)/0.833333333333333D-01/
  data d0(2)/-0.148148148148148D-01/
  data d0(3)/0.115740740740741D-02/,d0(4)/0.352733686067019D-03/
  data d0(5)/-0.178755144032922D-03/,d0(6)/0.391926317852244D-04/
  data d0(7)/-0.218544851067999D-05/,d0(8)/-0.185406221071516D-05/
  data d0(9)/0.829671134095309D-06/,d0(10)/-0.176659527368261D-06/
  data d0(11)/0.670785354340150D-08/,d0(12)/0.102618097842403D-07/
  data d0(13)/-0.438203601845335D-08/
  data d10/-0.185185185185185D-02/,d1(1)/-0.347222222222222D-02/
  data d1(2)/0.264550264550265D-02/,d1(3)/-0.990226337448560D-03/
  data d1(4)/0.205761316872428D-03/,d1(5)/-0.401877572016461D-06/
  data d1(6)/-0.180985503344900D-04/,d1(7)/0.764916091608111D-05/
  data d1(8)/-0.161209008945634D-05/,d1(9)/0.464712780280743D-08/
  data d1(10)/0.137863344691572D-06/,d1(11)/-0.575254560351770D-07/
  data d1(12)/0.119516285997781D-07/
  data d20/0.413359788359788D-02/,d2(1)/-0.268132716049383D-02/
  data d2(2)/0.771604938271605D-03/,d2(3)/0.200938786008230D-05/
  data d2(4)/-0.107366532263652D-03/,d2(5)/0.529234488291201D-04/
  data d2(6)/-0.127606351886187D-04/,d2(7)/0.342357873409614D-07/
  data d2(8)/0.137219573090629D-05/,d2(9)/-0.629899213838006D-06/
  data d2(10)/0.142806142060642D-06/
  data d30/0.649434156378601D-03/,d3(1)/0.229472093621399D-03/
  data d3(2)/-0.469189494395256D-03/,d3(3)/0.267720632062839D-03/
  data d3(4)/-0.756180167188398D-04/,d3(5)/-0.239650511386730D-06/
  data d3(6)/0.110826541153473D-04/,d3(7)/-0.567495282699160D-05/
  data d3(8)/0.142309007324359D-05/
  data d40/-0.861888290916712D-03/,d4(1)/0.784039221720067D-03/
  data d4(2)/-0.299072480303190D-03/,d4(3)/-0.146384525788434D-05/
  data d4(4)/0.664149821546512D-04/,d4(5)/-0.396836504717943D-04/
  data d4(6)/0.113757269706784D-04/
  data d50/-0.336798553366358D-03/,d5(1)/-0.697281375836586D-04/
  data d5(2)/0.277275324495939D-03/,d5(3)/-0.199325705161888D-03/
  data d5(4)/0.679778047793721D-04/
  data d60/0.531307936463992D-03/,d6(1)/-0.592166437353694D-03/
  data d6(2)/0.270878209671804D-03/
  data d70 / 0.344367606892378D-03/

  e = epsilon ( 1.0D+00 )

  if ( a < 0.0D+00 .or. x < 0.0D+00 ) then
    ans = 2.0D+00
    return
  end if

  if ( a == 0.0D+00 .and. x == 0.0D+00 ) then
    ans = 2.0D+00
    return
  end if

  if ( a * x == 0.0D+00 ) then
    if ( x <= a ) then
      ans = 0.0D+00
      qans = 1.0D+00
    else
      ans = 1.0D+00
      qans = 0.0D+00
    end if
    return
  end if

  iop = ind + 1
  if ( iop /= 1 .and. iop /= 2 ) iop = 3
  acc = max ( acc0(iop), e )
  e0 = e00(iop)
  x0 = x00(iop)
!
!  Select the appropriate algorithm.
!
  if ( 1.0D+00 <= a ) then
    go to 10
  end if

  if ( a == 0.5D+00 ) then
    go to 390
  end if

  if ( x < 1.1D+00 ) then
    go to 160
  end if

  t1 = a * log ( x ) - x
  u = a * exp ( t1 )

  if ( u == 0.0D+00 ) then
    ans = 1.0D+00
    qans = 0.0D+00
    return
  end if

  r = u * ( 1.0D+00 + gam1 ( a ) )
  go to 250

   10 continue

  if ( big(iop) <= a ) then
    go to 30
  end if

  if ( x < a .or. x0 <= x ) then
    go to 20
  end if

  twoa = a + a
  m = int ( twoa )

  if ( twoa == real ( m, kind = 8 ) ) then
    i = m / 2
    if ( a == real ( i, kind = 8 ) ) then
      go to 210
    end if
    go to 220
  end if

   20 continue

  t1 = a * log ( x ) - x
  r = exp ( t1 ) / gamma ( a )
  go to 40

   30 continue

  l = x / a

  if ( l == 0.0D+00 ) then
    ans = 0.0D+00
    qans = 1.0D+00
    return
  end if

  s = 0.5D+00 + ( 0.5D+00 - l )
  z = rlog ( l )
  if ( 700.0D+00 / a <= z ) then
    go to 410
  end if

  y = a * z
  rta = sqrt ( a )

  if ( abs ( s ) <= e0 / rta ) then
    go to 330
  end if

  if ( abs ( s ) <= 0.4D+00 ) then
    go to 270
  end if

  t = ( 1.0D+00 / a )**2
  t1 = ((( 0.75D+00 * t - 1.0D+00 ) * t + 3.5D+00 ) &
    * t - 105.0D+00 ) / ( a * 1260.0D+00 )
  t1 = t1 - y
  r = rt2pin * rta * exp ( t1 )

40    continue

  if ( r == 0.0D+00 ) then
    if ( x <= a ) then
      ans = 0.0D+00
      qans = 1.0D+00
    else
      ans = 1.0D+00
      qans = 0.0D+00
    end if
    return
  end if

  if ( x <= max ( a, alog10 ) ) then
    go to 50
  end if

  if ( x < x0 ) then
    go to 250
  end if

  go to 100
!
!  Taylor series for P/R.
!
50    continue

  apn = a + 1.0D+00
  t = x / apn
  wk(1) = t

  n = 20

  do i = 2, 20
    apn = apn + 1.0D+00
    t = t * ( x / apn )
    if ( t <= 1.0D-03 ) then
      n = i
      exit
    end if
    wk(i) = t
  end do

  sum1 = t

  tol = 0.5D+00 * acc

  do

    apn = apn + 1.0D+00
    t = t * ( x / apn )
    sum1 = sum1 + t

    if ( t <= tol ) then
      exit
    end if

  end do

  n_max = n - 1
  do m = 1, n_max
    n = n - 1
    sum1 = sum1 + wk(n)
  end do

  ans = ( r / a ) * ( 1.0D+00 + sum1 )
  qans = 0.5D+00 + ( 0.5D+00 - ans )
  return
!
!  Asymptotic expansion.
!
  100 continue

  amn = a - 1.0D+00
  t = amn / x
  wk(1) = t

  n = 20

  do i = 2, 20
    amn = amn - 1.0D+00
    t = t * ( amn / x )
    if ( abs ( t ) <= 1.0D-03 ) then
      n = i
      exit
    end if
    wk(i) = t
  end do

  sum1 = t

  do

    if ( abs ( t ) <= acc ) then
      exit
    end if

    amn = amn - 1.0D+00
    t = t * ( amn / x )
    sum1 = sum1 + t

  end do

  n_max = n - 1
  do m = 1, n_max
    n = n - 1
    sum1 = sum1 + wk(n)
  end do
  qans = ( r / x ) * ( 1.0D+00 + sum1 )
  ans = 0.5D+00 + ( 0.5D+00 - qans )
  return
!
!  Taylor series for P(A,X)/X**A
!
  160 continue

  an = 3.0D+00
  c = x
  sum1 = x / ( a + 3.0D+00 )
  tol = 3.0D+00 * acc / ( a + 1.0D+00 )

  do

    an = an + 1.0D+00
    c = -c * ( x / an )
    t = c / ( a + an )
    sum1 = sum1 + t

    if ( abs ( t ) <= tol ) then
      exit
    end if

  end do

  j = a * x * ( ( sum1 / 6.0D+00 - 0.5D+00 / &
    ( a +  2.0D+00  ) ) * x + 1.0D+00 &
    / ( a + 1.0D+00 ) )

  z = a * log ( x )
  h = gam1 ( a )
  g = 1.0D+00 + h

  if ( x < 0.25D+00 ) then
    go to 180
  end if

  if ( a < x / 2.59D+00 ) then
    go to 200
  end if

  go to 190

  180 continue

  if ( -0.13394D+00 < z ) then
    go to 200
  end if

  190 continue

  w = exp ( z )
  ans = w * g * ( 0.5D+00 + ( 0.5D+00 - j ))
  qans = 0.5D+00 + ( 0.5D+00 - ans )
  return

200   continue

  l = rexp ( z )
  w = 0.5D+00 + ( 0.5D+00 + l )
  qans = ( w * j - l ) * g - h

  if ( qans < 0.0D+00 ) then
    ans = 1.0D+00
    qans = 0.0D+00
    return
  end if

  ans = 0.5D+00 + ( 0.5D+00 - qans )
  return
!
!  Finite sums for Q when 1 <= A and 2*A is an integer.
!
210   continue

  sum1 = exp ( - x )
  t = sum1
  n = 1
  c = 0.0D+00
  go to 230

220   continue

  rtx = sqrt ( x )
  sum1 = error_fc ( 0, rtx )
  t = exp ( -x ) / ( rtpi * rtx )
  n = 0
  c = -0.5D+00

  230 continue

  do while ( n /= i )
    n = n + 1
    c = c + 1.0D+00
    t = ( x * t ) / c
    sum1 = sum1 + t
  end do

  240 continue

  qans = sum1
  ans = 0.5D+00 + ( 0.5D+00 - qans )
  return
!
!  Continued fraction expansion.
!
250   continue

  tol = max ( 5.0D+00 * e, acc )
  a2nm1 = 1.0D+00
  a2n = 1.0D+00
  b2nm1 = x
  b2n = x + ( 1.0D+00 - a )
  c = 1.0D+00

  do

    a2nm1 = x * a2n + c * a2nm1
    b2nm1 = x * b2n + c * b2nm1
    am0 = a2nm1 / b2nm1
    c = c + 1.0D+00
    cma = c - a
    a2n = a2nm1 + cma * a2n
    b2n = b2nm1 + cma * b2n
    an0 = a2n / b2n

    if ( abs ( an0 - am0 ) < tol * an0 ) then
      exit
    end if

  end do

  qans = r * an0
  ans = 0.5D+00 + ( 0.5D+00 - qans )
  return
!
!  General Temme expansion.
!
270   continue

  if ( abs ( s ) <= 2.0D+00 * e .and. 3.28D-03 < a * e * e ) then
    ans =  2.0D+00
    return
  end if

  c = exp ( - y )
  w = 0.5D+00 * error_fc ( 1, sqrt ( y ) )
  u = 1.0D+00 / a
  z = sqrt ( z + z )

  if ( l < 1.0D+00 ) then
    z = -z
  end if

  if ( iop < 2 ) then

    if ( abs ( s ) <= 1.0D-03 ) then

      c0 = ((((((     &
              d0(7)   &
        * z + d0(6) ) &
        * z + d0(5) ) &
        * z + d0(4) ) &
        * z + d0(3) ) &
        * z + d0(2) ) &
        * z + d0(1) ) &
        * z - 1.0D+00 / 3.0D+00

      c1 = (((((      &
              d1(6)   &
        * z + d1(5) ) &
        * z + d1(4) ) &
        * z + d1(3) ) &
        * z + d1(2) ) &
        * z + d1(1) ) &
        * z + d10

      c2 = ((((d2(5)*z+d2(4))*z+d2(3))*z+d2(2))*z+d2(1))*z + d20

      c3 = (((d3(4)*z+d3(3))*z+d3(2))*z+d3(1))*z + d30

      c4 = ( d4(2) * z + d4(1) ) * z + d40
      c5 = ( d5(2) * z + d5(1) ) * z + d50
      c6 = d6(1) * z + d60

      t = (((((( d70 &
        * u + c6 ) &
        * u + c5 ) &
        * u + c4 ) &
        * u + c3 ) &
        * u + c2 ) &
        * u + c1 ) &
        * u + c0

    else

      c0 = (((((((((((( &
              d0(13)   &
        * z + d0(12) ) &
        * z + d0(11) ) &
        * z + d0(10) ) &
        * z + d0(9)  ) &
        * z + d0(8)  ) &
        * z + d0(7)  ) &
        * z + d0(6)  ) &
        * z + d0(5)  ) &
        * z + d0(4)  ) &
        * z + d0(3)  ) &
        * z + d0(2)  ) &
        * z + d0(1)  ) &
        * z - 1.0D+00 / 3.0D+00

      c1 = ((((((((((( &
                d1(12) &
          * z + d1(11) &
        ) * z + d1(10) &
        ) * z + d1(9)  &
        ) * z + d1(8)  &
        ) * z + d1(7)  &
        ) * z + d1(6)  &
        ) * z + d1(5)  &
        ) * z + d1(4)  &
        ) * z + d1(3)  &
        ) * z + d1(2)  &
        ) * z + d1(1)  &
        ) * z + d10

      c2 = ((((((((( &
                d2(10) &
          * z + d2(9) &
        ) * z + d2(8) &
        ) * z + d2(7) &
        ) * z + d2(6) &
        ) * z + d2(5) &
        ) * z + d2(4) &
        ) * z + d2(3) &
        ) * z + d2(2) &
        ) * z + d2(1) &
        ) * z + d20

      c3 = ((((((( &
                d3(8) &
          * z + d3(7) &
        ) * z + d3(6) &
        ) * z + d3(5) &
        ) * z + d3(4) &
        ) * z + d3(3) &
        ) * z + d3(2) &
        ) * z + d3(1) &
        ) * z + d30

      c4 = ((((( d4(6)*z+d4(5))*z+d4(4))*z+d4(3))*z+d4(2))*z+d4(1))*z + d40

      c5 = (((d5(4)*z+d5(3))*z+d5(2))*z+d5(1))*z + d50

      c6 = ( d6(2) * z + d6(1) ) * z + d60

      t = ((((((   &
            d70    &
        * u + c6 ) &
        * u + c5 ) &
        * u + c4 ) &
        * u + c3 ) &
        * u + c2 ) &
        * u + c1 ) &
        * u + c0

    end if

  else if ( iop == 2 ) then

    c0 = (((((      &
            d0(6)   &
      * z + d0(5) ) &
      * z + d0(4) ) &
      * z + d0(3) ) &
      * z + d0(2) ) &
      * z + d0(1) ) &
      * z - 1.0D+00 / 3.0D+00

    c1 = ((( d1(4) * z + d1(3) ) * z + d1(2) ) * z + d1(1) ) * z + d10
    c2 = d2(1) * z + d20
    t = ( c2 * u + c1 ) * u + c0

  else if ( 2 < iop ) then

    t = (( d0(3) * z + d0(2) ) * z + d0(1) ) * z - 1.0D+00 / 3.0D+00

  end if

310   continue

  if ( 1.0D+00 <= l ) then
    qans = c * ( w + rt2pin * t / rta )
    ans = 0.5D+00 + ( 0.5D+00 - qans )
  else
    ans = c * ( w - rt2pin * t / rta )
    qans = 0.5D+00 + ( 0.5D+00 - ans )
  end if

  return
!
!  Temme expansion for L = 1
!
  330 continue

  if ( 3.28D-03 < a * e * e ) then
    ans =  2.0D+00
    return
  end if

  c = 0.5D+00 + ( 0.5D+00 - y )
  w = ( 0.5D+00 - sqrt ( y ) &
    * ( 0.5D+00 &
    + ( 0.5D+00 - y / 3.0D+00 ) ) / rtpi ) / c
  u = 1.0D+00 / a
  z = sqrt ( z + z )

  if ( l < 1.0D+00 ) then
    z = -z
  end if

  if ( iop < 2 ) then

    c0 = ((((((     &
            d0(7)   &
      * z + d0(6) ) &
      * z + d0(5) ) &
      * z + d0(4) ) &
      * z + d0(3) ) &
      * z + d0(2) ) &
      * z + d0(1) ) &
      * z - 1.0D+00 / 3.0D+00

    c1 = (((((      &
            d1(6)   &
      * z + d1(5) ) &
      * z + d1(4) ) &
      * z + d1(3) ) &
      * z + d1(2) ) &
      * z + d1(1) ) &
      * z + d10

    c2 = ((((d2(5)*z+d2(4))*z+d2(3))*z+d2(2))*z+d2(1))*z + d20

    c3 = (((d3(4)*z+d3(3))*z+d3(2))*z+d3(1))*z + d30

    c4 = ( d4(2) * z + d4(1) ) * z + d40
    c5 = ( d5(2) * z + d5(1) ) * z + d50
    c6 = d6(1) * z + d60

    t = (((((( d70 &
      * u + c6 ) &
      * u + c5 ) &
      * u + c4 ) &
      * u + c3 ) &
      * u + c2 ) &
      * u + c1 ) &
      * u + c0

  else if ( iop == 2 ) then

    c0 = ( d0(2) * z + d0(1) ) * z - 1.0D+00 / 3.0D+00
    c1 = d1(1) * z + d10
    t = ( d20 * u + c1 ) * u + c0

  else if ( 2 < iop ) then

    t = d0(1) * z - 1.0D+00 / 3.0D+00

  end if

  go to 310
!
!  Special cases
!
  390 continue

  if ( x < 0.25D+00 ) then
    ans = error_f ( sqrt ( x ) )
    qans = 0.5D+00 + ( 0.5D+00 - ans )
  else
    qans = error_fc ( 0, sqrt ( x ) )
    ans = 0.5D+00 + ( 0.5D+00 - qans )
  end if

  return

  410 continue

  if ( abs ( s ) <= 2.0D+00 * e ) then
    ans =  2.0D+00
    return
  end if

  if ( x <= a ) then
    ans = 0.0D+00
    qans = 1.0D+00
  else
    ans = 1.0D+00
    qans = 0.0D+00
  end if

  return
end
subroutine gamma_inc_inv ( a, x, x0, p, q, ierr )

!*****************************************************************************80
!
!! GAMMA_INC_INV computes the inverse incomplete gamma ratio function.
!
!  Discussion:
!
!    The routine is given positive A, and nonnegative P and Q where P + Q = 1.
!    The value X is computed with the property that P(A,X) = P and Q(A,X) = Q.
!    Schroder iteration is employed.  The routine attempts to compute X
!    to 10 significant digits if this is possible for the particular computer
!    arithmetic being used.
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter in the incomplete gamma
!    ratio.  A must be positive.
!
!    Output, real ( kind = 8 ) X, the computed point for which the
!    incomplete gamma functions have the values P and Q.
!
!    Input, real ( kind = 8 ) X0, an optional initial approximation
!    for the solution X.  If the user does not want to supply an
!    initial approximation, then X0 should be set to 0, or a negative
!    value.
!
!    Input, real ( kind = 8 ) P, Q, the values of the incomplete gamma
!    functions, for which the corresponding argument is desired.
!
!    Output, integer ( kind = 4 ) IERR, error flag.
!    0, the solution was obtained. Iteration was not used.
!    0 < K, The solution was obtained. IERR iterations were performed.
!    -2, A <= 0
!    -3, No solution was obtained. The ratio Q/A is too large.
!    -4, P + Q /= 1
!    -6, 20 iterations were performed. The most recent value obtained
!        for X is given.  This cannot occur if X0 <= 0.
!    -7, Iteration failed. No value is given for X.
!        This may occur when X is approximately 0.
!    -8, A value for X has been obtained, but the routine is not certain
!        of its accuracy.  Iteration cannot be performed in this
!        case. If X0 <= 0, this can occur only when P or Q is
!        approximately 0. If X0 is positive then this can occur when A is
!        exceedingly close to X and A is extremely large (say A .GE. 1.E20).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: a0 = 3.31125922108741D+00
  real ( kind = 8 ), parameter :: a1 = 11.6616720288968D+00
  real ( kind = 8 ), parameter :: a2 = 4.28342155967104D+00
  real ( kind = 8 ), parameter :: a3 = 0.213623493715853D+00
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) am1
  real ( kind = 8 ) amax
  real ( kind = 8 ), dimension(2) :: amin = (/ &
    500.0D+00, 100.0D+00 /)
  real ( kind = 8 ) ap1
  real ( kind = 8 ) ap2
  real ( kind = 8 ) ap3
  real ( kind = 8 ) apn
  real ( kind = 8 ) b
  real ( kind = 8 ), parameter :: b1 = 6.61053765625462D+00
  real ( kind = 8 ), parameter :: b2 = 6.40691597760039D+00
  real ( kind = 8 ), parameter :: b3 = 1.27364489782223D+00
  real ( kind = 8 ), parameter :: b4 = .036117081018842D+00
  real ( kind = 8 ), dimension ( 2 ) :: bmin = (/ &
    1.0D-28, 1.0D-13 /)
  real ( kind = 8 ), parameter :: c = 0.577215664901533D+00
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) c3
  real ( kind = 8 ) c4
  real ( kind = 8 ) c5
  real ( kind = 8 ) d
  real ( kind = 8 ), dimension ( 2 ) :: dmin = (/ &
    1.0D-06, 1.0D-04 /)
  real ( kind = 8 ) e
  real ( kind = 8 ) e2
  real ( kind = 8 ), dimension ( 2 ) :: emin = (/ &
    2.0D-03, 6.0D-03 /)
  real ( kind = 8 ) eps
  real ( kind = 8 ), dimension ( 2 ) :: eps0 = (/ &
    1.0D-10, 1.0D-08 /)
  real ( kind = 8 ) g
  real ( kind = 8 ) gamma_log
  real ( kind = 8 ) gamma_ln1
  real ( kind = 8 ) gamma
  real ( kind = 8 ) h
  real ( kind = 8 ), parameter :: half = 0.5D+00
  integer ( kind = 4 ) ierr
  integer ( kind = 4 ) iop
  real ( kind = 8 ), parameter :: ln10 = 2.302585D+00
  real ( kind = 8 ) p
  real ( kind = 8 ) pn
  real ( kind = 8 ) q
  real ( kind = 8 ) qg
  real ( kind = 8 ) qn
  real ( kind = 8 ) r
  real ( kind = 8 ) rcomp
  real ( kind = 8 ) rta
  real ( kind = 8 ) s
  real ( kind = 8 ) s2
  real ( kind = 8 ) sum1
  real ( kind = 8 ) t
  real ( kind = 8 ), parameter :: tol = 1.0D-05
  real ( kind = 8 ), parameter :: two =  2.0D+00
  real ( kind = 8 ) u
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) x0
  real ( kind = 8 ) xn
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  e = epsilon ( e )

  x = 0.0D+00

  if ( a <= 0.0D+00 ) then
    ierr = -2
    return
  end if

  t = p + q - 1.0D+00

  if ( e < abs ( t ) ) then
    ierr = -4
    return
  end if

  ierr = 0

  if ( p == 0.0D+00 ) then
    return
  end if

  if ( q == 0.0D+00 ) then
    x = huge ( x )
    return
  end if

  if ( a == 1.0D+00 ) then
    if ( 0.9D+00 <= q ) then
      x = -alnrel ( - p )
    else
      x = -log ( q )
    end if
    return
  end if

  e2 = two * e
  amax = 0.4D-10 / ( e * e )

  if ( 1.0D-10 < e ) then
    iop = 2
  else
    iop = 1
  end if

  eps = eps0(iop)
  xn = x0

  if ( 0.0D+00 < x0 ) then
    go to 160
  end if
!
!  Selection of the initial approximation XN of X when A < 1.
!
  if ( 1.0D+00 < a ) then
    go to 80
  end if

  g = gamma ( a + 1.0D+00 )
  qg = q * g

  if ( qg == 0.0D+00 ) then
    x = huge ( x )
    ierr = -8
    return
  end if

  b = qg / a

  if ( 0.6D+00 * a < qg ) then
    go to 40
  end if

  if ( a < 0.30D+00 .and. 0.35D+00 <= b ) then
    t = exp ( - ( b + c ) )
    u = t * exp ( t )
    xn = t * exp ( u )
    go to 160
  end if

  if ( 0.45D+00 <= b ) then
    go to 40
  end if

  if ( b == 0.0D+00 ) then
    x = huge ( x )
    ierr = -8
    return
  end if

  y = -log ( b )
  s = half + ( half - a )
  z = log ( y )
  t = y - s * z

  if ( 0.15D+00 <= b ) then
    xn = y - s * log ( t ) - log ( 1.0D+00 + s / ( t + 1.0D+00 ) )
    go to 220
  end if

  if ( 0.01D+00 < b ) then
    u = ( ( t + two * ( 3.0D+00 - a ) ) * t &
      + ( two - a ) * ( 3.0D+00 - a )) / &
      ( ( t + ( 5.0D+00 - a ) ) * t + two )
    xn = y - s * log ( t ) - log ( u )
    go to 220
  end if

30 continue

  c1 = -s * z
  c2 = -s * ( 1.0D+00 + c1 )

  c3 = s * (( half * c1 &
    + ( two - a ) ) * c1 + ( 2.5D+00 - 1.5D+00 * a ) )

  c4 = -s * ((( c1 / 3.0D+00 + ( 2.5D+00 - 1.5D+00 * a ) ) * c1 &
    + ( ( a - 6.0D+00 ) * a + 7.0D+00 ) ) &
    * c1 + ( ( 11.0D+00 * a - 46.0D+00 ) * a + 47.0D+00 ) / 6.0D+00 )

  c5 = -s * (((( - c1 / 4.0D+00 + ( 11.0D+00 * a - 17.0D+00 ) / 6.0D+00 ) * c1 &
     + ( ( -3.0D+00 * a + 13.0D+00 ) * a - 13.0D+00 ) ) * c1 &
     + half &
     * ( ( ( two * a - 25.0D+00 ) * a + 72.0D+00 ) &
     * a - 61.0D+00 ) ) * c1 &
     + ( ( ( 25.0D+00 * a - 195.0D+00 ) * a &
     + 477.0D+00 ) * a - 379.0D+00 ) / 12.0D+00 )

  xn = (((( c5 / y + c4 ) / y + c3 ) / y + c2 ) / y + c1 ) + y

  if ( 1.0D+00 < a ) then
    go to 220
  end if

  if ( bmin(iop) < b ) then
    go to 220
  end if

  x = xn
  return

   40 continue

  if ( b * q <= 1.0D-08 ) then
    xn = exp ( - ( q / a + c ))
  else if ( 0.9D+00 < p ) then
    xn = exp ( ( alnrel ( - q ) + gamma_ln1 ( a )) / a )
  else
    xn = exp ( log ( p * g ) / a )
  end if

  if ( xn == 0.0D+00 ) then
    ierr = -3
    return
  end if

  t = half + ( half - xn / ( a + 1.0D+00 ))
  xn = xn / t
  go to 160
!
!  Selection of the initial approximation XN of X when 1 < A.
!
   80 continue

  if ( 0.5D+00 < q ) then
    w = log ( p )
  else
    w = log ( q )
  end if

  t = sqrt ( - two * w )

  s = t - ((( a3 * t + a2 ) * t + a1 ) * t + a0 ) / (((( &
    b4 * t + b3 ) * t + b2 ) * t + b1 ) * t + 1.0D+00 )

  if ( 0.5D+00 < q ) then
    s = -s
  end if

  rta = sqrt ( a )
  s2 = s * s

  xn = a + s * rta + ( s2 - 1.0D+00 ) / 3.0D+00 + s * ( s2 - 7.0D+00 ) &
    / ( 36.0D+00 * rta ) - ( ( 3.0D+00 * s2 + 7.0D+00 ) * s2 - 16.0D+00 ) &
    / ( 810.0D+00 * a ) + s * (( 9.0D+00 * s2 + 256.0D+00 ) * s2 - 433.0D+00 ) &
    / ( 38880.0D+00 * a * rta )

  xn = max ( xn, 0.0D+00 )

  if ( amin(iop) <= a ) then

    x = xn
    d = half + ( half - x / a )

    if ( abs ( d ) <= dmin(iop) ) then
      return
    end if

  end if

  110 continue

  if ( p <= 0.5D+00 ) then
    go to 130
  end if

  if ( xn < 3.0D+00 * a ) then
    go to 220
  end if

  y = - ( w + gamma_log ( a ) )
  d = max ( two, a * ( a - 1.0D+00 ) )

  if ( ln10 * d <= y ) then
    s = 1.0D+00 - a
    z = log ( y )
    go to 30
  end if

  120 continue

  t = a - 1.0D+00
  xn = y + t * log ( xn ) - alnrel ( -t / ( xn + 1.0D+00 ) )
  xn = y + t * log ( xn ) - alnrel ( -t / ( xn + 1.0D+00 ) )
  go to 220

  130 continue

  ap1 = a + 1.0D+00

  if ( 0.70D+00 * ap1 < xn ) then
    go to 170
  end if

  w = w + gamma_log ( ap1 )

  if ( xn <= 0.15 * ap1 ) then
    ap2 = a + two
    ap3 = a + 3.0D+00
    x = exp ( ( w + x ) / a )
    x = exp ( ( w + x - log ( 1.0D+00 + ( x / ap1 ) &
      * ( 1.0D+00 + x / ap2 ) ) ) / a )
    x = exp ( ( w + x - log ( 1.0D+00 + ( x / ap1 ) &
      * ( 1.0D+00 + x / ap2 ) ) ) / a )
    x = exp ( ( w + x - log ( 1.0D+00 + ( x / ap1 ) &
      * ( 1.0D+00 + ( x / ap2 ) &
      * ( 1.0D+00 + x / ap3 ) ) ) ) / a )
    xn = x

    if ( xn <= 1.0D-02 * ap1 ) then
      if ( xn <= emin(iop) * ap1 ) then
        return
      end if
      go to 170
    end if

  end if

  apn = ap1
  t = xn / apn
  sum1 = 1.0D+00 + t

  do

    apn = apn + 1.0D+00
    t = t * ( xn / apn )
    sum1 = sum1 + t

    if ( t <= 1.0D-04 ) then
      exit
    end if

  end do

  t = w - log ( sum1 )
  xn = exp ( ( xn + t ) / a )
  xn = xn * ( 1.0D+00 - ( a * log ( xn ) - xn - t ) / ( a - xn ) )
  go to 170
!
!  Schroder iteration using P.
!
  160 continue

  if ( 0.5D+00 < p ) then
    go to 220
  end if

  170 continue

  if ( p <= 1.0D+10 * tiny ( p ) ) then
    x = xn
    ierr = -8
    return
  end if

  am1 = ( a - half ) - half

  180 continue

  if ( amax < a ) then
    d = half + ( half - xn / a )
    if ( abs ( d ) <= e2 ) then
      x = xn
      ierr = -8
      return
    end if
  end if

  190 continue

  if ( 20 <= ierr ) then
    ierr = -6
    return
  end if

  ierr = ierr + 1
  call gamma_inc ( a, xn, pn, qn, 0 )

  if ( pn == 0.0D+00 .or. qn == 0.0D+00 ) then
    x = xn
    ierr = -8
    return
  end if

  r = rcomp ( a, xn )

  if ( r == 0.0D+00 ) then
    x = xn
    ierr = -8
    return
  end if

  t = ( pn - p ) / r
  w = half * ( am1 - xn )

  if ( abs ( t ) <= 0.1D+00 .and. abs ( w * t ) <= 0.1D+00 ) then
    go to 200
  end if

  x = xn * ( 1.0D+00 - t )

  if ( x <= 0.0D+00 ) then
    ierr = -7
    return
  end if

  d = abs ( t )
  go to 210

  200 continue

  h = t * ( 1.0D+00 + w * t )
  x = xn * ( 1.0D+00 - h )

  if ( x <= 0.0D+00 ) then
    ierr = -7
    return
  end if

  if ( 1.0D+00 <= abs ( w ) .and. abs ( w ) * t * t <= eps ) then
    return
  end if

  d = abs ( h )

  210 continue

  xn = x

  if ( d <= tol ) then

    if ( d <= eps ) then
      return
    end if

    if ( abs ( p - pn ) <= tol * p ) then
      return
    end if

  end if

  go to 180
!
!  Schroder iteration using Q.
!
  220 continue

  if ( q <= 1.0D+10 * tiny ( q ) ) then
    x = xn
    ierr = -8
    return
  end if

  am1 = ( a - half ) - half

  230 continue

  if ( amax < a ) then
    d = half + ( half - xn / a )
    if ( abs ( d ) <= e2 ) then
      x = xn
      ierr = -8
      return
    end if
  end if

  if ( 20 <= ierr ) then
    ierr = -6
    return
  end if

  ierr = ierr + 1
  call gamma_inc ( a, xn, pn, qn, 0 )

  if ( pn == 0.0D+00 .or. qn == 0.0D+00 ) then
    x = xn
    ierr = -8
    return
  end if

  r = rcomp ( a, xn )

  if ( r == 0.0D+00 ) then
    x = xn
    ierr = -8
    return
  end if

  t = ( q - qn ) / r
  w = half * ( am1 - xn )

  if ( abs ( t ) <= 0.1 .and. abs ( w * t ) <= 0.1 ) then
    go to 250
  end if

  x = xn * ( 1.0D+00 - t )

  if ( x <= 0.0D+00 ) then
    ierr = -7
    return
  end if

  d = abs ( t )
  go to 260

  250 continue

  h = t * ( 1.0D+00 + w * t )
  x = xn * ( 1.0D+00 - h )

  if ( x <= 0.0D+00 ) then
    ierr = -7
    return
  end if

  if ( 1.0D+00 <= abs ( w ) .and. abs ( w ) * t * t <= eps ) then
    return
  end if

  d = abs ( h )

  260 continue

  xn = x

  if ( tol < d ) then
    go to 230
  end if

  if ( d <= eps ) then
    return
  end if

  if ( abs ( q - qn ) <= tol * q ) then
    return
  end if

  go to 230
end
subroutine gamma_inc_values ( n_data, a, x, fx )

!*****************************************************************************80
!
!! GAMMA_INC_VALUES returns some values of the incomplete Gamma function.
!
!  Discussion:
!
!    The (normalized) incomplete Gamma function P(A,X) is defined as:
!
!      PN(A,X) = 1/GAMMA(A) * Integral ( 0 <= T <= X ) T**(A-1) * exp(-T) dT.
!
!    With this definition, for all A and X,
!
!      0 <= PN(A,X) <= 1
!
!    and
!
!      PN(A,INFINITY) = 1.0
!
!    Mathematica can compute this value as
!
!      1 - GammaRegularized[A,X]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, X, the arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
    0.1D+00,  0.1D+00,  0.1D+00,  0.5D+00, &
    0.5D+00,  0.5D+00,  1.0D+00,  1.0D+00, &
    1.0D+00,  1.1D+00,  1.1D+00,  1.1D+00, &
    2.0D+00,  2.0D+00,  2.0D+00,  6.0D+00, &
    6.0D+00, 11.0D+00, 26.0D+00, 41.0D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.7420263D+00, 0.9119753D+00, 0.9898955D+00, 0.2931279D+00, &
    0.7656418D+00, 0.9921661D+00, 0.0951626D+00, 0.6321206D+00, &
    0.9932621D+00, 0.0757471D+00, 0.6076457D+00, 0.9933425D+00, &
    0.0091054D+00, 0.4130643D+00, 0.9931450D+00, 0.0387318D+00, &
    0.9825937D+00, 0.9404267D+00, 0.4863866D+00, 0.7359709D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    3.1622777D-02, 3.1622777D-01, 1.5811388D+00, 7.0710678D-02, &
    7.0710678D-01, 3.5355339D+00, 0.1000000D+00, 1.0000000D+00, &
    5.0000000D+00, 1.0488088D-01, 1.0488088D+00, 5.2440442D+00, &
    1.4142136D-01, 1.4142136D+00, 7.0710678D+00, 2.4494897D+00, &
    1.2247449D+01, 1.6583124D+01, 2.5495098D+01, 4.4821870D+01 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
function gamma_ln1 ( a )

!*****************************************************************************80
!
!! GAMMA_LN1 evaluates ln ( Gamma ( 1 + A ) ), for -0.2 <= A <= 1.25.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, defines the argument of the function.
!
!    Output, real ( kind = 8 ) GAMMA_LN1, the value of ln ( Gamma ( 1 + A ) ).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) bot
  real ( kind = 8 ) gamma_ln1
  real ( kind = 8 ), parameter :: p0 =  0.577215664901533D+00
  real ( kind = 8 ), parameter :: p1 =  0.844203922187225D+00
  real ( kind = 8 ), parameter :: p2 = -0.168860593646662D+00
  real ( kind = 8 ), parameter :: p3 = -0.780427615533591D+00
  real ( kind = 8 ), parameter :: p4 = -0.402055799310489D+00
  real ( kind = 8 ), parameter :: p5 = -0.673562214325671D-01
  real ( kind = 8 ), parameter :: p6 = -0.271935708322958D-02
  real ( kind = 8 ), parameter :: q1 =  0.288743195473681D+01
  real ( kind = 8 ), parameter :: q2 =  0.312755088914843D+01
  real ( kind = 8 ), parameter :: q3 =  0.156875193295039D+01
  real ( kind = 8 ), parameter :: q4 =  0.361951990101499D+00
  real ( kind = 8 ), parameter :: q5 =  0.325038868253937D-01
  real ( kind = 8 ), parameter :: q6 =  0.667465618796164D-03
  real ( kind = 8 ), parameter :: r0 = 0.422784335098467D+00
  real ( kind = 8 ), parameter :: r1 = 0.848044614534529D+00
  real ( kind = 8 ), parameter :: r2 = 0.565221050691933D+00
  real ( kind = 8 ), parameter :: r3 = 0.156513060486551D+00
  real ( kind = 8 ), parameter :: r4 = 0.170502484022650D-01
  real ( kind = 8 ), parameter :: r5 = 0.497958207639485D-03
  real ( kind = 8 ), parameter :: s1 = 0.124313399877507D+01
  real ( kind = 8 ), parameter :: s2 = 0.548042109832463D+00
  real ( kind = 8 ), parameter :: s3 = 0.101552187439830D+00
  real ( kind = 8 ), parameter :: s4 = 0.713309612391000D-02
  real ( kind = 8 ), parameter :: s5 = 0.116165475989616D-03
  real ( kind = 8 ) top
  real ( kind = 8 ) x

  if ( a < 0.6D+00 ) then

    top = (((((  &
            p6   &
      * a + p5 ) &
      * a + p4 ) &
      * a + p3 ) &
      * a + p2 ) &
      * a + p1 ) &
      * a + p0

    bot = (((((  &
            q6   &
      * a + q5 ) &
      * a + q4 ) &
      * a + q3 ) &
      * a + q2 ) &
      * a + q1 ) &
      * a + 1.0D+00

    gamma_ln1 = -a * ( top / bot )

  else

    x = ( a - 0.5D+00 ) - 0.5D+00

    top = ((((( r5 * x + r4 ) * x + r3 ) * x + r2 ) * x + r1 ) * x + r0 )

    bot = ((((( s5 * x + s4 ) * x + s3 ) * x + s2 ) * x + s1 ) * x + 1.0D+00 )

    gamma_ln1 = x * ( top / bot )

  end if

  return
end
function gamma_log ( a )

!*****************************************************************************80
!
!! GAMMA_LOG evaluates ln ( Gamma ( A ) ) for positive A.
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the argument of the function.
!    A should be positive.
!
!    Output, real ( kind = 8 ), GAMMA_LOG, the value of ln ( Gamma ( A ) ).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: c0 =  0.833333333333333D-01
  real ( kind = 8 ), parameter :: c1 = -0.277777777760991D-02
  real ( kind = 8 ), parameter :: c2 =  0.793650666825390D-03
  real ( kind = 8 ), parameter :: c3 = -0.595202931351870D-03
  real ( kind = 8 ), parameter :: c4 =  0.837308034031215D-03
  real ( kind = 8 ), parameter :: c5 = -0.165322962780713D-02
  real ( kind = 8 ), parameter :: d  =  0.418938533204673D+00
  real ( kind = 8 ) gamma_log
  real ( kind = 8 ) gamma_ln1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) t
  real ( kind = 8 ) w

  if ( a <= 0.8D+00 ) then

    gamma_log = gamma_ln1 ( a ) - log ( a )

  else if ( a <= 2.25D+00 ) then

    t = ( a - 0.5D+00 ) - 0.5D+00
    gamma_log = gamma_ln1 ( t )

  else if ( a < 10.0D+00 ) then

    n = a - 1.25D+00
    t = a
    w = 1.0D+00
    do i = 1, n
      t = t - 1.0D+00
      w = t * w
    end do

    gamma_log = gamma_ln1 ( t - 1.0D+00 ) + log ( w )

  else

    t = ( 1.0D+00 / a )**2

    w = ((((( c5 * t + c4 ) * t + c3 ) * t + c2 ) * t + c1 ) * t + c0 ) / a

    gamma_log = ( d + w ) + ( a - 0.5D+00 ) &
      * ( log ( a ) - 1.0D+00 )

  end if

  return
end
subroutine gamma_rat1 ( a, x, r, p, q, eps )

!*****************************************************************************80
!
!! GAMMA_RAT1 evaluates the incomplete gamma ratio functions P(A,X) and Q(A,X).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, X, the parameters of the functions.
!    It is assumed that A <= 1.
!
!    Input, real ( kind = 8 ) R, the value exp(-X) * X**A / Gamma(A).
!
!    Output, real ( kind = 8 ) P, Q, the values of P(A,X) and Q(A,X).
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2n
  real ( kind = 8 ) a2nm1
  real ( kind = 8 ) am0
  real ( kind = 8 ) an
  real ( kind = 8 ) an0
  real ( kind = 8 ) b2n
  real ( kind = 8 ) b2nm1
  real ( kind = 8 ) c
  real ( kind = 8 ) cma
  real ( kind = 8 ) eps
  real ( kind = 8 ) error_f
  real ( kind = 8 ) error_fc
  real ( kind = 8 ) g
  real ( kind = 8 ) gam1
  real ( kind = 8 ) h
  real ( kind = 8 ) j
  real ( kind = 8 ) l
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) r
  real ( kind = 8 ) rexp
  real ( kind = 8 ) sum1
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) z

  if ( a * x == 0.0D+00 ) then

    if ( x <= a ) then
      p = 0.0D+00
      q = 1.0D+00
    else
      p = 1.0D+00
      q = 0.0D+00
    end if

    return
  end if

  if ( a == 0.5D+00 ) then

    if ( x < 0.25D+00 ) then
      p = error_f ( sqrt ( x ) )
      q = 0.5D+00 + ( 0.5D+00 - p )
    else
      q = error_fc ( 0, sqrt ( x ) )
      p = 0.5D+00 + ( 0.5D+00 - q )
    end if

    return

  end if
!
!  Taylor series for P(A,X)/X**A
!
  if ( x < 1.1D+00 ) then

    an = 3.0
    c = x
    sum1 = x / ( a + 3.0D+00 )
    tol = 0.1D+00 * eps / ( a + 1.0D+00 )

    do

      an = an + 1.0D+00
      c = -c * ( x / an )
      t = c / ( a + an )
      sum1 = sum1 + t

      if ( abs ( t ) <= tol ) then
        exit
      end if

    end do

    j = a * x * ( ( sum1 / 6.0D+00 - 0.5D+00 &
      / ( a +  2.0D+00  ) ) &
      * x + 1.0D+00 / ( a + 1.0D+00 ) )

    z = a * log ( x )
    h = gam1 ( a )
    g = 1.0D+00 + h

    if ( x < 0.25D+00 ) then
      go to 30
    end if

    if ( a < x / 2.59D+00 ) then
      go to 50
    else
      go to 40
    end if

30 continue

    if ( -0.13394D+00 < z ) then
      go to 50
    end if

40 continue

    w = exp ( z )
    p = w * g * ( 0.5D+00 + ( 0.5D+00 - j ))
    q = 0.5D+00 + ( 0.5D+00 - p )
    return

50 continue

    l = rexp ( z )
    w = 0.5D+00 + ( 0.5D+00 + l )
    q = ( w * j - l ) * g - h

    if  ( q < 0.0D+00 ) then
      p = 1.0D+00
      q = 0.0D+00
    else
      p = 0.5D+00 + ( 0.5D+00 - q )
    end if
!
!  Continued fraction expansion.
!
  else

    a2nm1 = 1.0D+00
    a2n = 1.0D+00
    b2nm1 = x
    b2n = x + ( 1.0D+00 - a )
    c = 1.0D+00

    do

      a2nm1 = x * a2n + c * a2nm1
      b2nm1 = x * b2n + c * b2nm1
      am0 = a2nm1 / b2nm1
      c = c + 1.0D+00
      cma = c - a
      a2n = a2nm1 + cma * a2n
      b2n = b2nm1 + cma * b2n
      an0 = a2n / b2n

      if ( abs ( an0 - am0 ) < eps * an0 ) then
        exit
      end if

    end do

    q = r * an0
    p = 0.5D+00 + ( 0.5D+00 - q )

  end if

  return
end
subroutine gamma_values ( n_data, x, fx )

!*****************************************************************************80
!
!! GAMMA_VALUES returns some values of the Gamma function.
!
!  Definition:
!
!    Gamma(Z) = Integral ( 0 <= T < Infinity) T**(Z-1) exp(-T) dT
!
!  Recursion:
!
!    Gamma(X+1) = X * Gamma(X)
!
!  Restrictions:
!
!    0 < X ( a software restriction).
!
!  Special values:
!
!    GAMMA(0.5) = sqrt(PI)
!
!    For N a positive integer, GAMMA(N+1) = N!, the standard factorial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 18

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    4.590845D+00,     2.218160D+00,     1.489192D+00,     1.164230D+00, &
    1.0000000000D+00, 0.9513507699D+00, 0.9181687424D+00, 0.8974706963D+00, &
    0.8872638175D+00, 0.8862269255D+00, 0.8935153493D+00, 0.9086387329D+00, &
    0.9313837710D+00, 0.9617658319D+00, 1.0000000000D+00, 3.6288000D+05, &
    1.2164510D+17,    8.8417620D+30 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.2D+00,  0.4D+00,  0.6D+00,  0.8D+00, &
    1.0D+00,  1.1D+00,  1.2D+00,  1.3D+00, &
    1.4D+00,  1.5D+00,  1.6D+00,  1.7D+00, &
    1.8D+00,  1.9D+00,  2.0D+00, 10.0D+00, &
   20.0D+00, 30.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
function gsumln ( a, b )

!*****************************************************************************80
!
!! GSUMLN evaluates the function ln(Gamma(A + B)).
!
!  Discussion:
!
!    GSUMLN is used for 1 <= A <= 2 and 1 <= B <= 2
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, values whose sum is the argument of
!    the Gamma function.
!
!    Output, real ( kind = 8 ) GSUMLN, the value of ln(Gamma(A+B)).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) b
  real ( kind = 8 ) gamma_ln1
  real ( kind = 8 ) gsumln
  real ( kind = 8 ) x

  x = a + b - 2.0D+00

  if ( x <= 0.25D+00 ) then
    gsumln = gamma_ln1 ( 1.0D+00 + x )
  else if ( x <= 1.25D+00 ) then
    gsumln = gamma_ln1 ( x ) + alnrel ( x )
  else
    gsumln = gamma_ln1 ( x - 1.0D+00 ) + log ( x * ( 1.0D+00 + x ) )
  end if

  return
end
function ipmpar ( i )

!*****************************************************************************80
!
!! IPMPAR returns integer machine constants.
!
!  Discussion:
!
!    Input arguments 1 through 3 are queries about integer arithmetic.
!    We assume integers are represented in the N-digit, base A form
!
!      sign * ( X(N-1)*A^(N-1) + ... + X(1)*A + X(0) )
!
!    where 0 <= X(0:N-1) < A.
!
!    Then:
!
!      IPMPAR(1) = A, the base of integer arithmetic;
!      IPMPAR(2) = N, the number of base A digits;
!      IPMPAR(3) = A^N - 1, the largest magnitude.
!
!    It is assumed that the single and real ( kind = 8 ) floating
!    point arithmetics have the same base, say B, and that the
!    nonzero numbers are represented in the form
!
!      sign * (B^E) * (X(1)/B + ... + X(M)/B^M)
!
!    where X(1:M) is one of { 0, 1,..., B-1 }, and 1 <= X(1) and
!    EMIN <= E <= EMAX.
!
!    Input argument 4 is a query about the base of real arithmetic:
!
!      IPMPAR(4) = B, the base of single and real ( kind = 8 ) arithmetic.
!
!    Input arguments 5 through 7 are queries about single precision
!    floating point arithmetic:
!
!     IPMPAR(5) = M, the number of base B digits for single precision.
!     IPMPAR(6) = EMIN, the smallest exponent E for single precision.
!     IPMPAR(7) = EMAX, the largest exponent E for single precision.
!
!    Input arguments 8 through 10 are queries about real ( kind = 8 )
!    floating point arithmetic:
!
!     IPMPAR(8) = M, the number of base B digits for real ( kind = 8 ).
!     IPMPAR(9) = EMIN, the smallest exponent E for real ( kind = 8 ).
!     IPMPAR(10) = EMAX, the largest exponent E for real ( kind = 8 ).
!
!  Reference:
!
!    Phyllis Fox, Andrew Hall, Norman Schryer,
!    Algorithm 528:
!    Framework for a Portable FORTRAN Subroutine Library,
!    ACM Transactions on Mathematical Software,
!    Volume 4, 1978, pages 176-188.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the index of the desired constant.
!
!    Output, integer ( kind = 4 ) IPMPAR, the value of the desired constant.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) imach(10)
  integer ( kind = 4 ) ipmpar
!
!     MACHINE CONSTANTS FOR AMDAHL MACHINES.
!
!     data imach( 1) /   2 /
!     data imach( 2) /  31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /  16 /
!     data imach( 5) /   6 /
!     data imach( 6) / -64 /
!     data imach( 7) /  63 /
!     data imach( 8) /  14 /
!     data imach( 9) / -64 /
!     data imach(10) /  63 /
!
!     Machine constants for the AT&T 3B SERIES, AT&T
!     PC 7300, AND AT&T 6300.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     Machine constants for the BURROUGHS 1700 SYSTEM.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   33 /
!     data imach( 3) / 8589934591 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -256 /
!     data imach( 7) /  255 /
!     data imach( 8) /   60 /
!     data imach( 9) / -256 /
!     data imach(10) /  255 /
!
!     Machine constants for the BURROUGHS 5700 SYSTEM.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   39 /
!     data imach( 3) / 549755813887 /
!     data imach( 4) /    8 /
!     data imach( 5) /   13 /
!     data imach( 6) /  -50 /
!     data imach( 7) /   76 /
!     data imach( 8) /   26 /
!     data imach( 9) /  -50 /
!     data imach(10) /   76 /
!
!     Machine constants for the BURROUGHS 6700/7700 SYSTEMS.
!
!     data imach( 1) /      2 /
!     data imach( 2) /     39 /
!     data imach( 3) / 549755813887 /
!     data imach( 4) /      8 /
!     data imach( 5) /     13 /
!     data imach( 6) /    -50 /
!     data imach( 7) /     76 /
!     data imach( 8) /     26 /
!     data imach( 9) / -32754 /
!     data imach(10) /  32780 /
!
!     Machine constants for the CDC 6000/7000 SERIES
!     60 BIT ARITHMETIC, AND THE CDC CYBER 995 64 BIT
!     ARITHMETIC (NOS OPERATING SYSTEM).
!
!     data imach( 1) /    2 /
!     data imach( 2) /   48 /
!     data imach( 3) / 281474976710655 /
!     data imach( 4) /    2 /
!     data imach( 5) /   48 /
!     data imach( 6) / -974 /
!     data imach( 7) / 1070 /
!     data imach( 8) /   95 /
!     data imach( 9) / -926 /
!     data imach(10) / 1070 /
!
!     Machine constants for the CDC CYBER 995 64 BIT
!     ARITHMETIC (NOS/VE OPERATING SYSTEM).
!
!     data imach( 1) /     2 /
!     data imach( 2) /    63 /
!     data imach( 3) / 9223372036854775807 /
!     data imach( 4) /     2 /
!     data imach( 5) /    48 /
!     data imach( 6) / -4096 /
!     data imach( 7) /  4095 /
!     data imach( 8) /    96 /
!     data imach( 9) / -4096 /
!     data imach(10) /  4095 /
!
!     Machine constants for the CRAY 1, XMP, 2, AND 3.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    63 /
!     data imach( 3) / 9223372036854775807 /
!     data imach( 4) /     2 /
!     data imach( 5) /    47 /
!     data imach( 6) / -8189 /
!     data imach( 7) /  8190 /
!     data imach( 8) /    94 /
!     data imach( 9) / -8099 /
!     data imach(10) /  8190 /
!
!     Machine constants for the data GENERAL ECLIPSE S/200.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   15 /
!     data imach( 3) / 32767 /
!     data imach( 4) /   16 /
!     data imach( 5) /    6 /
!     data imach( 6) /  -64 /
!     data imach( 7) /   63 /
!     data imach( 8) /   14 /
!     data imach( 9) /  -64 /
!     data imach(10) /   63 /
!
!     Machine constants for the HARRIS 220.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   23 /
!     data imach( 3) / 8388607 /
!     data imach( 4) /    2 /
!     data imach( 5) /   23 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   38 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     Machine constants for the HONEYWELL 600/6000
!     AND DPS 8/70 SERIES.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   35 /
!     data imach( 3) / 34359738367 /
!     data imach( 4) /    2 /
!     data imach( 5) /   27 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   63 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     Machine constants for the HP 2100
!     3 WORD real ( kind = 8 ) OPTION WITH FTN4
!
!     data imach( 1) /    2 /
!     data imach( 2) /   15 /
!     data imach( 3) / 32767 /
!     data imach( 4) /    2 /
!     data imach( 5) /   23 /
!     data imach( 6) / -128 /
!     data imach( 7) /  127 /
!     data imach( 8) /   39 /
!     data imach( 9) / -128 /
!     data imach(10) /  127 /
!
!     Machine constants for the HP 2100
!     4 WORD real ( kind = 8 ) OPTION WITH FTN4
!
!     data imach( 1) /    2 /
!     data imach( 2) /   15 /
!     data imach( 3) / 32767 /
!     data imach( 4) /    2 /
!     data imach( 5) /   23 /
!     data imach( 6) / -128 /
!     data imach( 7) /  127 /
!     data imach( 8) /   55 /
!     data imach( 9) / -128 /
!     data imach(10) /  127 /
!
!     Machine constants for the HP 9000.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -126 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     Machine constants for the IBM 360/370 SERIES,
!     THE ICL 2900, THE ITEL AS/6, THE XEROX SIGMA
!     5/7/9 AND THE SEL SYSTEMS 85/86.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /   16 /
!     data imach( 5) /    6 /
!     data imach( 6) /  -64 /
!     data imach( 7) /   63 /
!     data imach( 8) /   14 /
!     data imach( 9) /  -64 /
!     data imach(10) /   63 /
!
!     Machine constants for the IBM PC.
!
!      data imach(1)/2/
!      data imach(2)/31/
!      data imach(3)/2147483647/
!      data imach(4)/2/
!      data imach(5)/24/
!      data imach(6)/-125/
!      data imach(7)/128/
!      data imach(8)/53/
!      data imach(9)/-1021/
!      data imach(10)/1024/
!
!     Machine constants for the MACINTOSH II - ABSOFT
!     MACFORTRAN II.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     Machine constants for the MICROVAX - VMS FORTRAN.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   56 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     Machine constants for the PDP-11 FORTRAN SUPPORTING
!     32-BIT integer ARITHMETIC.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   56 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     Machine constants for the SEQUENT BALANCE 8000.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     Machine constants for the SILICON GRAPHICS IRIS-4D
!     SERIES (MIPS R3000 PROCESSOR).
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
!     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
!     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
!
  data imach( 1) /     2 /
  data imach( 2) /    31 /
  data imach( 3) / 2147483647 /
  data imach( 4) /     2 /
  data imach( 5) /    24 /
  data imach( 6) /  -125 /
  data imach( 7) /   128 /
  data imach( 8) /    53 /
  data imach( 9) / -1021 /
  data imach(10) /  1024 /
!
!     Machine constants for the UNIVAC 1100 SERIES.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   35 /
!     data imach( 3) / 34359738367 /
!     data imach( 4) /    2 /
!     data imach( 5) /   27 /
!     data imach( 6) / -128 /
!     data imach( 7) /  127 /
!     data imach( 8) /   60 /
!     data imach( 9) /-1024 /
!     data imach(10) / 1023 /
!
!     Machine constants for the VAX 11/780.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   56 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
  ipmpar = imach(i)

  return
end
subroutine negative_binomial_cdf_values ( n_data, f, s, p, cdf )

!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_CDF_VALUES returns values of the negative binomial CDF.
!
!  Discussion:
!
!    Assume that a coin has a probability P of coming up heads on
!    any one trial.  Suppose that we plan to flip the coin until we
!    achieve a total of S heads.  If we let F represent the number of
!    tails that occur in this process, then the value of F satisfies
!    a negative binomial PDF:
!
!      PDF(F,S,P) = Choose ( F from F+S-1 ) * P**S * (1-P)**F
!
!    The negative binomial CDF is the probability that there are F or
!    fewer failures upon the attainment of the S-th success.  Thus,
!
!      CDF(F,S,P) = sum ( 0 <= G <= F ) PDF(G,S,P)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    FC Powell,
!    Statistical Tables for Sociology, Biology and Physical Sciences,
!    Cambridge University Press, 1982.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) F, the maximum number of failures.
!
!    Output, integer ( kind = 4 ) S, the number of successes.
!
!    Output, real ( kind = 8 ) P, the probability of a success on one trial.
!
!    Output, real ( kind = 8 ) CDF, the probability of at most F failures
!    before the S-th success.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 27

  real ( kind = 8 ) cdf
  real ( kind = 8 ), save, dimension ( n_max ) :: cdf_vec = (/ &
    0.6367D+00, 0.3633D+00, 0.1445D+00, &
    0.5000D+00, 0.2266D+00, 0.0625D+00, &
    0.3438D+00, 0.1094D+00, 0.0156D+00, &
    0.1792D+00, 0.0410D+00, 0.0041D+00, &
    0.0705D+00, 0.0109D+00, 0.0007D+00, &
    0.9862D+00, 0.9150D+00, 0.7472D+00, &
    0.8499D+00, 0.5497D+00, 0.2662D+00, &
    0.6513D+00, 0.2639D+00, 0.0702D+00, &
    1.0000D+00, 0.0199D+00, 0.0001D+00 /)
  integer ( kind = 4 ) f
  integer ( kind = 4 ), save, dimension ( n_max ) :: f_vec = (/ &
     4,  3,  2, &
     3,  2,  1, &
     2,  1,  0, &
     2,  1,  0, &
     2,  1,  0, &
    11, 10,  9, &
    17, 16, 15, &
     9,  8,  7, &
     2,  1,  0 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) p
  real ( kind = 8 ), save, dimension ( n_max ) :: p_vec = (/ &
    0.50D+00, 0.50D+00, 0.50D+00, &
    0.50D+00, 0.50D+00, 0.50D+00, &
    0.50D+00, 0.50D+00, 0.50D+00, &
    0.40D+00, 0.40D+00, 0.40D+00, &
    0.30D+00, 0.30D+00, 0.30D+00, &
    0.30D+00, 0.30D+00, 0.30D+00, &
    0.10D+00, 0.10D+00, 0.10D+00, &
    0.10D+00, 0.10D+00, 0.10D+00, &
    0.01D+00, 0.01D+00, 0.01D+00 /)
  integer ( kind = 4 ) s
  integer ( kind = 4 ), save, dimension ( n_max ) :: s_vec = (/ &
    4, 5, 6, &
    4, 5, 6, &
    4, 5, 6, &
    4, 5, 6, &
    4, 5, 6, &
    1, 2, 3, &
    1, 2, 3, &
    1, 2, 3, &
    0, 1, 2 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    f = 0
    s = 0
    p = 0.0D+00
    cdf = 0.0D+00
  else
    f = f_vec(n_data)
    s = s_vec(n_data)
    p = p_vec(n_data)
    cdf = cdf_vec(n_data)
  end if

  return
end
subroutine normal_01_cdf_values ( n_data, x, fx )

!*****************************************************************************80
!
!! NORMAL_01_CDF_VALUES returns some values of the Normal 01 CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = NormalDistribution [ 0, 1 ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 17

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.5000000000000000D+00, &
    0.5398278372770290D+00, &
    0.5792597094391030D+00, &
    0.6179114221889526D+00, &
    0.6554217416103242D+00, &
    0.6914624612740131D+00, &
    0.7257468822499270D+00, &
    0.7580363477769270D+00, &
    0.7881446014166033D+00, &
    0.8159398746532405D+00, &
    0.8413447460685429D+00, &
    0.9331927987311419D+00, &
    0.9772498680518208D+00, &
    0.9937903346742239D+00, &
    0.9986501019683699D+00, &
    0.9997673709209645D+00, &
    0.9999683287581669D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.0000000000000000D+00, &
    0.1000000000000000D+00, &
    0.2000000000000000D+00, &
    0.3000000000000000D+00, &
    0.4000000000000000D+00, &
    0.5000000000000000D+00, &
    0.6000000000000000D+00, &
    0.7000000000000000D+00, &
    0.8000000000000000D+00, &
    0.9000000000000000D+00, &
    0.1000000000000000D+01, &
    0.1500000000000000D+01, &
    0.2000000000000000D+01, &
    0.2500000000000000D+01, &
    0.3000000000000000D+01, &
    0.3500000000000000D+01, &
    0.4000000000000000D+01 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine normal_cdf_values ( n_data, mu, sigma, x, fx )

!*****************************************************************************80
!
!! NORMAL_CDF_VALUES returns some values of the Normal CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = NormalDistribution [ mu, sigma ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the variance of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 12

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.5000000000000000D+00, &
    0.9772498680518208D+00, &
    0.9999683287581669D+00, &
    0.9999999990134124D+00, &
    0.6914624612740131D+00, &
    0.6305586598182364D+00, &
    0.5987063256829237D+00, &
    0.5792597094391030D+00, &
    0.6914624612740131D+00, &
    0.5000000000000000D+00, &
    0.3085375387259869D+00, &
    0.1586552539314571D+00 /)
  real ( kind = 8 ) mu
  real ( kind = 8 ), save, dimension ( n_max ) :: mu_vec = (/ &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.4000000000000000D+01, &
    0.5000000000000000D+01 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ), save, dimension ( n_max ) :: sigma_vec = (/ &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.4000000000000000D+01, &
    0.5000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.4000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.3000000000000000D+01, &
    0.3000000000000000D+01, &
    0.3000000000000000D+01 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    mu = 0.0D+00
    sigma = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    mu = mu_vec(n_data)
    sigma = sigma_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine poisson_cdf_values ( n_data, a, x, fx )

!*****************************************************************************80
!
!! POISSON_CDF_VALUES returns some values of the Poisson CDF.
!
!  Discussion:
!
!    CDF(X)(A) is the probability of at most X successes in unit time,
!    given that the expected mean number of successes is A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!    Daniel Zwillinger,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition, CRC Press, 1996, pages 653-658.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, integer X, the arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 21

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
    0.02D+00, 0.10D+00, 0.10D+00, 0.50D+00, &
    0.50D+00, 0.50D+00, 1.00D+00, 1.00D+00, &
    1.00D+00, 1.00D+00, 2.00D+00, 2.00D+00, &
    2.00D+00, 2.00D+00, 5.00D+00, 5.00D+00, &
    5.00D+00, 5.00D+00, 5.00D+00, 5.00D+00, &
    5.00D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.980D+00, 0.905D+00, 0.995D+00, 0.607D+00, &
    0.910D+00, 0.986D+00, 0.368D+00, 0.736D+00, &
    0.920D+00, 0.981D+00, 0.135D+00, 0.406D+00, &
    0.677D+00, 0.857D+00, 0.007D+00, 0.040D+00, &
    0.125D+00, 0.265D+00, 0.441D+00, 0.616D+00, &
    0.762D+00 /)
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) x
  integer ( kind = 4 ), save, dimension ( n_max ) :: x_vec = (/ &
     0, 0, 1, 0, &
     1, 2, 0, 1, &
     2, 3, 0, 1, &
     2, 3, 0, 1, &
     2, 3, 4, 5, &
     6 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0.0D+00
    x = 0
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
function psi ( xx )

!*****************************************************************************80
!
!! PSI evaluates the psi or digamma function, d/dx ln(gamma(x)).
!
!  Discussion:
!
!    The main computation involves evaluation of rational Chebyshev
!    approximations.  PSI was written at Argonne National Laboratory
!    for FUNPACK, and subsequently modified by A. H. Morris of NSWC.
!
!  Reference:
!
!    William Cody, Anthony Strecok, Henry Thacher,
!    Chebyshev Approximations for the Psi Function,
!    Mathematics of Computation,
!    Volume 27, 1973, pages 123-127.
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XX, the argument of the psi function.
!
!    Output, real ( kind = 8 ) PSI, the value of the psi function.  PSI
!    is assigned the value 0 when the psi function is undefined.
!
  implicit none

  real ( kind = 8 ) aug
  real ( kind = 8 ) den
  real ( kind = 8 ), parameter :: dx0 = &
    1.461632144968362341262659542325721325D+00
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ipmpar
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nq
  real ( kind = 8 ), parameter, dimension ( 7 ) :: p1 = (/ &
   0.895385022981970D-02, &
   0.477762828042627D+01, &
   0.142441585084029D+03, &
   0.118645200713425D+04, &
   0.363351846806499D+04, &
   0.413810161269013D+04, &
   0.130560269827897D+04/)
  real ( kind = 8 ), dimension ( 4 ) :: p2 = (/ &
    -0.212940445131011D+01, &
    -0.701677227766759D+01, &
    -0.448616543918019D+01, &
    -0.648157123766197D+00 /)
  real ( kind = 8 ), parameter :: piov4 = 0.785398163397448D+00
  real ( kind = 8 ) psi
!
!  Coefficients for rational approximation of
!  PSI(X) / (X - X0),  0.5D+00 <= X <= 3.0D+00
!
  real ( kind = 8 ), dimension ( 6 ) :: q1 = (/ &
    0.448452573429826D+02, &
    0.520752771467162D+03, &
    0.221000799247830D+04, &
    0.364127349079381D+04, &
    0.190831076596300D+04, &
    0.691091682714533D-05 /)
  real ( kind = 8 ), dimension ( 4 ) :: q2 = (/ &
    0.322703493791143D+02, &
    0.892920700481861D+02, &
    0.546117738103215D+02, &
    0.777788548522962D+01 /)
  real ( kind = 8 ) sgn
  real ( kind = 8 ) upper
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) xmax1
  real ( kind = 8 ) xmx0
  real ( kind = 8 ) xsmall
  real ( kind = 8 ) xx
  real ( kind = 8 ) z
!
!  XMAX1 is the largest positive floating point constant with entirely
!  integer representation.  It is also used as negative of lower bound
!  on acceptable negative arguments and as the positive argument beyond which
!  psi may be represented as LOG(X).
!
  xmax1 = real ( ipmpar(3), kind = 8 )
  xmax1 = min ( xmax1, 1.0D+00 / epsilon ( xmax1 ) )
!
!  XSMALL is the absolute argument below which PI*COTAN(PI*X)
!  may be represented by 1/X.
!
  xsmall = 1.0D-09

  x = xx
  aug = 0.0D+00

  if ( x == 0.0D+00 ) then
    psi = 0.0D+00
    return
  end if
!
!  X < 0.5,  Use reflection formula PSI(1-X) = PSI(X) + PI * COTAN(PI*X)
!
  if ( x < 0.5D+00 ) then
!
!  0 < ABS ( X ) <= XSMALL.  Use 1/X as a substitute for PI*COTAN(PI*X)
!
    if ( abs ( x ) <= xsmall ) then
      aug = -1.0D+00 / x
      go to 40
    end if
!
!  Reduction of argument for cotangent.
!
    w = -x
    sgn = piov4

    if ( w <= 0.0D+00 ) then
      w = -w
      sgn = -sgn
    end if
!
!  Make an error exit if X <= -XMAX1
!
    if ( xmax1 <= w ) then
      psi = 0.0D+00
      return
    end if

    nq = int ( w )
    w = w - real ( nq, kind = 8 )
    nq = int ( w * 4.0D+00 )
    w = 4.0D+00 * ( w - real ( nq, kind = 8 ) * 0.25D+00 )
!
!  W is now related to the fractional part of 4.0D+00 * X.
!  Adjust argument to correspond to values in first
!  quadrant and determine sign.
!
    n = nq / 2
    if ( n + n /= nq ) then
      w = 1.0D+00 - w
    end if

    z = piov4 * w
    m = n / 2

    if ( m + m /= n ) then
      sgn = -sgn
    end if
!
!  Determine final value for -PI * COTAN(PI*X).
!
    n = ( nq + 1 ) / 2
    m = n / 2
    m = m + m

    if ( m == n ) then

      if ( z == 0.0D+00 ) then
        psi = 0.0D+00
        return
      end if

      aug = 4.0D+00 * sgn * ( cos(z) / sin(z) )

    else

      aug = 4.0D+00 * sgn * ( sin(z) / cos(z) )

    end if

   40   continue

    x = 1.0D+00 - x

  end if
!
!  0.5 <= X <= 3
!
  if ( x <= 3.0D+00 ) then

    den = x
    upper = p1(1) * x

    do i = 1, 5
      den = ( den + q1(i) ) * x
      upper = ( upper + p1(i+1) ) * x
    end do

    den = ( upper + p1(7) ) / ( den + q1(6) )
    xmx0 = real ( x, kind = 8 ) - dx0
    psi = den * xmx0 + aug
!
!  3 < X < XMAX1
!
  else if ( x < xmax1 ) then

    w = 1.0D+00 / x**2
    den = w
    upper = p2(1) * w

    do i = 1, 3
      den = ( den + q2(i) ) * w
      upper = ( upper + p2(i+1) ) * w
    end do

    aug = upper / ( den + q2(4) ) - 0.5D+00 / x + aug
    psi = aug + log ( x )
!
!  XMAX1 <= X
!
  else

    psi = aug + log ( x )

  end if

  return
end
subroutine psi_values ( n_data, x, fx )

!*****************************************************************************80
!
!! PSI_VALUES returns some values of the Psi or Digamma function.
!
!  Discussion:
!
!    PSI(X) = d LN ( GAMMA ( X ) ) / d X = GAMMA'(X) / GAMMA(X)
!
!    PSI(1) = - Euler's constant.
!
!    PSI(X+1) = PSI(X) + 1 / X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    -0.5772156649D+00, -0.4237549404D+00, -0.2890398966D+00, &
    -0.1691908889D+00, -0.0613845446D+00, -0.0364899740D+00, &
     0.1260474528D+00,  0.2085478749D+00,  0.2849914333D+00, &
     0.3561841612D+00,  0.4227843351D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    1.0D+00,  1.1D+00,  1.2D+00,  &
    1.3D+00,  1.4D+00,  1.5D+00,  &
    1.6D+00,  1.7D+00,  1.8D+00,  &
    1.9D+00,  2.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine r8_swap ( x, y )

!*****************************************************************************80
!
!! R8_SWAP swaps two R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) X, Y.  On output, the values of X and
!    Y have been interchanged.
!
  implicit none

  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  z = x
  x = y
  y = z

  return
end
function rcomp ( a, x )

!*****************************************************************************80
!
!! RCOMP evaluates exp(-X) * X**A / Gamma(A).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, X, arguments of the quantity to be computed.
!
!    Output, real ( kind = 8 ) RCOMP, the value of exp(-X) * X**A / Gamma(A).
!
!  Local parameters:
!
!    RT2PIN = 1/SQRT(2*PI)
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) gam1
  real ( kind = 8 ) gamma
  real ( kind = 8 ) rcomp
  real ( kind = 8 ) rlog
  real ( kind = 8 ), parameter :: rt2pin = 0.398942280401433D+00
  real ( kind = 8 ) t
  real ( kind = 8 ) t1
  real ( kind = 8 ) u
  real ( kind = 8 ) x

  if ( a < 20.0D+00 ) then

    t = a * log ( x ) - x

    if ( a < 1.0D+00 ) then
      rcomp = ( a * exp ( t ) ) * ( 1.0D+00 + gam1 ( a ) )
    else
      rcomp = exp ( t ) / gamma ( a )
    end if

  else

    u = x / a

    if ( u == 0.0D+00 ) then
      rcomp = 0.0D+00
    else
      t = ( 1.0D+00 / a )**2
      t1 = ((( 0.75D+00 * t - 1.0D+00 ) * t + 3.5D+00 ) * t - 105.0D+00 ) &
        / ( a * 1260.0D+00 )
      t1 = t1 - a * rlog ( u )
      rcomp = rt2pin * sqrt ( a ) * exp ( t1 )
    end if

  end if

  return
end
function rexp ( x )

!*****************************************************************************80
!
!! REXP evaluates the function EXP(X) - 1.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) REXP, the value of EXP(X)-1.
!
  implicit none

  real ( kind = 8 ), parameter :: p1 =  0.914041914819518D-09
  real ( kind = 8 ), parameter :: p2 =  0.238082361044469D-01
  real ( kind = 8 ), parameter :: q1 = -0.499999999085958D+00
  real ( kind = 8 ), parameter :: q2 =  0.107141568980644D+00
  real ( kind = 8 ), parameter :: q3 = -0.119041179760821D-01
  real ( kind = 8 ), parameter :: q4 =  0.595130811860248D-03
  real ( kind = 8 ) rexp
  real ( kind = 8 ) w
  real ( kind = 8 ) x

  if ( abs ( x ) <= 0.15D+00 ) then

    rexp = x * ( ( ( p2 * x + p1 ) * x + 1.0D+00 ) &
      / ( ( ( ( q4 * x + q3 ) * x + q2 ) * x + q1 ) * x + 1.0D+00 ) )

  else

    w = exp ( x )

    if ( x <= 0.0D+00 ) then
      rexp = ( w - 0.5D+00 ) - 0.5D+00
    else
      rexp = w * ( 0.5D+00 + ( 0.5D+00 - 1.0D+00 / w ) )
    end if

  end if

  return
end
function rlog ( x )

!*****************************************************************************80
!
!! RLOG computes X - 1 - LN(X).
!
!  Modified:
!
!    06 August 2004
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) RLOG, the value of the function.
!
  implicit none

  real ( kind = 8 ), parameter :: a  =  0.566749439387324D-01
  real ( kind = 8 ), parameter :: b  =  0.456512608815524D-01
  real ( kind = 8 ), parameter :: half = 0.5D+00
  real ( kind = 8 ), parameter :: p0 =  0.333333333333333D+00
  real ( kind = 8 ), parameter :: p1 = -0.224696413112536D+00
  real ( kind = 8 ), parameter :: p2 =  0.620886815375787D-02
  real ( kind = 8 ), parameter :: q1 = -0.127408923933623D+01
  real ( kind = 8 ), parameter :: q2 =  0.354508718369557D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) rlog
  real ( kind = 8 ) t
  real ( kind = 8 ), parameter :: two =  2.0D+00
  real ( kind = 8 ) u
  real ( kind = 8 ) w
  real ( kind = 8 ) w1
  real ( kind = 8 ) x

  if ( x < 0.61D+00 ) then

    r = ( x - 0.5D+00 ) - 0.5D+00
    rlog = r - log ( x )

  else if ( x < 1.57D+00 ) then

    if ( x < 0.82D+00 ) then

      u = x - 0.7D+00
      u = u / 0.7D+00
      w1 = a - u * 0.3D+00

    else if ( x < 1.18D+00 ) then

      u = ( x - half ) - half
      w1 = 0.0D+00

    else if ( x < 1.57D+00 ) then

      u = 0.75D+00 * x - 1.0D+00
      w1 = b + u / 3.0D+00

    end if

    r = u / ( u + two )
    t = r * r
    w = ( ( p2 * t + p1 ) * t + p0 ) / ( ( q2 * t + q1 ) * t + 1.0D+00 )
    rlog = two * t * ( 1.0D+00 / ( 1.0D+00 - r ) - r * w ) + w1

  else if ( 1.57D+00 <= x ) then

    r = ( x - half ) - half
    rlog = r - log ( x )

  end if

  return
end
function rlog1 ( x )

!*****************************************************************************80
!
!! RLOG1 evaluates the function X - ln ( 1 + X ).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument.
!
!    Output, real ( kind = 8 ) RLOG1, the value of X - ln ( 1 + X ).
!
  implicit none

  real ( kind = 8 ), parameter :: a = 0.566749439387324D-01
  real ( kind = 8 ), parameter :: b = 0.456512608815524D-01
  real ( kind = 8 ) h
  real ( kind = 8 ), parameter :: half = 0.5D+00
  real ( kind = 8 ), parameter :: p0 = 0.333333333333333D+00
  real ( kind = 8 ), parameter :: p1 = -0.224696413112536D+00
  real ( kind = 8 ), parameter :: p2 = 0.620886815375787D-02
  real ( kind = 8 ), parameter :: q1 = -0.127408923933623D+01
  real ( kind = 8 ), parameter :: q2 = 0.354508718369557D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) rlog1
  real ( kind = 8 ) t
  real ( kind = 8 ), parameter :: two =  2.0D+00
  real ( kind = 8 ) w
  real ( kind = 8 ) w1
  real ( kind = 8 ) x

  if ( x < -0.39D+00 ) then

    w = ( x + half ) + half
    rlog1 = x - log ( w )

  else if ( x < -0.18D+00 ) then

    h = x + 0.3D+00
    h = h / 0.7D+00
    w1 = a - h * 0.3D+00

    r = h / ( h + 2.0D+00 )
    t = r * r
    w = ( ( p2 * t + p1 ) * t + p0 ) / ( ( q2 * t + q1 ) * t + 1.0D+00 )
    rlog1 = two * t * ( 1.0D+00 / ( 1.0D+00 - r ) - r * w ) + w1

  else if ( x <= 0.18D+00 ) then

    h = x
    w1 = 0.0D+00

    r = h / ( h + two )
    t = r * r
    w = ( ( p2 * t + p1 ) * t + p0 ) / ( ( q2 * t + q1 ) * t + 1.0D+00 )
    rlog1 = two * t * ( 1.0D+00 / ( 1.0D+00 - r ) - r * w ) + w1

  else if ( x <= 0.57D+00 ) then

    h = 0.75D+00 * x - 0.25D+00
    w1 = b + h / 3.0D+00

    r = h / ( h + 2.0D+00 )
    t = r * r
    w = ( ( p2 * t + p1 ) * t + p0 ) / ( ( q2 * t + q1 ) * t + 1.0D+00 )
    rlog1 = two * t * ( 1.0D+00 / ( 1.0D+00 - r ) - r * w ) + w1

  else

    w = ( x + half ) + half
    rlog1 = x - log ( w )

  end if

  return
end
subroutine student_cdf_values ( n_data, a, x, fx )

!*****************************************************************************80
!
!! STUDENT_CDF_VALUES returns some values of the Student CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) A, real ( kind = 8 ) X, the arguments of
!    the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 13

  integer ( kind = 4 ) a
  integer ( kind = 4 ), save, dimension ( n_max ) :: a_vec = (/ &
    1, 2, 3, 4, &
    5, 2, 5, 2, &
    5, 2, 3, 4, &
    5 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.60D+00, 0.60D+00, 0.60D+00, 0.60D+00, &
    0.60D+00, 0.75D+00, 0.75D+00, 0.95D+00, &
    0.95D+00, 0.99D+00, 0.99D+00, 0.99D+00, &
    0.99D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.325D+00, 0.289D+00, 0.277D+00, 0.271D+00, &
    0.267D+00, 0.816D+00, 0.727D+00, 2.920D+00, &
    2.015D+00, 6.965D+00, 4.541D+00, 3.747D+00, &
    3.365D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
function stvaln ( p )

!*****************************************************************************80
!
!! STVALN provides starting values for the inverse of the normal distribution.
!
!  Discussion:
!
!    The routine returns an X for which it is approximately true that
!      P = CUMNOR(X),
!    that is,
!      P = Integral ( -infinity < U <= X ) exp(-U*U/2)/sqrt(2*PI) dU.
!
!  Reference:
!
!    William Kennedy, James Gentle,
!    Statistical Computing,
!    Marcel Dekker, NY, 1980, page 95,
!    QA276.4 K46
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the probability whose normal deviate
!    is sought.
!
!    Output, real ( kind = 8 ) STVALN, the normal deviate whose probability
!    is approximately P.
!
  implicit none

  real ( kind = 8 ) eval_pol
  real ( kind = 8 ) p
  real ( kind = 8 ) sgn
  real ( kind = 8 ) stvaln
  real ( kind = 8 ), parameter, dimension(0:4) :: xden = (/ &
    0.993484626060D-01, &
    0.588581570495D+00, &
    0.531103462366D+00, &
    0.103537752850D+00, &
    0.38560700634D-02 /)
  real ( kind = 8 ), parameter, dimension(0:4) :: xnum = (/ &
    -0.322232431088D+00, &
    -1.000000000000D+00, &
    -0.342242088547D+00, &
    -0.204231210245D-01, &
    -0.453642210148D-04 /)
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  if ( p <= 0.5D+00 ) then

    sgn = -1.0D+00
    z = p

  else

    sgn = 1.0D+00
    z = 1.0D+00 - p

  end if

  y = sqrt ( -2.0D+00 * log ( z ) )
  stvaln = y + eval_pol ( xnum, 4, y ) / eval_pol ( xden, 4, y )
  stvaln = sgn * stvaln

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
