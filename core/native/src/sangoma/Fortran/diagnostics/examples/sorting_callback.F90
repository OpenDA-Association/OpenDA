! ****************************************************
! ***        Examples for sort routines            ***
! ****************************************************

SUBROUTINE sisort(n, veca)

! Sorts a vector veca(1:n) into ascending numerical order, by
! straight insertion.
!
! For large vectors, this routine will not be efficient.

  IMPLICIT NONE

  INTEGER, INTENT(in) :: n 
  REAL, INTENT(inout) :: veca(n)

  INTEGER :: i, j, k 
  REAL :: tmpa
  LOGICAL :: eflag

  DO j = 2, n

     eflag = .false.

     tmpa = veca(j) 

     sortloop: DO i = j-1, 1, -1 
        k = i

        IF(veca(i) <= tmpa) THEN
           eflag = .true.
           EXIT sortloop
        END IF

        veca(i+1) = veca(i) 
     ENDDO sortloop

     IF (.NOT.eflag) k=0

     veca(k+1) = tmpa 

  ENDDO 

END SUBROUTINE sisort



SUBROUTINE sisort2(n, veca, vecb)

! Sorts a vector veca(1:n) into ascending numerical order, by
! straight insertion. vecb is sorted in the same order.
!
! For large vectors, this routine will not be efficient.

  IMPLICIT NONE

  INTEGER, INTENT(in) :: n 
  REAL, INTENT(inout) :: veca(n), vecb(n)

  INTEGER :: i, j, k 
  REAL :: tmpa, tmpb 
  LOGICAL :: eflag

  DO j = 2, n

     tmpa = veca(j) 
     tmpb = vecb(j) 

     sortloop: DO i = j-1, 1, -1 
        k = i

        IF(veca(i) <= tmpa) THEN
           eflag = .true.
           EXIT sortloop
        END IF

        veca(i+1) = veca(i) 
        vecb(i+1) = vecb(i) 
     ENDDO sortloop

     IF (.NOT.eflag) k=0

     veca(k+1) = tmpa 
     vecb(k+1) = tmpb 

  ENDDO 

END SUBROUTINE sisort2



