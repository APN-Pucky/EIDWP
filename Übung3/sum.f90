PROGRAM psum
IMPLICIT NONE
INTEGER ::  n,i
REAL :: r =0
WRITE(*,*) "n="
READ(*,*) n
DO i = 0,n
        r = r + 2.0**(-i)
END DO
WRITE(*,*) "sum n=" , r

END PROGRAM 
