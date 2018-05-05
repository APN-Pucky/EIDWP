PROGRAM pfac
IMPLICIT NONE
INTEGER ::  k
INTEGER :: i 
REAL ::  l
WRITE(*,*) "n="
READ(*,*) i
k = ifac(i)
WRITE(*,*) "n!=" , k , "(INTEGER)"
l = rfac(REAL(i))
WRITE(*,*) "n!=" , l , "(REAL)"

contains 
recursive FUNCTION ifac(n) result(r)
        IMPLICIT NONE
        INTEGER n,r
        IF(n==1) THEN
                r=1
        ELSE 
                r=n*ifac(n-1)
        END IF
        return 
END function

recursive FUNCTION rfac(n) result(r)
        IMPLICIT NONE
        REAL n,r
        IF(n<=1) THEN
                r=1
        ELSE 
                r=n*rfac(n-1)
        END IF
        return 
END function


END PROGRAM 
