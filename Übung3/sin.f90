PROGRAM psin
IMPLICIT NONE
REAL :: x,r=0,error=0
INTEGER :: i,n=0
REAL, PARAMETER :: PI= 4.D0*DATAN(1.D0)
!WRITE(*,("sin(", F9.6, ")=sin(", F9.6, ")")) input, x
!WRITE(*,*) "sin(x)=" , r
DO i = 1,10
        x = i*PI/10
        call sinus(x,n,r,error)
        WRITE(*,1337) x,n,r,sin(x),error
END DO
 
1337 FORMAT ("x=", F9.6, ",n=", I3, ",Summe= " , F11.8 ,",sin(x)=", F11.8, ",Fehler=", E11.4)
 
contains
SUBROUTINE sinus(x_in, n,r, error)
        IMPLICIT NONE
        REAL :: x_in, x,add, r, error
        INTEGER :: n
        n=0
        r=0
        x = MODULO(x_in, 2*PI) ! MOD 2*PI verhindert overflows bei großen x
        DO 
                add = ((-1)**(n))*(x**(2*n+1))/(fac(2.*n+1))
                r=r+add
                if(ABS(add)*100000 <= ABS(r)) EXIT
                n=n+1
        END DO
        error = ABS(r-sin(x))/sin(x)
END SUBROUTINE sinus

recursive FUNCTION fac(n) result(r)
        IMPLICIT NONE
        REAL n,r !REAL um Overflows zu verhindern
        IF(n<=1) THEN
                r=1
        ELSE 
                r=n*fac(n-1)
        END IF
        return 
END FUNCTION fac

END PROGRAM psin
