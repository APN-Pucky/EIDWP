PROGRAM fxy
IMPLICIT NONE
!REAL, PARAMETER :: PI= 4.D0*DATAN(1.D0)
REAL :: x,y,r
WRITE(*,*) "x= "
READ (*,*) x
WRITE(*,*) "y= "
READ (*,*) y

IF(x>=0 .AND. y>=0) r=x+y
IF(x>=0 .AND. y<0) r=x+y**2
IF(x<0 .AND. y>=0) r=x**2+y
IF(x<0 .AND. y<0) r=x**2+y**2
WRITE(*,*) "r_explizit= " , r

IF(x>=0) THEN
        IF(y>=0) THEN
                r=x+y
        ELSE
                r=x+y**2
        END IF
ELSE 
        IF(y>=0) THEN
                r=x**2+y
        ELSE
                r=x**2+y**2
        END IF
END IF
WRITE(*,*) "r_schachtel= " , r

END PROGRAM fxy
