PROGRAM pyt 
IMPLICIT NONE
REAL, PARAMETER :: PI= 4.D0*DATAN(1.D0)
REAL :: a,b,c,alpha
WRITE(*,*) "c= "
READ (*,*) c
WRITE(*,*) "alpha= "
READ (*,*) alpha
a = sin(alpha*PI/180)*c
b = cos(alpha*PI/180)*c
WRITE(*,*) "c_in = " ,c
WRITE(*,*) "c_out = " , sqrt(a**2+b**2)

END PROGRAM pyt
