PROGRAM period
IMPLICIT NONE
CHARACTER(10) :: ename = "Sauerstoff"
INTEGER :: ordnung = 8
REAL :: mass = 15.999
REAL :: radius = 6.048E-11

WRITE(*,1337) ename, ordnung, mass, radius

1337 FORMAT (50('='),/,'+',T50,'+',/,'+',T10, "Eigenschaften von ", A, T50, '+', /,&
'+', T10, 28('-'), T50, '+' , /, '+', T50, '+', /, '+', T5, &
'Ordnungszahl = ' , I0, T50, '+', /, '+', T5, &
'Atommasse = ' , F6.3, T50, '+',/, '+', T5, &
'Atomradius = ' , ES9.3, T50 '+', /, '+', T50 , '+', /, 50('='))
END PROGRAM 
