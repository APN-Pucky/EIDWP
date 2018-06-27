PROGRAM test
IMPLICIT NONE
Integer i,j
operator op

DO i = 1,10
         
        !j = i operator(+) i 
        op = operator(+)
        WRITE(*,*) op
END DO
END PROGRAM
