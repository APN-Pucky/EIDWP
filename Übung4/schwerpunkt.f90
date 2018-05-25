PROGRAM Schwerpunkt
IMPLICIT NONE
CHARACTER(2) :: elem
REAL :: sum_x=0,sum_y=0,sum_z=0,x,y,z
INTEGER :: n=0,i
INTEGER :: ierror1,ierror2

OPEN(UNIT=8,FILE='au12.xyz', STATUS='OLD', IOSTAT=ierror1)
DO
        x=0
        y=0
        z=0
        READ(8,'(A,3(F14.8))',IOSTAT=ierror2) elem,x,y,z
        IF(ierror2 > 0) GOTO 99 !continue
        IF(ierror2 < 0) GOTO 999 !end
        IF(elem == 'Au') THEN
                n = n+1
                !WRITE(*,*) x,y,z
                sum_x = sum_x + x
                sum_y = sum_y + y
                sum_z = sum_z + z
        ENDIF
        99 CONTINUE
END DO
999 CONTINUE
CLOSE(8)

WRITE(*,'("IOSTAT-Schwerpunkt (x,y,z): (", 2(F14.8,","),F14.8, ")")') sum_x/n,sum_y/n,sum_z/n

OPEN(UNIT=7,FILE='au12.xyz', STATUS='OLD', IOSTAT=ierror1)
READ(7, *) n
READ(7,*) !skip 2nd
sum_x =0
sum_y =0
sum_z =0
DO i=0,n,1
        x=0
        y=0
        z=0
        READ(7,'(A,3(F14.8))') elem,x,y,z
        IF(elem == 'Au') THEN
                !WRITE(*,*) x,y,z
                sum_x = sum_x + x
                sum_y = sum_y + y
                sum_z = sum_z + z
        ENDIF
END DO


WRITE(*,'("1.Zeile-Schwerpunkt2 (x,y,z): (", 2(F14.8,","),F14.8, ")")') sum_x/n,sum_y/n,sum_z/n



END PROGRAM
