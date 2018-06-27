PROGRAM schwerpunkt
        IMPLICIT NONE

        CHARACTER(100) :: fname

        REAL, DIMENSION(:,:), ALLOCATABLE :: xyz
        REAL, DIMENSION(3) :: r
        CHARACTER(3), DIMENSION(:), ALLOCATABLE :: elemname

        INTEGER :: n=0,i,j
        INTEGER :: ierror
        REAL :: d



        WRITE(*,'("Dateiname (default=au12.xyz): ")')
        READ(*,'(A)') fname
        IF(fname == '') fname = 'au12.xyz'
        WRITE(*,'("Öffne ", (A))') fname

        OPEN(UNIT=7,FILE=fname, STATUS='OLD', IOSTAT=ierror)

        READ(7, *) n
        READ(7,*) !skip 2nd

        ALLOCATE(xyz(n,3))
        ALLOCATE(elemname(n))


        DO i=1,n,1
                READ(7,'(A,3(F14.8))',IOSTAT=ierror) elemname(i), xyz(i,:)
                IF(ierror > 0) GOTO 99 !CONTINUE
                IF(ierror < 0) EXIT
                WRITE(*,'(I3, " ", A, ": (", 2(F14.8,","),F14.8, ")")') i,elemname(i),xyz(i,:)
                99 CONTINUE
        END DO

        CALL center(xyz,n,r)
        WRITE(*,'("Schwerpunkt (x,y,z): (", 2(F14.8,","),F14.8, ")")') r(:)
        
        WRITE(*,'("Zwei Atomindizes:")')
        READ(*,'(I3)') i
        READ(*,'(I3)') j

        IF(i > n .OR. j >n .OR. i <= 0 .OR. j <= 0) THEN
                WRITE(*,*) "Indizes nicht im Wertebereich" 
        ELSE
                d = distance(xyz,i,j)
                WRITE(*, '("Distanz ", (I3), " -> ", (I3) , " :" , F14.8)') i,j,d
        END IF 



        CONTAINS
        REAL FUNCTION distance (xyz,i,j)
                INTEGER :: i,j
                REAL, DIMENSION(:,:) :: xyz
                distance = SQRT(SUM((xyz(i,:)-xyz(j,:))**2))
                return 
        END FUNCTION

        SUBROUTINE center (xyz,n,r)
                INTEGER :: n
                REAL, DIMENSION(:,:) :: xyz
                REAL, DIMENSION(3) :: r
                r=SUM(xyz,1)/n
                RETURN 
        END SUBROUTINE

END PROGRAM


