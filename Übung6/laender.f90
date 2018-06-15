PROGRAM laender
        USE module_land
        IMPLICIT NONE
        INTEGER :: ierror,nlines=0,i,j
        TYPE(LAND), Dimension(:), ALLOCATABLE :: laenderlist
        INTEGER ,ALLOCATABLE:: index_list(:)
        REAL :: ran ! Für random/shuffle
        !Zaehlen der Zeilen für ALLOCATION
        OPEN(UNIT=1,FILE="laender-unsortiert.dat", STATUS='OLD', IOSTAT=ierror)
        DO
                READ(1,*,IOSTAT=ierror)
                IF(ierror /= 0) EXIT
                nlines = nlines +1
        END DO
        CLOSE(1)

        ALLOCATE(laenderlist(nlines))
        ALLOCATE(index_list(nlines))

        OPEN(UNIT=8,FILE="laender-unsortiert.dat", STATUS='OLD', IOSTAT=ierror)
        DO i=1,nlines,1
                READ(8,'(I12,A50,I12)',IOSTAT=ierror) laenderlist(i)%land_id, laenderlist(i)%land_name, &
                laenderlist(i)%land_population
                IF(ierror /= 0) EXIT !end
                !WRITE(*,*) laenderlist(i)%land_id, laenderlist(i)%land_name, laenderlist(i)%land_population
        END DO
        !SELECTION SORT Aufgabe 1 a)
        DO i=1,nlines,1
                DO j=i+1, nlines, 1 
                        IF(laenderlist(i)%land_name .GT. laenderlist(j)%land_name) THEN
                                CALL swap(laenderlist(i), laenderlist(j))
                        END IF
                 END DO
        END DO
        CALL set_index(index_list) ! INIT index_list(i)=i
        WRITE(*,*) "Alphabetische Laender:"
        CALL print_laender(laenderlist,index_list) !PRINT Laender in gegebener index_list Reihenfolge

        !SELECION SORT Aufgabe 1 b) 
        DO i=1,nlines,1
                DO j=i+1, nlines, 1 
                        IF(laenderlist(i)%land_population .LT. laenderlist(j)%land_population) THEN
                                CALL swap(laenderlist(i),laenderlist(j))
                        END IF
                 END DO
        END DO
        WRITE(*,*) "Laender nach Population 1:"
        CALL print_laender(laenderlist,index_list) !PRINT Laender in gegebener index_list Reihenfolge
        !Shuffle, da laenderlist bereits sortiert ist
        DO i = nlines,2,-1
                CALL RANDOM_NUMBER(ran)
                j = 1+FLOOR(i*ran)
                CALL swap(laenderlist(i),laenderlist(j))
        END DO
        !SELECTION SORT Aufgabe 1 c)
        CALL sort(get_laender_populations_real_array(laenderlist),index_list) 
        WRITE(*,*) "Laender nach Population 2:"
        CALL print_laender(laenderlist,index_list)
        !END
        CLOSE(8)
        DEALLOCATE(laenderlist)
        DEALLOCATE(index_list)
        CONTAINS 
        SUBROUTINE print_laender(laenderlist,index_list)
                TYPE(LAND), DIMENSION(:),INTENT(IN) :: laenderlist
                INTEGER, INTENT(IN) :: index_list(:)
                INTEGER :: n,  i
                n = SIZE(laenderlist)
                DO i = 1,n,1 
                        WRITE(*,'(A50,I12)') laenderlist(index_list(i))%land_name, &
                        laenderlist(index_list(i))%land_population
                END DO
        END SUBROUTINE
        SUBROUTINE sort(a,b) ! a wird sortiert und b enthaelt die neuen Indexe b(alt)=neu
                REAL , INTENT(IN):: a(:)
                INTEGER , INTENT(OUT):: b(:)
                INTEGER :: n,i,j, temp
                CALL set_index(b)
                n = SIZE(a)
                DO i=1,n
                        DO j=i+1, n 
                                IF(a(b(i)) .LT. a(b(j))) THEN
                                        temp = b(i) !swap
                                        b(i) = b(j)
                                        b(j) = temp
                                END IF
                        END DO
                END DO
        END SUBROUTINE 
        SUBROUTINE set_index(b)
                INTEGER , INTENT(OUT) :: b(:)
                INTEGER :: n,i
                n = SIZE(b)
                DO i = 1,n
                        b(i) = i
                END DO
         END SUBROUTINE
         SUBROUTINE swap (a,b)
                TYPE(LAND), INTENT(INOUT) :: a,b
                TYPE(LAND) :: tmp
                tmp = a
                a = b
                b = tmp
         END SUBROUTINE
         FUNCTION get_laender_populations_real_array(laenderlist) result(r)
                TYPE(LAND), DIMENSION(:),INTENT(IN) :: laenderlist
                REAL, ALLOCATABLE :: r(:)
                INTEGER :: i,n
                n = SIZE(laenderlist)
                ALLOCATE(r(n))
                DO i=1,n
                        r(i) = laenderlist(i)%land_population
                END DO
        END FUNCTION
END PROGRAM

        
