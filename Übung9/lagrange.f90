program lagrange
        implicit none
        CHARACTER(*), PARAMETER :: default_fname = "data.dat"
        CHARACTER(*), PARAMETER :: default_ogname = "output_global_data.dat"
        CHARACTER(*), PARAMETER :: default_olname = "output_local_data.dat"
        CHARACTER(*), PARAMETER :: default_odname = "output_diff_data.dat"
        CHARACTER(len=100) :: fname
        INTEGER :: ierror,nlines=0,i
        real, dimension(:,:),allocatable :: dat

        call load_data
        call write_data


         
        contains

        real function lagrange_fit(ddat, from,to, x)
                real, dimension(:,:),intent(in) :: ddat
                integer, intent(in):: from,to 
                real, dimension(size(ddat,1)) :: lambda
                real, intent(in) :: x
                if(size(ddat,1) .LT. to .OR. from .LT. 1) WRITE(*,*) "!! ERROR: OOB !!"
                call calc_lambda( ddat, lambda, from, to, x)
                if(abs(sum(lambda(from:to))-1) > 1e-4) write(*,*) "!! ERROR: sum_i lambda(i) /= 1 !!"
                lagrange_fit = sum(ddat(from:to,2)*lambda(from:to))
        end function

        subroutine calc_lambda( ddat,lambda, from ,to ,x)
                real, dimension(:,:),intent(in) :: ddat
                integer, intent(in):: from,to 
                real, dimension(:),intent(out) :: lambda
                real, intent(in) :: x
                integer :: i,j
                do i=from,to
                        lambda(i) = 1.0
                        do j=from,to
                                if(i/=j) then
                                        lambda(i) = lambda(i)*(x-ddat(j,1))/(ddat(i,1)-ddat(j,1))
                                end if
                        end do
               end do
        end subroutine 

        subroutine write_data()
                integer :: x,seg=0
                real :: f,h=sqrt(1e-7)
                open(unit=7, file=default_ogname, status='new', IOSTAT=ierror)
                do x=0,200,5
                        write(7,*) x,";", lagrange_fit(dat,1,nlines,real(x))
                end do
                close(7)
                open(unit=6, file=default_olname, status='new', IOSTAT=ierror)
                do x=0,200,5
                        write(6,*) x,";", lagrange_fit(dat,seg*2+1,seg*2+3,real(x))
                        seg = x/50
                end do
                close(6)
                open(unit=5, file=default_odname, status='new', IOSTAT=ierror)
                do x=0,195,5
                        seg = x/50
                        f=lagrange_fit(dat,seg*2+1,seg*2+3,real(x))
                        seg = (x+h)/50
                        f=(lagrange_fit(dat,seg*2+1,seg*2+3,real(x)+h)-f)/h
                        write(5,*) x,";",f
                end do
                close(5)

        end subroutine 

        subroutine load_data()
                write(*,'(3(A))', advance='no') "Dateiname (default=", default_fname ,"):"       
                read(*,'(100A)') fname
                if(fname=='') fname = default_fname
                write(*,'(2(A))') "Öffne: ", fname

                open(unit=9, file=fname, status='old', IOSTAT=ierror)
                do 
                        read(9,*,iostat=ierror)
                        if(ierror /=0)exit
                        nlines = nlines +1
                end do
                close(9)

                allocate(dat(nlines,2))
                open(unit=8,file=fname,status='old',iostat=ierror)
                DO i=1,nlines
                        read(8,*) dat(i,1), dat(i,2)
                        write(*,*) dat(i,1), dat(i,2)
                end do
                close(8)
        end subroutine 
        

end program



