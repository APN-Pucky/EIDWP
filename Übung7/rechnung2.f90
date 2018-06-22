program rechnung
      use funktion2
      implicit none
      real :: x_e,x_e2,r
      real :: e = 2.71828
      real, dimension(2) :: xa = (/1,0/)
      type(funktion) :: f,f1,f2
      CALL init
      f = ((x**2.)/dx)/dx
      read(*,*) x_e
      r =   f%evaluate(x_e) 
      write(*,*) r
      CALL f%dealloc
end program




