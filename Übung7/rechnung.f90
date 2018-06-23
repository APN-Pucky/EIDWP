program rechnung
      use mfunktion
      implicit none
      real :: x_input=0 !value needed, because of memory position should be allocated early!
      real :: best_h, h_opt_calc = sqrt(1e-7),xo,xu
      real,dimension(3) :: ha = (/0.01,0.001,0.0001/) ! test h Werte!
      integer :: i,s
      procedure(func), pointer  :: p_opt_h => optimize_h
      type(funktion) :: x2,df,opt_h,ex
      CALL init !setup x,dx,d!
      s = size(ha)

      WRITE(*,100) "Ableiten"
      x2 = (x**2.)/dx !x**2 ableiten!
      x_input = read_x("x =")
      df = x2-2.*x !differenz funktion!
      WRITE(*,100) "Vorwaertsdifferenz-Methode"
      DO i=1,s
                CALL print_h(ha(i))
      end do
      h = 1e-3
      opt_h = create_funktion(p_opt_h)
      eps = 1e-5
      best_h = minimum(opt_h,1e-3,1e-9)
      h = best_h
      WRITE(*,100) "optimize h"
      WRITE(*,101) "Bisektion"
      CALL print_h( best_h)
      WRITE(*,101) "Theorie"
      CALL print_h( h_opt_calc)

      WRITE(*,100) "Integrieren"
      ex = (e**x)*dx   !e**x integrieren
      xo = read_x( "Integral obere Grenze x1=")
      xu = read_x( "Integral unter Grenze x2=")
      WRITE(*,100) "Simpson-Methode"
      N=5
      WRITE(*,*) "int e**x von x2 bis x1 =", ex%get((/xo,xu/)) !ex Wert!
        
      !formats!
      100 FORMAT(50("="),T8,A)
      101 FORMAT(50("-"),T5,A)

      CALL df%dealloc
      CALL x2%dealloc
      CALL ex%dealloc
      CALL opt_h%dealloc
      CONTAINS 
              subroutine print_h (param_h)
                      real :: param_h
                      h = param_h
                      WRITE(*,*) "f(x)=(d*x**2)/dx , h=", param_h
                      WRITE(*,'(T9, "|->")',advance='no') 
                      WRITE(*,*) 'f(x)=', x2%get(x_input) !x2 Wert!
                      WRITE(*,'(T9, "`->")',advance='no') 
                      WRITE(*,*) 'diff=', df%get(x_input) !x2 Differenz!
              end subroutine 

              function read_x (s)
                      real :: read_x
                      character(*) :: s
                      WRITE(*,*) s
                      READ(*,*) read_x
                      WRITE(*,*) s,read_x
              end function


              real function optimize_h(oh) !Gibt die Differenz zum analytischen Wert in Abhängikeit von h=oh an der Stelle x_input!
                      real :: oh
                      type(funktion)  :: dff
                      real :: x_val
                      x_val = x_input !avoid memory/pointer issue
                      h=oh
                      dff = (x**2./dx)-2.*x
                      !WRITE(*,*) oh, dff%get(2.)
                      optimize_h = abs(dff%get(x_val))
              end function

end program



