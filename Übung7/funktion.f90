MODULE funktion
        IMPLICIT NONE
        type ff
                type(ff), pointer :: a
                procedure(func), pointer :: m
                CONTAINS
                procedure, pass :: v
                procedure, pass :: df
                procedure, pass :: ndf
        end type
        abstract interface
                function func(self,x)
                        import ff
                        class(ff) :: self
                        real :: x
                        real :: func
                end function 
        end interface
      
        CONTAINS 
                real recursive function v(self,x)
                        class(ff) :: self
                        real :: x
                        v = self%m(x)
                end function
                function df(self)
                        class(ff) :: self
                        type(ff) df
                        df%a = self 
                        df%m => ndf
                end function
                real function ndf(self,x)
                        class(ff) :: self
                        real :: x,h=sqrt(1e-7)
                        ndf = (self%a%v(x+h)-self%a%v(x))/h
                end function
                !!

                real function id(x)
                        real :: x
                        id = x
                end function
END MODULE
