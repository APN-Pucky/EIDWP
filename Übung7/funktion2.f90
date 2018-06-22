MODULE funktion2
        IMPLICIT NONE
        type funktion
                procedure(func), pointer,nopass :: p_f => id
                real :: scal=1
                procedure(rfunc), pointer :: p_eval => base_func
                procedure(rafunc), pointer :: p_eval_a => NULL()
                procedure(fffunc), pointer :: p_div => base_div
                procedure(fffunc), pointer :: p_minus => base_minus
                procedure(fffunc), pointer :: p_plus => base_plus
                procedure(fffunc), pointer :: p_pow => base_pow
                procedure(fffunc), pointer :: p_mult => base_mult
                type(funktion), pointer, dimension(:) :: p_f_array
                logical :: is_allocated = .false.
                CONTAINS
                procedure, pass :: dealloc
                procedure, pass :: base_func
                procedure, pass :: evaluate
                procedure, pass :: evaluate_a
        end type
        abstract interface
                real function func(x)
                        real :: x
                end function 
                real function rfunc(rf, x)
                        import funktion
                        class(funktion) :: rf
                        real :: x
                end function
                real function rafunc(rf, x)
                        import funktion
                        class(funktion) :: rf
                        real,dimension(:) :: x
                end function

                function fffunc(f1,f2)
                        import funktion
                        class(funktion) :: f1
                        type(funktion) :: f2,fffunc
                end function
        end interface

        interface operator(*)
                module procedure mult_f, mult_real_l, mult_real_r
        end interface
        interface operator(/)
                module procedure div_f, div_real_l, div_real_r
        end interface
        interface operator(**)
                module procedure pow_f, pow_real_l, pow_real_r
        end interface
        interface operator(+)
                module procedure plus_f, plus_real_l, plus_real_r
        end interface
        interface operator(-)
                module procedure minus_f, minus_real_l, minus_real_r
        end interface


        type(funktion) :: x,dx,d
        real  :: h = sqrt(1e-7)
        integer :: N = 5 !odd!

        CONTAINS 
                include 'base_operations.inc'
                include 'dx_operations.inc'

                subroutine alloc(self,n)
                        type(funktion) :: self
                        integer :: n
                        allocate(self%p_f_array(n))
                        self%is_allocated = .true.
                end subroutine
                recursive subroutine dealloc(self)
                        class(funktion) :: self
                        integer n,i
                        if(.not. self%is_allocated) return
                        n = size(self%p_f_array)
                        DO i=1,n
                                CALL self%p_f_array(i)%dealloc
                        end do
                        deallocate(self%p_f_array)
                        self%is_allocated = .false.
                end subroutine
                        
                function create_funktion(f)
                        type(funktion) :: create_funktion
                        procedure(func), pointer :: f
                        !create_funktion%p_eval => base_func
                        create_funktion%p_f => f
                end function
                real function base_func(self,x)
                        real :: x
                        class(funktion) :: self
                        base_func = self%p_f(x)
                end function

                real recursive function evaluate(self,x)
                        real :: x
                        class(funktion) :: self
                        !n=size(self%p_f_array)
                        evaluate = self%scal * self%p_eval(x)
                end function

                function evaluate_a(self,x)
                        real, dimension(:) :: x
                        real :: evaluate_a
                        class(funktion) :: self
                        !n=size(self%p_f_array)
                        evaluate_a = self%scal * self%p_eval_a(x)
                end function

                function init_dx()
                        type(funktion) :: init_dx
                        procedure(func), pointer :: p_id => id
                        init_dx%p_f =>  p_id !dx alone?!?
                        init_dx%p_div => dx_div
                        init_dx%p_mult => dx_mult
                end function        
                
                function init_x()
                        type(funktion) :: init_x
                        procedure(func), pointer :: p_id => id
                        init_x = create_funktion(p_id)
                end function
                function init_d()
                        type(funktion) :: init_d
                        procedure(func), pointer :: p_d => idd
                        init_d = create_funktion(p_d)
                end function
                real function idd(x)
                        real ::x
                        idd =1.0
                end function
                real function id(x)
                        real :: x
                        id =x
                end function
                subroutine init()
                        x = init_x()
                        d = init_d()
                        dx = init_dx()
                end subroutine
END MODULE
