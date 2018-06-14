MODULE module_land
        IMPLICIT NONE
        TYPE LAND
                INTEGER :: land_id
                CHARACTER(50) :: land_name
                INTEGER :: land_population
                CONTAINS
                procedure :: copy_land
                generic :: assignment(=) => copy_land
        END TYPE
        CONTAINS 
                subroutine copy_land(to, from)
                        class(LAND), intent(out) :: to
                        type(LAND), intent(in) :: from

                        to%land_id = from%land_id
                        to%land_name = from%land_name
                        to%land_population = from%land_population
                end subroutine
END MODULE
