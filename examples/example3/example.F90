!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                               !!
!!                  FORTIME                      !!
!!                                               !!
!!      Copyright (c) 2018, Thomas Stainer       !!
!!                                               !!
!!            All rights reserved.               !!
!!    Licensed under the 3-clause BSD license.   !!
!!                                               !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! Example using functor
module examplethree_m
    use fork_m
    use timefunctor_m

    type, extends(TimeFunctor), public :: TestFunctor
        real(kind=kr4) :: a
        real(kind=kr4) :: b
        real(kind=kr4) :: c
    contains
        procedure :: run
    end type TestFunctor

contains
    subroutine run(this)
        class(TestFunctor), intent(inout) :: this

        this%a = 2.5e-5_kr4
        this%b = 1.5e-5_kr4
        this%c = (this%a + this%b)**(2.1_ki4)/(this%a**4.5_ki4)

    end subroutine run

end module examplethree_m

!! Program
program example
    use fork_m
    use examplethree_m
    implicit none

    type(TestFunctor) :: fctr
    real(kind=kr4) :: elapsedseconds

    fctr%iterations = 10000000_ki4
    call timeit(fctr)
    call fctr%printsummary()

end program example
