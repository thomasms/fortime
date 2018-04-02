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
        real(kind=dp) :: a
        real(kind=dp) :: b
        real(kind=dp) :: c
    contains
        procedure :: run
    end type TestFunctor

contains
    subroutine run(this)
        class(TestFunctor), intent(inout) :: this

        integer(dp), parameter :: iterations = 1000000_sp
        integer(sp) :: i

        do i=1, iterations
            this%a = 2.5e-5
            this%b = 1.5e-5
            this%c = (this%a + this%b)**(i-2)/(this%a**i)
        enddo
    end subroutine run

end module examplethree_m

!! Program
program example
    use fork_m
    use examplethree_m
    implicit none

    type(TestFunctor) :: fctr
    real(kind=sp) :: elapsedseconds

    elapsedseconds = timeit(fctr)
    print *, elapsedseconds

end program example
