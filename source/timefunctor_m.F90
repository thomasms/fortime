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

module timefunctor_m
    use kinds_m
    use fortime_m
    implicit none
    private

    public :: timeit

    !> Extend this to benchmark functor
    type, public, abstract :: Functor
    contains
        procedure(functor_timeit), deferred :: run
    end type Functor

    abstract interface
        subroutine functor_timeit(this)
            import :: Functor
            implicit none
            class(Functor), intent(inout) :: this
        end subroutine functor_timeit
    end interface

contains

    !> Get the CPU time taken for functor
    function timeit(func) result(time)
        class(Functor), intent(inout) :: func
        real(kind=sp) :: time

        type(Timer) :: stopwatch

        call stopwatch%start()
        call func%run()
        call stopwatch%elapsed(time)
        call stopwatch%finish()

    end function timeit

end module timefunctor_m
