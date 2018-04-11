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
    use fork_m
    use fortime_m
    implicit none
    private

    public :: timeit

    !> Extend this to benchmark functor
    type, public, abstract :: TimeFunctor
        integer(kind=ki2) :: repeats = 3_ki2   ! Best of three by default
    contains
        procedure(functor_timeit), deferred :: run
    end type TimeFunctor

    abstract interface
        subroutine functor_timeit(this)
            import :: TimeFunctor
            implicit none
            class(TimeFunctor), intent(inout) :: this
        end subroutine functor_timeit
    end interface

contains

    !> Get the CPU time taken for functor
    function timeit(func) result(average_time)
        class(TimeFunctor), intent(inout) :: func

        real(kind=kr4)    :: time, average_time
        integer(kind=ki2) :: r

        type(Timer) :: stopwatch

        average_time = 0.0_kr4
        do r=1,func%repeats
            call stopwatch%start()
            call func%run()
            call stopwatch%elapsed(time)
            average_time = average_time + time
            call stopwatch%stop()
        enddo

        average_time = average_time/func%repeats
    end function timeit

end module timefunctor_m
